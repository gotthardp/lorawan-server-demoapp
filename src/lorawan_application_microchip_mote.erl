%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% Microchip LoRa(TM) Technology Mote
% http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138
%
-module(lorawan_application_microchip_mote).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_uplink/4, handle_rxq/5, handle_delivery/3]).

-include_lib("lorawan_server/include/lorawan.hrl").
-include_lib("lorawan_server/include/lorawan_db.hrl").

-record(mote, {devaddr, light, temp}).

init(_App) ->
    % setup database
    lists:foreach(fun({Name, TabDef}) -> lorawan_db:ensure_table(Name, TabDef) end, [
        {mote, [
            {attributes, record_info(fields, mote)},
            {disc_copies, [node()]}]}
        ]),
    % setup web-admin
    {ok, [{demoapp,
        % define custom authorization scopes used bellow
        #{scopes => [<<"motes.light:read">>, <<"motes.temp:read">>],
        % defined web-server routes (see cowboy documentation)
        routes => [
            % auto-generated REST API for the mote sensor readings
            {"/api/motes/[:devaddr]", lorawan_admin_db_record,
                {mote, record_info(fields, mote), lorawan_admin,
                    % standard read scope for administration
                    {[{<<"device:read">>, '*'},
                        % access to the light values only
                        {<<"motes.light:read">>, [devaddr, light]},
                        % access to the temperature values only
                        {<<"motes.temp:read">>, [devaddr, temp]}],
                    % standard write scope for administration
                    [{<<"device:write">>, '*'}]}}},
            % custom index file
            {"/demo", lorawan_admin_static,
                {priv_file, lorawan_demoapp, <<"demo/index.html">>,
                    [{<<"web-admin">>, '*'}]}},
            % custom web-admin pages
            {"/demo/[...]", lorawan_admin_static,
                {priv_dir, lorawan_demoapp, <<"demo">>,
                    [{<<"web-admin">>, '*'}]}}
        ]}}]}.

% called upon device join
handle_join({_Network, _Profile, _Device}, {_MAC, _RxQ}, _DevAddr) ->
    % accept any device
    ok.

% called upon uplink from the first gateway (before deduplication)
handle_uplink({_Network, _Profile, _Node}, _RxQ, {lost, _State}, _Frame) ->
    retransmit;
handle_uplink(_Context, _RxQ, _LastAcked, _Frame) ->
    % accept and wait for deduplication
    {ok, []}.

% called upon reception of the uplink from all gateways (after deduplication)
% the data structure is explained in
% Lora_Legacy_Mote_Firmware/Includes/Board/MOTEapp.c:520
handle_rxq({_Network, _Profile, #node{devaddr=DevAddr}}, _Gateways, _WillReply,
        #frame{data= <<Light:5/binary, Temp:3/binary>>}, []) ->
    lager:debug("PUSH_DATA ~w ~p ~p", [DevAddr, Light, Temp]),
    % store the most recent value
    ok = mnesia:dirty_write(motes, #mote{devaddr=DevAddr, light=Light, temp=Temp}),
    % downlink actual time back to the device
    {H, M, S} = time(),
    Time = lists:flatten(io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S])),
    {send, #txdata{port=2, data=list_to_binary(Time)}};

handle_rxq(_Context, _Gateways, _WillReply, Frame, _State) ->
    {error, {unexpected_data, Frame}}.

% called upon delivery of a confirmed downlink
handle_delivery({_Network, _Profile, _Node}, _Result, _Receipt) ->
    ok.

% end of file
