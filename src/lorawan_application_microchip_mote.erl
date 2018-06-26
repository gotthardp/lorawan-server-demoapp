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
        {motes, [
            {record_name, mote},
            {attributes, record_info(fields, mote)},
            {disc_copies, [node()]}]}
        ]),
    % setup web-admin
    {ok, [{shepherd, [
        {"/api/motes/[:devaddr]", lorawan_admin_db_record,
            [motes, mote, record_info(fields, mote), lorawan_admin]},
        {"/demo", cowboy_static, {priv_file, lorawan_demoapp, "demo/index.html"}},
        {"/demo/[...]", cowboy_static, {priv_dir, lorawan_demoapp, "demo"}}
    ]}]}.

handle_join({_Network, _Profile, _Device}, {_MAC, _RxQ}, _DevAddr) ->
    % accept any device
    ok.

handle_uplink({_Network, _Profile, _Node}, _RxQ, {lost, _State}, _Frame) ->
    retransmit;
handle_uplink(_Context, _RxQ, _LastAcked, _Frame) ->
    % accept and wait for deduplication
    {ok, []}.

% the data structure is explained in
% Lora_Legacy_Mote_Firmware/Includes/Board/MOTEapp.c:520
handle_rxq({_Network, _Profile, #node{devaddr=DevAddr}}, _Gateways, _WillReply,
        #frame{data= <<Light:5/binary, Temp:3/binary>>}, []) ->
    lager:debug("PUSH_DATA ~w ~p ~p", [DevAddr, Light, Temp]),
    % store the most recent value
    ok = mnesia:dirty_write(motes, #mote{devaddr=DevAddr, light=Light, temp=Temp}),
    % display actual time
    {H, M, S} = time(),
    Time = lists:flatten(io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S])),
    {send, #txdata{port=2, data=list_to_binary(Time)}};

handle_rxq(_Context, _Gateways, _WillReply, Frame, _State) ->
    {error, {unexpected_data, Frame}}.

handle_delivery({_Network, _Profile, _Node}, _Result, _Receipt) ->
    ok.

% end of file
