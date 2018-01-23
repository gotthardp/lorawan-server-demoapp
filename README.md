# Demo Application for the Compact LoRaWAN Server

This repository contains a simple extension of the
[compact server for private LoRaWAN networks](https://github.com/gotthardp/lorawan-server).
It demonstrates how to build a LoRaWAN application implemented in Erlang.

Compared to the extenal applications (Backends) the internal applications have
several benefits:
 - Smaller infrastructure as there is only one server, which is easier to secure and maintain
 - Ability to send downlinks in the RX1 window, which reduces the device power consumption
 - Lower development effort as internal applications can reuse the internal web-server and database

To develop internal applications you have to
[Learn You Some Erlang](http://learnyousomeerlang.com/introduction) first.

## Build Instructions

Build the application by:
```bash
rebar3 release
```

The lorawan-server and its dependencies will be downloaded automatically. There is
no need to do anything else.

Then start:
```
_build/default/rel/lorawan-server-demoapp/bin/lorawan-server-demoapp
```

## Getting Help

To ask server and LoRa related questions please join the
[lorawan-server mailing list](https://groups.google.com/forum/#!forum/lorawan-server).

To ask Erlang related questions please join the
[Erlang community](https://www.erlang.org/community).
