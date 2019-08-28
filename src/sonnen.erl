%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Load Json data from sonnen battery
%%% @end
%%% Created : 28 Aug 2019 by Tony Rogvall <tony@rogvall.se>

-module(sonnen).

-export([read_status/0, read_status/1]).

read_status() ->
    read_status("192.168.2.91").

read_status(IP) ->
    application:ensure_started(exo),
    exo_socket:wget("http://"++IP++"/v1/status").
