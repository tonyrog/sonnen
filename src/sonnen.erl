%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Load Json data from sonnen battery
%%% @end
%%% Created : 28 Aug 2019 by Tony Rogvall <tony@rogvall.se>

-module(sonnen).

-export([i/0, i/1, i/2]).
-export([read_status/0, read_status/1, read_status/2]).

-define(DEFAULT_IP, "192.168.2.91").
-define(DEFAULT_PORT, "8080").

i() -> i(?DEFAULT_IP, ?DEFAULT_PORT).
i(IP) -> i(IP,?DEFAULT_PORT).
i(IP,Port) ->
    Status = read_status(IP,Port),
    Consumption = proplists:get_value(consumption,Status),
    GridFeed = proplists:get_value(grid_feed,Status),
    Production = proplists:get_value(production,Status),
    SOC = proplists:get_value(usoc, Status),
    format_w("Consumption", w(Consumption)),
    format_w("GridFeed", w(GridFeed)),
    format_w("Production", w(Production)),
    io:format("SOC ~w%\n", [SOC]),
    ok.

format_w(Fmt, {Watt, Wfmt}) ->
    io:format(Fmt++" "++Wfmt++"\n", [Watt]).

w(W) when W > 1000000 ->
    {W / 1000000, "~f Mw"};
w(W) when W > 1000 ->
    {W / 1000, "~f Kw"};
w(W) ->
    {W, "~w W"}.

read_status() ->
    read_status(?DEFAULT_IP).
read_status(IP) ->
    read_status(IP, ?DEFAULT_PORT).
read_status(IP,Port) ->
    (catch error_logger:tty(false)),
    application:start(inets),
    case httpc:request("http://"++IP++":"++Port++"/api/v1/status") of
	{ok, {_Resp,_Headers,Data}} ->
	    case sonnen_json:decode_string(Data) of
		{ok, {struct,Fields}} ->
		    decode_struct(Fields);
		{ok, _} ->
		    {error, bad_data};
		E = {error,_} ->
		    E
	    end;
	E = {error,_} ->
	    E
    end.

decode_struct([{Key, Value} | Fs]) ->
    K = decode_key(Key),
    [{K,Value} | decode_struct(Fs)];
decode_struct([]) ->
    [].

%% Production_W - Consumption_W - Pac_total_W = GridFeedIn_W

decode_key(Key) ->
    case Key of
	"BackupBuffer" -> backup_buffer;
	"BatteryCharging" -> battery_charging;
	"BatteryDischarging" -> battery_discharging;
	"Consumption_W" -> consumption;
	"Fac" -> fac;
	"FlowConsumptionBattery" -> flow_consumption_battery;
	"FlowConsumptionGrid" -> flow_consumption_grid;
	"FlowConsumptionProduction" -> flow_consumption_production;
	"FlowGridBattery" -> flow_grid_battery;
	"FlowProductionBattery" -> flow_production_battery;
	"FlowProductionGrid" -> flow_production_grid;
	"GridFeedIn_W" -> grid_feed;
	"IsSystemInstalled" -> is_system_installed;
	"OperatingMode" -> operating_mode;
	"Pac_total_W" -> pac_total;
	"Production_W" -> production;
	"RSOC" -> rsoc;
	"SystemStatus" -> system_status;
	"Timestamp" -> timestamp; 
	"USOC" -> usoc;
	"Uac" -> uac;
	"Ubat" -> ubat;
	_ -> string:to_lower(Key)
    end.
