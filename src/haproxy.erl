-module(haproxy).
-export([parse_perfdata_line/1]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


% get useful data out of the CSV perf data
parse_perfdata_line(Data) ->
  %{ok, [{backend, <<"forgeapi">>}, {server, <<"app02-dev">>}, {health, up}]}.
  [ Backend,Server,_,_,_,_,_,_,_,_,_,_,_,_,_,Status|_ ] = string:tokens(binary_to_list(Data), ","),
  {ok, [{backend, Backend}, {server, Server}, {health, Status}]}.

parse_perfdata_line_test() ->
  {ok,[{backend,"forgeapi"},
       {server,"app02-dev"},
       {health,"UP"}]} = parse_perfdata_line(<<"forgeapi,app02-dev,0,0,0,3,4,20248,4317963,1030890514,,0,,0,0,0,0,UP,1,1,0,0,0,16953,0,,1,4,10,,20248,,2,3,,6,L7OK,200,12,0,19948,0,300,0,0,0,,,,0,0,,,,,0,OK,,0,1,57,63">>).
