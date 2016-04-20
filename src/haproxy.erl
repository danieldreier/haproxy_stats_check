-module(haproxy).
-export([parse_perfdata_line/1]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


% get useful data out of the CSV perf data
parse_perfdata_line(Data) ->
  %{ok, [{backend, <<"forgeapi">>}, {server, <<"app02-dev">>}, {health, up}]}.
  case string:tokens(binary_to_list(Data), ",") of
    [ "# pxname" | _ ] -> {ok, []};
    [ Backend,Server,_,_,_,_,_,_,_,_,_,_,_,_,_,Status|_ ] -> {ok, [{backend, Backend}, {server, Server}, {health, Status}]}
  end.

parse_perfdata_line_test() ->
  {ok,[]} = parse_perfdata_line(<<"# pxname,svname,qcur,qmax,scur,smax,slim,stot,bin,bout,dreq,dresp,ereq,econ,eresp,wretr,wredis,status,weight,act,bck,chkfail,chkdown,lastchg,downtime,qlimit,pid,iid,sid,throttle,lbtot,tracked,type,rate,rate_lim,rate_max,check_status,check_code,check_duration,hrsp_1xx,hrsp_2xx,hrsp_3xx,hrsp_4xx,hrsp_5xx,hrsp_other,hanafail,req_rate,req_rate_max,req_tot,cli_abrt,srv_abrt,comp_in,comp_out,comp_byp,comp_rsp,lastsess,last_chk,last_agt,qtime,ctime,rtime,ttime,">>),
  {ok,[{backend,"forgeapi"},
       {server,"app02-dev"},
       {health,"UP"}]} = parse_perfdata_line(<<"forgeapi,app02-dev,0,0,0,3,4,20248,4317963,1030890514,,0,,0,0,0,0,UP,1,1,0,0,0,16953,0,,1,4,10,,20248,,2,3,,6,L7OK,200,12,0,19948,0,300,0,0,0,,,,0,0,,,,,0,OK,,0,1,57,63">>),
  {ok,[{backend,"forge"},
       {server,"forge-i-526bf495_forgenext-app04-dev.ops.puppetlabs.net"},
       {health,"DOWN"}]} = parse_perfdata_line(<<"forge,forge-i-526bf495_forgenext-app04-dev.ops.puppetlabs.net,0,0,0,1,4,137,11508,79597,,0,,0,0,0,0,DOWN,1,1,0,3,1,8,8,,1,3,14,,137,,2,0,,1,L4CON,,0,0,137,0,0,0,0,0,,,,0,0,,,,,51,Connection refused,,0,1,5,6,">>).

%parse_perfdata_blob(Blob) ->
%  Line_list = binary:split(Blob, <<"\n">>),
%  lists
