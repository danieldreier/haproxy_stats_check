-module(haproxy).
-export([parse_perfdata_line/1]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% extract the useful data out of one line of CSV perf data
parse_perfdata_line(Data) ->
  case string:tokens(binary_to_list(Data), ",") of
    [] -> false;
    [ "# pxname" | _ ] -> false;
    [ "stats" | _ ] -> false;
    [ _, "BACKEND" | _ ] -> false;
    [ _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,Status|_ ] when Status == "no check" -> false;
    [ Backend,Server,_,_,_,_,_,_,_,_,_,_,_,_,_,Status|_ ] when Status /= "no check" -> {true, #{"backend" => Backend, "server" => Server, "health" => Status}}
  end.

% eunit test to validate that the correct data is extracted from each line
parse_perfdata_line_test() ->
  false = parse_perfdata_line(<<"# pxname,svname,qcur,qmax,scur,smax,slim,stot,bin,bout,dreq,dresp,ereq,econ,eresp,wretr,wredis,status,weight,act,bck,chkfail,chkdown,lastchg,downtime,qlimit,pid,iid,sid,throttle,lbtot,tracked,type,rate,rate_lim,rate_max,check_status,check_code,check_duration,hrsp_1xx,hrsp_2xx,hrsp_3xx,hrsp_4xx,hrsp_5xx,hrsp_other,hanafail,req_rate,req_rate_max,req_tot,cli_abrt,srv_abrt,comp_in,comp_out,comp_byp,comp_rsp,lastsess,last_chk,last_agt,qtime,ctime,rtime,ttime,">>),
  {true, #{"backend" := "forgeapi",
          "server"  := "app02-dev",
          "health"  := "UP"}} = parse_perfdata_line(<<"forgeapi,app02-dev,0,0,0,3,4,20248,4317963,1030890514,,0,,0,0,0,0,UP,1,1,0,0,0,16953,0,,1,4,10,,20248,,2,3,,6,L7OK,200,12,0,19948,0,300,0,0,0,,,,0,0,,,,,0,OK,,0,1,57,63">>),
  {true, #{"backend" := "forge",
           "server"  := "forge-i-526bf495_forgenext-app04-dev.ops.puppetlabs.net",
           "health"  := "DOWN"}} = parse_perfdata_line(<<"forge,forge-i-526bf495_forgenext-app04-dev.ops.puppetlabs.net,0,0,0,1,4,137,11508,79597,,0,,0,0,0,0,DOWN,1,1,0,3,1,8,8,,1,3,14,,137,,2,0,,1,L4CON,,0,0,137,0,0,0,0,0,,,,0,0,,,,,51,Connection refused,,0,1,5,6,">>).

% split the raw bit string text output from haproxy stats into individual lines
parse_perfdata_blob(Blob) ->
  Line_list = binary:split(Blob, <<"\n">>, [global]),
  lists:filtermap(fun(Line) -> parse_perfdata_line(Line) end, Line_list).

% dumb test that compares a known good input and output
parse_perfdata_blob_test() ->
  Blob = <<"forgeapi,forgeapi-backup-i-d3a8eb09_forgenext-app05-dev.ops.puppetlabs.net,0,0,0,0,4,0,0,0,,0,,0,0,0,0,no check,1,0,1,,,,,,1,4,15,,0,,2,0,,0,,,,0,0,0,0,0,0,0,,,,0,0,,,,,-1,,,0,0,0,0,\nforgeapi,forgeapi-i-d3a8eb09_forgenext-app05-dev.ops.puppetlabs.net,0,0,0,4,4,99130,21142496,4976400059,,0,,0,0,0,0,UP,1,1,0,31,2,51240,11,,1,4,16,,99130,,2,1,,8,L7OK,200,10,0,97738,0,1392,0,0,0,,,,2,0,,,,,1,OK,,0,1,69,75,\nforgeapi,forgeapi-backup-i-7cd771a4_forgenext-app06-dev.ops.puppetlabs.net,0,0,0,0,4,0,0,0,,0,,0,0,0,0,no check,1,0,1,,,,,,1,4,17,,0,,2,0,,0,,,,0,0,0,0,0,0,0,,,,0,0,,,,,-1,,,0,0,0,0,\nforgeapi,forgeapi-i-7cd771a4_forgenext-app06-dev.ops.puppetlabs.net,0,0,0,4,4,99135,21143810,4977417293,,0,,0,0,0,0,UP,1,1,0,0,0,93694,0,,1,4,18,,99135,,2,1,,8,L7OK,200,13,0,97759,0,1376,0,0,0,,,,5,0,,,,,1,OK,,0,1,64,70,\nforgeapi,forgeapi-backup-i-ef69f628_forgenext-app07-dev.ops.puppetlabs.net,0,0,0,0,4,0,0,0,,0,,0,0,0,0,no check,1,0,1,,,,,,1,4,19,,0,,2,0,,0,,,,0,0,0,0,0,0,0,,,,0,0,,,,,-1,,,0,0,0,0,\nforgeapi,forgeapi-i-ef69f628_forgenext-app07-dev.ops.puppetlabs.net,0,0,0,3,4,99136,21144396,5001675177,,0,,0,0,0,0,UP,1,1,0,0,0,93694,0,,1,4,20,,99136,,2,1,,8,L7OK,200,10,0,97733,0,1403,0,0,0,,,,2,0,,,,,1,OK,,0,1,52,57,\nforgeapi,forgeapi-backup-i-b1a9ea6b_forgenext-app08-dev.ops.puppetlabs.net,0,0,0,0,4,0,0,0,,0,,0,0,0,0,no check,1,0,1,,,,,,1,4,21,,0,,2,0,,0,,,,0,0,0,0,0,0,0,,,,0,0,,,,,-1,,,0,0,0,0,\nforgeapi,forgeapi-i-b1a9ea6b_forgenext-app08-dev.ops.puppetlabs.net,0,0,0,3,4,99136,21140376,4976920716,,0,,0,0,0,0,UP,1,1,0,0,0,93694,0,,1,4,22,,99136,,2,1,,8,L7OK,200,11,0,97629,0,1507,0,0,0,,,,7,0,,,,,1,OK,,0,1,64,70,\nforgeapi,BACKEND,0,0,0,16,800,1090296,232538140,54700259490,0,0,,0,0,0,25,UP,11,11,11,,0,93694,0,,1,4,0,,1090321,,1,9,,90,,,,0,1074568,0,15718,10,0,,,,,38,0,0,0,0,0,0,,,0,1,74,80,\nstats,FRONTEND,,,1,1,8000,1575,210314,15654630,0,0,0,,,,,OPEN,,,,,,,,,1,5,0,,,,0,1,0,2,,,,0,1574,0,0,0,0,,1,2,1575,,,0,0,0,0,,,,,,,,\nstats,BACKEND,0,0,0,0,800,0,210314,15654630,0,0,,0,0,0,0,UP,0,0,0,,0,93694,0,,1,5,0,,0,,1,0,,0,,,,0,0,0,0,0,0,,,,,0,0,0,0,0,0,0,,,825,0,0,1,\n">>,

[#{"backend" := "forgeapi",
		"health" := "UP",
		"server" := "forgeapi-i-d3a8eb09_forgenext-app05-dev.ops.puppetlabs.net"},
	#{"backend" := "forgeapi",
		"health" := "UP",
		"server" := "forgeapi-i-7cd771a4_forgenext-app06-dev.ops.puppetlabs.net"},
	#{"backend" := "forgeapi",
		"health" := "UP",
		"server" := "forgeapi-i-ef69f628_forgenext-app07-dev.ops.puppetlabs.net"},
	#{"backend" := "forgeapi",
		"health" := "UP",
		"server" := "forgeapi-i-b1a9ea6b_forgenext-app08-dev.ops.puppetlabs.net"}] = parse_perfdata_blob(Blob).

filter_by_backend(Backend, List) when is_list(List) ->
  lists:filtermap(fun(Line) -> filter_by_backend(Backend, Line) end, List);
filter_by_backend(Target_backend, #{"backend" := Backend} = Line) when is_map(Line) ->
  Target_backend == Backend.

filter_by_backend_test() ->
  Backend = "forgeapi",
  % start with a list that includes both "forgeapi" and "forge" backends
  List = [#{"backend" => "forgeapi",
    "health" => "UP",
    "server" => "forgeapi-i-d3a8eb09_forgenext-app05-dev.ops.puppetlabs.net"},
  #{"backend" => "forgeapi",
    "health" => "UP",
    "server" => "forgeapi-i-7cd771a4_forgenext-app06-dev.ops.puppetlabs.net"},
  #{"backend" => "forge",
    "health" => "UP",
    "server" => "forge-i-ef69f628_forgenext-app07-dev.ops.puppetlabs.net"},
  #{"backend" => "forge",
    "health" => "UP",
    "server" => "forge-i-b1a9ea6b_forgenext-app08-dev.ops.puppetlabs.net"}],

  % we should only get back the maps that have backend == forgeapi
  [#{"backend" := "forgeapi",
    "health" := "UP",
    "server" := "forgeapi-i-d3a8eb09_forgenext-app05-dev.ops.puppetlabs.net"},
  #{"backend" := "forgeapi",
    "health" := "UP",
    "server" := "forgeapi-i-7cd771a4_forgenext-app06-dev.ops.puppetlabs.net"}] = filter_by_backend(Backend, List).
