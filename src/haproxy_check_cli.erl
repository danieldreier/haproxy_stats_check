-module(haproxy_check_cli).
-export([main/1,setting/2]).
-include_lib("eunit/include/eunit.hrl").

% main is the function that gets called with args by the CLI parser
main([]) ->
  % print usage help if we get called with no options
  getopt:usage(option_spec_list(), escript:script_name());
main(Args) ->
  OptSpecList = option_spec_list(),
  case getopt:parse(OptSpecList, Args) of
    {ok, {Options, _NonOptArgs}} ->
      % if the options parsed correctly, run the check
      run_check(Options);
    {error, {Reason, Data}} ->
      % if options parsing failed print why and the usage instructions
      io:format("Error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(OptSpecList, escript:script_name())
  end.

% convenience function to get a specific setting from the settings list
setting(Param, Settings) ->
  {Param, Value} = lists:keyfind(Param, 1, Settings),
  Value.

run_check(Options) ->
   StdOut = nagios:new(setting(check_name, Options)),
   Results = case haproxy:query_haproxy(setting(url, Options)) of
     {ok, Data}      -> Data;
     {error, Reason} -> nagios:add_output("error while connecting to Icinga API: " ++ io_lib:format("~p",[Reason]), nagios:set_state(unknown, StdOut))
   end,
%   Results = haproxy:query_haproxy(setting(url, Options)),
   Backend = setting(backend, Options),
   Counts = haproxy:count_by_status(haproxy:filter_by_backend(Backend, Results)),
   Crit_Threshold = setting(crit_threshold, Options),
   Warn_Threshold = setting(warn_threshold, Options),
   Status = check_status(Counts, Crit_Threshold, Warn_Threshold, Options),
   StdOut1 = nagios:set_state(Status, StdOut),
   UpCount = erlang:integer_to_list(maps:get(up, Counts)),
   DownCount = erlang:integer_to_list(maps:get(down, Counts)),
   StdOut2 = nagios:add_perfdata("up", UpCount, StdOut1),
   StdOut3 = nagios:add_perfdata("down", DownCount, StdOut2),
   Message = UpCount ++ " up, " ++ DownCount ++ " down",
   StdOut4 = nagios:add_output(Message, StdOut3),
   io:format("~s\n", [nagios:render(StdOut4)]),
   nagios:halt_with(Status).

check_status(#{down := Down, up := Up}, Crit_Threshold, Warn_threshold, Options ) ->
  case setting(state, Options) of
    down -> check_status(Down, Crit_Threshold, Warn_threshold);
    up   -> check_status(Up, Crit_Threshold, Warn_threshold)
  end.
check_status(Count, Crit_Threshold, _Warn_threshold ) when Count >= Crit_Threshold ->
  critical;
check_status(Count, _Crit_Threshold, Warn_threshold ) when Count >= Warn_threshold ->
  warning;
check_status(Count, Crit_Threshold, Warn_threshold ) when Count < Crit_Threshold, Count < Warn_threshold ->
  ok.

check_status_test() ->
  critical = check_status(1,1,1),
  warning  = check_status(1,2,1),
  critical = check_status(2,2,1),
  ok = check_status(0,1,1).

option_spec_list() ->
    [
     %% {Name,        ShortOpt,  LongOpt,          ArgSpec,               HelpMsg}
     {help,           undefined, "help",           undefined,             "Show the program options"},
     {check_name,     $n,        "name",           {string, "haproxy backend count"},   "Check name to display in output"},
     {url ,           $u,        "url",            {string, "http://localhost:7070/haproxy?stats;csv"}, "URL for haproxy stats CSV"},
     {warn_threshold, $w,        "warn-threshold", {integer, 1},          "warning threshold for matching servers"},
     {crit_threshold, $c,        "crit-threshold", {integer, 2},          "critical threshold for matching servers"},
     {state,          $s,        "state",          {atom, down},        "check state to test (up, down)"},
     {backend,        $b,        "backend",        string,                "haproxy backend to count servers for"},
     {debug,          undefined, "debug",          undefined,             "Enable verbose debug output"},
     {verbose,        $v,        "verbose",        integer,               "Verbosity level"}
    ].
