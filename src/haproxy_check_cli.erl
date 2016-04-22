-module(haproxy_check_cli).
-export([main/1,setting/2]).
-ifdef(TEST).
% if in test mode, include eunit unit test headers
% and also expose run_check/1 so we can run the check from
% the erl shell rather than just the escript executable
-include_lib("eunit/include/eunit.hrl").
-export([test_options/0,run_check/1]).
-endif.

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
  case lists:keyfind(Param, 1, Settings) of
    {Param, Value} -> Value;
    false ->
      io:format("Error: command line parameter \"~s\" must be set ~n", [erlang:atom_to_list(Param)]),
      erlang:halt(3)
  end.

% the main function that actually executes the check
run_check(Options) ->
   % set up nagios check data structure
   StdOut = nagios:new(setting(check_name, Options)),

   % query haproxy stats
   Results = case haproxy:query_haproxy(setting(url, Options)) of
     {ok, Data}      -> Data;
     {error, Reason} ->
                 ErrorOutput = nagios:add_output("error while connecting to haproxy: " ++ io_lib:format("~p",[Reason]), StdOut),
                 Output = nagios:set_state(unknown, ErrorOutput),
                 io:format("~s\n", [nagios:render(Output)]),
                 nagios:halt_with(unknown)
   end,

   % compare haproxy stats to the thresholds
   Backend = setting(backend, Options),
   Counts = haproxy:count_by_status(haproxy:filter_by_backend(Backend, Results)),
   Crit_Threshold = setting(crit_threshold, Options),
   Warn_Threshold = setting(warn_threshold, Options),
   UpCount = maps:get(up, Counts),
   DownCount = maps:get(down, Counts),
   Status = check_status(DownCount, Crit_Threshold, Warn_Threshold),

   % generate nagios-format output and exit with correct status
   StdOut1 = nagios:set_state(Status, StdOut),
   StdOut2 = nagios:add_perfdata("up", erlang:integer_to_list(UpCount), StdOut1),
   StdOut3 = nagios:add_perfdata("down", erlang:integer_to_list(DownCount), StdOut2),
   Message = "haproxy backend \"" ++ Backend ++ "\" has " ++ erlang:integer_to_list(UpCount) ++ " up, " ++ erlang:integer_to_list(DownCount) ++ " down",
   StdOut4 = nagios:add_output(Message, StdOut3),
   io:format("~s\n", [nagios:render(StdOut4)]),
   nagios:halt_with(Status).

% logic for converting check counts and thresholds to check statuses
check_status(Count, Crit_Threshold, _Warn_threshold ) when Count >= Crit_Threshold ->
  critical;
check_status(Count, _Crit_Threshold, Warn_threshold ) when Count >= Warn_threshold ->
  warning;
check_status(Count, Crit_Threshold, Warn_threshold ) when Count < Crit_Threshold, Count < Warn_threshold ->
  ok.

% command line options for the resulting executable
option_spec_list() ->
    [
     %% {Name,        ShortOpt,  LongOpt,          ArgSpec,               HelpMsg}
     {check_name,     $n,        "name",           {string, "haproxy backend count"},   "Check name to display in output"},
     {url ,           $u,        "url",            {string, "http://localhost:7070/haproxy?stats;csv"}, "URL for haproxy stats CSV"},
     {warn_threshold, $w,        "warn-threshold", {integer, 1},          "warning threshold for matching servers"},
     {crit_threshold, $c,        "crit-threshold", {integer, 2},          "critical threshold for matching servers"},
     {backend,        $b,        "backend",        string,                "haproxy backend to count servers for"}
    ].

-ifdef(TEST).
% eunit tests are inlined with the code to make it easier to keep them current
check_status_test() ->
  critical = check_status(1,1,1),
  warning  = check_status(1,2,1),
  critical = check_status(2,2,1),
  ok = check_status(0,1,1).

test_options() ->
  % simple options data structure to simplify testing
  [{warn_threshold,3},
   {crit_threshold,11},
   {backend,"forgeapi"},
   {check_name,"haproxy backend count"},
   {url,"http://localhost:7070/haproxy?stats;csv"}].
-endif.
