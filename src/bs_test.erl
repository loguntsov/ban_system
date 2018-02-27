-module(bs_test).

%% API
-export([
  init/0
]).

-define(SERVER, test).

init() ->
  try
    test()
  catch
    E:R ->
      io:format("~n--------------~n~p: ~p ~p~n~n", [ E, R, erlang:get_stacktrace() ]),
      halt(1)
  end,
  halt(0).

test() ->
  {ok, _ } = application:ensure_all_started(ban_system),
  {ok, _ } = ban_system:start(?SERVER, 1000, 2, []),
  io:format("~n~nBasic no blocked object test~n", []),
  [{ <<"hello">>, false}] = ban_system:is_blocked(?SERVER, [<<"hello">>]),

  io:format("Attempts test~n", []),
  [{ <<"hello">>, 1 }] = ban_system:add_attempt(?SERVER, [<<"hello">>]),
  [{ <<"hello">>, 2 }] = ban_system:add_attempt(?SERVER, [<<"hello">>]),
  [{ <<"hello">>, blocked }] = ban_system:attempts(?SERVER, [<<"hello">>]),

  io:format("Basic block object test~n", []),
  ok = ban_system:block(?SERVER, [ <<"blabla">>] ),
  [{ <<"blabla">>, blocked }] = ban_system:attempts(?SERVER, [<<"blabla">>]),
  [{ <<"blabla">>, true}] = ban_system:is_blocked(?SERVER, [ <<"blabla">>]),

  timer:sleep(1000),

  [{ <<"blabla">>, true}] = ban_system:is_blocked(?SERVER, [ <<"blabla">>]),

  timer:sleep(1000),

  [{ <<"blabla">>, false}] = ban_system:is_blocked(?SERVER, [ <<"blabla">>]),

  io:format("Clear block test~n", []),
  ban_system:clear(?SERVER),
  ban_system:clear(?SERVER),

  ok = ban_system:block(?SERVER, [ <<"blabla">>] ),
  [{ <<"blabla">>, true}] = ban_system:is_blocked(?SERVER, [ <<"blabla">>]),
  ban_system:clear(?SERVER),
  [{ <<"blabla">>, true}] = ban_system:is_blocked(?SERVER, [ <<"blabla">>]),
  ban_system:clear(?SERVER),
  [{ <<"blabla">>, false}] = ban_system:is_blocked(?SERVER, [ <<"blabla">>]),

  io:format("All test passed~n", []).


