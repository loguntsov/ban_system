-module(ban_system_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

start(_StartType, _StartArgs) ->
  { ok, Pid } = bs_app_sup:start_link(),
  bs_server:init(),
  { ok, Pid }.

-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
