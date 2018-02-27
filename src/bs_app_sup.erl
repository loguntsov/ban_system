-module(bs_app_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_child/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, []}}.

start_child(Name, Args) ->
  supervisor:start_child(?SERVER, { Name, { bs_server, start_link, [ Name | Args ] }, permanent, 2000, worker, [ bs_server ]}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
