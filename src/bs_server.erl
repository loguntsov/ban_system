-module(bs_server).

-behaviour(gen_server).

%% API
-export([
  init/0,
  start_link/3, start_link/4,
  clear/1,
  get_counters/2,
  increments/2,
  add/2, check/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(ETS, ?MODULE).

-record(state, {
  name :: atom(),
  timeout :: pos_integer(),
  current_bloom :: reference(),
  old_bloom :: reference(),
  attempts_total :: pos_integer(),
  counter :: tuple()
}).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
  ?ETS = ets:new(?ETS, [ named_table, public, set, { read_concurrency, true}, { write_concurrency, true }]),
  ok.

start_link(Name, Attempts, Timeout) ->
  start_link(Name, Attempts, Timeout, []).

start_link(Name, Attempts, Timeout, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [ Name, Attempts, Timeout, Opts ], []).

clear(Name) ->
  gen_server:call(Name, clear).

get_counters(Name, Objects) when is_list(Objects) ->
  Counter = get_counter_info(Name),
  Size = size(Counter),
  lists:map(fun(Obj) ->
    Num = case check_counter(Obj, 1, Counter) of
      false -> 0;
      true ->
        case get_counter_value(Obj, Counter) of
          { _, S, true } when S =:= Size -> blocked;
          { _, A, true } -> A;
          { _, A, false } -> A-1
        end
    end,
    { Obj, Num }
  end, Objects).

increments(Name, Objects) when is_list(Objects)->
  Counter = get_counter_info(Name),
  lists:map(fun(Obj) ->
    Result = case increment(Obj, Counter) of
      full ->
        add(Name, [Obj]),
        banned;
      {ok, A } -> A
    end,
    { Obj, Result }
  end, Objects).

add(Name, Objects) when is_list(Objects) ->
  [{_, CurrentBloom}] = ets:lookup(?ETS, { current_bloom, Name }),
  [{_, Counter}] = ets:lookup(?ETS, {counter, Name}),
  CounterList = tuple_to_list(Counter),
  lists:foreach(fun(Obj) ->
    B = term_to_binary(Obj),
    ebloom:insert(CurrentBloom, B),
    lists:foreach(fun(C) ->
      ebloom:insert(C, B)
    end, CounterList)
  end, Objects).


check(Name, Objects) ->
  [{ _, CurrentBloom}] = ets:lookup(?ETS, { current_bloom, Name }),
  [{ _, OldBloom}] = ets:lookup(?ETS, { old_bloom, Name }),
  lists:map(fun(Obj) ->
    B = term_to_binary(Obj),
    Flag = ebloom:contains(CurrentBloom, B) orelse ebloom:contains(OldBloom, B),
    { Obj, Flag }
  end, Objects).

init([ Name, Attempts, Timeout, Opts ]) ->
  random:seed(os:timestamp()),
  PredictedElementCount = proplists:get_value(predicted_element_count, Opts, 10000000),
  FalsePositiveProbability = proplists:get_value(false_positive_probability, Opts, 0.00001),
  { ok, CurrentBloom } = ebloom:new(
    PredictedElementCount,
    FalsePositiveProbability,
    random:uniform(99999999999)
  ),
  { ok, OldBloom } = ebloom:new(
    PredictedElementCount,
    FalsePositiveProbability,
    random:uniform(99999999999)
  ),
  Counter = list_to_tuple(lists:map(fun(_Index) ->
    { ok, Ref } = ebloom:new(
    PredictedElementCount,
    FalsePositiveProbability,
      random:uniform(99999999999)
    ),
    Ref
  end, lists:seq(1, Attempts))),
  State = #state{
    name = Name,
    attempts_total = Attempts,
    timeout = Timeout,
    current_bloom = CurrentBloom,
    old_bloom = OldBloom,
    counter = Counter
  },
  save_state(State),
  ets:insert(?ETS, [
    {{ counter, Name }, Counter }
  ]),
  {ok, tick(State)}.

handle_call(clear, _From, State) ->
  NewState = do_clear(State),
  {reply, ok, NewState};
handle_call(_, _, State) ->
  { reply, error, State }.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(tick, State) ->
  NewState = do_clear(State),
  { noreply, tick(NewState) };
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(State) ->
  case State#state.timeout of
    undefined -> ok;
    infinity -> ok;
    _ -> erlang:send_after( State#state.timeout, self(), tick)
  end,
  State.

do_clear(State) ->
  ebloom:clear(State#state.old_bloom),
  NewState = State#state{
    current_bloom = State#state.old_bloom,
    old_bloom = State#state.current_bloom
  },
  save_state(NewState),
  lists:map(fun(Ref) ->
    ebloom:clear(Ref)
  end, tuple_to_list(State#state.counter)),
  NewState.

save_state(State) ->
  Name = State#state.name,
  ets:insert(?ETS, [
    {{ current_bloom, Name }, State#state.current_bloom },
    {{ old_bloom, Name }, State#state.old_bloom }
  ]).


get_counter_info(Name) ->
  [{_, Value}] = ets:lookup(?ETS, {counter, Name}),
  Value.

get_counter_value(Obj, Counter) ->
  counter_half_div(Obj, 0, size(Counter), Counter, undefined).

counter_half_div(Obj, A, B, Counter, Acc) when A + 1 =:= B ->
  NewAcc = case Acc of
    undefined ->
      check_counter(Obj, B, Counter);
    _ -> Acc
  end,
  { element(B, Counter), B, NewAcc };
counter_half_div(Obj, From, To, Counter, _Acc) ->
  Div = (From + To + 1) div 2,
  case check_counter(Obj, Div, Counter) of
    true -> counter_half_div(Obj, Div, To, Counter, undefined);
    false -> counter_half_div(Obj, From, Div, Counter, false)
  end.

check_counter(Obj, Num, Counter) ->
  ebloom:contains(element(Num, Counter), term_to_binary(Obj)).

increment(Obj, Counter) ->
  N = size(Counter),
  case get_counter_value(Obj, Counter) of
    { _, A, true } when N =:= A -> full;
    { Ref, A, false } ->
      ebloom:insert(Ref, term_to_binary(Obj)),
      { ok, A }
  end.