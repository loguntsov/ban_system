-module(ban_system).

-export([
  start/4, start/3,
  is_blocked/2, block/2,
  add_attempt/2, attempts/2,
  clear/1
]).

start(Name, Timeout, Attempts) ->
  start(Name, Timeout, Attempts, []).

start(Name, Timeout, Attempts, Opt) ->
  bs_app_sup:start_child(Name, [ Attempts, Timeout, Opt ]).

is_blocked(Name, Objects) ->
  bs_server:check(Name, Objects).

block(Name, Objects) ->
  bs_server:add(Name, Objects).

add_attempt(Name, Objects) ->
  bs_server:increments(Name, Objects).

attempts(Name, Objects) ->
  bs_server:get_counters(Name, Objects).

clear(Name) ->
  bs_server:clear(Name).

