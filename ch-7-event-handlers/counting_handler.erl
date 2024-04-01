-module(counting_handler).
-behaviour(gen_event).
-export([init/1, terminate/2, handle_event/2, handle_info/2]).
-export([get_counters/1, handle_call/2]).

init({}) ->
  TableId = ets:new(counting_handler, []),
  {ok, TableId}.

terminate(_Reason, TableId) ->
  Counters = ets:tab2list(TableId),
  ets:delete(TableId),
  {counting_handler, Counters}.

handle_event(Event, TableId) ->
  try ets:update_counter(TableId, Event, 1) of
      _ok -> {ok, TableId}
  catch
      error:_ -> ets:insert(TableId, {Event, 1}),
                 {ok, TableId}
  end.

get_counters(Pid) ->
  gen_event:call(Pid, counting_handler, get_counters).

handle_call(get_counters, TableId) ->
  {ok, {counting_handler, ets:tab2list(TableId)}, TableId}.

handle_info(_, TableId) ->
  {ok, TableId}.



