-module(coffee_fsm).
-behaviour(gen_fsm).

-export([start_link/0, stop/0]).
-export([init/1, terminate/3, handle_event/3, handle_sync_event/4]). % callback functions
-export([selection/2, payment/2, remove/2]).    % states
-export([americano/0, cappuccino/0, tea/0,      % client functions
         espresso/0, pay/1, cancel/0, cup_removed/0]).

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  hardware:reboot(),
  hardware:display("Make Your Selection", []),
  process_flag(trap_exit, true),
  {ok, selection, []}.

stop() -> 
  gen_fsm:sync_send_all_state_event(?MODULE, stop).

handle_event(stop, _State, LoopData) ->
  {stop, normal, LoopData}.

handle_sync_event(stop, _From, _State, LoopData) ->
  {stop, normal, LoopData}.

terminate(_Reason, payment, {_Type, _Price, Paid}) ->
  hardware:return_change(Paid);
terminate(_Reason, _StateName, _LoopData) ->
  ok.

%% client functions

tea()         -> gen_fsm:send_event(?MODULE, {selection, tea, 100}).
espresso()    -> gen_fsm:send_event(?MODULE, {selection, espresso, 100}).
americano()   -> gen_fsm:send_event(?MODULE, {selection, americano, 100}).
cappuccino()  -> gen_fsm:send_event(?MODULE, {selection, cappuccino, 100}).

pay(Coin)     -> gen_fsm:send_event(?MODULE, {pay, Coin}).
cancel()      -> gen_fsm:send_event(?MODULE, cancel).
cup_removed() -> gen_fsm:send_event(?MODULE, cup_removed).

%% states

selection({selection, Type, Price}, _LoopData) ->
  hardware:display("Please pay:~w", [Price]),
  {next_state, payment, {Type, Price, 0}};
selection({pay, Coin}, LoopData) ->
  hardware:return_change(Coin),
  {next_state, selection, LoopData};
selection(_Other, LoopData) ->
  {next_state, selection, LoopData}.

payment({pay, Coin}, {Type, Price, Paid}) when Coin+Paid < Price ->
  NewPaid = Coin + Paid,
  hardware:display("Please pay:~w", [Price - NewPaid]),
  {next_state, payment, {Type, Price, NewPaid}};
payment({pay, Coin}, {Type, Price, Paid}) when Coin+Paid >= Price ->
  NewPaid = Coin + Paid,
  hardware:display("Preparing Drink.", []),
  hardware:return_change(NewPaid - Price),
  hardware:drop_cup(), hardware:prepare(Type),
  hardware:display("Remove Drink.", []),
  {next_state, remove, null};
payment(cancel, {_Type, _Price, Paid}) ->
  hardware:display("Make your selection", []),
  hardware:return_change(Paid),
  {next_state, selection, null};
payment(_Other, LoopData) ->
  {next_state, payment, LoopData}.

remove(cup_removed, LoopData) ->
  hardware:display("Make Your Selection", []),
  {next_state, selection, LoopData};
remove({pay, Coin}, LoopData) ->
  hardware:return_change(Coin),
  {next_state, remove, LoopData};
remove(_Other, LoopData) ->
  {next_state, remove, LoopData}.


