-module(coffee).
-export([tea/0, espresso/0, americano/0, cappuccino/0, pay/1, cup_removed/0, cancel/0]).
-export([start_link/0, init/0]).

start_link() ->
  {ok, spawn_link(?MODULE, init, [])}.

init() ->
  register(?MODULE, self()),
  hardware:reboot(),
  hardware:display("Make Your Selection", []),
  selection().

%% Client Functions for Drink Selections

tea()         -> ?MODULE ! {selection, tea,         100}.
espresso()    -> ?MODULE ! {selection, espresso,    150}.
americano()   -> ?MODULE ! {selection, americano,   100}.
cappuccino()  -> ?MODULE ! {selection, cappuccino,  150}.

%% Client Functions for Actions

cup_removed() -> ?MODULE ! cup_removed.
pay(Coin)     -> ?MODULE ! {pay, Coin}.
cancel()      -> ?MODULE ! cancel.

%% State: drink selection

selection() ->
  receive
    {selection, Type, Price} ->
      hardware:display("Please pay:~w", [Price]),
      payment(Type, Price, 0);
    {pay, Coin} ->
      hardware:return_change(Coin),
      selection();
    _Other -> % cancel
      selection()
  end.

%% State: payment

payment(Type, Price, Paid) ->
  receive
    {pay, Coin} ->
      if
        Coin + Paid >= Price ->
          hardware:display("Preparing Drink.", []),
          hardware:return_change(Coin + Paid - Price),
          hardware:drop_cup(), hardware:prepare(Type),
          hardware:display("Remove Drink>", []),
          remove();
        true ->
          ToPay = Price - (Coin + Paid),
          hardware:display("Please pay:~w", [ToPay]),
          payment(Type, Price, Coin + Paid)
      end;
    cancel ->
      hardware:display("Make Your Selection", []),
      hardware:return_change(Paid),
      selection();
    _Other -> %selection
      payment(Type, Price, Paid)
  end.

%% State: remove cup

remove() ->
  receive
    cup_removed ->
      hardware:display("Make Your Selection", []),
      selection();
    {pay, Coin} ->
        hardware:return_change(Coin),
        remove();
    _Other -> % cancel / selection
      remove()
  end.
