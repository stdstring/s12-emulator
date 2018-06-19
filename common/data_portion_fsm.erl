-module(data_portion_fsm).
-behavior(gen_fsm).

-export([start/1, get/2]).

%% export for gen_fsm behavior
-export([store/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(data_portion_state, {storage::list(term()), cursor::integer()}).

start(Data) -> gen_fsm:start_link(?MODULE, Data, []).

get(DataStoragePID, PortionSize) -> gen_fsm:sync_send_event(DataStoragePID, PortionSize).

init(Storage) -> {ok, store, #data_portion_state{storage = Storage, cursor = 1}}.

store(PortionSize, _From, #data_portion_state{storage = Storage}) when length(Storage) < PortionSize -> erlang:error(invalid_argument);
store(PortionSize, _From, State) ->
	Storage = State#data_portion_state.storage,
	OldCursor = State#data_portion_state.cursor,
	if
		OldCursor + PortionSize =< length(Storage) ->
			Portion = lists:sublist(Storage, OldCursor, PortionSize),
			{reply, Portion, store, State#data_portion_state{cursor = OldCursor + PortionSize}};
		OldCursor + PortionSize == length(Storage) + 1 ->
			Portion = lists:sublist(Storage, OldCursor, PortionSize),
			{reply, Portion, store, State#data_portion_state{cursor = 1}};
		OldCursor + PortionSize > length(Storage) + 1 ->
			FirstPieceSize = length(Storage)-OldCursor+1,
			SecondPieceSize = PortionSize-FirstPieceSize,
			Portion = lists:sublist(Storage, OldCursor, FirstPieceSize) ++ lists:sublist(Storage, SecondPieceSize),
			{reply, Portion, store, State#data_portion_state{cursor = SecondPieceSize + 1}}
	end.
	
handle_event(reset, _StateName, State) -> {next_state, store, State#data_portion_state{cursor = 1}}.

%% unused export
handle_sync_event(_Event, _From, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_info(_Info, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
terminate(_Reason, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
code_change(_OldVsn, _StateName, _State, _Extra) -> erlang:error(notused_gen_fsm_func).