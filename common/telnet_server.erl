-module(telnet_server).
-behavior(gen_server).

-include("common_defs.hrl").

-export([start/3, stop/0]).
-export([client_connected/1, client_disconnected/1]).
%% export for gen_server behavior
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).

%% start(PortNumber, WorkerLimit) -> gen_server:start_link({local, telnet_server}, telnet_server, [PortNumber, WorkerLimit], []).
start(PortNumber, WorkerLimit, ServerInitializer) ->
	gen_server:start_link({local, telnet_server}, telnet_server, [PortNumber, WorkerLimit, ServerInitializer], []).

stop() -> gen_server:cast(telnet_server, stop).

client_connected(ClientDescriptor) -> gen_server:call(telnet_server, {client_connected, ClientDescriptor}).

client_disconnected(Reason) -> gen_server:call(telnet_server, {client_disconnected, Reason}).

%% export for gen_server behavior
init([PortNumber, WorkerLimit, ServerInitializer]) -> {ok, ServerInitializer(PortNumber, WorkerLimit)}.

handle_call({client_connected, ClientDescriptor}, {Worker, _Tag}, State) ->
	log_client_connected(State#server_state.log, ClientDescriptor),
	Workers = State#server_state.workers,
	UpdatedWorkers = lists:keystore(Worker, 1, Workers, {Worker, ClientDescriptor}),
	NewState = process_workers_change(UpdatedWorkers, State),
	{reply, ok, NewState};
handle_call({client_disconnected, Reason}, {Worker, _Tag}, State) ->
	Workers = State#server_state.workers,
	{Worker, ClientDescriptor} = lists:keyfind(Worker, 1, Workers),
	log_client_disconnected(State#server_state.log, ClientDescriptor, Reason),
	UpdatedWorkers = lists:keydelete(Worker, 1, Workers),
	NewState = process_workers_change(UpdatedWorkers, State),
	{reply, ok, NewState}.

handle_info({'EXIT', _Worker, normal}, State) -> {noreply, State};
handle_info({'EXIT', Worker, Reason}, State) -> 
	Workers = State#server_state.workers,
	{Worker, ClientDescriptor} = lists:keyfind(Worker, 1, Workers),
	log_client_failed(State#server_state.log, ClientDescriptor, Reason),
	UpdatedWorkers = lists:keydelete(Worker, 1, Workers),
	{noreply, process_workers_change(UpdatedWorkers, State)}.

handle_cast(stop, State) -> {stop, shutdown, State}.

terminate(_Reason, State) ->
	Log = State#server_state.log,
	Workers = State#server_state.workers,
	lists:foreach(fun({_Worker, ClientDescriptor}) -> log_client_disconnected(Log, ClientDescriptor, server_stopped) end, Workers),
	disk_log:blog(Log, logging_helper:log_server_stopped()),
	disk_log:close(Log),
	ok.

%% unused export
code_change(_OldVsn, _State, _Extra) -> erlang:error(notused_gen_server_func).
format_status(_Opt, [_PDict, _State]) -> erlang:error(notused_gen_server_func).

process_workers_change(UpdatedWorkers, State) when length(UpdatedWorkers) < State#server_state.worker_limit ->
	NewSessionNumber = State#server_state.session_number + 1,
	{ok, NewWorker} = (State#server_state.worker_factory)(NewSessionNumber),
	State#server_state{session_number = NewSessionNumber, workers = UpdatedWorkers ++ [{NewWorker, #client_descriptor{}}]};
process_workers_change(UpdatedWorkers, State) when length(UpdatedWorkers) == State#server_state.worker_limit ->
	State#server_state{workers = UpdatedWorkers}.

log_client_connected(Log, #client_descriptor{ip_address = Address, port = Port})  ->
	disk_log:blog(Log, logging_helper:log_client_connected(Address, Port)).
	
log_client_disconnected(_Log, #client_descriptor{ip_address = {}, port = -1}, _Reason) -> nothing;
log_client_disconnected(Log, #client_descriptor{ip_address = Address, port = Port}, Reason) ->
	disk_log:blog(Log, logging_helper:log_client_disconnected(Address, Port, Reason)).

log_client_failed(_Log, #client_descriptor{ip_address = {}, port = -1}, _Reason) -> nothing;
log_client_failed(Log, #client_descriptor{ip_address = Address, port = Port}, Reason) ->
	disk_log:blog(Log, logging_helper:log_client_failed(Address, Port, Reason)).