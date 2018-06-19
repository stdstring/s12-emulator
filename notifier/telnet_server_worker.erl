-module(telnet_server_worker).

-include("../common/common_defs.hrl").
-include("../common/log_defs.hrl").

%% for proc_lib
-export([start_worker/4, start_worker/5]).

start_worker(ListenSocket, ConsoleFactory, LogDest, SessionNumber) -> worker_accept(ListenSocket, ConsoleFactory, LogDest, SessionNumber).

start_worker(ListenSocket, ConsoleFactory, LogDest, SessionNumber, Parent) ->
	proc_lib:init_ack(Parent, {ok, self()}),
	worker_accept(ListenSocket, ConsoleFactory, LogDest, SessionNumber).

worker_accept(ListenSocket, ConsoleFactory, LogDest, SessionNumber) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			{ok, {ClientAddress, ClientPort}} = inet:peername(Socket),
			telnet_server:client_connected(#client_descriptor{ip_address = ClientAddress, port = ClientPort}),
			FileName = "Session" ++ integer_to_list(SessionNumber) ++ ".log",
			{ok, Log} = disk_log:open([{name, FileName}, {file, filename:join(LogDest, FileName)}, {format, external}, {type, halt}, {head, logging_helper:log_client_connected(ClientAddress, ClientPort)}]),
			Logger = create_logger(Log),
			Console = ConsoleFactory(),
			CloseReason = worker_process(Socket, Console, Logger),
			disk_log:blog(Log, logging_helper:log_client_disconnected(ClientAddress, ClientPort, CloseReason)),
			disk_log:close(Log),
			telnet_server:client_disconnected(CloseReason);
		{error, ErrorReason} ->
			erlang:error(ErrorReason)
	end.

worker_process(Socket, Console, Logger) ->
	receive
		{tcp_closed, Socket} -> client_initiative;
		_Other -> can_work
	after
		0 -> can_work
	end,
	OutputCommand = console:input(Console),
	OutputCommand(fun(OutputString) -> gen_tcp:send(Socket, OutputString) end, Logger),
	inet:setopts(Socket, [{active, once}]),
	worker_process(Socket, Console, Logger).

%% may be not need in this
create_logger(Log) ->
	LogEntryHeaderFun = fun(Header) ->
		TimeStamp = logging_helper:get_timestamp(),
		disk_log:blog(Log, "[" ++ TimeStamp ++ "] [" ++ Header ++ "] ")
	end,
	LogEntryFun = fun(Body) ->
		disk_log:blog(Log, Body)
	end,
	LogEntryFooterFun = fun(Footer) ->
		disk_log:blog(Log, Footer)
	end,
	#logger{log_entry_header = LogEntryHeaderFun, log_entry = LogEntryFun, log_entry_footer = LogEntryFooterFun}.