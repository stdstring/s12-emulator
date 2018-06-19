-module(telnet_server_worker).

-include("../common/common_defs.hrl").
-include("../common/log_defs.hrl").

%% for proc_lib
-export([start_worker/5, start_worker/6]).

start_worker(ListenSocket, ConnectionLifetime, ConsoleFactory, LogDest, SessionNumber) -> worker_accept(ListenSocket, ConnectionLifetime, ConsoleFactory, LogDest, SessionNumber).
	
start_worker(ListenSocket, ConnectionLifetime, ConsoleFactory, LogDest, SessionNumber, Parent) ->
	proc_lib:init_ack(Parent, {ok, self()}),
	worker_accept(ListenSocket, ConnectionLifetime, ConsoleFactory, LogDest, SessionNumber).

worker_accept(ListenSocket, ConnectionLifetime, ConsoleFactory, LogDest, SessionNumber) ->
	case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
			{ok, {ClientAddress, ClientPort}} = inet:peername(Socket),
			telnet_server:client_connected(#client_descriptor{ip_address = ClientAddress, port = ClientPort}),
			FileName = "Session" ++ integer_to_list(SessionNumber) ++ ".log",
			{ok, Log} = disk_log:open([{name, FileName}, {file, filename:join(LogDest, FileName)}, {format, external}, {type, halt}, {head, logging_helper:log_client_connected(ClientAddress, ClientPort)}]),
			Logger = create_logger(Log),
			Console = ConsoleFactory(),
            CloseReason = worker_read(Socket, ConnectionLifetime, Console, Logger),
			disk_log:blog(Log, logging_helper:log_client_disconnected(ClientAddress, ClientPort, CloseReason)),
			disk_log:close(Log),
			telnet_server:client_disconnected(CloseReason);
        {error, ErrorReason} ->
			erlang:error(ErrorReason)
    end.

worker_read(Socket, ConnectionLifetime, _Console, _Logger) when ConnectionLifetime =< 0 ->
	gen_tcp:close(Socket),
	server_initiative;	
worker_read(Socket, ConnectionLifetime, Console, Logger) ->
	HeadNow = now(),
	receive
		{tcp, _Socket, Data} ->
			(Logger#logger.log_entry_header)("input_command"),
			(Logger#logger.log_entry)(Data),
			(Logger#logger.log_entry_footer)(""),
			#output_command{continue = Continue, output_fun = OutputCommand} = console:input(Console, preprocess_input_string(Data)),
			OutputCommand(fun(OutputString) -> gen_tcp:send(Socket, OutputString) end, Logger),
			inet:setopts(Socket, [{active, once}]),
			if
				Continue == true ->
					worker_read(Socket, miscellaneous_utils:time_rest(ConnectionLifetime, HeadNow, now()), Console, Logger);
				Continue == false ->
					gen_tcp:close(Socket),
					server_initiative
			end;
        {tcp_closed, _Socket} ->
            client_initiative;
		#output_callback{output_fun = OutputCommand} ->
			OutputCommand(fun(OutputString) -> gen_tcp:send(Socket, OutputString) end, Logger),
			worker_read(Socket, miscellaneous_utils:time_rest(ConnectionLifetime, HeadNow, now()), Console, Logger)
	after
		ConnectionLifetime ->
			gen_tcp:close(Socket),
			server_initiative
	end.

%% may be not need in this
preprocess_input_string(InputString) ->
	HasWinEndl = lists:suffix("\r\n", InputString),
	HasUnixEndl = lists:suffix("\n", InputString),
	if
		HasWinEndl == true -> lists:sublist(InputString, 1, length(InputString)-2);
		HasUnixEndl == true -> lists:sublist(InputString, 1, length(InputString)-1);
		true -> InputString
	end.
	
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