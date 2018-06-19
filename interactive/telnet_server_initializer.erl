-module(telnet_server_initializer).

-include("../common/common_defs.hrl").

-export([init/2]).

init(PortNumber, WorkerLimit) ->
	process_flag(trap_exit, true),
	%% logging
	LogDest = filename:join("log", logging_helper:get_current_datetime("_", "_", "_")),
	file:make_dir(LogDest),
	{ok, Log} = disk_log:open([{name, server_log}, {file, filename:join("log", "common.log")}, {format, external}, {type, halt}]),
	disk_log:blog(Log, logging_helper:log_server_started()),
	%% console factory
	InputParser = config_builder:input_command_config("config/input.def"),
	OutputFactory = config_builder:output_command_config("config/output.def", "config/behavior.def"),
	ConsoleConfig = console_config_builder:console_config("config/console.def"),
	RuntimeConfig = config_builder:read_config("config/runtime.def"),
	ConsoleFactory = fun() -> init_console(InputParser, OutputFactory, ConsoleConfig, RuntimeConfig) end,
	%% listen socket
	{ok, ListenSocket} = gen_tcp:listen(PortNumber, [list, {active, once}, {packet, line}]),
	%% worker factory
	{connection_lifetime, ConnectionLifetime} = lists:keyfind(connection_lifetime, 1, RuntimeConfig),
	WorkerFactory = fun(SessionNumber) -> proc_lib:start_link(telnet_server_worker, start_worker, [ListenSocket, ConnectionLifetime, ConsoleFactory, LogDest, SessionNumber, self()]) end,
	%% waiting worker
	SessionNumber = 1,
	{ok, Worker} = WorkerFactory(SessionNumber),
	#server_state{session_number = SessionNumber, listen_socket = ListenSocket, workers = [{Worker, #client_descriptor{}}], worker_limit = WorkerLimit, worker_factory = WorkerFactory, log = Log}.

init_console(InputParser, OutputFactory, ConsoleConfig, RuntimeConfig) ->
	CommandConfig = #command_settings{input_parser = InputParser, output_factory = OutputFactory},
	{ok, ConsolePID} = console:start(ConsoleConfig, CommandConfig, RuntimeConfig),
	ConsolePID.