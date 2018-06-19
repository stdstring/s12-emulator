-module(console).

-include("../common/common_defs.hrl").
-include("console_defs.hrl").

-export([start/3, input/2]).
-export([init/1]).

start(ConsoleConfig, CommandConfig, RuntimeConfig) -> proc_lib:start_link(?MODULE, init, [{ConsoleConfig, CommandConfig, RuntimeConfig, self()}]).

input(ConsolePID, InputCommand) -> gen_fsm:sync_send_event(ConsolePID, #console_event{command = InputCommand}).

init({ConsoleConfig, CommandConfig, RuntimeConfig, MasterPID}) ->
	proc_lib:init_ack(MasterPID, {ok, self()}),
	{ok, LogicPID} = logic_fsm:start(CommandConfig),
	{session_timeout, SessionTimeout} = lists:keyfind(session_timeout, 1, RuntimeConfig),
	{editmode_timeout, EditmodeTimeout} = lists:keyfind(editmode_timeout, 1, RuntimeConfig),
	ConsoleState = #console_state{console_settings = ConsoleConfig, logic_fsm = LogicPID, editmode_timeout = EditmodeTimeout, session_timeout = SessionTimeout, master = MasterPID},
	gen_fsm:enter_loop(console_fsm, [], handshake_mode, ConsoleState).
