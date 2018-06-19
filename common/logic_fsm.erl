-module(logic_fsm).
-behavior(gen_fsm).

-include("common_defs.hrl").

-export([start/1, process/2]).
-export([wait_command/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(logic_state, {input_parser, output_factory, output, output_state = none :: 'none' | term()}).
-record(logic_event, {command::list(byte())}).

start(CommandSettings) -> gen_fsm:start_link(?MODULE, CommandSettings, []).

process(LogicPID, InputCommand) -> gen_fsm:sync_send_event(LogicPID, #logic_event{command = InputCommand}).

init(CommandSettings) ->
	InputParser = CommandSettings#command_settings.input_parser,
	OutputFactory = CommandSettings#command_settings.output_factory,
	State = init_output_state(#logic_state{input_parser = InputParser, output_factory = OutputFactory}),
	{ok, wait_command, State}.

wait_command(#logic_event{command = EventCommand}, _From, State) ->
	InputCommandParser = State#logic_state.input_parser,
	InputCommand = InputCommandParser(EventCommand),
	OutputCommand = get_output_command(InputCommand#input_command.command_type, State#logic_state.output, State#logic_state.output_state),
	NewOutputState = update_output_state(InputCommand#input_command.command_type, State#logic_state.output, State#logic_state.output_state),
	PreparedCommand = fun(Writer, Logger) ->
		OutputCommand(Writer, Logger, InputCommand#input_command.command_params, [{"Timestamp", erlang:universaltime(), now()}])
	end,
	{reply, PreparedCommand, wait_command, State#logic_state{output_state = NewOutputState}}.

%% unused export
handle_event(_Event, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_sync_event(_Event, _From, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_info(_Info, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
terminate(_Reason, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
code_change(_OldVsn, _StateName, _State, _Extra) -> erlang:error(notused_gen_fsm_func).

%% working with state
init_output_state(State) ->
	Output = lists:map(fun({CommandType, FactoryList}) -> {CommandType, lists:flatmap(fun(Factory) -> Factory() end, FactoryList)} end, State#logic_state.output_factory),
	OutputState = lists:map(fun({Key, _Value}) -> {Key, 1} end, Output),
	State#logic_state{output_state = OutputState, output = Output}.

update_output_state(CommandType, Output, OutputState) ->
	{CommandType, OutputCommands} = lists:keyfind(CommandType, 1, Output),
	{CommandType, CommandState} = lists:keyfind(CommandType, 1, OutputState),
	if
		CommandState == length(OutputCommands) -> lists:keystore(CommandType, 1, OutputState, {CommandType, 1});
		CommandState < length(OutputCommands) -> lists:keystore(CommandType, 1, OutputState, {CommandType, CommandState+1})
	end.

get_output_command(CommandType, Output, OutputState) ->	
	{CommandType, OutputCommands} = lists:keyfind(CommandType, 1, Output),
	{CommandType, CommandState} = lists:keyfind(CommandType, 1, OutputState),
	lists:nth(CommandState, OutputCommands).