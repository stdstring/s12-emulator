-module(config_builder).

-include("common_defs.hrl").

-export([input_command_config/1, output_command_config/2, read_config/1]).

input_command_config(InputCommandConfig) ->
	ConfigPath = filename:dirname(InputCommandConfig),
	InputCommandSource = miscellaneous_utils:read_term(InputCommandConfig),
	InputCommands = lists:map(fun({CommandType, CommandBodyFile}) ->
			CommandBody = miscellaneous_utils:read_string_data(filename:join(ConfigPath, CommandBodyFile)),
			CommandParser = command_parser_builder:build(CommandBody),
			{CommandType, CommandParser}
		end, InputCommandSource),
	fun(InputString) ->
		UnknownCommand = #input_command{command_type = unknown_command, command_params = []},
		find_command(InputString, InputCommands, UnknownCommand)
	end.

output_command_config(OutputCommandConfig, OutputBehaviorConfig) ->
	ConfigPath = filename:dirname(OutputCommandConfig),
	OutputCommandSource = miscellaneous_utils:read_term(OutputCommandConfig),
	OutputBehaviorSource = miscellaneous_utils:read_term(OutputBehaviorConfig),
	OutputBehaviorFactory = lists:map(fun({CommandType, Behaviors}) ->
		{CommandType, lists:map(fun(Behaviour) -> create_output_command(ConfigPath, OutputCommandSource, Behaviour) end, Behaviors)}
	end, OutputBehaviorSource),
	OutputBehaviorFactory.

read_config(Config) -> miscellaneous_utils:read_term(Config).

find_command(_InputString, [], UnknownCommand) -> UnknownCommand;
find_command(InputString, [{CommandType, CommandParser} | Commands], UnknownCommand) ->
	{ParseResult, Params} = CommandParser(InputString),
	if
		ParseResult == true -> #input_command{command_type = CommandType, command_params = Params};
		ParseResult == false -> find_command(InputString, Commands, UnknownCommand)
	end.

create_output_command(ConfigPath, OutputCommandSource, {OutputType, Weight, OutputParams}) ->
	Search = lists:keyfind(OutputType, 1, OutputCommandSource),
	if
		Search == false -> erlang:error(bad_behavior);
		true ->
			OutputCommandBodyFile = element(2, Search),
			CommandBody = miscellaneous_utils:read_string_data(filename:join(ConfigPath, OutputCommandBodyFile)),
			Factory = output_builder:build(CommandBody, ConfigPath),
			fun() ->
				OutputCommand = Factory(),
				lists:duplicate(Weight, fun(Writer, Logger, CommandParams, RuntimeParams) -> OutputCommand(Writer, Logger, CommandParams, OutputParams, RuntimeParams) end)
			end
	end.