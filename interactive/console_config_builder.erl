-module(console_config_builder).

-include("../common/common_defs.hrl").
-include("console_defs.hrl").

-export([console_config/1]).

console_config(ConsoleConfig) ->
	ConsoleConfigSource = miscellaneous_utils:read_term(ConsoleConfig),
	{handshake_message, HandshakeMessage} = lists:keyfind(handshake_message, 1, ConsoleConfigSource),
	{read_mode_prompt, ReadModePrompt} = lists:keyfind(read_mode_prompt, 1, ConsoleConfigSource),
	{edit_mode_prompt, EditModePrompt} = lists:keyfind(edit_mode_prompt, 1, ConsoleConfigSource),
	{edit_mode_message, EditModeMessage} = lists:keyfind(edit_mode_message, 1, ConsoleConfigSource),
	{edit_mode_command, EditModeCommand} = lists:keyfind(edit_mode_command, 1, ConsoleConfigSource),
	{wrong_command_message, WrongCommandMessage} = lists:keyfind(wrong_command_message, 1, ConsoleConfigSource),
	{bye_command, ByeCommand} = lists:keyfind(bye_command, 1, ConsoleConfigSource),
	{bye_message, ByeMessage} = lists:keyfind(bye_message, 1, ConsoleConfigSource),
	{userid_message, UserIDMessage} = lists:keyfind(userid_message, 1, ConsoleConfigSource),
	{pwd_message, PwdMessage} = lists:keyfind(pwd_message, 1, ConsoleConfigSource),
	{login_successful_message, LoginSuccessfulMessage} = lists:keyfind(login_successful_message, 1, ConsoleConfigSource),
	{login_failed_message, LoginFailedMessage} = lists:keyfind(login_failed_message, 1, ConsoleConfigSource),
	#console_settings{handshake_message = HandshakeMessage,
					  read_mode_prompt = ReadModePrompt,
					  edit_mode_prompt = EditModePrompt,
					  edit_mode_message = EditModeMessage,
					  edit_mode_command = EditModeCommand,
					  wrong_command_message = WrongCommandMessage,
					  bye_command = ByeCommand,
					  bye_message = ByeMessage,
					  userid_message = UserIDMessage,
					  pwd_message = PwdMessage,
					  login_successful_message = LoginSuccessfulMessage,
					  login_failed_message = LoginFailedMessage}.