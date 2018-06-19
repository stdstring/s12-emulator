-module(console_message).

-include("../common/common_defs.hrl").
-include("console_defs.hrl").
-include("../common/log_defs.hrl").

-export([handshake_message/3, read_mode_prompt/3, edit_mode_message/3, edit_mode_prompt/3, wrong_command_message/3, bye_message/3, userid_message/3, pwd_message/3, login_successful_message/3, login_failed_message/3]).

handshake_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.handshake_message, Logger),
	write_message(Writer, ConsoleSettings#console_settings.read_mode_prompt, Logger).
	
read_mode_prompt(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.read_mode_prompt, Logger).
	
edit_mode_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.edit_mode_message, Logger).
	
edit_mode_prompt(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.edit_mode_prompt, Logger).
	
wrong_command_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.wrong_command_message, Logger).
	
bye_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.bye_message, Logger).
	
userid_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.userid_message, Logger).
	
pwd_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.pwd_message, Logger).
	
login_successful_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.login_successful_message, Logger).
	
login_failed_message(Writer, #console_state{console_settings = ConsoleSettings}, Logger) ->
	write_message(Writer, ConsoleSettings#console_settings.login_failed_message, Logger).
	
write_message(Writer, Message, Logger) ->
	(Logger#logger.log_entry)(Message),
	Writer(Message).