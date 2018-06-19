-module(console_fsm).
-behavior(gen_fsm).

-include("../common/common_defs.hrl").
-include("console_defs.hrl").
-include("../common/log_defs.hrl").

%% export for gen_fsm behavior
-export([handshake_mode/3, read_mode/3, trusted_read_mode/2, trusted_read_mode/3, edit_mode/2, edit_mode/3, bye_mode/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

handshake_mode(_Event, _From, State) ->
	OutputCommand = fun(Writer, _Logger) -> console_message:handshake_message(Writer, State, _Logger) end,
	{reply, #output_command{continue = true, output_fun = OutputCommand}, read_mode, State}.

read_mode(#console_event{command = ""}, _From, State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:read_mode_prompt(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	{reply, #output_command{continue = true, output_fun = OutputCommand}, read_mode, State};
read_mode(#console_event{command = InputCommand}, From, State) ->
	case recognize_command(InputCommand, State#console_state.console_settings) of
		edit_mode_command -> process_edit_mode_command(From, State);
		bye_command -> process_bye_command(State);
		wrong_command -> process_wrong_command(State);
		_Other -> erlang:error(unsupported_console_command)
	end.

trusted_read_mode(timeout, State) -> {next_state, read_mode, State#console_state{session = null}}.

trusted_read_mode(#console_event{command = ""}, _From, State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:read_mode_prompt(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	{reply, #output_command{continue = true, output_fun = OutputCommand}, trusted_read_mode, State};
trusted_read_mode(#console_event{command = InputCommand}, From, State) ->
	case recognize_command(InputCommand, State#console_state.console_settings) of
		edit_mode_command -> process_edit_mode_command(From, State);
		bye_command -> process_bye_command(State);
		wrong_command -> process_wrong_command(State);
		_Other -> erlang:error(unsupported_console_command)
	end.

edit_mode(timeout, State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:read_mode_prompt(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	State#console_state.master!#output_callback{output_fun = OutputCommand},
	RestTimeout = miscellaneous_utils:timeout_delta(State#console_state.session_timeout, State#console_state.editmode_timeout),
	{next_state, trusted_read_mode, State, RestTimeout}.

edit_mode(#console_event{command = ""}, _From, State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:edit_mode_prompt(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	{reply, #output_command{continue = true, output_fun = OutputCommand}, edit_mode, State, State#console_state.editmode_timeout};
edit_mode(#console_event{command = InputCommand}, _From, State) ->
	Command = logic_fsm:process(State#console_state.logic_fsm, InputCommand),
	%% pass command to logic_fsm
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		Command(Writer, Logger),
		console_message:read_mode_prompt(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	{reply, #output_command{continue = true, output_fun = OutputCommand}, read_mode, State}.

bye_mode(_Event, _From, _State) -> erlang:error(console_terminal_state).

%% unused export
init(_Args) -> erlang:error(notused_gen_fsm_func).
handle_event(_Event, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_sync_event(_Event, _From, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_info(_Info, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
terminate(_Reason, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
code_change(_OldVsn, _StateName, _State, _Extra) -> erlang:error(notused_gen_fsm_func).

%% process funcs
process_edit_mode_command(From, State) ->
	Session = State#console_state.session,
	if
		Session /= null ->
			OutputCommand = fun(Writer, Logger) ->
				(Logger#logger.log_entry_header)("output_command"),
				console_message:edit_mode_message(Writer, State, Logger),
				console_message:edit_mode_prompt(Writer, State, Logger),
				(Logger#logger.log_entry_footer)("\r\n")
			end,
			{reply, #output_command{continue = true, output_fun = OutputCommand}, edit_mode, State, State#console_state.editmode_timeout};
		Session == null ->
			login_fsm:input_userid(From, State)
	end.

process_bye_command(State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:bye_message(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	{reply, #output_command{continue = false, output_fun = OutputCommand}, bye_mode, State}.

process_wrong_command(State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:wrong_command_message(Writer, State, Logger),
		console_message:read_mode_prompt(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	Session = State#console_state.session,
	if
		Session /= null ->
			{reply, #output_command{continue = true, output_fun = OutputCommand}, trusted_read_mode, State, State#console_state.session_timeout};
		Session == null ->
			{reply, #output_command{continue = true, output_fun = OutputCommand}, read_mode, State}
	end.

%% addition funcs
recognize_command(InputString, ConsoleSettings) ->
	EditModeCommand = ConsoleSettings#console_settings.edit_mode_command,
	ByeCommand = ConsoleSettings#console_settings.bye_command,
	case string:to_lower(InputString) of
		EditModeCommand -> edit_mode_command;
		ByeCommand -> bye_command;
		_Other -> wrong_command
	end.