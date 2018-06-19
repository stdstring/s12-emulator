-module(login_fsm).
-behavior(gen_fsm).

-include("../common/common_defs.hrl").
-include("console_defs.hrl").
-include("../common/log_defs.hrl").

%% export for gen_fsm behavior
-export([input_userid/2, input_pwd/3, process_login/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

input_userid(From, State) ->
	OutputCommand = fun(Writer, Logger) ->
				(Logger#logger.log_entry_header)("output_command"),
				console_message:userid_message(Writer, State, Logger),
				(Logger#logger.log_entry_footer)("\r\n")
			end,
	gen_fsm:reply(From, #output_command{continue = true, output_fun = OutputCommand}),
	gen_fsm:enter_loop(?MODULE, [], input_pwd, State).

input_pwd(#console_event{command = InputCommand}, _From, State) ->
	OutputCommand = fun(Writer, Logger) ->
		(Logger#logger.log_entry_header)("output_command"),
		console_message:pwd_message(Writer, State, Logger),
		(Logger#logger.log_entry_footer)("\r\n")
	end,
	{reply, #output_command{continue = true, output_fun = OutputCommand}, process_login, State#console_state{params = [{"UserID", InputCommand}]}}.

process_login(#console_event{command = InputCommand}, From, State) ->
	{"UserID", UserID} = lists:keyfind("UserID", 1, State#console_state.params),
	Pwd = InputCommand,
	#login_result{result = LoginResult} = auth_logic:login(UserID, Pwd),
	if
		LoginResult == true ->
			OutputCommand = fun(Writer, Logger) ->
				(Logger#logger.log_entry_header)("output_command"),
				console_message:login_successful_message(Writer, State, Logger),
				console_message:edit_mode_prompt(Writer, State, Logger),
				(Logger#logger.log_entry_footer)("\r\n")
			end,
			gen_fsm:reply(From, #output_command{continue = true, output_fun = OutputCommand}),
			gen_fsm:enter_loop(console_fsm, [], edit_mode, State#console_state{params = [], session = #session{userid = UserID}}, State#console_state.editmode_timeout);
		LoginResult == false ->
			OutputCommand = fun(Writer, Logger) ->
				(Logger#logger.log_entry_header)("output_command"),
				console_message:login_failed_message(Writer, State, Logger),
				console_message:read_mode_prompt(Writer, State, Logger),
				(Logger#logger.log_entry_footer)("\r\n")
			end,
			gen_fsm:reply(From, #output_command{continue = true, output_fun = OutputCommand}),
			gen_fsm:enter_loop(console_fsm, [], read_mode, State#console_state{params = [], session = null})
	end.

%% unused export
init(_Args) -> erlang:error(notused_gen_fsm_func).	
handle_event(_Event, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_sync_event(_Event, _From, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
handle_info(_Info, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
terminate(_Reason, _StateName, _State) -> erlang:error(notused_gen_fsm_func).
code_change(_OldVsn, _StateName, _State, _Extra) -> erlang:error(notused_gen_fsm_func).