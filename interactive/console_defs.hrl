-record(console_settings, {handshake_message::string(),
						   read_mode_prompt::string(),
						   edit_mode_prompt::string(),
						   edit_mode_message::string(),
						   edit_mode_command::string(),
						   wrong_command_message::string(),
						   bye_command::string(),
						   bye_message::string(),
						   userid_message::string(),
						   pwd_message::string(),
						   login_successful_message::string(),
						   login_failed_message::string()}).

-record(login_result, {result:: 'good' | 'bad', reason = "" ::string()}).
-record(session, {userid::string()}).

-record(console_state, {console_settings::#console_settings{}, logic_fsm::pid(), master::pid(), editmode_timeout::integer(), session_timeout::integer(), params = [] ::params_type(), session = null ::#session{}}).
-record(console_event, {command::string()}).