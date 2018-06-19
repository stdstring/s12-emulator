-record(client_descriptor, {ip_address = {} ::tuple(), port = -1 ::integer()}).
-record(server_state, {session_number::integer(), listen_socket::term(), workers::list({pid(), #client_descriptor{}}), worker_limit::integer(), worker_factory::fun((integer()) -> term), log::term()}).

-type params_type()::list({string(), term()}).
-record(input_command, {command_type::atom(), command_params::params_type()}).

-type input_parser_fun()::fun((string()) -> #input_command{}).
%%-type output_builder(Key, Val)::dictionary().
-record(command_settings, {input_parser::input_parser_fun(), output_factory}).

-record(output_command, {continue::boolean(), output_fun::fun((term(), term()) -> term())}).
-record(output_callback, {output_fun::fun((term(), term()) -> term())}).