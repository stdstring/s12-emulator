-module(dataportion_output_builder).

-export([build/2]).

-record(datasource_params, {filename::string(), portion_size::integer()}).

% PartBody = datasource("filename",data_portion_size); e.c. datasource("file.dat",11)
build(PartBody, ConfigPath) ->
	Prefix = "datasource(",
	Suffix = ")",
	HasPrefix = lists:prefix(Prefix, PartBody),
	HasSuffix = lists:suffix(Suffix, PartBody),
	if
		(HasPrefix == true) and (HasSuffix == true) ->
			Params = extract_params(string:sub_string(PartBody, length(Prefix)+1, length(PartBody)-length(Suffix))),
			StringData = string:tokens(miscellaneous_utils:read_string_data(filename:join(ConfigPath, Params#datasource_params.filename)), "\r\n"),
			InternalOutputFactories = lists:map(fun(String) -> output_builder:build(String ++ "\r\n", ConfigPath) end, StringData),
			Factory = fun() -> create_output_command(Params, InternalOutputFactories) end,
			{true, Factory};
		true ->  {false, nothing}
	end.
	
create_output_command(Params, InternalOutputFactories) ->
	InternalOutput = lists:map(fun(Factory) -> Factory() end, InternalOutputFactories),
	{ok, DataStorageFSM} = data_portion_fsm:start(InternalOutput),
	fun(Writer, Logger, CommandParams, OutputParams, RuntimeParams) ->
		Portion = data_portion_fsm:get(DataStorageFSM, Params#datasource_params.portion_size),
		lists:foreach(fun(DataItemCommand) -> DataItemCommand(Writer, Logger, CommandParams, OutputParams, RuntimeParams) end, Portion)
	end.

extract_params(ParamsString) ->
	FirstQuotes = string:chr(ParamsString, $"),
	LastQuotes = string:rchr(ParamsString, $"),
	Filename = string:sub_string(ParamsString, FirstQuotes+1, LastQuotes-1),
	{ok, [PortionSize], []} = io_lib:fread(",~d", string:sub_string(ParamsString, LastQuotes+1)),
	#datasource_params{filename = Filename, portion_size = PortionSize}.