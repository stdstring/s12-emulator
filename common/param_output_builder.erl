-module(param_output_builder).

-include("log_defs.hrl").

-export([build/2]).

build(PartBody, _ConfigPath) ->
	Factory = fun() ->
		fun(Writer, Logger, CommandParams, OutputParams, _RuntimeParams) ->
			{SearchResult, Value} = find_param(PartBody, OutputParams ++ CommandParams),
			if
				SearchResult == true ->
					(Logger#logger.log_entry)(Value),
					Writer(Value);
				SearchResult == false -> erlang:error(param_not_found)
			end
		end
	end,
	{true, Factory}.

find_param(ParamName, ParamsStorage) ->
	Search = lists:keyfind(ParamName, 1, ParamsStorage),
	if
		Search == false -> {false, {}};
		Search /= false -> {true, element(2, Search)}
	end.