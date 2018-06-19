-module(output_builder).

-include("parse_defs.hrl").
-include("log_defs.hrl").

-export([build/2]).

build(CommandBody, ConfigPath) ->
	{A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
	TemplatesPart =  template_parser:parse(CommandBody),
	PartParserFactories = lists:map(fun(Part) -> build_template_part_parser(Part, ConfigPath) end, TemplatesPart),
	fun() ->
		PartParsers = lists:map(fun(PartParserFactory) -> PartParserFactory() end, PartParserFactories),
		fun(Writer, Logger, CommandParams, OutputParams, RuntimeParams) ->
			lists:foreach(fun(PartParser) -> PartParser(Writer, Logger, CommandParams, OutputParams, RuntimeParams) end, PartParsers)
		end
	end.
	
build_template_part_parser(#template_part{part_type = param, part_body = PartBody}, ConfigPath) ->
	search_suitable_output([fun dataportion_output_builder:build/2, fun delay_output_builder:build/2, fun timestamp_output_builder:build/2, fun param_output_builder:build/2], PartBody, ConfigPath);
build_template_part_parser(#template_part{part_type = text, part_body = PartBody}, _ConfigPath) ->
	fun() ->
		fun(Writer, Logger, _CommandParams, _OutputParams, _RuntimeParams) ->
			(Logger#logger.log_entry)(PartBody),
			Writer(PartBody)
		end
	end.
	
search_suitable_output([], _PartBody, _ConfigPath) -> erlang:error(unexpected_output_builder_behavior);
search_suitable_output([Factory | Others], PartBody, ConfigPath) ->
	case Factory(PartBody, ConfigPath) of
		{true, PartOutputFactory} -> PartOutputFactory;
		{false, _Something} -> search_suitable_output(Others, PartBody, ConfigPath)
	end.