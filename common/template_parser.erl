-module(template_parser).

-export([parse/1]).

parse(SourceTemplate) ->
	ParserFSM = template_parser_fsm:start(),
	lists:foreach(fun(Char) -> template_parser_fsm:process_char(ParserFSM, Char) end, SourceTemplate),
	template_parser_fsm:finish(ParserFSM).