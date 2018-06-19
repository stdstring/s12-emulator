-module(command_parser_builder).

-include("common_defs.hrl").
-include("parse_defs.hrl").

-export([build/1]).

-record(parsing_result, {part_parsing_result::boolean(), command_params = [] ::params_type(), string_rest = [] ::string()}).

build(CommandBody) ->
	TemplateParts = parse_and_check(CommandBody),
	PartParsers = for_each_part(TemplateParts, []),
	fun(InputString) -> parse_all(InputString, PartParsers) end.

parse_all(InputString, PartParsers) -> parse_all(InputString, [], PartParsers).
	
parse_all("", Params, []) -> {true, Params};
parse_all(InputString, Params, [PartParser | PartParsers]) ->
	ParseResult = PartParser(InputString),
	if
		ParseResult#parsing_result.part_parsing_result == true ->
			NewInputString = ParseResult#parsing_result.string_rest,
			NewParams = ParseResult#parsing_result.command_params ++ Params,
			parse_all(NewInputString, NewParams, PartParsers);
		ParseResult#parsing_result.part_parsing_result == false -> {false, []}
	end.
	
parse_and_check(CommandBody) ->
	TemplateParts = template_parser:parse(CommandBody),
	CheckResult = check_command_template(TemplateParts),
	if
		CheckResult == true -> TemplateParts;
		CheckResult == false -> erlang:error(bad_command_template)
	end.

check_command_template([]) -> true;
check_command_template([_H]) -> true;
check_command_template([H1, H2 | OtherParts]) ->
	H1Type = H1#template_part.part_type,
	H2Type = H2#template_part.part_type,
	if
		(H1Type == param) and (H2Type == param) -> false;
		true -> check_command_template([H2] ++ OtherParts)
	end.
	
for_each_part([], []) -> [];
for_each_part([H], PartParsers) ->
	Parser = build_template_part_parser(H),
	lists:reverse([Parser] ++ PartParsers);
for_each_part([H1, H2 | OtherParts], PartParsers) ->
	NextChar = lists:nth(1, H2#template_part.part_body),
	Parser = build_template_part_parser(H1, NextChar),
	for_each_part([H2] ++ OtherParts, [Parser] ++ PartParsers).

build_template_part_parser(#template_part{part_type = param, part_body = PartBody}) ->
	fun(InputString) ->
		#parsing_result{part_parsing_result = true, command_params = [{PartBody, InputString}]}
	end;
	
build_template_part_parser(#template_part{part_type = text, part_body = PartBody}) ->
	fun(InputString) ->
		#parsing_result{part_parsing_result = (PartBody == InputString)}
	end.
	
build_template_part_parser(#template_part{part_type = text, part_body = PartBody}, _NextChar) ->
	fun(InputString) ->
		ParseResult = lists:prefix(PartBody, InputString),
		if
			ParseResult == true -> #parsing_result{part_parsing_result = true, string_rest = lists:sublist(InputString, length(PartBody)+1, length(InputString))};
			ParseResult == false -> #parsing_result{part_parsing_result = false}
		end
	end;

build_template_part_parser(#template_part{part_type = param, part_body = PartBody}, NextChar) ->
	fun(InputString) ->
		ParamValue = lists:takewhile(fun(Char) -> Char /= NextChar end, InputString),
		#parsing_result{part_parsing_result = true, string_rest = lists:sublist(InputString, length(ParamValue)+1, length(InputString)), command_params = [{PartBody, ParamValue}]}
	end.