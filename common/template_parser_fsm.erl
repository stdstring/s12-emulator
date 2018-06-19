-module(template_parser_fsm).
-behavior(gen_fsm).

-include("parse_defs.hrl").

-export([start/0, process_char/2, finish/1]).
%% export for gen_fsm behavior
-export([continue_parse/2, parse_text/2, parse_escape/2, parse_param/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(parser_state, {template_parts::list(#template_part{}), current_part::string()}).
-record(parser_event, {char::char()}).

start() ->
	{ok, ParserFSM} = gen_fsm:start_link(?MODULE, [], []),
	ParserFSM.

process_char(ParserFSM, Char) -> gen_fsm:send_event(ParserFSM, #parser_event{char = Char}).

finish(ParserFSM) -> gen_fsm:sync_send_all_state_event(ParserFSM, finish_parse_event).

%% export for gen_fsm behavior
init(_Args) -> {ok, continue_parse, #parser_state{template_parts = [], current_part = []}}.

continue_parse(Event, State) when is_record(Event, parser_event) ->
	case Event#parser_event.char of
		$\\ -> {next_state, parse_escape, State};
		${ -> {next_state, parse_param, State};
		_OtherChar -> {next_state, parse_text, #parser_state{template_parts = State#parser_state.template_parts, current_part = [Event#parser_event.char]}}
	end.

parse_text(Event, State) when is_record(Event, parser_event) ->
	case Event#parser_event.char of
		$\\ -> {next_state, parse_escape, State};
		${ ->
			Part = #template_part{part_type = text, part_body = lists:reverse(State#parser_state.current_part)},
			{next_state, parse_param, #parser_state{template_parts = [Part] ++ State#parser_state.template_parts, current_part = []}};
		_OtherChar -> {next_state, parse_text, State#parser_state{current_part = [Event#parser_event.char] ++ State#parser_state.current_part}}
	end.
	
parse_escape(Event, State) when is_record(Event, parser_event) ->
	case Event#parser_event.char of
		$r -> {next_state, parse_text, State#parser_state{current_part = "\r" ++ State#parser_state.current_part}};
		$n -> {next_state, parse_text, State#parser_state{current_part = "\n" ++ State#parser_state.current_part}};
		$s -> {next_state, parse_text, State#parser_state{current_part = "\t" ++ State#parser_state.current_part}};
		$t -> {next_state, parse_text, State#parser_state{current_part = "\t" ++ State#parser_state.current_part}};
		$' -> {next_state, parse_text, State#parser_state{current_part = "'" ++ State#parser_state.current_part}};
		$" -> {next_state, parse_text, State#parser_state{current_part = [$"] ++ State#parser_state.current_part}};
		$\\ -> {next_state, parse_text, State#parser_state{current_part = "\\" ++ State#parser_state.current_part}};
		_OtherChar -> {next_state, parse_text, State#parser_state{current_part = [Event#parser_event.char] ++ State#parser_state.current_part}}
	end.
	
parse_param(Event, State) when is_record(Event, parser_event) ->
	case Event#parser_event.char of
		$\\ -> erlang:error(bad_param);
		${ -> erlang:error(bad_param);
		$} ->
			Part = #template_part{part_type = param, part_body = lists:reverse(State#parser_state.current_part)},
			{next_state, continue_parse, #parser_state{template_parts = [Part] ++ State#parser_state.template_parts, current_part = []}};
		_OtherChar -> {next_state, parse_param, State#parser_state{current_part = [Event#parser_event.char] ++ State#parser_state.current_part}}
	end.
	
handle_sync_event(finish_parse_event, _From, StateName, State) ->
	case StateName of
		continue_parse ->
			{reply, lists:reverse(State#parser_state.template_parts), finish_parse, State};
		parse_text ->
			TextPart = #template_part{part_type = text, part_body = lists:reverse(State#parser_state.current_part)},
			{reply, lists:reverse([TextPart] ++ State#parser_state.template_parts), finish_parse, State};
		parse_escape ->
			TextPart = #template_part{part_type = text, part_body = lists:reverse("\\" ++ State#parser_state.current_part)},
			{reply, lists:reverse([TextPart] ++ State#parser_state.template_parts), finish_parse, State};
		parse_param -> erlang:error(bad_param)
	end.

%% unused export
handle_event(_Event, _StateName, _State) -> ok.
handle_info(_Info, _StateName, _State) -> ok.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, _StateName, _State, _Extra) -> ok.
