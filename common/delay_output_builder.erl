-module(delay_output_builder).

-include("log_defs.hrl").

-export([build/2]).

build(PartBody, _ConfigPath) ->
	case io_lib:fread("delay(~d,~d)", PartBody) of
		{ok, [MinDelay, MaxDelay], []} when MinDelay =< MaxDelay ->
			Factory = fun() ->
				fun(_Writer, Logger, _CommandParams, _OutputParams, _RuntimeParams) ->
					(Logger#logger.log_entry_footer)("\r\n"),
					(Logger#logger.log_entry_header)("execute_command"),
					DelayValue = trunc(1000*(MinDelay + random:uniform() * (MaxDelay - MinDelay))),
					(Logger#logger.log_entry)("delay on " ++ integer_to_list(DelayValue) ++ " ms"),
					timer:sleep(DelayValue),
					(Logger#logger.log_entry_footer)("\r\n"),
					(Logger#logger.log_entry_header)("output_command")
				end
			end,
			{true, Factory};
		{ok, [MinDelay, MaxDelay], []} when MinDelay > MaxDelay -> erlang:error(incorrect_delay_range);
		_Other -> {false, nothing}
	end.