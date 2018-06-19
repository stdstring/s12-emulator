-module(timestamp_output_builder).

-include("log_defs.hrl").

-export([build/2]).

-record(date_time, {year :: 0..9999 , month :: 1..12, day :: 1..31, hour :: 0..23, minute :: 0..59, second :: 0..59, millisecond :: 0..999}).

build(PartBody, _ConfigPath) ->
	Prefix = "timestamp(",
	Suffix = ")",
	HasPrefix = lists:prefix(Prefix, PartBody),
	HasSuffix = lists:suffix(Suffix, PartBody),
	if
		(HasPrefix == true) and (HasSuffix == true) ->
			FormatString = string:sub_string(PartBody, length(Prefix)+1, length(PartBody)-length(Suffix)),
			Factory = fun() ->
				fun(Writer, Logger, _CommandParams, _OutputParams, RuntimeParams) ->
					{"Timestamp", DateTime, Timestamp} = lists:keyfind("Timestamp", 1, RuntimeParams),
					TimestampString = format_time(FormatString, DateTime, Timestamp),
					(Logger#logger.log_entry)(TimestampString),
					Writer(TimestampString)
				end
			end,
			{true, Factory};
		true ->  {false, nothing}
	end.

% FormatString : use DD, MM, YY (or YYYY) for Date, use hh, mm, ss, ms for Time
format_time(FormatString, {{Year, Month, Day}, {Hour, Minute, Second}}, {_MegaSecs, _Secs, MicroSecs}) ->
	format_time_impl(FormatString, #date_time{year=Year , month=Month, day=Day, hour=Hour, minute=Minute, second=Second, millisecond=(MicroSecs div 1000)}, "").
	
format_time_impl([], _DateTime, ResultString) -> ResultString;
format_time_impl([$D, $D | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.day), 2, $0));
format_time_impl([$M, $M | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.month), 2, $0));
format_time_impl([$Y, $Y, $Y, $Y | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.year), 4, $0));
format_time_impl([$Y, $Y | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.year rem 100), 2, $0));
format_time_impl([$h, $h | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.hour), 2, $0));
format_time_impl([$m, $m | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.minute), 2, $0));
format_time_impl([$s, $s | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.second), 2, $0));
format_time_impl([$m, $s | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ string:right(integer_to_list(DateTime#date_time.millisecond), 4, $0));
format_time_impl([Char | Other], DateTime, ResultString) -> format_time_impl(Other, DateTime, ResultString ++ [Char]).