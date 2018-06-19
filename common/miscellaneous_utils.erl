-module(miscellaneous_utils).

-export([read_term/1, read_string_data/1, timeout_delta/2, time_rest/3]).

read_term(FileName) ->
	{ok, IoDevice} = file:open(FileName, read),
	try
		{ok, Term, _EndLine} = io:read(IoDevice, "", 1),
		Term
	after
		file:close(IoDevice)
	end.
	
read_string_data(FileName) ->
	{ok, BinaryContent} = file:read_file(FileName),
	binary_to_list(BinaryContent).

timeout_delta(infinity, _Timeout2) -> infinity;
timeout_delta(_Timeout1, infinity) -> 0;
timeout_delta(Timeout1, Timeout2) when Timeout1 < Timeout2 -> 0;
timeout_delta(Timeout1, Timeout2) -> Timeout1 - Timeout2.

time_rest(infinity, _LastNow, _Now) -> infinity;
time_rest(Rest, LastNow, Now) ->
	TimeDiff = timer:now_diff(Now, LastNow) div 1000,
	NewRest = Rest - TimeDiff,
	if
		NewRest =< 0 -> 0;
		NewRest > 0 -> NewRest 
	end.