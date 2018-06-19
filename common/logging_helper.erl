-module(logging_helper).

-export([tuple_to_string/3]).
-export([get_current_datetime/0, get_current_datetime/3, get_timestamp/0, get_timestamp/4]).
-export([log_client_connected/2, log_client_disconnected/3, log_client_failed/3, log_server_started/0, log_server_stopped/0]).

tuple_to_string(Source, Separator, PartToListFun) when is_tuple(Source), is_list(Separator) ->
	lists:foldl(
		fun(Part, AccIn) ->
			if
				AccIn == "" -> PartToListFun(Part);
				AccIn /= "" -> AccIn ++ Separator ++ PartToListFun(Part)
			end
		end, "", tuple_to_list(Source)).
	
get_current_datetime() -> get_current_datetime(".", " ", ":").
	
get_current_datetime(DateSeparator, DateTimeSeparator, TimeSeparator) ->
	{{Year, Month, Day}, Time} = erlang:universaltime(),
	tuple_to_string({Day, Month, Year}, DateSeparator, fun integer_to_list/1) ++ DateTimeSeparator ++ tuple_to_string(Time, TimeSeparator, fun integer_to_list/1).
	
get_timestamp() -> get_timestamp(".", " ", ":", ".").
	
get_timestamp(DateSeparator, DateTimeSeparator, TimeSeparator, MillisecSeparator) ->
	{_MegaSecs, _Secs, MicroSecs} = now(),
	get_current_datetime(DateSeparator, DateTimeSeparator, TimeSeparator) ++ MillisecSeparator ++ integer_to_list(MicroSecs div 1000).
	
log_client_connected(ClientAddress, ClientPort) -> 
	"Client from " ++ tuple_to_string(ClientAddress, ".", fun integer_to_list/1) ++ ":" ++ integer_to_list(ClientPort) ++ " connected at " ++ get_current_datetime() ++ "\r\n".
	
log_client_disconnected(ClientAddress, ClientPort, Reason) ->
	"Client from " ++ tuple_to_string(ClientAddress, ".", fun integer_to_list/1) ++ ":" ++ integer_to_list(ClientPort) ++ " disconnected at " ++ get_current_datetime() ++ " because " ++ io_lib:format("~p", [Reason]) ++ "\r\n".
	
log_client_failed(ClientAddress, ClientPort, Reason) ->
	"Client from " ++ tuple_to_string(ClientAddress, ".", fun integer_to_list/1) ++ ":" ++ integer_to_list(ClientPort) ++ " failed at " ++ get_current_datetime() ++ " because " ++ io_lib:format("~p", [Reason]) ++ "\r\n".
	
log_server_started() -> 
	"Server started at " ++ get_current_datetime() ++ "\r\n".
	
log_server_stopped() -> 
	"Server stopped at " ++ get_current_datetime() ++ "\r\n".