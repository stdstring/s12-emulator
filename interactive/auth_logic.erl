-module(auth_logic).

-include("../common/common_defs.hrl").
-include("console_defs.hrl").

-export([login/2]).

login(UserID, Pwd) ->
	ClearUserId = string:strip(UserID),
	ClearPwd = string:strip(Pwd),
	if
		(ClearUserId /= "") and (ClearPwd /= "") -> #login_result{result = true, reason = "Login successful\r\n"};
		(ClearUserId == "") or (ClearPwd == "") -> #login_result{result = false, reason = "Incorrect login or/and password\r\n"}
	end.