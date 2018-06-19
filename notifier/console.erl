%% pseudoconsole
-module(console).

-include("../common/common_defs.hrl").

-export([start/1, input/1]).

start(CommandConfig) -> logic_fsm:start(CommandConfig).

input(ConsolePID) -> logic_fsm:process(ConsolePID, "go").