-module(telnet_server_app).
-behavior(application).

-export([start/0, stop/0]).
%% export for application behavior
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

start() -> application:start(telnet_server_app).
stop() -> application:stop(telnet_server_app).

%% export for application behavior
start(_StartType, [PortNumber, WorkerLimit]) -> telnet_server:start(PortNumber, WorkerLimit, fun telnet_server_initializer:init/2).
stop(_State) -> ok.

%% unused export
start_phase(_Phase, _StartType, _PhaseArgs) -> ok.
prep_stop(_State) -> ok.
config_change(_Changed, _New, _Removed) -> ok.