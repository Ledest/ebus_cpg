-module(ebus_cpg).

-behaviour(application).

%% Application callbacks and functions
-export([start/0, start/2, stop/0, stop/1]).

%% Utilities
-export([server/0, default_ps_server/0]).

-define(DEFAULT_PS_SERVER, ebus_cpg_ps).

%%%===================================================================
%%% Application callbacks and functions
%%%===================================================================

%% @doc Starts `ebus_cpg' application.
-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(ebus_cpg).

%% @doc Stops `ebus_cpg' application.
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(ebus_cpg).

%% @hidden
start(_StartType, _StartArgs) ->
    application:load(ebus),
    ebus_ps_cpg:start_link(server(), application:get_env(ebus, pubsub, [])).

%% @hidden
stop(_State) -> ok.

%%%===================================================================
%%% Utilities
%%%===================================================================

%% @doc Returns the registered `ebus_cpg' server name.
-spec server() -> atom().
server() -> application:get_env(ebus_cpg, name, ?DEFAULT_PS_SERVER).

%% @doc Returns default `ebus_cpg' server name: `ebus_cpg_ps'.
-spec default_ps_server() -> ebus_ps.
default_ps_server() -> ?DEFAULT_PS_SERVER.
