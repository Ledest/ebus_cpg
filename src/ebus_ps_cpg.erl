%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original `Phoenix.PubSub.PG2'
%%% module.
%%% Copyright (c) 2014 Chris McCord
%%%
%%% Phoenix PubSub adapter based on PG2.
%%% To use it as your PubSub adapter, simply add it to your
%%% Endpoint's config file (see e.g.: test/test.config):
%%%
%%% ```
%%% [
%%%  {ebus,
%%%   [
%%%    {pubsub,
%%%     [
%%%      {adapter, ebus_ps_cpg},
%%%      {pool_size, 5},
%%%      {name, ebus_ps_test}
%%%     ]
%%%    }
%%%   ]
%%%  }
%%% ].
%%% '''
%%%
%%% Options:
%%% <ul>
%%% <li>`name': The name to register the PubSub processes,
%%% ie: `ebus_ps'.</li>
%%% <li>`pool_size': Both the size of the local pubsub server pool and
%%% subscriber shard size. Defaults `1'. A single pool is often enough
%%% for most use cases, but for high subscriber counts on a single
%%% topic or greater than 1M clients, a pool size equal to the number
%%% of schedulers (cores) is a well rounded size.</li>
%%% </ul>
%%%
%%% @reference See
%%% <a href="https://github.com/phoenixframework/phoenix">Phoenix</a>
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_ps_cpg).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(atom(), [term()]) -> supervisor:startlink_ret().
start_link(Name, Opts) ->
    supervisor:start_link({local, ebus_common:build_name([Name, <<"sup">>], <<"_">>)}, ?MODULE, [Name, Opts]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([Server, Opts]) ->
    PoolSize = ebus_common:keyfind(pool_size, Opts, 1),
    ebus_supervisor_spec:supervise([ebus_supervisor_spec:supervisor(ebus_ps_local_sup,
                                                                    [Server, PoolSize,
                                                                     [{broadcast, ebus_ps_cpg_server,
                                                                       [Server, PoolSize]}]]),
                                    ebus_supervisor_spec:worker(ebus_ps_cpg_server, [Server])],
                                   #{strategy => rest_for_one}).
