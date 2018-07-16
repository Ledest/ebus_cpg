%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original `Phoenix.PubSub.PG2Server'
%%% module.
%%% Copyright (c) 2014 Chris McCord
%%% @reference See
%%% <a href="https://github.com/phoenixframework/phoenix">Phoenix</a>
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_ps_cpg_server).

-behaviour(gen_server).

%% API
-export([start_link/1, broadcast/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom()) -> gen:start_ret().
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, Name, []).

-spec broadcast(atom(), pos_integer(), pid(), binary(), any()) -> ok | {error, no_such_group}.
broadcast(Name, PoolSize, FromPid, Topic, Msg) ->
    case cpg:get_local_members(cpg_ebus_scope, Name) of
        {error, {no_such_group, _}} -> ok;
        {ok, _, [_|_]} ->
            ebus_ps_local:broadcast(Name, PoolSize, FromPid, Topic, Msg),
            case cpg:get_remote_members(cpg_ebus_scope, Name) of
                {error, {no_such_group, _}} -> ok;
                {ok, _, Pids} ->
                    lists:foreach(fun(Pid) -> Pid ! {forward_to_local, FromPid, PoolSize, Topic, Msg} end, Pids)
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init(Name) ->
    ok = cpg:scope_exists(cpg_ebus_scope),
    ok = cpg:join(cpg_ebus_scope, Name),
    {ok, Name}.

%% @hidden
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) -> {noreply, State}.

%% @hidden
handle_info({forward_to_local, FromPid, PoolSize, Topic, Msg}, Name) ->
    % The whole broadcast will happen inside the current process
    % but only for messages coming from the distributed system.
    ebus_ps_local:broadcast(Name, PoolSize, FromPid, Topic, Msg),
    {noreply, Name};
handle_info(_Info, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, _State) -> ok.

%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.
