%% @copyright 2011 Hunter Morris
%% @doc Implementation of `supervisor' behaviour.
%% @private
%% @end
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_nif_pool_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/0, init/1]).

%% @doc Creates a supervisor process as part of a supervision tree.


-spec start_link() -> Result when
	Result :: {ok, pid()} | ignore | {error, StartlinkError},
	StartlinkError :: {already_started, pid()} | {shutdown, term()} | term().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Dynamically adds a child specification to supervisor, 
%% which starts the corresponding child process.

-spec start_child() -> Result when
	Info :: term(),
	Child :: undefined | pid(),
	Result :: {ok, Child} | {ok, Child, Info} | {error, StartChildError},
	StartChildError :: already_present | {already_started, Child} | term().
start_child() -> supervisor:start_child(?MODULE, []).

-spec init(Args) -> Result when 
	Args :: list(),
	Result :: {ok,{SupFlags, ChildSpec}} | ignore,
	SupFlags :: {one_for_one, 10, 10},
	ChildSpec :: [supervisor:child_spec()].
init([]) ->
    {ok, PoolSize} = application:get_env(bcrypt, nif_pool_size),
    {ok, MaxOverFlow} = application:get_env(bcrypt, nif_pool_max_overflow),

    PoolArgs = [
        {name, {local, bcrypt_nif_pool}},
        {size, PoolSize},
        {max_overflow, MaxOverFlow},
        {worker_module, bcrypt_nif_worker}
    ],

    PoolSpecs = [
        poolboy:child_spec(bcrypt_nif_pool, PoolArgs, [])
    ],

    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.