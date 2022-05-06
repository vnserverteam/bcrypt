%% @copyright 2011 Hunter Morris. 
%% @doc Implementation of `gen_server' behaviour.
%% @end
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_nif_worker).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(gen_server).

-export([start_link/1]).
-export([gen_salt/0, gen_salt/1]).
-export([hashpw/2]).

%% gen_server
-export([init/1, code_change/3, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
          default_log_rounds :: integer(), 
		  context :: term()
         }).

-type state() :: #state{default_log_rounds :: integer(), context :: term()}.

%% @doc Creates a `gen_server' process as part of a supervision tree.

-spec start_link(Args) -> Result when
	Args :: term(),
	Result :: {ok,Pid} | ignore | {error,Error},
	Pid :: pid(),
	Error :: {already_started,Pid} | term().
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

%% @doc Returns bcrypt salt.

-spec gen_salt() -> Result when
	Result :: [byte()].
gen_salt() ->
    poolboy:transaction(bcrypt_nif_pool, fun(Worker) ->
        gen_server:call(Worker, gen_salt, infinity)
    end).

%% @doc Returns bcrypt salt.

-spec gen_salt(Rounds) -> Result when
	Rounds :: bcrypt:rounds(),
	Result :: [byte()].
gen_salt(Rounds) ->
    poolboy:transaction(bcrypt_nif_pool, fun(Worker) ->
        gen_server:call(Worker, {gen_salt, Rounds}, infinity)
    end).

%% @doc Make hash string based on `Password' and `Salt'.

-spec hashpw( Password, Salt ) -> Result when
	Password :: [byte()] | binary(), 
	Salt :: [byte()] | binary(),
	Result :: {ok, Hash} | {error, ErrorDescription},
	Hash :: [byte()],
	ErrorDescription :: bcrypt:pwerr().
hashpw(Password, Salt) ->
    poolboy:transaction(bcrypt_nif_pool, fun(Worker) ->
         gen_server:call(Worker, {hashpw, Password, Salt}, infinity)
    end).

%% @private

-spec init(Args) -> Result when 
	Args :: list(),
	Result :: {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, Default} = application:get_env(bcrypt, default_log_rounds),
    Ctx = bcrypt_nif:create_ctx(),
    {ok, #state{default_log_rounds = Default, context = Ctx}}.

%% @private

terminate(shutdown, _) -> ok.

%% @private

-spec handle_call(Request, From, State) -> Result when 
    Request :: gen_salt,
    From :: {pid(), atom()},
    State :: state(),
	Result :: {reply, Reply, State},
	Reply :: {ok, Salt},
	Salt :: integer();
(Request, From, State) -> Result when
	Request :: {gen_salt, Rounds},
	From :: {pid(), atom()},
	State :: state(),
	Rounds :: bcrypt:rounds(),
	Result :: {reply, Reply, State},
	Reply :: {ok, Salt},
	Salt :: integer();
(Request, From, State) -> Result when
	Request :: {hashpw, Password, Salt},
	From :: {pid(), atom()},
	State :: state(),
	Password :: [byte()],
	Salt :: integer(),
	Result :: {reply, Reply, State} | {reply, Reply, State},
	Reply :: {ok, ResultInfo} | {error, ResultInfo},
	ResultInfo :: term().

handle_call(gen_salt, _From, #state{default_log_rounds = R} = State) ->
    Salt = bcrypt_nif:gen_salt(R),
    {reply, {ok, Salt}, State};
handle_call({gen_salt, R}, _From, State) ->
    Salt = bcrypt_nif:gen_salt(R),
    {reply, {ok, Salt}, State};
handle_call({hashpw, Password, Salt}, _From, #state{context=Ctx}=State) ->
    Ref = make_ref(),
    ok = bcrypt_nif:hashpw(Ctx, Ref, self(), to_list(Password), to_list(Salt)),
    receive
        {ok, Ref, Result} ->
            {reply, {ok, Result}, State};
        {error, Ref, Result} ->
            {reply, {error, Result}, State}
    end;
handle_call(Msg, _, _) -> exit({unknown_call, Msg}).

%% @private

handle_cast(Msg, _) -> exit({unknown_cast, Msg}).

%% @private

handle_info(Msg, _) -> exit({unknown_info, Msg}).

%% @private

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec to_list(List) -> Result when
	List :: [byte()],
	Result :: [byte()];
(Binary) -> Result when
	Binary :: binary(),
	Result :: [byte()].	
to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> binary_to_list(B).
