%% @copyright 2011 Hunter Morris
%% @doc Implementation of `application' behaviour.
%% @private
%% @end
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_app).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-behaviour(application).

-export([start/2, stop/1]).

-spec start(StartType, StartArgs) -> Result when
	StartType :: normal, 
	StartArgs :: term(),
	Result :: {ok, pid()} | {error, Reason},
	Reason :: term().
start(normal, _Args) ->
    case bcrypt_sup:start_link() of
        {ok, Pid}          -> {ok, Pid};
        {error, _} = Error -> Error
    end.

-spec stop(State) -> Result when
	State :: term(),
	Result :: ok.
stop(_State) -> ok.
