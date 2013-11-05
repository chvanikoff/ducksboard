-module(ducksboard).

%% API
-export([
	start/0,
	stop/0,
	push/2, push/3,
	set_api_key/1
]).

%% Application behaviour
-behaviour(application).
-export([
	start/2,
	stop/1
]).

%% Supervisor behaviour
-behaviour(supervisor).
-export([init/1]).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	ok = ensure_started([?MODULE]),
	ok.

stop() -> application:stop(?MODULE).

push(Key, Val) -> ducksboard_srv:push(Key, Val).

push(Type, Key, Val) -> ducksboard_srv:push(Type, Key, Val).

set_api_key(ApiKey) -> ducksboard_srv:set_api_key(ApiKey).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart_strategy = {one_for_one, 5, 10},
	Children = [
		?CHILD(ducksboard_srv, worker)
	],
	{ok, {Restart_strategy, Children}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps] = All) ->
	{R, Msg} = case application:start(App) of
		ok -> {ok, "started"};
		{error, {already_started, App}} -> {ok, "already started"};
		{error, {not_started, Dependency}} -> {Dependency, "requires " ++ atom_to_list(Dependency)}
	end,
	io:format("~s ~s~n", [App, Msg]),
	case R of ok -> ensure_started(Apps); D -> ensure_started([D | All]) end.
