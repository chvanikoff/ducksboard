-module(ducksboard_srv).

%% API
-export([
	start_link/0, start_link/1,
	set_api_key/1,
	push/2
]).

%% Gen_server behaviour
-behaviour(gen_server).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	code_change/3,
	terminate/2
]).

-define(URL, "https://push.ducksboard.com/v/").

-define(OPTS, [
	{timeout, 1000 * 15},
	{connect_timeout, 1000 * 10}
]).

-record(state, {
	headers = []
}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> start_link("").

start_link(ApiKey) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiKey], []).

set_api_key(ApiKey) -> gen_server:cast(?MODULE, {set_api_key, ApiKey}).

push(Key, Val) -> gen_server:cast({push, Key, Val}).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([ApiKey]) -> {ok, #state{headers = get_headers(ApiKey)}}.

handle_call(_Request, _From, State) -> {reply, ignored, State}.

handle_cast({set_api_key, ApiKey}, State) ->
	{noreply, State#state{headers = get_headers()}};
handle_cast({push, Key, Val}, #state{headers = Headers} = State) ->
	Timestamp = timer:now_diff(os:timestamp(), {0,0,0}) div 1000000,
	JSON = jsx:encode([
		{<<"timestamp">>, Timestamp},
		{<<"value">>, Value}
	]),
	case httpc:request(post, {?URL ++ Key, Headers, "application/x-www-form-urlencoded", JSON}, ?OPTS, []) of
		{ok, {{"HTTP/1.1", 200, "OK"}, _, Response}} ->  ok;
		Answer -> io:format("Could not push data to widget ~p, answer:~n~p~n", [Key, Answer])
	end,
	{noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_headers(ApiKey) ->
	[{"Authorization", "Basic " ++ base64:encode_to_string(ApiKey ++ ":unused")}].
