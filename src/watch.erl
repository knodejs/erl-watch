%% TODO write unit tests
-module(watch).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0]).
-export([poll/0]).
-export([reload/0]).
-export([start_reloader/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([find_apps_to_watch/0]).
-export([md5sum_beam_files/1]).
-export([find_changed_files/2]).
-export([reload_loop/0]).

-import(filename, [basename/1, rootname/1]).
-import(filelib, [wildcard/1]).
-import(lists, [foreach/2]).

-define(SERVER, ?MODULE).

-record(state, {version = 0,    % incremente on change in watched files
                watched = []}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  application:start(watch).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


poll() ->
    gen_server:call(?SERVER, poll).


reload() ->
    %%io:format("Reload ~p~n",["Reload Code SSS"]),
    Changed = poll(),
    foreach(fun({reload, _, Module}) ->
                    io:format("~nwatched test change module >> ~s << changed reloading~n",
                              [Module]),
                    os:cmd("cd D:/ToGo/Mbk/travsock/priv & webpack --config webpack-production.config.js --progress --colors"),
                    code:purge(Module),
                    code:load_file(Module)
            end,
            Changed).


%% Blocking call to reload loop
reload_loop() ->
    reload(),
    timer:sleep(1000),
    reload_loop().


%% Non blocking call to reload loop
start_reloader() ->
    %%io:format("start_reloader ~p~n",["reload_loop test"]),
    {ok, spawn_link(?MODULE, reload_loop, [])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(poll, _From, State) ->
    Dirs = find_apps_to_watch(),
    NewWatched = md5sum_beam_files(Dirs),

    Version = State#state.version,
    Watched = State#state.watched,

    {Changed, NewState} =
        if Watched /= NewWatched ->
                {find_changed_files(Watched, NewWatched),
                 State#state{version = Version+1,
                             watched = NewWatched}};
           true ->
                {[], State}
        end,
    Reload = [R || R = {reload, _, _} <- Changed],

    {reply, Reload, NewState}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
find_apps_to_watch() ->
    LibDir = "./priv/src/app",
    
    lists:filter(fun(Dir) -> string:rstr(Dir, LibDir) == 0 end,
                 code:get_path()).


md5sum_beam_files(Dirs) ->
    %%io:format("Dirs ~p~n",[Dirs]),
    lists:sort(lists:flatten(md5sum_beam_files(Dirs, []))).


md5sum_beam_files([], Result) ->
    Result;
md5sum_beam_files([Dir|Rest], Result) ->
    io:format("Dir Monitor : ~p~n",[Dir]),
    BeamFiles = wildcard(Dir ++ '/*.js'),
    NameMD5 = lists:map(
                fun(FileName) ->
                        {ok, Contents} = file:read_file(FileName),
                        {FileName, erlang:md5(Contents)}
                end,
                BeamFiles),

    NewResult = [NameMD5|Result],
    md5sum_beam_files(Rest, NewResult).


module_name(Filename) ->
    list_to_atom(rootname(basename(Filename))).


find_changed_files([], _) ->
    [];
find_changed_files(Cur, New) ->
    lists:map(fun({Filename, NewMd5}) ->
                      case lists:keyfind(Filename, 1, Cur) of
                          {Filename, CurMd5} when CurMd5 /= NewMd5 ->
                              {reload, modified, module_name(Filename)};
                          false ->
                              {reload, new, module_name(Filename)};
                          _ ->
                              {noreload, not_changed, module_name(Filename)}
                      end
              end,
              New).
