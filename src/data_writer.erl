%% 实时回写进程，程序中的添加，修改和删除等事件实时同步到MYSQL.
-module(data_writer).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1, event/3]).

%% 启动模块.
start_link(Table) ->
  Atom = model:atom(writer, Table),
  gen_server:start_link({local, Atom}, ?MODULE, [Table], []).

%% db事件.
event(Table, Event, Db) ->
  Atom = model:atom(writer, Table),
  gen_server:cast(Atom, {event, Table, Event, Db}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server api

init([Key]) ->
  {ok, {Key}}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast({event, Table, Event, Db}, State) ->
  io:format("* event ~p ~p ~n", [Event, Db]),
  Model = model:model(Table),
  Poll = model:atom_poll(Table, write),
  case Event of
    add -> Model:insert_n(Poll, Db);
    upt -> Model:update_n(Poll, Db);
    del -> Model:delete_n(Poll, Db)
  end,
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.
