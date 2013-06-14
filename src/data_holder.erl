%% 实现ets表保存逻辑.
%% EtsHolder -> 兼职id分配.
-module(data_holder).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("model.hrl").

start_link(DataBase, Key) ->
  Atom = model:atom(holder, Key),
  gen_server:start_link({local, Atom}, ?MODULE, [DataBase, Key], []).

id(Key) ->
  Atom = model:atom(holder, Key),
  gen_server:call(Atom, id).

stop(Key) ->
  Atom = model:atom(holder, Key),
  gen_server:cast(Atom, stop).

%% 读取数据库，获得Table表的最大id.
max_id(Poll, Table) ->
  model:max_id(Poll, Table).

init([DataBase, Key]) ->
  %%{_, _, Atom} = data:map(Key),
  data:new(Key),
  model:module_new(Key),
  model:start(#db_conf{poll=model:atom(read, Key), worker=3, database=DataBase}),
  model:start(#db_conf{poll=model:atom(write, Key), worker=1, database=DataBase}),
  MaxId = max_id(model:atom(read, Key), Key),
  erlang:send_after(10000, self(), write),
  {ok, {Key, MaxId}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_call(id, _From, {Key, MaxID}) ->
  %% id策略还没考虑进来
  %% OID = MaxID div 1000,
  %% SID = MaxID rem 1000,
  %% OID1 = OID + 1,
  %% MaxID1 = OID1 * 1000 + SID,
  MaxID1 = MaxID +1,
  {reply, MaxID1, {Key, MaxID1}};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

write_back(Key, Pid) ->
  %% mysql:fetch(normal, <<"select sleep(23);">>),
  data:write_add(Key),
  data:write_update(Key),
  data:write_del(Key),
  %% io:format("sync mysql ~n"),
  erlang:send_after(10000, Pid, write),
  ok.

handle_info(write, State = {Key, _}) ->
  %% 会阻塞进程，所以最好在其它进程里做，比如spawn个新的,这样也不用处理异常。
  spawn(?MODULE, write_back, [Key, self()]),
  {noreply, State};

handle_info(_Info, State) ->
  %% {stop, normal, State}.
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.
