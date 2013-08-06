%% 实现ets表保存逻辑.
%% EtsHolder -> 兼职id分配.
-module(data_holder).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("model.hrl").

%%
%% 将默认使用一个大的连接池，除非显示的指定其为self.
%%
start_link(DataBase, Key, Opts) ->
  Atom = model:atom(holder, Key),
  gen_server:start_link({local, Atom}, ?MODULE, [DataBase, Key, Opts], []).

id(Key) ->
  Atom = model:atom(holder, Key),
  gen_server:call(Atom, id).

stop(Key) ->
  Atom = model:atom(holder, Key),
  gen_server:cast(Atom, stop).

init([Dbc, Key, _Opts]) ->
  data_ets:new(Key),
  %% model:start(Dbc#db_conf{poll=model:atom_poll(Key, read), worker=3}),
  %% model:start(Dbc#db_conf{poll=model:atom_poll(Key, write), worker=1}),
  MaxId = model:max_id(Key),
  data_guard_super:start_guard(Key),
  data_writer_super:start_writer(Key),
  data_clear_super:start_clear(Key),
  {ok, {Key, MaxId, Dbc}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_call(id, _From, {Key, MaxID, Dbc}) ->
  %% id策略还没考虑进来
  OID = MaxID div 1000,
  SID = model_config:sid(),
  MaxID1 = (OID+1) * 1000 + SID,
  {reply, MaxID1, {Key, MaxID1, Dbc}};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  %% {stop, normal, State}.
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.
