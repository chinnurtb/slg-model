%% 处理data模型中的ets表
%% 当数据在ets表中发生变化时，会记录其状态.
%% ets表中会维护:更新id列表, 删除id列表, 新建id列表
%% 并将id列表中的数据按相应的规则写回数据库.
-module(data_ets).
-compile(export_all).

-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").

%% 安全新建ets表
safe_create(AtomName, Options) ->
  case ets:info(AtomName) of
    undefined -> ets:new(AtomName, Options);
    _Other -> AtomName
  end.

%% 启动一个ets表.
new(Table) ->
  Table = safe_create(Table, [named_table, public, set, {keypos, 2}]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 方便ets查找的函数

ets_i(EtsTable, Id) ->
  case ets:lookup(EtsTable, Id) of
    [] -> {error, not_exist};
    [R]-> {ok, R}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 这两个函数从db中load数据到ets.

%% MySQL对应的ets表全部为set类型，且key_pos为2.
%% 如果数据不在ets中，加载单条数据到ets，如果在则不加载.

load_s(EtsTable, UsrId) ->
  Model = model:model(EtsTable),
  case Model:select(model:atom(read, EtsTable), UsrId) of
    [] -> {error, not_exist};
    [R]-> K = element(2, R),
          ets:insert(EtsTable, {single, {key, UsrId}, K, get_time()}),
          ets:insert(EtsTable, R),
          {ok, R}
  end.

load_a(EtsTable, UsrId) ->
  Model = model:model(EtsTable),
  List = Model:select(model:atom(read, EtsTable), UsrId),
  KList = lists:foldl(fun(R, KL) ->
                          ets:insert(EtsTable, R),
                          K = element(2, R),
                          ets:insert(EtsTable, R),
                          [K|KL]
                      end, [], List),
  ets:insert(EtsTable, {array, {key, UsrId}, KList, get_time()}),
  {ok, List}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 查询函数接口

%% 通过key查找单条数据.
lookup_s(Table, UsrId) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{single, {key, UsrId}, Id, Time}] ->
      [R] = ets:lookup(Table, Id),
      case get_time() of
        Time -> do_nothing;
        T -> ets:update_element(Table, {key, UsrId}, {4, T})
      end,
      {ok, R}
  end.

%% 通过key查找数组数据
lookup_a(Table, UsrId) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{array, {key, UsrId}, Ids, Time}] ->
      case get_time() of
        Time -> do_nothing;
        T -> ets:update_element(Table, {key, UsrId}, {4, T})
      end,
      L = lists:map(fun(Id) -> [R] = ets:lookup(Table, Id), R end, Ids),
      {ok, L}
  end.

lookup_i(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> {error, not_exist};
    [R]-> {ok, R}
  end.

%% 更新操作
update_s(Table, UsrId, Data) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{single, {key, UsrId}, Id, _}] ->
      Id = element(2, Data),
      ets:insert(Table, Data), ok
  end.

%% 分条更新
update_i(Table, Data) ->
  ets:insert(Table, Data),
  ok.

delete_s(Table, UsrId, Id) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{single, {key, UsrId}, Id, _}] ->
      ets:delete(Table, Id),
      ets:delete(Table, {key, UsrId}),
      ok
  end.

%% 分条删除，不需要单条删除
delete_i(Table, UsrId, Id) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{array, {key, UsrId}, Ids, _}] ->
      ets:delete(Table, Id),
      ets:update_element(Table, {key, UsrId}, {3, lists:delete(Id, Ids)}),
      ok
  end.

%% 增加一个新条目
add_i(Table, UsrId, Data) ->
  Id = element(2, Data),
  [{array, {key, UsrId}, Ids, _}] = ets:lookup(Table, {key, UsrId}),
  ets:insert(Table, Data),
  ets:update_element(Table, {key, UsrId}, {3, [Id|Ids]}),
  ok.

%% 添加单条
add_s(Table, UsrId, Data) ->
  Id = element(2, Data),
  case ets:lookup(Table, {key, UsrId}) of
    [{single, {key, UsrId}, _Id, _}] ->
      {error, already_exist};
    [] ->
      ets:insert(Table, Data),
      ets:insert(Table, {single, {key, UsrId}, Id, get_time()}),
      ok
  end.

find_s(Table, UsrId) ->
  case lookup_s(Table, UsrId) of
    {ok, R} -> {ok, R};
    {error, _ } -> load_s(Table, UsrId)
  end.

find_a(Table, UsrId) ->
  case lookup_a(Table, UsrId) of
    {ok, R} -> {ok, R};
    {error, _ } -> load_a(Table, UsrId)
  end.

find_i(Table, Id) ->
  lookup_i(Table, Id).

%% 该字段没有具体的时间含义，因为erlang:now有性能问题，不能频繁调用.
%% 所以某条数据的最后访问时间将取内存里固定的一个时间而已.
get_time() ->
  case erlang:get(time_current) of
    undefined ->
      {MegaSecs, Secs, _} = erlang:now(),
      T = MegaSecs * 1000000 + Secs,
      erlang:put(time_current, T),
      T;
    R -> R
  end.

