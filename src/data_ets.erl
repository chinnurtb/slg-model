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
          ets:insert(EtsTable, {single, {key, UsrId}, K}),
          ets:insert(EtsTable, R),
          {ok, R}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 如果数据不在ets中，加载数组数据到ets，如果在则不加载.

load_a(EtsTable, UsrId) ->
  Model = model:model(EtsTable),
  List = Model:select(model:atom(read, EtsTable), UsrId),
  KList = lists:foldl(fun(R, KL) ->
                          ets:insert(EtsTable, R),
                          K = element(2, R),
                          ets:insert(EtsTable, R),
                          [K|KL]
                      end, [], List),
  ets:insert(EtsTable, {array, {key, UsrId}, KList}),
  {ok, List}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 查询函数接口

%% 通过key查找单条数据.
lookup_s(Table, UsrId) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{single, {key, UsrId}, Id}] ->
      [R] = ets:lookup(Table, Id), {ok, R}
  end.

%% 通过key查找数组数据
lookup_a(Table, UsrId) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{array, {key, UsrId}, Ids}] ->
      L = lists:map(fun(Id) -> [R] = ets:lookup(Table, Id), R end, Ids),
      {ok, L}
  end.

lookup_i(Table, UsrId, Id) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{array, {key, UsrId}, Ids}] ->
      true = lists:memeber(Id, Ids),
      case ets:lookup(Table, Id) of
        [] -> {error, not_exist};
        [R]-> {ok, R}
      end
  end.
lookup_i(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> {error, not_exist};
    [R]-> {ok, R}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 更新ets表

%% 基础函数
attr_stage_add(Table, DictName, Id) ->
  Dict = case ets:lookup(Table, {stage, DictName}) of
           [] -> dict:new();
           [{state, {stage, DictName}, Dict1}] -> Dict1
         end,
  Dict2 = dict:store(Id, true, Dict),
  ets:insert(Table, {state, {stage, DictName}, Dict2}).

%% 获取更新id列表
attr_stage_all(Table, DictName) ->
  case ets:lookup(Table, {stage, DictName}) of
    [] -> [];
    [{state, {stage, DictName}, Dict}] -> dict:to_list(Dict)
  end.

attr_stage_del(Table, DictName, Id) ->
  Dict = case ets:lookup(Table, {stage, DictName}) of
           [] -> dict:new();
           [{state, {stage, DictName}, Dict1}] -> Dict1
         end,
  Dict2 = dict:erase(Id, Dict),
  ets:insert(Table, {state, {stage, DictName}, Dict2}).

attr_stage_test() ->
  new(revenues),
  attr_stage_add(revenues, up, 23),
  [{23, true}] = attr_stage_all(revenues, up),
  ok.


%% 在更新列表
update_stage_add(Table, Id) -> attr_stage_add(Table, up, Id).
update_stage_all(Table) -> attr_stage_all(Table, up).
update_stage_del(Table, Id) -> attr_stage_del(Table, up, Id).

%% 删除列表
delete_stage_add(Table, Id) -> attr_stage_add(Table, del, Id).
delete_stage_all(Table) -> attr_stage_all(Table, del).
delete_stage_del(Table, Id) -> attr_stage_del(Table, del, Id).

%% 新建列表
add_stage_add(Table, Id) -> attr_stage_add(Table, add, Id).
add_stage_all(Table) -> attr_stage_all(Table, add).
add_stage_del(Table, Id) -> attr_stage_del(Table, add, Id).

%% 更新操作
update_s(Table, UsrId, Data) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{single, {key, UsrId}, Id}] ->
      update_stage_add(Table, Id),
      ets:insert(Table, Data), ok
  end.

%% 分条更新
update_i(Table, _UsrId, Data) when is_tuple(Data) ->
  Id = element(2, Data),
  update_stage_add(Table, Id),
  ets:insert(Table, Data),
  ok.

delete_s(Table, UsrId, Id) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{single, {key, UsrId}, Id}] ->
      delete_stage_add(Table, Id),
      ets:delete(Table, Id),
      ets:delete(Table, {key, UsrId}),
      ok
  end.

%% 分条删除，不需要单条删除
delete_i(Table, UsrId, Id) ->
  case ets:lookup(Table, {key, UsrId}) of
    [] -> {error, not_exist};
    [{array, {key, UsrId}, Ids}] ->
      ets:delete(Table, Id),
      ets:insert(Table, {array, {key, UsrId}, lists:delete(Id, Ids)}),
      delete_stage_add(Table, Id),
      ok
  end.

%% 增加一个新条目
add_i(Table, UsrId, Data) ->
  Id = element(2, Data),
  Ids = case ets:lookup(Table, {key, UsrId}) of
          [] -> [];
          [{array, {key, UsrId}, Ids1}] -> Ids1
        end,
  ets:insert(Table, {array, {key, UsrId}, [Id|Ids]}),
  ets:insert(Table, Data),
  add_stage_add(Table, Id),
  ok.

%% 添加单条
add_s(Table, UsrId, Data) ->
  Id = element(2, Data),
  case ets:lookup(Table, {key, UsrId}) of
    [{single, {key, UsrId}, _Id}] ->
      {error, already_exist};
    [] ->
      ets:insert(Table, {single, {key, UsrId}, Id}),
      ets:insert(Table, Data),
      add_stage_add(Table, Id),
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 定时回写数据到数据

%% 新增数据写回数据库，
write_add_i(Table, Model, Id) ->
  case ets_i(Table, Id) of
    {error, _} -> ok;
    {ok, R} ->
      Poll = model:atom(write, Table),
      Model:insert(Poll, R),
      add_stage_del(Table, Id)
  end.

write_add(Table) ->
  L = add_stage_all(Table),
  Model = model:model(Table),
  [write_add_i(Table, Model, Id) || {Id, _} <- L],
  ok.

%% 更新数据写回数据库
write_update_i(Table, Model, Id) ->
  case ets_i(Table, Id) of
    {error, _} -> ok;
    {ok, R} ->
      Poll = model:atom(write, Table),
      Model:update(Poll, R),
      update_stage_del(Table, Id)
  end.

write_update(Table) ->
  L = update_stage_all(Table),
  Model = model:model(Table),
  [write_update_i(Table, Model, Id) || {Id, _} <- L],
  ok.

%% 删除数据写回数据库
write_del_i(Table, Model, Id) ->
  Poll = model:atom(write, Table),
  Model:delete(Poll, Id),
  delete_stage_del(Table, Id),
  ok.

write_del(Table) ->
  L = delete_stage_all(Table),
  Model = model:model(Table),
  [write_del_i(Table, Model, Id) || {Id, _} <- L],
  ok.


id(Key) -> data_holder:id(Key).

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

find_i(Table, UsrId, Id) ->
  lookup_i(Table, UsrId, Id).
