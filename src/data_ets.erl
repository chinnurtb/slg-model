%% 处理data模型中的ets表
%% 当数据在ets表中发生变化时，会记录其状态.
%% ets表中会维护:更新id列表, 删除id列表, 新建id列表
%% 并将id列表中的数据按相应的规则写回数据库.
-module(data_ets).
-compile(export_all).

-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


-spec new(atom()) -> atom().

%% 启动一个ets表.
new(Table) ->
  Table = spt_ets:safe_create(Table, [named_table, public, set, {keypos, 2}]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 方便ets查找的函数
-spec ets_i(atom(), integer()) -> {ok, tuple()} | {error, not_exist}.
ets_i(EtsTable, Id) ->
  case ets:lookup(EtsTable, Id) of
    [] -> {error, not_exist};
    [R]-> {ok, R}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 这两个函数从db中load数据到ets.

%% MySQL对应的ets表全部为set类型，且key_pos为2.
%% 如果数据不在ets中，加载单条数据到ets，如果在则不加载.
-spec load_s(atom(), integer()) -> {ok, tuple()} | {error, not_exist}.
load_s(EtsTable, UsrId) ->
  Model = model:model(EtsTable),
  case Model:select_n(model:atom_poll(EtsTable, write), UsrId) of
    [] -> {error, not_exist};
    [R]-> K = element(2, R),
          ets:insert(EtsTable, {single, {key, UsrId}, K, get_time()}),
          ets:insert(EtsTable, R),
          {ok, R}
  end.
-spec load_a(atom(), integer()) -> {ok, list()}.
load_a(EtsTable, UsrId) ->
  Model = model:model(EtsTable),
  List = Model:select_n(model:atom_poll(EtsTable, write), UsrId),
  KList = lists:foldl(fun(R, KL) ->
                          ets:insert(EtsTable, R),
                          K = element(2, R),
                          ets:insert(EtsTable, R),
                          [K|KL]
                      end, [], List),
  ets:insert(EtsTable, {array, {key, UsrId}, KList, get_time()}),
  {ok, List}.

%% 确保玩家数据在内存
ensure_s(EtsTable, UsrId) ->
  case ets:lookup(EtsTable, {key, UsrId}) of
    [] -> load_s(EtsTable, UsrId);
    [_] -> ok
  end.
ensure_a(EtsTable, UsrId) ->
  case ets:lookup(EtsTable, {key, UsrId}) of
    [] -> load_a(EtsTable, UsrId);
    [_] -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 查询函数接口

-spec lookup_s(atom(), integer()) -> {ok, tuple()}.
%% 通过key查找单条数据.
lookup_s(Table, UsrId) ->
  ensure_s(Table, UsrId),
  [{single, {key, UsrId}, Id, Time}] = ets:lookup(Table, {key, UsrId}),
  [R] = ets:lookup(Table, Id),
  case get_time() of
    Time -> do_nothing;
    T -> ets:update_element(Table, {key, UsrId}, {4, T})
  end,
  {ok, R}.

-spec lookup_s_e(atom(), integer(), integer()) -> {ok, term()} | {error, not_exist}.
%% 按pos查找属性，必须要确认数据存在才能用.
lookup_s_e(Table, UsrId, Pos) ->
  ensure_s(Table, UsrId),
  [{single, {key, UsrId}, Id, _Time}] = ets:lookup(Table, {key, UsrId}),
  {ok, ets:lookup_element(Table, Id, Pos)}.

-spec lookup_a(atom(), integer()) -> {ok, list()} | {error, not_exist}.

%% 通过key查找数组数据
lookup_a(Table, UsrId) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, Time}] = ets:lookup(Table, {key, UsrId}),
  case get_time() of
    Time -> do_nothing;
    T -> ets:update_element(Table, {key, UsrId}, {4, T})
  end,
  L = lists:map(fun(Id) -> [R] = ets:lookup(Table, Id), R end, Ids),
  {ok, L}.

count_a(Table, UsrId) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _Time}] =  ets:lookup(Table, {key, UsrId}),
  {ok, length(Ids)}.

-spec lookup_a_e(atom(), integer(), integer()) -> {ok, list()} | {error, not_exist}.
lookup_a_e(Table, UsrId, Pos) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _Time}] = ets:lookup(Table, {key, UsrId}),
  E = [ets:lookup_element(Table, Id, Pos) || Id <- Ids],
  {ok, E}.

-spec lookup_i(atom(), integer()) -> {ok, tuple()} | {error, not_exist}.
lookup_i(Table, Id) ->
  case ets:lookup(Table, Id) of
    [] -> {error, not_exist};
    [R]-> {ok, R}
  end.

-spec lookup_i(atom(), integer(), integer()) -> {ok, tuple()} | {error, not_exist}
                                                  |{error, not_belong_user}.
%% 判断Id是否属于UsrId的接口.
lookup_i(Table, UsrId, Id) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _Time}] = ets:lookup(Table, {key, UsrId}),
  case lists:member(Id, Ids) of
    true -> lookup_i(Table, Id);
    false -> {error, not_belong_user}
  end.

-spec lookup_i_e(atom(), integer(), integer()) -> {ok, tuple()}.

%% 按pos查找数据.
lookup_i_e(Table, Id, Pos) ->
  R = ets:lookup_element(Table, Id, Pos),
  {ok, R}.

-spec lookup_i_e(atom(), integer(), integer(), integer()) -> {ok, tuple()} | {error, not_exist}
                                                               | {error, not_belong_user}.
%% 判断了Id是否属于UsrId的接口.
lookup_i_e(Table, UsrId, Id, Pos) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _Time}] = ets:lookup(Table, {key, UsrId}),
  case lists:member(Id, Ids) of
    true -> lookup_i_e(Table, Id, Pos);
    false -> {error, not_belong_user}
  end.

-spec update_s(atom(), integer(), tuple()) -> ok | {error, not_exist}.
%% 更新操作
update_s(Table, UsrId, Data) ->
  ensure_s(Table, UsrId),
  [{single, {key, UsrId}, Id, _}] = ets:lookup(Table, {key, UsrId}),
  Id = element(2, Data),
  ets:insert(Table, Data),
  ok.

-spec update_s_e(atom(), integer(), list()) -> {ok, integer()} | {error, not_exist}.
%% 更新部分数据.
update_s_e(Table, UsrId, List) ->
  ensure_s(Table, UsrId),
  [{single, {key, UsrId}, Id, _}] = ets:lookup(Table, {key, UsrId}),
  ets:update_element(Table, Id, List),
  {ok, Id}.

-spec update_i(atom(), tuple()) -> ok.
%% 分条更新
update_i(Table, Data) ->
  ets:insert(Table, Data),
  ok.

-spec update_i(atom(), integer(), tuple()) -> ok.
%% 加入了判断是否属于user的接口
update_i(Table, UsrId, Data) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _Time}] = ets:lookup(Table, {key, UsrId}),
  Id = element(2, Data),
  case lists:member(Id, Ids) of
    true -> update_i(Table, Data);
    false -> {error, not_belong_user}
  end.

-spec update_i_e(atom(), tuple(), list()) -> {ok, integer()} | {error, not_exist}.
update_i_e(Table, Id, List) ->
  ets:update_element(Table, Id, List),
  ok.

-spec update_i_e(atom(), integer(), tuple(), list()) -> {ok, integer()} | {error, not_exist}
                                                          | {error, not_belong_user}.
%% 判断了id是否等于userid的接口
update_i_e(Table, UsrId, Id, List) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _Time}] = ets:lookup(Table, {key, UsrId}),
  case lists:member(Id, Ids) of
    true -> update_i_e(Table, Id, List);
    false -> {error, not_belong_user}
  end.

-spec delete_s(atom(), integer(), integer()) -> ok | {error, not_exist}.

delete_s(Table, UsrId, Id) ->
  ensure_s(Table, UsrId),
  [{single, {key, UsrId}, Id, _}] = ets:lookup(Table, {key, UsrId}),
  ets:delete(Table, Id),
  ets:delete(Table, {key, UsrId}),
  ok.

-spec delete_i(atom(), integer(), integer()) -> ok | {error, not_exist}.
%% 分条删除，不需要单条删除
delete_i(Table, UsrId, Id) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _}] = ets:lookup(Table, {key, UsrId}),
  case lists:member(Id, Ids) of
    false -> {error, not_belong_user};
    true  ->
      ets:delete(Table, Id),
      ets:update_element(Table, {key, UsrId}, {3, lists:delete(Id, Ids)}), ok
  end.

-spec delete_i_a(atom(), integer(), list()) -> ok | {error, not_exist}.

%% 多条条删除.
delete_i_a(Table, UsrId, IdList) ->
  ensure_a(Table, UsrId),
  [{array, {key, UsrId}, Ids, _}] = ets:lookup(Table, {key, UsrId}),
  L2 = lists:foldl(fun(Id, L) -> lists:delete(Id, L)
                   end, Ids, IdList),
  ets:update_element(Table, {key, UsrId}, {3, L2}),
  [ets:delete(Table, Id) || Id <- IdList],
  ok.


-spec add_i(atom(), integer(), tuple()) -> ok.

%% 增加一个新条目
add_i(Table, UsrId, Data) ->
  ensure_a(Table, UsrId),
  Id = element(2, Data),
  [{array, {key, UsrId}, Ids, _}] = ets:lookup(Table, {key, UsrId}),
  ets:insert(Table, Data),
  ets:update_element(Table, {key, UsrId}, {3, [Id|Ids]}),
  ok.

-spec add_s(atom(), integer(), tuple()) -> ok | {error, already_exist}.

%% 添加单条
add_s(Table, UsrId, Data) ->
  Id = element(2, Data),
  case ets:lookup(Table, {key, UsrId}) of
    [{single, {key, UsrId}, _Id, _}] -> {error, already_exist};
    [] ->
      ets:insert(Table, Data),
      ets:insert(Table, {single, {key, UsrId}, Id, get_time()}),
      ok
  end.

%% -spec find_s(atom(), integer()) -> {ok, tuple} | {error, not_exist}.
%% find_s(Table, UsrId) ->
%%   case lookup_s(Table, UsrId) of
%%     {ok, R} -> {ok, R};
%%     {error, _ } -> load_s(Table, UsrId)
%%   end.

%% -spec find_a(atom(), integer()) -> {ok, list()}.
%% find_a(Table, UsrId) ->
%%   case lookup_a(Table, UsrId) of
%%     {ok, R} -> {ok, R};
%%     {error, _ } -> load_a(Table, UsrId)
%%   end.

%% -spec find_i(atom(), integer()) -> {ok, tuple()} | {error, not_exist}.

%% find_i(Table, Id) ->
%%   lookup_i(Table, Id).

%% -spec find_i(atom(), integer(), integer()) -> {ok, tuple()} | {error, not_exist} | {error, not_belong_user}.

%% find_i(Table, UserId, Id) ->
%%   lookup_i(Table, UserId, Id).

-spec clear(atom(), integer()) -> true.

%% 清除玩家单元数据.
clear(Table, UsrId) ->
  case ets:lookup(Table, {key, UsrId}) of
    [{single, {key, UsrId}, Id, _}] ->
      ets:delete(Table, Id),
      ets:delete(Table, {key, UsrId});
    [{array, {key, UsrId}, Ids, _}] ->
      [ets:delete(Table, Id) || Id <- Ids],
      ets:delete(Table, {key, UsrId})
  end.

%% 该字段没有具体的时间含义，因为erlang:now有性能问题，不能频繁调用.
%% 所以某条数据的最后访问时间将取内存里固定的一个时间而已.
current() ->
  {MegaSecs, Secs, _} = erlang:now(),
  MegaSecs * 1000000 + Secs.

get_time() ->
  case erlang:get(time_current) of
    undefined ->
      {MegaSecs, Secs, _} = erlang:now(),
      T = MegaSecs * 1000000 + Secs,
      erlang:put(time_current, T),
      T;
    R -> R
  end.

%% 筛选最后一次访问时间超过6小时的数据，对其进行清除操作.
out_time(Table) ->
  Now = current(),
  MS = ets:fun2ms(fun({_, {key, UsrId}, _, Time}) when ((Now - 3 ) > Time) -> UsrId end),
  ets:select(Table, MS).
