-module(data).
-export([lookup_s/2, lookup_a/2, lookup_i/2]).
-export([guard_f/0]).
-export([update_s/3, update_i/2, delete_i/3, delete_i_a/3, delete_s/3, clear/1]).
-export([add_s/3, add_i/3, id/1]).
-export([lookup_s_e/3, lookup_a_e/3, lookup_i_e/3, update_s_e/3, update_i_e/3]).
-export([lookup_i/3, lookup_i_e/4, update_i/3, update_i_e/4, count_a/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 互斥访问

-type guarder() :: 'reader' | 'writer'.

-spec guard_r(atom(), integer()) -> ok | {error, not_exist}.
%% 用进程字典cache一下状态.
guard_r(Table, UsrId) ->
  case erlang:get({guard, Table, UsrId}) of
    undefined ->
      case data_guard:read_on(Table, UsrId, self()) of
        ok -> erlang:put({guard, Table, UsrId}, true),
              guard_a(Table, UsrId, reader),
              ok;
        error -> {error, read_guard_failed}
      end;
    true -> ok
  end.

-spec guard_w(atom(), integer()) -> ok | {error, not_exist}.
%% 用进程字典cache一下状态.
guard_w(Table, UsrId) ->
  case erlang:get({guard, Table, UsrId}) of
    undefined ->
      case data_guard:write_on(Table, UsrId, self()) of
        ok -> erlang:put({guard, Table, UsrId}, true),
              guard_a(Table, UsrId, writer),
              ok;
        error -> {error, write_guard_failed}
      end;
    true -> ok
  end.

-spec guard_a(atom(), integer(), guarder()) -> ok.
%% 维护一个进程获取的guard列表，进程退出时释放.
guard_a(Table, UsrId, Role) ->
  R = case erlang:get({guard, list}) of
        undefined -> [];
        R1 -> R1
      end,
  erlang:put({guard, list}, [{Table, UsrId, Role}|R]),
  ok.

-spec guard_f() -> ok.
%% 在进程退出时,清除所有的guard.
guard_f() ->
  R = case erlang:get({guard, list}) of undefined -> []; R1 -> R1 end,
  Fun = fun({Table, UsrId, Role}) ->
            ok = case Role of
                   reader -> data_guard:read_off(Table, UsrId, self());
                   writer -> data_guard:write_off(Table, UsrId, self())
                 end,
            erlang:erase({guard, Table, UsrId})
        end,
  lists:foreach(Fun, R),
  erlang:erase({guard, list}),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 其他API.
-spec lookup_s(atom(), integer()) -> {ok, tuple()} | {error, not_exist}.
%% 不管是查看自己或者其他玩家的数据，lookup_s或lookup_a必须先调用.
lookup_s(Table, UsrId) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_s(Table, UsrId);   %% 递归下去，直到成功.
    ok -> data_ets:lookup_s(Table, UsrId)
  end.

-spec lookup_s_e(atom(), integer(), integer()) -> {ok, term()} | {error, not_exist}.
%% 按元素查找.
lookup_s_e(Table, UsrId, Pos) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_s_e(Table, UsrId, Pos);   %% 递归下去，直到成功.
    ok -> data_ets:lookup_s_e(Table, UsrId, Pos)
  end.

-spec lookup_a(atom(), integer()) -> {ok, list()}.
lookup_a(Table, UsrId) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_a(Table, UsrId);   %% 递归下去，直到成功.
    ok -> data_ets:lookup_a(Table, UsrId)
  end.

%% 查找多条数据的某个元素.
-spec lookup_a_e(atom(), integer(), integer()) -> {ok, list()} | {error, not_exist}.
lookup_a_e(Table, UsrId, Pos) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_a_e(Table, UsrId, Pos);
    ok -> data_ets:lookup_a_e(Table, UsrId, Pos)
  end.

-spec count_a(atom(), integer()) -> {ok, integer()} | {error, not_exist}.
count_a(Table, UsrId) ->
  case guard_r(Table, UsrId) of
    {error, _} -> count_a(Table, UsrId);
    ok -> data_ets:count_a(Table, UsrId)
  end.

%%%%%%%%

-spec lookup_i(atom(), integer()) -> {ok, tuple()} | {error, not_exist}.
lookup_i(Table, Id) ->
  data_ets:lookup_i(Table, Id).


-spec lookup_i(atom(), integer(), integer()) -> {ok, tuple()} | {error, not_exist}
                                                  | {error, not_belong_user}.
lookup_i(Table, UsrId, Id) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_i(Table, UsrId, Id);
    ok -> data_ets:lookup_i(Table, UsrId, Id)
  end.

-spec lookup_i_e(atom(), integer(), integer()) -> {ok, term()} | {error, not_exist}.
%% 按元素查找.
lookup_i_e(Table, Id, Pos) ->
  data_ets:lookup_i_e(Table, Id, Pos).

-spec lookup_i_e(atom(), integer(), integer(), integer()) -> {ok, term()} |
                                                             {error, not_exist} |
                                                             {error, not_belong_user}.
%% 按元素查找.
lookup_i_e(Table, UsrId, Id, Pos) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_i_e(Table, UsrId, Id, Pos);
    ok -> data_ets:lookup_i_e(Table, UsrId, Id, Pos)
  end.

-spec update_s(atom(), integer(), integer()) -> ok | {error, not_exist}.
update_s(Table, UsrId, Data) ->
  case guard_r(Table, UsrId) of
    {error, _} -> update_s(Table, UsrId, Data);
    ok -> case data_ets:update_s(Table, UsrId, Data) of
            ok -> spt_notify:post(slg_m_upt_s, {Table, UsrId, Data}),
                  data_writer:event(Table, upt, Data), ok;
            {error, R} -> {error, R}
          end
  end.

-spec update_s_e(atom(), atom(), list()) -> ok.
%% 按位置更新.
update_s_e(Table, UsrId, List) ->
  case guard_r(Table, UsrId) of
    {error, _} -> update_s_e(Table, UsrId, List);
    ok -> {ok, Id} = data_ets:update_s_e(Table, UsrId, List),
          spt_notify:post(slg_m_upt_s_e, {Table, UsrId, Id, List}),
          data_writer:event(Table, upt, {Id, List}),
          ok
  end.

-spec update_i(atom(), tuple()) -> ok.
%% 分条更新
update_i(Table, Data) ->
  data_ets:update_i(Table, Data),
  spt_notify:post(slg_m_upt_i, {Table, Data}),
  data_writer:event(Table, upt, Data),
  ok.

-spec update_i(atom(), integer(), tuple()) -> ok.
update_i(Table, UsrId, Data) ->
  case guard_r(Table, UsrId) of
    {error, _ } -> update_i(Table, UsrId, Data);
    ok -> ok = data_ets:update_i(Table, UsrId, Data),   %% defence
          spt_notify:post(slg_m_upt_i, {Table, Data}),
          data_writer:event(Table, upt, Data),
          ok
  end.

-spec update_i_e(atom(), integer(), list()) -> ok.
%% 按位置更新. deprecated
update_i_e(Table, Id, List) ->
  data_ets:update_i_e(Table, Id, List),
  spt_notify:post(slg_m_upt_i_e, {Table, Id, List}),
  data_writer:event(Table, upt, {Id, List}),
  ok.

update_i_e(Table, UsrId, Id, List) ->
  case guard_r(Table, UsrId) of
    {error, _ } -> update_i_e(Table, UsrId, Id, List);
    ok ->
      ok = data_ets:update_i_e(Table, UsrId, Id, List),
      spt_notify:post(slg_m_upt_i_e, {Table, Id, List}),
      data_writer:event(Table, upt, {Id, List}),
      ok
  end.

-spec delete_s(atom(), integer(), integer()) -> ok | {error, not_exist}.
delete_s(Table, UsrId, Id) ->
  case guard_r(Table, UsrId) of
    {error, _} -> delete_s(Table, UsrId, Id);
    ok -> case data_ets:delete_s(Table, UsrId, Id) of
            ok ->
              spt_notify:post(slg_m_del_s, {Table, UsrId, Id}),
              data_writer:event(Table, del, Id), ok;
            {error, R} -> {error, R}
          end
  end.

-spec delete_i(atom(), integer(), integer()) -> ok | {error, not_exist}.
delete_i(Table, UsrId, Id) ->
  case guard_r(Table, UsrId) of
    {error, _} -> delete_i(Table, UsrId, Id);
    ok ->
      case data_ets:delete_i(Table, UsrId, Id) of
        ok ->
          spt_notify:post(slg_m_del_i, {Table, UsrId, Id}),
          data_writer:event(Table, del, Id), ok;
        {error, R} -> {error, R}
      end
  end.


-spec delete_i_a(atom(), integer(), list()) -> ok | {error, not_exist}.
%% 删除一个id列表
delete_i_a(Table, UsrId, IdList) ->
  case guard_r(Table, UsrId) of
    {error, _} -> delete_i_a(Table, UsrId, IdList);
    ok ->
      case data_ets:delete_i_a(Table, UsrId, IdList) of
        ok ->
          spt_notify:post(slg_m_del_i_a, {Table, UsrId, IdList}),
          data_writer:event(Table, del, {in, IdList}), ok;
        {error, R} -> {error, R}
      end
  end.

-spec add_s(atom(), integer(), tuple()) -> ok | {error, not_exist}.
add_s(Table, UsrId, Data) ->
  case guard_r(Table, UsrId) of
    {error, _} -> add_s(Table, UsrId, Data);
    ok ->
      case data_ets:add_s(Table, UsrId, Data) of
        ok ->
          spt_notify:post(slg_m_add_s, {Table, UsrId, Data}),
          data_writer:event(Table, add, Data), ok;
        {error, R} -> {error, R}
      end
  end.

-spec add_i(atom(), integer(), tuple()) -> ok | {error, not_exist}.
add_i(Table, UsrId, Data) ->
  case guard_r(Table, UsrId) of
    {error, _} -> add_i(Table, UsrId, Data);
    ok ->
      case data_ets:add_i(Table, UsrId, Data) of
        ok ->
          spt_notify:post(slg_m_add_i, {Table, UsrId, Data}),
          data_writer:event(Table, add, Data), ok;
        {error, R} -> {error, R}
      end
  end.

clear_fun(Table, UsrId) ->
  case guard_w(Table, UsrId) of
    ok ->
      io:format("clear ~p ~p~n", [Table, UsrId]),
      data_ets:clear(Table, UsrId),
      guard_f();
    {error, _} -> do_nothing
  end.

-spec clear(atom()) -> ok.

%% 对数据进行清除，有可能会失败，不过没关系.
clear(Table) ->
  All = data_ets:out_time(Table),
  [clear_fun(Table, UsrId) || UsrId <- All],
  ok.

-spec id(atom()) -> integer().

%% 获取id.
id(Key) -> data_holder:id(Key).
