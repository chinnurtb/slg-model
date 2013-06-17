-module(data).
-export([lookup_s/2, lookup_a/2, lookup_i/2]).
-export([guard_f/0]).
-export([update_s/3, update_i/2, delete_i/3, delete_s/3, clear/1]).
-export([add_s/3, add_i/3, id/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 互斥访问

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


%% 维护一个进程获取的guard列表，进程退出时释放.
guard_a(Table, UsrId, Role) ->
  R = case erlang:get({guard, list}) of
        undefined -> [];
        R1 -> R1
      end,
  erlang:put({guard, list}, [{Table, UsrId, Role}|R]),
  ok.

%% 在进程退出时,清除所有的guard.
guard_f() ->
  R = case erlang:get({guard, list}) of undefined -> []; R1 -> R1 end,
  Fun = fun({Table, UsrId, Role}) ->
            ok = case Role of
                   reader -> data_guard:read_off(Table, UsrId, self());
                   writer -> data_guard:write_off(Table, UsrId, self())
                 end
        end,
  lists:foreach(Fun, R),
  erlang:erase({guard, list}),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 其他API.

%% 不管是查看自己或者其他玩家的数据，lookup_s或lookup_a必须先调用.
lookup_s(Table, UsrId) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_s(Table, UsrId);   %% 递归下去，直到成功.
    ok -> data_ets:find_s(Table, UsrId)
  end.

lookup_a(Table, UsrId) ->
  case guard_r(Table, UsrId) of
    {error, _} -> lookup_a(Table, UsrId);   %% 递归下去，直到成功.
    ok -> data_ets:find_a(Table, UsrId)
  end.

%%%%%%%%

lookup_i(Table, Id) ->
  data_ets:find_i(Table, Id).

update_s(Table, UsrId, Data) ->
  case data_ets:update_s(Table, UsrId, Data) of
    ok -> data_writer:event(Table, upt, Data), ok;
    {error, R} -> {error, R}
  end.

%% 分条更新
update_i(Table, Data) ->
  data_ets:update_i(Table, Data),
  data_writer:event(Table, upt, Data),
  ok.

delete_s(Table, UsrId, Id) ->
  case data_ets:delete_s(Table, UsrId, Id) of
    ok -> data_writer:event(Table, del, Id), ok;
    {error, R} -> {error, R}
  end.

delete_i(Table, UsrId, Id) ->
  case data_ets:delete_i(Table, UsrId, Id) of
    ok -> data_writer:event(Table, del, Id), ok;
    {error, R} -> {error, R}
  end.

add_s(Table, UsrId, Data) ->
  case data_ets:add_s(Table, UsrId, Data) of
    ok -> data_writer:event(Table, add, Data), ok;
    {error, R} -> {error, R}
  end.

add_i(Table, UsrId, Data) ->
  case data_ets:add_i(Table, UsrId, Data) of
    ok -> data_writer:event(Table, add, Data), ok;
    {error, R} -> {error, R}
  end.

clear_fun(Table, UsrId) ->
  case guard_w(Table, UsrId) of
    ok ->
      data_ets:clear(Table, UsrId),
      guard_f();
    {error, _} -> do_nothing
  end.

%% 对数据进行清除，有可能会失败，不过没关系.
clear(Table) ->
  All = data_ets:out_time(Table),
  [clear_fun(Table, UsrId) || UsrId <- All],
  ok.

%% 获取id.
id(Key) -> data_holder:id(Key).
