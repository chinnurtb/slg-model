-module(data).
-export([lookup_s/2, lookup_a/2, lookup_i/2]).
-export([guard_list_free/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 互斥访问

%% 用进程字典cache一下状态.
guard_read_on(Table, UsrId) ->
  case erlang:get({guard, Table, UsrId}) of
    undefined ->
      case guard:read_on(Table, UsrId, self()) of
        ok -> erlang:put({guard, Table, UsrId}, true),
              guard_list_add(Table, UsrId, reader),
              ok;
        error -> {error, read_guard_failed}
      end;
    true -> ok
  end.

%% 用进程字典cache一下状态.
guard_write_on(Table, UsrId) ->
  case erlang:get({guard, Table, UsrId}) of
    undefined ->
      case guard:read_on(Table, UsrId, self()) of
        ok -> erlang:put({guard, Table, UsrId}, true),
              guard_list_add(Table, UsrId, reader),
              ok;
        error -> {error, read_guard_failed}
      end;
    true -> ok
  end.


%% 维护一个进程获取的guard列表，进程退出时释放.
guard_list_add(Table, UsrId, Role) ->
  R = case erlang:get({guard, list}) of
        undefined -> [];
        R1 -> R1
      end,
  erlang:put({guard, list}, [{Table, UsrId, Role}|R]),
  ok.

%% 在进程退出时,清除所有的guard.
guard_list_free() ->
  R = case erlang:get({guard, list}) of undefined -> []; R1 -> R1 end,
  lists:foreach(fun({Table, UsrId, Role}) ->
                    ok = case Role of
                           reader -> guard:read_off(Table, UsrId, self());
                           writer -> guard:write_off(Table, UsrId, self())
                         end
                end, R),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 其他API.

lookup_s(Table, UsrId) ->
  case guard_read_on(Table, UsrId) of
    {error, R} -> {error, R};
    ok -> data_ets:find_s(Table, UsrId)
  end.

lookup_a(Table, UsrId) ->
  case guard_read_on(Table, UsrId) of
    {error, R} -> {error, R};
    ok -> data_ets:find_a(Table, UsrId)
  end.

lookup_i(Table, Id) ->
  data_ets:find_i(Table, Id).
