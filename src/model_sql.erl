%% 本模块主要将参数形式转为可执行的SQl.
-module(model_sql).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% 拼接表格的fileds列表, select @(name, type, level)，只接受原子参数.
k_column(S) when is_atom(S) -> "`"++ atom_to_list(S) ++ "`".

k_column_list(all) -> ["*"];
k_column_list([H]) ->
  H1 = k_column(H), [H1];
k_column_list([H|L]) ->
  H1 = k_column(H),
  [H1, ", "] ++ k_column_list(L).

k_column_list_test() ->
  ["*"] = k_column_list(all),
  ["`name`"] = k_column_list([name]),
  L = ["`name`", ", ", "`password`"] = k_column_list([name, password]),
  <<"`name`, `password`">> = list_to_binary(L),
  ok.

%% 拼接语句中可能出现的value列表，只接受数字，二进制和列表
v_column(V) when is_integer(V) -> integer_to_list(V);
v_column(<<V/binary>>) -> <<$", V/binary, $">>;
v_column(V) when is_list(V) ->
  B = list_to_binary(V),
  <<$", B/binary, $">>.

v_column_list([H]) ->
  H1 = v_column(H), [H1];
v_column_list([H|L]) ->
  H1 = v_column(H),
  [H1, ", "] ++ v_column_list(L).

v_column_list_test() ->
  [<<"\"name\"">>] = v_column_list([<<"name">>]),
  L = [<<"\"name\"">>, ", ", <<"\"password\"">>] = v_column_list([<<"name">>, <<"password">>]),
  <<"\"name\", \"password\"">> = list_to_binary(L),
  ok.

%% 拼接kv串，用作where语句查询
kv_column_value_list(_, [{K, V}]) ->
  K1 = k_column(K),
  V1 = v_column(V),
  [K1, " = ", V1];
kv_column_value_list(C, [{K, V}| L]) ->
  K1 = k_column(K),
  V1 = v_column(V),
  [K1, " = ", V1, C] ++ kv_column_value_list(C, L).

kv_column_value_list_test() ->
  R = ["`k1`"," = ",<<"\"nice\"">>, ", ", "`k2`", " = ", "23", ", ", "`k3`", " = ", <<"\"v3\"">>] = kv_column_value_list(", ", [{k1, <<"nice">>}, {k2, 23}, {k3, <<"v3">>}]),
  <<"`k1` = \"nice\", `k2` = 23, `k3` = \"v3\"">> = list_to_binary(R),
  ok.

limit(V) ->
  [" LIMIT ", integer_to_list(V)].

%% 条件语句
condition({Key, in, ValueList}) ->
  [" WHERE "] ++ k_column(Key) ++ " IN (" ++ v_column_list(ValueList) ++ ")";
condition(all) ->
  [];
condition(Cond) ->
  [" WHERE "] ++ kv_column_value_list(" and ", Cond).


%% only atom
table_name(Table) when is_atom(Table) -> "`" ++ atom_to_list(Table) ++ "`".
from(Table) -> [" FROM ", table_name(Table)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 各种语句拼接

-spec select(atom(), list(), list()) -> binary().

%% 拼接查询语句
select(Table, Column, Cond) ->
  L = "SELECT " ++ k_column_list(Column)  ++ from(Table) ++ condition(Cond) ++";",
  list_to_binary(L).

select_test() ->
  <<"SELECT `id`, `name` FROM `users` WHERE `id` = 23;">>
    = select(users, [id, name], [{id, 23}]).

-spec select(atom(), list(), list(), integer()) -> binary().

select(Table, Column, Cond, Limit) ->
  L = "SELECT " ++ k_column_list(Column)  ++ from(Table)
    ++ condition(Cond) ++ limit(Limit) ++";",
  list_to_binary(L).

select_e_test() ->
  <<"SELECT `id`, `name` FROM `users` WHERE `id` = 23 LIMIT 3;">>
    = select(users, [id, name], [{id, 23}], 3).


%% 拼接更新语句.
update(Table, Column, Cond) ->
  L = "UPDATE "
    ++ table_name(Table)
    ++ " SET "
    ++ kv_column_value_list(", ", Column)
    ++ condition(Cond)
    ++ ";",
  list_to_binary(L).

%% 拼接更新语句
update_test() ->
  <<"UPDATE `user` SET `name` = \"fe\", `value` = \"fewf\" WHERE `id` = 23;">>
    = update(user, [{name, <<"fe">>}, {value,  <<"fewf">>}], [{id, 23}]).

%% 拼接删除语句
delete(Table, Cond) ->
  L = "DELETE"
    ++ from(Table)
    ++ condition(Cond)
    ++ ";",
  list_to_binary(L).

delete_test() ->
  <<"DELETE FROM `user` WHERE `id` = 23;">>
    = delete(user, [{id, 23}]),
  <<"DELETE FROM `user` WHERE `id` IN (1, 2, 3);">>
    = delete(user, {id, in, [1,2,3]}).

insert(Table, KvList) ->
  {K1, V1} = lists:foldl(fun({K,V}, {Kl, Vl}) ->
                             {[K|Kl], [V|Vl]}
                         end, {[], []}, KvList),
  Kl = lists:reverse(K1),
  Vl = lists:reverse(V1),
  %%true = lists:keymember(id, 1, KvList),
  L = "INSERT INTO "
    ++ table_name(Table)
    ++ "( "
    ++ k_column_list(Kl)
    ++ " )"
    ++ " VALUES ( "
    ++ v_column_list(Vl)
    ++ " );",
  list_to_binary(L).

insert_test() ->
  <<"INSERT INTO `user`( `id`, `name` ) VALUES ( 23, \"xx\" );">>
    = insert(user, [{id, 23}, {name, <<"xx">>}]),
  ok.

max(Table, Field, Cond) ->
  L = "select max(" ++ k_column(Field) ++ ") "
    ++ from(Table)
    ++ condition(Cond),
  list_to_binary(L).

max_test() ->
  <<"select max(`id`)  FROM `buildings` WHERE `name` = \"nice\"">>
    = ?MODULE:max(buildings, id, [{name, <<"nice">>}]),
  <<"select max(`id`)  FROM `buildings`">>
    = ?MODULE:max(buildings, id, all),
  ok.

count(Table, Cond) ->
  L = "select count(*) "
    ++ from(Table)
    ++ condition(Cond),
  list_to_binary(L).

count_test() ->
  <<"select count(*)  FROM `building` WHERE `name` = \"nice\"">>
    = count(building, [{name, <<"nice">>}]),
  <<"select count(*)  FROM `building`">>
    = count(building, all),
  ok.
