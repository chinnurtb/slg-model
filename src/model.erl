-module(model).
-compile(export_all).

-include("model.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("deps/mysql/include/mysql.hrl").

%% 数据库SQL日志记录输出.
logger(_, _, _Level, _Fun) ->
  %% {Str, Val} = _Fun(),
  %% io:format("~p ~p~n", [Str, Val]).
  pass.

%% 开启一个poll
start(DbsConf) ->
  #db_conf{poll=Poll, host=HostName, port=Port, username=UserName,
           password=Password, database=DataBase, worker=Worker} = DbsConf,
  mysql:start_link(Poll, HostName, Port, UserName, Password, DataBase, fun logger/4),
  [mysql:connect(Poll, HostName, undefined, UserName, Password, DataBase, true) ||
    _ <- lists:seq(1, Worker)],
  Poll.

record(buildings) -> record_info(fields, db_building).

%% 根据原子返回它的model层模块名 building -> model_building
model(Atom) -> atom_prefix(Atom, model).

atom_suffix(Key, Suffix) ->
  L = atom_to_list(Key) ++ "_" ++ atom_to_list(Suffix),
  list_to_atom(L).
atom_prefix(Key, Prefix) ->
  L = atom_to_list(Prefix) ++ "_" ++ atom_to_list(Key),
  list_to_atom(L).

%% 产生原子.
atom(Suffix, Key) -> atom_suffix(Key, Suffix).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 更高层次的执行
select_t(Record, Table, Column, Cond) ->
  SQL = model_sql:select(Table, Column, Cond),
  io:format("sql 1 ~p~n", [SQL]),
  model_exec:select_t(Record, SQL).

select_n(Poll, Record, Table, Column, Cond) ->
  SQL = model_sql:select(Table, Column, Cond),
  io:format("sql 2 ~p~n", [SQL]),
  model_exec:select_n(Poll, Record, SQL).

kv_list([K], [V]) ->  [{K, V}];
kv_list([K|Kl], [V|Vl]) -> [{K, V}] ++ kv_list(Kl, Vl).

%% Data为Record的实例.
update_t([_Id|Keys], Table, Data) ->
  [_Name, Id | Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:update(Table, Kv, [{id, Id}]),
  model_exec:update_t(SQL),
  ok.

%% Data为Record的实例.
update_n(Poll, [_Id|Keys], Table, Data) ->
  [_Name, Id | Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:update(Table, Kv, [{id, Id}]),
  model_exec:update_n(Poll, SQL),
  ok.

%% 参数为普通kv
insert_n(Poll, Kv, Table) ->
  SQL = model_sql:insert(Table, Kv),
  io:format("sql ~p~n", [SQL]),
  model_exec:insert_n(Poll, SQL).

%% 参数为记录
insert_n(Poll, Keys, Table, Data) when is_list(Keys) ->
  [_Name|Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:insert(Table, Kv),
  io:format("sql ~p~n", [SQL]),
  model_exec:insert_n(Poll, SQL).


%% 参数为普通kv
insert_t(Kv, Table) ->
  SQL = model_sql:insert(Table, Kv),
  io:format("sql ~p~n", [SQL]),
  model_exec:insert_t(SQL).

%% 参数为记录
insert_t(Keys, Table, Data) when is_list(Keys) ->
  [_Name|Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:insert(Table, Kv),
  io:format("sql2 ~p~n", [SQL]),
  model_exec:insert_t(SQL).

delete_n(Poll, {in, IdList}, Table) ->
  SQL = model_sql:delete(Table, {id, in, IdList}),
  model_exec:delete_n(Poll, SQL),
  ok;
delete_n(Poll, KvList, Table) when is_list(KvList) ->
  SQL = model_sql:delete(Table, KvList),
  model_exec:delete_n(Poll, SQL),
  ok;
delete_n(Poll, Id, Table) ->
  SQL = model_sql:delete(Table, [{id, Id}]),
  model_exec:delete_n(Poll, SQL),
  ok.

delete_t({in, IdList}, Table) ->
  SQL = model_sql:delete(Table, {id, in, IdList}),
  model_exec:delete_t(SQL),
  ok;
delete_t(KvList, Table) when is_list(KvList) ->
  SQL = model_sql:delete(Table, KvList),
  model_exec:delete_t(SQL),
  ok;
delete_t(Id, Table) ->
  SQL = model_sql:delete(Table, [{id, Id}]),
  model_exec:delete_t(SQL),
  ok.

insert_delete_test() ->
  Poll = start(#db_conf{username="root", password="", database="slg_model"}),
  delete_n(Poll, {in, [23]}, buildings),
  delete_n(Poll, [{user_id, 2001}], buildings),
  insert_n(Poll, [{id, 23}, {type, <<"gog">>}, {level, 23}], buildings),
  ok.

max(Poll, Column, Table) ->
  L = model_sql:max(Table, Column, all),
  {data, Result} = model_exec:exec(Poll, L),
  [[Id]] = mysql:get_result_rows(Result),
  Id.

max_id(Poll, Table) -> max(Poll, id, Table).

count(Poll, Cond, Table) ->
  Sql = model_sql:count(Table, Cond),
  {data, Result} = model_exec:exec(Poll, Sql),
  [[Count]] = mysql:get_result_rows(Result),
  Count.
count_test() ->
  Poll = start(#db_conf{username="root", password="", database="slg_model"}),
  S = count(Poll, all, buildings),
  true = is_integer(S),
  S.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 动态生成模块

%% 有了这个函数就不用手写model层了。
module_new(Key) ->
  Atom = Key,
  ModuleAtom = atom_prefix(Atom, model),
  DbAtom = atom_prefix(Atom, db),
  M1 = data_smerl:new(ModuleAtom),
  Table = Key,
  %% 普通查询函数.
  SelectFun1 = io_lib:format("select(Poll, Cond) when is_list(Cond) ->
     model:select_n(Poll, ~p, ~p, all, Cond);
     select(Poll, UserId) when is_integer(UserId) ->
     model:select_n(Poll, ~p, ~p, all, [{user_id, UserId}]).",
                             [DbAtom, Table, DbAtom, Table]),
  SelectFun = lists:flatten(SelectFun1),
  UpdateFun1 = io_lib:format("update(Poll, Db) ->
     model:update_n(Poll, model_record:m(~p), ~p, Db).", [Key, Table]),
  UpdateFun = lists:flatten(UpdateFun1),
  InsertFun1 = io_lib:format("insert(Poll, Db) ->
     model:insert_n(Poll, model_record:m(~p), ~p, Db).", [Key, Table]),
  InsertFun = lists:flatten(InsertFun1),
  DeleteFun1 = io_lib:format("delete(Poll, ID)  ->
     model:delete_n(Poll, ID, ~p).
   ", [Table]),
  DeleteFun = lists:flatten(DeleteFun1),
  {ok, M2} = data_smerl:add_func(M1, SelectFun),
  {ok, M3} = data_smerl:add_func(M2, UpdateFun),
  {ok, M4} = data_smerl:add_func(M3, InsertFun),
  {ok, M5} = data_smerl:add_func(M4, DeleteFun),
  %% 事务查询函数
  SelectFunt1 = io_lib:format("select(Cond) when is_list(Cond) ->
     model:select(~p, ~p, all, Cond);
     select(UserId) when is_integer(UserId) ->
     model:select(~p, ~p, all, [{user_id, UserId}]).",
                              [DbAtom, Table, DbAtom, Table]),
  SelectFunt = lists:flatten(SelectFunt1),
  UpdateFunt1 = io_lib:format("update(Db) ->
     model:update(model_record:m(~p), ~p, Db).", [Key, Table]),
  UpdateFunt = lists:flatten(UpdateFunt1),
  InsertFunt1 = io_lib:format("insert(Db) ->
     model:insert(model_record:m(~p), ~p, Db).", [Key, Table]),
  InsertFunt = lists:flatten(InsertFunt1),
  DeleteFunt1 = io_lib:format("delete(ID) ->
     model:delete(ID, ~p).", [Table]),
  DeleteFunt = lists:flatten(DeleteFunt1),
  {ok, M6} = data_smerl:add_func(M5, SelectFunt),
  {ok, M7} = data_smerl:add_func(M6, UpdateFunt),
  {ok, M8} = data_smerl:add_func(M7, InsertFunt),
  {ok, M9} = data_smerl:add_func(M8, DeleteFunt),
  M10 = data_smerl:set_exports(M9, [{select,2}, {update,2}, {insert,2}, {delete,2},
                                    {select,1}, {update,1}, {insert,1}, {delete,1}]),
  data_smerl:compile(M10),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 生成映射表.

%% 映射表是一个动态模块，该模块用来做model-key 到起对应的record的属性列表
%% record_info(fields,x)
%% 在生成之前先使用一张ets表获取用户输入，之后一次性生成.

init_m() ->
  data_ets:new(slg_model_map).

%% Key为复数.
add_m(Key, KeyList, Db) ->
  ets:insert(slg_model_map, {Key, KeyList, Db}).

%% 生成动态record映射.
gen_m() ->
  All = ets:tab2list(slg_model_map),
  M0 = data_smerl:new(model_record),
  DFun = fun({K, L, _}, F_0) ->
             F_0 ++ lists:flatten(io_lib:format("m(~p) -> ~p;", [K, L]))
         end,
  Fun = lists:foldl(DFun, "", All),
  Fun1 = Fun ++ "m(_) -> throw(map_error).",
  %% io:format("~p", [Fun1]),
  {ok, M1} = data_smerl:add_func(M0, Fun1),
  M2 = data_smerl:set_exports(M1, [{m, 1}]),
  data_smerl:compile(M2),
  gen_h(),
  ok.

%% 生成模块管理函数.
gen_h() ->
  All = ets:tab2list(slg_model_map),
  lists:foreach(fun({K, _, Db}) ->
                    data_super:start_holder(Db, K)
                end,
                All),
  ok.