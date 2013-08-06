-module(model).
-compile(export_all).

-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").
%%-include("deps/mysql/include/mysql.hrl").

format(Pid, _) when is_pid(Pid) -> ok;
format("fetch ~p (id ~p)", [D|_]) ->
  io:format("SQL~s~n", [D]);
format(Str, Val) ->
  io:format(Str ++ "~n", Val).

%% 数据库SQL日志记录输出.
logger(_, _, _Level, _Fun) ->
  %% {Str, Val} = _Fun(),
  %% io:format("str ~p~n", [Str]),
  %% io:format("val ~p~n", [Val]),
  %% format(Str, Val),
  %% io:format("str ~p~n", [Str]),
  %% R = io_lib:format(Str, Val),
  %% io:format("sql: ~s~n", [R]),
  pass.

%% 开启一个poll
start(DbsConf) ->
  #db_conf{poll=Poll, host=HostName, port=Port, username=UserName,
           password=Password, database=DataBase, worker=Worker} = DbsConf,
  mysql:start_link(Poll, HostName, Port, UserName, Password, DataBase, fun logger/4, utf8),
  [mysql:connect(Poll, HostName, undefined, UserName, Password, DataBase, utf8, true) ||
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

atom_poll(Suffix, Key) ->
  L = atom_to_list(Key) ++ "_"++ atom_to_list(Suffix) ++
    "" ++ integer_to_list(model:sid_g()),
  list_to_atom(L).

%% record
record_atom(Key) ->
  S = "db_" ++ atom_to_list(Key),
  S1 = string:strip(S, both, $s),
  list_to_atom(S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 更高层次的执行
select_t(Record, Table, Column, Cond) ->
  SQL = model_sql:select(Table, Column, Cond),
  model_exec:select_t(Record, SQL).

%% with limit
select_t(Record, Table, Column, Cond, Limit) ->
  SQL = model_sql:select(Table, Column, Cond, Limit),
  model_exec:select_t(Record, SQL).

select_n(Record, Table, Column, Cond) ->
  SQL = model_sql:select(Table, Column, Cond),
  model_exec:select_n(Record, SQL).

%% with limit
select_n(Record, Table, Column, Cond, Limit) ->
  SQL = model_sql:select(Table, Column, Cond, Limit),
  model_exec:select_n(Record, SQL).


kv_list([K], [V]) ->  [{K, V}];
kv_list([K|Kl], [V|Vl]) -> [{K, V}] ++ kv_list(Kl, Vl).

%% Data为Record的实例.
update_t([_Id|Keys], Table, Data) ->
  [_Name, Id | Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:update(Table, Kv, [{id, Id}]),
  model_exec:update_t(SQL),
  ok;
%% 简单列表修改.
update_t(Id, Table, List) ->
  SQL = model_sql:update(Table, List, [{id, Id}]),
  model_exec:update_t(SQL),
  ok.

%% Data为Record的实例.
update_n([_Id|Keys], Table, Data) ->
  [_Name, Id | Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:update(Table, Kv, [{id, Id}]),
  model_exec:update_n(SQL),
  ok;
%% 简单列表修改.
update_n(Id, Table, List) ->
  SQL = model_sql:update(Table, List, [{id, Id}]),
  model_exec:update_n(SQL),
  ok.

%% 参数为普通kv
insert_n(Kv, Table) ->
  SQL = model_sql:insert(Table, Kv),
  model_exec:insert_n(SQL).

%% 参数为记录
insert_n(Keys, Table, Data) when is_list(Keys) ->
  [_Name|Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:insert(Table, Kv),
  model_exec:insert_n(SQL).


%% 参数为普通kv
insert_t(Kv, Table) ->
  SQL = model_sql:insert(Table, Kv),
  model_exec:insert_t(SQL).

%% 参数为记录
insert_t(Keys, Table, Data) when is_list(Keys) ->
  [_Name|Values] = tuple_to_list(Data),
  true = (length(Keys) == length(Values)),
  Kv = kv_list(Keys, Values),
  SQL = model_sql:insert(Table, Kv),
  model_exec:insert_t(SQL).

delete_n({in, IdList}, Table) ->
  SQL = model_sql:delete(Table, {id, in, IdList}),
  model_exec:delete_n(SQL),
  ok;
delete_n(KvList, Table) when is_list(KvList) ->
  SQL = model_sql:delete(Table, KvList),
  model_exec:delete_n(SQL),
  ok;
delete_n(Id, Table) ->
  SQL = model_sql:delete(Table, [{id, Id}]),
  model_exec:delete_n(SQL),
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

%% insert_delete_test() ->
%%   Poll = start(#db_conf{username="root", password="", database="slg_model"}),
%%   delete_n(Poll, {in, [23]}, buildings),
%%   delete_n(Poll, [{user_id, 2001}], buildings),
%%   insert_n(Poll, [{id, 23}, {type, <<"gog">>}, {level, 23}], buildings),
%%   ok.

max(Column, Table) ->
  L = model_sql:max(Table, Column, all),
  {data, Result} = model_exec:exec_n(L),
  [[Id]] = mysql:get_result_rows(Result),
  Id.

max_id(Table) ->
  case ?MODULE:max(id, Table) of
    undefined -> 0;
    Id ->  Id
  end.

count(Cond, Table) ->
  Sql = model_sql:count(Table, Cond),
  {data, Result} = model_exec:exec_n(Sql),
  [[Count]] = mysql:get_result_rows(Result),
  Count.
%% count_test() ->
%%   Poll = start(#db_conf{username="root", password="", database="slg_model"}),
%%   S = count(all, buildings),
%%   true = is_integer(S),
%%   S.

pos_attr(Attrs, List) ->
%%  io:format("~p ~p", [Attrs, List]),
  lists:map(fun({Pos, V}) ->
                %%io:format("pos ~p attr ~p~n", [Pos, Attrs]),
                N = lists:nth(Pos-1, Attrs),
                {N, V}
            end, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 动态生成模块

%% 有了这个函数就不用手写model层了。
module_new(Key) ->
  Atom = Key,
  ModuleAtom = atom_prefix(Atom, model),
  DbAtom = record_atom(Key),
  M0 = spt_smerl:new(ModuleAtom),
  Table = Key,

  SelectFun01 = io_lib:format("
     select_n(Cond, Limit) ->
     model:select_n(~p, ~p, all, Cond, Limit).",
                              [DbAtom, Table]),
  SelectFun0 = lists:flatten(SelectFun01),
  %% 普通查询函数.
  SelectFun1 = io_lib:format("
     select_n(Cond) when is_list(Cond) ->
     model:select_n(~p, ~p, all, Cond);
     select_n(UserId) when is_integer(UserId) ->
     model:select_n(~p, ~p, all, [{user_id, UserId}]).",
                             [DbAtom, Table, DbAtom, Table]),
  SelectFun = lists:flatten(SelectFun1),
  UpdateFun1 = io_lib:format("update_n({Id, List}) ->
             List1 = model:pos_attr(model_record:m(~p), List),
             model:update_n(Id, ~p, List1);
            update_n(Db) ->
     model:update_n(model_record:m(~p), ~p, Db).", [Key, Table, Key, Table]),
  UpdateFun = lists:flatten(UpdateFun1),
  InsertFun1 = io_lib:format("insert_n(Db) ->
     model:insert_n(model_record:m(~p), ~p, Db).", [Key, Table]),
  InsertFun = lists:flatten(InsertFun1),
  DeleteFun1 = io_lib:format("delete_n(ID)  ->
     model:delete_n(ID, ~p).
   ", [Table]),
  DeleteFun = lists:flatten(DeleteFun1),
  {ok, M1} = spt_smerl:add_func(M0, SelectFun0),
  {ok, M2} = spt_smerl:add_func(M1, SelectFun),
  {ok, M3} = spt_smerl:add_func(M2, UpdateFun),
  {ok, M4} = spt_smerl:add_func(M3, InsertFun),
  {ok, M5} = spt_smerl:add_func(M4, DeleteFun),
  SelectFunt01 = io_lib:format("select_t(Cond, Limit) ->
  model:select_t(~p, ~p, all, Cond, Limit).
  ", [DbAtom, Table]),
  SelectFunt0 = lists:flatten(SelectFunt01),
  %% 事务查询函数
  SelectFunt1 = io_lib:format("select_t(Cond) when is_list(Cond) ->
     model:select_t(~p, ~p, all, Cond);
     select_t(UserId) when is_integer(UserId) ->
     model:select_t(~p, ~p, all, [{user_id, UserId}]).",
                              [DbAtom, Table, DbAtom, Table]),
  SelectFunt = lists:flatten(SelectFunt1),
  UpdateFunt1 = io_lib:format("update_t({Id, List}) ->
     List1 = model:pos_attr(model_record:m(~p), List),
     model:update_t(Id, ~p, List1);
    update_t(Db) ->
     model:update_t(model_record:m(~p), ~p, Db).", [Key, Table, Key, Table]),
  UpdateFunt = lists:flatten(UpdateFunt1),
  InsertFunt1 = io_lib:format("insert_t(Db) ->
     model:insert_t(model_record:m(~p), ~p, Db).", [Key, Table]),
  InsertFunt = lists:flatten(InsertFunt1),
  DeleteFunt1 = io_lib:format("delete_t(ID) ->
     model:delete_t(ID, ~p).", [Table]),
  DeleteFunt = lists:flatten(DeleteFunt1),
  {ok, M6} = spt_smerl:add_func(M5, SelectFunt),
  {ok, M7} = spt_smerl:add_func(M6, UpdateFunt),
  {ok, M8} = spt_smerl:add_func(M7, InsertFunt),
  {ok, M9} = spt_smerl:add_func(M8, DeleteFunt),
  {ok, M10} = spt_smerl:add_func(M9, SelectFunt0),
  M11 = spt_smerl:set_exports(M10, [{select_n,1}, {update_n,1},
                                    {insert_n,1}, {delete_n,1},
                                    {select_t,1}, {update_t,1},
                                    {insert_t,1}, {delete_t,1},
                                    {select_n,2}, {select_t, 2}
                                   ]),
  spt_smerl:compile(M11),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 生成映射表.


%% 映射表是一个动态模块，该模块用来做model-key 到起对应的record的属性列表
%% record_info(fields,x)
%% 在生成之前先使用一张ets表获取用户输入，之后一次性生成.

%% 安全新建ets表
safe_create(AtomName, Options) ->
  case ets:info(AtomName) of
    undefined -> ets:new(AtomName, Options);
    _Other -> AtomName
  end.

init_m() ->
  safe_create(slg_model_map, [named_table, public, set, {keypos, 1}]).

%% Key为复数.
add_m(Key, KeyList, Db) ->
  add_m(Key, KeyList, Db, []).

%% 新的add_m函数，可以对表格进行详细配置.
add_m(Key, KeyList, Db, Opts) ->
  ets:insert(slg_model_map, {Key, KeyList, Db, Opts}).

sid_g() ->
  case catch model_config:sid() of
    ID when is_integer(ID) -> ID;
    _ -> -1
  end.

sid_s(Dbc, Opts) ->
  SID = spt_atom:opt(s_id, Opts, 1),
  Worker = spt_atom:opt(worker, Opts, 31),
  M1 = spt_smerl:new(model_config),
  {ok, M2} = spt_smerl:add_func(M1, "sid() -> " ++ integer_to_list(SID) ++ "."),
  {ok, M3} = spt_smerl:add_func(M2, "worker() -> " ++ integer_to_list(Worker) ++ "."),
  {ok, M4} = spt_smerl:add_func(M3, "poll() -> p" ++ integer_to_list(Worker) ++ "_poll."),
  {ok, M5} = spt_smerl:add_func(M4, "dbc() -> " ++ spt_atom:term_to_string(Dbc) ++ "."),
  M6 = spt_smerl:set_exports(M5, [{sid, 0}, {worker, 0}, {poll, 0}, {dbc, 0}]),
  spt_smerl:compile(M6).

%% 生成动态record映射.
gen_m() ->
  All = ets:tab2list(slg_model_map),
  M0 = spt_smerl:new(model_record),
  DFun = fun({K, L, _Dbc, _Opts}, F_0) ->
             F_0 ++ lists:flatten(io_lib:format("m(~p) -> ~p;", [K, L]))
         end,
  Fun = lists:foldl(DFun, "", All),
  Fun1 = Fun ++ "m(_) -> throw(map_error).",
  {ok, M1} = spt_smerl:add_func(M0, Fun1),
  M2 = spt_smerl:set_exports(M1, [{m, 1}]),
  ok = spt_smerl:compile(M2),
  gen_p(),
  gen_h(),
  ok.

%% 创建连接池.
gen_p() ->
  Poll = model_config:poll(),
  Worker = model_config:worker(),
  Dbc = model_config:dbc(),
  model:start(Dbc#db_conf{poll=Poll, worker=Worker}),
  ok.

%% 生成模块管理函数.
gen_h() ->
  All = ets:tab2list(slg_model_map),
  lists:foreach(fun({K, _, Db, Opts}) ->
                    model:module_new(K),
                    {ok, _} = data_holder_super:start_holder(Db, K, Opts)
                end,
                All),
  ok.

trans(Fun) ->
  Poll = model_config:poll(),
  mysql:transaction(Poll, Fun).
