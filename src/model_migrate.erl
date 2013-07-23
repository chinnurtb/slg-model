%%% ==================================================================
%%% @doc migrate模块，维护数据库.
%%%
%%% redo()          重建数据库，并执行所有操作.
%%% do()            执行新加的migrate命令.
%%% new(FileName)   新建migrate文件，位于src/migrate/.
%%% up(Version)     根据版本号执行up操作.
%%% down(Version)   根据版本号执行down操作.
%%%
%%% @end
%%% ==================================================================
-module(model_migrate).
-compile([export_all]).
-include("model.hrl").

%% 执行:os:cmd
cmd_exec(Cmd) ->
  io:format("exec ~p~n", [Cmd]),
  os:cmd(Cmd).

%% 生成基本的sql执行命令.
cmd_gen({HostName, User, Password, Sql}) when length(Password) == 0 ->
  MetaCmd = io_lib:format("mysql -h~s -u~s -e \"~s ;\" ", [HostName, User,  Sql]),
  lists:flatten(MetaCmd);
cmd_gen({HostName, UserName, Password, Sql}) when length(Password) > 0 ->
  MetaCmd = io_lib:format("mysql -h~s -u~s -p\"~s\" -e \"~s ;\" ",
                          [HostName, UserName, Password, Sql]),
  lists:flatten(MetaCmd);
cmd_gen({HostName, UserName, DataBase, Password, Sql}) when length(Password) == 0 ->
  MetaCmd = io_lib:format("mysql -h~s -u~s ~s -e \"~s ;\" ",
                          [HostName, UserName, DataBase, Sql]),
  lists:flatten(MetaCmd);
cmd_gen({HostName, UserName, DataBase, Password, Sql}) when length(Password) > 0 ->
  MetaCmd = io_lib:format("mysql -h~s -u~s -p\"~s\" ~s -e \"~s ;\" ",
                          [HostName, UserName, Password, DataBase, Sql]),
  lists:flatten(MetaCmd).

%% 建立数据库
create_db(HostName, DataBase, UserName, Password) ->
  Sql = "create database "  ++ DataBase,
  Cmd = cmd_gen({HostName, UserName, Password, Sql}),
  cmd_exec(Cmd).

%% 删除数据库
drop_db(HostName, DataBase, UserName, Password) ->
  Sql = "drop database "  ++ DataBase,
  Cmd = cmd_gen({HostName, UserName, Password, Sql}),
  cmd_exec(Cmd).

%% migrate版本表.
-define(SCHEMA_TABLE, "`migrate`").

%% 建立migate表
create_migrate_t(HostName, DataBase, UserName, Password) ->
  Source = "source config/migrate.sql",
  Cmd = cmd_gen({HostName, UserName, DataBase, Password, Source}),
  cmd_exec(Cmd).

%%
create_and_drop_test() ->
  create_db("locahost", "nice", "root", ""),
  create_migrate_t("localhost" ,"nice", "root", ""),
  drop_db("locahost", "nice", "root", "").

%% source 某个文件.
source(HostName, DataBase, UserName, Password, Path) ->
  Source = "source " ++ Path,
  Cmd = cmd_gen({HostName, UserName, DataBase, Password, Source}),
  cmd_exec(Cmd).

current() ->
  {MegaSecs, Secs, _} = erlang:now(),
  MegaSecs * 1000000 + Secs.

%% migrate 文件.
new(Path, Name) when is_atom(Name) ->
  {Year, Month, Day} = date(),
  MetaName = io_lib:format("~p_~p~s~s~s.erl", [Name, Year,
                                               grant(Month,2),
                                               grant(Day,2),
                                               grant(current(), 10)]),
  FileName = lists:flatten(MetaName),
  MetaCmd = io_lib:format("cat config/demo.conf > ~p/~s",
                          [Path, FileName]),
  Cmd = lists:flatten(MetaCmd),
  cmd_exec(Cmd).

%% 当前的migrate最大版本号.
max_version() ->
  B = case model:max(migrate, version, migrate) of
        undefined -> <<"0">>;
        B1 -> B1
      end,
  binary_to_list(B).

grant(Int, W) ->
  S = integer_to_list(Int),
  case length(S) of
    W -> S;
    W2 when W2 > W -> S;
    W2 when W2 < W -> [ "0" || _ <-lists:seq(1, W- W2)] ++ S
  end.

%% 获取所有的migrate模块，并排序.
all_migrates(Path) ->
  AllFile = os:cmd("cd " ++ Path ++ " && ls *.erl"),
  FileArray = string:tokens(AllFile, "\n"),
  lists:sort(fun(A, B) ->
                 {_, TimeA} = parse_module_file_name(A),
                 {_, TimeB} = parse_module_file_name(B),
                 TimeA < TimeB
             end, FileArray).


%% 解析文件名返回相应参数.
parse_module_file_name(FileName) ->
  [ModuleName | _Suffix] = string:tokens(FileName, "."),
  Array = string:tokens(ModuleName, "_"),
  Time = lists:last(Array),
  {ModuleName, Time}.

%% 插入新版本记录
insert(Version) ->
  model:insert_n(migrate, [{version, Version}], migrate).

delete(Version) ->
  model:delete_n(migrate, [{version, Version}], migrate).

%% 执行migrate操作
do(Path, HostName, UsrName, Password, Database) ->
  create_db(HostName, Database, UsrName, Password),
  source(HostName, Database, UsrName, Password, Path ++ "/db.sql"),
  create_migrate_t(HostName,Database, UsrName, Password),
  migrate = model:start(#db_conf{poll=migrate, username=UsrName, worker=1,
                                 host=HostName,
                                 password=Password, database=Database}),
  Max = max_version(),
  io:format("max ~p~n", [Max]),
  All = all_migrates(Path),
  Fun = fun(FileName) ->
            {Module, Time} = parse_module_file_name(FileName),
            case model:count(migrate, [{version, Time}], migrate) of
              0 -> Atom = list_to_atom(Module),
                   Atom:up(),
                   insert(Time);
              _ -> do_nothing
            end
        end,
  [Fun(File) || File <- All],
  ok.

%% 重建数据库
redo(Path, HostName, UsrName, Password, Database) ->
  drop_db(HostName, Database, UsrName, Password),
  do(Path, HostName, UsrName, Password, Database),
  ok.

up(Module) ->
  Module:up(),
  {_, Time} = parse_module_file_name(atom_to_list(Module) ++ ".erl"),
  insert(Time),
  ok.

down(Module) ->
  Module:down(),
  {_, Time} = parse_module_file_name(atom_to_list(Module) ++ ".erl"),
  delete(Time),
  ok.
