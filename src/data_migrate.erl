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

-module(data_migrate).
-export([max_version/0,
         create_migrate_table/0,
         insert/1,
         do_db_cmd/1,
         drop_database/0,
         redo/0,
         up/1,
         down/1,
         do/0,
         new/1,
         grant/2,
         create_database/0]).
-define(SCHEMA_TABLE, "`migrate`").

cmd_exec(Cmd) ->
  io:format("exec ~p~n", [Cmd]),
  os:cmd(Cmd),
  ok.

%% 生成基本的sql执行命令.
cmd_gen({User, Password, Sql}) when length(Password) == 0 ->
  io:format("sql:~p~n", [Sql]),
  MetaCmd = io_lib:format("mysql -u~s -e \"~s ;\" ", [User,  Sql]),
  lists:flatten(MetaCmd);
cmd_gen({UserName, Password, Sql}) when length(Password) > 0 ->
  MetaCmd =io_lib:format("mysql -u~s -p\"~s\" -e \"~s ;\" ", [UserName, Password, Sql]),
  lists:flatten(MetaCmd).

cmd_with_db_gen({UserName, DataBase, Password, Sql}) when length(Password) == 0 ->
  MetaCmd = io_lib:format("mysql -u~s ~s -e \"~s ;\" ", [UserName, DataBase, Sql]),
  lists:flatten(MetaCmd);
cmd_with_db_gen({UserName, DataBase, Password, Sql}) when length(Password) > 0 ->
  MetaCmd =io_lib:format("mysql -u~s -p\"~s\" ~s -e \"~s ;\" ", [UserName, Password, DataBase, Sql]),
  lists:flatten(MetaCmd).

do_db_cmd({Sql}) ->
  {_Host, _Port, UserName, Password, _DbName} = database:config(),
  Cmd = cmd_gen({UserName, Password, Sql}),
  cmd_exec(Cmd);
do_db_cmd({database, Sql}) ->
  {_Host, _Port, UserName, Password, DbName} = database:config(),
  Cmd = cmd_with_db_gen({UserName, DbName, Password, Sql}),
  cmd_exec(Cmd).

%% 执行数据库删除.
drop_database() ->
  {_Host, _Port, _UserName, _Password, DbName} = database:config(),
  DropSql= "drop database "  ++ DbName,
  do_db_cmd({DropSql}).

%% 建立数据库.
create_database() ->
  {_Host, _Port, _UserName, _Password, DbName} = database:config(),
  CreateSql= "create database "  ++ DbName,
  do_db_cmd({CreateSql}).

%% 解析文件名返回相应参数.
parse_module_file_name(FileName) ->
  [ModuleName | _Suffix] = string:tokens(FileName, "."),
  Array = string:tokens(ModuleName, "_"),
  Time = lists:last(Array),
  %% io:format("x ~p~n", [Time]),
  {ModuleName, Time}.

%% 获取所有的migrate文件.
all_migrate_files() ->
  AllFile = os:cmd("ls src/migrate"),
  FileArray = string:tokens(AllFile, "\n"),
  lists:sort(fun(A, B) ->
                 {_, TimeA} = parse_module_file_name(A),
                 {_, TimeB} = parse_module_file_name(B),
                 TimeA < TimeB
             end, FileArray).

grant(Int, W) ->
  S = integer_to_list(Int),
  case length(S) of
    W -> S;
    W2 when W2 > W -> S;
    W2 when W2 < W -> [ "0" || _ <-lists:seq(1, W- W2)] ++ S
  end.

new(Name) when is_atom(Name) ->
  %%Name = atom_to_list(Atom),
  {Year, Month, Day} = date(),
  MetaName = io_lib:format("~p_~p~s~s~s.erl", [Name, Year, grant(Month,2), grant(Day,2), grant(time:current(), 10) ]),
  FileName = lists:flatten(MetaName),
  %% FileName = Name ++ "_" ++ integer_to_list(Year) ++
  %%   integer_to_list(Month) ++ integer_to_list(Day) ++
  %%   integer_to_list(time:current()) ++ ".erl" ,
  MetaCmd = io_lib:format("cat config/migrate_demo.conf > src/migrate/~s", [FileName]),
  Cmd = lists:flatten(MetaCmd),
  cmd_exec(Cmd).

%% 重新执行所有的migate文件.
redo() ->
  drop_database(),
  create_database(),
  create_migrate_table(),
  create_base_table(),
  do().

%% migrate最新的文件.
do() ->
  MaxVersion = max_version(),
  AllFileName = all_migrate_files(),
  io:format("~p~n", [AllFileName]),
  lists:foreach(fun(FileName) ->
                    {Module, Time} = parse_module_file_name(FileName),
                    if
                      Time > MaxVersion ->
                        io:format("version ~p ~p ~p up~n", [Module ,Time, MaxVersion]),
                        Atom = list_to_atom(Module),
                        Atom:up(),
                        insert(Time);
                      true -> do_nothing
                    end
                end, AllFileName).

up(Module) ->
  io:format("up module ~p~n", [Module]),
  Module:up(),
  {_, Time} = parse_module_file_name(atom_to_list(Module) ++ ".erl"),
  insert(Time),
  ok.

down(Module) ->
  io:format("down module ~p~n", [Module]),
  Module:down(),
  ok.

%% 建立数据库migrate表.
create_migrate_table() ->
  Source = "source doc/migrate/migrate.sql",
  do_db_cmd({database, Source}).

%% 初始化数据文件
create_base_table() ->
  Source = "source doc/migrate/db.sql",
  do_db_cmd({database, Source}).

%% 当前的最大版本号.
max_version() ->
  Query = "select max(version) from `migrate`",
  case model:fetch(Query) of
    {ok, [[undefined]]} -> 0;
    {ok, [[MaxVersion]]} -> binary_to_list(MaxVersion);
    _ -> 0
  end.

%% 插入新版本记录
insert(Version) when is_list(Version) ->
  model:insert_without_id(?SCHEMA_TABLE, [{version, Version}]).
