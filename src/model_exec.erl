-module(model_exec).
-compile([export_all]).

-include("model.hrl").
-include_lib("eunit/include/eunit.hrl").

%% 统一的超时处理
exec(Poll, SQL) ->
  io:format("SQL >> ~p~n", [SQL]),
  case catch mysql:fetch(Poll, SQL) of
    {'EXIT', _} -> io:format("exit 2 ~n"), error;
    {timeout, _}-> io:format("timeout 2 ~n"), error;
    _Other -> _Other
  end.
exec(SQL) ->
  io:format("SQL >> ~p~n", [SQL]),
  case catch mysql:fetch(SQL) of
    {'EXIT', _} -> io:format("exit 2 ~n"), error;
    {timeout, _}-> io:format("timeout 2 ~n"), error;
    _Other -> _Other
  end.

%% 普通执行函数以_n为后缀，事务执行以_t为后缀.

%% 不指定poll，用于事务.
select_t(SQL) ->
  {data, Result} = mysql:fetch(SQL),
  mysql:get_result_rows(Result).

%% 不指定poll，用于事务.
select_t(RecordName, SQL) ->
  Rows = select_t(SQL),
  lists:map(fun(R) -> R1=[RecordName | R], list_to_tuple(R1) end, Rows).

%% 执行select语句。
select_n(Poll, SQL) ->
  {data, Result} = exec(Poll, SQL),
  mysql:get_result_rows(Result).

select_n(Poll, RecordName, SQL) when is_atom(Poll) ->
  Rows = select_n(Poll, SQL),
  lists:map(fun(R) -> R1=[RecordName | R], list_to_tuple(R1) end, Rows).

update_t(SQL) ->
  {updated, _Result} = mysql:fetch(SQL),
  ok.
update_n(Poll, SQL) ->
  {updated, _Result} = exec(Poll, SQL),
  ok.

delete_t(SQL) ->
  {updated, _Result} = mysql:fetch(SQL),
  ok.

delete_n(Poll, SQL) ->
  R = exec(Poll, SQL),
  %% io:format("del ~p R:~p~n", [SQL, R]),
  {updated, _Result} = R,
  ok.

insert_t(SQL) ->
  case mysql:fetch(SQL) of
    {updated, _Result} -> ok;
    {error, _Result} -> error
  end.

insert_n(Poll, SQL) ->
  case exec(Poll, SQL) of
    {updated, _Result} -> ok;
    {error, _Result} -> error
  end.
