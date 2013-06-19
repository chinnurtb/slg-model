%% 本模块实现实现数据的访问令牌.
%%
%% 为了互斥的访问玩家数据，每一个表对应一个令牌进程，该进程负责管理各进程对数据的访问权限.
%% 类似于读写锁，slg_model中有以下两个角色：
%%
%% * 读取者：当执行第一次find的时候将申请读取权限，如果这时候已经有写者占有数据则申请失败，立即返回.
%% *       一个系统里可以有多个读者并存，但只能有一写者.
%% * 写者：主要用于对不活跃数据的清除，会清除ets表的数据，因此如果有读者存在，则写者申请失败.
%%
%% 在slg-server中，玩家进程本身在启动时第一次访问数据，用UsrId申请读者，到玩家进程退出的时候释放.
%% 如果有玩家需要访问别人的数据，先申请读者，也在进程退出时释放.
%%
%% ets清理进程在清除数据时先申请数据写，如果成功再对其进行清除.
%%
%% guard的操作全部使用进程字典.
%%
-module(data_guard).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1, stop/1, read_on/3, write_on/3, read_off/3, write_off/3]).

-include_lib("eunit/include/eunit.hrl").
-include("model.hrl").

start_link(Key) ->
  Atom = model:atom(guard, Key),
  gen_server:start_link({local, Atom}, ?MODULE, [Key], []).

stop(Key) ->
  Atom = model:atom(guard, Key),
  gen_server:cast(Atom, stop).

write_on(Key, UsrID, PID) ->
  Atom = model:atom(guard, Key),
  gen_server:call(Atom, {write_on, UsrID, PID}).

read_on(Key, UsrID, PID) ->
  Atom = model:atom(guard, Key),
  gen_server:call(Atom, {read_on, UsrID, PID}).

write_off(Key, UsrID, PID) ->
  Atom = model:atom(guard, Key),
  gen_server:call(Atom, {write_off, UsrID, PID}).

read_off(Key, UsrID, PID) ->
  Atom = model:atom(guard, Key),
  gen_server:call(Atom, {read_off, UsrID, PID}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server api

init([Key]) ->
  {ok, {Key}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

%% 获取身份接口.
handle_call({read_on, UsrID, PID}, _From, State) ->
  R = try_reader(UsrID, PID),
  {reply, R, State};

handle_call({write_on, UsrID, PID}, _From, State) ->
  R = try_writer(UsrID, PID),
  {reply, R, State};

handle_call({read_off, UsrID, PID}, _From, State) ->
  R = try_release_reader(UsrID, PID),
  {reply, R, State};

handle_call({write_off, UsrID, PID}, _From, State) ->
  R = try_release_writer(UsrID, PID),
  {reply, R, State};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(write, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% inner routine

%% 是否有写者?
does_writer(UsrID) ->
  case erlang:get({writer, UsrID}) of
    undefined -> false;
    _ -> true
  end.

%% 是否有读者?
does_reader(UsrID) ->
  case erlang:get({reader, UsrID}) of
    undefined -> false;
    _ -> true
  end.

%% 读者列表
get_reader(UsrID) ->
  case erlang:get({reader, UsrID}) of
    undefined -> [];
    R -> R
  end.

%% 设置读者.
set_reader(UsrID, PID) ->
  Rs = get_reader(UsrID),
  erlang:put({reader, UsrID}, [PID|Rs]).

%% 设置写者.
set_writer(UsrID, PID) ->
  erlang:put({writer, UsrID}, PID).

try_release_reader(UsrID, PID) ->
  Rs = get_reader(UsrID),
  Rs1 = lists:delete(PID, Rs),
  case Rs1 of
    [] -> erlang:erase({reader, UsrID});
    _ -> erlang:put({reader, UsrID}, Rs1)
  end,
  case Rs of
    Rs1 -> not_reader;
    _ -> ok
  end.

try_release_writer(UsrID, PID) ->
  Writer = erlang:get({writer, UsrID}),
  case Writer of
    PID -> erlang:erase({writer, UsrID}), ok;
    _ -> not_writer
  end.


%% 尝试获取读者.
try_reader(UsrID, PID) ->
  case does_writer(UsrID) of
    true -> error;
    false -> set_reader(UsrID, PID),
             ok
  end.

%% 尝试获取读者.
try_writer(UsrID, PID) ->
  case does_reader(UsrID) of
    true -> error;
    false ->
      case does_writer(UsrID) of
        true -> error;
        false ->
          set_writer(UsrID, PID),
          ok
      end
  end.

all_test() ->
  start_link(test),
  ok = read_on(test, 1, p_1),
  ok = read_on(test, 1, p_2),
  error = write_on(test, 1, p_3),
  ok = read_off(test, 1, p_1),
  not_reader = read_off(test, 1, p_3),
  ok = read_off(test, 1, p_2),
  ok = write_on(test, 1, p_3),
  error = write_on(test, 1, p_4),
  not_writer = write_off(test, 1, p_4),
  ok = write_off(test, 1, p_3),
  ok = read_on(test, 1, p_1),
  ok = read_on(test, 1, p_2),
  error = write_on(test, 1, p_3),
  ok = read_off(test, 1, p_1),
  not_reader = read_off(test, 1, p_3),
  ok = read_off(test, 1, p_2),
  ok = write_on(test, 1, p_3),
  error = write_on(test, 1, p_4),
  not_writer = write_off(test, 1, p_4),
  ok = write_off(test, 1, p_3),
  ok.
