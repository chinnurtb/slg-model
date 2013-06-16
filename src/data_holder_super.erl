%%% ==================================================================
%%% @author zhuoyikang
%%% 网络端口进程监督者
%%% ==================================================================
-module(data_holder_super).
-behaviour(supervisor).

%% API
-export([init/1, start_holder/2, start_link/0, stop/0]).

-define(MAX_RESTART, 5000000).
-define(MAX_TIME, 60).

%% 开启一个连接服务进程.
start_holder(Db, Key) ->
  supervisor:start_child(?MODULE, [Db, Key]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  Pid = whereis(?MODULE),
  exit(Pid, normal).

init([]) ->
  {ok,
   {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    [{undefined, {data_holder, start_link, []}, temporary, 2000, worker, []}]
   }
  }.
