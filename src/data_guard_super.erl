%%% ==================================================================
%%% @author zhuoyikang
%%% 网络端口进程监督者
%%% ==================================================================
-module(data_guard_super).
-behaviour(supervisor).

%% API
-export([init/1, start_guard/1, start_link/0, stop/0]).

-define(MAX_RESTART, 5000000).
-define(MAX_TIME, 60).

%% 开启一个连接服务进程.
start_guard(Key) ->
  supervisor:start_child(?MODULE, [Key]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  Pid = whereis(?MODULE),
  exit(Pid, normal).

init([]) ->
  {ok,
   {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    [{undefined, {data_guard, start_link, []}, temporary, 2000, worker, []}]
   }
  }.
