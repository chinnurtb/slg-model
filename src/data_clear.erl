%% 不活跃数据清除进程.
%% 每10分钟检查一次.
-module(data_clear).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1]).

%% 启动模块.
start_link(Table) ->
  Atom = model:atom(clear, Table),
  gen_server:start_link({local, Atom}, ?MODULE, [Table], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server api

-define(time_clear_span, 3000). %% 10分钟 600000

init([Key]) ->
  erlang:send_after(?time_clear_span, self(), clear_time),
  {ok, {Key}}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

%% 执行清除操作
handle_info(clear_time, State = {Key}) ->
  data:clear(Key),
  erlang:send_after(?time_clear_span, self(), clear_time),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 清除函数.
