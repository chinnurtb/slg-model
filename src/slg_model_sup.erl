-module(slg_model_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Clear = {
    data_clear_super,
    {supervisor, start_link, [{local, data_clear_super}, data_clear_super, []]},
    permanent,
    infinity,
    supervisor,
    []
   },
  Writer = {
    data_writer_super,
    {supervisor, start_link, [{local, data_writer_super}, data_writer_super, []]},
    permanent,
    infinity,
    supervisor,
    []
   },
  Data = {
    data_holder_super,
    {supervisor, start_link, [{local, data_holder_super}, data_holder_super, []]},
    permanent,
    infinity,
    supervisor,
    []
   },
  Guard = {
    data_guard_super,
    {supervisor, start_link, [{local, data_guard_super}, data_guard_super, []]},
    permanent,
    infinity,
    supervisor,
    []
   },
  {ok, { {one_for_one, 5, 10}, [Clear, Data, Guard, Writer]} }.
