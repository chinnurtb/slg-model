-module(slg_model).
-compile([export_all]).

start() ->
  application:start(slg_support),
  application:start(slg_model).
