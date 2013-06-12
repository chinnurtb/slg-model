-module(test_201306121371025642).
-export([up/0, down/0]).

up() ->
  io:format("create up ~n"),
  ok.

down() ->
  io:format("create down ~n"),
  ok.
