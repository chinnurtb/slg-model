%% 一个模版模块，slg_model将会按表生成相应的模块.
-module(model_building).
-export([select/2, update/2, insert/2, delete/2,
         select/1, update/1, insert/1, delete/1]).

-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").

select(Poll, Cond) when is_list(Cond) ->
  model:select_n(Poll, db_building, buildings, all, Cond);
select(Poll, UserId) ->
  model:select_n(Poll, db_building, buildings, all, [{user_id, UserId}]).

update(Poll, DbBuilding) ->
  model:update_n(Poll, model_record:m(buildings), buildings, DbBuilding).

insert(Poll, DbBuilding) ->
  model:insert_n(Poll, model_record:m(buildings), buildings, DbBuilding).

delete(Poll, P) ->
  model:delete_n(Poll, P, buildings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 通过外键查询
select(Cond) when is_list(Cond) ->
  model:select_t(db_building, buildings, all, Cond);
select(UserId) ->
  model:select_t(db_building, buildings, all, [{user_id, UserId}]).

update(DbBuilding) ->
  model:update_t(model_record:m(buildings), buildings, DbBuilding).

insert(DbBuilding) ->
  model:insert_t(model_record:m(buildings), buildings, DbBuilding).

delete(ID) ->
  model:delete_t(ID, buildings).
