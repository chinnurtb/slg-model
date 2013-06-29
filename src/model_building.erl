%% 一个模版模块，slg_model将会按表生成相应的模块.
-module(model_building).
-export([select_n/2, update_n/2, insert_n/2, delete_n/2,
         select_n/3, select_t/2,
         select_t/1, update_t/1, insert_t/1, delete_t/1]).

-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").

select_n(Poll, Cond, Limit) ->
  model:select_n(Poll, db_building, buildings, all, Cond, Limit).

select_n(Poll, Cond) when is_list(Cond) ->
  model:select_n(Poll, db_building, buildings, all, Cond);
select_n(Poll, UserId) ->
  model:select_n(Poll, db_building, buildings, all, [{user_id, UserId}]).

update_n(Poll, {Id, List}) ->
  List1 = model:pos_attr(model_record:m(buildings), List),
  model:update_n(Poll, Id, buildings, List1);
update_n(Poll, DbBuilding) ->
  model:update_n(Poll, model_record:m(buildings), buildings, DbBuilding).

insert_n(Poll, DbBuilding) ->
  model:insert_n(Poll, model_record:m(buildings), buildings, DbBuilding).

delete_n(Poll, P) ->
  model:delete_n(Poll, P, buildings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 通过外键查询
select_t(Cond, Limit) ->
  model:select_t(db_building, buildings, all, Cond, Limit).

select_t(Cond) when is_list(Cond) ->
  model:select_t(db_building, buildings, all, Cond);
select_t(UserId) ->
  model:select_t(db_building, buildings, all, [{user_id, UserId}]).

update_t({Id, List}) ->
  List1 = model:pos_attr(model_record:m(buildings), List),
  model:update_t(Id, buildings, List1);

update_t(DbBuilding) ->
  model:update_t(model_record:m(buildings), buildings, DbBuilding).

insert_t(DbBuilding) ->
  model:insert_t(model_record:m(buildings), buildings, DbBuilding).

delete_t(ID) ->
  model:delete_t(ID, buildings).
