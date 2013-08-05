%% 一个模版模块，slg_model将会按表生成相应的模块.
-module(model_building).
-export([select_n/1, update_n/1, insert_n/1, delete_n/1, select_n/2,
         select_t/2, select_t/1, update_t/1, insert_t/1, delete_t/1]).

-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").

select_n(Cond, Limit) ->
  model:select_n(model_config:poll(), db_building, buildings, all, Cond, Limit).
select_n(Cond) when is_list(Cond) ->
  model:select_n(model_config:poll(), db_building, buildings, all, Cond);
select_n(UserId) ->
  model:select_n(model_config:poll(), db_building, buildings, all, [{user_id, UserId}]).
update_n({Id, List}) ->
  List1 = model:pos_attr(model_record:m(buildings), List),
  model:update_n(model_config:poll(), Id, buildings, List1);
update_n(DbBuilding) ->
  model:update_n(model_config:poll(), model_record:m(buildings), buildings, DbBuilding).
insert_n(DbBuilding) ->
  model:insert_n(model_config:poll(), model_record:m(buildings), buildings, DbBuilding).
delete_n(P) ->
  model:delete_n(model_config:poll(), P, buildings).

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
