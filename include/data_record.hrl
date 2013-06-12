%% 三个例子模型

-record(db_revenue, {
          id = 0 :: integer(),
          user_id = 0 :: integer(),
          last_time = 0 :: integer()
         }).

%% 建筑物品
-record(db_building, {
          id = 0 :: integer(),
          user_id = 0 :: integer(),
          type = <<"">> :: binary(),
          level = 0 :: integer()
         }).

%% cd时间
-record(db_cdtime, {
          id = 0 :: integer(),
          user_id = 0 :: integer(),
          end_time = 0 :: integer(),
          status = 0 :: integer(),
          content = 0 :: integer(),
          type = 0 :: integer()
         }).
