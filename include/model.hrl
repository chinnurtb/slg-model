%% 启动一个连接池的参数配置
-record(db_conf, {
          poll=dbs_pl ::atom(),
          host="localhost" ::list(),
          port=3306 ::list(),
          username="root" ::list(),
          password="" ::list(),
          database="slg_model" ::list(),
          worker=33 ::integer(),
          base=1 ::integer()
         }).

%% MySQL result record:
-record(mysql_result, {
          fieldinfo=[],
          rows=[],
          affectedrows=0,
          insertid=0,
          error="",
          errcode=0,
          errsqlstate=""}).
