# 介绍

slg_model旨在提供这样一个模块：

游戏服务器中，用户登陆的时候会把玩家的数据从数据库总load到内存中，之后就在内存中对玩家数据进行操作，定时写回到数据库进行持久化（这里我选用了MySql数据，在游戏行业用的比mongodb稳定广泛），而这一套机制，可以作为服务框架的一部分存在，不需要框架的使用者编程，只需要他们按照相应的接口编写逻辑即可。

slg_model提供了以下三个重要的功能：

* SQL操作：对底层的直接SQL执行封装了比较友好的上层操作，类似于active_record，但是没有那么强大，也没有表表关联，但是游戏服务器也不需要表表关联或者复杂的查询。
* ets缓存：将用户数的数据暂存在ets表中，通过一定的机制将不活跃的内存清除掉，用户在使用时不需要知道ets和MySql的存在，比如查找数据，系统先会在ets表中查找，如果不存在则cache不命中，再从MySql中查找。
* 自动回写和同步：在内存中插入或者修改删除了数据，系统会自动将其同步到MySql，同步时间间隔大概为1分钟，对于每个表，都开了两个erlang-mysql-driver连接池，一个读(3worker)，一个写(1worker).
* migrate：在游戏开发的过程中，常常会对表结构进行修改，比较土的方法是删除重建整个数据库，而使用migrate对表格进行修改会比较方便，完全仿照Ruby On Rails的migrate做，但是不强大，migrate函数中只能执行SQL，没有封装上层操作。

*规则约定*

使用slg_model必须要遵守它的命名规则，而不过多的使用配置，有以下规则：

* ets表名使用复数:buildings, cdtimes...
* MySql表名使用复数：buildings, cdtimes
* 表代理模块使用单数，并以model_为前缀：model_building， model_cdtime
* 记录record使用单数，并以db_为前缀:db_building，db_cdtime
* 每个表行都以一个record作为映射定义，这个record的字段和其顺序必须要和MySQL表中定义的一样。
* record中字段如果是字符串不使用list类型，而要使用binary类型，因为这样对MsSql或者erlang本身更友好.

# 数据事件

当数据发生变化时，会有以下事件，你可以通过spt_notify订阅这些事件，做出相应的处理:

* slg_m_upt_s {Table, UsrId, Data}: 单条玩家数据发生更新。
* slg_m_upt_s_e {Table, UsrId, Id, List}: 单条玩家数据发生元素更新。
* slg_m_upt_i {Table, Data}: 多条玩家数据发生更新。
* slg_m_upt_i_e {Table, Id, List}： 单条玩家数据发生更新。
* slg_m_del_s {Table, UsrId, Id}： 单条玩家数据发生删除。
* slg_m_del_i {Table, UsrId, Id}: 多条玩家数据发生删除。
* slg_m_add_s {Table, UsrId, Data}: 单条玩家数据增加。
* slg_m_add_i {Table, UsrId, Data}： 多条玩家数据增加。

通过`spt_notify:sub(slg_m_upt_s, Fun/1)`注册这些事件，当时事件发生时Fun/1会被被调用。

