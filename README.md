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

# 使用说明

本章详细描述如何使用这个库.

# 设计细节

或许你对这个系统实现感兴趣，我将对大部分的代码和功能进行描述，有的实现比较搓，如果你愿意可以改进它。

## migrate

migrate的idea来源于RubyOnRails，表格的scheme在开发过程中会不断发生改变，必须要能在开发的过程中保持以前的数据不丢失，因为我实在无法接受测试人员或客户端开发者的不断抱怨，而migrate是比较好的解决方法.

这个migrate的代码位于`model_migrate.erl`，它在指定的数据库中建立了一张表:`migrate`，主要有一个字符串格式的`version`字段，每一次数据库结构的改变，都新建一个新的的migrate文件，使用`migrate:new(test)`，函数结构如下：

    new(Path, Name) when is_atom(Name)

这样会在你指定的path下建立一个新的erlang文件，大概如下:

    -module(test_201306121371025642).
    -export([up/0, down/0]).
    up() ->
    io:format("create up ~n"),
    ok.

    down() ->
    io:format("create down ~n"),
    ok.

文件名后跟上一个时间戳，开发过程中各个程序员都可以建立自己的migarte文件，执行:

    %% 执行增量迁移，如果版本号在`migrate`表中不存在，则该模块的`up`函数会被执行.
    model_migrate:do(Path, UsrName, Password, Database)
    %% 全部重建数据和全部的migrate文件.
    model_migrate:redo(Path, UsrName, Password, Database)

你也可以制定一个模块来单独执行:

    model_migrate:up(test_201306121371025642).
    model_migrate:down(test_201306121371025642).

没有提供rollback功能，因为我还没有遇到需要的情况，如果愿意你可以自行加上，不甚感谢。

推荐在项目代码下建立:`migrate`文件夹存放新建的migrate文件，常用命令为：

    model_migrate:new("./migrate", test).


在migrate模块中你可以调用model层提供的各种代码，也可以调用`model_migrate:source`函数来对一个SQl文件进行直接source.

## ets表

*为什么不使用mnesia*

mnesia是erlang提供的分布式数据库，功能比较强大，但它的功能对于我设计游戏服务器并不是有用，暂时没有需要使用mnesia的理由，如果设计单服的游戏服务器，并且要防止单点故障，那么可能用mnesia比较合适。

再次说明本模块主要为小服的游戏服务器设计的，而非单服。

*为什么不使用进程字典*

很多书上都会告诉你不要使用进程字典，因为有副作用，我起初也不理解，所以花费了一个项目来实践证明了：进程字典确实不好用。

国内有些页游开发团队使用进程字典存储玩家数据，即玩家登陆后将其数据加载到进程字典，然后就在进程字典中操作了，并且按时写回MySql，这样主要的好处是：进程字典操作比较快，纳秒级别；ets操作比较慢，微秒级别。

主要的坏处是：

* 必须使用防御式编程：进程字典数据是随着进程的退出而消失的，所以你的进程将不能再动不动就crash了，于是你会编写大量的防御式编程代码，将erlang本身优雅的函数式编写的奇丑无比；而使用ets，玩家进程不在了ets表数据也不会丢失，可以随便崩。
* 完全写不了单元测试：理论上函数式编程是最好写单元测试，但是你现在每个函数都跟进程上下文相关了，如果要写的话每个单元测试函数你都需要建立起一个新进程，我感觉是没法写，丑的很。

ets也不是太慢，微秒级别，对于小服，拖个几千人就ok了，完全够用，所以我采用ets，也推荐你采用。 ps(神仙道也用ets)。

*进程字典只适合存储临时数据*，那种丢了就丢了，或者session相关的，不需要造成你防御式编程的。

## MySQL层

有的人会问为什么要单独为MySql建立一个层来做，而不直接让程序拼SQL再送过去执行，我的看法是：这样太丑了，如果你用过active_record，你会感觉到不需要自己拼sql的好处，你有可能会拼错，会花时间调试，每个程序员按照自己的拼发做，也非常难于维护(在某个页游项目里看到过，非常让人痛苦)。

active_record是对表格数据的ORM，但是游戏服务器大量只是单表操作，有外键但是不需要约束，所以只需要active_record的一部分功能，你可以说是一个阉割版。

主要包含两个部分，`SQL拼接`和`SQL执行`

### SQL拼接

游戏服务器中SQl操作集中在对单表的增，删，查，改，在`model_sql`中将其封装为比较上层的操作，有以下四个函数分别完成了增删查改的SQL拼接：

* select(Table, Column, Cond)
* update(Table, Column, Cond)
* delete(Table, Cond)
* insert(Table, KvList)

如果你不知怎么用源码模块中有很多测试例子，它们仅仅是字符串拼接而已。

### SQl执行

完后了SQL拼接，接下来要把这些SQL发送给mysql执行，这个部分已经被erlang-mysql-driver做了，所以只需要集成到项目中使用。

在项目的rebar.config有以下依赖关系:

    {mysql, ".*", {git, "git@github.com:dizzyd/erlang-mysql-driver.git", {tag, "HEAD"}}}
在此基础上我编写了`model_exec`模块来完成执行及结果处理，它主要有增删查改四个函数，值得注意的是：

* _t结尾的函数不要指定连接池，用于在事务中使用。
* _n结尾的函数第一个参数为连接池。

为什么需要让其支持事务？？你或许会问，比如一些充值操作等需要立刻持久化，另一方面是如果你很多操作需要串行一系列的SQL，如果不封装到事务里，这写操作之间SQl会被erlang-mysql-driver串行掉，造成总体执行时间延长，非常可怕。

### 组合接口

`model.erl`中组合了基础的SQL操作，除了增删查改外还有以下两个附加操作：

* count 计数
* max_id 计算最大表格id

以下是一部分接口：

    select_n(Poll, Record, Table, Column, Cond)
    insert_n(Poll, Kv, Table)
    insert_n(Poll, Keys, Table, Data)
    delete_n(Poll, {in, IdList}, Table)
    delete_n(Poll, KvList, Table)

幸运的是，这些接口你都不需要直接调用，列在这里只方便你理解和看懂源码。

### 表model模块.

本系统可以为每个表生成一个model模块，你可以看`model_building.erl`，系统可以为你生成一个`model_`+表名的动态模块，可以少写你很多代码，查看`model_building.erl`你会发现里面增删查改样样俱全。

## ets层

slg_model为每一个MySql表都建立一张对应的ets表，如果需要检索数据，首先在ets表中查询，如果表中没有，则调用相应的数据库模块接口将其加载到ets，然后返回，这部分操作在`data_ets`中实现.


# 数据事件

当数据发生变化时，会有以下事件，你可以通过spt_notify订阅这些事件，做成相应的处理:

* slg_m_upt_s {Table, UsrId, Data}: 单条玩家数据发生更新。
* slg_m_upt_s_e {Table, UsrId, Id, List}: 单条玩家数据发生元素更新。
* slg_m_upt_i {Table, Data}: 多条玩家数据发生更新。
* slg_m_upt_i_e {Table, Id, List}： 单条玩家数据发生更新。
* slg_m_del_s {Table, UsrId, Id}： 单条玩家数据发生删除。
* slg_m_del_i {Table, UsrId, Id}: 多条玩家数据发生删除。
* slg_m_add_s {Table, UsrId, Data}: 单条玩家数据增加。
* slg_m_add_i {Table, UsrId, Data}： 多条玩家数据增加。

通过`spt_notify:sub(slg_m_upt_s, Fun/1)`注册这些事件，当时事件发生时Fun/1会被被调用。

