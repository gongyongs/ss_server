使用sys_log注意事项：
1.前提：在bin中的相应文件中加入export PUBLIC_LOG_PATH=$ROOT 
 
2._app.erl中加入 ok = dd_util:ensure_app_started(sys_log),
 
3.有五个接口：SYS_LOG,ADMIN_LOG,PLAYER_LOG,COM_LOG,ERROR_LOG，其中前三个函数参数为列表形式，后两个参数类似file_log_info和file_log_error函数。

SYS_LOG：系统操作日志，使用示例：？SYS_LOG([...]}
ADMIN_LOG：后台管理日志，使用示例：？ADMIN_LOG([...])
PLAYER_LOG：玩家操作日志，使用示例：？PLAYER_LOG([...])
COM_LOG：公用操作日志，记录流水操作，功能与用法类似于FILE_LOG_INFO,使用示例：？COM_LOG("...",[...])
ERROR_LOG：错误日志，记录错误信息，功能与用法类似于FILE_LOG_ERROR,使用示例：？ERROR_LOG("...",[...])

SYS_LOG,ADMIN_LOG,PLAYER_LOG接口写入时，列表格式：
【操作类型，操作者，被操作者，节点名称，操作名，操作值，操作结果，操作前的值，操作后的值，备注信息】    （10个类型）

其中前7个类型为固定格式，后三个视不同功能操作数据而定，固定格式说明如下：

操作类型：
（查询query，修改（更新）update，添加add，删除delete 等） 

操作者，被操作者：
系统（system)，后台管理（admin），玩家(uin) （所有玩家 all）

节点名称：
admin,cache,database,gateway,guid,http_proc,log,mail,master ,ranking ,session ,time 

操作名：
功能函数名称

操作结果：
成功（success），失败（fail）

注：
所有操作固定格式部分相同，其余部分视不同功能而定

  