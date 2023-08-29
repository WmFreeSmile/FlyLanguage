
Dim Shared 临界资源_内存池 As CRITICAL_SECTION
Dim Shared 临界资源_线程表 As CRITICAL_SECTION
Dim Shared 临界资源_指令执行 As CRITICAL_SECTION

Dim Shared 装载路径 As String
Dim Shared 装载目录 As String
Dim Shared 装载文件名 As String

Dim Shared 主线程ID As DWORD
Dim Shared 主线程句柄 As HANDLE
Dim Shared 主线程返回值地址 As 容器类型 Ptr
Dim Shared 主线程基地址 As Integer
Dim Shared 主线程模块索引 As Integer

Dim Shared 调试模式 As BOOL

Dim Shared 附加函数表() As Any Ptr

'Dim Shared 内存区域 As Any Ptr
'Dim Shared 内存范围 As LongInt
'
'Dim Shared 未分配堆表() As 内存块信息
'Dim Shared 已分配堆表() As 内存块信息

Dim Shared 指令表() As 指令类型
Dim Shared 线程表() As 线程信息

Dim Shared 模块表() As 模块类型
Dim Shared 导出表() As 导出类型

Dim Shared 窗口表() As 窗口类型
Dim Shared 结构表() As 结构类型

Dim Shared 交互信息 As 交互信息类型
Dim Shared 库列表() As 库类型
