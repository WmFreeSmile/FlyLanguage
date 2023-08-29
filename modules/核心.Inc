Sub 暂停所有线程()
   
   For i As Integer = LBound(线程表) To UBound(线程表)
      If 线程表(i).线程ID = GetCurrentThreadId() Then
         Continue For
      End If
      SuspendThread(线程表(i).线程句柄)
   Next
End Sub

Sub 恢复所有线程()
   For i As Integer = LBound(线程表) To UBound(线程表)
      If 线程表(i).线程ID = GetCurrentThreadId() Then
         Continue For
      End If
      ResumeThread(线程表(i).线程句柄)
   Next
End Sub

Sub 结束所有线程()
   For i As Integer = LBound(线程表) To UBound(线程表)
      If 线程表(i).线程ID = GetCurrentThreadId() Then
         Continue For
      End If
      TerminateThread(线程表(i).线程句柄 ,0)
   Next
End Sub


Function 加入线程(线程ID As DWORD,线程句柄 As HANDLE) As Integer
   EnterCriticalSection(@临界资源_线程表)  ' 如果有多个线程同时结束，将排队进行
   
   Dim 索引 As Integer
   
   If UBound(线程表) - LBound(线程表) + 1 = 0 Then
      索引 = 0
   Else
      索引 = UBound(线程表) + 1
   End If
   
   ReDim Preserve 线程表(索引)
   
   线程表(索引).线程ID = 线程ID
   线程表(索引).线程句柄 = 线程句柄
   
   LeaveCriticalSection(@临界资源_线程表)
   
   Return 索引
   
End Function

Sub 删除线程(线程ID As DWORD)
   EnterCriticalSection(@临界资源_线程表)
   
   If UBound(线程表) - LBound(线程表) + 1 <> 0 Then
      For i As Integer = LBound(线程表) To UBound(线程表)
         If 线程表(i).线程ID = 线程ID Then
            
            线程表(i)=线程表(UBound(线程表))
            
            ReDim Preserve 线程表(UBound(线程表) -1)
            
            Exit For
         End If
      Next
   End If
   LeaveCriticalSection(@临界资源_线程表)
End Sub

Sub 进入调用(调用栈() As 调用栈类型 ,ByRef 栈顶 As Integer,指针指针 As ULongInt)
   栈顶 = 栈顶 + 1
   调用栈(栈顶).回转地址 = 指针指针
   调用栈(栈顶).重定义容器上限(容器数量 -1)
   调用栈(栈顶).重定义循环栈上限(循环栈尺寸 -1)
   调用栈(栈顶).重定义变量循环栈上限(变量循环栈尺寸 -1)
End Sub

Sub 退出调用(调用栈() As 调用栈类型 ,ByRef 栈顶 As Integer,ByRef 指令指针 As ULongInt)
   指令指针 = 调用栈(栈顶).回转地址
   调用栈(栈顶).回转地址 = 0
   调用栈(栈顶).循环栈顶 = 0
   调用栈(栈顶).变量循环栈顶 = 0
   
   Erase 调用栈(栈顶).容器
   Erase 调用栈(栈顶).循环栈
   Erase 调用栈(栈顶).变量循环栈
   栈顶 = 栈顶 -1
End Sub

Sub 加载模块(ByVal 名称 As String)
   Dim 路径(7) As String
   
   路径(0) = 名称
   路径(1) = 装载目录 + "\" + 名称
   路径(2) = 装载目录 + "\pak\" + 名称
   路径(3) = App.Path + 名称
   路径(4) = App.Path + "pak\" + 名称
   路径(5) = *getenv("FLY_PAK") + "\" + 名称
   路径(6) = *getenv("FLY_HOME") + "\" + 名称
   路径(7) = *getenv("FLY_HOME") + "\pak\" + 名称
   
   For i As Integer = 0 To 7
      If 装载器(路径(i)) = True Then
         Return
      End If
   Next
End Sub

Function 取模块基地址_相对(ByVal 名称 As String) As Integer
   Dim 返回值 As Integer = -1
   
   Dim 路径(7) As String
   
   路径(0) = 名称
   路径(1) = 装载目录 + "\" + 名称
   路径(2) = 装载目录 + "\pak\" + 名称
   路径(3) = App.Path + 名称
   路径(4) = App.Path + "pak\" + 名称
   路径(5) = *getenv("FLY_PAK") + "\" + 名称
   路径(6) = *getenv("FLY_HOME") + "\" + 名称
   路径(7) = *getenv("FLY_HOME") + "\pak\" + 名称
   
   For i As Integer = 0 To 7
      返回值 = 取模块基地址(路径(i))
      If 返回值 <> -1 Then
         Return 返回值
      End If
   Next
   Function = -1
End Function

Function 取模块基地址(路径 As String) As Integer
   For i As Integer = LBound(模块表) To UBound(模块表)
      If 模块表(i).绝对路径 = 路径 Then
         Return 模块表(i).基地址
      End If
   Next
   Return -1
End Function

Function 取导出地址(基地址 As Integer, 名称 As String) As Integer
   For i As Integer = LBound(导出表) To UBound(导出表)
      If 导出表(i).基地址 = 基地址 Then
         
         If 导出表(i).名称 = 名称 Then
            Return 导出表(i).基地址 + 导出表(i).地址
         End If
      End If
   Next
   Return -1
End Function


Function 取模块索引_相对(ByVal 名称 As String) As Integer
   Dim 返回值 As Integer = -1
   Dim 路径(7) As String
   
   路径(0) = 名称
   路径(1) = 装载目录 + "\" + 名称
   路径(2) = 装载目录 + "\pak\" + 名称
   路径(3) = App.Path + 名称
   路径(4) = App.Path + "pak\" + 名称
   路径(5) = *getenv("FLY_PAK") + "\" + 名称
   路径(6) = *getenv("FLY_HOME") + "\" + 名称
   路径(7) = *getenv("FLY_HOME") + "\pak\" + 名称
   
   For i As Integer = 0 To 7
      返回值 = 取模块索引(路径(i))
      If 返回值 <> -1 Then
         Return 返回值
      End If
   Next
   Function = -1
End Function

Function 取模块索引(路径 As String) As Integer
   For i As Integer = LBound(模块表) To UBound(模块表)
      If 模块表(i).绝对路径 = 路径 Then
         Return i
      End If
   Next
   Return -1
End Function

Function IS_GetStr(is_code As UByte) As String
   Select Case is_code
      Case is_nop
         Function = "nop"
      Case is_push
         Function = "push"
      Case is_pop
         Function = "pop"
      Case is_popn
         Function = "popn"
      Case is_jmp
         Function = "jmp"
      Case is_je
         Function = "je"
      Case is_jne
         Function = "jne"
      Case is_ja
         Function = "ja"
      Case is_jae
         Function = "jae"
      Case is_jb
         Function = "jb"
      Case is_jbe
         Function = "jbe"
      Case is_add
         Function = "add"
      Case is_sub
         Function = "sub"
      Case is_mul
         Function = "mul"
      Case is_div
         Function = "div"
      Case is_mod
         Function = "mod"
      Case is_tcall
         Function = "tcall"
      Case is_sleep
         Function = "sleep"
      Case is_ret
         Function = "ret"
      Case is_write
         Function = "write"
      Case is_read
         Function = "read"
      Case is_quit
         Function = "quit"
      Case is_call
         Function = "call"
      Case is_exit
         Function = "exit"
      Case is_str
         Function = "str"
      Case is_msgbox
         Function = "msgbox"
      Case is_stop
         Function = "stop"
      Case is_set
         Function = "set"
      Case is_push_obj_this
         Function = "push_obj_this"
      Case is_node
         Function = "node"
      Case is_bound
         Function = "bound"
      Case is_ptr
         Function = "ptr"
      Case is_set_box
         Function = "set_box"
      Case is_jz
         Function = "jz"
      Case is_jnz
         Function = "jnz"
      Case is_and
         Function = "and"
      Case is_or
         Function = "or"
      Case is_xor
         Function = "xor"
      Case is_not
         Function = "not"
      Case is_band
         Function = "band"
      Case is_bor
         Function = "bor"
      Case is_bxor
         Function = "bxor"
      Case is_bnot
         Function = "bnot"
      Case is_pop_box
         Function = "pop_box"
      Case is_wait
         Function = "wait"
      Case is_round
         Function = "round"
      Case is_left
         Function = "left"
      Case is_right
         Function = "right"
      Case is_mid
         Function = "mid"
      Case is_InStr
         Function = "InStr"
      Case is_InStrRev
         Function = "InStrRev"
      Case is_UCase
         Function = "UCase"
      Case is_LCase
         Function = "LCase"
      Case is_Len
         Function = "Len"
      Case is_ValInt
         Function = "ValInt"
      Case is_ValDou
         Function = "ValDou"
      Case is_LoadWindow
         Function = "LoadWindow"
      Case is_GetQuitMsg
         Function = "GetQuitMsg"
      Case is_PostQuitMsg
         Function = "PostQuitMsg"
      Case is_DoEvents
         Function = "DoEvents"
      Case is_DestroyWindow
         Function = "DestroyWindow"
      Case is_src
         Function = "src"
      Case is_next
         Function = "next"
      Case is_obj
         Function = "obj"
      Case is_join
         Function = "join"
      Case is_del
         Function = "del"
      Case is_data
         Function = "data"
      Case is_GetDataLen
         Function = "GetDataLen"
      Case is_GetDataLeft
         Function = "GetDataLeft"
      Case is_GetDataRight
         Function = "GetDataRight"
      Case is_GetDataMid
         Function = "GetDataMid"
      Case is_InData
         Function = "InData"
      Case is_InDataRev
         Function = "InDataRev"
      Case is_SetDataMid
         Function = "SetDataMid"
      Case is_SpaceData
         Function = "SpaceData"
      Case is_GetDataVal
         Function = "GetDataVal"
      Case is_SetDataVal
         Function = "SetDataVal"
      Case is_ReadFile
         Function = "ReadFile"
      Case is_WriteFile
         Function = "WriteFile"
      Case is_MkDir
         Function = "MkDir"
      Case is_RmDir
         Function = "RmDir"
      Case is_FileCopy
         Function = "FileCopy"
      Case is_FileMove
         Function = "FileMove"
      Case is_FileKill
         Function = "FileKill"
      Case is_FileName
         Function = "FileName"
      Case is_GetLoadDir
         Function = "GetLoadDir"
      Case is_GetLoadName
         Function = "GetLoadName"
      Case is_TerminateThread
         Function = "TerminateThread"
      Case is_ResumeThread
         Function = "ResumeThread"
      Case is_SuspendThread
         Function = "SuspendThread"
      Case is_InitializeCriticalSection
         Function = "InitializeCriticalSection"
      Case is_DeleteCriticalSection
         Function = "DeleteCriticalSection"
      Case is_EnterCriticalSection
         Function = "EnterCriticalSection"
      Case is_LeaveCriticalSection
         Function = "LeaveCriticalSection"
      Case is_Launch
         Function = "Launch"
      Case is_GetPack
         Function = "GetPack"
      Case is_GetPackExport
         Function = "GetPackExport"
      Case is_IsFileExist
         Function = "IsFileExist"
      Case is_IsObj
         Function = "IsObj"
      Case is_sgn
         Function = "sgn"
      Case is_abs
         Function = "abs"
      Case is_int
         Function = "int"
      Case is_fix
         Function = "fix"
      Case is_pow
         Function = "pow"
      Case is_sqr
         Function = "sqr"
      Case is_sin
         Function = "sin"
      Case is_cos
         Function = "cos"
      Case is_tan
         Function = "tan"
      Case is_atn
         Function = "atn"
      Case is_log
         Function = "log"
      Case is_exp
         Function = "exp"
      Case is_randomize
         Function = "randomize"
      Case is_rnd
         Function = "rnd"
      Case is_GetTickCount
         Function = "GetTickCount"
      Case is_other
         Function = "other"
   End Select
End Function


Sub 压入栈 stdcall(栈() As 栈类型, ByRef 栈顶 As Integer, 值 As 栈类型 Ptr)
   压入(栈, 栈顶, *值)
End Sub
Function 弹出栈 stdcall(栈() As 栈类型, ByRef 栈顶 As Integer) As 栈类型 Ptr
   栈顶 = 栈顶 -1
   Function = @栈(栈顶)
End Function

Function 栈取对象 stdcall(栈 As 栈类型 Ptr) As 对象类型 Ptr
   Function = 栈->栈空间.容器值->对象
End Function
Function 栈取容器 stdcall(栈 As 栈类型 Ptr) As 容器类型 Ptr
   Function = 栈->栈空间.容器值
End Function
Function 栈转对象 stdcall(栈 As 栈类型 Ptr) As 对象类型 Ptr
   Function = 栈->转对象()
End Function

Function 下一个 stdcall(容器 As 容器类型 Ptr,容器组索引 As Integer) As 容器类型 Ptr
   Function = 容器->容器组(容器组索引)
End Function

Function 容器取对象 stdcall(容器 As 容器类型 Ptr) As 对象类型 Ptr
   Function = 容器->对象
End Function

Function 对象取整数 stdcall(对象 As 对象类型 Ptr) As LongInt
   Function = 对象->整数值
End Function
Function 对象取文本 stdcall(对象 As 对象类型 Ptr) As ZString Ptr
   Function = StrPtr(对象->文本值)
End Function
Function 对象取小数 stdcall(对象 As 对象类型 Ptr) As Double
   Function = 对象->小数值
End Function
Function 对象取数据 stdcall(对象 As 对象类型 Ptr,ByRef 长度 As Integer) As Any Ptr
   长度 = 对象->数据值.取长度()
   Function = 对象->数据值.取数据指针()
End Function

Function 分配容器 stdcall() As 容器类型 Ptr
   Function = New 容器类型
End Function
Sub 删除容器 stdcall(容器 As 容器类型 Ptr)
   Delete 容器
End Sub

Sub 分配容器节 stdcall(容器 As 容器类型 Ptr, 上限 As Integer)
   容器->分配节(上限)
End Sub
Sub 删除容器节 stdcall(容器 As 容器类型 Ptr, 数量 As Integer)
   容器->删除节(数量)
End Sub

Sub 定义节 stdcall(容器 As 容器类型 Ptr, 模型 As UByte, 库句柄 As HANDLE)
   容器->定义库节(模型, 库句柄)
End Sub

Function 分配对象 stdcall() As 对象类型 Ptr
   Function = New 对象类型
End Function
Sub 删除对象 stdcall(对象 As 对象类型 Ptr)
   Delete 对象
End Sub

Sub 置对象整数 stdcall(对象 As 对象类型 Ptr, 值 As LongInt)
   对象->定义(类型_整数)
   对象->整数值 = 值
End Sub

Sub 置对象文本 stdcall(对象 As 对象类型 Ptr, 值 As ZString Ptr)
   对象->定义(类型_文本)
   对象->文本值 = *值
End Sub

Sub 置对象小数 stdcall(对象 As 对象类型 Ptr, 值 As Double)
   对象->定义(类型_小数)
   对象->小数值 = 值
End Sub

Sub 置对象数据 stdcall(对象 As 对象类型 Ptr, 值 As UByte Ptr, 长度 As Integer)
   对象->定义(类型_数据)
   对象->数据值.设长度(长度)
   memcpy(对象->数据值.取数据指针(), 值, 长度)
End Sub

Function 分配栈 stdcall() As 栈类型 Ptr
   Function = New 栈类型
End Function
Sub 删除栈 stdcall(栈 As 栈类型 Ptr)
   Delete 栈
End Sub

Sub 置栈整数 stdcall(栈 As 栈类型 Ptr, 值 As LongInt)
   栈->类型 = 栈_整数
   栈->栈空间.整数值 = 值
End Sub

Sub 置栈文本 stdcall(栈 As 栈类型 Ptr, 值 As ZString Ptr)
   栈->类型 = 栈_文本
   栈->文本值 = *值
End Sub

Sub 置栈小数 stdcall(栈 As 栈类型 Ptr, 值 As Double)
   栈->类型 = 栈_小数
   栈->栈空间.小数值 = 值
End Sub

Sub 置栈数据 stdcall(栈 As 栈类型 Ptr, 值 As UByte Ptr, 长度 As Integer)
   栈->类型 = 栈_数据
   栈->数据值.设长度(长度)
   memcpy(栈->数据值.取数据指针(), 值, 长度)
End Sub

Function 取容器组上限 stdcall(容器 As 容器类型 Ptr) As Integer 
   Function = UBound(容器->容器组)
End Function

Sub 取容器节模型 stdcall(容器 As 容器类型 Ptr, ByRef 模型 As UByte, ByRef 基地址 As Integer, ByRef 模块索引 As Integer, ByRef 库句柄 As HANDLE)
   模型 = 容器->模型
   If 容器->模块索引 <> -1 Then
      基地址 = 模块表(容器->模块索引).基地址
   Else
      基地址 = -1
   End If
   模块索引 = 容器->模块索引
   库句柄 = 容器->库句柄
End Sub
Sub 定义核心节 stdcall(容器 As 容器类型 Ptr, 模型 As UByte, 基地址 As Integer, 模块索引 As Integer)
   容器->定义(模型, 基地址, 模块索引)
End Sub
Function 取节结构 stdcall(模型 As UByte, 库句柄 As HANDLE) As 库结构类型
   For i As Integer = LBound(库列表) To UBound(库列表)
      If 库列表(i).库句柄 = 库句柄 Then
         For j As Integer = LBound(库列表(i).库信息.结构表) To UBound(库列表(i).库信息.结构表)
            If 库列表(i).库信息.结构表(j).ID = 模型 Then
               Function = 库列表(i).库信息.结构表(j)
            End If
            Exit For
         Next
         Exit For
      End If
   Next
End Function

Function 取核心节结构 stdcall(模型 As UByte, 模块索引 As Integer) As 库结构类型
   Dim 基地址 As Integer
   Dim 返回值 As 库结构类型
   基地址 = 模块表(模块索引).基地址
   
   For i As Integer = LBound(结构表) To UBound(结构表)
      If 结构表(i).ID = 模型 Then
         返回值.ID = 结构表(i).ID
         ReDim 返回值.结构成员表(LBound(结构表(i).结构成员表) To UBound(结构表(i).结构成员表))
         For j As Integer = LBound(结构表(i).结构成员表) To UBound(结构表(i).结构成员表)
            返回值.结构成员表(j).ID = 结构表(i).结构成员表(j).ID
            返回值.结构成员表(j).是否数组 = 结构表(i).结构成员表(j).是否数组
         Next
         Exit For
      End If
   Next
   Function = 返回值
End Function

Sub 容器拷贝 stdcall(目标容器 As 容器类型 Ptr, 源容器 As 容器类型 Ptr, 只实例结构 As BOOL = False) 
   目标容器->拷贝(源容器, 只实例结构)
End Sub

Sub 置容器对象 stdcall(容器 As 容器类型 Ptr, 对象 As 对象类型 Ptr)
   容器->类型 = 容器类型_值
   容器->对象 = 对象
End Sub

Sub 对象拷贝 stdcall(目标对象 As 对象类型 Ptr, 源对象 As 对象类型 Ptr)
   目标对象->拷贝(源对象)
End Sub



Sub 取容器信息 stdcall(容器 As 容器类型 Ptr, ByRef 结构索引 As Integer, ByRef 模块索引 As Integer, ByRef 库句柄 As HANDLE, ByRef 模型 As Integer)
   结构索引 = 容器->结构索引
   模块索引 = 容器->模块索引
   库句柄 = 容器->库句柄
   模型 = 容器->模型
End Sub

Sub 置容器信息 stdcall(容器 As 容器类型 Ptr, ByVal 结构索引 As Integer, ByVal 模块索引 As Integer, ByVal 库句柄 As HANDLE, ByVal 模型 As Integer)
   容器->结构索引 = 结构索引
   容器->模块索引 = 模块索引
   容器->库句柄 = 库句柄
   容器->模型 = 模型
End Sub


Sub 容器组构造 stdcall(容器 As 容器类型 Ptr)
   If 容器->结构索引 <> -1 Then
      结构表(容器->结构索引).构造(容器, 容器->模块索引)
   End If
End Sub
Sub 容器组析构 stdcall(容器 As 容器类型 Ptr)
   If 容器->结构索引 <> -1 Then
      结构表(容器->结构索引).析构(容器, 容器->模块索引)
   End If
End Sub
Sub 容器组拷贝 stdcall(容器 As 容器类型 Ptr, 源 As 容器类型 Ptr) 
   If 容器->结构索引 <> -1 Then
      结构表(容器->结构索引).拷贝(容器, 源, 容器->模块索引)
   End If
End Sub
Sub 取结构默认方法 stdcall(结构索引 As Integer, ByRef 构造函数 As ULongInt, ByRef 析构函数 As ULongInt, ByRef 拷贝函数 As ULongInt)
   构造函数 = 结构表(结构索引).构造函数
   析构函数 = 结构表(结构索引).析构函数
   拷贝函数 = 结构表(结构索引).拷贝函数
End Sub
Function 取栈类型 stdcall(栈 As 栈类型 Ptr) As UByte
   Function = 栈->类型
End Function
Function 取对象类型 stdcall(对象 As 对象类型 Ptr) As UByte
   Function = 对象->类型
End Function

Sub 加载库(ByVal 名称 As String)
   Dim 路径(7) As String
   
   路径(0) = 名称
   路径(1) = 装载目录 + "\" + 名称
   #ifdef __FB_64BIT__
   路径(2) = 装载目录 + "\lib\" + 名称
   #else
   路径(2) = 装载目录 + "\lib32\" + 名称
   #endif
   路径(3) = App.Path + 名称
   #ifdef __FB_64BIT__
   路径(4) = App.Path + "lib\" + 名称
   #else
   路径(4) = App.Path + "lib32\" + 名称
   #endif
   #ifdef __FB_64BIT__
   路径(5) = *getenv("FLY_LIB") + "\" + 名称
   #else
   路径(5) = *getenv("FLY_LIB32") + "\" + 名称
   #endif
   路径(6) = *getenv("FLY_HOME") + "\" + 名称
   #ifdef __FB_64BIT__
   路径(7) = *getenv("FLY_HOME") + "\lib\" + 名称
   #else
   路径(7) = *getenv("FLY_HOME") + "\lib32\" + 名称
   #endif
   
   For i As Integer = 0 To 7
      If 运行库(路径(i)) = True Then
         Return
      End If
   Next
End Sub

Function 运行库(ByVal 路径 As String) As BOOL
   路径 = 取文件绝对路径(路径)
   
   For i As Integer = LBound(库列表) To UBound(库列表)
      If 库列表(i).绝对路径 = 路径 Then
         Return True
      End If
   Next
   
   Dim 句柄 As HANDLE = DyLibLoad(路径)
   
   If 句柄 = 0 Then
      Return False
   End If
   
   Dim Func_LoadLib As Function stdcall(临时交互信息 As 交互信息类型 Ptr,临时库句柄 As HANDLE) As 库信息类型 Ptr
   Func_LoadLib = DyLibSymbol(句柄, "LoadLib")
   If Func_LoadLib = 0 Then
      Return False
   End If
   
   Dim 库信息 As 库信息类型 Ptr
   库信息 = Func_LoadLib(@交互信息, 句柄)
   If 库信息 = 0 Then
      Return False
   End If
   
   Dim 索引 As Integer
   ArrayAdd(库列表, 索引)
   
   库列表(索引).库句柄 = 句柄
   库列表(索引).绝对路径 = 路径
   库列表(索引).库信息 = *库信息
   
   Function = True
End Function

Function 取库句柄_相对(ByVal 名称 As String) As HANDLE
   Dim 返回值 As Integer = -1
   Dim 路径(7) As String
   
   路径(0) = 名称
   路径(1) = 装载目录 + "\" + 名称
   #ifdef __FB_64BIT__
   路径(2) = 装载目录 + "\lib\" + 名称
   #else
   路径(2) = 装载目录 + "\lib32\" + 名称
   #endif
   路径(3) = App.Path + 名称
   #ifdef __FB_64BIT__
   路径(4) = App.Path + "lib\" + 名称
   #else
   路径(4) = App.Path + "lib32\" + 名称
   #endif
   #ifdef __FB_64BIT__
   路径(5) = *getenv("FLY_LIB") + "\" + 名称
   #else
   路径(5) = *getenv("FLY_LIB32") + "\" + 名称
   #endif
   路径(6) = *getenv("FLY_HOME") + "\" + 名称
   #ifdef __FB_64BIT__
   路径(7) = *getenv("FLY_HOME") + "\lib\" + 名称
   #else
   路径(7) = *getenv("FLY_HOME") + "\lib32\" + 名称
   #endif
   
   For i As Integer = 0 To 7
      返回值 = 取库句柄(路径(i))
      If 返回值 <> -1 Then
         Return 返回值
      End If
   Next
   Function = -1
End Function

Function 取库句柄(ByVal 路径 As String) As HANDLE
   路径 = 取文件绝对路径(路径)
   
   For i As Integer = LBound(库列表) To UBound(库列表)
      If 库列表(i).绝对路径 = 路径 Then
         Return 库列表(i).库句柄
      End If
   Next
   Return -1
End Function

Sub 释放库(句柄 As HANDLE)
   For i As Integer = LBound(库列表) To UBound(库列表)
      If 库列表(i).库句柄 = 句柄 Then
         Dim Func_FreeLib As Sub stdcall()
         
         Func_FreeLib = DyLibSymbol(句柄, "FreeLib")
         If Func_FreeLib <> 0 Then
            Func_FreeLib()
         End If
         
         ArrayDel(库列表, i)
         Exit For
      End If
   Next
End Sub

Function 取引用地址(句柄 As HANDLE, 名称 As String) As Integer
   For i As Integer = LBound(库列表) To UBound(库列表)
      If 库列表(i).库句柄 = 句柄 Then
         For j As Integer = LBound(库列表(i).库信息.函数表) To UBound(库列表(i).库信息.函数表)
            If *库列表(i).库信息.函数表(j).名称 = 名称 Then
               Return 库列表(i).库信息.函数表(j).地址
               Exit For
            End If
         Next
         Exit For
      End If
   Next
   Function = -1
End Function

Sub 调用库(栈() As 栈类型,ByRef 栈顶 As Integer,函数 As Sub stdcall(栈() As 栈类型, ByRef 栈顶 As Integer))
   函数(栈(), 栈顶)
End Sub
