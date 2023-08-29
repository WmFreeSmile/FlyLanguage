Function main() As Integer
   交互信息.Fly_压入栈=@压入栈
   交互信息.Fly_弹出栈=@弹出栈
   交互信息.Fly_栈取对象=@栈取对象
   交互信息.Fly_栈取容器=@栈取容器
   交互信息.Fly_栈转对象=@栈转对象
   交互信息.Fly_下一个=@下一个
   交互信息.Fly_容器取对象=@容器取对象
   交互信息.Fly_对象取整数=@对象取整数
   交互信息.Fly_对象取文本=@对象取文本
   交互信息.Fly_对象取小数=@对象取小数
   交互信息.Fly_对象取数据=@对象取数据
   交互信息.Fly_分配容器=@分配容器
   交互信息.Fly_删除容器=@删除容器
   交互信息.Fly_分配容器节=@分配容器节
   交互信息.Fly_删除容器节=@删除容器节
   交互信息.Fly_定义节=@定义节
   交互信息.Fly_分配对象=@分配对象
   交互信息.Fly_删除对象=@删除对象
   交互信息.Fly_置对象整数=@置对象整数
   交互信息.Fly_置对象文本=@置对象文本
   交互信息.Fly_置对象小数=@置对象小数
   交互信息.Fly_置对象数据=@置对象数据
   交互信息.Fly_分配栈=@分配栈
   交互信息.Fly_删除栈=@删除栈
   交互信息.Fly_置栈整数=@置栈整数
   交互信息.Fly_置栈文本=@置栈文本
   交互信息.Fly_置栈小数=@置栈小数
   交互信息.Fly_置栈数据=@置栈数据
   交互信息.Fly_取容器组上限 = @取容器组上限
   交互信息.Fly_取容器节模型 = @取容器节模型
   交互信息.Fly_定义核心节 = @定义核心节
   交互信息.Fly_取节结构 = @取节结构
   交互信息.Fly_取核心节结构 = @取核心节结构
   交互信息.Fly_容器拷贝 = @容器拷贝
   交互信息.Fly_置容器对象 = @置容器对象
   交互信息.Fly_对象拷贝 = @对象拷贝
   交互信息.Fly_取容器信息 = @取容器信息
   交互信息.Fly_置容器信息 = @置容器信息
   交互信息.Fly_容器组构造 = @容器组构造
   交互信息.Fly_容器组析构 = @容器组析构
   交互信息.Fly_容器组拷贝 = @容器组拷贝
   交互信息.Fly_取结构默认方法 = @取结构默认方法
   交互信息.Fly_取栈类型 = @取栈类型
   交互信息.Fly_取对象类型 = @取对象类型
   
   注册默认窗口类()
   
   InitializeCriticalSection(@临界资源_内存池)
   InitializeCriticalSection(@临界资源_线程表)
   InitializeCriticalSection(@临界资源_指令执行)
   
   ReDim 附加函数表(0)
   
   Dim 返回值 As LongInt
   
   #if 0
   '装载路径 = "D:\UserDir\Desktop\Fly\Projects\使用栈和队列库\compile\新的Fly工程.fly_app"
   '装载路径 = "D:\UserDir\Desktop\Fly\Projects\丝滑窗口\compile\丝滑窗口.fly_app"
   '装载路径 = "D:\UserDir\Desktop\Fly\Projects\输出爱心\compile\输出爱心.fly_app"
   '装载路径 = "D:\UserDir\Desktop\Fly\Projects\列举语句的应用\compile\新的Fly工程.fly_app"
   '装载路径 = "D:\UserDir\Desktop\Fly\Projects\中文大写数字转阿拉伯数字\compile\新的Fly工程.fly_app"
   装载路径 = "D:\UserDir\Desktop\Fly\Projects\新的Fly工程\compile\新的Fly工程.fly_app"
   
   If 装载器(装载路径) = False Then
      FlyDebug("装载失败")
      Return 0
   End If
   
   主线程基地址 = 取模块基地址_相对(装载路径)
   主线程模块索引 = 取模块索引_相对(装载路径)
   
   装载目录 = 文件取目录(装载路径)
   装载文件名=*PathFindFileNameA(装载路径)
   部署主线程(返回值)
   
   Print "主线程已结束，返回值" ,返回值
   Sleep
   
   DeleteCriticalSection(@临界资源_内存池)
   DeleteCriticalSection(@临界资源_线程表)
   DeleteCriticalSection(@临界资源_指令执行)
   ExitProcess(0)
   #endif
   
   Dim position As Integer, arg As String

   position = 1
   Do
      arg = Command( position )
      If( Len( arg ) = 0 ) Then
         Exit Do
      End If
      
      'Print "argument " & position & ": """ & arg & """"
      
      If position = 1 Then
         If FileLen(arg) > 0 Then
            装载路径 = arg
            
            Exit Do
         End If
      End If
      
      position += 1
   Loop

   If ( position = 1 ) Then
      'Print "(no arguments)"
      
      
   End If
   
   If Len(装载路径) <> 0 Then
      If 装载器(装载路径) = False Then
         FlyDebug("装载失败")
         Return 0
      End If
      
      主线程基地址 = 取模块基地址(装载路径)
      主线程模块索引 = 取模块索引_相对(装载路径)
      装载目录 = 文件取目录(装载路径)
      装载文件名=*PathFindFileNameA(装载路径)
      部署主线程(返回值)
   End If
   
   DeleteCriticalSection(@临界资源_内存池)
   DeleteCriticalSection(@临界资源_线程表)
   DeleteCriticalSection(@临界资源_指令执行)
   
   ExitProcess(Cast(Integer ,返回值))
   
   Function = 返回值
End Function


Sub 部署主线程(ByRef 返回值 As LongInt)
   Dim 初始IP As ULongInt = 0
   
   Dim 初始执行栈() As 栈类型
   
   Dim 返回值容器 As 容器类型
   返回值容器.重置()
   
   主线程返回值地址 = @返回值容器
   
   部署线程(主线程基地址,主线程模块索引, 初始执行栈(), 初始IP, 主线程句柄, 主线程ID, 主线程返回值地址)
   
   If 调试模式 Then
      发送调试信息(Fly_MainThreadStart ,主线程ID)
   End If
   
   WaitForSingleObject(主线程句柄 , -1)
   返回值 = 主线程返回值地址->对象->强转类型(类型_整数).整数值
   
   ' 回收虚拟机信息数据   
   
   For i As Integer = 0 To UBound(模块表)
      CloseHandle(模块表(i).文件号)
   Next
   
   For i As Integer = 0 To UBound(库列表)
      释放库(库列表(i).库句柄)
   Next
   
   Erase 模块表
   Erase 指令表
   Erase 窗口表
   Erase 结构表
   Erase 导出表
   Erase 库列表
   
   主线程ID = 0
   主线程句柄 = 0
   主线程返回值地址 = 0
   主线程基地址 = 0
End Sub


Sub 发送调试信息(消息 As Integer ,参数1 As Integer = 0 ,参数2 As Integer = 0 ,参数3 As Integer = 0 ,参数4 As Integer = 0)
   '没有写没有写没有写
   
End Sub

Sub 部署线程 Overload(基地址 As Integer,模块索引 As Integer, 初始栈() As 栈类型, 初始地址 As ULongInt, ByRef 返回线程句柄 As HANDLE, ByRef 返回线程ID As DWORD, 返回值地址 As Any Ptr)
   Dim 线程ID As DWORD
   
   Dim 初始信息 As 初始信息类型 Ptr = New 初始信息类型
   
   ReDim 初始信息->初始栈(UBound(初始栈))
   For i As Integer = 0 To UBound(初始栈)
      初始信息->初始栈(i) = 初始栈(i)
   Next
   
   初始信息->模块索引 = 模块索引
   初始信息->基地址 = 基地址
   初始信息->返回值地址 = 返回值地址
   初始信息->初始地址 = 初始地址
   
   Dim 线程句柄 As HANDLE = CreateThread(0 ,0 ,@工作线程 ,初始信息 ,CREATE_SUSPENDED ,@线程ID)
   
   加入线程(线程ID ,线程句柄)
   
   Dim ptr_data2 As Any Ptr = Allocate(SizeOf(DWORD) + SizeOf(HANDLE))
   
   Poke DWORD ,ptr_data2 ,线程ID
   Poke HANDLE,ptr_data2 +SizeOf(DWORD) ,线程句柄
   
   Dim 回收线程句柄 As HANDLE = CreateThread(0 ,0 ,@线程回收 ,ptr_data2 ,0 ,0)
   CloseHandle(回收线程句柄)
   
   If 调试模式 Then
      发送调试信息(Fly_ThreadStart ,线程ID)
   End If
   
   ResumeThread(线程句柄)
   
   返回线程ID = 线程ID
   返回线程句柄 = 线程句柄
End Sub

Function 启动线程 Overload(基地址 As Integer,模块索引 As Integer,栈() As 栈类型 ,栈顶 As Integer ,参数数量 As Integer ,返回值 As Any Ptr ,地址 As ULongInt) As LongInt
   Dim 初始执行栈() As 栈类型
   
   取栈参数(栈() ,栈顶 ,初始执行栈() ,参数数量)
   
   Dim 线程句柄 As HANDLE
   Dim 线程ID As DWORD
   
   部署线程(基地址,模块索引, 初始执行栈(), 地址, 线程句柄, 线程ID, 返回值)
   
   Function = Cast(Integer ,线程句柄)
End Function

Function 装载器(ByVal 路径 As String) As BOOL
   路径 = 取文件绝对路径(路径)
   
   Dim 基地址 As ULongInt = -1
   Dim 文件号 As HANDLE = -1
   For i As Integer = LBound(模块表) To UBound(模块表)
      If 模块表(i).绝对路径 = 路径 Then
         Return True
      End If
   Next
   
   If GetFileLength(StringToCWSTR(路径)) <> 0 Then
      文件号 = CreateFileA(路径 ,GENERIC_READ ,FILE_SHARE_READ ,0 ,OPEN_EXISTING ,FILE_FLAG_SEQUENTIAL_SCAN ,0)
   End If
   
   If 文件号 = -1 Then
      Return False
   End If
   
   Dim toReadSize As DWORD
   
   Dim buffer As UByte Ptr = Allocate(1024)
   
   ReadFile(文件号, buffer, 3, @toReadSize, 0)
   
   If buffer[0] <> 70 OrElse buffer[1] <> 76 OrElse buffer[2] <> 89 Then
      Deallocate(buffer)
      Return False
   End If
   
   ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
   Dim Fly_PE格式版本 As ULong = *Cast(ULong Ptr ,buffer)
   
   If Fly_PE格式版本 = 1 Then
      ReadFile(文件号 ,buffer ,3 ,@toReadSize ,0)
      
      If buffer[0] = 80 AndAlso buffer[1] = 65 AndAlso buffer[2] = 75 Then 'pack
         
      ElseIf buffer[0] = 65 AndAlso buffer[1] = 80 AndAlso buffer[2] = 80 Then 'App
         
      Else
         Deallocate(buffer)
         Return False
      End If
      
      ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
      Dim 要求虚拟机最低版本 As ULong = *Cast(ULong Ptr ,buffer)
      
      If 要求虚拟机最低版本 > 虚拟机版本 Then
         Deallocate(buffer)
         Return False
      End If
      
      Dim 窗口数_头 As Integer
      Dim 结构数_头 As Integer
      Dim 导出数_头 As Integer
      
      Dim 窗口数_尾 As Integer
      Dim 结构数_尾 As Integer
      Dim 导出数_尾 As Integer
      
      Dim 临时数 As ULong
      
      ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
      临时数 = *Cast(ULong Ptr ,buffer)
      
      窗口数_头 = UBound(窗口表) + 1
      窗口数_尾 = 窗口数_头 + 临时数 -1
      
      ReDim Preserve 窗口表(窗口数_尾)
      For i As Integer = 窗口数_头 To 窗口数_尾
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).ID = *Cast(ULong Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).左边 = *Cast(ULong Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).顶边 = *Cast(ULong Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).宽度 = *Cast(ULong Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).高度 = *Cast(ULong Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).可视 = ( *Cast(ULong Ptr ,buffer) = 1)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).禁止 = ( *Cast(ULong Ptr ,buffer) = 1)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         临时数 = *Cast(ULong Ptr ,buffer)
         ReadFile(文件号 ,buffer ,临时数 ,@toReadSize ,0)
         窗口表(i).标题 = *Cast(ZString Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).风格 = *Cast(ULong Ptr ,buffer) 
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         窗口表(i).附加风格 = *Cast(ULong Ptr ,buffer) 
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         临时数 = *Cast(ULong Ptr ,buffer) 
         
         重定义控件表(窗口表(i).控件表() ,临时数 -1)
         
         For j As Integer = 0 To 临时数 -1
            ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
            窗口表(i).控件表(j).ID = *Cast(ULong Ptr ,buffer) 
            
            ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
            窗口表(i).控件表(j).类型 = *Cast(ULong Ptr ,buffer) 
            
            Select Case As Const 窗口表(i).控件表(j).类型
               Case 控件类型_按钮
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  临时数 = *Cast(ULong Ptr ,buffer)
                  ReadFile(文件号 ,buffer ,临时数 ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.标题 = *Cast(ZString Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.左边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.顶边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.宽度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.高度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.可视 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.禁止 = ( *Cast(ULong Ptr, buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.风格 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.附加风格 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.事件_被单击 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.事件_获取焦点 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).按钮值.事件_失去焦点 = *Cast(ULongInt Ptr ,buffer)
                  
               Case 控件类型_编辑框
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  临时数 = *Cast(ULong Ptr ,buffer)
                  ReadFile(文件号 ,buffer ,临时数 ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.内容 = *Cast(ZString Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.左边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.顶边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.宽度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.高度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.可视 = ( *Cast(ULong Ptr, buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.禁止 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.风格 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.对齐方式 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.只读 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.纵向滚动条 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.横向滚动条 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.附加风格 = *Cast(ULong Ptr ,buffer) 
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.事件_内容被改变 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.事件_字符输入 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.事件_获取焦点 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).编辑框值.事件_失去焦点 = *Cast(ULongInt Ptr ,buffer)
               Case 控件类型_标签
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  临时数 = *Cast(ULong Ptr ,buffer)
                  ReadFile(文件号 ,buffer ,临时数 ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.标题 = *Cast(ZString Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.左边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.顶边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.宽度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.高度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.可视 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.禁止 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.对齐方式 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.附加风格 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.事件_被单击 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).标签值.事件_被双击 = *Cast(ULongInt Ptr, buffer)
                  
               Case 控件类型_时钟
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).时钟值.时钟周期 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).时钟值.事件_周期事件 = *Cast(ULongInt Ptr, buffer)
                  
               Case 控件类型_图片框
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.左边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.顶边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.宽度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.高度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.可视 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.禁止 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.显示方式 = *Cast(ULong Ptr, buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).图片框值.附加风格 = *Cast(ULong Ptr ,buffer)
                  
               Case 控件类型_选择框
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  临时数 = *Cast(ULong Ptr ,buffer)
                  ReadFile(文件号 ,buffer ,临时数 ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.标题 = *Cast(ZString Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.左边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.顶边 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.宽度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.高度 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.可视 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.禁止 = ( *Cast(ULong Ptr ,buffer) = 1)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.选择方式 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.状态 = *Cast(ULong Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.事件_被单击 = *Cast(ULongInt Ptr ,buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.事件_获取焦点 = *Cast(ULongInt Ptr, buffer)
                  
                  ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
                  窗口表(i).控件表(j).选择框值.事件_失去焦点 = *Cast(ULongInt Ptr, buffer)
                  
            End Select
         Next
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_尺寸被改变 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_将被销毁 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_鼠标左键被按下 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_鼠标左键被放开 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_鼠标右键被按下 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_鼠标右键被放开 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_获取焦点 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_失去焦点 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_按下某键 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_放开某键 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_创建完毕 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_被双击 = *Cast(ULongInt Ptr, buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         窗口表(i).事件_将被关闭 = *Cast(ULongInt Ptr ,buffer)
      Next
      
      ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
      临时数 = *Cast(ULong Ptr, buffer)
      
      结构数_头 = UBound(结构表) + 1
      结构数_尾 = 结构数_头 + 临时数 -1
      ReDim Preserve 结构表(结构数_尾)
      
      For i As Integer = 结构数_头 To 结构数_尾
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         结构表(i).ID = *Cast(ULong Ptr, buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
         临时数 = *Cast(ULong Ptr ,buffer)
         
         结构表(i).重定义结构成员表上限(临时数 -1)
         For j As Integer = 0 To 临时数 -1
            ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
            结构表(i).结构成员表(j).ID = *Cast(ULong Ptr, buffer)
            
            ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
            结构表(i).结构成员表(j).是否数组 = ( *Cast(ULong Ptr, buffer) = 1)
         Next
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         结构表(i).构造函数 = *Cast(ULongInt Ptr, buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         结构表(i).析构函数 = *Cast(ULongInt Ptr, buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         结构表(i).拷贝函数 = *Cast(ULongInt Ptr, buffer)
      Next
      
      ReadFile(文件号 ,buffer ,SizeOf(ULong) ,@toReadSize ,0)
      临时数 = *Cast(ULong Ptr ,buffer)
      
      导出数_头 = UBound(导出表) + 1
      导出数_尾 = 导出数_头 + 临时数 -1
      ReDim Preserve 导出表(导出数_尾)
      
      For i As Integer = 导出数_头 To 导出数_尾
         ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
         导出表(i).地址 = *Cast(ULongInt Ptr ,buffer)
         
         ReadFile(文件号 ,buffer ,SizeOf(Long) ,@toReadSize ,0)
         临时数 = *Cast(Long Ptr ,buffer)
         ReadFile(文件号 ,buffer ,临时数 ,@toReadSize ,0)
         导出表(i).名称 = *Cast(ZString Ptr, buffer)
      Next
      
      ReadFile(文件号 ,buffer ,SizeOf(ULongInt) ,@toReadSize ,0)
      Dim 内存范围 As ULongInt = *Cast(ULongInt Ptr ,buffer)
      
      Dim 块数量   As ULongInt = Int(内存范围 / 块)
      Dim 临时大小 As ULongInt = 内存范围 Mod 块
      
      Dim 内存区域 As Any Ptr = malloc(内存范围)
      
      For i As Integer = 0 To 块数量 -1
         ReadFile(文件号 ,buffer ,块 ,@toReadSize ,0)
         memcpy(内存区域 + i *块 ,buffer ,块)
      Next
      ReadFile(文件号 ,buffer ,临时大小 ,@toReadSize ,0)
      memcpy(内存区域 + 块数量 *块 ,buffer ,临时大小)
      
      Dim 指令指针 As UByte Ptr = 内存区域
      Dim 索引 As Integer
      Dim temp As ULongInt
      
      'Dim index As Integer
      While (1)
         ArrayAdd(指令表 ,索引)
         
         If 基地址 = -1 Then
            基地址 = 索引
         End If
         
         指令表(索引).指令 = *CPtr(UByte Ptr ,指令指针)
         指令指针 = 指令指针 + 1
         
         'Print index ,指令表(索引).指令 ,索引
         'index = index + 1
         'Sleep
         
         Select Case As Const 指令表(索引).指令
            Case is_push
               指令表(索引).数据 = New is_push_type
               CPtr(is_push_type Ptr ,指令表(索引).数据)->类型 = *CPtr(UByte Ptr ,指令指针)
               指令指针 = 指令指针 + 1
               
               Select case as const CPtr(is_push_type Ptr ,指令表(索引).数据)->类型
                  Case Push_压入容器
                     CPtr(is_push_type Ptr ,指令表(索引).数据)->容器序号=*CPtr(LongInt Ptr ,指令指针)
                     指令指针 = 指令指针 + 8
                  Case Push_压入整数
                     CPtr(is_push_type Ptr ,指令表(索引).数据)->整数值=*CPtr(LongInt Ptr ,指令指针)
                     指令指针 = 指令指针 + 8
                  Case Push_压入文本
                     temp = *CPtr(ULongInt Ptr ,指令指针)
                     指令指针 = 指令指针 + 8
                     
                     CPtr(is_push_type Ptr ,指令表(索引).数据)->文本值 = *CPtr(ZString Ptr ,指令指针)
                     指令指针 = 指令指针 + Len(CPtr(is_push_type Ptr ,指令表(索引).数据)->文本值) + 1
                  Case Push_压入小数
                     CPtr(is_push_type Ptr ,指令表(索引).数据)->小数值=*CPtr(Double Ptr ,指令指针)
                     指令指针 = 指令指针 + 8
                  Case Push_压入相对地址
                     CPtr(is_push_type Ptr ,指令表(索引).数据)->相对地址 = *CPtr(ULongInt Ptr ,指令指针)
                     指令指针 = 指令指针 + 8
                  Case Push_压入数据
                     temp = *CPtr(ULongInt Ptr ,指令指针)
                     指令指针 = 指令指针 + 8
                     
                     CPtr(is_push_type Ptr ,指令表(索引).数据)->数据值.设长度(temp)
                     
                     memcpy(CPtr(is_push_type Ptr ,指令表(索引).数据)->数据值.取数据指针(), 指令指针, temp)
                     
                     指令指针 = 指令指针 + temp
                     
               End Select
            Case is_set
               指令表(索引).数据 = New is_set_type
               CPtr(is_set_type Ptr ,指令表(索引).数据)->类型 = *CPtr(UByte Ptr ,指令指针)
               指令指针 = 指令指针 + 1
         End Select
         
         If 指令指针 >= 内存区域 + 内存范围 Then
            Exit While
         End If
      Wend
      
      free(内存区域)
      
      For i As Integer = 窗口数_头 To 窗口数_尾
         窗口表(i).基地址 = 基地址
      Next
      For i As Integer = 结构数_头 To 结构数_尾
         结构表(i).基地址 = 基地址
      Next
      For i As Integer = 导出数_头 To 导出数_尾
         导出表(i).基地址 = 基地址
      Next
   Else
      Deallocate(buffer)
      Return False
   End If
   
   Dim 索引 As Integer
   
   ArrayAdd(模块表, 索引)
   
   模块表(索引).文件号 = 文件号
   模块表(索引).基地址 = 基地址
   模块表(索引).绝对路径 = 路径
   模块表(索引).重定义全局容器上限(容器数量 -1)
   
   Deallocate(buffer)
   Return True
End Function

Sub 重定义控件表(控件表() As 控件类型,上限 As Integer)
   ReDim 控件表(上限)
End Sub

