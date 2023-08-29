
Type 线程信息
   Dim 线程ID As DWORD
   Dim 线程句柄 As HANDLE
End Type

Type 数据
   ReDim 数据空间(Any) As UByte
   
   Declare Function 取长度() As ULongInt
   Declare Sub 清空()
   Declare Function 是否相等(ByRef 临时数据 As 数据) As BOOL
   Declare Sub 合并(存放空间 As 数据 Ptr, ByRef 欲合并的空间 As 数据)
   Declare Function 取数据指针() As UByte Ptr
   Declare Sub 与(存放空间 As 数据 Ptr, ByRef 预运算的控件 As 数据)
   Declare Sub 或(存放空间 As 数据 Ptr, ByRef 预运算的控件 As 数据)
   Declare Sub 异或(存放空间 As 数据 Ptr, ByRef 预运算的控件 As 数据)
   Declare Sub 非(存放空间 As 数据 Ptr)
   Declare Sub 设长度(长度 As ULongInt)
End Type

Function 数据.取长度() As ULongInt
   Function = UBound(This.数据空间) + 1
End Function

Sub 数据.清空()
   ReDim This.数据空间( -1)
End Sub

Function 数据.是否相等(ByRef 临时数据 As 数据) As BOOL
   If 临时数据.取长度() <> This.取长度() Then
      Return False
   End If
   
   For i As Integer = 0 To 临时数据.取长度() -1
      If 临时数据.数据空间(i) <> This.数据空间(i) Then
         Return False
      End If
   Next
   
   Return True
End Function
Sub 数据.合并(存放空间 As 数据 Ptr, ByRef 欲合并的空间 As 数据)'this+欲合并的空间
   Dim 上限 As Integer = This.取长度() + 欲合并的空间.取长度() -1
   ReDim 存放空间->数据空间(上限)
   If 上限 <> -1 Then
      memcpy(@存放空间->数据空间(0), @This.数据空间(0), This.取长度())
      memcpy(@存放空间->数据空间(UBound(This.数据空间))+1, @欲合并的空间.数据空间(0), 欲合并的空间.取长度())
   End If
End Sub
Function 数据.取数据指针() As UByte Ptr
   Function = @This.数据空间(0)
End Function
Sub 数据.与(存放空间 As 数据 Ptr, ByRef 预运算的控件 As 数据) '确保数据长度一致
   If 预运算的控件.取长度() <> This.取长度() Then
      Return
   End If
   ReDim 存放空间->数据空间(This.取长度() -1)
   For i As Integer = 0 To This.取长度() -1
      存放空间->数据空间(i) = This.数据空间(i) And 预运算的控件.数据空间(i)
   Next
End Sub
Sub 数据.或(存放空间 As 数据 Ptr, ByRef 预运算的控件 As 数据) '确保数据长度一致
   If 预运算的控件.取长度() <> This.取长度() Then
      Return
   End If
   ReDim 存放空间->数据空间(This.取长度() -1)
   For i As Integer = 0 To This.取长度() -1
      存放空间->数据空间(i) = This.数据空间(i) Or 预运算的控件.数据空间(i)
   Next
End Sub
Sub 数据.异或(存放空间 As 数据 Ptr, ByRef 预运算的控件 As 数据) '确保数据长度一致
   If 预运算的控件.取长度() <> This.取长度() Then
      Return
   End If
   ReDim 存放空间->数据空间(This.取长度() -1)
   For i As Integer = 0 To This.取长度() -1
      存放空间->数据空间(i) = This.数据空间(i) Xor 预运算的控件.数据空间(i)
   Next
End Sub
Sub 数据.非(存放空间 As 数据 Ptr)
   ReDim 存放空间->数据空间(This.取长度() -1)
   For i As Integer = 0 To This.取长度() -1
      存放空间->数据空间(i) = Not This.数据空间(i)
   Next
End Sub

Sub 数据.设长度(长度 As ULongInt)
   ReDim This.数据空间(长度 -1)
End Sub

Type 对象类型
   Dim 类型 As UByte
   Dim m_整数值 As LongInt
   Dim 文本值 As String
   Dim m_小数值 As Double
   Dim 数据值 As 数据
   
   Declare Property 整数值() As LongInt
   Declare Property 小数值() As Double
   Declare Property 整数值(l As LongInt)
   Declare Property 小数值(l As Double)
   
   Declare Sub 定义(类型 As UByte)
   Declare Function 强转类型(目标类型 As UByte) As 对象类型
   Declare Sub 拷贝(指针 As 对象类型 Ptr)
   Declare Function 取数据指针() As 数据 Ptr
End Type

Property 对象类型.整数值() As LongInt
   If This.类型 = 类型_整数 Then
      Return This.m_整数值
   ElseIf This.类型 = 类型_小数 Then
      Return This.m_小数值
   End If
   End Property
Property 对象类型.小数值() As Double
   If This.类型 = 类型_整数 Then
      Return This.m_整数值
   ElseIf This.类型 = 类型_小数 Then
      Return This.m_小数值
   End If
End Property
Property 对象类型.整数值(l As LongInt)
   If This.类型 = 类型_整数 Then
      This.m_整数值=l
   ElseIf This.类型 = 类型_小数 Then
      This.m_小数值=l
   End If
End Property
Property 对象类型.小数值(l As Double)
   If This.类型 = 类型_整数 Then
      This.m_整数值=l
   ElseIf This.类型 = 类型_小数 Then
      This.m_小数值=l
   End If
End Property

Sub 对象类型.拷贝(指针 As 对象类型 Ptr)
   This.类型 = 指针->类型
   
   Select Case As Const 指针->类型
      Case 类型_整数
         This.整数值 = 指针->整数值
      Case 类型_文本
         This.文本值 = 指针->文本值
      Case 类型_小数
         This.小数值 = 指针->小数值
      Case 类型_数据
         This.数据值 = 指针->数据值
   End Select
End Sub
Function 对象类型.取数据指针() As 数据 Ptr
   If This.类型 = 类型_数据 Then
      Return @This.数据值
   End If
   Return 0
End Function

Function 对象类型.强转类型(目标类型 As UByte) As 对象类型
   Dim 返回值 As 对象类型
   返回值.定义(目标类型)
   Select Case As Const 目标类型
      Case 类型_整数
         Select Case As Const This.类型
            Case 类型_整数
               返回值.整数值 = This.整数值
            Case 类型_文本
               返回值.整数值 = ValLng(This.文本值)
            Case 类型_小数
               返回值.整数值 = This.小数值
            Case 类型_数据
               FlyDebug(Err_数据类型不能隐式转换)
         End Select
      Case 类型_文本
         Select Case As Const This.类型
            Case 类型_整数
               返回值.文本值 = Str(This.整数值)
            Case 类型_文本
               返回值.文本值 = This.文本值
            Case 类型_小数
               返回值.文本值 = Str(This.小数值)
            Case 类型_数据
               FlyDebug(Err_数据类型不能隐式转换)
         End Select
      Case 类型_小数
         Select Case As Const This.类型
            Case 类型_整数
               返回值.小数值 = This.整数值
            Case 类型_文本
               返回值.小数值 = Val(This.文本值)
            Case 类型_小数
               返回值.小数值 = This.小数值
            Case 类型_数据
               FlyDebug(Err_数据类型不能隐式转换)
         End Select
      Case 类型_数据
         If This.类型 <> 类型_数据 Then
            FlyDebug(Err_数据类型不能隐式转换)
         End If
         返回值.数据值 = This.数据值
   End Select
   Function = 返回值
End Function

Sub 对象类型.定义(类型 As UByte)
   This.类型 = 类型
   Select Case As Const 类型
      Case 类型_整数
         This.整数值 = 0
      Case 类型_文本
         This.文本值 = ""
      Case 类型_小数
         This.小数值 = 0
      Case 类型_数据
         This.数据值.清空()
   End Select
End Sub

Type 容器类型
   Dim 是否已初始化 As BOOL
   Dim 类型 As UByte
   Dim 对象 As 对象类型 Ptr
   
   Dim 容器指针 As 容器类型 Ptr
   
   ReDim 容器组(Any) As 容器类型 Ptr
   
   /'
   以下为容器组信息
   '/
   Dim 结构索引 As Integer = -1
   Dim 模块索引 As Integer = -1 
   Dim 库句柄 As HANDLE = -1
   Dim 模型 As Integer = -1
   
   Declare Sub 定义(类型 As UByte, 基地址 As Integer, 模块索引 As Integer)
   Declare Sub 定义库节(类型 As UByte, 库句柄 As HANDLE)
   Declare Sub 清空()
   Declare Sub 重置()
   Declare Sub 拷贝(指针 As 容器类型 Ptr, 只实例结构 As BOOL = False)
   Declare Sub 分配节(上限 As Integer)
   Declare Sub 加入节(个数 As Integer)
   Declare Sub 删除节(ByVal 个数 As Integer)
   Declare Sub 加入指定节(位置 As Integer,个数 As Integer)
   Declare Sub 删除指定节(位置 As Integer,个数 As Integer)
   
   Declare Destructor()
End Type

Destructor 容器类型
   Select Case As Const This.类型
      Case 容器类型_值
         
         If This.对象 <> 0 Then
            Delete(This.对象)
         End If
         
      Case 容器类型_容器指针
         
      Case 容器类型_容器组
         If This.结构索引 <> -1 Then
            结构表(This.结构索引).析构(@This, This.模块索引)
         End If
         
         For i As Integer = 0 To UBound(This.容器组)
            Delete This.容器组(i)'只用析构当前的节，其他链表节会连锁反应
         Next
         
   End Select
   This.是否已初始化 = False
End Destructor
Sub 容器类型.清空()
   This.Destructor()
End Sub
Sub 容器类型.重置()
   This.Destructor()
   
   类型 = 容器类型_值
   对象 = New 对象类型
   
   This.是否已初始化 = True
End Sub

Sub 容器类型.拷贝(指针 As 容器类型 Ptr, 只实例结构 As BOOL = False)   
   This.清空()
   Select Case As Const 指针->类型
      Case 容器类型_值
         This.类型 = 容器类型_值
         This.对象 = New 对象类型
         If 只实例结构 Then
            This.对象->类型 = 指针->对象->类型
         Else
            This.对象->拷贝(指针->对象)
         End If
      Case 容器类型_容器指针
         This.类型 = 容器类型_容器指针
         This.容器指针 = 指针->容器指针
      Case 容器类型_容器组
         This.类型 = 容器类型_容器组
         
         ReDim This.容器组(UBound(指针->容器组))
         For i As Integer = 0 To UBound(指针->容器组)
            This.容器组(i) = New 容器类型
            
            If This.结构索引 <> -1 Then
               
               If 结构表(This.结构索引).拷贝函数 = 0 Then
                  This.容器组(i)->拷贝(指针->容器组(i))
               Else
                  This.容器组(i)->拷贝(指针->容器组(i),True)
               End If
               
            Else
               This.容器组(i)->拷贝(指针->容器组(i))
               
            End If
            
         Next
         
         If This.结构索引 <> -1 Then
            结构表(This.结构索引).拷贝(@This, 指针, This.模块索引)
         End If
         
   End Select
   This.是否已初始化 = True
End Sub

'定义容器组类型模型
Sub 容器类型.定义(类型 As UByte, 基地址 As Integer,模块索引 As Integer)
   If 类型 <> 类型模型_整数 AndAlso 类型 <> 类型模型_文本 AndAlso 类型 <> 类型模型_小数 AndAlso 类型 <> 类型模型_数据 Then
      '初始化结构体
      Dim 索引 As Integer = -1
      
      For i As Integer = 0 To UBound(结构表)
         If 结构表(i).ID = 类型 AndAlso 结构表(i).基地址 = 基地址 Then
            索引 = i
            Exit For
         End If
      Next
      
      If 索引 = -1 Then
         Dim err_str As String = Space(256)
         sprintf(StrPtr(err_str), Err_未找到指定类型模型, 类型)
         FlyDebug(err_str)
      End If
      
      If 索引 <> -1 Then
         If This.类型 = 容器类型_容器组 Then
            '这是一个数组，已经被定义上限了
            For i As Integer = 0 To UBound(This.容器组)
               If This.容器组(i)->是否已初始化 Then
                  Continue For
               End If
               
               This.容器组(i)->定义(类型, 基地址,模块索引)
            Next
         Else
            This.类型 = 容器类型_容器组
            This.清空()
            
            This.分配节(UBound(结构表(索引).结构成员表))
            For i As Integer = 0 To UBound(结构表(索引).结构成员表)
               If 结构表(索引).结构成员表(i).是否数组 Then
                  This.容器组(i)->分配节(-1)
               Else
                  If 结构表(索引).结构成员表(i).ID <> 类型模型_整数 AndAlso 结构表(索引).结构成员表(i).ID <> 类型模型_文本 AndAlso 结构表(索引).结构成员表(i).ID <> 类型模型_小数 AndAlso 结构表(索引).结构成员表(i).ID<>类型模型_数据 Then
                     This.容器组(i)->定义(结构表(索引).结构成员表(i).ID, 基地址,模块索引)
                  Else
                     This.容器组(i)->重置()
                     Select Case As Const 结构表(索引).结构成员表(i).ID
                        Case 类型模型_整数
                           This.容器组(i)->对象->定义(类型_整数)
                        Case 类型模型_文本
                           This.容器组(i)->对象->定义(类型_文本)
                        Case 类型模型_小数
                           This.容器组(i)->对象->定义(类型_小数)
                        Case 类型模型_数据
                           This.容器组(i)->对象->定义(类型_数据)
                     End Select
                  End If
               End If
            Next
            
            This.结构索引 = 索引
            This.模块索引 = 模块索引
            This.库句柄 = -1
            This.模型 = 类型
            
            结构表(This.结构索引).构造(@This, This.模块索引)
         End If
      End If
   Else
      If This.类型 = 容器类型_容器组 Then
         '数组
         
         Select Case As Const 类型
            Case 类型模型_整数
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_整数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_文本
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_文本)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_小数
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_小数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_数据
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_数据)
                  This.容器组(i)->是否已初始化 = True
               Next
         End Select
      Else
         This.类型 = 容器类型_容器组
         Select Case As Const 类型
            Case 类型模型_整数
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_整数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_文本
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_文本)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_小数
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_小数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_数据
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_数据)
                  This.容器组(i)->是否已初始化 = True
               Next
         End Select
      End If
   End If
   
   This.是否已初始化 = True
End Sub

Sub 容器类型.定义库节(类型 As UByte, 库句柄 As HANDLE)
   If 类型 <> 类型模型_整数 AndAlso 类型 <> 类型模型_文本 AndAlso 类型 <> 类型模型_小数 AndAlso 类型 <> 类型模型_数据 Then
      '初始化结构体
      Dim 索引 As Integer = -1
      Dim 库索引 As Integer = -1
      
      For i As Integer = 0 To UBound(库列表)
         If 库列表(i).库句柄 = 库句柄 Then
            For j As Integer = 0 To UBound(库列表(i).库信息.结构表)
               If 库列表(i).库信息.结构表(j).ID = 类型 Then
                  库索引 = i
                  索引 = j
                  Exit For
               End If
            Next
            If 库索引 <> -1 AndAlso 索引 <> -1 Then
               Exit For
            End If
         End If
      Next
      
      If 索引 = -1 OrElse 库索引 =-1 Then
         Dim err_str As String = Space(256)
         sprintf(StrPtr(err_str), Err_未找到指定类型模型, 类型)
         FlyDebug(err_str)
      End If
      
      If 索引 <> -1 AndAlso 库索引 <> -1 Then
         If This.类型 = 容器类型_容器组 Then
            '这是一个数组，已经被定义上限了
            For i As Integer = 0 To UBound(This.容器组)
               If This.容器组(i)->是否已初始化 Then
                  Continue For
               End If
               
               This.容器组(i)->定义库节(类型, 库句柄)
            Next
         Else
            This.类型 = 容器类型_容器组
            This.清空()
            
            This.分配节(UBound(库列表(库索引).库信息.结构表(索引).结构成员表))
            For i As Integer = 0 To UBound(库列表(库索引).库信息.结构表(索引).结构成员表)
               If 库列表(库索引).库信息.结构表(索引).结构成员表(i).是否数组 Then
                  This.容器组(i)->分配节(-1)
               Else
                  If 库列表(库索引).库信息.结构表(索引).结构成员表(i).ID <> 类型模型_整数 AndAlso 库列表(库索引).库信息.结构表(索引).结构成员表(i).ID <> 类型模型_文本 AndAlso 库列表(库索引).库信息.结构表(索引).结构成员表(i).ID <> 类型模型_小数 AndAlso 库列表(库索引).库信息.结构表(索引).结构成员表(i).ID<>类型模型_数据 Then
                     This.容器组(i)->定义库节(库列表(库索引).库信息.结构表(索引).结构成员表(i).ID, 库句柄)
                  Else
                     This.容器组(i)->重置()
                     Select Case As Const 库列表(库索引).库信息.结构表(索引).结构成员表(i).ID
                        Case 类型模型_整数
                           This.容器组(i)->对象->定义(类型_整数)
                        Case 类型模型_文本
                           This.容器组(i)->对象->定义(类型_文本)
                        Case 类型模型_小数
                           This.容器组(i)->对象->定义(类型_小数)
                        Case 类型模型_数据
                           This.容器组(i)->对象->定义(类型_数据)
                     End Select
                  End If
               End If
            Next
            
            
            This.结构索引 = 索引
            This.模块索引 = -1
            This.库句柄 = 库句柄
            This.模型 = 类型
            
         End If
      End If
   Else
      If This.类型 = 容器类型_容器组 Then
         Select Case As Const 类型
            Case 类型模型_整数
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_整数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_文本
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_文本)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_小数
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_小数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_数据
               For i As Integer = 0 To UBound(This.容器组)
                  If This.容器组(i)->是否已初始化 Then
                     Continue For
                  End If
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_数据)
                  This.容器组(i)->是否已初始化 = True
               Next
         End Select
      Else
         This.类型 = 容器类型_容器组
         Select Case As Const 类型
            Case 类型模型_整数
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_整数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_文本
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_文本)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_小数
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_小数)
                  This.容器组(i)->是否已初始化 = True
               Next
            Case 类型模型_数据
               For i As Integer = 0 To UBound(This.容器组)
                  This.容器组(i)->对象 = New 对象类型
                  This.容器组(i)->对象->定义(类型_数据)
                  This.容器组(i)->是否已初始化 = True
               Next
         End Select
      End If
   End If
   
   This.是否已初始化 = True
End Sub


Sub 容器类型.分配节(上限 As Integer)'会清空数据
   This.清空()
   
   This.类型 = 容器类型_容器组
   ReDim This.容器组(上限)
   For i As Integer = 0 To 上限
      This.容器组(i) = New 容器类型
   Next
   
   This.是否已初始化 = True
End Sub

Sub 容器类型.加入节(个数 As Integer)
   This.类型 = 容器类型_容器组
   Dim old_ubound As Integer = UBound(This.容器组)
   
   ReDim Preserve This.容器组(UBound(This.容器组) + 个数)
   For i As Integer = old_ubound + 1 To UBound(This.容器组)
      This.容器组(i) = New 容器类型
   Next
   
   This.是否已初始化 = True
End Sub
Sub 容器类型.删除节(ByVal 个数 As Integer)
   This.类型 = 容器类型_容器组
   
   If 个数 > ARRAYSIZE(This.容器组) Then
      个数 = ARRAYSIZE(This.容器组)
   End If
   
   For i As Integer = UBound(This.容器组) To UBound(This.容器组) - 个数 +1 Step -1
      If This.容器组(i)->结构索引 <> -1 Then
         结构表(This.容器组(i)->结构索引).析构(This.容器组(i), This.容器组(i)->模块索引)
      End If
      
      Delete This.容器组(i)
   Next
   If UBound(This.容器组) - 个数 = -1 Then
      Erase This.容器组
   Else
      ReDim Preserve This.容器组(UBound(This.容器组) - 个数)
   End If
   
   This.是否已初始化 = True
End Sub

Sub 容器类型.加入指定节(位置 As Integer, 个数 As Integer)
   This.类型 = 容器类型_容器组
   
   ReDim 保存_容器组(LBound(This.容器组) To UBound(This.容器组)) As 容器类型 Ptr
   
   For i As Integer = LBound(This.容器组) To UBound(This.容器组)
      保存_容器组(i) = This.容器组(i)
   Next
   Erase This.容器组
   Dim index As Integer
   For i As Integer = LBound(保存_容器组) To 位置 -1
      ArrayAdd(This.容器组, index)
      This.容器组(index) = 保存_容器组(i)
   Next
   For i As Integer = 1 To 个数
      ArrayAdd(This.容器组, index)
      This.容器组(index) = New 容器类型
   Next
   For i As Integer = 位置 To UBound(保存_容器组)
      ArrayAdd(This.容器组, index)
      This.容器组(index) = 保存_容器组(i)
   Next
   This.是否已初始化 = True
End Sub
Sub 容器类型.删除指定节(位置 As Integer, 个数 As Integer)
   This.类型 = 容器类型_容器组
   
   ReDim 保存_容器组(LBound(This.容器组) To UBound(This.容器组)) As 容器类型 Ptr
   For i As Integer = LBound(This.容器组) To UBound(This.容器组)
      保存_容器组(i) = This.容器组(i)
   Next
   Erase This.容器组
   Dim index As Integer
   For i As Integer = LBound(保存_容器组) To UBound(保存_容器组)
      If i >= 位置 AndAlso i < 位置 + 个数 Then
         
         If 保存_容器组(i)->结构索引 <> -1 Then
            结构表(保存_容器组(i)->结构索引).析构(保存_容器组(i)->容器组(i), 保存_容器组(i)->模块索引)
         End If
         
         Delete 保存_容器组(i)
         Continue For
      End If
      ArrayAdd(This.容器组, index)
      This.容器组(index) = 保存_容器组(i)
   Next
   
   This.是否已初始化 = True
End Sub

Type 循环栈类型
   Dim 循环位置 As Integer
   Dim 循环次数 As Integer
   Dim 循环存储对象 As 对象类型 Ptr
End Type

Type 变量循环栈类型
   Dim 循环位置 As Integer
   Dim 循环目标值 As Integer
   Dim 循环步长 As Integer
   Dim 循环存储对象 As 对象类型 Ptr
End Type

Union 栈空间联合
   Dim 容器值 As 容器类型 Ptr
   Dim 整数值 As LongInt
   Dim 小数值 As Double
   Dim 地址 As ULongInt
End Union

Type 栈类型
   Dim 类型 As UByte
   
   Dim 栈空间 As 栈空间联合
   Dim 本地对象 As 对象类型
   Dim 文本值 As String
   Dim 数据值 As 数据
   
   Declare Function 取值类型() As UByte
   Declare Function 转对象() As 对象类型 Ptr
   Declare Function 取数据指针() As 数据 Ptr
   Declare Sub 清空()
End Type
Sub 栈类型.清空()
   This.类型 = 0
   This.栈空间.整数值 = 0
   This.本地对象.类型 = 0
   This.本地对象.m_整数值 = 0
   This.本地对象.文本值 = ""
   This.本地对象.m_小数值 = 0
   This.本地对象.数据值.清空()
   This.文本值 = ""
   This.数据值.清空()
End Sub

Function 栈类型.取数据指针() As 数据 Ptr
   Select Case As Const This.类型
      Case 栈_容器
         If This.栈空间.容器值->对象->类型 = 类型_数据 Then
            Function=@This.栈空间.容器值->对象->数据值
         End If
      Case 栈_数据
         Function = @This.数据值
   End Select
End Function

Function 栈类型.转对象() As 对象类型 Ptr
   Select Case 类型
      Case 栈_容器
         This.本地对象 = *This.栈空间.容器值->对象
      Case 栈_整数
         This.本地对象.类型 = 类型_整数
         This.本地对象.整数值 = This.栈空间.整数值
      Case 栈_文本
         This.本地对象.类型 = 类型_文本
         This.本地对象.文本值 = This.文本值
      Case 栈_小数
         This.本地对象.类型 = 类型_小数
         This.本地对象.小数值 = This.栈空间.小数值
      Case 栈_数据
         This.本地对象.类型 = 类型_数据
         This.本地对象.数据值 = This.数据值
      Case 栈_地址
         This.本地对象.类型 = 类型_整数
         This.本地对象.整数值 = This.栈空间.地址
   End Select
   Function = @This.本地对象
End Function

Function 栈类型.取值类型() As UByte
   Select Case As Const This.类型
      Case 栈_容器
         Function = This.栈空间.容器值->对象->类型
      Case 栈_整数
         Function = 类型_整数
      Case 栈_文本
         Function = 类型_文本
      Case 栈_小数
         Function = 类型_小数
      Case 栈_地址
         Function = 类型_整数
      Case 栈_数据
         Function = 类型_数据
   End Select
End Function

Type 初始信息类型
   ReDim 初始栈(Any) As 栈类型
   Dim 初始地址 As ULongInt
   Dim 返回值地址 As Any Ptr
   Dim 基地址 As ULongInt
   Dim 模块索引 As Integer
End Type

Type 模块类型
   Dim 文件号 As HANDLE
   Dim 基地址 As ULongInt
   Dim 绝对路径 As String
   ReDim 全局容器(Any) As 容器类型
   
   Declare Sub 重定义全局容器上限(上限 As Integer)
End Type

Sub 模块类型.重定义全局容器上限(上限 As Integer)
   ReDim This.全局容器(上限)
End Sub

Type 导出类型
   Dim 基地址 As ULongInt
   Dim 地址 As ULongInt
   Dim 名称 As String
End Type

Type 按钮类型
   Dim 标题 As String
   Dim 左边 As ULong
   Dim 顶边 As ULong
   Dim 宽度 As ULong
   Dim 高度 As ULong
   Dim 可视 As BOOL
   Dim 禁止 As BOOL
   Dim 风格 As ULong
   Dim 附加风格 As ULong
   
   Dim 事件_被单击 As ULongInt
   Dim 事件_获取焦点 As ULongInt
   Dim 事件_失去焦点 As ULongInt
End Type

Type 编辑框类型
   Dim 内容 As String
   Dim 左边 As ULong
   Dim 顶边 As ULong
   Dim 宽度 As ULong
   Dim 高度 As ULong
   Dim 风格 As ULong
   Dim 对齐方式 As ULong
   Dim 只读 As BOOL
   Dim 纵向滚动条 As BOOL
   Dim 横向滚动条 As BOOL
   Dim 可视 As BOOL
   Dim 禁止 As BOOL
   
   Dim 附加风格 As ULong
   
   Dim 事件_内容被改变 As ULongInt
   Dim 事件_字符输入 As ULongInt
   Dim 事件_获取焦点 As ULongInt
   Dim 事件_失去焦点 As ULongInt
End Type

Type 标签类型
   Dim 标题 As String
   Dim 左边 As ULong
   Dim 顶边 As ULong
   Dim 宽度 As ULong
   Dim 高度 As ULong
   Dim 对齐方式 As ULong
   Dim 附加风格 As ULong
   Dim 可视 As BOOL
   Dim 禁止 As BOOL
   
   Dim 事件_被单击 As ULongInt
   Dim 事件_被双击 As ULongInt
End Type

Type 时钟类型
   Dim 时钟周期 As ULong
   
   Dim 事件_周期事件 As ULongInt
End Type

Type 图片框类型
   Dim 左边 As ULong
   Dim 顶边 As ULong
   Dim 宽度 As ULong
   Dim 高度 As ULong
   Dim 可视 As BOOL
   Dim 禁止 As BOOL
   Dim 显示方式 As ULong
   Dim 附加风格 As ULong
   
   ReDim 图片(Any) As UByte
   
   Dim 位图句柄 As HBITMAP
   
   Declare Sub 图片_ReDim(上限 As Integer)
End Type
Sub 图片框类型.图片_ReDim(上限 As Integer)
   ReDim This.图片(上限)
End Sub

Type 选择框类型
   Dim 标题 As String
   Dim 左边 As ULong
   Dim 顶边 As ULong
   Dim 宽度 As ULong
   Dim 高度 As ULong
   Dim 可视 As BOOL
   Dim 禁止 As BOOL
   Dim 选择方式 As ULong
   Dim 状态 As ULong
   
   Dim 附加风格 As ULong
   
   Dim 事件_被单击 As ULongInt
   Dim 事件_获取焦点 As ULongInt
   Dim 事件_失去焦点 As ULongInt
End Type

Type 控件类型
   Dim ID As ULong
   Dim 类型 As ULong
   Dim 按钮值 As 按钮类型
   Dim 编辑框值 As 编辑框类型
   Dim 标签值 As 标签类型
   Dim 时钟值 As 时钟类型
   Dim 图片框值 As 图片框类型
   Dim 选择框值 As 选择框类型
   
   Dim 是否静态 As BOOL
End Type

Type 窗口类型
   Dim 基地址 As ULongInt
   
   Dim ID As ULong
   Dim 左边 As ULong
   Dim 顶边 As ULong
   Dim 宽度 As ULong
   Dim 高度 As ULong
   Dim 可视 As BOOL
   Dim 禁止 As BOOL
   Dim 标题 As String
   Dim 风格 As ULong
   Dim 附加风格 As ULong
   ReDim 控件表(Any) As 控件类型
   
   Dim 事件_尺寸被改变 As ULongInt
   Dim 事件_将被销毁 As ULongInt
   Dim 事件_鼠标左键被按下 As ULongInt
   Dim 事件_鼠标左键被放开 As ULongInt
   Dim 事件_鼠标右键被按下 As ULongInt
   Dim 事件_鼠标右键被放开 As ULongInt
   Dim 事件_获取焦点 As ULongInt
   Dim 事件_失去焦点 As ULongInt
   Dim 事件_按下某键 As ULongInt
   Dim 事件_放开某键 As ULongInt
   Dim 事件_创建完毕 As ULongInt
   Dim 事件_被双击 As ULongInt
   Dim 事件_将被关闭 As ULongInt
   
   Dim 成员X位置 As ULong
   Dim 成员Y位置 As ULong
   Dim 成员_宽度 As ULong
   Dim 成员_高度 As ULong
   Dim 成员_控件句柄 As hWnd
   
   Declare Sub 载入窗口()
End Type

Sub 窗口类型.载入窗口()
   If IsWindow(This.成员_控件句柄) Then
      Return
   End If
   
   Dim 局部_返回句柄 As hWnd
   局部_返回句柄 = CreateWindowExA(0 ,"Fly_Window" ,This.标题 ,This.风格 Or This.附加风格 ,This.左边 ,This.顶边 ,This.宽度 ,This.高度 ,0 ,0 ,GetModuleHandleA(0) ,0)
   
   If 局部_返回句柄 = 0 Then
      Return
   End If
   
   Dim 窗口事件索引 As Integer
   窗口事件索引 = UBound(内部_窗口事件) + 1
   ReDim Preserve 内部_窗口事件(窗口事件索引)
   内部_窗口事件(窗口事件索引).窗口句柄 = 局部_返回句柄
   内部_窗口事件(窗口事件索引).ID = This.ID
   
   This.成员X位置 = 左边
   This.成员Y位置 = 顶边
   This.成员_宽度 = 宽度
   This.成员_高度 = 高度
   This.成员_控件句柄 = 局部_返回句柄
   
   For i As Integer = 0 To UBound(This.控件表)
      内部_子类化处理(This.成员_控件句柄)
      Dim 控件句柄 As hWnd
      
      Select Case As Const This.控件表(i).类型
         Case 控件类型_按钮
            窗口事件索引 = UBound(内部_窗口事件) + 1
            ReDim Preserve 内部_窗口事件(窗口事件索引)
            
            Select Case As Const This.控件表(i).按钮值.风格
               Case 按钮_类型_普通按钮
                  控件句柄 = CreateWindowExA(0 ,"BUTTON" ,This.控件表(i).按钮值.标题 ,BS_PUSHBUTTON Or WS_TABSTOP Or 1073741824 Or 268435456 Or This.控件表(i).按钮值.附加风格 ,This.控件表(i).按钮值.左边 ,This.控件表(i).按钮值.顶边 ,This.控件表(i).按钮值.宽度 ,This.控件表(i).按钮值.高度 ,局部_返回句柄 ,Cast(Integer,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
               Case 按钮_类型_默认按钮
                  控件句柄 = CreateWindowExA(0 ,"BUTTON" ,This.控件表(i).按钮值.标题 ,BS_DEFPUSHBUTTON Or WS_TABSTOP Or 1073741824 Or 268435456 Or This.控件表(i).按钮值.附加风格 ,This.控件表(i).按钮值.左边 ,This.控件表(i).按钮值.顶边 ,This.控件表(i).按钮值.宽度 ,This.控件表(i).按钮值.高度 ,局部_返回句柄 ,Cast(Integer,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
            End Select
            
            内部_窗口事件(窗口事件索引).窗口句柄 = 控件句柄
            内部_窗口事件(窗口事件索引).ID = This.控件表(i).ID
            模块_设置字体(控件句柄, "微软雅黑")
            
            ShowWindowAsync(控件句柄, This.控件表(i).按钮值.可视)
            EnableWindow(控件句柄, This.控件表(i).按钮值.禁止 = False)
         Case 控件类型_编辑框
            窗口事件索引 = UBound(内部_窗口事件) + 1
            ReDim Preserve 内部_窗口事件(窗口事件索引)
            
            Select Case As Const This.控件表(i).编辑框值.风格
               Case 编辑框_类型_单行编辑框
                  控件句柄 = CreateWindowExA (512, "Edit", This.控件表(i).编辑框值.内容, This.控件表(i).编辑框值.对齐方式 Or IIf(This.控件表(i).编辑框值.只读,ES_READONLY,0) Or ES_WANTRETURN Or WS_TABSTOP Or ES_AUTOHSCROLL Or ES_NOHIDESEL Or 1073741824 Or 268435456 Or IIf(This.控件表(i).编辑框值.纵向滚动条,WS_VSCROLL,0) Or IIf(This.控件表(i).编辑框值.横向滚动条,WS_HSCROLL,0) Or This.控件表(i).编辑框值.附加风格, This.控件表(i).编辑框值.左边, This.控件表(i).编辑框值.顶边, This.控件表(i).编辑框值.宽度, This.控件表(i).编辑框值.高度, 局部_返回句柄, Cast(Integer,This.控件表(i).ID), GetModuleHandleA(0), 0)
               Case 编辑框_类型_多行编辑框
                  控件句柄 = CreateWindowExA(512 ,"Edit" ,This.控件表(i).编辑框值.内容 ,This.控件表(i).编辑框值.对齐方式 Or IIf(This.控件表(i).编辑框值.只读 ,ES_READONLY ,0) Or WS_TABSTOP Or ES_MULTILINE Or WS_VSCROLL Or ES_AUTOVSCROLL Or ES_NOHIDESEL Or 1073741824 Or 268435456 Or IIf(This.控件表(i).编辑框值.纵向滚动条 ,WS_VSCROLL ,0) Or IIf(This.控件表(i).编辑框值.横向滚动条 ,WS_HSCROLL ,0) Or This.控件表(i).编辑框值.附加风格 ,This.控件表(i).编辑框值.左边 ,This.控件表(i).编辑框值.顶边 ,This.控件表(i).编辑框值.宽度 ,This.控件表(i).编辑框值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
               Case 编辑框_类型_密码框
                  控件句柄 = CreateWindowExA(512 ,"Edit" ,This.控件表(i).编辑框值.内容 ,This.控件表(i).编辑框值.对齐方式 Or IIf(This.控件表(i).编辑框值.只读 ,ES_READONLY ,0) Or ES_WANTRETURN Or WS_TABSTOP Or ES_PASSWORD Or ES_NOHIDESEL Or 1073741824 Or 268435456 Or IIf(This.控件表(i).编辑框值.纵向滚动条 ,WS_VSCROLL ,0) Or IIf(This.控件表(i).编辑框值.横向滚动条 ,WS_HSCROLL ,0) Or This.控件表(i).编辑框值.附加风格 ,This.控件表(i).编辑框值.左边 ,This.控件表(i).编辑框值.顶边 ,This.控件表(i).编辑框值.宽度 ,This.控件表(i).编辑框值.高度 ,局部_返回句柄 ,Cast(Integer,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
            End Select
            
            内部_窗口事件(窗口事件索引).窗口句柄 = 控件句柄
            内部_窗口事件(窗口事件索引).ID = This.控件表(i).ID
            模块_设置字体(控件句柄, "微软雅黑")
            
            ShowWindowAsync(控件句柄, This.控件表(i).编辑框值.可视)
            EnableWindow(控件句柄, This.控件表(i).编辑框值.禁止 = False)
         Case 控件类型_标签
            This.控件表(i).是否静态 = True
            
            窗口事件索引 = UBound(内部_窗口事件) + 1
            ReDim Preserve 内部_窗口事件(窗口事件索引)
            
            Select Case As Const This.控件表(i).标签值.对齐方式
               Case 标签_对齐方式_右对齐
                  控件句柄 = CreateWindowEx(0 ,"Static" ,This.控件表(i).标签值.标题 ,SS_RIGHT Or WS_GROUP Or SS_NOTIFY Or 1073741824 Or 268435456 Or This.控件表(i).标签值.附加风格 ,This.控件表(i).标签值.左边 ,This.控件表(i).标签值.顶边 ,This.控件表(i).标签值.宽度 ,This.控件表(i).标签值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
                  
               Case 标签_对齐方式_左对齐
                  控件句柄 = CreateWindowEx(0 ,"Static" ,This.控件表(i).标签值.标题 ,SS_LEFT Or WS_GROUP Or SS_NOTIFY Or 1073741824 Or 268435456 Or This.控件表(i).标签值.附加风格 ,This.控件表(i).标签值.左边 ,This.控件表(i).标签值.顶边 ,This.控件表(i).标签值.宽度 ,This.控件表(i).标签值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
               Case 标签_对齐方式_居中对齐
                  控件句柄 = CreateWindowEx(0 ,"Static" ,This.控件表(i).标签值.标题 ,SS_CENTER Or WS_GROUP Or SS_NOTIFY Or 1073741824 Or 268435456 Or This.控件表(i).标签值.附加风格 ,This.控件表(i).标签值.左边 ,This.控件表(i).标签值.顶边 ,This.控件表(i).标签值.宽度 ,This.控件表(i).标签值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
               Case 标签_对齐方式_垂直对齐
                  控件句柄 = CreateWindowEx(0 ,"Static" ,This.控件表(i).标签值.标题 ,SS_CENTERIMAGE Or SS_CENTER Or WS_GROUP Or SS_NOTIFY Or 1073741824 Or 268435456 Or This.控件表(i).标签值.附加风格 ,This.控件表(i).标签值.左边 ,This.控件表(i).标签值.顶边 ,This.控件表(i).标签值.宽度 ,This.控件表(i).标签值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
            End Select
            
            内部_窗口事件(窗口事件索引).窗口句柄 = 控件句柄
            内部_窗口事件(窗口事件索引).ID = This.控件表(i).ID
            模块_设置字体(控件句柄, "微软雅黑")
            
            ShowWindowAsync(控件句柄,This.控件表(i).标签值.可视)
            EnableWindow(控件句柄, This.控件表(i).标签值.禁止 = False)
         Case 控件类型_时钟
            窗口事件索引 = UBound(内部_窗口事件) + 1
            ReDim Preserve 内部_窗口事件(窗口事件索引)
            
            If This.控件表(i).时钟值.时钟周期 = 0 Then
               KillTimer(局部_返回句柄, This.控件表(i).ID)
            Else
               SetTimer(局部_返回句柄, This.控件表(i).ID, This.控件表(i).时钟值.时钟周期, 0)
            End If
            
            '内部_窗口事件(窗口事件索引).窗口句柄 = 控件句柄
            内部_窗口事件(窗口事件索引).ID = This.控件表(i).ID
            
         Case 控件类型_图片框
            This.控件表(i).是否静态 = True
            
            窗口事件索引 = UBound(内部_窗口事件) + 1
            ReDim Preserve 内部_窗口事件(窗口事件索引)
            
            Select Case As Const This.控件表(i).图片框值.显示方式
               Case 图片框_显示方式_拉伸
                  控件句柄 = CreateWindowEx(0 ,"Static" ,"" ,SS_BITMAP Or SS_NOTIFY Or SS_CENTERIMAGE Or 1073741824 Or 268435456 Or  This.控件表(i).图片框值.附加风格,This.控件表(i).图片框值.左边 ,This.控件表(i).图片框值.顶边 ,This.控件表(i).图片框值.宽度 ,This.控件表(i).图片框值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
               Case 图片框_显示方式_居中
                  控件句柄 = CreateWindowEx(0 ,"Static" ,"" ,SS_BITMAP Or SS_NOTIFY Or 1073741824 Or 268435456 Or  This.控件表(i).图片框值.附加风格,This.控件表(i).图片框值.左边 ,This.控件表(i).图片框值.顶边 ,This.控件表(i).图片框值.宽度 ,This.控件表(i).图片框值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
            End Select
            
            内部_窗口事件(窗口事件索引).窗口句柄 = 控件句柄
            内部_窗口事件(窗口事件索引).ID = This.控件表(i).ID
            模块_设置字体(控件句柄, "微软雅黑")
            
            ShowWindowAsync(控件句柄,This.控件表(i).图片框值.可视)
            EnableWindow(控件句柄, This.控件表(i).图片框值.禁止 = False)
            
         Case 控件类型_选择框
            
            窗口事件索引 = UBound(内部_窗口事件) + 1
            ReDim Preserve 内部_窗口事件(窗口事件索引)
            
            Select Case As Const This.控件表(i).选择框值.选择方式
               Case 选择框_逻辑
                  控件句柄 = CreateWindowEx(0 ,"Button" ,This.控件表(i).选择框值.标题 ,BS_AUTOCHECKBOX Or WS_TABSTOP Or BS_TOP Or 1073741824 Or 268435456 Or This.控件表(i).选择框值.附加风格,This.控件表(i).选择框值.左边 ,This.控件表(i).选择框值.顶边 ,This.控件表(i).选择框值.宽度 ,This.控件表(i).选择框值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
               Case 选择框_三态
                  控件句柄 = CreateWindowEx(0 ,"Button" ,This.控件表(i).选择框值.标题 ,BS_AUTO3STATE Or WS_TABSTOP Or BS_TOP Or 1073741824 Or 268435456 Or This.控件表(i).选择框值.附加风格,This.控件表(i).选择框值.左边 ,This.控件表(i).选择框值.顶边 ,This.控件表(i).选择框值.宽度 ,This.控件表(i).选择框值.高度 ,局部_返回句柄 ,Cast(Integer ,This.控件表(i).ID) ,GetModuleHandleA(0) ,0)
            End Select
            
            内部_窗口事件(窗口事件索引).窗口句柄 = 控件句柄
            内部_窗口事件(窗口事件索引).ID = This.控件表(i).ID
            模块_设置字体(控件句柄, "微软雅黑")
            
            ShowWindowAsync(控件句柄,This.控件表(i).选择框值.可视)
            EnableWindow(控件句柄, This.控件表(i).选择框值.禁止 = False)
            
            SendMessage(控件句柄, BM_SETCHECK, This.控件表(i).选择框值.状态, 0)
            
      End Select
   Next
   
   If This.可视 Then
      ShowWindow(局部_返回句柄 ,SW_SHOW)
      UpdateWindow(局部_返回句柄)
   End If
   
   SendMessage(局部_返回句柄 ,WM_CREATE ,0 ,0)
   
   If 内部_基窗口句柄 = 0 Then
      内部_基窗口句柄 = 局部_返回句柄
   End If
End Sub

Type 消息类型
   Dim 基地址 As Integer
   
   Dim ID As ULong
   Dim 值 As ULong
   Dim 参数1 As ULongInt
   Dim 参数2 As ULongInt
   Dim 参数3 As ULongInt
   Dim 参数4 As ULongInt
End Type

Type 窗口事件类型
   Dim 窗口句柄 As hWnd
   Dim ID As ULong
End Type

Type 窗口函数类型
   Dim 窗口句柄 As hWnd
   
   Dim 窗口函数 As MSGWND
End Type

Type 调用栈类型
   Dim 回转地址 As ULongInt
   ReDim 容器(Any) As 容器类型
   ReDim 循环栈(Any) As 循环栈类型
   Dim 循环栈顶 As Integer = 0
   
   ReDim 变量循环栈(Any) As 变量循环栈类型
   Dim 变量循环栈顶 As Integer = 0
   
   Declare Sub 重定义容器上限(上限 As Integer)
   Declare Sub 重定义循环栈上限(上限 As Integer)
   Declare Sub 重定义变量循环栈上限(上限 As Integer)
End Type

Sub 调用栈类型.重定义容器上限(上限 As Integer)
   ReDim This.容器(上限)
End Sub
Sub 调用栈类型.重定义循环栈上限(上限 As Integer)
   ReDim This.循环栈(上限)
End Sub
Sub 调用栈类型.重定义变量循环栈上限(上限 As Integer)
   ReDim This.变量循环栈(上限)
End Sub

Type 指令类型
   Dim 指令 As UByte
   Dim 数据 As Any Ptr
   
   Declare Destructor()
End Type

Destructor 指令类型
   If This.数据 <> 0 Then
      Delete This.数据
   End If
End Destructor

Type is_push_type
   Dim 类型 As UByte
   Dim 容器序号 As LongInt
   Dim 整数值 As LongInt
   Dim 文本值 As String
   Dim 小数值 As Double
   Dim 相对地址 As ULongInt
   Dim 数据值 As 数据
   
   Dim 绝对地址 As ULongInt
End Type

Type is_set_type
   Dim 类型 As UByte
End Type

Type 结构成员类型
   Dim ID As ULong
   Dim 是否数组 As BOOL
End Type

Type 结构类型
   Dim 基地址 As ULongInt
   
   Dim ID As ULong
   ReDim 结构成员表(Any) As 结构成员类型
   Dim 构造函数 As ULongInt
   Dim 析构函数 As ULongInt
   Dim 拷贝函数 As ULongInt
   
   Declare Sub 重定义结构成员表上限(上限 As Integer)
   
   Declare Sub 构造(本 As 容器类型 Ptr,模块索引 As Integer)
   Declare Sub 析构(本 As 容器类型 Ptr, 模块索引 As Integer)
   Declare Sub 拷贝(本 As 容器类型 Ptr,源 As 容器类型 Ptr,模块索引 As Integer)
End Type

Sub 结构类型.重定义结构成员表上限(上限 As Integer)
   ReDim This.结构成员表(上限)
End Sub

Sub 结构类型.构造(本 As 容器类型 Ptr,模块索引 As Integer)
   If This.构造函数 = 0 Then
      Return
   End If
   ReDim 栈(0) As 栈类型
   栈(0).类型 = 栈_容器
   栈(0).栈空间.容器值 = 本
   
   Dim 线程句柄 As HANDLE
   Dim 线程ID As DWORD
   
   部署线程(This.基地址, 模块索引, 栈(), This.构造函数, 线程句柄, 线程ID, 0)
   WaitForSingleObject(线程句柄, -1)
End Sub
Sub 结构类型.析构(本 As 容器类型 Ptr,模块索引 As Integer)
   If This.析构函数 = 0 Then
      Return
   End If
   
   ReDim 栈(0) As 栈类型
   栈(0).类型 = 栈_容器
   栈(0).栈空间.容器值 = 本
   
   Dim 线程句柄 As HANDLE
   Dim 线程ID As DWORD
   
   部署线程(This.基地址, 模块索引, 栈(), This.析构函数, 线程句柄, 线程ID, 0)
   WaitForSingleObject(线程句柄, -1)
End Sub

Sub 结构类型.拷贝(本 As 容器类型 Ptr,源 As 容器类型 Ptr,模块索引 As Integer)
   If This.拷贝函数 = 0 Then
      Return
   End If
   
   ReDim 栈(1) As 栈类型
   栈(0).类型 = 栈_容器
   栈(0).栈空间.容器值 = 本
   栈(1).类型 = 栈_容器
   栈(1).栈空间.容器值 = 源
   
   Dim 线程句柄 As HANDLE
   Dim 线程ID As DWORD
   
   部署线程(This.基地址, 模块索引, 栈(), This.拷贝函数, 线程句柄, 线程ID, 0)
   WaitForSingleObject(线程句柄, -1)
End Sub

Type 库结构类型
   Dim ID As ULong
   ReDim 结构成员表(Any) As 结构成员类型
End Type

Type 交互信息类型
   Dim Fly_压入栈 As Sub stdcall(栈() As 栈类型, ByRef 栈顶 As Integer, 值 As 栈类型 Ptr)
   Dim Fly_弹出栈 As Function stdcall(栈() As 栈类型, ByRef 栈顶 As Integer) As 栈类型 Ptr
   Dim Fly_栈取对象 As Function stdcall(栈 As 栈类型 Ptr) As 对象类型 Ptr
   Dim Fly_栈取容器 as Function stdcall(栈 As 栈类型 Ptr) As 容器类型 Ptr
   Dim Fly_栈转对象 As Function stdcall(栈 As 栈类型 Ptr) As 对象类型 Ptr
   Dim Fly_下一个 as Function stdcall(容器 As 容器类型 Ptr,容器组索引 As Integer) As 容器类型 Ptr
   Dim Fly_容器取对象 as Function stdcall(容器 As 容器类型 Ptr) As 对象类型 Ptr
   Dim Fly_对象取整数 As Function stdcall(对象 As 对象类型 Ptr) As LongInt
   Dim Fly_对象取文本 As Function stdcall(对象 As 对象类型 Ptr) As ZString Ptr
   Dim Fly_对象取小数 As Function stdcall(对象 As 对象类型 Ptr) As Double
   Dim Fly_对象取数据 As Function stdcall(对象 As 对象类型 Ptr,ByRef 长度 As Integer) As Any Ptr
   Dim Fly_分配容器 As Function stdcall() As 容器类型 Ptr
   Dim Fly_删除容器 As Sub stdcall(容器 As 容器类型 Ptr)
   Dim Fly_分配容器节 As Sub stdcall(容器 As 容器类型 Ptr, 上限 As Integer)
   Dim Fly_删除容器节 As Sub stdcall(容器 As 容器类型 Ptr, 数量 As Integer)
   Dim Fly_定义节 As Sub stdcall(容器 As 容器类型 Ptr, 模型 As UByte, 库句柄 As HANDLE)
   Dim Fly_分配对象 As Function stdcall() As 对象类型 Ptr
   Dim Fly_删除对象 As Sub stdcall(对象 As 对象类型 Ptr)
   Dim Fly_置对象整数 As Sub stdcall(对象 As 对象类型 Ptr, 值 As LongInt)
   Dim Fly_置对象文本 As Sub stdcall(对象 As 对象类型 Ptr, 值 As ZString Ptr)
   Dim Fly_置对象小数 As Sub stdcall(对象 As 对象类型 Ptr, 值 As Double)
   Dim Fly_置对象数据 As Sub stdcall(对象 As 对象类型 Ptr, 值 As UByte Ptr, 长度 As Integer)
   Dim Fly_分配栈 As Function stdcall() As 栈类型 Ptr
   Dim Fly_删除栈 As Sub stdcall(栈 As 栈类型 Ptr)
   Dim Fly_置栈整数 As Sub stdcall(栈 As 栈类型 Ptr, 值 As LongInt)
   Dim Fly_置栈文本 As Sub stdcall(栈 As 栈类型 Ptr, 值 As ZString Ptr)
   Dim Fly_置栈小数 As Sub stdcall(栈 As 栈类型 Ptr, 值 As Double)
   Dim Fly_置栈数据 As Sub stdcall(栈 As 栈类型 Ptr, 值 As UByte Ptr, 长度 As Integer)
   Dim Fly_取容器组上限 As Function stdcall(容器 As 容器类型 Ptr) As Integer
   Dim Fly_取容器节模型 As Sub stdcall(容器 As 容器类型 Ptr, ByRef 模型 As UByte, ByRef 基地址 As Integer, ByRef 模块索引 As Integer, ByRef 库句柄 As HANDLE) '如果模块索引=-1则表示是库结构，如果库句柄=-1则表示是核心结构
   Dim Fly_定义核心节 As Sub stdcall(容器 As 容器类型 Ptr, 模型 As UByte, 基地址 As Integer, 模块索引 As Integer)
   Dim Fly_取节结构 As Function stdcall(模型 As UByte, 库句柄 As HANDLE) As 库结构类型
   Dim Fly_取核心节结构 As Function stdcall(模型 As UByte, 模块索引 As Integer) As 库结构类型
   Dim Fly_容器拷贝 As Sub stdcall(目标容器 As 容器类型 Ptr, 源容器 As 容器类型 Ptr, 只实例结构 As BOOL = False) 
   Dim Fly_置容器对象 As Sub stdcall(容器 As 容器类型 Ptr, 对象 As 对象类型 Ptr)
   Dim Fly_对象拷贝 As Sub stdcall(目标对象 As 对象类型 Ptr, 源对象 As 对象类型 Ptr)
   
   Dim Fly_取容器信息 As Sub stdcall(容器 As 容器类型 Ptr, ByRef 结构索引 As Integer, ByRef 模块索引 As Integer, ByRef 库句柄 As HANDLE, ByRef 模型 As Integer)
   Dim Fly_置容器信息 As Sub stdcall(容器 As 容器类型 Ptr, ByVal 结构索引 As Integer, ByVal 模块索引 As Integer, ByVal 库句柄 As HANDLE, ByVal 模型 As Integer)
   Dim Fly_容器组构造 As Sub stdcall(容器 As 容器类型 Ptr)
   Dim Fly_容器组析构 As Sub stdcall(容器 As 容器类型 Ptr)
   Dim Fly_容器组拷贝 As Sub stdcall(容器 As 容器类型 Ptr, 源 As 容器类型 Ptr) 
   
   Dim Fly_取结构默认方法 As Sub stdcall(结构索引 As Integer, ByRef 构造函数 As ULongInt, ByRef 析构函数 As ULongInt, ByRef 拷贝函数 As ULongInt)
   Dim Fly_取栈类型 As Function stdcall(栈 As 栈类型 Ptr) As UByte
   Dim Fly_取对象类型 As Function stdcall(对象 As 对象类型 Ptr) As UByte
End Type

Type 函数信息类型
   Dim 名称 As ZString Ptr
   Dim 地址 As Sub stdcall(栈 As Any Ptr,ByRef 栈顶 As Integer)
End Type

Type 库信息类型
   ReDim 函数表(Any) As 函数信息类型
   ReDim 结构表(Any) As 库结构类型
End Type

Type 库类型
   Dim 库句柄 As HANDLE
   Dim 绝对路径 As String
   Dim 库信息 As 库信息类型
End Type

