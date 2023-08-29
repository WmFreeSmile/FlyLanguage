Dim Shared 离开消息 As LongInt
Dim Shared 消息队列 As CQueue
Dim Shared 内部_窗口事件() As 窗口事件类型
Dim Shared 内部_基窗口句柄 As hWnd
Dim Shared 内部_原窗口函数() As 窗口函数类型

Sub 注册默认窗口类()
   Dim MyWndClass As WNDCLASSEXA
   
   MyWndClass.cbSize = SizeOf(MyWndClass)
   MyWndClass.style = CS_HREDRAW Or CS_VREDRAW Or CS_DBLCLKS
   MyWndClass.lpfnWndProc = @内部_默认窗口过程
   MyWndClass.cbClsExtra = 0
   MyWndClass.cbWndExtra = 0
   MyWndClass.HINSTANCE = GetModuleHandleA(0)
   MyWndClass.HICON = LoadIconA(MyWndClass.HINSTANCE ,"AAAAA_APPICON")
   MyWndClass.HCURSOR = LoadCursorA(0 ,IDC_ARROW)
   
   MyWndClass.hbrBackground = COLOR_BTNFACE + 1
   MyWndClass.lpszMenuName = @""
   MyWndClass.lpszClassName = @"Fly_Window"
   
   RegisterClassExA(@MyWndClass)
End Sub

Function 内部_取窗口事件ID(窗口句柄 As hWnd) As Long
   For i As Integer = 0 To UBound(内部_窗口事件)
      If 内部_窗口事件(i).窗口句柄 = 窗口句柄 Then
         Return 内部_窗口事件(i).ID
      End If
   Next
   Return -1
End Function

Sub 内部_子类化处理(参数_父窗口 As hWnd)
   
   For i As Integer = 0 To UBound(内部_原窗口函数)
      If 内部_原窗口函数(i).窗口句柄 = 参数_父窗口 Then
         Return
      End If
   Next
   
   Dim 索引 As Integer
   
   索引 = UBound(内部_原窗口函数) + 1
   
   ReDim Preserve 内部_原窗口函数(索引)
   #ifdef __FB_64BIT__
   内部_原窗口函数(索引).窗口函数 = GetWindowLongPtrA(参数_父窗口 ,GWLP_WNDPROC)
   内部_原窗口函数(索引).窗口句柄 = 参数_父窗口
   SetWindowLongPtrA(参数_父窗口 ,GWLP_WNDPROC ,@内部_子窗口过程)
   #else
   内部_原窗口函数(索引).窗口函数 = GetWindowLongA(参数_父窗口 ,GWL_WNDPROC)
   内部_原窗口函数(索引).窗口句柄 = 参数_父窗口
   SetWindowLongA(参数_父窗口 ,GWL_WNDPROC ,Cast(Long ,@内部_子窗口过程))
   #endif
   
End Sub

Function ID取窗口索引(ID As Long) As Integer
   For i As Integer = 0 To UBound(窗口表)
      If 窗口表(i).ID = ID Then
         Return i
      End If
   Next
   Return -1
End Function

Sub ID取控件索引(ID As Long ,ByRef 窗口索引 As Integer ,ByRef 控件索引 As Integer)
   For i As Integer = 0 To UBound(窗口表)
      For j As Integer = 0 To UBound(窗口表(i).控件表)
         If 窗口表(i).控件表(j).ID = ID Then
            窗口索引=i
            控件索引 = j
            Return
         End If
      Next
   Next
   窗口索引 = -1
   控件索引 = -1
End Sub

Function ID取窗口句柄(ID As Long) As hWnd
   For i As Integer = 0 To UBound(内部_窗口事件)
      If 内部_窗口事件(i).ID = ID Then
         Return 内部_窗口事件(i).窗口句柄
      End If
   Next
   Return 0
End Function

Function 内部_默认窗口过程 stdcall(ByVal p_hwnd As hWnd ,ByVal p_msg As UINT ,ByVal p_wparam As wParam ,ByVal p_lparam As lParam) As LResult
   Dim 索引 As Integer
   Dim 控件索引 As Integer
   Dim ID As Long
   Dim 消息 As 消息类型 Ptr
   
   Select Case As Const p_msg
      Case WM_CREATE'窗口创建完毕
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_创建完毕
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
      Case WM_CLOSE '窗口关闭
         ID = 内部_取窗口事件ID(p_hwnd)
         
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_将被关闭
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If
         End If
      Case WM_DESTROY'窗口销毁
         ID = 内部_取窗口事件ID(p_hwnd)
         
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_将被销毁
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
               If 窗口表(索引).事件_将被销毁 = 0 Then'事件不存在，进行自动离开
                  If p_hwnd = 内部_基窗口句柄 Then
                     离开消息 = 1
                     PostQuitMessage(0)
                  End If
               End If
            End If
         End If
      Case WM_SIZE '窗口大小被改变
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_尺寸被改变
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
      Case WM_MOUSEMOVE'鼠标位置被移动
         '这个回调开销太大，暂时不开放
      Case WM_LBUTTONDBLCLK'鼠标左键双击
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_被双击
               消息->参数1 = p_lparam And 65535 
               消息->参数2 = p_lparam Shr 16
               消息->参数3 = p_wparam
               消息->参数4 = 0
               
               'Print "wnd" ,消息->参数1 ,消息->参数2 ,消息->参数3 ,消息->参数4
               'Sleep
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If
         End If
      Case WM_LBUTTONDOWN '窗口左键被按下
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_鼠标左键被按下
               消息->参数1 = p_lparam And 65535 
               消息->参数2 = p_lparam Shr 16
               消息->参数3 = p_wparam
               消息->参数4 = 0
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_LBUTTONUP '窗口左键被放开
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_鼠标左键被放开
               消息->参数1 = p_lparam And 65535 
               消息->参数2 = p_lparam Shr 16
               消息->参数3 = p_wparam
               消息->参数4 = 0
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_RBUTTONDOWN '鼠标右键被按下
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_鼠标右键被按下
               消息->参数1 = p_lparam And 65535 
               消息->参数2 = p_lparam Shr 16
               消息->参数3 = p_wparam
               消息->参数4 = 0
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_RBUTTONUP '鼠标右键被放开
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_鼠标右键被放开
               消息->参数1 = p_lparam And 65535 
               消息->参数2 = p_lparam Shr 16
               消息->参数3 = p_wparam
               消息->参数4 = 0
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_SETFOCUS '获取焦点
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_获取焦点
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_KILLFOCUS '失去焦点
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_失去焦点
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_KEYDOWN '按下某键
         
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_按下某键
               消息->参数1 = p_wparam
               消息->参数2 = p_lparam
               消息->参数3 = 0
               消息->参数4 = 0
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_KEYUP '放开某键
         
         ID = 内部_取窗口事件ID(p_hwnd)
         If ID <> -1 Then
            索引 = ID取窗口索引(ID)
            If 索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(索引).基地址
               消息->ID = ID
               消息->值 = 窗口_事件_放开某键
               消息->参数1 = p_wparam
               消息->参数2 = p_lparam
               消息->参数3 = 0
               消息->参数4 = 0
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
            End If 
         End If
         
      Case WM_NCHITTEST '鼠标移动或按下或放开左键，不知道是什么鬼玩意，反正不回调就完了
         
         
   End Select
   
   Function = DefWindowProcA(p_hwnd ,p_msg ,p_wparam ,p_lparam)
End Function

Function 内部_取原窗口函数(参数_句柄 As hWnd) As MSGWND
   For i As Integer = 0 To UBound(内部_原窗口函数)
      If 内部_原窗口函数(i).窗口句柄 = 参数_句柄 Then
         
         Return 内部_原窗口函数(i).窗口函数
      End If
   Next
   Return 0
End Function

Function 内部_子窗口过程 stdcall(ByVal p_hwnd As hWnd ,ByVal p_msg As UINT ,ByVal p_wparam As wParam ,ByVal p_lparam As lParam) As LResult
   Dim 局部_原窗口函数 As MSGWND
   Dim ID As Long
   Dim 窗口索引 As Integer
   Dim 控件索引 As Integer
   Dim 消息 As 消息类型 Ptr
   
   Select Case As Const p_msg
      Case WM_COMMAND
         ID = 内部_取窗口事件ID(p_hwnd)
         
         If ID <> -1 Then
            ID取控件索引(p_wparam And 65535 ,窗口索引 ,控件索引)
            
            If 窗口索引 <> -1 AndAlso 控件索引 <> -1 Then
               If 窗口表(窗口索引).控件表(控件索引).是否静态 Then
                  
                  Select Case As Const 窗口表(窗口索引).控件表(控件索引).类型
                     Case 控件类型_标签
                        If p_wParam Shr 16 = 0 Then ' 静态控件被单击
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 标签_事件_被单击
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = 1 Then ' 静态控件被双击
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 标签_事件_被双击
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                           
                        End If
                        
                  End Select
                  
                  
                  
               Else
                  
                  Select Case As Const 窗口表(窗口索引).控件表(控件索引).类型
                     Case 控件类型_按钮
                        If p_wparam Shr 16 = BN_CLICKED AndAlso p_lparam <> 0 Then' 按钮\图形钮\ 被单击
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 按钮_事件_被单击
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = BN_SETFOCUS Then'按钮获取焦点
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 按钮_事件_获取焦点
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = BN_KILLFOCUS Then'按钮失去焦点
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 按钮_事件_失去焦点
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        End If
                        
                     Case 控件类型_选择框
                        
                        If p_wparam Shr 16 = BN_CLICKED AndAlso p_lparam <> 0 Then' 按钮\图形钮\ 被单击
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 选择框_事件_被单击
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = BN_SETFOCUS Then'获取焦点
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 选择框_事件_获取焦点
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = BN_KILLFOCUS Then'失去焦点
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 选择框_事件_失去焦点
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        End If
                        
                     Case 控件类型_编辑框
                        If p_wparam Shr 16 = 512 Then '编辑框失去焦点
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 编辑框_事件_失去焦点
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = 256 Then '编辑框获取焦点
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 编辑框_事件_获取焦点
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = EN_CHANGE Then '编辑框内容被改变
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 编辑框_事件_内容被改变
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        ElseIf p_wparam Shr 16 = 1024 Then '编辑框字符输入
                           消息 = New 消息类型
                           消息->基地址 = 窗口表(窗口索引).基地址
                           消息->ID = p_wparam And 65535
                           消息->值 = 编辑框_事件_字符输入
                           
                           #ifdef __FB_64BIT__
                           消息队列.Enqueue(Cast(ULongInt ,消息))
                           #else
                           消息队列.Enqueue(Cast(ULong ,消息))
                           #endif
                           Return 0
                        End If
                  End Select
               End If
            End If
         End If
         
      Case WM_TIMER '时钟周期事件
         ID = 内部_取窗口事件ID(p_hwnd)
         
         If ID <> -1 Then
            ID取控件索引(p_wparam And 65535, 窗口索引, 控件索引)
            
            If 窗口索引 <> -1 AndAlso 控件索引 <> -1 Then
               消息 = New 消息类型
               消息->基地址 = 窗口表(窗口索引).基地址
               消息->ID = p_wparam And 65535
               消息->值 = 时钟_事件_周期事件
               
               #ifdef __FB_64BIT__
               消息队列.Enqueue(Cast(ULongInt ,消息))
               #else
               消息队列.Enqueue(Cast(ULong ,消息))
               #endif
               Return 0
            End If
         End If
   End Select
   
   局部_原窗口函数 = 内部_取原窗口函数(p_hwnd)
   If 局部_原窗口函数 <> 0 Then
      Function = CallWindowProcA(局部_原窗口函数 ,p_hwnd ,p_msg ,p_wparam ,p_lparam)
   Else
      Function = DefWindowProc(p_hwnd ,p_msg ,p_wparam ,p_lparam)
   End If
End Function

Sub 载入窗口(ID As Long, 基地址 As Integer)
   Dim 索引 As Integer = -1
   
   For i As Integer = 0 To UBound(窗口表)
      If 窗口表(i).ID = ID AndAlso 窗口表(i).基地址 = 基地址 Then
         索引 = i
      End If
   Next
   
   If 索引 = -1 Then
      Return
   End If
   
   窗口表(索引).载入窗口()
End Sub

Sub 处理消息()
   Dim l_Msg As MSG
   If GetMessageA(@l_Msg ,0 ,0 ,0) <> 0 Then
      TranslateMessage(@l_Msg)
      DispatchMessageA(@l_Msg)
   End If
End Sub


Sub 模块_设置字体(参窗口句柄 As hWnd ,参字体名 As String ,ByVal 参字号大小 As Long = -1 ,加粗 As BOOL = 0 ,斜体 As BOOL = 0 ,下划线 As BOOL = 0 ,删除线 As BOOL = 0)
   If 参字号大小 = -1 Then
      参字号大小 = 20
   End If
   
   Dim 旧字体句柄 As LResult
   Dim 字体句柄 As HFONT
   旧字体句柄 = SendMessageA(参窗口句柄 ,WM_GETFONT ,0 ,0)
   字体句柄 = CreateFontA(参字号大小 ,0 ,0 ,0 ,加粗 ,斜体 ,下划线 ,删除线 ,GB2312_CHARSET ,0 ,0 ,0 ,0 ,参字体名)
   SendMessage(参窗口句柄 ,WM_SETFONT ,字体句柄 ,1)
   DeleteObject(旧字体句柄)
End Sub

