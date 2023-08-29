


Sub 暂停(栈() As 栈类型 ,栈顶 As LongInt,调用栈() As 调用栈类型,调用栈顶 As LongInt,容器() As 容器类型)
   
   'Print 栈(栈顶 -1).对象值->小数值,栈(栈顶 -1).对象值->类型
   'Sleep
   
   暂停所有线程()
   
   Print "执行栈 下标:";LBound(栈) ,"上标:";UBound(栈) ,"栈顶:";栈顶
   
   Print ""
   Print ""

   Print "调用栈 下标:";LBound(调用栈) ,"上标:";UBound(调用栈) ,"栈顶:";调用栈顶
   
   Print ""
   Print ""
   
   监听指令(栈() ,容器())
End Sub

Sub 监听指令(栈() As 栈类型,容器() As 容器类型)
   Dim 指令 As String
   Dim 指令参数 As String
   Dim 当前参数 As String
   
   Print ">";
   
   Line Input 指令参数
   
   
   Dim StrPos As Integer
   StrPos = InStr(1 ,指令参数 ," ")
   If StrPos = 0 Then
      If UCase(指令参数) = UCase("Start") Then
         恢复所有线程()
         
         Return
      '
      'ElseIf UCase(指令参数) = UCase("PutMemUse") Then
      '   
      '   If ARRAYSIZE(已分配堆表) <> 0 Then
      '      For i As Integer = LBound(已分配堆表) To UBound(已分配堆表)
      '         Print "Address:";已分配堆表(i).地址,"Size:";已分配堆表(i).长度
      '      Next
      '   End If
      '   
      '   
      'ElseIf UCase(指令参数) = UCase("PutMemFree") Then
      '   
      '   If ARRAYSIZE(未分配堆表) <> 0 Then
      '      For i As Integer = LBound(未分配堆表) To UBound(未分配堆表)
      '         Print "Address:";未分配堆表(i).地址,"Size:";未分配堆表(i).长度
      '      Next
      '   End If
      '   
      '   
      '   
      ElseIf 指令参数 = "" Then
         监听指令(栈(),容器())
      Else
         Print "未知指令:";指令参数
         监听指令(栈(),容器())
      End If
   Else
      指令 = Left(指令参数 ,StrPos -1)
      
      While (StrPos <> 0)
         If Mid(指令参数 ,StrPos ,1) = " " Then
            StrPos = StrPos + 1
         Else
            Exit While
         End If
      Wend
      指令参数 = Right(指令参数 ,Len(指令参数) - StrPos + 1)
      
      StrPos = InStr(1 ,指令参数 ," ")
      If StrPos <> 0 Then
         当前参数 = Left(指令参数 ,StrPos -1)
      Else
         当前参数 = 指令参数
      End If
      
      If UCase(指令) = UCase("PutStack") Then
         Print "Type:";栈(ValLng(当前参数)).类型;" ";
         
         Select Case 栈(ValLng(当前参数)).类型
            Case 栈_容器
               Print "BoxPtr:";栈(ValLng(当前参数)).栈空间.容器值;" ";
               Print "BoxType:";栈(ValLng(当前参数)).栈空间.容器值->类型;" ";
               
               Select Case 栈(ValLng(当前参数)).栈空间.容器值->类型
                  Case 容器类型_容器组
                     Print "BoxGroup:";"bound:";UBound(栈(ValLng(当前参数)).栈空间.容器值->容器组)
                     For i As Integer = 0 To UBound(栈(ValLng(当前参数)).栈空间.容器值->容器组)
                        Print "[";i;"]=";栈(ValLng(当前参数)).栈空间.容器值->容器组(i)
                     Next
               End Select
            Case 栈_整数
               Print "Val:";栈(ValLng(当前参数)).转对象()->整数值;" ";
         End Select
         Print ""
         
      ElseIf UCase(指令)=UCase("PutBox") Then
         
      ElseIf UCase(指令) = UCase("Is") Then
         Print "Is:";IS_GetStr(指令表(ValLng(当前参数)).指令)
         
      Else
         Print "未知指令:";指令参数
         监听指令(栈(),容器())
      End If
      
   End If
   监听指令(栈(),容器())
End Sub
