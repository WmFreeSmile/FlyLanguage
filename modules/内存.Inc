

Function 分配堆内存 stdcall(长度 As LongInt) As LongInt
   EnterCriticalSection(@临界资源_内存池)
   Dim 地址 As LongInt
   
   Dim 分配成功 As BOOL
   
   If UBound(未分配堆表) - LBound(未分配堆表) + 1 <> 0 Then
      For i As Integer = LBound(未分配堆表) To UBound(未分配堆表)
         Dim 索引 As Integer
         
         If 未分配堆表(i).长度 > 长度 Then
            地址 = 未分配堆表(i).地址
            
            未分配堆表(i).长度 = 未分配堆表(i).长度 - 长度
            未分配堆表(i).地址 = 未分配堆表(i).地址 + 长度
            
            ArrayAdd(已分配堆表 ,索引)
            
            已分配堆表(索引).地址 = 地址
            已分配堆表(索引).长度 = 长度
            
            分配成功 = True
            Exit For
            
         ElseIf 未分配堆表(i).长度 = 长度 Then
            地址 = 未分配堆表(i).地址
            
            ArrayDel(未分配堆表 ,i)
            
            ArrayAdd(已分配堆表 ,索引)
            
            已分配堆表(索引).地址 = 地址
            已分配堆表(索引).长度 = 长度
            
            分配成功 = True
            Exit For
            
         End If
      Next
   End If

   If 分配成功 = False Then
      地址 = 创建堆内存_已分配(长度)
      
      If 调试模式 Then
         发送调试信息(Fly_MemoryExtend)
      End If
   End If
   
   memset(内存区域 + 地址 ,0 ,长度)
   LeaveCriticalSection(@临界资源_内存池)
   
   Function = 地址
End Function

Sub 释放堆内存 stdcall(地址 As LongInt)
   EnterCriticalSection(@临界资源_内存池)
   
   If UBound(已分配堆表) - LBound(已分配堆表) + 1 <> 0 Then
      For i As Integer = LBound(已分配堆表) To UBound(已分配堆表)
         If 已分配堆表(i).地址 = 地址 Then
            Dim 索引 As Integer
            ArrayAdd(未分配堆表 ,索引)
            
            未分配堆表(索引).地址 = 已分配堆表(i).地址
            未分配堆表(索引).长度 = 已分配堆表(i).长度
            
            memset(内存区域 + 已分配堆表(i).地址 ,0 ,已分配堆表(i).长度)
            
            ArrayDel(已分配堆表 ,i)
            
            对齐内存块(地址)  ' 对齐空闲内存块
            Exit For
         End If
      Next
   End If
   
   LeaveCriticalSection (@临界资源_内存池)
End Sub



Sub 对齐内存块(ByVal 地址 As LongInt) '基地址
   Dim 索引 As Integer
   
   If UBound(未分配堆表) - LBound(未分配堆表) + 1 <> 0 Then
      索引 = 未分配堆表_取索引(地址)
      For i As Integer = LBound(未分配堆表) To UBound(未分配堆表) 
         If 未分配堆表(i).地址 = 未分配堆表(索引).地址 + 未分配堆表(索引).长度 Then' 往右对齐
            未分配堆表(索引).长度 = 未分配堆表(索引).长度 + 未分配堆表(i).长度
            
            ArrayDel(未分配堆表 ,i)
            
            对齐内存块(地址)
            
            Exit For
            
         ElseIf 未分配堆表(i).地址 + 未分配堆表(i).长度 = 未分配堆表(索引).地址 Then' 往左对齐
            '未分配堆表(索引).地址 = 未分配堆表(索引).地址 - 未分配堆表(i).长度
            '未分配堆表(索引).长度 = 未分配堆表(索引).长度 + 未分配堆表(i).长度
            
            
            'Dim 临时地址 As Integer = 未分配堆表(索引).地址
            
            'ArrayDel(未分配堆表 ,i)
            
            '对齐内存块(临时地址)
            
            未分配堆表(i).长度 = 未分配堆表(i).长度 + 未分配堆表(索引).长度
            地址 = 未分配堆表(i).地址
            
            ArrayDel(未分配堆表 ,索引)
            
            对齐内存块(地址)
            
            Exit For
         End If
      Next
   End If
   
End Sub

Function 未分配堆表_取索引(地址 As LongInt) As Integer
   If UBound(未分配堆表) - LBound(未分配堆表) + 1 <> 0 Then
      For i As Integer = LBound(未分配堆表) To UBound(未分配堆表)
         If 未分配堆表(i).地址 = 地址 Then
            return i
         End If
      Next
   End If
   Return -1
End Function

Function 已分配堆表_取索引(地址 As LongInt) As Integer
   If UBound(未分配堆表) - LBound(未分配堆表) + 1 <> 0 Then
      For i As Integer = LBound(已分配堆表) To UBound(已分配堆表)
         If 已分配堆表(i).地址 = 地址 Then
            Return i
         End If
      Next
      Return -1
   End If
End Function

Function 创建堆内存_未分配(长度 As LongInt) As LongInt
   
   Dim 索引 As Integer
   
   ArrayAdd(未分配堆表 ,索引)
   
   未分配堆表(索引).地址 = 内存范围
   未分配堆表(索引).长度 = 长度
   
   内存范围 = 内存范围 + 长度
   
   内存区域 = realloc(内存区域 ,内存范围)
   
   If 内存区域 = 0 Then
      FlyDebug("内存不足")
   End If
   Return 未分配堆表(索引).地址
End Function

Function 创建堆内存_已分配(长度 As LongInt) As LongInt
   Dim 索引 As Integer
   
   ArrayAdd(已分配堆表 ,索引)
   
   已分配堆表(索引).地址 = 内存范围
   已分配堆表(索引).长度 = 长度
   
   内存范围 = 内存范围 + 长度
   
   内存区域 = realloc(内存区域 ,内存范围)
   
   If 内存区域 = 0 Then
      FlyDebug("内存不足")
   End If
   
   Return 已分配堆表(索引).地址
End Function

