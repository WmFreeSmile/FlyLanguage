
#macro ArrayDel(arr,i)
If LBound(arr) = UBound(arr) AndAlso i = UBound(arr) Then
   Erase arr
Else
   arr(i) = arr(UBound(arr))
   ReDim Preserve arr(UBound(arr)-1)
End If
#endmacro

#macro ArrayAdd(arr,index)
ReDim Preserve arr(UBound(arr) + 1)
index = UBound(arr)
#endmacro

#macro 压入(栈,栈顶,值)
栈(栈顶) = 值
栈顶 = 栈顶 + 1
#endmacro

#macro 弹出(栈,栈顶,返回值)
If 栈顶 = 0 Then
   FlyDebug ("非法弹出栈")
End If
栈顶 = 栈顶 -1
返回值=栈(栈顶)
#endmacro

#define 取栈参数_1_0(栈,栈顶)  栈(栈顶-1)

#define 取栈参数_2_0(栈,栈顶) 栈(栈顶-2)
#define 取栈参数_2_1(栈,栈顶) 栈(栈顶-1)

#define 取栈参数_3_0(栈,栈顶) 栈(栈顶-3)
#define 取栈参数_3_1(栈,栈顶) 栈(栈顶-2)
#define 取栈参数_3_2(栈,栈顶) 栈(栈顶-1)

#define 取栈参数_4_0(栈,栈顶) 栈(栈顶-4)
#define 取栈参数_4_1(栈,栈顶) 栈(栈顶-3)
#define 取栈参数_4_2(栈,栈顶) 栈(栈顶-2)
#define 取栈参数_4_3(栈,栈顶) 栈(栈顶-1)

#define 取栈参数_5_0(栈,栈顶) 栈(栈顶-5)
#define 取栈参数_5_1(栈,栈顶) 栈(栈顶-4)
#define 取栈参数_5_2(栈,栈顶) 栈(栈顶-3)
#define 取栈参数_5_3(栈,栈顶) 栈(栈顶-2)
#define 取栈参数_5_4(栈,栈顶) 栈(栈顶-1)

#define 取栈参数_6_0(栈,栈顶) 栈(栈顶-6)
#define 取栈参数_6_1(栈,栈顶) 栈(栈顶-5)
#define 取栈参数_6_2(栈,栈顶) 栈(栈顶-4)
#define 取栈参数_6_3(栈,栈顶) 栈(栈顶-3)
#define 取栈参数_6_4(栈,栈顶) 栈(栈顶-2)
#define 取栈参数_6_5(栈,栈顶) 栈(栈顶-1)

Function 文件取目录(路径 As String) As String
   Function = Left(路径 ,InStrRev(路径 ,"\") -1)
End Function

Sub PrintBytes(address_ As Any Ptr ,Size_ As Integer)
   Dim temp As String
   
   temp = "{"
   
   For i As Integer = 0 To Size_-1
      temp = temp + Str( *Cast(UByte Ptr ,address_ + i))
      
      If i <> Size_-1 Then
         temp=temp+","
      End If
      
   Next
   
   temp = temp + "}"
   
   Debug.Print(temp)
End Sub

Function 取文件绝对路径(路径 As String) As String
   Dim ptr_str As ZString Ptr = Allocate(MAX_PATH)
   Dim 返回值 as string
   GetFullPathNameA(路径, MAX_PATH, ptr_str, 0)
   返回值 = *ptr_str
   Deallocate(ptr_str)
   Function = 返回值
End Function

Function 图片转位图(图片() As UByte) As HBITMAP
   Dim 位图头 As BITMAPFILEHEADER
   Dim 无关位图 As BITMAPINFOHEADER
   Dim 有关位图 As BITMAPINFO
   ReadProcessMemory(GetCurrentProcess(), @图片(0), @位图头, SizeOf(位图头), 0)
   ReadProcessMemory(GetCurrentProcess(), @图片(0) + 14, @无关位图, SizeOf(无关位图), 0)
   ReadProcessMemory(GetCurrentProcess(), @图片(0) + 14, @有关位图, SizeOf(有关位图), 0)
   
   Dim DC As HDC
   DC = GetDC(0)
   Dim 位图句柄 As HBITMAP
   位图句柄 = CreateDIBitmap(DC, @无关位图, 4, @图片(0) + 位图头.bfOffBits, @有关位图, 0)
   ReleaseDC(0, DC)
   Function = 位图句柄

End Function




