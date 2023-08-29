


Sub OGL_Create(ByVal Window_ID As Long, ByVal nBitsPerPel As Long = 32, ByVal cDepthBits As Long = 24, ByRef Out_hDC As HDC, ByRef Out_hRC As HGLRC)
   Dim hWnd As hWnd
   hWnd = ID取窗口句柄(Window_ID)
   
   If hWnd = NULL Then Return
   
   Out_hDC = GetDC(hWnd)
   If Out_hDC = NULL Then Return
   
   Dim pfd As PIXELFORMATDESCRIPTOR
   pfd.nSize      = SizeOf(PIXELFORMATDESCRIPTOR)
   pfd.nVersion   = 1
   pfd.dwFlags    = PFD_DRAW_TO_WINDOW Or PFD_SUPPORT_OPENGL Or PFD_DOUBLEBUFFER
   pfd.iPixelType = PFD_TYPE_RGBA
   pfd.cColorBits = nBitsPerPel
   pfd.cDepthBits = cDepthBits
   
   Dim pf As Long = ChoosePixelFormat(Out_hDC, @pfd)
   If pf = 0 Then
      Return
   End If
   
   If SetPixelFormat(Out_hDC, pf, @pfd) = False Then
      Return
   End If

   ' // Create a new OpenGL rendering context
   Out_hRC = wglCreateContext(Out_hDC)
   If Out_hRC = NULL Then
      Return
   End If

   ' // Make it current
   If wglMakeCurrent(Out_hDC, Out_hRC) = False Then
      Return
   End If

End Sub

Sub OGL_Delete(window_ID As Long,p_hDC As HDC,p_hRC As HGLRC)
   Dim hWnd As hWnd
   hWnd = ID取窗口句柄(window_ID)
   
   If hWnd = NULL Then Return
   
   ' // Release the device and rendering contexts
   wglMakeCurrent p_hDC, NULL
   ' // Make the rendering context no longer current
   wglDeleteContext p_hRC
   ' // Release the device context
   ReleaseDC hWnd, p_hDC
End Sub
