Const Fly_Step = 0
Const Fly_ThreadStart = 1
Const Fly_ThreadStop = 2
Const Fly_Statement = 3
Const Fly_SendDebugMsg = 4
Const Fly_SendText = 5
Const Fly_MainThreadStart = 6
Const Fly_SendDebug = 7
Const Fly_MemoryExtend = 8
Const Fly_LoadLib = 9
Const Fly_LoadPak = 10
Const Fly_FreeLib = 11
Const Fly_FreePak = 12

Const GetMemoryBase = 0
Const GetMemorySize = 1

#define  虚拟机版本  1
#define  块  1024

#define  类型_整数  0
#define  类型_文本  1
#define  类型_小数  2
#define  类型_数据  3

#define 类型模型_整数   0
#define 类型模型_文本   1
#define 类型模型_小数   2
#define 类型模型_数据   3

'基本模型，在初始化容器的时候会全部定义为相同类型

#define  容器类型_值  0
#define  容器类型_容器指针  1
#define  容器类型_容器组    2

#define  容器数量 256
#define  执行栈尺寸  128
#define  调用栈尺寸  2048
#define  循环栈尺寸  32
#define  变量循环栈尺寸  32

#define  Push_压入容器 0
#define  Push_压入整数  1
#define  Push_压入文本 2
#define  Push_压入小数  3
#define  Push_压入相对地址  4
#define  Push_压入数据      5

#define 栈_容器  0
#define 栈_整数  1
#define 栈_文本  2
#define 栈_小数 3
#define 栈_地址 4
#define 栈_数据 5

#define  数据_字节    0
#define  数据_字      1
#define  数据_双字    2
#define  数据_四字    3

Const 控件类型_按钮 = 0
Const 控件类型_编辑框 = 1
Const 控件类型_标签 = 2
Const 控件类型_时钟 = 3
Const 控件类型_图片框 = 4
Const 控件类型_选择框 = 5

Const 窗口_事件_尺寸被改变 = 0
Const 窗口_事件_将被销毁 =1
Const 窗口_事件_鼠标左键被按下 =2
Const 窗口_事件_鼠标左键被放开 =3
Const 窗口_事件_鼠标右键被按下=4
Const 窗口_事件_鼠标右键被放开=5
Const 窗口_事件_获取焦点=6
Const 窗口_事件_失去焦点=7
Const 窗口_事件_按下某键=8
Const 窗口_事件_放开某键=9
Const 窗口_事件_创建完毕=10
Const 窗口_事件_被双击=11
Const 窗口_事件_将被关闭 = 12

Const 按钮_事件_被单击=0
Const 按钮_事件_获取焦点=1
Const 按钮_事件_失去焦点 = 2

Const 编辑框_事件_内容被改变=0
Const 编辑框_事件_字符输入=1
Const 编辑框_事件_获取焦点=2
Const 编辑框_事件_失去焦点=3

Const 标签_事件_被单击 = 0
Const 标签_事件_被双击 = 1

Const 时钟_事件_周期事件 = 0

Const 选择框_事件_被单击=0
Const 选择框_事件_获取焦点=1
Const 选择框_事件_失去焦点 = 2


Const 按钮_类型_普通按钮 = 0
Const 按钮_类型_默认按钮 = 1

Const 编辑框_类型_单行编辑框 = 0
Const 编辑框_类型_多行编辑框 = 1
Const 编辑框_类型_密码框 = 2

Const 标签_对齐方式_居中对齐 = 0
Const 标签_对齐方式_左对齐 = 1
Const 标签_对齐方式_右对齐 = 2
Const 标签_对齐方式_垂直对齐 = 3

Const 图片框_显示方式_拉伸 = 0
Const 图片框_显示方式_居中 = 1

Const 选择框_逻辑 = 0
Const 选择框_三态 = 1

Const 选择框_状态_未选中 = 0
Const 选择框_状态_已选中 = 1
Const 选择框_状态_不确定 = 2

#define Err_未找到指定类型模型   "未找到指定类型模型:'%d'"
#define Err_运算过程发现类型不匹配   "'%d'类型无法与'%d'类型进行运算"
#define Err_不能直接操作容器         "不能直接操作容器"
#define Err_数据类型不能隐式转换     "数据类型不能隐式转换"
#define Err_参数类型不匹配           "参数类型不匹配"

#define is_nop    0
#define is_push   1
#define is_pop    2
#define is_popn   3
#define is_jmp    4
#define is_je     5
#define is_jne    6
#define is_ja     7
#define is_jae    8
#define is_jb     9
#define is_jbe    10
#define is_add    11
#define is_sub    12
#define is_mul    13
#define is_div    14
#define is_mod    15
#define is_tcall  16
#define is_sleep  17
#define is_ret    18
#define is_write  19
#define is_read   20
#define is_quit   21
#define is_call   22
#define is_exit   23
#define is_str    24
#define is_msgbox 25
#define is_stop   26
#define is_set    27
#define is_push_obj_this   28
#define is_node            29
#define is_bound           30
#define is_ptr             31
#define is_set_box          32
#define is_jz              33
#define is_jnz             34
#define is_and             35
#define is_or              36
#define is_xor             37
#define is_not             38
#define is_band            39
#define is_bor             40
#define is_bxor            41
#define is_bnot            42
#define is_pop_box         43
#define is_wait            44
#define is_round           45
#define is_left            46
#define is_right           47
#define is_mid             48
#define is_InStr           49
#define is_InStrRev        50
#define is_UCase           51
#define is_LCase           52
#define is_Len             53
#define is_ValInt          54
#define is_ValDou          55
#define is_LoadWindow      56
#define is_GetQuitMsg      57
#define is_PostQuitMsg     58
#define is_DoEvents        59
#define is_DestroyWindow   60
#define  is_src            61
#define  is_next           62
#define  is_obj           63
#define  is_join           64
#define  is_del           65
#define  is_data          66
#define  is_GetDataLen     67
#define  is_GetDataLeft    68
#define  is_GetDataRight   69
#define  is_GetDataMid     70
#define  is_InData         71
#define  is_InDataRev      72
#define  is_SetDataMid     73
#define  is_SpaceData      74
#define  is_GetDataVal     75
#define  is_SetDataVal     76
#define is_ReadFile     77
#define is_WriteFile 78
#define  is_MkDir    79
#define is_RmDir 80
#define is_FileCopy 81
#define is_FileMove 82
#define is_FileKill 83
#define is_FileName 84
#define is_GetLoadDir 85
#define is_GetLoadName 86
#define is_TerminateThread 87
#define is_ResumeThread 88
#define is_SuspendThread 89
#define is_InitializeCriticalSection 90
#define is_DeleteCriticalSection 91
#define is_EnterCriticalSection 92
#define is_LeaveCriticalSection 93
#define is_Launch 94
#define is_GetPack 95
#define is_GetPackExport 96
#define is_IsFileExist 97
#define is_IsObj  98

#define is_sgn 99
#define is_abs 100
#define is_int 101
#define is_fix 102
#define is_pow 103
#define is_sqr 104
#define is_sin 105
#define is_cos 106
#define is_tan 107
#define is_atn 108
#define is_log 109
#define is_exp 110
#define is_randomize 111
#define is_rnd 112
#define is_GetTickCount 113


#define is_loop 114
#define is_loop_up 115
#define is_loop_pure 116


#define is_insert 117
#define is_remove 118

#define is_LoadLib 119
#define is_FreeLib 120
#define is_GetLibFunc 121
#define is_CallLib 122

#define is_loop_pop 123

#define is_for 124
#define is_for_up 125
#define is_for_pop 126

#define is_other   255

