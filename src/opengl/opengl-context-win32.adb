-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.Unchecked_Conversion;
with OpenGL; use OpenGL;
with Win32; use Win32;
with Win32.User32; use Win32.User32;
with Win32.Kernel32; use Win32.Kernel32;
with Win32.GDI32; use Win32.GDI32;
with Win32.OpenGL32; use Win32.OpenGL32;
with Interfaces.C.Strings;
with System;
with ProcessLoop;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config; use Config;
with Basics; use Basics;

with Ada.Text_IO; use Ada.Text_IO;

with GUIKeys; use GUIKeys;
with GUIMouse; use GUIMouse;

package body OpenGL.Context.Win32 is

   KeyTable : constant array(0..255) of Key_Enum:=
     (KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 0..3
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 4..7
      KeyBackspace,KeyUnknown,KeyUnknown,KeyUnknown,  -- 8..11
      KeyUnknown,KeyReturn ,KeyUnknown,KeyUnknown,  -- 12..15
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 16..19
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 20..23
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 24..27
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 28..31
      KeyUnknown,KeyUnknown,KeyUnknown,KeyEnd,      -- 32..35
      KeyHome   ,KeyLeft,KeyUnknown,KeyRight,       -- 36..39
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 40..43
      KeyUnknown,KeyUnknown,KeyDelete ,KeyUnknown,  -- 44..47
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 48..51
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 52..55
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 56..59
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 60..63
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 64..67
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 68..71
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 72..75
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 76..79
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 80..83
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 84..87
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 88..91
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 92..95
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 96..99
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 100..103
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 104..107
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 108..111
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 112..115
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 116..119
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 120..123
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 124..127
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 128..131
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 132..135
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 136..139
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 140..143
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 144..147
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 148..151
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 152..155
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 156..159
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 160..163
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 164..167
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 168..171
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 172..175
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 176..179
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 180..183
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 184..187
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 188..191
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 192..195
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 196..199
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 200..203
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 204..207
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 208..211
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 212..215
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 216..219
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 220..223
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 224..227
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 228..231
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 232..235
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 236..239
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 240..243
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 244..247
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 248..251
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown); -- 252..255

   LibraryHandle : HMODULE_Type := NULLHANDLE;
   LibraryCount  : Natural:=0;
   LibraryName   : constant String:="opengl32.dll"&Character'Val(0);

   type Context_Type;
   type Context_Access is access all Context_Type;
   type Context_Type is new OpenGL.Context.Context_Type with
      record
         WindowHandle        : HWND_Type         := NULLHANDLE;
         DeviceContext       : HDC_Type          := NULLHANDLE;
         RenderContext       : HGLRC_Type        := NULLHANDLE;
         NextContext         : Context_Access    := null;
         LastContext         : Context_Access    := null;
         DestroySignalSend   : Boolean           := False;
         DoubleBuffered      : Boolean;
         ContextInitialized  : Boolean           := False;
         MouseButtonsPressed : MouseButton_Array := NoMouseButtons;
         HasCapture          : Boolean           := False;
         LibraryLoaded       : Boolean           := False;

         CSTR_ClassName : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
         CSTR_Title     : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
      end record;

   overriding
   procedure Finalize
     (Context : in out Context_Type);

   overriding
   procedure Paint
     (Context : in out Context_Type);
   ---------------------------------------------------------------------------

   Contexts : Context_Access:=null;

   function WGLGetProc
     (Str : String)
      return System.Address is

      use type System.Address;

      Result : System.Address;

      CName : Interfaces.C.char_array:=Interfaces.C.To_C(Str);

   begin

      Result := wglGetProcAddress(CName(CName'First)'Access);
      if Result=System.Null_Address then
         Result:=GetProcAddress(LibraryHandle,CName(CName'First)'Access);
         if Result=System.Null_Address then
            raise FailedToCreateContext with "wglGetProcAddress and library GetProcAddress returned null for """&Str(Str'First..Str'Last-1)&"""";
         end if;
      end if;
      return Result;

   end WGLGetProc;
   ---------------------------------------------------------------------------

   procedure Paint
     (Context : in out Context_Type) is

      Result : BOOL_Type;
      pragma Unreferenced(Result);

   begin

      if Context.RenderContext=NULLHANDLE then
         return;
      end if;

      Result:=wglMakeCurrent
        (hdc   => Context.DeviceContext,
         hglrc => Context.RenderContext);

      OpenGL.Context.Paint(OpenGL.Context.Context_Type(Context));

      if Context.DoubleBuffered then
         Result:=SwapBuffers(Context.DeviceContext);
      else
         glFinish.all;
      end if;

   end Paint;
   ---------------------------------------------------------------------------

   procedure MouseDown
     (Context     : Context_Access;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is

      Result : HWND_Type;
      pragma Unreferenced(Result);

   begin
      if Context.MouseButtonsPressed=NoMouseButtons then
         Context.HasCapture:=True;
         Result:=SetCapture(Context.WindowHandle);
      end if;
      Context.MouseButtonsPressed(MouseButton):=True;
      GUI.ContextMouseDown
        (Context     => Context_ClassAccess(Context),
         MouseButton => MouseButton,
         AbsX        => AbsX,
         AbsY        => AbsY);
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Context     : Context_Access;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is

      Result : BOOL_Type;
      pragma Unreferenced(Result);

   begin
      Context.MouseButtonsPressed(MouseButton):=False;
      if Context.MouseButtonsPressed=NoMouseButtons then
         Result:=ReleaseCapture;
         Context.HasCapture:=False;
      end if;
      GUI.ContextMouseUp
        (Context     => Context_ClassAccess(Context),
         MouseButton => MouseButton,
         AbsX        => AbsX,
         AbsY        => AbsY);
   end MouseUp;
   ---------------------------------------------------------------------------

   function WndProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LRESULT_Type;
   pragma Convention(C,WndProc);

   function WndProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LRESULT_Type is

      -- Context belonging to this window
      Context : Context_Access;

   begin
      -- Extract the Context_Access from the windows LongPtr storage
      -- associated with this window.
      -- Context will be null if WM_CREATE has not been called before
      -- PORTABILITY : Expects Context_Access to be a simple pointer
      --   Shown to work with GNAT GPL 2010 (20100603)
      declare
         function ConvertLongPtrToContextAccess is new Ada.Unchecked_Conversion
           (Source => LONG_PTR_Type,
            Target => Context_Access);
      begin
         Context:=ConvertLongPtrToContextAccess
           (GetWindowLongPtr(hWnd,0));
      end;

      case uMsg is
         when WM_MOUSELEAVE =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_PAINT =>
            Paint(Context.all);
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_CREATE =>
            declare
               Result : UINT_PTR_Type;
               pragma Unreferenced(Result);
               -- TODO: Maybe you should check this result, but a way
               --       to propagate the error to NewContext is required first.
            begin
               Result:=SetTimer
                 (hwnd        => hwnd,
                  nIDEvent    => 1,
                  uElapse     => 10,
                  lpTimerFunc => System.Null_Address);
            end;

            declare
               function ConvertLParamToCREATESTRUCTPtr is
                 new Ada.Unchecked_Conversion
                   (Source => LPARAM_Type,
                    Target => CREATESTRUCT_Access);

               CreateStruct : constant CREATESTRUCT_Access
                 :=ConvertLParamToCREATESTRUCTPtr(lParam);
               Result : LONG_Type;
               pragma Unreferenced(Result);
            begin
               Result:=User32.SetWindowLongPtr
                 (hWnd      => hWnd,
                  nIndex    => 0,
                  dwNewLong => CreateStruct.lpCreateParams);
            end;
            return 0;
         when WM_SIZE =>
            -- TODO : Extract Resize
            GUI.PropagateContextResize
              (Context => Context_ClassAccess(Context),
               Width   => Integer(LOWORD(lParam)),
               Height  => Integer(HIWORD(lParam)));
            return 0;
         when WM_SIZING =>
            return 0;
         when WM_LBUTTONDBLCLK =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_RBUTTONDBLCLK =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONDOWN =>
            MouseDown
              (Context     => Context,
               MouseButton => LeftButton,
               AbsX        => GET_X_LPARAM(lParam),
               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONUP =>
            MouseUp
              (Context     => Context,
               MouseButton => LeftButton,
               AbsX        => GET_X_LPARAM(lParam),
               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONDOWN =>
            MouseDown
              (Context     => Context,
               MouseButton => RightButton,
               AbsX        => GET_X_LPARAM(lParam),
               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONUP =>
            MouseUp
              (Context     => Context,
               MouseButton => RightButton,
               AbsX        => GET_X_LPARAM(lParam),
               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_MOUSEMOVE =>
            ContextMouseMove
              (Context     => Context_ClassAccess(Context),
               AbsX        => GET_X_LPARAM(lParam),
               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_DESTROY =>
            declare
               BoolResult : BOOL_Type;
               pragma Unreferenced(BoolResult);
            begin
               BoolResult:=PostMessage
                 (hWnd   => hWnd,
                  Msg    => WM_QUIT,
                  wParam => 0,
                  lParam => System.Null_Address);
            end;
            Context.DestroySignalSend:=True;
            return 0;

         when WM_TIMER =>
            Paint(Context.all);
            return 0;

         when WM_ERASEBKGND =>
            return 1;

         when WM_KEYDOWN =>
            if wParam<=255 then
               GUI.ContextKeyDown
                 (Context => Context_ClassAccess(Context),
                  Key     => KeyTable(Integer(wparam)));
            end if;
            return User32.DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_KEYUP =>
            if wParam<=255 then
               GUI.ContextKeyUp
                 (Context => Context_ClassAccess(Context),
                  Key     => KeyTable(Integer(wParam)));
            end if;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_CHAR =>
            declare
               -- Warnings are unnecessary, this is a dirty 32 to 16 bit
               -- convert.
               pragma Warnings(Off);
               function Convert is new Ada.Unchecked_Conversion
                 (Source => WPARAM_Type,
                  Target => Wide_Character);
               pragma Warnings(On);
            begin
               if wParam>=32 then
                  Put_Line("Win32WM_CHAR:"&WParam_Type'Image(wParam));
                  GUI.ContextCharacterInput
                    (Context => Context_ClassAccess(Context),
                     Chars   => UCS2ToUTF8(Convert(wParam)));
               end if;
            end;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when others =>
            return DefWindowProc
              (hWnd   => hWnd,
               uMsg   => uMsg,
               wParam => wParam,
               lParam => lParam);
      end case;
   end WndProc;
   ---------------------------------------------------------------------------

   -- This procedure extracts all waiting messages for each window/context
   -- and forwards them to the window proc responsible.
   -- It is implemented in a non blocking way using PeekMessage instead of
   -- GetMessage.
   procedure Process
     (Object : AnyObject_ClassAccess) is
      pragma Unreferenced(Object);

      lMsg        : aliased MSG_Type;
      Context     : Context_Access;
      NextContext : Context_Access;

   begin
      Context := Contexts;
      ContextLoop:
      while Context/=null loop

         NextContext:=Context.NextContext;
         ---------------------------------------------------------------------
         MessageLoop:
         while User32.PeekMessage
           (lpMsg         => lMsg'Access,
            hWnd          => Context.WindowHandle,
            wMsgFilterMin => 0,
            wMsgFilterMax => 0,
            wRemoveMsg    => PM_REMOVE)/=0 loop

            declare
               BoolResult : BOOL_Type;
               LResult    : LRESULT_Type;
               pragma Unreferenced(BoolResult);
               pragma Unreferenced(LResult);
            begin
               BoolResult := TranslateMessage(lMsg'Access);
               LResult    := DispatchMessage(lMsg'Access);
            end;

         end loop MessageLoop;
         ---------------------------------------------------------------------

         if Context.DestroySignalSend
           and Context.OnClose/=null then
            Context.OnClose(Context.CallBackObject);
         end if;

         Context:=NextContext;
      end loop ContextLoop;

   end Process;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Context : in out Context_Type) is

      BoolResult : BOOL_Type;
      pragma Unreferenced(BoolResult);
      IntResult  : Interfaces.C.int;
      pragma Unreferenced(IntResult);

   begin

      -- Deinitialize basic window environment
      if Context.ContextInitialized then
         GUI.Context_Type(Context).Finalize;
      end if;

      -- Remove Context from the contexts list
      if Context.LastContext/=null then
         Context.LastContext.NextContext:=Context.NextContext;
      else
         Contexts:=Context.NextContext;
         if Contexts=null then
            ProcessLoop.Remove(Process'Access,null);
         end if;

      end if;

      if Context.NextContext/=null then
         Context.NextContext.LastContext:=Context.LastContext;
      end if;

      -- Free Device Context
      if Context.DeviceContext/=NULLHANDLE then
         IntResult:=ReleaseDC
           (hWnd => Context.WindowHandle,
            hDC => Context.DeviceContext);
      end if;

      -- Destroy and free window
      if Context.WindowHandle/=NULLHANDLE then
         BoolResult:=DestroyWindow
           (hWnd => Context.WindowHandle);
      end if;

      -- Remove Window class
      BoolResult:=UnregisterClass
        (Context.CSTR_ClassName,
         GetModuleHandle(Interfaces.C.Strings.Null_Ptr));

      if Context.LibraryLoaded then
         pragma Assert(LibraryCount>=1);
         LibraryCount:=LibraryCount-1;
         if LibraryCount=0 then
            OpenGL.UnloadFunctions;
         end if;
      end if;

      -- Free C String memory
      Interfaces.C.Strings.Free(Context.CSTR_ClassName);
      Interfaces.C.Strings.Free(Context.CSTR_Title);

   end Finalize;
   ---------------------------------------------------------------------------

   function NewContext
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Context_ClassAccess is

      use type Interfaces.C.int;

      Context    : Context_Access;
      WndClass   : aliased WNDCLASS_Type;
      HInstance  : HINSTANCE_Type;

      dwStyle    : DWORD_Type;
      dwExStyle  : DWORD_Type;

      BoolResult : BOOL_Type;
      HWNDResult : HWND_Type;
      pragma Unreferenced(BoolResult);
      pragma Unreferenced(HWNDResult);

      Title : Unbounded_String:=U("Window");

   begin

      -- Create new context object and add it to the contexts list
      Context:=new Context_Type;
      Context.NextContext := Contexts;
      if Contexts/=null then
         Contexts.LastContext:=Context;
      else
         ProcessLoop.Add(Process'Access,null);
      end if;
      Contexts:=Context;

      Context.DoubleBuffered:=False;

      -- Obtain handle to current process
      HInstance:=GetModuleHandle
        (lpModuleName => Interfaces.C.Strings.Null_Ptr);

      declare
         Key : Unbounded_String:=Node&".Title";
      begin
         if Configuration.Contains(Key) then
            Title:=Configuration.Element(Key);
         end if;
      end;

      Context.CSTR_ClassName
        := Interfaces.C.Strings.New_String("OpenGLWindow");
      Context.CSTR_Title
        := Interfaces.C.Strings.New_String(To_String(Title));

      -- Create and Register a window class for an ordinary window
      WndClass.Style
        := CS_HREDRAW
        or CS_VREDRAW
        or CS_OWNDC;

      WndClass.lpfnWndProc := WndProc'Access;
      WndClass.hInstance   := HInstance;
      WndClass.lpszClassName := Context.CSTR_ClassName;
      WndClass.cbWndExtra    := System.Address'Size/8;
      WndClass.hIcon         := LoadIcon
        (hInstance  => NULLHANDLE,
         lpIconName => MAKEINTRESOURCE(IDI_APPLICATION));
      WndClass.hCursor       := LoadCursor
        (hInstance    => NULLHANDLE,
         lpCursorName => MAKEINTRESOURCE(IDC_ARROW));

      if RegisterClass
        (lpWndClass => WndClass'Access)=0 then

         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "RegisterClass failed with "
             &DWORD_Type'Image(GetLastError);

      end if;

      -- Create the window with the previously created window class
      dwStyle := WS_OVERLAPPEDWINDOW
        or WS_CLIPCHILDREN
        or WS_CLIPSIBLINGS;
      dwExStyle := WS_EX_APPWINDOW
        or WS_EX_CLIENTEDGE;

      Context.WindowHandle
        := CreateWindowEx
          (dwExStyle    => dwExStyle,
           lpClassName  => Context.CSTR_ClassName,
           lpWindowName => Context.CSTR_Title,
           dwStyle      => dwStyle,
           x            => 100,
           y            => 100,
           nwidth       => 1024,
           nheight      => 768,
           hWndParent   => NULLHANDLE,
           hMenu        => NULLHANDLE,
           hInstance    => HInstance,
           lpParam      => Context.all'Address);

      if Context.WindowHandle=NULLHANDLE then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to CreateWindowEx exited with "
             &DWORD_Type'Image(GetLastError);
      end if;

      -- Obtain a device context for the window
      Context.DeviceContext := GetDC(Context.WindowHandle);
      if Context.DeviceContext=NULLHANDLE then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to GetDC exited with "
             &DWORD_Type'Image(GetLastError);
      end if;

      -- Ensure window is visible and focused
      BoolResult:=ShowWindow
        (hWnd     => Context.WindowHandle,
         nCmdShow => SW_SHOW);

      BoolResult:=SetForegroundWindow
        (hWnd => Context.WindowHandle);

      HWNDResult:=SetFocus
        (hWnd => Context.WindowHandle);

      -- Setup OpenGL context
      declare
         pdf         : aliased PIXELFORMATDESCRIPTOR_Type;
         PixelFormat : Interfaces.C.int;
      begin
         pdf.nSize    := PIXELFORMATDESCRIPTOR_Type'Size/8;
         pdf.nVersion := 1;
         pdf.dwFlags
           := PFD_DRAW_TO_WINDOW
           or PFD_SUPPORT_OPENGL;
         pdf.iPixelType := PFD_TYPE_RGBA;
         pdf.cColorBits := 24;
         pdf.cDepthBits := 24;
         pdf.iLayerType := PFD_MAIN_PLANE;
         PixelFormat := GDI32.ChoosePixelFormat
           (hdc => Context.DeviceContext,
            ppfd => pdf'Access);
         if PixelFormat=0 then
            FreeContext(Context_ClassAccess(Context));
            raise FailedToCreateContext
              with "Failed to pick a Pixelformat using ChoosePixelFormat. Error code :"
                &Interfaces.C.int'Image(PixelFormat);
         end if;
         if GDI32.SetPixelFormat
           (hdc          => Context.DeviceContext,
            iPixelFormat => PixelFormat,
            ppfd         => pdf'Access)/=Standard.Win32.TRUE then
            FreeContext(Context_ClassAccess(Context));
            raise FailedToCreateContext
              with "Failed to set the Pixelformat using SetPixelFormat. Error code :"
                &DWORD_Type'Image(GetLastError);
         end if;
      end;

      Context.RenderContext
        :=wglCreateContext(Context.DeviceContext);

      if wglMakeCurrent
        (hdc   => Context.DeviceContext,
         hglrc => Context.RenderContext)/=Standard.Win32.TRUE then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to wglMakeCurrent with "
             &DWORD_Type'Image(GetLastError);
      end if;

      if LibraryCount=0 then
         LibraryHandle:=LoadLibrary(LibraryName(LibraryName'First)'Address);
         if LibraryHandle=NULLHANDLE then
            raise FailedToCreateContext with "Could not load library handle";
         end if;
      end if;

      Context.LibraryLoaded := True;
      LibraryCount  := LibraryCount+1;

      -- Initialize basic window environment
      GUI.Initialize
        (Context => Context_ClassAccess(Context));

      OpenGL.LoadFunctions(WGLGetProc'Access,True);

      Context.ContextInitialized:=True;

      return Context_ClassAccess(Context);
   end NewContext;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access);

   Identifier : constant Unbounded_String:=To_Unbounded_String("OpenGL");

   procedure Register is
   begin

      Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);

   end Register;
   ---------------------------------------------------------------------------

   procedure UnRegister is
   begin

      Implementations.UnRegister
        (Identifier => Identifier);

   end;
   ---------------------------------------------------------------------------

end OpenGL.Context.Win32;
