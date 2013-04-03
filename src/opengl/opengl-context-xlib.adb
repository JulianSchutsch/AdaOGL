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

with Xlib; use Xlib;
with OpenGL; use OpenGL;
with ProcessLoop;
with Interfaces.C.Strings;
with glX;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Basics; use Basics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;
with GUIMouse; use GUIMouse;
with GUIKeys; use GUIKeys;

package body OpenGL.Context.Xlib is

   ErrorStorage : XErrorEvent_Type;
   ErrorStorageSet : Boolean;

   function ErrorCodeString
     return String is

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.char,
         Target => Interfaces.Unsigned_8);
   begin
      if ErrorStorageSet then
         return " Serial:"&Interfaces.C.long'Image(ErrorStorage.serial)
           &" Error Code:"&Interfaces.Unsigned_8'Image(Convert(ErrorStorage.error_code))
           &" Request Code:"&Interfaces.Unsigned_8'Image(Convert(ErrorStorage.request_code))
           &" Minor Code:"&Interfaces.Unsigned_8'Image(Convert(ErrorStorage.minor_code));
      else
         return " No further information since the error handler was not called!";
      end if;
   end;
   ---------------------------------------------------------------------------

   function ErrorHandler
     (display : Display_Access;
      error   : XErrorEvent_Access)
      return Interfaces.C.int;
   pragma Convention(C,ErrorHandler);

   function ErrorHandler
     (display : Display_Access;
      error   : XErrorEvent_Access)
      return Interfaces.C.int is

      pragma Unreferenced(display);

   begin
      ErrorStorage    := error.all;
      ErrorStorageSet := True;
      Put("**********************ErrorHandler Called*********************");
      Put(ErrorCodeString);
      New_Line;
      return 0;
   end ErrorHandler;

   type CIntArray is array(Natural range <>) of aliased Interfaces.C.int;
   pragma Convention(C,CIntArray);

   XVisualAttribs : CIntArray:=
     (GLX.GLX_RGBA,
      GLX.GLX_RED_SIZE,8,
      GLX.GLX_GREEN_SIZE,8,
      GLX.GLX_BLUE_SIZE,8,
      GLX.GLX_DOUBLEBUFFER,
      GLX.GLX_DEPTH_SIZE,1,
      0);

   type Context_Type;
   type Context_Access is access all Context_Type;
   type Context_Type is new OpenGL.Context.Context_Type with
      record
         Display             : Display_Access        := null;
         NextContext         : Context_Access        := null;
         LastContext         : Context_Access        := null;
         DestroyedSignalSend : Boolean               := False;
         GLXMajor            : aliased GLint_Type    := 0;
         GLXMinor            : aliased GLint_Type    := 0;
         Screen              : Interfaces.C.int      := 0;
         Visual              : XVisualInfo_Access    := null;
         ColorMap            : ColorMap_Type         := 0;
         Window              : Window_Type           := 0;
         DeleteWindowAtom    : aliased Atom_Type     := 0;
         GLXContext          : glX.GLXContext_Access := null;
         InputIM             : XIM_Access            := null;
         InputContext        : XIC_Access            := null;
         DoubleBuffered      : Boolean               := True;
         ContextInitialized  : Boolean               := False;
         WMHints             : XWMHints_Access       := null;
         MapNotified         : Boolean               := False;
      end record;

   overriding
   procedure Finalize
     (Context : in out Context_Type);
   ---------------------------------------------------------------------------

   Contexts : Context_Access:=null;

   procedure ProcessContext
     (Context : Context_Access) is

      use type Interfaces.C.int;
      use type Interfaces.C.long;

      EventCount : Interfaces.C.int;

      Event : aliased XEvent_Type;

      procedure Paint is
      begin
         OpenGL.Context.Paint(OpenGL.Context.Context_Type(Context.all));
         if Context.DoubleBuffered then
            glX.glXSwapBuffers
              (dpy => Context.Display,
               drawable => glX.GLXDrawable_Type(Context.Window));
         else
            glFinish;
         end if;
      end Paint;

   begin
      if glX.glxMakeCurrent
        (dpy      => Context.Display,
         drawable => glX.GLXDrawable_Type(Context.Window),
         context  => Context.GLXContext)=0 then
         raise InvalidContext
           with "glxMakeCurrent failed";
      end if;

      EventCount:=XPending
        (display => Context.Display);

      while EventCount>0 loop
         EventCount := EventCount-1;

         -- WARNING : Unchecked access necessary since Xlib makes use
         --           of the passed structure as temporary buffer,
         --           which is local to this Context.
         XNextEvent
           (display => Context.Display,
            event_return => Event'Unchecked_Access);

         case Event.ttype is

            when ClientMessage =>

               Put("ClientMessage");
               Put(Integer(event.ClientMessage.l(0)));
               Put(Integer(Context.DeleteWindowAtom));
               New_Line;

               if Event.ClientMessage.l(0)
                 =Interfaces.C.long(Context.DeleteWindowAtom) then
                  Context.DestroyedSignalSend:=True;
                  return;
               end if;

            when Expose =>
               Put("Expose");
               New_Line;
               Paint;

            when ButtonPress =>
               -- FAULT : Please may some guru explain to me why i have an
               -- offset of two pixels in every window i create using xlib?
               case Event.ButtonPress.button is
                  when Button1 =>
                     ContextMouseDown
                       (Context     => Context_ClassAccess(Context),
                        MouseButton => LeftButton,
                        AbsX        => Integer(Event.ButtonPress.x),
                        AbsY        => Integer(Event.ButtonPress.y-2));
                  when Button2 =>
                     null;
                  when others =>
                     Put("Unknown Button pressed");
                     Put(Integer(Event.ButtonPress.button));
                     New_Line;
               end case;

            when ButtonRelease =>
               case Event.ButtonRelease.button is
                  when Button1 =>
                     ContextMouseUp
                       (Context     => Context_ClassAccess(Context),
                        MouseButton => LeftButton,
                        AbsX        => Integer(Event.ButtonPress.x),
                        AbsY        => Integer(Event.ButtonPress.y-2));
                  when Button2 =>
                     null;
                  when others =>
                     null;
               end case;

            when MotionNotify =>
               ContextMouseMove
                 (Context => Context_ClassAccess(Context),
                  AbsX    => Integer(Event.ButtonMotion.x),
                  AbsY    => Integer(Event.ButtonMotion.y-2));

            when KeyPress =>
               declare

                  BufferLength  : constant:=64;
                  NumberOfChars : Interfaces.C.int;
                  AdaBuffer     : String(1..BufferLength):=(others => ' ');
                  Buffer        : Interfaces.C.Strings.chars_ptr;
                  Status        : aliased Status_Type;
                  KeySym        : aliased KeySim_Type;

               begin

                  Buffer:=Interfaces.C.Strings.New_String(AdaBuffer);
                  NumberOfChars:=XUtf8LookupString
                    (ic            => Context.InputContext,
                     event         => Event'Access,
                     Buffer_Return => Buffer,
                     Bytes_Buffer  => BufferLength,
                     KeySym_Return => KeySym'Unchecked_Access,
                     Status_Return => Status'Unchecked_Access);
                  if (NumberOfChars/=0)
                    and ((Status=XLookupChars)
                         or (Status=XLookupBoth)) then
                     AdaBuffer(1..Integer(NumberOfChars)):=Interfaces.C.Strings.Value
                       (Item   => Buffer,
                        Length => Interfaces.C.size_t(NumberOfChars));
                     if NumberOfChars=1 then
                        case AdaBuffer(1) is
                           when Character'Val(8) =>
                              ContextKeyDown
                                (Context => Context_ClassAccess(Context),
                                 Key     => KeyBackspace);
                           when Character'Val(13) =>
                              ContextKeyDown
                                (Context => Context_ClassAccess(Context),
                                 Key     => KeyReturn);
                           when Character'Val(127) =>
                              ContextKeyDown
                                (Context => Context_ClassAccess(Context),
                                 Key     => KeyDelete);
                           when others =>
                              Put("Char");
                              Put(Character'Pos(AdaBuffer(1)));
                              New_Line;
                              ContextCharacterInput
                                (Context => Context_ClassAccess(Context),
                                 Chars   => U(AdaBuffer(1..Integer(NumberOfChars))));
                        end case;
                     else
                        ContextCharacterInput
                          (Context => Context_ClassAccess(Context),
                           Chars   => U(AdaBuffer(1..Integer(NumberOfChars))));
                     end if;

                  end if;

                  if (Status=XLookupKeySym) then
                     case KeySym is
                        when 65363 =>
                           ContextKeyDown
                             (Context => Context_ClassAccess(Context),
                              Key     => KeyRight);
                        when 65361 =>
                           ContextKeyDown
                             (Context => Context_ClassAccess(Context),
                              Key     => KeyLeft);
                        when 65362 =>
                           ContextKeyDown
                             (Context => Context_ClassAccess(Context),
                              Key     => KeyUp);
                        when 65364 =>
                           ContextKeyDown
                             (Context => Context_ClassAccess(Context),
                              Key     => KeyDown);
                        when others =>
                           null;
                     end case;
                  end if;

               end; -- Of Keyboard processing

            when KeyRelease =>
               null;

            when ConfigureNotify =>
               PropagateContextResize
                 (Context => Context_ClassAccess(Context),
                  Height  => Integer(Event.Configure.height),
                  Width   => Integer(Event.Configure.width));
               -- TODO: Send update signal

            when ResizeRequest =>

               Put("Resize");
               New_Line;
               Context.Bounds:=
                 (Top     => 0,
                  Left    => 0,
                  Height  => Integer(Event.ResizeRequest.height),
                  Width   => Integer(Event.ResizeRequest.width),
                  Visible => True);
               -- TODO: Send update signal

            when ReparentNotify =>
               Put("Reparent");
               New_Line;

            when MapNotify =>
               Context.MapNotified:=True;

            when others =>
               Put("Unknown Event Type:");
               Put(Integer(Event.ttype));
               New_Line;
         end case;

      end loop;

      Paint;

   end ProcessContext;
   ---------------------------------------------------------------------------

   procedure Process
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);

      Context     : Context_Access;
      NextContext : Context_Access;

   begin

      Context:=Contexts;
      ContextLoop:

      while Context/=null loop

         NextContext:=Context.NextContext;
         ProcessContext(Context);
         if Context.DestroyedSignalSend
           and Context.OnClose/=null then
            Context.OnClose(Context.CallBackObject);
         end if;

         Context:=NextContext;

      end loop ContextLoop;

   end Process;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Context : in out Context_Type) is

      use type glX.GLXContext_Access;
      use type Interfaces.C.int;

   begin
      -- Deinitialize basic window environment
      if Context.ContextInitialized then
         GUI.Context_Type(Context).Finalize;
      end if;

      if Context.InputContext/=null then
         XDestroyIC(Context.InputContext);
      end if;

      if Context.InputIM/=null then
         XCloseIM(Context.InputIM);
      end if;

      if Context.WMHints/=null then
         XFree
           (Context.WMHints.all'Address);
      end if;

      if Context.Visual/=null then
         XFree
           (Context.Visual.all'Address);
      end if;

      if Context.GLXContext/=null then

         if glX.glXMakeCurrent
           (dpy      => Context.Display,
            drawable => 0,
            context  => Null)=0 then
            raise FailedToDestroyContext
              with "Failed Call to glXMakeCurrent using drawable=0";
         end if;

         glX.glXDestroyContext
           (dpy => Context.Display,
            ctx => Context.GLXContext);

      end if;

      if Context.Window/=0 then
         XDestroyWindow
           (display => Context.Display,
            window  => Context.Window);
      end if;

      if Context.Display/=null then
         XCloseDisplay
           (display => Context.Display);
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

   end Finalize;
   ---------------------------------------------------------------------------

   function NewContext
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Context_ClassAccess is

      use type Interfaces.C.int;
      use type Interfaces.C.long;

      pragma Unreferenced(Configuration);
      pragma Unreferenced(Node);

      Context : Context_Access;

   begin
      EnableDebug;
      -- Create new context object and add it to the contexts list
      Context:=new Context_Type;
      context.NextContext:=Contexts;
      if Contexts/=null then
         Contexts.LastContext:=Context;
      else
         ProcessLoop.Add(Process'Access,null);
      end if;

      Contexts:=Context;

      -- Initalize X context
      Context.Display:=XOpenDisplay(Interfaces.C.Strings.Null_Ptr);
      if Context.Display=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to XOpenDisplay";
      end if;

      if XSetErrorHandler
        (func => ErrorHandler'Access)=0 then
         raise FailedToCreateContext
           with "Failed call to XSetErrorHandler";
      end if;

      if glX.glXQueryVersion
        (dpy   => Context.Display,
         major => Context.GLXMajor'Access,
         minor => Context.GLXMinor'Access)=0 then

         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to glXQueryVersion"
             &ErrorCodeString;

      end if;
      Put("GLX Version:");
      Put(Integer(Context.GLXMajor));
      Put(".");
      Put(Integer(Context.GLXMinor));
      New_Line;
      if not
        (
           ((Context.GLXMajor=1) and (Context.GLXMinor>=2))
         or (Context.GLXMajor>1)) then

         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "GLX version is too small. Found "
             &GLint_Type'Image(Context.GLXMajor)&"."
             &GLint_Type'Image(Context.GLXMinor)
             &", but needed 1.2";

      end if;

      Context.Screen:=DefaultScreen(Context.Display);

      Context.Visual:=GLX.glXChooseVisual
        (dpy        => Context.Display,
         screen     => Context.Screen,
         attribList => XVisualAttribs(0)'Access);
      if Context.Visual=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to glXChooseVisual"
             &ErrorCodeString;
      end if;

      Context.ColorMap := XCreateColormap
        (display => Context.Display,
         window  => RootWindow
           (display => Context.Display,
            screen  => Context.Visual.screen),
         visual  => Context.Visual.visual,
         alloc   => AllocNone);

      declare
         Attr : aliased XSetWindowAttributes_Type;
      begin
         Attr.colormap:=Context.ColorMap;
         Attr.border_pixel:=16#FFFFFFFF#;
         -- This sum of events should work, but OR would be the better
         -- choice
         Attr.event_mask:=
           StructureNotifyMask
           + ExposureMask
           + KeyPressMask
           + KeyReleaseMask
           + ButtonPressMask
           + ButtonReleaseMask
           + PointerMotionMask;

         Context.Window:=XCreateWindow
           (display => Context.Display,
            parent  => RootWindow
              (display => Context.Display,
               screen  => Context.Visual.screen),
            x       => 0,
            y       => 0,
            width   => 640,
            height  => 480,
            border_width => 0,
            depth   => Context.Visual.depth,
            class   => InputOutput,
            visual  => Context.Visual.visual,
            valuemask => CWBackPixel
                        +CWBorderPixel
                        +CWColorMap
                        +CWEventMask,
            attributes => Attr'Unchecked_Access);

      end;

      if Context.Window=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XCreateWindow returned no window"
             &ErrorCodeString;
      end if;

      declare
         SizeHints : aliased XSizeHints_Type;
      begin
         SizeHints.width  := 400;
         SizeHints.height := 400;
         SizeHints.flags  := USSize+USPosition;
         XSetNormalHints
           (display => Context.Display,
            window  => Context.Window,
            hints   => SizeHints'Unchecked_Access);
--         Xlib.XSetStandardProperties
--           (display => Context.Display,
--            window  => Context.Window,

      end;

      declare
         AtomName : Interfaces.C.Strings.chars_ptr
           :=Interfaces.C.Strings.New_String("WM_DELETE_WINDOW");
      begin
         Context.DeleteWindowAtom:=XInternAtom
           (display => Context.Display,
            atom_name => AtomName,
            only_if_exists => 1);
         Interfaces.C.Strings.Free(AtomName);
      end;
      if Context.DeleteWindowAtom=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XInternAtom with WM_DELETE_WINDOW failed"
             &ErrorCodeString;
      end if;

      if XSetWMProtocols
        (display   => Context.Display,
         window    => Context.Window,
         protocols => Context.DeleteWindowAtom'Access,
         count     => 1)=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XSetWMProtocols failed"
             &ErrorCodeString;
      end if;

      Context.GLXContext:=glX.glXCreateContext
        (dpy       => Context.Display,
         vis       => Context.Visual,
         shareList => null,
         direct    => 1);

      if Context=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XCreateContext failed"
             &ErrorCodeString;
      end if;

      XMapWindow
        (display => Context.Display,
         window  => Context.Window);

      if glX.glxMakeCurrent
        (dpy      => Context.Display,
         drawable => GLX.GLXDrawable_Type(Context.Window),
         context  => Context.GLXContext)=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to glxMakeCurrent failed"
             &ErrorCodeString;
      end if;

      Put("Waiting For MapNotify");
      New_Line;
      -- TODO: Build a timeout in here
      while not Context.MapNotified loop
         Process(null);
      end loop;
      Put("Done");
      New_Line;

      Context.InputIM:=XOpenIM
        (display   => Context.Display,
         db        => null,
         res_name  => Interfaces.C.Strings.Null_Ptr,
         res_class => Interfaces.C.Strings.Null_Ptr);

      if Context.InputIM=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed to create input IM with XOpenIM"
             &ErrorCodeString;
      end if;

      Put("*****************************************");
      New_Line;
      Put("Find out allowed IM Values:");
      New_Line;
      declare

         use type Interfaces.C.Strings.chars_ptr;

         ReturnString : Interfaces.C.Strings.chars_ptr;
         styles       : aliased XIMStyles_Access;
         cursor       : XIMStyle_Access;

      begin
         ReturnString:=XGetIMValues_1
           (xim                 => Context.InputIM,
            im_supported_styles => styles'Unchecked_Access);
         if Interfaces.C.Strings.Null_Ptr=ReturnString then
            Put("Null Return String");
            New_Line;
         else
            Put("Return String:");
            Put(Interfaces.C.Strings.Value(ReturnString));
            New_Line;
         end if;
         Put("Number of supported styles:");
         Put(Interfaces.C.short'Image(styles.count_styles));
         New_Line;
         cursor:=styles.supported_styles;
         for i in 1..styles.count_styles loop
            Put("Style ");
            Put(Integer(i));
            Put(":");
            Put(cursor.all'Address);
            Put(":");
            Put(XIMStyle_Type'Image(cursor.all));
            New_Line;
            cursor:=cursor+Interfaces.C.size_t(Interfaces.C.long'Size/8);
         end loop;

      end;
      Put("*****************************************");
      New_Line;

      Context.InputContext:=XCreateIC_1
        (im         => Context.InputIM,
         window     => Context.Window,
         inputstyle => XIMPreeditNothing+XIMStatusNothing);

      Put("Try to create Input Context");
      New_Line;
      if Context.InputContext=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed to create Input Context with XCreateIC"
             &ErrorCodeString;
      end if;

      -- Initialize basic window environment
      GUI.Initialize
        (Context => Context_ClassAccess(Context));

      Context.ContextInitialized:=True;

      return Context_ClassAccess(Context);
   end NewContext;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access);
   Identifier     : constant Unbounded_String:=To_Unbounded_String("OpenGL");

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

   end UnRegister;

end OpenGL.Context.Xlib;
