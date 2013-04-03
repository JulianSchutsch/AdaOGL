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

-- Revision History
--   24.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;
with Interfaces.C.Strings;
with System;

package Xlib is

   type XID_Type is new Interfaces.C.long;
   type XID_Access is access all XID_Type;

   type KeySim_Type is new XID_Type;
   type KeySim_Access is access all KeySim_Type;

   type Window_Type is new XID_Type;
   type Window_Access is access all Window_Type;

   type Display_Type is null record;
   type Display_Access is access Display_Type;

   type Visual_Type is null record;
   type Visual_Access is access all Visual_Type;

   type VisualID_Type is new Interfaces.C.long;
   type VisualID_Access is access all VisualID_Type;

   type ColorMap_Type is new XID_Type;
   type ColorMap_Access is access all ColorMap_Type;

   type Pixmap_Type is new XID_Type;
   type Pixmap_Access is access all Pixmap_Type;

   type Cursor_Type is new XID_Type;
   type Cursor_Access is access all Cursor_Type;

   type Atom_Type is new Interfaces.C.long;
   type Atom_Access is access all Atom_Type;

   type XIC_Type is null record;
   type XIC_Access is access all XIC_Type;

   type XIM_Type is null record;
   type XIM_Access is access all XIM_Type;

   type Status_Type is new Interfaces.C.int;
   type Status_Access is access all Status_Type;

   type XrmDatabase_Type is null record;
   type XrmDatabase_Access is access all XrmDatabase_Type;

   type Time_Type is new Interfaces.C.long;

   type XWMHints_Type is
      record
         flags         : Interfaces.C.long := 0;
         input         : Interfaces.Unsigned_32 := 0;
         initial_state : Interfaces.C.int  := 0;
         icon_pixmap   : Pixmap_Type       := 0;
         icon_window   : Window_Type       := 0;
         icon_x        : Interfaces.C.int  := 0;
         icon_y        : Interfaces.C.int  := 0;
         icon_mask     : Pixmap_Type       := 0;
         window_group  : XID_Type          := 0;
      end record;
   pragma Convention(C,XWMHints_Type);
   type XWMHints_Access is access all XWMHints_Type;

   type XVisualInfo_Type is
      record
         visual        : Visual_Access:=null;
         visualid      : VisualID_Type:=0;
         screen        : Interfaces.C.int:=0;
         depth         : Interfaces.C.int:=0;
         class         : Interfaces.C.int:=0;
         red_mask      : Interfaces.C.long:=0;
         green_mask    : Interfaces.C.long:=0;
         blue_mask     : Interfaces.C.long:=0;
         colormap_size : Interfaces.C.int:=0;
         bits_per_rgb  : Interfaces.C.int:=0;
      end record;
   pragma Convention(C,XVisualInfo_Type);
   type XVisualInfo_Access is access all XVisualInfo_Type;

   StructureNotifyMask : constant := 2**17;
   ExposureMask        : constant := 2**15;
   KeyPressMask        : constant := 2**0;
   KeyReleaseMask      : constant := 2**1;
   ButtonPressMask     : constant := 2**2;
   ButtonReleaseMask   : constant := 2**3;
   PointerMotionMask   : constant := 2**6;

   type XSetWindowAttributes_Type is
      record
         background_pixmap    : Pixmap_Type       := 0;
         background_pixel     : Interfaces.C.long := 0;
         border_pixmap        : Pixmap_Type       := 0;
         border_pixel         : Interfaces.C.unsigned_long := 0;
         bit_gravity          : Interfaces.C.int  := 0;
         win_gravity          : Interfaces.C.int  := 0;
         backing_store        : Interfaces.C.int  := 0;
         backing_planes       : Interfaces.C.long := 0;
         backing_pixel        : Interfaces.C.long := 0;
         save_under           : Interfaces.C.int  := 0;
         event_mask           : Interfaces.C.long := 0;
         do_not_propagate_ask : Interfaces.C.long := 0;
         override_redirect    : Interfaces.C.int  := 0;
         colormap             : Colormap_Type     := 0;
         cursor               : Cursor_Type       := 0;
      end record;
   pragma Convention(C,XSetWindowAttributes_Type);
   type XSetWindowAttributes_Access is access all XSetWindowAttributes_Type;

   type Aspect_Type is
      record
         x : Interfaces.C.int:=0;
         y : Interfaces.C.int:=0;
      end record;
   pragma Convention(C,Aspect_Type);

   USSize     : constant:=2**1;
   USPosition : constant:=2**0;

   type XSizeHints_Type is
      record
         flags       : Interfaces.C.long:=0;
         x           : Interfaces.C.int:=0;
         y           : Interfaces.C.int:=0;
         width       : Interfaces.C.int:=0;
         height      : Interfaces.C.int:=0;
         min_width   : Interfaces.C.int:=0;
         min_height  : Interfaces.C.int:=0;
         max_width   : Interfaces.C.int:=0;
         max_height  : Interfaces.C.int:=0;
         width_inc   : Interfaces.C.int:=0;
         height_inc  : Interfaces.C.int:=0;
         min_aspect  : Aspect_Type;
         max_ascpect : Aspect_Type;
         base_width  : Interfaces.C.int:=0;
         base_height : Interfaces.C.int:=0;
         win_gravity : Interfaces.C.int:=0;
      end record;
   pragma Convention(C,XSizeHints_Type);
   type XSizeHints_Access is access all XSizeHints_Type;

   type LongArray is array(Natural range <>) of Interfaces.C.long;
   pragma Convention(C,LongArray);

   type XClientMessageEvent_Type is
      record
         Message_Type : Atom_Type;
         format       : Interfaces.C.int;
         l            : LongArray(0..4);
      end record;
   pragma Convention(C,XClientMessageEvent_Type);

   type XExposeEvent_Type is
      record
         x          : Interfaces.C.int;
         y          : Interfaces.C.int;
         width      : Interfaces.C.int;
         height     : Interfaces.C.int;
         count      : Interfaces.C.int;
      end record;
   pragma Convention(C,XExposeEvent_Type);

   type XButtonEvent_Type is
      record
         root        : Window_Type;
         subwindow   : Window_Type;
         time        : Time_Type;
         x           : Interfaces.C.int;
         y           : Interfaces.C.int;
         x_root      : Interfaces.C.int;
         y_root      : Interfaces.C.int;
         state       : Interfaces.C.unsigned;
         button      : Interfaces.C.unsigned;
         same_screen : Interfaces.Unsigned_32;
      end record;
   pragma Convention(C,XButtonEvent_Type);

   type XMotionEvent_Type is
      record
         root        : Window_Type;
         subwindow   : Window_Type;
         time        : Time_Type;
         x           : Interfaces.C.int;
         y           : Interfaces.C.int;
         x_root      : Interfaces.C.int;
         y_root      : Interfaces.C.int;
         state       : Interfaces.C.unsigned;
         is_hint     : Interfaces.C.char;
         same_screen : Interfaces.Unsigned_32;
      end record;
   pragma Convention(C,XMotionEvent_Type);

   type XKeyEvent_Type is
      record
         root        : Window_Type;
         subwindow   : Window_Type;
         time        : Time_Type;
         x           : Interfaces.C.int;
         y           : Interfaces.C.int;
         x_root      : Interfaces.C.int;
         y_root      : Interfaces.C.int;
         state       : Interfaces.C.unsigned;
         keycode     : Interfaces.C.unsigned;
         same_screen : Interfaces.C.int;
      end record;
   pragma Convention(C,XKeyEvent_Type);

   type XConfigureEvent_Type is
      record
         -- event is now window in XEvent_Type
         window            : Window_Type;
         x                 : Interfaces.C.int;
         y                 : Interfaces.C.int;
         width             : Interfaces.C.int;
         height            : Interfaces.C.int;
         border_width      : Interfaces.C.int;
         above             : Window_Type;
         override_redirect : Interfaces.C.int;
      end record;
   pragma Convention(C,XConfigureEvent_Type);

   type XReparentEvent_Type is
      record
         window : Window_Type;
         x      : Interfaces.C.int;
         y      : Interfaces.C.int;
      end record;
   pragma Convention(C,XReparentEvent_Type);

   type XResizeRequestEvent_Type is
      record
         width  : Interfaces.C.int;
         height : Interfaces.C.int;
      end record;
   pragma Convention(C,XResizeRequestEvent_Type);

   type EventType_Enum is
     (EventTypeUnknown,
      EventTypeClientMessage,
      EventTypeExpose,
      EventTypeButtonPress,
      EventTypeButtonRelease,
      EventTypeMotionNotify,
      EventTypeKeyPress,
      EventTypeKeyRelease,
      EventTypeConfigureNotify,
      EventTypeResizeRequest,
      EventTypeReparentNotify);

   MapNotify       : constant:=19;
   ClientMessage   : constant:=33;
   Expose          : constant:=12;
   ButtonPress     : constant:=4;
   ButtonRelease   : constant:=5;
   MotionNotify    : constant:=6;
   KeyPress        : constant:=2;
   KeyRelease      : constant:=3;
   ConfigureNotify : constant:=22;
   ResizeRequest   : constant:=25;
   ReparentNotify  : constant:=21;

   Button1 : constant:=1;
   Button2 : constant:=2;

   XLookupChars  : constant Status_Type:=2;
   XLookupBoth   : constant Status_Type:=4;
   XLookupKeySym : constant Status_Type:=3;

   type XEvent_Type(EventType : EventType_Enum:=EventTypeUnknown) is
      record
         ttype      : Interfaces.C.int;
         serial     : Interfaces.C.long;
         send_event : Boolean;
         display    : Display_Access;
         window     : Window_Type;
         case EventType is
            when EventTypeUnknown =>
               data  : String(1..1020);
            when EventTypeClientMessage =>
               ClientMessage : XClientMessageEvent_Type;
            when EventTypeExpose =>
               Expose : XExposeEvent_Type;
            when EventTypeButtonPress =>
               ButtonPress : XButtonEvent_Type;
            when EventTypeButtonRelease =>
               ButtonRelease : XButtonEvent_Type;
            when EventTypeMotionNotify =>
               ButtonMotion : XMotionEvent_Type;
            when EventTypeKeyPress =>
               KeyPress : XKeyEvent_Type;
            when EventTypeKeyRelease =>
               KeyRelease : XKeyEvent_Type;
            when EventTypeConfigureNotify =>
               Configure : XConfigureEvent_Type;
            when EventTypeResizerequest =>
               ResizeRequest : XResizeRequestEvent_Type;
            when EventTypeReparentNotify =>
               Reparent : XReparentEvent_Type;
         end case;
      end record;
   pragma Unchecked_Union(XEvent_Type);
   pragma Convention(C,XEvent_Type);
   type XEvent_Access is access all XEvent_Type;

   type XErrorEvent_Type is
      record
         ttype        : Interfaces.C.int;
         display      : Display_Access;
         serial       : Interfaces.C.long;
         error_code   : Interfaces.C.char;
         request_code : Interfaces.C.char;
         minor_code   : Interfaces.C.char;
      end record;
   pragma Convention(C,XErrorEvent_Type);
   type XErrorEvent_Access is access XErrorevent_Type;

   type XIMStyle_Type is new Interfaces.C.long;
   pragma Convention(C,XIMStyle_Type);
   type XIMStyle_Access is access XIMStyle_Type;

   type XIMStyles_Type is
      record
         count_styles     : Interfaces.C.short;
         supported_styles : XIMStyle_Access;
      end record;
   pragma Convention(C,XIMStyles_Type);

   type XIMStyles_Access is access all XIMStyles_Type;

   type ErrorFunction_Access is
     access function
       (display : Display_Access;
        ErrorEvent : XErrorEvent_Access)
        return Interfaces.C.int;
   pragma Convention(C,ErrorFunction_Access);

   function XSetErrorHandler
     (func : ErrorFunction_Access)
      return Interfaces.C.int;
   pragma Import(C,XSetErrorHandler,"XSetErrorHandler");

   function XOpenDisplay
     (display_name : Interfaces.C.Strings.chars_ptr)
      return Display_Access;
   pragma Import(C,XOpenDisplay,"XOpenDisplay");

   function DefaultScreen
     (pdf : Display_Access)
      return Interfaces.C.int;
   pragma Import(C,DefaultScreen,"_DefaultScreen");

   function RootWindow
     (display : Display_Access;
      screen  : Interfaces.C.int)
      return Window_Type;
   pragma Import(C,RootWindow,"_RootWindow");

   InputOutput   : constant := 1;
   CWBackPixel   : constant := 2**1;
   CWBorderPixel : constant := 2**3;
   CWColorMap    : constant := 2**13;
   CWEventMask   : constant := 2**11;

   function XCreateWindow
     (display      : Display_Access;
      parent       : Window_Type;
      x            : Interfaces.C.int;
      y            : Interfaces.C.int;
      width        : Interfaces.C.unsigned;
      height       : Interfaces.C.unsigned;
      border_width : Interfaces.C.int;
      depth        : Interfaces.C.int;
      class        : Interfaces.C.unsigned;
      visual       : Visual_Access;
      valuemask    : Interfaces.C.long;
      attributes   : XSetWindowAttributes_Access)
      return Window_Type;
   pragma Import(C,XCreateWindow,"XCreateWindow");

   AllocNone : constant:=0;

   function XCreateColormap
     (display : Display_Access;
      window  : Window_Type;
      visual  : Visual_Access;
      alloc   : Interfaces.C.int)
      return ColorMap_Type;
   pragma Import(C,XCreateColorMap,"XCreateColormap");

   procedure XSetNormalHints
     (display : Display_Access;
      window  : Window_Type;
      hints   : XSizeHints_Access);
   pragma Import(C,XSetNormalHints,"XSetNormalHints");

   procedure XSetStandardProperties
     (display     : Display_Access;
      window      : Window_Type;
      window_name : Interfaces.C.Strings.chars_ptr;
      icon_name   : Interfaces.C.Strings.chars_ptr;
      icon_pixmap : Pixmap_Type;
      argv        : access Interfaces.C.Strings.chars_ptr;
      argc        : Interfaces.C.int;
      hints       : XSizeHints_Access);
   pragma Import(C,XSetStandardProperties,"XSetStandardProperties");

   function XInternAtom
     (display        : Display_Access;
      atom_name      : Interfaces.C.Strings.chars_ptr;
      only_if_exists : Interfaces.C.int)
      return Atom_Type;
   pragma Import(C,XInternAtom,"XInternAtom");

   procedure XMapWindow
     (display : Display_Access;
      window  : Window_Type);
   pragma Import(C,XMapWindow,"XMapWindow");

   function XSetWMProtocols
     (display   : Display_Access;
      window    : Window_Type;
      protocols : Atom_Access;
      count     : Integer)
      return Status_Type;
   pragma Import(C,XSetWMProtocols,"XSetWMProtocols");

   function XOpenIM
     (display   : Display_Access;
      db        : XrmDatabase_Access;
      res_name  : Interfaces.C.Strings.chars_ptr;
      res_class : Interfaces.C.Strings.chars_ptr)
      return XIM_Access;
   pragma Import(C,XOpenIM,"XOpenIM");

   XIMPreeditNothing : constant := 8;
   XIMStatusNothing  : constant := 16#400#;

   function XCreateIC_1
     (im : XIM_Access;
      window : Window_Type;
      inputstyle : Interfaces.C.int)
      return XIC_Access;
   pragma Import(C,XCreateIC_1,"_XCreateIC_1");

   function XPending
     (display : Display_Access)
      return Interfaces.C.int;
   pragma Import(C,XPending,"XPending");

   procedure XNextEvent
     (display : Display_Access;
      event_return : XEvent_Access);
   pragma Import(C,XNextEvent,"XNextEvent");

   procedure XDestroyWindow
     (display : Display_Access;
      window  : Window_Type);
   pragma Import(C,XDestroyWindow,"XDestroyWindow");

   procedure XCloseDisplay
     (display : Display_Access);
   pragma Import(C,XCloseDisplay,"XCloseDisplay");

   procedure XFree
     (data : System.Address);
   pragma Import(C,XFree,"XFree");

   function XGetIMValues_1
     (xim                 : XIM_Access;
      im_supported_styles : access XIMStyles_Access)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import(C,XGetIMValues_1,"XGetIMValues_1");

   function XUtf8LookupString
     (ic            : XIC_Access;
      event         : access XEvent_Type;
      buffer_return : Interfaces.C.Strings.chars_ptr;
      bytes_buffer  : Interfaces.C.int;
      keysym_return : KeySim_Access;
      status_return : Status_Access)
      return Interfaces.C.int;
   pragma Import(C,XUtf8LookupString,"Xutf8LookupString");

   procedure XSetWMHints
     (display : Display_Access;
      window  : Window_Type;
      wmhints : XWMHints_Access);
   pragma Import(C,XSetWMHints,"XSetWMHints");

   function XAllocWMHints
     return XWMHints_Access;
   pragma Import(C,XAllocWMHints,"XAllocWMHints");

   procedure XDestroyIC
     (xic : XIC_Access);
   pragma Import(C,XDestroyIC,"XDestroyIC");

   procedure XCloseIM
     (xim : XIM_Access);
   pragma Import(C,XCloseIM,"XCloseIM");

   procedure EnableDebug;
   pragma Import(C,EnableDebug,"EnableDebug");

   function "+" (Left : XIMStyle_Access; Right : Interfaces.C.size_t)
                 return XIMStyle_Access;

end XLib;
