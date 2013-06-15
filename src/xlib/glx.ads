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

with Xlib; use Xlib;
with OpenGL; use OpenGL;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Ada.Unchecked_Conversion;

package glX is

   FailedglXLoading : Exception;

   GLX_RGBA         : constant:=4;
   GLX_RED_SIZE     : constant:=8;
   GLX_GREEN_SIZE   : constant:=9;
   GLX_BLUE_SIZE    : constant:=10;
   GLX_DOUBLEBUFFER : constant:=5;
   GLX_DEPTH_SIZE   : constant:=12;
   GLX_NONE         : constant:=101;
   GLX_DRAWABLE_TYPE : constant:=16#8010#;
   GLX_WINDOW_BIT    : constant:=1;
   GLX_RENDER_TYPE   : constant:=16#8011#;
   GLX_RGBA_BIT      : constant:=1;
   GLX_X_RENDERABLE  : constant:=16#8012#;
   GLX_X_VISUAL_TYPE : constant:=22;
   GLX_TRUE_COLOR    : constant:=16#8002#;
   GLX_STENCIL_SIZE  : constant:=13;

   GLX_CONTEXT_MAJOR_VERSION_ARB : constant:=16#2091#;
   GLX_CONTEXT_MINOR_VERSION_ARB : constant:=16#2092#;
   GLX_CONTEXT_FLAGS_ARB         : constant:=16#2094#;
   GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB : constant:=2;

   type GLXContext_Type is null record;
   type GLXContext_Access is access all GLXContext_Type;

   type GLXDrawable_Type is new Xlib.XID_Type;

   type GLXFBConfigRec_Type is null record;
   type GLXFBConfigRec_Access is access all GLXFBConfigRec_Type;
   type GLXFBConfig_Type is array(0..Natural'Last) of GLXFBConfigRec_Access;
   pragma Convention(C,GLXFBConfig_Type);
   type GLXFBConfig_Access is access all GLXFBConfig_Type;

   -- TODO PORTABILITY : Boolean may not be defined as expected, working
   --               on Debian Sqeeze so far

   type glXGetProcAddressARB_Access is
     access function
       (procName : access Interfaces.C.char)
        return System.Address;
   pragma Convention(C,glXGetProcAddressARB_Access);

   glXGetProcAddressARB : glXGetProcAddressARB_Access:=null;

   function GetProcAddressARB
     (Name : String)
      return System.Address;

   function GetProcAddress
     (Name : String)
      return System.Address;

   type glXCreateContextAttribsARB_Access is
     access function
       (dpy           : Display_Access;
        config        : GLXFBConfigRec_Access;
        share_context : GLXContext_Access;
        direct        : Interfaces.C.int;
        attrib_list   : access Interfaces.C.int)
        return GLXContext_Access;
   pragma Convention(C,glXCreateContextAttribsARB_Access);

   function Conv is new Ada.Unchecked_Conversion(System.Address,glXCreateContextAttribsARB_Access);

   function glXQueryVersion
     (dpy   : Display_Access;
      major : GLint_Access;
      minor : GLint_Access)
      return Interfaces.C.int; -- FOR BOOLEAN
   pragma Import(C,glXQueryVersion,"glXQueryVersion");

   function glXQueryExtensionsString
     (dpy    : Display_Access;
      screen : Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import(C,glXQueryExtensionsString,"glXQueryExtensionsString");

   function QueryExtensionsString
     (Display : Display_Access;
      Screen  : Interfaces.C.int)
      return String;

   function glXChooseVisual
     (dpy        : Display_Access;
      screen     : Interfaces.C.int;
      attribList : access Interfaces.C.int)
      return XVisualInfo_Access;
   pragma Import(C,glXChooseVisual,"glXChooseVisual");

   function glXCreateContext
     (dpy       : Display_Access;
      vis       : XVisualInfo_Access;
      shareList : GLXContext_Access;
      direct    : Interfaces.C.int)
      return GLXContext_Access;
   pragma Import(C,glXCreateContext,"glXCreateContext");

   function glXMakeCurrent
     (dpy      : Display_Access;
      drawable : GLXDrawable_Type;
      context  : GLXContext_Access)
      return Interfaces.C.int; -- FOR BOOLEAN
   pragma Import(C,glXMakeCurrent,"glXMakeCurrent");

   procedure glXDestroyContext
     (dpy : Display_Access;
      ctx : GLXContext_Access);
   pragma Import(C,glXDestroyContext,"glXDestroyContext");

   procedure glXSwapBuffers
     (dpy      : Display_Access;
      drawable : GLXDrawable_Type);
   pragma Import(C,glXSwapBuffers,"glXSwapBuffers");

   function glXGetProcAddress
     (procName : access Interfaces.C.char)
      return System.Address;
   pragma Import(C,glXGetProcAddress,"glXGetProcAddressARB");

   function glXChooseFBConfig
     (dpy         : Display_Access;
      screen      : Interfaces.C.int;
      attrib_list : access Interfaces.C.int;
      nelements   : access Interfaces.C.int)
      return GLXFBConfig_Access;
   pragma Import(C,glXChooseFBConfig,"glXChooseFBConfig");

   function glXGetVisualFromFBConfig
     (dpy : Display_Access;
      config : GLXFBConfigRec_Access)
      return XVisualInfo_Access;
   pragma Import(C,glXGetVisualFromFBConfig,"glXGetVisualFromFBConfig");

   function glXQueryExtension
     (dpy : Display_Access;
      errorBase : access Interfaces.C.int;
      eventBase : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,glXQueryExtension,"glXQueryExtension");

   function glXQueryExtensionString
     (dpy    : Display_Access;
      screen : Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import(C,glXQueryExtensionString,"glXQueryExtensionString");

   procedure LoadGLX
     (Display : Display_Access);

   VersionMajor : aliased GLint_Type:=0;
   VersionMinor : aliased GLint_Type:=0;

end glX;
