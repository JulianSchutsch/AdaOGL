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

package glX is

   GLX_RGBA         : constant:=4;
   GLX_RED_SIZE     : constant:=8;
   GLX_GREEN_SIZE   : constant:=9;
   GLX_BLUE_SIZE    : constant:=10;
   GLX_DOUBLEBUFFER : constant:=5;
   GLX_DEPTH_SIZE   : constant:=12;
   GLX_NONE         : constant:=101;

   type GLXContext_Type is
      record
         null;
      end record;
   type GLXContext_Access is access GLXContext_Type;

   type GLXDrawable_Type is new Xlib.XID_Type;

   -- PORTABILITY : Boolean may not be defined as expected, working
   --               on Debian Sqeeze so far

   function glXQueryVersion
     (dpy   : Display_Access;
      major : GLint_Access;
      minor : GLint_Access)
      return Interfaces.C.int; -- FOR BOOLEAN
   pragma Import(C,glXQueryVersion,"glXQueryVersion");

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
     (dpy : Display_Access;
      drawable : GLXDrawable_Type;
      context : GLXContext_Access)
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

end glX;
