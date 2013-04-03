-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
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
--   18.Mar 2012 Julian Schutsch
--     - Original version

with Bytes; use Bytes;

package Win32.OpenGL32 is

   WGL_CONTEXT_MAJOR_VERSION_ARB : constant Int_Type:=16#2091#;
   WGL_CONTEXT_MINOR_VERSION_ARB : constant Int_Type:=16#2092#;
   WGL_CONTEXT_LAYER_PLANE_ARB   : constant Int_Type:=16#2093#;
   WGL_CONTEXT_FLAGS_ARB         : constant Int_Type:=16#2094#;
   WGL_CONTEXT_PROFILE_MASK_ARB  : constant Int_Type:=16#9126#;

   WGL_CONTEXT_DEBUG_BIT_ARB               : constant Int_Type := 1;
   WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB : constant Int_Type := 2;

   WGL_CONTEXT_CORE_PROFILE_BIT_ARB          : constant Int_Type := 1;
   WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB : constant Int_Type := 2;

   ERROR_INVALID_VERSION_ARB : constant := 16#2095#;
   ERROR_INVALID_PROFILE_ARB : constant := 16#2096#;

   function wglGetProcAddress
     (Str : access Interfaces.C.char)
      return System.Address;
   pragma Import(StdCall,wglGetProcAddress,"wglGetProcAddress");

   function wglCreateContext
     (hdc : HDC_Type)
      return HGLRC_Type;
   pragma Import(StdCall,wglCreateContext,"wglCreateContext");

   function wglMakeCurrent
     (hdc : HDC_Type;
      hglrc : HGLRC_Type)
      return BOOL_Type;
   pragma Import(StdCall,wglMakeCurrent,"wglMakeCurrent");

   function wglDeleteContext
     (hglrc : HGLRC_Type)
      return BOOL_Type;
   pragma Import(StdCall,wglDeleteContext,"wglDeleteContext");

   function SwapBuffers
     (hdc : HDC_Type)
      return BOOL_Type;
   pragma Import(StdCall,SwapBuffers,"SwapBuffers");

   function wglCreateContextAttribsARB
     (hdc           : HDC_Type;
      hShareContext : HGLRC_Type;
      attriblist    : Int_Access)
      return HGLRC_Type;

end Win32.OpenGL32;
