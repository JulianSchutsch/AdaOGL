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

pragma Ada_2005;

with Interfaces.C;
with Interfaces;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package OpenGL is

   subtype Extension_Type is Unbounded_String;
   type Extension_Array is array(Natural range <>) of Extension_Type;
   type Extension_ArrayAccess is access Extension_Array;

   Extensions : Extension_ArrayAccess;

   InvalidOpenGLVersion    : Exception;
   InvalidGLSLVersion      : Exception;
   InvalidExtensionString  : Exception;
   OpenGLLoadError         : Exception;
   OpenGLError             : Exception;
   InvalidTextureImageUnit : Exception;

   -- Portability : Maybe risky and non portable when using anything but
   --               the GNAT compiler
   subtype GLdouble_Type is Long_Float;
   subtype GLfloat_Type is Float;
   type GLfloat_Array is array (Integer range <>) of aliased GLfloat_Type;
   pragma Convention(C,GLfloat_Array);
   type GLfloat_Matrix4x4 is array(0..3,0..3) of aliased GLfloat_Type;
   pragma Convention(C,GLfloat_Matrix4x4);

   subtype GLclampf_Type is Float;
   type GLbitfield_Type is new Interfaces.C.unsigned;
   type GLint_Type is new Interfaces.C.int;
   type GLuint_Type is new Interfaces.C.unsigned;
   type GLuint_Access is access all GLuint_Type;
   pragma No_Strict_Aliasing(GLuint_Access);
   type GLint_Access is access all GLint_Type;
   pragma No_Strict_Aliasing(GLint_Access);
   type GLsizei_Type is new Interfaces.C.ptrdiff_t;
   type GLsizei_Access is access all GLsizei_Type;
   type GLintptr_Type is new Interfaces.C.ptrdiff_t;
   type GLsizeiptr_Type is new Interfaces.C.ptrdiff_t;
   type GLenum_Type is new Interfaces.Unsigned_32;
   subtype GLchar_Type is Interfaces.C.char;
   type GLchar_Access is access all GLchar_Type;
   type GLboolean_Type is new Interfaces.Unsigned_8;

   type CChar_Access is access all Interfaces.C.char;
   pragma Convention(C,CChar_Access);

   type OpenGLVersion_Type is
      record
         Major : aliased GLint_Type;
         Minor : aliased GLint_Type;
      end record;

   type GLSLVersion_Type is
      record
         Major : aliased GLint_Type:=0;
         Minor : aliased GLint_Type:=0;
      end record;

   GL_MODELVIEW  : constant GLenum_Type:=16#1700#;
   GL_PROJECTION : constant GLenum_Type:=16#1701#;

   GL_SCISSOR_TEST : constant GLenum_Type:=16#C11#;
   GL_DEPTH_TEST   : constant GLenum_Type:=16#B71#;
   GL_BLEND        : constant GLenum_Type:=16#BE2#;
   GL_TEXTURE_1D   : constant GLenum_Type:=16#DE0#;
   GL_TEXTURE_2D   : constant GLenum_Type:=16#DE1#;
   GL_TEXTURE_3D   : constant GLenum_Type:=16#806F#;
   GL_TEXTURE_CUBE_MAP : constant GLenum_Type:=16#8513#;

   GL_SRC_ALPHA           : constant GLenum_Type:=16#302#;
   GL_ONE_MINUS_SRC_ALPHA : constant GLenum_Type:=16#303#;
   GL_GREATER             : constant GLenum_Type:=16#204#;

   GL_COLOR_BUFFER_BIT : constant GLbitfield_Type:=16#4000#;
   GL_DEPTH_BUFFER_BIT : constant GLbitfield_Type:=16#100#;

   GL_RGBA            : constant:=16#1908#;
   GL_UNSIGNED_BYTE   : constant:=16#1401#;
   GL_BGRA            : constant:=16#80E1#;

   GL_QUADS : constant GLenum_Type:=7;
   GL_TRIANGLE_STRIP : constant GLenum_Type:=5;

   GL_TEXTURE_MIN_FILTER : constant GLenum_Type:=16#2801#;
   GL_TEXTURE_MAG_FILTER : constant GLenum_Type:=16#2800#;
   GL_TEXTURE_WRAP_S     : constant GLenum_Type:=16#2802#;
   GL_TEXTURE_WRAP_T     : constant GLenum_Type:=16#2803#;

   GL_NEAREST            : constant GLint_Type:=16#2600#;
   GL_CLAMP              : constant GLint_Type:=16#2900#;
   GL_REPEAT             : constant GLint_Type:=16#2901#;

   GL_VENDOR                   : constant GLenum_Type:=16#1F00#;
   GL_RENDERER                 : constant GLenum_Type:=16#1F01#;
   GL_VERSION                  : constant GLenum_Type:=16#1F02#;
   GL_EXTENSIONS               : constant GLenum_Type:=16#1F03#;
   GL_SHADING_LANGUAGE_VERSION : constant GLenum_Type:=16#8B8C#;

   GL_MAJOR_VERSION : constant GLenum_Type:=16#821B#;
   GL_MINOR_VERSION : constant GLenum_Type:=16#821C#;

   GL_FLOAT : constant GLenum_Type:=16#1406#;
   GL_FALSE : constant GLboolean_Type:=0;
   GL_TRIANGLES : constant GLenum_Type:=4;

   GL_NUM_EXTENSIONS : constant GLenum_Type:=16#821D#;

   GL_LINEAR : constant GLint_Type:=16#2601#;

   GL_TEXTURE_BUFFER          : constant GLenum_Type := 16#8C2A#;
   GL_MAX_TEXTURE_BUFFER_SIZE : constant GLenum_Type := 16#8C2B#;


   GL_RGBA8    : constant GLenum_Type:=16#8058#;
   GL_RGBA8I   : constant GLenum_Type:=16#8D8E#;
   GL_RGBA32F  : constant GLenum_Type:=16#8814#;
   GL_RGBA32UI : constant GLenum_Type:=16#8D70#;

   GL_DYNAMIC_DRAW : constant GLenum_Type:=16#88E8#;

   GL_MAP_WRITE_BIT : constant GLbitfield_Type:=2;

   GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS : constant GLenum_Type:=16#8B4D#;
   GL_ACTIVE_TEXTURE : constant GLenum_Type:=16#84E0#;

   GL_TEXTURE0 : constant GLenum_Type:=16#84C0#;

   -- TODO : REMOVE THIS SECTION

   GL_LIGHTING : constant GLenum_Type:=16#B50#;
   GL_LIGHT0   : constant GLenum_Type:=16#4000#;
   GL_COLOR_MATERIAL : constant GLenum_Type:=16#B57#;
   GL_POSITION : constant GLenum_Type:=16#1203#;

   procedure glLightfv
     (light  : GLenum_Type;
      pname  : GLenum_Type;
      params : access GLfloat_Type);
   pragma Import(StdCall,glLightfv,"glLightfv");

   procedure glMatrixMode
     (mode : GLenum_Type);
   pragma Import(StdCall,glMatrixMode,"glMatrixMode");

   procedure glBegin
     (mode : GLenum_Type);
   pragma Import(StdCall,glBegin,"glBegin");

   procedure glEnd;
   pragma Import(StdCall,glEnd,"glEnd");

   procedure glTexCoord2f
     (s : GLfloat_Type;
      t : GLfloat_Type);
   pragma Import(StdCall,glTexCoord2f,"glTexCoord2f");

   procedure glVertex2f
     (x : GLfloat_Type;
      y : GLfloat_Type);
   pragma Import(StdCall,glVertex2f,"glVertex2f");

   procedure glVertex3f
     (x : GLfloat_Type;
      y : GLfloat_Type;
      z : GLfloat_Type);
   pragma Import(StdCall,glVertex3f,"glVertex3f");

   procedure glNormal3f
     (x : GLfloat_Type;
      y : GLfloat_Type;
      z : GLfloat_Type);
   pragma Import(StdCall,glNormal3f,"glNormal3f");

   procedure glColor4f
     (red   : GLfloat_Type;
      green : GLfloat_Type;
      blue  : GLfloat_Type;
      alpha : GLfloat_Type);
   pragma Import(StdCall,glColor4f,"glColor4f");

   procedure glLoadIdentity;
   pragma Import(StdCall,glLoadIdentity,"glLoadIdentity");

   procedure glOrtho
     (left    : GLdouble_Type;
      right   : GLdouble_Type;
      bottom  : GLdouble_Type;
      top     : GLdouble_Type;
      nearVal : GLdouble_Type;
      farVal  : GLdouble_Type);
   pragma Import(StdCall,glOrtho,"glOrtho");

   procedure glFrustum
     (left   : GLDouble_Type;
      right  : GLDouble_Type;
      bottom : GLDouble_Type;
      top    : GLDouble_Type;
      zNear  : GLDouble_Type;
      zFar   : GLDouble_Type);
   pragma Import(StdCall,glFrustum,"glFrustum");

   procedure glEnable
     (cap : GLenum_Type);
   pragma Import(StdCall,glEnable,"glEnable");

   procedure glDisable
     (cap : GLenum_Type);
   pragma Import(StdCall,glDisable,"glDisable");

   procedure glBlendFunc
     (sfactor : GLenum_Type;
      dfactor : GLenum_Type);
   pragma Import(StdCall,glBlendFUnc,"glBlendFunc");

   procedure glAlphaFunc
     (func : GLenum_Type;
      ref  : GLclampf_Type);
   pragma Import(StdCall,glAlphaFunc,"glAlphaFunc");

   procedure glTranslatef
     (x : GLFloat_Type;
      y : GLFloat_Type;
      z : GlFloat_Type);
   pragma Import(StdCall,glTranslatef,"glTranslatef");

   procedure glRotatef
     (Alpha : GLFloat_Type;
      x     : GLFloat_Type;
      y     : GLFloat_Type;
      z     : GLFloat_Type);
   pragma Import(StdCall,glRotatef,"glRotatef");
   -- ///////

   -- GetProc_Access expects null terminated strings and returns
   -- a pointer to a function/procedure of the OpenGL interface
   type GetProc_Access is
     access function
       (Name : String) return System.Address;

   type glGetString_Access is
     access function
       (name : GLenum_Type)
        return chars_ptr;
   pragma Convention(StdCall,glGetString_Access);

   type glGetStringi_Access is
     access function
       (name : GLenum_Type;
        index : GLint_Type)
        return chars_ptr;
   pragma Convention(StdCall,glGetstringi_Access);

   function glGetString
     (name    : GLenum_Type;
      GetProc : not null GetProc_Access)
      return String;

   type glGetIntegerv_Access is
     access procedure
       (pname  : GLenum_Type;
        params : access GLint_Type);
   pragma Convention(StdCall,glGetIntegerv_Access);

   procedure PreGetIntegerv
     (pname : GLenum_Type;
      params : access GLint_Type;
      GetProc : not null GetProc_Access);

   type glClearColor_Access is
     access procedure
       (red   : GLclampf_Type;
        green : GLclampf_Type;
        blue  : GLclampf_Type;
        alpha : GLclampf_Type);
   pragma Convention(StdCall,glClearColor_Access);

   type glClear_Access is
     access procedure
       (mask : GLbitfield_Type);
   pragma Convention(StdCall,glClear_Access);

   type glViewport_Access is
     access procedure
       (x      : GLint_Type;
        y      : GLint_Type;
        width  : GLsizei_Type;
        height : GLsizei_Type);
   pragma Convention(StdCall,glViewport_Access);

   type glDrawArrays_Access is
     access procedure
       (mode  : GLenum_Type;
        first : GLint_Type;
        count : GLsizei_Type);
   pragma Convention(StdCall,glDrawArrays_Access);

   type glDrawArraysInstanced_Access is
     access procedure
       (mode      : GLenum_Type;
        first     : GLint_Type;
        count     : GLint_Type;
        primcount : GLint_Type);
   pragma Convention(StdCall,glDrawArraysInstanced_Access);

   type glFinish_Access is
     access procedure;
   pragma Convention(StdCall,glFinish_Access);

   type glGetError_Access is
     access function
     return GLenum_Type;
   pragma Convention(StdCall,glGetError_Access);

   type glGenTextures_Access is
     access procedure
       (n        : GLsizei_Type;
        textures : access GLuint_Type);
   pragma Convention(StdCall,glGenTextures_Access);

   type glBindTexture_Access is
     access procedure
       (target  : GLenum_Type;
        texture : GLuint_Type);
   pragma Convention(StdCall,glBindTexture_Access);

   type glTexParameteri_Access is
     access procedure
       (target : GLenum_Type;
        pname  : GLenum_Type;
        param  : GLint_Type);
   pragma Convention(StdCall,glTexParameteri_Access);

   type glDeleteTextures_Access is
     access procedure
       (n        : GLsizei_Type;
        textures : access GLuint_Type);
   pragma Convention(StdCall,glDeleteTextures_Access);

   type glTexImage2D_Access is
     access procedure
       (target         : GLenum_Type;
        level          : GLint_Type;
        internalFormat : GLint_Type;
        width          : GLsizei_Type;
        height         : GLsizei_Type;
        border         : GLint_Type;
        format         : GLenum_Type;
        ttype          : GLenum_Type;
        data           : System.Address);
   pragma Convention(StdCall,glTexImage2D_Access);

   type glTexSubImage2D_Access is
     access procedure
       (target  : GLenum_Type;
        level   : GLint_Type;
        xoffset : GLint_Type;
        yoffset : GLint_Type;
        width   : GLsizei_Type;
        height  : GLsizei_Type;
        format  : GLenum_Type;
        ttype   : GLenum_Type;
        data    : System.Address);
   pragma Convention(StdCall,glTexSubImage2D_Access);

   type glActiveTexture_Access is
     access procedure
       (texture : GLenum_Type);

   glClearColor          : glClearColor_Access:=null;
   glClear               : glClear_Access:=null;
   glViewport            : glViewport_Access:=null;
   glDrawArrays          : glDrawArrays_Access:=null;
   glFinish              : glFinish_Access:=null;
   glGetIntegerv         : glGetIntegerv_Access:=null;
   glGetError            : glGetError_Access:=null;
   glGenTextures         : glGenTextures_Access:=null;
   glBindTexture         : glBindTexture_Access:=null;
   glTexParameteri       : glTexParameteri_Access:=null;
   glDeleteTextures      : glDeleteTextures_Access:=null;
   glTexImage2D          : glTexImage2D_Access:=null;
   glTexSubImage2D       : glTexSubImage2D_Access:=null;
   glActiveTexture       : glActiveTexture_Access:=null;
   glDrawArraysInstanced : glDrawArraysInstanced_Access:=null;
   ---------------------------------------------------------------------------

   -- Buffer Objects

   type glGenBuffers_Access is
     access procedure
       (n       : GLsizei_Type;
        buffers : access GLuint_Type);
   pragma Convention(StdCall,glGenBuffers_Access);

   type glDeleteBuffers_Access is
     access procedure
       (n       : GLsizei_Type;
        buffers : access GLuint_Type);
   pragma Convention(StdCall,glDeleteBuffers_Access);

   type glBindBuffer_Access is
     access procedure
       (target : GLenum_Type;
        buffer : GLuint_Type);
   pragma Convention(StdCall,glBindBuffer_Access);

   type glBufferData_Access is
     access procedure
       (target : GLenum_Type;
        size   : GLsizeiptr_Type;
        data   : System.Address;
        usage  : GLenum_Type);
   pragma Convention(StdCall,glBufferData_Access);

   type glTexBuffer_Access is
     access procedure
       (target         : GLenum_Type;
        internalFormat : GLenum_Type;
        buffer         : GLuint_Type);
   pragma Convention(StdCall,glTexBuffer_Access);

   type glMapBufferRange_Access is
     access function
       (target  : GLenum_Type;
        offset  : GLintptr_Type;
        length  : GLsizeiptr_Type;
        aaccess : GLbitfield_Type)
        return System.Address;
   pragma Convention(StdCall,glMapBufferRange_Access);

   type glUnmapBuffer_Access is
     access function
       (target : GLenum_Type)
        return GLboolean_Type;
   pragma Convention(StdCall,glUnmapBuffer_Access);

   GL_ARRAY_BUFFER : GLenum_Type:=16#8892#;
   GL_STATIC_DRAW  : GLenum_Type:=16#88E4#;

   glGenBuffers     : glGenBuffers_Access:=null;
   glDeleteBuffers  : glDeleteBuffers_Access:=null;
   glBindBuffer     : glBindBuffer_Access:=null;
   glBufferData     : glBufferData_Access:=null;
   glTexBuffer      : glTexBuffer_Access:=null;
   glMapBufferRange : glMapBufferRange_Access:=null;
   glUnmapBuffer    : glUnmapBuffer_Access:=null;

   SupportBufferObjects : Boolean:=False;

   -- Vertex Attributes

   type glBindAttribLocation_Access is
     access procedure
       (program : GLuint_Type;
        index   : GLuint_Type;
        name    : access GLchar_Type);
   pragma Convention(StdCall,glBindAttribLocation_Access	);

   type glVertexAttribPointer_Access is
     access procedure
       (index      : GLuint_Type;
        size       : GLint_Type;
        ttype      : GLenum_Type;
        normalized : GLboolean_Type;
        stride     : GLsizei_Type;
        pointer    : System.Address);
   pragma Convention(StdCall,glVertexAttribPointer_Access);

   type glEnableVertexAttribArray_Access is
     access procedure
       (index : GLuint_Type);
   pragma Convention(StdCall,glEnableVertexAttribArray_Access);

   type glGenVertexArrays_Access is
     access procedure
       (n      : GLsizei_Type;
        arrays : access GLuint_Type);
   pragma Convention(StdCall,glGenVertexArrays_Access);

   type glBindVertexArray_Access is
     access procedure
       (arr : GLuint_Type);
   pragma Convention(StdCall,glBindVertexArray_Access);

   glVertexAttribPointer     : glVertexAttribPointer_Access:=null;
   glEnableVertexAttribArray : glEnableVertexAttribArray_Access:=null;
   glBindAttribLocation      : glBindAttribLocation_Access:=null;
   glGenVertexArrays         : glGenVertexArrays_Access:=null;
   glBindVertexArray         : glBindVertexArray_Access:=null;
   glGetStringi              : glGetStringi_Access:=null;

   SupportVertexAttributes : Boolean:=False;

   -- GLSL --

   type glCreateProgram_Access is
     access function
     return GLuint_Type;
   pragma Convention(StdCall,glCreateProgram_Access);

   type glDeleteProgram_Access is
     access procedure
       (program : GLuint_Type);
   pragma Convention(StdCall,glDeleteProgram_Access);

   type glUseProgram_Access is
     access procedure
       (program : GLuint_Type);
   pragma Convention(StdCall,glUseProgram_Access);

   type glAttachShader_Access is
     access procedure
       (program : GLuint_Type;
        shader  : GLuint_Type);
   pragma Convention(StdCall,glAttachShader_Access);

   type glDetachShader_Access is
     access procedure
       (program : GLuint_Type;
        shader  : GLuint_Type);
   pragma Convention(StdCall,glDetachShader_Access);

   type glLinkProgram_Access is
     access procedure
       (program : GLuint_Type);
   pragma Convention(StdCall,glLinkProgram_Access);

   type glGetProgramInfoLog_Access is
     access procedure
       (program   : GLuint_Type;
        maxLength : GLsizei_Type;
        length    : access GLsizei_Type;
        infoLog   : access GLchar_Type);
   pragma Convention(StdCall,glGetProgramInfoLog_Access);

   type glGetProgramiv_Access is
     access procedure
       (program : GLuint_Type;
        pname   : GLenum_Type;
        params  : access GLint_Type);
   pragma Convention(StdCall,glGetProgramiv_Access);

   type glGetShaderInfoLog_Access is
     access procedure
       (shader    : GLuint_Type;
        maxLength : GLsizei_Type;
        length    : access GLsizei_Type;
        infoLog   : access GLchar_Type);
   pragma Convention(StdCall,glGetShaderInfoLog_Access);

   type glGetUniformLocation_Access is
     access function
       (program : GLuint_Type;
        name    : access GLchar_Type) -- const
        return GLint_Type;
   pragma Convention(StdCall,glGetUniformLocation_Access);

   type glCreateShader_Access is
     access function
       (shaderType : GLenum_Type)
        return GLuint_Type;
   pragma Convention(StdCall,glCreateShader_Access);

   type glDeleteShader_Access is
     access procedure
       (shader : GLuint_Type);
   pragma Convention(StdCall,glDeleteShader_Access);

   type glShaderSource_Access is
     access procedure
       (shader  : GLuint_Type;
        count   : GLsizei_Type;
        strings : access CChar_Access;
        lengths : access GLint_Type);
   pragma Convention(StdCall,glShaderSource_Access);

   type glCompileShader_Access is
     access procedure
       (shader : GLuint_Type);
   pragma Convention(StdCall,glCompileShader_Access);

   type glGetShaderiv_Access is
     access procedure
       (shader : GLuint_Type;
        pname  : GLenum_Type;
        params : access GLint_Type);
   pragma Convention(StdCall,glGetShaderiv_Access);

   type glUniform1i_Access is
     access procedure
       (location : GLint_Type;
        v0       : GLint_Type);
   pragma Convention(StdCall,glUniform1i_Access);

   type glUniformMatrix4fv_Access is
     access procedure
       (location  : GLint_Type;
        count     : GLsizei_Type;
        transpose : GLboolean_Type;
        value     : access GLfloat_Type);
   pragma Convention(StdCall,glUniformMatrix4fv_Access);

   glCreateProgram      : glCreateProgram_Access      := null;
   glDeleteProgram      : glDeleteProgram_Access      := null;
   glUseProgram         : glUseProgram_Access         := null;
   glAttachShader       : glAttachShader_Access       := null;
   glDetachShader       : glDetachShader_Access       := null;
   glLinkProgram        : glLinkProgram_Access        := null;
   glGetProgramiv       : glGetProgramiv_Access       := null;
   glGetShaderInfoLog   : glGetShaderInfoLog_Access   := null;
   glGetUniformLocation : glGetUniformLocation_Access := null;
   glGetProgramInfoLog  : glGetProgramInfoLog_Access  := null;

   glCreateShader       : glCreateShader_Access  := null;
   glDeleteShader       : glDeleteShader_Access  := null;
   glShaderSource       : glShaderSource_Access  := null;
   glCompileShader      : glCompileShader_Access := null;
   glGetShaderiv        : glGetShaderiv_Access   := null;

   glUniform1i : glUniform1i_Access:=null;
   glUniformMatrix4fv : glUniformMatrix4fv_Access:=null;

   SupportProgram  : Boolean := False;

   GL_FRAGMENT_SHADER : constant GLenum_Type:=16#8B30#;
   GL_VERTEX_SHADER   : constant GLenum_Type:=16#8B31#;

   ---------------------------------------------------------------------------

   function GetVersion
     (GetProc : not null GetProc_Access)
      return OpenGLVersion_Type;

   -- These functions are like the usual opengl calls, but the
   -- function pointer is loaded on demand.
   -- This is necessary for initialisation

   procedure LoadFunctions
     (GetProc    : not null GetProc_Access;
      Compatible : Boolean);

   procedure UnloadFunctions;

   function IsExtensionSupported
     (Name : String)
      return Boolean;

   -- This function is published since some OGL implementations expect
   -- a query of the extensions prior to the existence of any context.
   procedure ProcessExtensionString
     (ExtStr : String);

   procedure AssertError
     (Extra : String);

   Version     : OpenGLVersion_Type;
   GLSLVersion : GLSLVersion_Type;

   function GetMaxCombinedTextureImageUnits
     return Natural;

   procedure BindTexture
     (Target  : GLenum_Type;
      Unit    : Natural;
      Texture : GLuint_Type);

   procedure BindTextureBuffer
     (Buffer : GLuint_Type);

   procedure DeleteBuffer
     (Buffer : GLuint_Type);

   procedure DeleteTexture
     (Texture : GLuint_Type);

   function OrthoMatrix
     (Left   : GLfloat_Type;
      Right  : GLfloat_Type;
      Bottom : GLfloat_Type;
      Top    : GLfloat_Type;
      Near   : GLfloat_Type:=-1.0;
      Far    : Glfloat_Type:=1.0)
      return GLFloat_Matrix4x4;

   function IdentityMatrix
     return GLFloat_Matrix4x4;

end OpenGL;
