pragma Ada_2012;

package OpenGL.Program is

   FailedToCreateShader : Exception;
   FailedToCreateProgram : Exception;

   type Shader_Enum is
     (ShaderVertex,
      ShaderFragment);

   type Shader_Type is private;
   type Shader_Access is access all Shader_Type;

   type Shader_Set is array(Shader_Enum) of Shader_Access;

   procedure Create
     (Shader     : in out Shader_Type;
      ShaderType : Shader_Enum;
      Source     : String);

   procedure Reset
     (Shader : in out Shader_Type);

   function GetCompileLog
     (Shader : Shader_Type)
      return Unbounded_String;

   procedure Destroy
     (Shader : in out Shader_Type);
   ---------------------------------------------------------------------------

   type Program_Type is private;

   procedure Create
     (Program : in out Program_Type;
      Shaders : Shader_Set);

   procedure Reset
     (Program : in out Program_Type);

   procedure UseProgram
     (Program : in out Program_Type);

   function GetLinkLog
     (Program : Program_Type)
      return Unbounded_String;

   procedure BindAttribLocation
     (Program : Program_Type;
      Index   : GLuint_Type;
      Name    : String);

   function GetUniformLocation
     (Program : Program_Type;
      Name    : String)
      return GLint_Type;

   procedure Destroy
     (Program : in out Program_Type);

private

   type Shader_Type is
      record
         ID         : GLuint_Type:=0; -- 0 means no shader created
         ShaderType : Shader_Enum;
         CompileLog : Unbounded_String;
      end record;

   type Program_Type is
      record
         ID      : GLuint_Type:=0;
         Shaders : Shader_Set;
         LinkLog : Unbounded_String;
      end record;

end OpenGL.Program;
