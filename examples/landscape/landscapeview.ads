with GUI; use GUI;
with GUI.Themes;
with OpenGL; use OpenGL;
with OpenGL.BufferTexture;
with OpenGL.Program;
with VectorMath; use VectorMath;
with GUIMouse; use GUIMouse;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Real_Time;

package LandscapeView is

   type TerrainCell_Type is
      record
         Flevel : GLFloat_Type;
         FDeriveX : GLFloat_Type;
         FDeriveY : GLFloat_Type;
         FDeriveXY : GLFloat_Type;
      end record;
   type Terrain_Array is array(Integer range <>) of TerrainCell_Type;
   type Terrain_Access is access all Terrain_Array;

   type MouseMode_Enum is
     (MouseModeUnknown, MouseModeMove, MouseModeSelect);

   type LandscapeView_Type is new GUI.Object_Type with
      record
         RotateX : GLFloat_Type:=0.0;
         RotateY : GLFLoat_Type:=0.0;
         RotateZ : GLFloat_Type:=0.0;
         Translate : GLFloat_Array(0..2);
         MouseDownTranslate : GLFloat_Array(0..2);
         Debug     : Boolean;
         TerrainVBO : aliased GLuint_Type;
         TerrainGeometryBuffer  : OpenGL.BufferTexture.BufferTexture_Type;
         TerrainSelectionBuffer : OpenGL.BufferTexture.BufferTexture_Type;
         ViewInformationBuffer  : OpenGL.BufferTexture.BufferTexture_Type;
         TerrainFShader         : aliased OpenGL.Program.Shader_Type;
         TerrainVShader         : aliased OpenGL.Program.Shader_Type;
         TerrainShader          : OpenGL.Program.Program_Type;
         TerrainShaderUniformTerrain           : GLInt_Type;
         TerrainShaderUniformSelect            : GLInt_Type;
         TerrainShaderUniformViewInformation   : GLInt_Type;
         TerrainShaderUniformPerspectiveMatrix : GLInt_Type;
         Terrain : Terrain_Access;
         WaterFShader                          : aliased OpenGL.Program.Shader_Type;
         WaterVShader                          : aliased OpenGL.Program.Shader_Type;
         WaterShader                           : OpenGL.Program.Program_Type;
         WaterShaderUniformSelect              : GLint_Type;
         WaterShaderUniformViewInformation     : GLint_Type;
         WaterShaderUniformPerspectiveMatrix   : GLint_Type;
         InverseModelMatrix : HomogenMatrix_Type;
         InvModelRotation   : HomogenMatrix_Type;
         ModelMatrix        : HomogenMatrix_Type;
         BoundMinX : Integer;
         BoundMaxX : Integer;
         BoundMinY : Integer;
         BoundMaxY : Integer;
         Selecting : Boolean;
         SelectX   : Integer;
         SelectY   : Integer;
         AspectRatio  : GLFloat_Type;
         NearDistance : GLFloat_Type;
         FarDistance  : GLFloat_Type;
         TerrainWidth : Integer:=1024;
         TerrainHeight : Integer := 1024;
         MouseMode  : MouseMode_Enum:=MouseModeUnknown;
         MouseDownX : Integer;
         MouseDownY : Integer;
         StartTime : Ada.Real_Time.Time;
      end record;
   type LandscapeView_Access is access all LandscapeView_Type;

   overriding
   function CharacterInput
     (View  : access LandscapeView_Type;
      Chars : Unbounded_String)
      return Boolean;

   overriding
   function MouseDown
     (View   : access LandscapeView_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure MouseMove
     (View : access LandscapeView_Type;
      X : Integer;
      Y : Integer);

   overriding
   procedure MouseUp
     (View   : access LandscapeView_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer);

   overriding
   procedure RenderCustom
     (View : access LandscapeView_Type);

   overriding
   procedure Resize
     (View : access LandscapeView_Type);
   ---------------------------------------------------------------------------

   function NewLandscapeView
     (Parent : GUI.Object_ClassAccess;
      Theme  : GUI.Themes.Implementation_Type)
      return LandscapeView_Access;

end LandscapeView;
