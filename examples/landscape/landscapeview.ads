with GUI;
with GUI.Themes;
with OpenGL; use OpenGL;
with OpenGL.BufferTexture;
with OpenGL.Program;
with VectorMath; use VectorMath;

package LandscapeView is

   type LandscapeView_Type is new GUI.Object_Type with
      record
         RotateX : GLFloat_Type:=0.0;
         RotateY : GLFLoat_Type:=0.0;
         RotateZ : GLFloat_Type:=0.0;
         Translate : GLFloat_Array(0..2);
         Debug     : Boolean;
         TerrainVBO : aliased GLuint_Type;
         TerrainGeometryBuffer  : OpenGL.BufferTexture.BufferTexture_Type;
         TerrainSelectionBuffer : OpenGL.BufferTexture.BufferTexture_Type;
         ViewInformationBuffer  : OpenGL.BufferTexture.BufferTexture_Type;
         TerrainFShader         : aliased OpenGL.Program.Shader_Type;
         TerrainVShader         : aliased OpenGL.Program.Shader_Type;
         TerrainShader          : OpenGL.Program.Program_Type;
         TerrainShaderUniformTerrain : GLInt_Type;
         TerrainShaderUniformSelect  : GLInt_Type;
         TerrainShaderUniformViewInformation   : GLInt_Type;
         TerrainShaderUniformPerspectiveMatrix : GLInt_Type;
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
      end record;
   type LandscapeView_Access is access all LandscapeView_Type;

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
