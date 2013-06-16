with Basics; use Basics;
with GUI.Button;
with BoundsCalc; use BoundsCalc;
with GUIDefinitions;
with VectorMath;
with OpenGL.BufferTexture;
with OpenGL.Program;
with Ada.Numerics.Elementary_Functions;
with Bytes; use Bytes;
with Types; use Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

package body LandscapeView is

   LineFeed : constant Character := Character'Val(10);

   TerrainShader : String :=
       "#version 330"&LineFeed&
       "#extension GL_EXT_gpu_shader4 : enable"&LineFeed&
       "uniform mat4 PerspectiveMatrix;"&LineFeed&
       "uniform samplerBuffer terrain;"&LineFeed&
       "uniform samplerBuffer select;"&LineFeed&
       "uniform samplerBuffer ViewInformation;"&LineFeed&
       "smooth out vec4 vertexColor;"&LineFeed&
       "layout (location = 0) in vec4 inVertex;"&LineFeed&
       "void main()"&LineFeed&
       "{"&LineFeed&
       " int TerrainWidth = int(texelFetchBuffer(ViewInformation,0).x);"&LineFeed&
       " int ViewTop      = int(texelFetchBuffer(ViewInformation,1).x);"&LineFeed&
       " int ViewLeft     = int(texelFetchBuffer(ViewInformation,2).x);"&LineFeed&
       " int ViewWidth    = int(texelFetchBuffer(ViewInformation,3).x);"&LineFeed&
       " vec4 Position = inVertex;"&LineFeed&
       " int deltaY=gl_InstanceID/ViewWidth;"&LineFeed&
       " int deltaX=gl_InstanceID-deltaY*ViewWidth;"&LineFeed&
       " int texelPos=(ViewTop+deltaY)*TerrainWidth+deltaX+ViewLeft;"&LineFeed&
       " vec4 Data1=texelFetchBuffer(terrain,texelPos);"&LineFeed&
       " vec4 Data2=texelFetchBuffer(terrain,texelPos+TerrainWidth);"&LineFeed&
       " vec4 Data3=texelFetchBuffer(terrain,texelPos+1);"&LineFeed&
       " vec4 Data4=texelFetchBuffer(terrain,texelPos+TerrainWidth+1);"&LineFeed&
       " mat4 C=mat4(Data1.r,Data3.r,Data1.g,Data3.g,"&LineFeed&
       "             Data2.r,Data4.r,Data2.g,Data4.g,"&LineFeed&
       "             Data1.b,Data3.b,Data1.a,Data3.a,"&LineFeed&
       "             Data2.b,Data4.b,Data2.a,Data4.a);"&LineFeed&
       " const mat4 M1=mat4( 1.0 ,  0.0 ,  0.0 ,  0.0 ,"&LineFeed&
       "               0.0 ,  0.0 ,  1.0 ,  0.0 ,"&LineFeed&
       "              -3.0 ,  3.0 , -2.0 , -1.0 ,"&LineFeed&
       "               2.0 , -2.0 ,  1.0 ,  1.0 );"&LineFeed&
       " vec4 x=M1*vec4(1.0,inVertex.x,inVertex.x*inVertex.x,inVertex.x*inVertex.x*inVertex.x);"&LineFeed&
       " vec4 y=M1*vec4(1.0,inVertex.y,inVertex.y*inVertex.y,inVertex.y*inVertex.y*inVertex.y);"&LineFeed&
       " vec4 dx=M1*vec4(0.0,1.0,2.0*inVertex.x,3.0*inVertex.x*inVertex.x);"&LineFeed&
       " vec4 dy=M1*vec4(0.0,1.0,2.0*inVertex.y,3.0*inVertex.y*inVertex.y);"&LineFeed&
       " float deltaH=dot(x,C*y);"&LineFeed&
       " float dfx=dot(dx,C*y);"&LineFeed&
       " float dfy=dot(x,C*dy);"&LineFeed&
       " vec3 n=normalize(vec3(-dfx,-dfy,1.0));"&LineFeed&
       " vec3 lightvec=normalize(vec3(1.0,1.0,-1.0));"&LineFeed&
       " float addi=-dot(lightvec,n);"&LineFeed&
       " float intensity=max(0.0,addi);"&LineFeed&
       " Position += vec4(float(ViewLeft+deltaX),float(ViewTop+deltaY),deltaH,0);"&LineFeed&
       " gl_Position = PerspectiveMatrix*Position;"&LineFeed&
       " vertexColor = vec4(fract(float(ViewLeft+deltaX)/2.0),intensity*0.5+0.2,fract(float(ViewTop+deltaY)/2.0),1);"&LineFeed&
       "}"&LineFeed;

   FragmentShader : String :=
       "#version 330"&LineFeed&
       "smooth in vec4 vertexColor;"&LineFeed&
       "out vec4 outputColor;"&LineFeed&
       "void main()"&LineFeed&
       "{"&LineFeed&
       " outputColor=vertexColor;"&LineFeed&
       "}"&LineFeed;

   BicubicTransformationMatrixT : FourDMatrix_Type:=
     ((1.0,0.0,0.0,0.0),(0.0,0.0,1.0,0.0),(-3.0,3.0,-2.0,-1.0),(2.0,-2.0,1.0,1.0));
   BicubicTransformationMatrix : FourDMatrix_Type:=
     ((1.0,0.0,-3.0,2.0),(0.0,0.0,3.0,-2.0),(0.0,1.0,-2.0,1.0),(0.0,0.0,-1.0,1.0));

   procedure CalcPerspective
     (View : access LandscapeView_Type) is
      DirTopLeft     : HomogenVector_Type;
      DirTopRight    : HomogenVector_Type;
      DirBottomLeft  : HomogenVector_Type;
      DirBottomRight : HomogenVector_Type;
      PosSpot        : HomogenVector_Type;
      PosTopLeft     : HomogenVector_Type;
      PosTopRight    : HomogenVector_Type;
      PosBottomLeft  : HomogenVector_Type;
      PosBottomRight : HomogenVector_Type;
      BoundingBox    : BoundingBox_Type;
      InvLength      : GLFloat_Type;

      Bounds : constant BoundsCalc.Bounds_Type:=View.GetBounds;
   begin

      if Bounds.Width=0 then
         return;
      end if;
      View.NearDistance := 1.0;
      View.FarDistance  := 200.0;
      View.AspectRatio := GLFloat_Type(Bounds.Width)/GLFLoat_Type(Bounds.Height);
      Put_Line("AspectRatio"&GLFloat_Type'Image(View.AspectRatio));

      View.InvModelRotation := RotateZHomogenMAtrix(-View.RotateZ)*RotateXHomogenMatrix(View.RotateX);
      DebugHomogenMatrix(View.InvModelRotation);

      View.InverseModelMatrix := TranslateHomogenMatrix(-View.Translate(0), -View.Translate(1), -View.Translate(2))
        * RotateZHomogenMatrix(-View.RotateZ)
        * RotateXHomogenMatrix(-View.RotateX);

      View.ModelMatrix := RotateXHomogenMatrix(View.RotateX)
        * RotateZHomogenMatrix(View.RotateZ)
        * TranslateHomogenMatrix(View.Translate(0),View.Translate(1),View.Translate(2));

      -- TODO: This length should be influenced by AspectRatio!
      InvLength := 1.0/Ada.Numerics.Elementary_Functions.Sqrt(2.0+View.NearDistance**2);
      Put_Line("InvLength"&GLFLoat_Type'Image(InvLength));

      PosSpot := View.InverseModelMatrix * AssignHomogenVector(0.0,0.0,0.0);
      DebugHomogenVector(PosSpot);

      DirTopLeft := View.InvModelRotation*(InvLength*AssignHomogenVector(-View.AspectRatio,1.0,-View.NearDistance));
      DirTopRight := View.InvModelRotation*(InvLength*AssignHomogenVector(View.AspectRatio,1.0,-View.NearDistance));
      DirBottomLeft := View.InvModelRotation*(InvLength*AssignHomogenVector(-View.AspectRatio,-1.0,-View.NearDistance));
      DirBottomRight := View.InvModelRotation*(InvLength*AssignHomogenVector(View.AspectRatio,-1.0,-View.NearDistance));
      DebugHomogenVector(DirTopLeft);

      PosTopLeft := AssignHomogenVector(PosSpot(0)-DirTopLeft(0)*PosSpot(2)/DirTopLeft(2), PosSpot(1)-DirTopLeft(1)*PosSpot(2)/DirTopLeft(2),0.0);
      PosTopRight := AssignHomogenVector(PosSpot(0)-DirTopRight(0)*PosSpot(2)/DirTopRight(2), PosSpot(1)-DirTopRight(1)*PosSpot(2)/DirTopRight(2),0.0);
      PosBottomLeft := AssignHomogenVector(PosSpot(0)-DirBottomLeft(0)*PosSpot(2)/DirBottomLeft(2),PosSpot(1)-DirBottomLeft(1)*PosSpot(2)/DirBottomLeft(2),0.0);
      PosBottomRight := AssignHomogenVector(PosSpot(0)-DirBottomRight(0)*PosSpot(2)/DirBottomRight(2),PosSpot(1)-DirBottomRight(1)*PosSpot(2)/DirBottomRight(2),0.0);

      CalculateMinimalBoundingBox((PosSpot, PosTopLeft, PosTopRight, PosBottomLeft, PosBottomRight),BoundingBox);

      View.BoundMinX := Integer(GLFloat_Type'Max(GLFloat_Type'Floor(BoundingBox.MinX) , 1.0));
      View.BoundMaxX := Integer(GLFloat_Type'Min(GLFloat_Type'Ceiling(BoundingBox.MaxX), GLFloat_Type(View.TerrainWidth)-2.0));
      View.BoundMinY := Integer(GLFloat_Type'Max(GLFloat_Type'Floor(BoundingBox.MinY), 1.0));
      View.BoundMaxY := Integer(GLFloat_Type'Min(GLFloat_Type'Ceiling(BoundingBox.MaxY), GLFloat_Type(View.TerrainHeight)-2.0));

      Put_Line("MIN:"&Integer'Image(View.BoundMinX));
      Put_Line("MAX:"&Integer'Image(View.BoundMaxX));
      Put_Line("MIN:"&Integer'Image(View.BoundMinY));
      Put_Line("MAX:"&Integer'Image(View.BoundMaxY));

   end CalcPerspective;
   ---------------------------------------------------------------------------

   procedure UpdateViewCaptions
     (View : access LandscapeView_Type) is
      type Vis_Type is delta 0.1 range -10000.0..+10000.0;
   begin
      null;
   end UpdateViewCaptions;
   ---------------------------------------------------------------------------

   procedure Resize
     (View : access LandscapeView_Type) is
   begin
      CalcPerspective(View);
   end Resize;
   ---------------------------------------------------------------------------

   procedure RenderCustom
     (View : access LandscapeView_Type) is

      AbsBounds   : AbsBounds_Type renames View.AbsBounds;
      Bounds      : Bounds_Type := View.GetBounds;
      AspectRatio : GLDouble_Type;
      InfoBuffer : Cardinal32_Access;

   begin
      AssertError("RenderCustom.Start");
      if not AbsBounds.AbsVisible then
         return;
      end if;

      InfoBuffer:=AddressToCardinal32Access(OpenGL.BufferTexture.MapWriteOnly(View.ViewInformationBuffer));
      InfoBuffer.all:=Cardinal32(View.TerrainWidth);
      InfoBuffer.all:=Cardinal32(View.BoundMinY);
      InfoBuffer.all:=Cardinal32(View.BoundMinX);
      InfoBuffer.all:=Cardinal32(View.BoundMaxX-View.BoundMinX+1);
      OpenGL.BufferTexture.Unmap(View.ViewInformationBuffer);

      glViewPort(GLint_Type(AbsBounds.AbsLeft - AbsBounds.AbsSubLeft),
                 GLint_Type(View.GetContextArea.AbsBounds.AbsHeight - (AbsBounds.AbsTop - AbsBounds.AbsSubTop) - Bounds.Height),
                 GLsizei_Type(Bounds.Width),
                 GLsizei_Type(Bounds.Height));
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      AssertError("RenderCustom.AspectCalc");

      if Bounds.Height/=0 then
         AspectRatio:=GLDouble_Type(Bounds.Width)/GLDouble_Type(Bounds.Height);
      else
         AspectRatio:=1.0;
      end if;

      glFrustum(-AspectRatio, AspectRatio, -1.0, 1.0, GLdouble_Type(View.NearDistance), GLdouble_Type(View.FarDistance*3.0));
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glClear(GL_DEPTH_BUFFER_BIT);
      glEnable(GL_DEPTH_TEST);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      declare
         Position : GLfloat_Array(0..3):=(-1.0,0.0,0.0,1.0);
      begin
         glLightfv(GL_LIGHT0,GL_POSITION,Position(0)'Access);
      end;

      glRotatef(View.RotateX, 1.0, 0.0, 0.0);
      glRotatef(View.RotateZ, 0.0, 0.0, 1.0);
      glTranslatef(View.Translate(0), View.Translate(1), View.Translate(2));
      glBegin(GL_QUADS);
      glColor4f(0.0,0.0,1.0,1.0);
      glVertex3f(GLFLoat_Type(View.BoundMinX),GLFLoat_Type(View.BoundMinY),0.1);
      glVertex3f(GLFloat_Type(View.BoundMinX),GLFloat_Type(View.BoundMaxY+1),0.1);
      glVertex3f(GLFloat_Type(View.BoundMaxX),GLFloat_Type(View.BoundMaxY+1),0.1);
      glVertex3f(GLFLoat_Type(View.BoundMaxX),GLFLoat_Type(View.BoundMinY),0.1);
      glEnd;

      OpenGL.Program.UseProgram(View.TerrainShader);

      declare
         usematrix : HomogenMatrix_Type;
      begin
         glGetFloatv(GL_PROJECTION_MATRIX, usematrix(0,0)'Access);
         AssertError("get Projection");
         usematrix:=Transpose(View.ModelMatrix)*usematrix;
         glUniformMatrix4fv(View.TerrainShaderUniformPerspectiveMatrix,1, GL_FALSE, usematrix(0,0)'Access);
         AssertError("UniformMatrix4f");
      end;

      glUseProgram(0);

      UpdateViewCaptions(View);

      glDisable(GL_LIGHTING);
      AssertError("RenderCustom");

   end RenderCustom;
   ---------------------------------------------------------------------------

   procedure CreateTerrainGeometryBuffer
     (View : access LandscapeView_Type) is
      Data : Cardinal32_Access;
   begin

      -- TODO: Check why there must be a +1 in order to prevent an exception when filling the buffer.
      --       Where is the -1 ?
      OpenGL.BufferTexture.Create
        (Tex       => View.TerrainGeometryBuffer,
         Size      => GLsizeiptr_Type(View.TerrainHeight*View.TerrainWidth*4*4+1),
         BasicType => GL_RGBA32F,
         UseHint   => GL_STATIC_DRAW);

      Data := AddressToCardinal32Access(OpenGL.BufferTexture.MapWriteOnly(Tex => View.TerrainGeometryBuffer));

      for y in 0..View.TerrainHeight-1 loop
         for x in 0..View.TerrainWidth-1 loop
            Data.all:=1;
            Data:=Data+1;
            Data.all:=0;
            Data:=Data+1;
            Data.all:=0;
            Data:=Data+1;
            Data.all:=0;
            Data:=Data+1;
         end loop;
      end loop;

      OpenGL.BufferTexture.Unmap(Tex => View.TerrainGeometryBuffer);

   end CreateTerrainGeometryBuffer;
   ---------------------------------------------------------------------------

   procedure CreateTerrainInformationBuffer
     (View : access LandscapeView_Type) is
   begin
      OpenGL.BufferTexture.Create
        (Tex       => View.ViewInformationBuffer,
         Size      => 16*4,
         BasicType => GL_LUMINANCE32F,
         UseHint   => GL_DYNAMIC_DRAW);

   end CreateTerrainInformationBuffer;
   ---------------------------------------------------------------------------

   procedure CreateTerrainSelectionBuffer
     (View : access LandscapeView_Type) is
      Data : Cardinal32_Access;
   begin
      OpenGL.BufferTexture.Create
        (Tex       => View.TerrainSelectionBuffer,
         Size      => GLsizeiptr_Type(View.TerrainHeight*View.TerrainWidth*4+1),
         BasicType => GL_LUMINANCE32F,
         UseHint   => GL_STATIC_DRAW);
      Data := AddressToCardinal32Access(OpenGL.BufferTexture.MapWriteOnly(Tex => View.TerrainSelectionBuffer));
      for y in 0..View.TerrainHeight-1 loop
         for x in 0..View.TerrainWidth-1 loop
            Data.all:=Cardinal32((x+y) mod 4);
            Data:=Data+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(Tex => View.TerrainSelectionBuffer);
   end CreateTerrainSelectionBuffer;
   ---------------------------------------------------------------------------

   procedure CreateTerrainVertexBuffer
     (View : access LandscapeView_Type) is

      Detail : constant Integer:=32;
      Size : constant Integer:=Detail*(Detail*2+2);
      Data : Float_Access;
      CurrentDirection : Integer;

      procedure AddVertex(AX, AY : integer) is
      begin
         Data.all:=GLFLoat_Type(AX)/GLFloat_Type(Detail);
         Data:=Data+1;
         Data.all:=GLFloat_Type(AY)/GLFloat_Type(Detail);
         Data:=Data+1;
         Data.all:=0.0;
         Data:=Data+1;
         Data.all:=1.0;
         Data:=Data+1;
      end AddVertex;
      ------------------------------------------------------------------------

   begin
      glGenBuffers(1,View.TerrainVBO'Access);
      glBindBuffer(GL_ARRAY_BUFFER, View.TerrainVBO);
--      glEnableClientState(GL_VERTEX_ARRAY);
      glBufferData(GL_ARRAY_BUFFER, GLsizeiptr_Type(Size*16), System.Null_Address, GL_STATIC_DRAW);
      Data := AddressToFloatAccess(glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY));
      Put(Data.all'Address);
      Put_Line("FIll VBO");
      CurrentDirection := 1;
      for CurrentY in 0..Detail-1 loop
         if CurrentDirection = 1 then
            AddVertex(0,CurrentY);
            for CurrentX in 0..Detail-1 loop
               AddVertex(CurrentX, CurrentY+1);
               AddVertex(CurrentX+1,CurrentY);
            end loop;
            CurrentDirection := -1;
            AddVertex(Detail,CurrentY+1);
         else
            AddVertex(Detail,CurrentY);
            for CurrentX in reverse 0..Detail-1 loop
               AddVertex(CurrentX+1,CurrentY+1);
               AddVertex(CurrentX,CurrentY);
            end loop;
            CurrentDirection:=1;
            AddVertex(0,CurrentY+1);
         end if;
      end loop;
      Put_Line("Done VBO");

      if glUnmapBuffer(GL_ARRAY_BUFFER)=0 then
         raise OpenGL.BufferTexture.FailedUnMap;
      end if;

   end CreateTerrainVertexBuffer;
   ---------------------------------------------------------------------------

   function NewLandscapeView
     (Parent : GUI.Object_ClassAccess;
      Theme  : GUI.Themes.Implementation_Type)
      return LandscapeView_Access is

      View    : LandscapeView_Access;

   begin
      View        := new LandscapeView_Type;
      View.Render := GUIDefinitions.RenderCustom;
      View.Initialize(Parent => Parent);

      CreateTerrainGeometryBuffer(View);
      CreateTerrainInformationBuffer(View);
      CreateTerrainSelectionBuffer(View);
      CreateTerrainVertexBuffer(View);
      View.Translate(2) := -18.0;
      View.Translate(0) := -64.0;
      View.Translate(1) := -32.0;
      View.RotateX      := -30.0/180.0*3.14;

      OpenGL.Program.Create
        (Shader     => View.TerrainVShader,
         ShaderType => OpenGL.Program.ShaderVertex,
         Source     => TerrainShader);
      Put_Line(To_String(OpenGL.Program.GetCompileLog(View.TerrainVShader)));

      OpenGL.Program.Create
        (Shader     => View.TerrainFShader,
         ShaderType => OpenGL.Program.ShaderFragment,
         Source     => FragmentShader);
      Put_Line(To_String(OpenGL.Program.GetCompileLog(View.TerrainFShader)));

      OpenGL.Program.Create
        (Program => View.TerrainShader,
         Shaders => (OpenGL.Program.ShaderVertex   => View.TerrainVShader'Access,
                     OpenGL.Program.ShaderFragment => View.TerrainFShader'Access));

      OpenGL.Program.UseProgram(View.TerrainShader);
      AssertError("UseProgram");
      View.TerrainShaderUniformTerrain := OpenGL.Program.GetUniformLocation(View.TerrainShader,"terrain");
      Put_Line("Terrain(Uniform):"&GLint_Type'Image(View.TerrainShaderUniformTerrain));
      View.TerrainShaderUniformSelect := OpenGL.Program.GetUniformLocation(View.TerrainShader,"select");
      Put_Line("Select(Uniform):"&GLint_Type'Image(View.TerrainShaderUniformSelect));
      View.TerrainShaderUniformViewInformation := OpenGL.Program.GetUniformLocation(View.TerrainShader,"ViewInformation");
      Put_Line("ViewInformation(Uniform):"&GLint_Type'Image(View.TerrainShaderUniformViewInformation));
      View.TerrainShaderUniformPerspectiveMatrix := OpenGL.Program.GetUniformLocation(View.TerrainShader,"PerspectiveMatrix");
      Put_Line("Perspective(Uniform):"&GLint_Type'Image(View.TerrainShaderUniformPerspectiveMatrix));
      AssertError("Get Uniforms");
      glUniform1i(View.TerrainShaderUniformViewInformation,1);
      glUniform1i(View.TerrainShaderUniformTerrain,2);
      glUniform1i(View.TerrainShaderUniformSelect,3);
      AssertError("Set Uniforms");
      glUseProgram(0);

      CalcPerspective(View);

      return LandscapeView_Access(View);

   end NewLandscapeView;

end LandscapeView;
