with BoundsCalc; use BoundsCalc;
with GUIDefinitions;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Bytes; use Bytes;
with Types; use Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with Ada.Real_Time;
with VectorMath; use VectorMath;

package body LandscapeView is

   LineFeed : constant Character := Character'Val(10);

   DetailLevel  : constant Integer:=16;
   DetailVertexCount : constant Integer:=DetailLevel*(DetailLevel*2+2);

   -- ViewInformation :
   --  0 : TerrainWidth
   --  1 : ViewTop
   --  2 : ViewLeft
   --  3 : ViewWidth
   --  4 : Amplitude
   --  5 : WaveVector
   --  6 : Frequency
   --  7 : Time

   TerrainVShader : constant String :=
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
       " float SelFlag = texelFetchBuffer(select,texelPos);"&LineFeed&
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
       " vertexColor = (0.2*intensity+0.8)*vec4(SelFlag*0.5+fract(float(ViewLeft+deltaX)/2.0),0.6,fract(float(ViewTop+deltaY)/2.0),1);"&LineFeed&
       "}"&LineFeed;

   TerrainFShader : constant String :=
       "#version 330"&LineFeed&
       "smooth in vec4 vertexColor;"&LineFeed&
       "out vec4 outputColor;"&LineFeed&
       "void main()"&LineFeed&
       "{"&LineFeed&
       " outputColor=vertexColor;"&LineFeed&
       "}"&LineFeed;

   WaterVShader : constant String :=
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
       " float Amplitude  = texelFetchBuffer(ViewInformation,4).x;"&LineFeed&
       " float WaveVector = texelFetchBuffer(ViewInformation,5).x;"&LineFeed&
       " float Frequency  = texelFetchBuffer(ViewInformation,6).x;"&LineFeed&
       " float Time       = texelFetchBuffer(ViewInformation,7).x;"&LineFeed&
       " vec4 Position = inVertex;"&LineFeed&
       " int deltaY=gl_InstanceID/ViewWidth;"&LineFeed&
       " int deltaX=gl_InstanceID-deltaY*ViewWidth;"&LineFeed&
       " int texelPos=(ViewTop+deltaY)*TerrainWidth+deltaX+ViewLeft;"&LineFeed&
       " float SelFlag = texelFetchBuffer(select,texelPos);"&LineFeed&
       " float tx     = ViewLeft+deltaX+inVertex.x;"&LineFeed&
       " float px     = tx*WaveVector-Time*Frequency;"&LineFeed&
       " float deltaH = sin(px)+5.0;"&LineFeed&
       " float dfx    = cos(px);"&LineFeed&
       " float dfy    = 0;"&LineFeed&
       " vec3 n=normalize(vec3(-dfx,-dfy,1.0));"&LineFeed&
       " vec3 lightvec=normalize(vec3(1.0,1.0,-1.0));"&LineFeed&
       " float addi=-dot(lightvec,n);"&LineFeed&
       " float intensity=max(0.0,addi);"&LineFeed&
       " Position += vec4(float(ViewLeft+deltaX),float(ViewTop+deltaY),deltaH,0);"&LineFeed&
       " gl_Position = PerspectiveMatrix*Position;"&LineFeed&
       " vertexColor = (0.2*intensity+0.8)*vec4(SelFlag*0.5,0,1,1);"&LineFeed&
       "}"&LineFeed;

   WaterFShader : constant String :=
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

      View.InvModelRotation := RotateZHomogenMatrix(-View.RotateZ)*RotateXHomogenMatrix(-View.RotateX);
      DebugHomogenMatrix(View.InvModelRotation);

      View.InverseModelMatrix := TranslateHomogenMatrix(-View.Translate(0), -View.Translate(1), -View.Translate(2))
        * RotateZHomogenMatrix(-View.RotateZ)
        * RotateXHomogenMatrix(-View.RotateX);

      View.ModelMatrix := RotateXHomogenMatrix(View.RotateX)
        * RotateZHomogenMatrix(View.RotateZ)
        * TranslateHomogenMatrix(View.Translate(0),View.Translate(1),View.Translate(2));

      -- TODO: This length should be influenced by AspectRatio!
      InvLength := 1.0/Ada.Numerics.Elementary_Functions.Sqrt(2.0+View.NearDistance**2);

      PosSpot := View.InverseModelMatrix * AssignHomogenVector(0.0,0.0,0.0);

      DirTopLeft := View.InvModelRotation*(InvLength*AssignHomogenVector(-View.AspectRatio,1.0,-View.NearDistance));
      DirTopRight := View.InvModelRotation*(InvLength*AssignHomogenVector(View.AspectRatio,1.0,-View.NearDistance));
      DirBottomLeft := View.InvModelRotation*(InvLength*AssignHomogenVector(-View.AspectRatio,-1.0,-View.NearDistance));
      DirBottomRight := View.InvModelRotation*(InvLength*AssignHomogenVector(View.AspectRatio,-1.0,-View.NearDistance));

      PosTopLeft := AssignHomogenVector(PosSpot(0)-DirTopLeft(0)*PosSpot(2)/DirTopLeft(2), PosSpot(1)-DirTopLeft(1)*PosSpot(2)/DirTopLeft(2),0.0);
      PosTopRight := AssignHomogenVector(PosSpot(0)-DirTopRight(0)*PosSpot(2)/DirTopRight(2), PosSpot(1)-DirTopRight(1)*PosSpot(2)/DirTopRight(2),0.0);
      PosBottomLeft := AssignHomogenVector(PosSpot(0)-DirBottomLeft(0)*PosSpot(2)/DirBottomLeft(2),PosSpot(1)-DirBottomLeft(1)*PosSpot(2)/DirBottomLeft(2),0.0);
      PosBottomRight := AssignHomogenVector(PosSpot(0)-DirBottomRight(0)*PosSpot(2)/DirBottomRight(2),PosSpot(1)-DirBottomRight(1)*PosSpot(2)/DirBottomRight(2),0.0);

      CalculateMinimalBoundingBox((PosSpot, PosTopLeft, PosTopRight, PosBottomLeft, PosBottomRight),BoundingBox);

      View.BoundMinX := Integer'Min(Integer(GLFloat_Type'Max(GLFloat_Type'Floor(BoundingBox.MinX) , 1.0)),View.TerrainWidth-1);
      View.BoundMaxX := Integer'Max(Integer(GLFloat_Type'Min(GLFloat_Type'Ceiling(BoundingBox.MaxX), GLFloat_Type(View.TerrainWidth)-2.0)),0);
      View.BoundMinY := Integer'Min(Integer(GLFloat_Type'Max(GLFloat_Type'Floor(BoundingBox.MinY), 1.0)),View.TerrainHeight-1);
      View.BoundMaxY := Integer'Max(Integer(GLFloat_Type'Min(GLFloat_Type'Ceiling(BoundingBox.MaxY), GLFloat_Type(View.TerrainHeight)-2.0)),0);

      Put_Line(Integer'Image(View.BoundMinX));
      Put_Line(Integer'Image(View.BoundMaxX));
      Put_Line(Integer'Image(View.BoundMinY));
      Put_Line(Integer'Image(View.BoundMaxY));

   end CalcPerspective;
   ---------------------------------------------------------------------------

   procedure ModifySelectionBuffer
     (View : access LandscapeView_Type;
      StartX : Integer;
      StartY : Integer;
      StopX  : Integer;
      StopY  : Integer;
      Value  : GLFLoat_Type) is

      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;

   begin

      RStartX := Integer'Max(Integer'Min(StartX,StopX),0);
      Put_Line("RX:"&Integer'Image(RStartX));
      RStopX  := Integer'Min(Integer'Max(StartX,StopX),View.TerrainWidth-1);
      RStartY := Integer'Max(Integer'Min(StartY,StopY),0);
      RStopY  := Integer'Min(Integer'Max(StartY,StopY),View.TerrainHeight-1);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainSelectionBuffer));

      for y in RStartY..RStopY loop
         DataP:=Data+y*View.TerrainWidth+RStartX;
         for x in RStartX..RStopX loop
            DataP.all:=Value;
            DataP:=DataP+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);

   end ModifySelectionBuffer;
   ---------------------------------------------------------------------------

   procedure TerrainRayIntersect
     (View : access LandscapeView_Type;
      Ax   : Integer;
      Ay   : Integer;
      Sx   : out Integer;
      Sy   : out Integer) is

      Ray                  : HomogenVector_Type;
      Position             : HomogenVector_Type;
      StepSize             : GLFloat_Type;
      PlanePosition        : HomogenVector_Type;
      PlaneRay             : HomogenVector_Type;
      PlaneIntersection    : HomogenVector_Type;
      Step                 : GLFloat_Type:=0.0;
      StepMaximum          : GLFloat_Type;
      CurrentPosition      : HomogenVector_Type;
      NextPosition         : HomogenVector_Type;
      CurrentIntegerX      : Integer;
      CurrentIntegerY      : Integer;
      CurrentFracX         : GLFloat_Type;
      CurrentFracY         : GLFloat_Type;
      NextFracX            : GLFloat_Type;
      NextFracY            : GLFloat_Type;
      LastIntegerX         : Integer;
      LastIntegerY         : Integer;
      CurrentBicubicMatrix : FourDMatrix_Type;
      CurrentUseMatrix     : FourDMatrix_Type;
      p00                  : Integer;
      p01                  : Integer;
      p10                  : Integer;
      p11                  : Integer;
      CurrentHeight        : GLFloat_Type;
      NextHeight           : GLFloat_Type;
      CurrentBicubicHeight : GLFloat_Type;
      NextBicubicHeight    : GLFloat_Type;
      Intersection         : GLFloat_Type;
      Diff                 : GLFloat_Type;
      PlanePathLength      : GLFloat_Type;
      PlaneRayLength       : GLFloat_Type;
      InvPlaneRayLength    : GLFloat_Type;
      Bounds               : constant BoundsCalc.Bounds_Type:=View.GetBounds;
   begin
      Ray := View.InvModelRotation * AssignHomogenVector(GLFloat_Type(Ax-Bounds.Width/2)*(2.0*View.AspectRatio/GLFloat_Type(Bounds.Width)),GLFloat_Type(Ay-Bounds.Height/2)*(-2.0/GLFloat_Type(Bounds.Height)),-View.NearDistance);
      Position := View.InverseModelMatrix * AssignHomogenVector(0.0, 0.0, 0.0);
      PlanePosition := AssignHomogenVector(Position(0), Position(1), 0.0);
      PlaneRay := AssignHomogenVector(Ray(0), Ray(1), 0.0);
      PlaneIntersection := Position - (Position(2) / Ray(2)) * Ray;

      PlanePathLength := Length3DHomogenVector(PlaneIntersection - PlanePosition);
      PlaneRayLength  := Length3DHomogenVector(PlaneRay);

      InvPlaneRayLength := 1.0 / PlaneRayLength;

      StepSize := 0.05 * InvPlaneRayLength;


      StepMaximum := PlanePathLength * InvPlaneRayLength;

      StepMaximum  := StepMaximum + 1.0;
      LastIntegerX := -1;
      LastIntegerY := -1;
      while Step <= StepMaximum loop
         CurrentPosition := PlanePosition + Step * PlaneRay;
         NextPosition := PlanePosition + (Step + StepSize) * PlaneRay;
         CurrentIntegerX := Integer(GLFloat_Type'Floor(CurrentPosition(0)));
         CurrentIntegerY := Integer(GLFloat_Type'Floor(CurrentPosition(1)));
         if (CurrentIntegerX>=1) and (CurrentIntegerY>=1) and
           (CurrentIntegerX<View.TerrainWidth-1) and (CurrentIntegerY<View.TerrainHeight-1) then
            CurrentFracX  := CurrentPosition(0)-GLFloat_Type'Floor(CurrentPosition(0));
            CurrentFracY  := CurrentPosition(1)-GLFloat_Type'Floor(CurrentPosition(1));
            NextFracX     := NextPosition(0)-GLFloat_Type'Floor(NextPosition(0));
            NextFracY     := NextPosition(1)-GLFloat_Type'Floor(NextPosition(1));
            CurrentHeight := Position(2) * (1.0 - PlaneRayLength * Step / PlanePathLength);
            NextHeight    := Position(2) + (1.0 - PlaneRayLength * (Step + StepSize) / PlanePathLength);
            if (CurrentIntegerX /= LastIntegerX) or (CurrentIntegerY /= LastIntegerY) then
               p00 := CurrentIntegerX + CurrentIntegerY * View.TerrainWidth;
               p01 := p00 + View.TerrainWidth;
               p10 := p00 + 1;
               p11 := p01 + 1;
               CurrentBicubicMatrix(0, 0) := View.Terrain(p00).FLevel;
               CurrentBicubicMatrix(0, 1) := View.Terrain(p01).FLevel;
               CurrentBicubicMatrix(0, 2) := View.Terrain(p00).FDeriveY;
               CurrentBicubicMatrix(0, 3) := View.Terrain(p01).FDeriveY;
               CurrentBicubicMatrix(1, 0) := View.Terrain(p10).FLevel;
               CurrentBicubicMatrix(1, 1) := View.Terrain(p11).FLevel;
               CurrentBicubicMatrix(1, 2) := View.Terrain(p10).FDeriveY;
               CurrentBicubicMatrix(1, 3) := View.Terrain(p11).FDeriveY;
               CurrentBicubicMatrix(2, 0) := View.Terrain(p00).FDeriveX;
               CurrentBicubicMatrix(2, 1) := View.Terrain(p01).FDeriveX;
               CurrentBicubicMatrix(2, 2) := View.Terrain(p00).FDeriveXY;
               CurrentBicubicMatrix(2, 3) := View.Terrain(p01).FDeriveXY;
               CurrentBicubicMatrix(3, 0) := View.Terrain(p10).FDeriveX;
               CurrentBicubicMatrix(3, 1) := View.Terrain(p11).FDeriveX;
               CurrentBicubicMatrix(3, 2) := View.Terrain(p10).FDeriveXY;
               CurrentBicubicMatrix(3, 3) := View.Terrain(p11).FDeriveXY;
               CurrentUseMatrix := BicubicTransformationMatrixT * CurrentBicubicMatrix * BicubicTransformationMatrix;
            end if;
            CurrentBicubicHeight := Assign4DVector(1.0, CurrentFracX, CurrentFracX * CurrentFracX, CurrentFracX * CurrentFracX *CurrentFracX)
              * ( CurrentUseMatrix * Assign4DVector(1.0, CurrentFracY, CurrentFracY*CurrentFracY, CurrentFracY * CurrentFracY * CurrentFracY));
            NextBicubicHeight := Assign4DVector(1.0, NextFracX, NextFracX *NextFracX, NextFracX * NextFracX * NextFracX)
              * (CurrentUseMatrix * Assign4DVector(1.0, NextFracY, NextFracY * NextFracY,NextFracY * NextFracY * NextFracY));
            Diff := (NextBicubicHeight - CurrentBicubicHeight) - (NextHeight - CurrentHeight);

            if (Diff /= 0.0) then
               InterSection := (CurrentHeight - CurrentBicubicHeight) / Diff;
               if (InterSection >= 0.0) and (InterSection <= 1.0) then
                  SX := CurrentIntegerX;
                  SY := CurrentIntegerY;
                  Exit;
               end if;
            end if;

         end if;
         Step         := Step + StepSize;
         LastIntegerX := CurrentIntegerX;
         LastIntegerY := CurrentIntegerY;
      end loop;
   end;

   procedure UpdateViewCaptions
     (View : access LandscapeView_Type) is
   begin
      null;
   end UpdateViewCaptions;
   ---------------------------------------------------------------------------

   procedure Terraform_Flat
     (View : access LandscapeView_Type) is
      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),0);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-1);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),0);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-1);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));

      for y in RStartY..RStopY loop
         Pos:=y*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for x in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
            begin
               Cell.Flevel    := 8.0;
               Cell.FDeriveX  := 0.0;
               Cell.FDeriveY  := 0.0;
               Cell.FDeriveXY := 0.0;
               DataP.all:=Cell.FLevel;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveX;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveY;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveXY;
               DataP:=DataP+1;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Flat;
   ---------------------------------------------------------------------------

   procedure Terraform_Up
     (View : access LandscapeView_Type) is
      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),0);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-1);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),0);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-1);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));

      for y in RStartY..RStopY loop
         Pos:=y*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for x in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
            begin
               Cell.Flevel    := Cell.FLevel+0.1;
               DataP.all:=Cell.FLevel;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveX;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveY;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveXY;
               DataP:=DataP+1;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Up;
   ---------------------------------------------------------------------------

   procedure Terraform_Down
     (View : access LandscapeView_Type) is
      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),0);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-1);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),0);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-1);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));

      for y in RStartY..RStopY loop
         Pos:=y*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for x in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
            begin
               Cell.Flevel    := Cell.FLevel-0.1;
               DataP.all:=Cell.FLevel;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveX;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveY;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveXY;
               DataP:=DataP+1;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Down;
   ---------------------------------------------------------------------------

   procedure Terraform_Average
     (View : access LandscapeView_Type) is
      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),1);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-2);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),1);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-2);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));

      for y in RStartY..RStopY loop
         Pos:=y*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for x in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
               Up:TerrainCell_Type renames View.Terrain(Pos-View.TerrainWidth);
               Down: TerrainCell_Type renames View.Terrain(Pos+View.TerrainWidth);
               Left:TerrainCell_Type renames View.Terrain(Pos-1);
               Right:TerrainCell_Type renames View.Terrain(Pos+1);
            begin
               Cell.Flevel    := (Cell.FLevel+Up.FLevel+Down.FLevel+Left.FLevel+Right.FLevel)/5.0;
               DataP.all:=Cell.FLevel;
               DataP:=DataP+4;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Average;
   ---------------------------------------------------------------------------

   procedure Terraform_Smooth
     (View : access LandscapeView_Type) is
      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),1);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-2);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),1);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-2);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));

      for y in RStartY..RStopY loop
         Pos:=y*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for x in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
               Width:constant Integer:=View.TerrainWidth;
               Terrain:Terrain_Access renames View.Terrain;
            begin
               Cell.FDeriveX:=(Terrain(Pos+1).FLevel-Terrain(Pos-1).FLevel)*0.5;
               Cell.FDeriveY:=(Terrain(Pos+Width).FLevel-Terrain(Pos-Width).FLevel)*0.5;
               Cell.FDeriveXY:=((Terrain(Pos+Width+1).FLevel-Terrain(Pos+Width-1).FLevel)
                 -(Terrain(Pos-Width+1).FLevel-Terrain(Pos-Width-1).FLevel))*0.0;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveX;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveY;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveXY;
               DataP:=DataP+1;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Smooth;
   ---------------------------------------------------------------------------

   procedure Terraform_Null
     (View : access LandscapeView_Type) is
      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),1);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-2);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),1);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-2);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));
      for y in RStartY..RStopY loop
         Pos:=y*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for x in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
            begin
               Cell.Flevel    := 0.0;
               Cell.FDeriveX  := 0.0;
               Cell.FDeriveY  := 0.0;
               Cell.FDeriveXY := 0.0;
               DataP.all:=Cell.FLevel;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveX;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveY;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveXY;
               DataP:=DataP+1;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Null;
   ---------------------------------------------------------------------------

   procedure Terraform_Sin
     (View : access LandscapeView_Type;
      Ampl : GLFloat_Type;
      Kx   : GLFloat_Type;
      Ky   : GLFloat_Type) is

      RStartX : Integer;
      RStartY : Integer;
      RStopX  : Integer;
      RStopY  : Integer;
      Data    : Float_Access;
      DataP   : Float_Access;
      Pos     : Natural;
   begin
      RStartX := Integer'Max(Integer'Min(View.SelectStartX,View.SelectStopX),1);
      RStopX  := Integer'Min(Integer'Max(View.SelectStartX,View.SelectStopX),View.TerrainWidth-2);
      RStartY := Integer'Max(Integer'Min(View.SelectStartY,View.SelectStopY),1);
      RStopY  := Integer'Min(Integer'Max(View.SelectStartY,View.SelectStopY),View.TerrainHeight-2);

      Data:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.TerrainGeometryBuffer));
      for iy in RStartY..RStopY loop
         Pos:=iy*View.TerrainWidth+RStartX;
         DataP:=(Data+Pos*4);
         for ix in RStartX..RStopX loop
            declare
               Cell:TerrainCell_Type renames View.Terrain(Pos);
               x : constant GLFloat_Type:=GLFloat_Type(ix);
               y : constant GLFloat_Type:=GLFloat_Type(iy);
            begin
               Cell.Flevel    := Cell.Flevel+Ampl*Sin(Kx*x+Ky*y,2.0*Ada.Numerics.Pi);
               Cell.FDeriveX  := Cell.FDeriveX+Ampl*Kx*Cos(Kx*x+Ky*y,2.0*Ada.Numerics.Pi);
               Cell.FDeriveY  := Cell.FDeriveY+Ampl*Ky*Cos(Kx*x+Ky*y,2.0*Ada.Numerics.Pi);
               Cell.FDeriveXY := Cell.FDeriveXY-Ampl*Kx*Ky*Sin(Kx*x+Ky*y,2.0*Ada.Numerics.Pi);
               DataP.all:=Cell.FLevel;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveX;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveY;
               DataP:=DataP+1;
               DataP.all:=Cell.FDeriveXY;
               DataP:=DataP+1;
            end;
            Pos:=Pos+1;
         end loop;
      end loop;
      OpenGL.BufferTexture.Unmap(View.TerrainSelectionBuffer);
   end Terraform_Sin;
   ---------------------------------------------------------------------------

   function CharacterInput
     (View  : access LandscapeView_Type;
      Chars : Unbounded_String)
      return Boolean is
   begin
      Put_Line("Char:"&To_String(Chars)&GLFloat_Type'Image(View.Translate(2)));
      if Chars="+" then
         View.Translate(2):=GLFLoat_Type'Min(View.Translate(2)+1.0,-2.0);
         CalcPerspective(View);
         return True;
      elsif Chars="-" then
         View.Translate(2):=GLFloat_Type'Max(View.Translate(2)-1.0,-60.0);
         CalcPerspective(View);
         return True;
      elsif Chars="a" then
         View.RotateZ:=View.RotateZ-0.01;
         CalcPerspective(View);
         return True;
      elsif Chars="d" then
         View.RotateZ:=View.RotateZ+0.01;
         CalcPerspective(View);
         return True;
      elsif Chars="f" then
         Terraform_Flat(View);
         return True;
      elsif Chars="l" then
         Terraform_Average(View);
         return True;
      elsif Chars="k" then
         Terraform_Smooth(View);
         return True;
      elsif Chars="n" then
         Terraform_Null(View);
         return True;
      elsif Chars="1" then
         Terraform_Sin(View,0.1,1.0/5.0,1.0/5.0);
         Return True;
      elsif Chars="2" then
         Terraform_Sin(View,0.1,1.0/5.0,0.0);
         Return True;
      elsif Chars="3" then
         Terraform_Sin(View,0.1,0.0,1.0/5.0);
         Return True;
      end if;
      return False;
   end CharacterInput;
   ---------------------------------------------------------------------------

   function KeyDown
     (View : access LandscapeView_Type;
      Key  : Key_Enum)
      return Boolean is
   begin
      if Key=KeyPageUp then
         Terraform_Up(View);
         return True;
      elsif Key=KeyPageDown then
         Terraform_Down(View);
         return True;
      end if;
      return False;
   end KeyDown;
   ---------------------------------------------------------------------------

   procedure ProcessMouseMove
     (View : access LandscapeView_Type;
      X    : Integer;
      Y    : Integer) is
   begin
      case View.MouseMode is
         when MouseModeUnknown =>
            null;
         when MouseModeSelect =>
            ModifySelectionBuffer(View,View.SelectStartX,View.SelectStartY,View.SelectStopX,View.SelectStopY,0.0);
            TerrainRayIntersect(View,X,Y,View.SelectStopX,View.SelectStopY);
            ModifySelectionBuffer(View,View.SelectStartX,View.SelectStartY,View.SelectStopX,View.SelectStopY,1.0);
         when MouseModeMove =>
            declare
               DX : GLFLoat_Type:=0.2*GLFloat_Type(View.MouseDownX-X);
               DY : GLFloat_Type:=0.2*GLFloat_Type(View.MouseDownY-Y);
            begin
               View.Translate(0):=View.MouseDownTranslate(0)-cos(View.RotateZ)*DX+sin(View.RotateZ)*DY;
               View.Translate(1):=View.MouseDownTranslate(1)+sin(View.RotateZ)*DX+cos(View.ROtateZ)*DY;

            end;
            View.Translate(0):=GLFloat_Type'Max(GLFloat_Type'Min(View.Translate(0),0.0),-GLFloat_Type(View.TerrainWidth));
            View.Translate(1):=GLFloat_Type'Max(GLFloat_Type'Min(View.Translate(1),0.0),-GLFloat_Type(View.TerrainHeight));
            CalcPerspective(View);
      end case;
   end ProcessMouseMove;
   ---------------------------------------------------------------------------

   function MouseDown
     (View   : access LandscapeView_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

   begin

      -- TODO: Finalize MouseMode
      View.MouseDownX := X;
      View.MouseDownY := Y;
      View.MouseDownTranslate := View.Translate;

      if Button=LeftButton then
         if View.ValidSelection then
            ModifySelectionBuffer(View,View.SelectStartX,View.SelectStartY,View.SelectStopX,View.SelectStopY,0.0);
         end if;
         View.MouseMode := MouseModeSelect;
         View.ValidSelection:=True;
         TerrainRayIntersect(View,X,Y,View.SelectStartX,View.SelectStartY);
      elsif Button=RightButton then
         if View.ValidSelection then
            View.ValidSelection:=False;
            ModifySelectionBuffer(View,View.SelectStartX,View.SelectStartY,View.SelectStopX,View.SelectStopY,0.0);
         end if;
         View.MouseMode := MouseModeMove;
      end if;
      ProcessMouseMove(View,X,Y);

      return True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (View : access LandscapeView_Type;
      X : Integer;
      Y : Integer) is
   begin
      ProcessMouseMove(View,X,Y);
   end MouseMove;
   ---------------------------------------------------------------------------

   overriding
   procedure MouseUp
     (View   : access LandscapeView_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is
   begin
      View.MouseMode := MouseModeUnknown;
   end MouseUp;
   ---------------------------------------------------------------------------

   procedure Resize
     (View : access LandscapeView_Type) is
   begin
      CalcPerspective(View);
   end Resize;
   ---------------------------------------------------------------------------

   Count : Float:=0.0;

   procedure RenderCustom
     (View : access LandscapeView_Type) is

      use type Ada.Real_Time.Time_Span;

      AbsBounds   : AbsBounds_Type renames View.AbsBounds;
      Bounds      : constant Bounds_Type := View.GetBounds;
      AspectRatio : GLDouble_Type;
      InfoBuffer  : Float_Access;
      usematrix   : HomogenMatrix_Type;
      DeltaTime   : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Clock-View.StartTime;

   begin
      AssertError("RenderCustom.Start");
      if not AbsBounds.AbsVisible then
         return;
      end if;

      InfoBuffer:=AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(View.ViewInformationBuffer));
      InfoBuffer.all:=GLFLoat_Type(View.TerrainWidth);
      InfoBuffer:=InfoBuffer+1;
      InfoBuffer.all:=GLFLoat_Type(View.BoundMinY);
      InfoBuffer:=InfoBuffer+1;
      InfoBuffer.all:=GLFloat_Type(View.BoundMinX);
      InfoBuffer:=InfoBuffer+1;
      InfoBuffer.all:=GLFLoat_Type(View.BoundMaxX-View.BoundMinX+1);
      InfoBuffer:=InfoBuffer+1;
      InfoBuffer.all:=0.1; -- Amplitude
      InfoBuffer:=InfoBuffer+1;
      InfoBuffer.all:=1.0/2.0; -- Wavevector
      InfoBuffer:=InfoBuffer+1;
      InfoBuffer.all:=1.0/2.0; -- Frequency
      InfoBuffer:=InfoBuffer+1;
      Count:=Count+1.0;
      InfoBuffer.all:=Float(Ada.Real_Time.To_Duration(DeltaTime)); -- Time
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

      glUseProgram(0);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      glRotatef(View.RotateX*180.0/3.14, 1.0, 0.0, 0.0);
      glRotatef(View.RotateZ*180.0/3.14, 0.0, 0.0, 1.0);
      glTranslatef(View.Translate(0), View.Translate(1), View.Translate(2));
      glGetFloatv(GL_PROJECTION_MATRIX, usematrix(0,0)'Access);
      usematrix:=Transpose(View.ModelMatrix)*usematrix;

      OpenGL.Program.UseProgram(View.WaterShader);

      glUniformMatrix4fv(View.WaterShaderUniformPerspectiveMatrix,1, GL_FALSE, usematrix(0,0)'Access);
      glBindBuffer(GL_ARRAY_BUFFER, View.TerrainVBO);
      glEnableVertexAttribArray(0);
      glVertexAttribPointer(0,4,GL_FLOAT,GL_FALSE,0,System.Null_Address);
      OpenGL.BufferTexture.Activate(View.ViewInformationBuffer,1);
      OpenGL.BufferTexture.Activate(View.TerrainGeometryBuffer,2);
      OpenGL.BufferTexture.Activate(View.TerrainSelectionBuffer,3);
      glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, GLint_Type(DetailVertexCount),
                            GLint_Type((View.BoundMaxY-View.BoundMinY+1)*(View.BoundMaxX-View.BoundMinX+1)));

      AssertError("UniformMatrix4f");

      OpenGL.Program.UseProgram(View.TerrainShader);

      glUniformMatrix4fv(View.TerrainShaderUniformPerspectiveMatrix,1, GL_FALSE, usematrix(0,0)'Access);
      glBindBuffer(GL_ARRAY_BUFFER, View.TerrainVBO);
      glVertexAttribPointer(0,4,GL_FLOAT,GL_FALSE,0,System.Null_Address);
      glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, GLint_Type(DetailVertexCount),
                            GLint_Type((View.BoundMaxY-View.BoundMinY+1)*(View.BoundMaxX-View.BoundMinX+1)));
      BindTexture(GL_TEXTURE_BUFFER,1,0);
      BindTexture(GL_TEXTURE_BUFFER,2,0);
      BindTexture(GL_TEXTURE_BUFFER,3,0);
      glDisableVertexAttribArray(0);
      glBindBuffer(GL_ARRAY_BUFFER, 0);

      AssertError("UniformMatrix4f");

      glUseProgram(0);

      UpdateViewCaptions(View);

      AssertError("RenderCustom");

   end RenderCustom;
   ---------------------------------------------------------------------------

   procedure CreateTerrainGeometryBuffer
     (View : access LandscapeView_Type) is
      Data : Float_Access;
      AmplitudeX : constant Float:=5.0;
      FreqX : constant Float:=1.0/10.0;
      Pos : Natural;
   begin

      -- TODO: Check why there must be a +1 in order to prevent an exception when filling the buffer.
      --       Where is the -1 ?
      OpenGL.BufferTexture.Create
        (Tex       => View.TerrainGeometryBuffer,
         Size      => GLsizeiptr_Type(View.TerrainHeight*View.TerrainWidth*4*4+1),
         BasicType => GL_RGBA32F,
         UseHint   => GL_STATIC_DRAW);

      Data := AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(Tex => View.TerrainGeometryBuffer));

      View.Terrain:=new Terrain_Array(0..View.TerrainHeight*View.TerrainWidth-1);
      Pos:=0;

      for y in 0..View.TerrainHeight-1 loop
         for x in 0..View.TerrainWidth-1 loop
            declare
               Cell : TerrainCell_Type;
            begin
               Cell.Flevel := AmplitudeX*sin(GLFloat_Type(x)*FreqX,2.0*Ada.Numerics.Pi)+AmplitudeX;
               Cell.FDeriveX := AmplitudeX*FreqX*cos(GLFloat_Type(x)*FreqX,2.0*Ada.Numerics.Pi);
               Cell.FDeriveY := 0.0;
               Cell.FDeriveXY := 0.0;
               Data.all := Cell.Flevel;
               Data:=Data+1;
               Data.all := Cell.FDeriveX;
               Data:=Data+1;
               Data.all := Cell.FDeriveY;
               Data:=Data+1;
               Data.all := Cell.FDeriveXY;
               Data:=Data+1;
               View.Terrain(Pos):=Cell;
               Pos:=Pos+1;
            end;
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
         Size      => 32*4,
         BasicType => GL_LUMINANCE32F,
         UseHint   => GL_DYNAMIC_DRAW);

   end CreateTerrainInformationBuffer;
   ---------------------------------------------------------------------------

   procedure CreateTerrainSelectionBuffer
     (View : access LandscapeView_Type) is
      Data : Float_Access;
   begin
      OpenGL.BufferTexture.Create
        (Tex       => View.TerrainSelectionBuffer,
         Size      => GLsizeiptr_Type(View.TerrainHeight*View.TerrainWidth*4+1),
         BasicType => GL_LUMINANCE32F,
         UseHint   => GL_STATIC_DRAW);

      Data := AddressToFloatAccess(OpenGL.BufferTexture.MapWriteOnly(Tex => View.TerrainSelectionBuffer));

      for y in 0..View.TerrainHeight-1 loop
         for x in 0..View.TerrainWidth-1 loop
            Data.all:=0.0;
            Data:=Data+1;
         end loop;
      end loop;

      OpenGL.BufferTexture.Unmap(Tex => View.TerrainSelectionBuffer);
   end CreateTerrainSelectionBuffer;
   ---------------------------------------------------------------------------

   procedure CreateTerrainVertexBuffer
     (View : access LandscapeView_Type) is

      Data    : GLFloat_Array(0..DetailVertexCount*4-1);
      DataPos : Integer:=0;
      CurrentDirection : Integer;

      procedure AddVertex(AX, AY : integer) is
      begin
         Data(DataPos):=GLFLoat_Type(AX)/GLFloat_Type(DetailLevel);
         DataPos:=DataPos+1;
         Data(DataPos):=GLFloat_Type(AY)/GLFloat_Type(DetailLevel);
         DataPos:=DataPos+1;
         Data(DataPos):=0.0;
         DataPos:=DataPos+1;
         Data(DataPos):=1.0;
         DataPos:=DataPos+1;
      end AddVertex;
      ------------------------------------------------------------------------

   begin
      CurrentDirection := 1;
      for CurrentY in 0..DetailLevel-1 loop
         if CurrentDirection = 1 then
            AddVertex(0,CurrentY);
            for CurrentX in 0..DetailLevel-1 loop
               AddVertex(CurrentX, CurrentY+1);
               AddVertex(CurrentX+1,CurrentY);
            end loop;
            CurrentDirection := -1;
            AddVertex(DetailLevel,CurrentY+1);
         else
            AddVertex(DetailLevel,CurrentY);
            for CurrentX in reverse 0..DetailLevel-1 loop
               AddVertex(CurrentX+1,CurrentY+1);
               AddVertex(CurrentX,CurrentY);
            end loop;
            CurrentDirection:=1;
            AddVertex(0,CurrentY+1);
         end if;
      end loop;

      glGenBuffers(1,View.TerrainVBO'Access);
      glBindBuffer(GL_ARRAY_BUFFER, View.TerrainVBO);
      glBufferData(GL_ARRAY_BUFFER, GLsizeiptr_Type(Data'Size/8), Data(0)'Address, GL_STATIC_DRAW);
      glEnableVertexAttribArray(0);
      glVertexAttribPointer
        (index => 0,
         size  => 4,
         ttype => GL_FLOAT,
         normalized => GL_FALSE,
         stride => 0,
         pointer => System.Null_Address);
      glBindVertexArray(0);
      AssertError("Init VBO");

   end CreateTerrainVertexBuffer;
   ---------------------------------------------------------------------------

   function NewLandscapeView
     (Parent : GUI.Object_ClassAccess;
      Theme  : GUI.Themes.Implementation_Type)
      return LandscapeView_Access is
      pragma Unreferenced(Theme);

      View    : LandscapeView_Access;

   begin
      View        := new LandscapeView_Type;
      View.Render := GUIDefinitions.RenderCustom;
      View.Initialize(Parent => Parent);
      View.FocusStyle:=GUIDefinitions.FocusStyleAccept;
      View.StartTime := Ada.Real_Time.Clock;

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
         Source     => TerrainVShader);
      Put_Line(To_String(OpenGL.Program.GetCompileLog(View.TerrainVShader)));

      OpenGL.Program.Create
        (Shader     => View.TerrainFShader,
         ShaderType => OpenGL.Program.ShaderFragment,
         Source     => TerrainFShader);
      Put_Line(To_String(OpenGL.Program.GetCompileLog(View.TerrainFShader)));

      OpenGL.Program.Create
        (Program => View.TerrainShader,
         Shaders => (OpenGL.Program.ShaderVertex   => View.TerrainVShader'Access,
                     OpenGL.Program.ShaderFragment => View.TerrainFShader'Access));

      OpenGL.Program.UseProgram(View.TerrainShader);
      AssertError("UseProgram");
      View.TerrainShaderUniformTerrain := OpenGL.Program.GetUniformLocation(View.TerrainShader,"terrain");
      View.TerrainShaderUniformSelect := OpenGL.Program.GetUniformLocation(View.TerrainShader,"select");
      View.TerrainShaderUniformViewInformation := OpenGL.Program.GetUniformLocation(View.TerrainShader,"ViewInformation");
      View.TerrainShaderUniformPerspectiveMatrix := OpenGL.Program.GetUniformLocation(View.TerrainShader,"PerspectiveMatrix");
      AssertError("Get Uniforms");
      glUniform1i(View.TerrainShaderUniformViewInformation,1);
      glUniform1i(View.TerrainShaderUniformTerrain,2);
      glUniform1i(View.TerrainShaderUniformSelect,3);
      AssertError("Set Uniforms");
      glUseProgram(0);

      OpenGL.Program.Create
        (Shader     => View.WaterVShader,
         ShaderType => OpenGL.Program.ShaderVertex,
         Source     => WaterVShader);
      Put_Line(To_String(OpenGL.Program.GetCompileLog(View.WaterFShader)));

      OpenGL.Program.Create
        (Shader     => View.WaterFShader,
         ShaderType => OpenGL.Program.ShaderFragment,
         Source     => WaterFShader);
      Put_Line(To_String(OpenGL.Program.GetCompileLog(View.WaterVShader)));

      OpenGL.Program.Create
        (Program => View.WaterShader,
         Shaders => (OpenGL.Program.ShaderVertex   => View.WaterVShader'Access,
                     OpenGL.Program.ShaderFragment => View.WaterFShader'Access));

      OpenGL.Program.UseProgram(View.WaterShader);
      AssertError("UseProgram");
      View.WaterShaderUniformSelect := OpenGL.Program.GetUniformLocation(View.WaterShader,"select");
      View.WaterShaderUniformViewInformation := OpenGL.Program.GetUniformLocation(View.WaterShader,"ViewInformation");
      View.WaterShaderUniformPerspectiveMatrix := OpenGL.Program.GetUniformLocation(View.WaterShader,"PerspectiveMatrix");
      AssertError("Get Uniforms");
      glUniform1i(View.WaterShaderUniformViewInformation,1);
      glUniform1i(View.WaterShaderUniformSelect,3);
      AssertError("Set Uniforms");
      glUsePRogram(0);

      CalcPerspective(View);

      return View;

   end NewLandscapeView;

end LandscapeView;
