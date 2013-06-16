with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;

package body VectorMath is

   function AssignHomogenVector(AValue1, AValue2, AValue3 : GLFloat_Type) return HomogenVector_Type is
   begin
      return Result:HomogenVector_Type do
         Result(0) := AValue1;
         Result(1) := AValue2;
         Result(2) := AValue3;
         Result(3) := 1.0;
      end return;
   end AssignHomogenVector;

   function "*" (ALeftVector,ARightVector:HomogenVector_Type) return GLFloat_Type is
   begin
      return ALeftVector(0)*ARightVector(0)+ALeftVector(1)*ARightVector(1)+ALeftVector(2)*ARightVector(2);
   end;

   function "*" (ALeftVector,ARightVector:FourDVector_Type) return GLFloat_Type is
   begin
      return ALeftVector(0)*ARightVector(0)+ALeftVector(1)*ARightVector(1)+ALeftVector(2)*ARightVector(2)+ALeftVector(3)*ARightVector(3);
   end;

   function "+" (ALeftVector,ARightVector:HomogenVector_Type) return HomogenVector_Type is
   begin
      return Result:HomogenVector_Type do
         Result(0):=ALeftVector(0)+ARightVector(0);
         Result(1):=ALeftVector(1)+ARightVector(1);
         Result(2):=ALeftVector(2)+ARightVector(2);
         Result(3):=1.0;
      end return;
   end;

   function "-" (ALeftVector,ARightVector:HomogenVector_Type) return HomogenVector_Type is
   begin
      return Result:HomogenVector_Type do
         Result(0):=ALeftVector(0)-ARightVector(0);
         Result(1):=ALeftVector(1)-ARightVector(1);
         Result(2):=ALeftVector(2)-ARightVector(2);
         Result(3):=1.0;
      end return;
   end;

   function Length3DHomogenVector(AVector:HomogenVector_Type) return GLFloat_Type is
   begin
      return Ada.Numerics.Elementary_Functions.Sqrt((AVector(0)**2)+(AVector(1)**2)+(AVector(2))**2);
   end;

   function Normalize3DHomogenVector(AVector:HomogenVector_Type) return HomogenVector_Type is
      InvLength : GLFloat_Type;
   begin
      InvLength := 1.0/Length3DHomogenVector(AVector);
      return Result:HomogenVector_Type do
         Result(0) := AVector(0)*InvLength;
         Result(1) := AVector(1)*InvLength;
         Result(2) := AVector(2)*InvLength;
         Result(3) := 1.0;
      end return;
   end;

   function Transpose(AMatrix:HomogenMatrix_Type) return HomogenMatrix_Type is
   begin
      return Result:HomogenMatrix_Type do
         for i in 0..3 loop
            for n in 0..3 loop
               Result(n,i):=AMatrix(i,n);
            end loop;
         end loop;
      end return;
   end;

   procedure DebugHomogenMatrix(AMatrix:HomogenMatrix_Type) is
   begin
      for i in 0..3 loop
         Put(" ");
         for n in 0..3 loop
            Put(GLFLoat_Type'Image(AMatrix(i,n))&" ");
         end loop;
         New_Line;
      end loop;
   end;

   procedure DebugHomogenVector(AVector:HomogenVector_Type) is
   begin
      Put_Line("X:"&GLFLoat_Type'Image(AVector(0))&" Y:"&GLFLoat_Type'Image(AVector(1))&" Z: "&GLFLoat_Type'Image(AVector(2)));
   end;

   procedure CalculateMinimalBoundingBox(AVectors:HomogenVector_Array;ABox:out BoundingBox_Type) is
   begin
      if AVectors'Length=0 then
         raise ZeroBoundingBox;
      end if;
      ABox.MinX := AVectors(0)(0);
      ABox.MaxX := ABox. MinX;
      ABox.MinY := AVectors(0)(1);
      ABox.MaxY := ABox.MinY;
      ABox.MaxZ := AVectors(0)(2);
      ABox.MaxZ := ABox.MinZ;
      for i in AVectors'Range loop
         if AVectors(i)(0)<ABox.MinX then
            ABox.MinX:=AVectors(i)(0);
         end if;
         if AVectors(i)(0)>ABox.MaxX then
            ABox.MaxX:=AVectors(i)(0);
         end if;
         if AVectors(i)(1)<ABox.MinY then
            ABox.MinY:=AVectors(i)(1);
         end if;
         if AVectors(i)(1)>ABox.MaxY then
            ABox.MaxY:=AVectors(i)(1);
         end if;
         if AVectors(i)(2)<ABox.MinZ then
            ABox.MinZ:=AVectors(i)(2);
         end if;
         if AVectors(i)(2)>ABox.MaxZ then
            ABox.MaxZ:=AVectors(i)(2);
         end if;
      end loop;
   end;

   function RotateXHomogenMatrix(ADegree:GLFLoat_Type) return HomogenMatrix_Type is
   begin
      return Result:HomogenMatrix_Type do
         Result      := IdentityHomogenMatrix;
         Result(1,1) := Ada.Numerics.Elementary_Functions.Cos(ADegree);
         Result(2,1) := Ada.Numerics.Elementary_Functions.Sin(ADegree);
         Result(1,2) := -Result(2,1);
         Result(2,2) := Result(1,1);
      end return;
   end;

   function RotateYHomogenMatrix(ADegree:GLFLoat_Type) return HomogenMatrix_Type is
   begin
      return Result:HomogenMatrix_Type do
         Result      := IdentityHomogenMatrix;
         Result(0,0) := Ada.Numerics.Elementary_Functions.Cos(ADegree);
         Result(0,2) := Ada.Numerics.Elementary_Functions.Sin(ADegree);
         Result(2,0) := -Result(0,2);
         Result(2,2) := Result(0,0);
      end return;
   end;

   function RotateZHomogenMatrix(ADegree:GLFLoat_Type) return HomogenMatrix_Type is
   begin
      return Result:HomogenMatrix_Type do
         Result      := IdentityHomogenMatrix;
         Result(0,0) := Ada.Numerics.Elementary_Functions.Cos(ADegree);
         Result(1,0) := Ada.Numerics.Elementary_Functions.Sin(ADegree);
         Result(0,1) := -Result(1,0);
         Result(1,1) := Result(0,0);
      end return;
   end;

   function "*" (AScalar:GLFloat_Type;AVector:HomogenVector_Type) return HomogenVector_Type is
   begin
      return Result:HomogenVector_Type do
         for i in 0..2 loop
            Result(i) := AScalar*AVector(i);
         end loop;
         Result(3):=AVector(3);
      end return;
   end;

   function "*" (AMatrix:HomogenMatrix_Type;AVector:HomogenVector_Type) return HomogenVector_Type is
   begin
      return Result:HomogenVector_Type do
         for i in 0..3 loop
            Result(i) := AMatrix(i,0)*AVector(0)
              +AMatrix(i,1)*AVector(1)
              +AMatrix(i,2)*AVector(2)
              +AMAtrix(i,3)*AVector(3);
         end loop;
      end return;
   end;

   function "*" (ALeftMatrix,ARightMatrix:FourDMatrix_Type) return FourDMatrix_Type is
   begin
      return Result:FourDMatrix_Type do
         for i in 0..3 loop
            for n in 0..3 loop
               Result(i,n) := ALeftMatrix(i,0)*ARightMatrix(0,n)
                 +ALeftMatrix(i,1)*ARightMatrix(1,n)
                 +ALeftMatrix(i,2)*ARightMatrix(2,n)
                 +ALeftMatrix(i,3)*ARightMatrix(3,n);
            end loop;
         end loop;
      end return;
   end;

   function "*" (ALeftMatrix,ARightMatrix:HomogenMatrix_Type) return HomogenMatrix_Type is
   begin
      return Result:HomogenMAtrix_Type do
         for i in 0..3 loop
            for n in 0..3 loop
               Result(i,n) := ALeftMatrix(i,0)*ARightMatrix(0,n)
                 +ALeftMatrix(i,1)*ARightMatrix(1,n)
                 +ALeftMatrix(i,2)*ARightMatrix(2,n)
                 +ALeftMatrix(i,3)*ARightMatrix(3,n);
            end loop;
         end loop;
      end return;
   end;

   function TranslateHomogenMatrix(ATransX,ATransY,ATransZ:GLFLoat_Type) return HomogenMatrix_Type is
   begin
      return Result:HomogenMatrix_Type do
         Result      := IdentityHomogenMatrix;
         Result(0,3) := ATransX;
         Result(1,3) := ATransY;
         Result(2,3) := ATransZ;
      end return;
   end;

end VectorMath;
