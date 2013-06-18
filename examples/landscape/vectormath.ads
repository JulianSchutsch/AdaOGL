with OpenGL; use OpenGL;

package VectorMath is

   ZeroBoundingBox : Exception;

   type HomogenVector_Type is array(0..3) of GLFLoat_Type;
   pragma Convention(C,HomogenVector_Type);
   type HomogenMatrix_Type is array(0..3,0..3) of aliased GLFloat_Type;
   pragma Convention(C,HomogenMatrix_Type);
   type FourDVector_Type is array(0..3) of GLFloat_Type;
   pragma Convention(C,FourDVector_Type);
   type FourDMatrix_Type is array(0..3,0..3) of GLFloat_Type;
   pragma Convention(C,FourDMatrix_Type);

   type HomogenVector_Array is array(Integer range <>) of HomogenVector_Type;

   type BoundingBox_Type is
      record
         MinX : GLFloat_Type;
         MaxX : GLFLoat_Type;
         MinY : GLFloat_Type;
         MaxY : GLFloat_Type;
         MinZ : GLFloat_Type;
         MaxZ : GLFloat_Type;
      end record;

   IdentityHomogenMatrix : constant HomogenMatrix_Type :=
      ((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0));

   function TranslateHomogenMatrix(ATransX,ATransY,ATransZ:GLFloat_Type) return HomogenMatrix_Type;
   function RotateXHomogenMatrix(ADegree:GLFloat_Type) return HomogenMatrix_Type;
   function RotateYHomogenMatrix(ADegree:GLFLoat_Type) return HomogenMatrix_Type;
   function RotateZHomogenMatrix(ADegree:GLFloat_Type) return HomogenMatrix_Type;
   function Transpose(AMatrix:HomogenMatrix_Type) return HomogenMatrix_Type;
   function Normalize3DHomogenVector(AVector:HomogenVector_Type) return HomogenVector_Type;
   function Length3DHomogenVector(AVector:HomogenVector_Type) return GLFLoat_Type;

   procedure CalculateMinimalBoundingBox(AVectors:HomogenVector_Array;ABox:out BoundingBox_Type);

   function "*" (ALeftMatrix,ARightMatrix:HomogenMatrix_Type) return HomogenMatrix_Type;
   function "*" (AMatrix:HomogenMatrix_Type;AVector:HomogenVector_Type) return HomogenVector_Type;
   function "*" (AMatrix:FourDMatrix_Type;AVector:FourDVector_Type) return FourDVector_Type;
   function "*" (AScalar:GLFloat_Type;AVector:HomogenVector_Type) return HomogenVector_Type;
   function "*" (ALeftVector,ARightVector:HomogenVector_Type) return GLFloat_Type;
   function "-" (ALeftVector,ARightVector:HomogenVector_Type) return HomogenVector_Type;
   function "+" (ALeftVector,ARightVector:HomogenVector_Type) return HomogenVector_Type;

   function "*" (ALeftMatrix,ARightMatrix:FourDMatrix_Type) return FourDMatrix_Type;
   function "*" (ALeftVector,ARightVector:FourDVector_Type) return GLFLoat_Type;

   procedure DebugHomogenVector(AVector:HomogenVector_Type);
   procedure DebugHomogenMatrix(AMatrix:HomogenMatrix_Type);

   function AssignHomogenVector(AValue1, AValue2, AValue3 : GLFloat_Type) return HomogenVector_Type;
   function Assign4DVector(AValue1,AValue2,AValue3,AValue4 : GLFLoat_Type) return FourDVector_Type;

end VectorMath;
