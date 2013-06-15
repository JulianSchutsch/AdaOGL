with GUI;
with GUI.Label;
with GUI.Themes;
with OpenGL; use OpenGL;

package CubeView is

   type LandscapeView_Type is new GUI.Object_Type with
      record
         RotateX : GLFloat_Type:=0.0;
         RotateY : GLFLoat_Type:=0.0;
         RotateZ : GLFloat_Type:=0.0;
         RotateSpeedX : GLFloat_Type:=0.0;
         RotateSpeedY : GLFloat_Type:=0.0;
         RotateSpeedZ : GLFloat_Type:=0.0;
         LabelX  : GUI.Label.Label_ClassAccess:=null;
         LabelY  : GUI.Label.Label_ClassAccess:=null;
         LabelZ  : GUI.Label.Label_ClassAccess:=null;
      end record;
   type LandscapeView_Access is access all LandscapeView_Type;

   overriding
   procedure RenderCustom
     (Object : access LandscapeView_Type);
   ---------------------------------------------------------------------------

   function NewCubeView
     (Parent : GUI.Object_ClassAccess;
      Theme  : GUI.Themes.Implementation_Type)
      return LandscapeView_Access;

end CubeView;
