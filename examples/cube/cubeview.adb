with Basics; use Basics;
with GUI.Button;
with BoundsCalc; use BoundsCalc;
with GUIDefinitions;

package body CubeView is

   procedure UpdateViewCaptions
     (View : access LandscapeView_Type) is
      type Vis_Type is delta 0.1 range -10000.0..+10000.0;
   begin
      View.LabelX.SetCaption(U("rotX:"&Vis_Type'Image(Vis_Type(View.RotateX))));
      View.LabelY.SetCaption(U("rotY:"&Vis_Type'Image(Vis_Type(View.RotateY))));
      View.LabelZ.SetCaption(U("rotZ:"&Vis_Type'Image(Vis_Type(view.RotateZ))));
   end UpdateViewCaptions;
   ---------------------------------------------------------------------------

   procedure RenderCustom
     (Object : access LandscapeView_Type) is

      AbsBounds   : AbsBounds_Type renames Object.AbsBounds;
      Bounds      : Bounds_Type:=Object.GetBounds;
      AspectRatio : GLDouble_Type;

   begin
      if not AbsBounds.AbsVisible then
         return;
      end if;
      glViewPort(GLint_Type(AbsBounds.AbsLeft - AbsBounds.AbsSubLeft),
                 GLint_Type(Object.GetContextArea.AbsBounds.AbsHeight - (AbsBounds.AbsTop - AbsBounds.AbsSubTop) - Bounds.Height),
                 GLsizei_Type(Bounds.Width),
                 GLsizei_Type(Bounds.Height));
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;

      if Bounds.Height/=0 then
         AspectRatio:=GLDouble_Type(Bounds.Width)/GLDouble_Type(Bounds.Height);
      else
         AspectRatio:=1.0;
      end if;

      glFrustum(-AspectRatio, AspectRatio, -1.0, 1.0, 1.0, 200.0);
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glClear(GL_DEPTH_BUFFER_BIT);
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      declare
         Position : GLfloat_Array(0..3):=(-1.0,0.0,0.0,1.0);
      begin
         glLightfv(GL_LIGHT0,GL_POSITION,Position(0)'Access);
      end;

      glEnable(GL_COLOR_MATERIAL);

      Object.RotateX:=Object.RotateX+Object.RotateSpeedX;
      Object.RotateY:=Object.RotateY+Object.RotateSpeedY;
      Object.RotateZ:=Object.RotateZ+Object.RotateSpeedZ;
      if Object.RotateX>=360.0 then
         Object.RotateX:=Object.RotateX-360.0;
      end if;
      if Object.RotateX<0.0 then
         Object.RotateX:=Object.RotateX+360.0;
      end if;
      if Object.RotateY>=360.0 then
         Object.RotateY:=Object.RotateY-360.0;
      end if;
      if Object.RotateY<0.0 then
         Object.RotateY:=Object.RotateY+360.0;
      end if;
      if Object.RotateZ>=360.0 then
         Object.RotateZ:=Object.RotateZ-360.0;
      end if;
      if Object.RotateZ<0.0 then
         Object.RotateZ:=Object.RotateZ+360.0;
      end if;
      UpdateViewCaptions(Object);

      glTranslatef(0.0,0.0,-4.0);
      glRotatef(Object.RotateX,1.0,0.0,0.0);
      glRotatef(Object.RotateY,0.0,1.0,0.0);
      glRotatef(Object.RotateZ,0.0,0.0,1.0);

      glBegin(GL_QUADS);
      glColor4f(1.0,0.0,0.0,1.0);
      glNormal3f(0.0,0.0,1.0);
      glVertex3f(-1.0,-1.0,1.0);
      glVertex3f(-1.0,1.0,1.0);
      glVertex3f(1.0,1.0,1.0);
      glVertex3f(1.0,-1.0,1.0);

      glNormal3f(0.0,0.0,-1.0);
      glVertex3f(-1.0,-1.0,-1.0);
      glVertex3f(-1.0,1.0,-1.0);
      glVertex3f(1.0,1.0,-1.0);
      glVertex3f(1.0,-1.0,-1.0);

      glNormal3f(-1.0,0.0,0.0);
      glVertex3f(-1.0,-1.0,1.0);
      glVertex3f(-1.0,-1.0,-1.0);
      glVertex3f(-1.0,1.0,-1.0);
      glVertex3f(-1.0,1.0,1.0);

      glNormal3f(1.0,0.0,0.0);
      glVertex3f(1.0,-1.0,1.0);
      glVertex3f(1.0,-1.0,-1.0);
      glVertex3f(1.0,1.0,-1.0);
      glVertex3f(1.0,1.0,1.0);

      glNormal3f(0.0,1.0,0.0);
      glVertex3f(-1.0,1.0,1.0);
      glVertex3f(1.0,1.0,1.0);
      glVertex3f(1.0,1.0,-1.0);
      glVertex3f(-1.0,1.0,-1.0);

      glNormal3f(0.0,-1.0,0.0);
      glVertex3f(-1.0,-1.0,1.0);
      glVertex3f(1.0,-1.0,1.0);
      glVertex3f(1.0,-1.0,-1.0);
      glVertex3f(-1.0,-1.0,-1.0);
      glend;
      glDisable(GL_LIGHTING);
   end RenderCustom;
   ---------------------------------------------------------------------------

   procedure IncreaseRotX
     (CallBackObject: AnyObject_ClassAccess) is
      View : LandscapeView_Access:=LandscapeView_Access(CallBackObject);
   begin
      View.RotateSpeedX:=View.RotateSpeedX+0.1;
   end IncreaseRotX;
   ---------------------------------------------------------------------------

   procedure DecreaseRotX
     (CallBackObject: AnyObject_ClassAccess) is
      View : LandscapeView_Access:=LandscapeView_Access(CallBackObject);
   begin
      View.RotateSpeedX:=View.RotateSpeedX-0.1;
   end DecreaseRotX;
   ---------------------------------------------------------------------------

   procedure IncreaseRotY
     (CallBackObject: AnyObject_ClassAccess) is
      View : LandscapeView_Access:=LandscapeView_Access(CallBackObject);
   begin
      View.RotateSpeedY:=View.RotateSpeedY+0.1;
   end IncreaseRotY;
   ---------------------------------------------------------------------------

   procedure DecreaseRotY
     (CallBackObject: AnyObject_ClassAccess) is
      View : LandscapeView_Access:=LandscapeView_Access(CallBackObject);
   begin
      View.RotateSpeedY:=View.RotateSpeedY-0.1;
   end DecreaseRotY;
   ---------------------------------------------------------------------------

   procedure IncreaseRotZ
     (CallBackObject: AnyObject_ClassAccess) is
      View : LandscapeView_Access:=LandscapeView_Access(CallBackObject);
   begin
      View.RotateSpeedZ:=View.RotateSpeedZ+0.1;
   end IncreaseRotZ;
   ---------------------------------------------------------------------------

   procedure DecreaseRotZ
     (CallBackObject: AnyObject_ClassAccess) is
      View : LandscapeView_Access:=LandscapeView_Access(CallBackObject);
   begin
      View.RotateSpeedZ:=View.RotateSpeedZ-0.1;
   end DecreaseRotZ;
   ---------------------------------------------------------------------------

   function NewCubeView
     (Parent : GUI.Object_ClassAccess;
      Theme  : GUI.Themes.Implementation_Type)
      return LandscapeView_Access is

      View    : LandscapeView_Access;
      Button  : GUI.Button.Button_ClassAccess;

   begin
      View        := new LandscapeView_Type;
      View.Render := GUIDefinitions.RenderCustom;
      View.Initialize(Parent => Parent);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(View));
      Button.SetCaption(U("+"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=IncreaseRotX'Unrestricted_Access;
      Button.SetBounds
        (Top    => 5,
         Left   => -25,
         Height => 20,
         Width  => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(View));
      Button.SetCaption(U("-"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=DecreaseRotX'Unrestricted_Access;
      Button.SetBounds
        (Top => 5,
         Left => -50,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      View.LabelX:=Theme.NewLabel(GUI.Object_ClassAccess(View));
      View.LabelX.SetBounds
        (Top => 5,
         Left => -175,
         Height => 20,
         Width  => 100,
         Visible => True);
      View.LabelX.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(View));
      Button.SetCaption(U("+"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=IncreaseRotY'Unrestricted_Access;
      Button.SetBounds
        (Top => 30,
         Left => -25,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(View));
      Button.SetCaption(U("-"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=DecreaseRotY'Unrestricted_Access;
      Button.SetBounds
        (Top => 30,
         Left => -50,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      View.LabelY:=Theme.NewLabel(GUI.Object_ClassAccess(View));
      View.LabelY.SetBounds
        (Top => 30,
         Left => -175,
         Height => 20,
         Width => 100,
         Visible => True);
      View.LabelY.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(View));
      Button.SetCaption(U("+"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=IncreaseRotZ'Unrestricted_Access;
      Button.SetBounds
        (Top => 55,
         Left => -25,
         Height => 20,
         Width  => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(View));
      Button.SetCaption(U("-"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=DecreaseRotZ'Unrestricted_Access;
      Button.SetBounds
        (Top => 55,
         Left => -50,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      View.LabelZ:=Theme.NewLabel(GUI.Object_ClassAccess(View));
      View.LabelZ.SetBounds
        (Top => 55,
         Left => -175,
         Height => 20,
         Width => 100,
         Visible => True);
      View.LabelZ.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      return LandscapeView_Access(View);

   end NewCubeView;

end CubeView;
