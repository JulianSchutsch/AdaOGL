-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
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
--   28.Apr 2012 Julian Schutsch
--     - Original version

-- Demo
--   Demonstration for setting up a GUI context and a button and
--   reacting to two basic events:
--     * Closing of the context
--     * Click on a button

pragma Ada_2012;

with GUI;
with GUI.Window;
with GUI.Button;
with GUI.Label;
with GUI.Themes;
with GUI.UseImplementations;
with YellowBlue;
with Config;
with ProcessLoop;
with Basics; use Basics;
with OpenGL; use OpenGL;
with BoundsCalc; use BoundsCalc;
with Ada.Text_IO; use Ada.Text_IO;
with GUIDefinitions;

procedure Landscape is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Button            : GUI.Button.Button_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;

   Terminated        : Boolean:=False;
   pragma Warnings(Off,Terminated); -- Terminated is never changed
                                    -- from GNATs perspective

   procedure Click
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      Terminated:=True;
   end Click;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      Terminated:=True;
   end ContextClose;
   ---------------------------------------------------------------------------

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
                 GLint_Type(Context.BasisArea.AbsBounds.AbsHeight - (AbsBounds.AbsTop - AbsBounds.AbsSubTop) - Bounds.Height),
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

   procedure DeleteWindowClick
     (Window : GUI.Object_ClassAccess) is
   begin
      Window.Free;
   end DeleteWindowClick;

   procedure CloseWindowClick
     (CallBackObject : AnyObject_ClassAccess) is

      Window : GUI.Window.Window_ClassAccess:=GUI.Window.Window_ClassAccess(CallBackObject);

   begin
      Window.AddASync(DeleteWindowClick'Unrestricted_Access);
   end CloseWindowClick;
   ---------------------------------------------------------------------------

   procedure NewWindowClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);

      View    : access LandscapeView_Type;
      Window  : GUI.Window.Window_ClassAccess;
      Button  : GUI.Button.Button_ClassAccess;
      WBounds : Bounds_Type;

   begin

      Window:=Theme.NewWindow(Context.BasisArea);
      Window.CallBackObject:=AnyObject_ClassAccess(Window);
      Window.OnCloseWindow:=CloseWindowClick'Unrestricted_Access;
      Window.SetCaption(U("Cube"));
      Window.SetBounds
        (Top     => 20,
         Left    => 20,
         Height  => 200,
         Width   => 320,
         Visible => True);
      Window.SetButtons((GUI.Window.WindowButtonClose=>true));
      WBounds:=Window.GetClientBounds;

      View:=new LandscapeView_Type;
      View.Render:=GUIDefinitions.RenderCustom;
      View.Initialize(Parent => GUI.Object_ClassAccess(Window));
      View.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => WBounds.Height,
         Width   => WBounds.Width,
         Visible => True);
      View.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(Window));
      Button.SetCaption(U("+"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=IncreaseRotX'Unrestricted_Access;
      Button.SetBounds
        (Top    => 5,
         Left   => WBounds.Width-25,
         Height => 20,
         Width  => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(Window));
      Button.SetCaption(U("-"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=DecreaseRotX'Unrestricted_Access;
      Button.SetBounds
        (Top => 5,
         Left => WBounds.Width-50,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      View.LabelX:=Theme.NewLabel(GUI.Object_ClassAccess(Window));
      View.LabelX.SetBounds
        (Top => 5,
         Left => WBounds.Width-175,
         Height => 20,
         Width  => 100,
         Visible => True);
      View.LabelX.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(Window));
      Button.SetCaption(U("+"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=IncreaseRotY'Unrestricted_Access;
      Button.SetBounds
        (Top => 30,
         Left => WBounds.Width-25,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(Window));
      Button.SetCaption(U("-"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=DecreaseRotY'Unrestricted_Access;
      Button.SetBounds
        (Top => 30,
         Left => WBounds.Width-50,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      View.LabelY:=Theme.NewLabel(GUI.Object_ClassAccess(Window));
      View.LabelY.SetBounds
        (Top => 30,
         Left => WBounds.Width-175,
         Height => 20,
         Width => 100,
         Visible => True);
      View.LabelY.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(Window));
      Button.SetCaption(U("+"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=IncreaseRotZ'Unrestricted_Access;
      Button.SetBounds
        (Top => 55,
         Left => WBounds.Width-25,
         Height => 20,
         Width  => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      Button:=Theme.NewButton(GUI.Object_ClassAccess(Window));
      Button.SetCaption(U("-"));
      Button.CallBackObject:=AnyObject_ClassAccess(View);
      Button.OnClick:=DecreaseRotZ'Unrestricted_Access;
      Button.SetBounds
        (Top => 55,
         Left => WBounds.Width-50,
         Height => 20,
         Width => 20,
         Visible => True);
      Button.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

      View.LabelZ:=Theme.NewLabel(GUI.Object_ClassAccess(Window));
      View.LabelZ.SetBounds
        (Top => 55,
         Left => WBounds.Width-175,
         Height => 20,
         Width => 100,
         Visible => True);
      View.LabelZ.SetAnchors
        (Top => True,
         Left => False,
         Right => True,
         Bottom => False);

   end NewWindowClick;
   ---------------------------------------------------------------------------

begin

   GUI.UseImplementations.Register;

   YellowBlue.Register;

   GUIImplementation := GUI.Implementations.FindAny;

   Theme             := GUI.Themes.Implementations.FindAny;

   Config.Insert
     (Container => Configuration,
      Key       => U("Window.Title"),
      New_Item  => U("Cubes"));

   Context:=GUIImplementation.NewContext
     (Configuration => Configuration,
      Node          => U("Window"));

   Context.OnClose:=ContextClose'Unrestricted_Access;

   Button:=Theme.NewButton(Context.BasisArea);
   Button.SetCaption(U("New Window"));
   Button.SetBounds
     (Top     => Context.BasisArea.GetBounds.Height-40,
      Left    => 0,
      Height  => 40,
      Width   => 200,
      Visible => True);
   Button.SetAnchors
     (Top    => False,
      Left   => True,
      Right  => False,
      Bottom => True);
   Button.OnClick := NewWindowClick'Unrestricted_Access;

   NewWindowClick(null);

   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   GUI.FreeContext(Context);

   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end Landscape;
