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

with CubeView;

procedure Cube is

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

   procedure DeleteWindowClick
     (Window : GUI.Object_ClassAccess) is
   begin
      Window.Free;
   end DeleteWindowClick;
   ---------------------------------------------------------------------------

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

      View    : CubeView.LandscapeView_Access;
      Window  : GUI.Window.Window_ClassAccess;
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

      View:=CubeView.NewCubeView
        (Parent => GUI.Object_CLassAccess(Window),
         Theme  => Theme);
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

end Cube;
