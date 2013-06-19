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
with GUI.Themes;
with GUI.UseImplementations;
with GUI.Console;
with YellowBlue;
with Config;
with ProcessLoop;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Canvas;

with LandscapeView;

procedure Landscape is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Button            : GUI.Button.Button_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;
   View              : LandscapeView.LandscapeView_Access;

   Terminated        : Boolean:=False;
   pragma Warnings(Off,Terminated); -- Terminated is never changed
                                    -- from GNATs perspective

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

      Window : constant GUI.Window.Window_ClassAccess:=GUI.Window.Window_ClassAccess(CallBackObject);

   begin
      Window.AddASync(DeleteWindowClick'Unrestricted_Access);
   end CloseWindowClick;
   ---------------------------------------------------------------------------

   procedure NewWindowClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);

      Content : GUI.Console.Console_ClassAccess;
      Window  : GUI.Window.Window_ClassAccess;
      WBounds : Bounds_Type;

      TextColor : constant Canvas.Color_Type:=16#FFFFFFFF#;

   begin

      Window:=Theme.NewWindow(Context.BasisArea);
      Window.CallBackObject:=AnyObject_ClassAccess(Window);
      Window.OnCloseWindow:=CloseWindowClick'Unrestricted_Access;
      Window.SetCaption(U("Help"));
      Window.SetBounds
        (Top     => 10,
         Left    => 10,
         Height  => 600,
         Width   => 800,
         Visible => True);
      Window.SetButtons((GUI.Window.WindowButtonClose=>true));

      WBounds:=Window.GetClientBounds;

      Content:=Theme.NewConsole(GUI.Object_ClassAccess(Window));

      Content.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => WBounds.Height,
         Width   => WBounds.Width,
         Visible => True);
      Content.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);

      Content.WriteLine(U("Bicubic landscape demo"),TextColor);
      Content.WriteLine(U(""),TextColor);
      Content.WriteLine(U("Mouse buttons:"),TextColor);
      Content.WriteLine(U(" Left  : Select area"),TextColor);
      Content.WriteLine(U(" Right : Move view"),TextColor);
      Content.WriteLine(U(""),TextColor);
      Content.WriteLine(U("Keys:"),TextColor);
      Content.WriteLine(U(" d : Rotate view anti clockwise"),TextColor);
      Content.WriteLine(U(" s : Rotate view clockwise"),TextColor);
      Content.WriteLine(U(" + : Zoom in"),TextColor);
      Content.WriteLine(U(" - : Zoom out"),TextColor);
      Content.WriteLine(U(" f : Produce flat terrain, level 8.0"),TextColor);
      Content.WriteLine(U(" n : Produce flat terrain, level 0.0"),TextColor);
      Content.WriteLine(U(" l : Average terrain"),TextColor);
      Content.WriteLine(U(" k : Smooth terrain"),TextColor);
      Content.WriteLine(U(" 1 : Add sin with k=(1/5,1/5) and amplitude of 0.1"),TextColor);
      Content.WriteLine(U(" 2 : Add sin with k=(1/5,0) and amplitude of 0.1"),TextColor);
      Content.WriteLine(U(" 3 : Add sin with k=(0,1/5) and amplitude of 0.1"),TextColor);
      Content.WriteLine(U(" Page Up   : Move terrain up by 0.1"),TextColor);
      Content.WriteLine(U(" Page Down : Move terrain down by 0.1"),TextColor);

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

   Button:=Theme.NewButton(Context.WindowArea);
   Button.SetCaption(U("Help"));
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

   declare
      WBounds : constant BoundsCalc.Bounds_Type:=Context.BasisArea.GetClientBounds;
   begin

      View:=LandscapeView.NewLandscapeView
        (Parent => GUI.Object_CLassAccess(Context.BasisArea),
         Theme => Theme);
      View.SetBounds
        (Top => 0,
         Left => 0,
         Height => WBounds.Height,
         Width => WBounds.Width,
         Visible => True);
      View.SetAnchors
        (Top => True,
         Left => True,
         Right => True,
         Bottom => True);
   end;

   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   GUI.FreeContext(Context);

   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end Landscape;
