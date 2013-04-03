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
--   2.Mai 2012 Julian Schutsch
--     - Original version

-- Demo
--   Demonstration for a tabs-control.

pragma Ada_2005;

with GUI;
with GUI.Themes;
with GUI.Button;
with GUI.TabControl;
with GUI.UseImplementations;

with YellowBlue;

with Config;
with ProcessLoop;
with Basics; use Basics;

with ExceptionOutput;

procedure Tabs is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;
   TabControl        : GUI.TabControl.TabControl_ClassAccess;
   Tabs              : array(1..10) of GUI.TabControl.Tab_ClassAccess;
   TempButton        : GUI.Button.Button_ClassAccess;

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

begin

   -- GUI.UseImplementations.Register adds all supported implementations
   -- on this plattform to GUI.Implementations.
   -- The GUI.UseImplementations package was created by /buildcfg/configprog
   GUI.UseImplementations.Register;

   -- This is the default theme, now added to GUI.Themes.Implementations
   YellowBlue.Register;

   -- Now select any available GUI implementation
   GUIImplementation := GUI.Implementations.FindAny;

   -- And the only theme we added to GUI.Themes.Implementations
   Theme             := GUI.Themes.Implementations.FindAny;

   -- Often when creating a new context (main window), one may wish
   -- to specify further data in Configuration.
   Context:=GUIImplementation.NewContext
     (Configuration => Configuration,
      Node          => U(""));

   -- Called when the main window's close button is clicked
   Context.OnClose:=ContextClose'Unrestricted_Access;

   -- Create a tab-control in the window area
   TabControl:=Theme.NewTabControl(Context.BasisArea);

   for i in Tabs'Range loop
      Tabs(i):=TabControl.NewTab(U("Tab Nr."&Integer'Image(i)));
      TempButton:=Theme.NewButton(GUI.Object_ClassAccess(Tabs(i)));
      TempButton.SetBounds
        (Top     => 10,
         Left    => 10,
         Height  => 30,
         Width   => 200,
         Visible => True);
      TempButton.SetCaption(U("Button in Tab Nr."&Integer'Image(i)));
   end loop;

   TabControl.SetBounds
     (Top     => 10,
      Left    => 10,
      Height  => Context.BasisArea.GetBounds.Height-20,
      Width   => Context.BasisArea.GetBounds.Width-20,
      Visible => True);
   TabControl.SetAnchors
     (Top     => True,
      Left    => True,
      Right   => True,
      Bottom  => True);

   -- Waiting until either Context.OnClose is triggered
   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   -- All objects placed in the context are deleted also
   GUI.FreeContext(Context);

   -- Unregister all which has been registered
   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

exception
   when E:Others =>
      ExceptionOutput.Put(E);

end Tabs;
