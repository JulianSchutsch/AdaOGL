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
--   Demonstration for setting up a GUI context and a window and
--   reacting to events:
--     * Closing of the context

pragma Ada_2005;

with GUI;
with GUI.Themes;
with GUI.UseImplementations;
with GUI.Console;

with YellowBlue;

with Config;
with ProcessLoop;
with Basics; use Basics;

procedure Console is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Console           : GUI.Console.Console_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;

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

   Console:=Theme.NewConsole(Context.BasisArea);
   Console.WriteLine(U("XXX"),16#FFFFFFFF#);
   Console.SetBounds
     (Top     => 0,
      Left    => 0,
      Height  => Context.BasisArea.GetBounds.Height,
      Width   => Context.BasisArea.GetBounds.Width,
      Visible => True);
   Console.SetAnchors
     (Top    => True,
      Left   => True,
      Right  => True,
      Bottom => True);

   -- Waiting until Cotext.OnClose is triggered
   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   -- All objects placed in the context are deleted also
   GUI.FreeContext(Context);

   -- Unregister all which has been registered
   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end Console;
