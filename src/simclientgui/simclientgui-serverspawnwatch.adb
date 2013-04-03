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

pragma Ada_2005;

with GUI;
with GUI.Window;
with GUI.Console;
with SimClient.CreateServer;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BoundsCalc; use BoundsCalc;
with Basics; use Basics;

package body SimClientGUI.ServerSpawnWatch is

   Window  : GUI.Window.Window_ClassAccess;
   Enabled : Boolean:=False;
   Console : GUI.Console.Console_ClassAccess;

   procedure CreateServerMessage
     (Message : Unbounded_String) is
   begin
      Console.WriteLine(Message,16#FFFFFFFF#);
   end CreateServerMessage;

   procedure Enable is
   begin
      if Enabled then
         return;
      end if;
      Window:=ThemeImplementation.NewWindow(GUIContext.WindowArea);
      declare
         Bounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;
      begin
         Window.SetBounds
           (Top     => Bounds.Height-240,
            Left    => 0,
            Height  => 240,
            Width   => 320,
            Visible => True);
      end;
      Window.SetCaption(U("Output for Server spawn"));
      Console:=ThemeImplementation.NewConsole(GUI.Object_ClassAccess(Window));
      declare
         Bounds : constant Bounds_Type:=Window.GetClientBounds;
      begin
         Console.SetBounds
           (Top     => 0,
            Left    => 0,
            Height  => Bounds.Height,
            Width   => Bounds.Width,
            Visible => True);
         Console.SetAnchors
           (Top    => True,
            Left   => True,
            Right  => True,
            Bottom => True);
      end;
      SimClient.CreateServer.OnMessage := CreateServerMessage'Access;
      Enabled:=True;
   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable is
   begin
      if not Enabled then
         return;
      end if;
      SimClient.CreateServer.OnMessage := null;
      Window.Free;
      Enabled:=False;
   end Disable;
   ---------------------------------------------------------------------------

end SimClientGUI.ServerSpawnWatch;
