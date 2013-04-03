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

with GUI.Window;
with GUI.Console;

with Logging;
with Logging.Server;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body SimClientGUI.Logging is

   Enabled : Boolean:=False;
   Window  : GUI.Window.Window_ClassAccess;
   Console : GUI.Console.Console_ClassAccess;

   procedure LogEvent
     (Source  : StringStringMap_Pack.Map;
      Level   : Standard.Logging.Level_Enum;
      Module  : Unbounded_String;
      Channel : Unbounded_String;
      Message : Unbounded_String) is
      pragma Unreferenced(Level);
      pragma Unreferenced(Source);
   begin
      Console.WriteLine("["&Module&":"&Channel&"]:"&Message,16#FFFFFFFF#);
   end LogEvent;
   ---------------------------------------------------------------------------

   procedure Enable
     (Configuration : Config.Config_Type) is

      use type StringStringMap_Pack.Cursor;

      Cursor : StringStringMap_Pack.Cursor;

   begin
      if Enabled then
         return;
      end if;
      Cursor:=Configuration.Find(U("SimClientGUI.Logging"));
      if (Cursor/=StringStringMap_Pack.No_Element) then
         if StringStringMap_Pack.Element(Cursor)="On" then
            declare
               Bounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;
               ClientBounds : Bounds_Type;
            begin
               Window:=ThemeImplementation.NewWindow(GUIContext.WindowArea);
               Window.SetBounds
                 (Top     => Bounds.Height-240,
                  Left    => Bounds.Width-320,
                  Height  => 240,
                  Width   => 320,
                  Visible => True);
               Window.SetCaption(U("Logging"));
               Console:=ThemeImplementation.NewConsole(GUI.Object_ClassAccess(Window));
               ClientBounds:=Window.GetClientBounds;
               Console.SetBounds
                 (Top     => 0,
                  Left    => 0,
                  Height  => ClientBounds.Height,
                  Width   => ClientBounds.Width,
                  Visible => True);
               Console.SetAnchors
                 (Top    => True,
                  Left   => True,
                  Right  => True,
                  Bottom => True);
            end;
            Standard.Logging.Server.OnLogEvent:=LogEvent'Access;
            Standard.Logging.Server.Initialize(Configuration);
            Enabled:=True;
            Put_Line("Logging Server enabled, should be visible now");
         elsif StringStringMap_Pack.Element(Cursor)/="Off" then
            raise InvalidOption with "SimClientGUI.Logging must be either Yes or No";
         end if;
      end if;
   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable is
   begin
      if not Enabled then
         return;
      end if;
      Standard.Logging.Server.OnLogevent:=null;
      Standard.Logging.Server.Finalize;
      Window.Free;
      Enabled:=False;
   end Disable;

end SimClientGUI.Logging;
