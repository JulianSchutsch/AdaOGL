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

with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with GUI.Button;
with Ada.Text_IO; use Ada.Text_IO;

with SimClientGUI.CreateMenu;

package body SimClientGUI.MainMenu is

   ButtonCreateGame : GUI.Button.Button_ClassAccess:=null;
   ButtonJoinGame   : GUI.Button.Button_ClassAccess:=null;
   ButtonExit       : GUI.Button.Button_ClassAccess:=null;

   ButtonHeight : constant := 50;
   ButtonWidth  : constant := 200;
   ButtonSpace  : constant := 10;
   MenuHeight   : constant := 3*(ButtonHeight+ButtonSpace)-ButtonSpace;

   Enabled : Boolean:=False;

   procedure ASyncExit
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      Disable;
      Terminated:=True;
   end ASyncExit;
   ---------------------------------------------------------------------------

   procedure ButtonExitClick
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

   begin
      ButtonExit.AddASync(ASyncExit'Access);
   end ButtonExitClick;
   ---------------------------------------------------------------------------

   procedure ASyncGameCreate
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      Disable;
      SimClientGUI.CreateMenu.Enable;
   end ASyncGameCreate;
   ---------------------------------------------------------------------------

   procedure ButtonCreateGameClick
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

   begin
      ButtonCreateGame.AddASync(ASyncGameCreate'Access);
   end ButtonCreateGameClick;
   ---------------------------------------------------------------------------

   procedure ResizeWindowArea
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

      WindowBounds : constant Bounds_Type:=GUIContext.BasisArea.GetBounds;

      Top    : constant Integer := (WindowBounds.Height-MenuHeight)/2-1;
      Left   : constant Integer := (WindowBounds.Width-200)/2-1;

   begin
      ButtonCreateGame.SetBounds
        (Top     => Top,
         Left    => Left,
         Height  => ButtonHeight,
         Width   => ButtonWidth,
         Visible => True);
      ButtonJoinGame.SetBounds
        (Top     => Top+ButtonHeight+ButtonSpace,
         Left    => Left,
         Height  => ButtonHeight,
         Width   => ButtonWidth,
         Visible => True);
      ButtonExit.SetBounds
        (Top     => Top+2*(ButtonHeight+ButtonSpace),
         Left    => Left,
         Height  => ButtonHeight,
         Width   => ButtonWidth,
         Visible => True);
   end ResizeWindowArea;
   ---------------------------------------------------------------------------

   procedure Enable is
   begin
      if Enabled then
         raise ReenabledGUIModule with "MainMenu";
      end if;
      ButtonCreateGame := ThemeImplementation.NewButton(GUIContext.BasisArea);
      ButtonCreateGame.SetCaption(U("Create Game"));
      ButtonCreateGame.OnClick:=ButtonCreateGameClick'Access;

      ButtonJoinGame   := ThemeImplementation.NewButton(GUIContext.BasisArea);
      ButtonJoinGame.SetCaption(U("Join Game"));

      ButtonExit       := ThemeImplementation.NewButton(GUIContext.BasisArea);
      ButtonExit.SetCaption(U("Exit"));
      ButtonExit.OnClick:=ButtonExitClick'Access;

      ResizeWindowArea(null);
      GUIContext.BasisArea.OnResize:=ResizeWindowArea'Access;
      Enabled := True;
   end Enable;

   procedure Disable is
   begin
      if not Enabled then
         raise RedisabledGUIModule with "MainMenu";
      end if;
      GUIContext.BasisArea.OnResize:=null;
      ButtonCreateGame.Free;
      ButtonJoinGame.Free;
      ButtonExit.Free;
      Enabled:=False;
      New_Line;
   end Disable;

end SimClientGUI.MainMenu;
