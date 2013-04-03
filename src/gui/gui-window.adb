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

package body GUI.Window is

   procedure SetCaption
     (Window  : access Window_Type;
      Caption : Unbounded_String) is
   begin
      Window.Caption:=Caption;
   end SetCaption;
   ---------------------------------------------------------------------------

   procedure SetButtons
     (Window  : access Window_Type;
      Buttons : WindowButtons_Set) is
   begin
      Window.Buttons:=Buttons;
   end SetButtons;
   ---------------------------------------------------------------------------

   function GetButtons
     (Window : access Window_Type)
      return WindowButtons_Set is
   begin
      return Window.Buttons;
   end GetButtons;
   ---------------------------------------------------------------------------

   function GetCaption
     (Window : access Window_Type)
      return Unbounded_String is
   begin
      return Window.Caption;
   end GetCaption;
   ---------------------------------------------------------------------------

   procedure Focus
     (Item : access Window_Type) is
   begin
      GUI.BringToFront(Object_ClassAccess(Item));
   end Focus;
   ---------------------------------------------------------------------------

   procedure StartChange
     (Window : access Window_Type;
      RefX   : Integer;
      RefY   : Integer;
      Mode   : WindowChangeMode_Enum) is
   begin
      Window.RefX      := RefX;
      Window.RefY      := RefY;
      Window.RefHeight := Window.Bounds.Height;
      Window.RefWidth  := Window.Bounds.Width;
      Window.Mode:=Mode;
   end StartChange;
   ---------------------------------------------------------------------------

   procedure ApplyChange
     (Window : access Window_Type;
      RefX   : Integer;
      RefY   : Integer) is

      Bounds : Bounds_Type renames Window.Bounds;

   begin
      case Window.Mode is
         when WindowChangeModeNothing =>
            null;
         when WindowChangeModeMove =>
            Window.SetBounds
              (Top     => Bounds.Top+RefY-Window.RefY,
               Left    => Bounds.Left+RefX-Window.RefX,
               Height  => Bounds.Height,
               Width   => Bounds.Width,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeTopLeft =>
            Window.SetBounds
              (Top     => Bounds.Top+RefY-Window.RefY,
               Left    => Bounds.Left+RefX-Window.RefX,
               Height  => Bounds.Height-RefY+Window.RefY,
               Width   => Bounds.Width-RefX+Window.RefX,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeTop =>
            Window.SetBounds
              (Top     => Bounds.Top+RefY-Window.RefY,
               Left    => Bounds.Left,
               Height  => Bounds.Height-RefY+Window.RefY,
               Width   => Bounds.Width,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeTopRight =>
            Window.SetBounds
              (Top     => Bounds.Top+RefY-Window.RefY,
               Left    => Bounds.Left,
               Height  => Bounds.Height-RefY+Window.RefY,
               Width   => Window.RefWidth+RefX-Window.RefX,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeLeft =>
            Window.SetBounds
              (Top     => Bounds.Top,
               Left    => Bounds.Left+RefX-Window.RefX,
               Height  => Bounds.Height,
               Width   => Bounds.Width-RefX+Window.RefX,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeRight =>
            Window.SetBounds
              (Top     => Bounds.Top,
               Left    => Bounds.Left,
               Height  => Bounds.Height,
               Width   => Window.RefWidth+RefX-Window.RefX,
                Visible => Bounds.Visible);
         when WindowChangeModeSizeBottomLeft =>
            Window.SetBounds
              (Top     => Bounds.Top,
               Left    => Bounds.Left+RefX-Window.RefX,
               Height  => Window.RefHeight+RefY-Window.RefY,
               Width   => Bounds.Width-RefX+Window.RefX,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeBottom =>
            Window.SetBounds
              (Top     => Bounds.Top,
               Left    => Bounds.Left,
               Height  => Window.RefHeight+RefY-Window.RefY,
               Width   => Bounds.Width,
               Visible => Bounds.Visible);
         when WindowChangeModeSizeBottomRight =>
            Window.SetBounds
              (Top     => Bounds.Top,
               Left    => Bounds.Left,
               Height  => Window.RefHeight+RefY-Window.RefY,
               Width   => Window.RefWidth+RefX-Window.RefX,
               Visible => Bounds.Visible);
      end case;

   end ApplyChange;
   ---------------------------------------------------------------------------

   procedure StopChange
     (Window : access Window_Type) is
   begin
      Window.Mode:=WindowChangeModeNothing;
   end StopChange;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item    : access Window_Type;
      Parent  : Object_ClassAccess) is
   begin

      GUI.Initialize
        (Item   => Object_Access(Item),
         Parent => Parent);

      Item.FocusStyle:=FocusStyleContainer;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Window_Type) is
   begin

      GUI.Free
        (Item => Object_Access(Item));

   end Free;
   ---------------------------------------------------------------------------

end GUI.Window;
