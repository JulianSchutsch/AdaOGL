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
--   26.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.Window is

   type WindowChangeMode_Enum is
     (WindowChangeModeNothing,
      WindowChangeModeMove,
      WindowChangeModeSizeTopLeft,
      WindowChangeModeSizeTop,
      WindowChangeModeSizeTopRight,
      WindowChangeModeSizeLeft,
      WindowChangeModeSizeRight,
      WindowChangeModeSizeBottomLeft,
      WindowChangeModeSizeBottom,
      WindowChangeModeSizeBottomRight);

   type WindowButtons_Enum is
     (WindowButtonClose);
   type WindowButtons_Set is array(WindowButtons_Enum) of Boolean;

   type OnCloseWindow_Access is
     access procedure(CallBackObject : AnyObject_ClassAccess);

   type Window_Public is new Object_Type with
      record
         OnCloseWindow : OnCloseWindow_Access:=null;
      end record;

   type Window_Type is new Window_Public with private;
   type Window_Access is access all Window_Type;
   type Window_ClassAccess is access all Window_Type'Class;

   type Window_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return Window_ClassAccess;

   procedure Initialize
     (Item    : access Window_Type;
      Parent  : Object_ClassAccess);

   overriding
   procedure Free
     (Item : access Window_Type);

   overriding
   procedure Focus
     (Item : access Window_Type);

   procedure StartChange
     (Window : access Window_Type;
      Refx   : Integer;
      Refy   : Integer;
      Mode   : WindowChangeMode_Enum);

   procedure ApplyChange
     (Window : access Window_Type;
      RefX   : Integer;
      RefY   : Integer);

   procedure StopChange
     (Window : access Window_Type);

   procedure SetCaption
     (Window  : access Window_Type;
      Caption : Unbounded_String);

   function GetCaption
     (Window : access Window_Type)
      return Unbounded_String;

   procedure SetButtons
     (Window  : access Window_Type;
      Buttons : WindowButtons_Set);

   function GetButtons
     (Window : access Window_Type)
      return WindowButtons_Set;

private

   type Window_Type is new Window_Public with
      record
         RefX       : Integer;
         RefY       : Integer;
         RefHeight  : Integer;
         RefWidth   : Integer;
         Mode       : WindowChangeMode_Enum:=WindowChangeModeNothing;
         Caption    : Unbounded_String;
         Buttons    : WindowButtons_Set:=(others=>False);
      end record;

end GUI.Window;
