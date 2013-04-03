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
--   26.Apr 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   This is an abstract foundation for a combobox type.
--   It implements a list of entries and an index.

pragma Ada_2005;

with Canvas;

with GUI.Basics; use GUI.Basics;

package GUI.ComboBox is

   IndexOutOfRange : Exception;

   type OnSelect_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type ComboBox_Public is new Object_Type with
      record
         OnSelect : OnSelect_Access:=null;
      end record;

   type ComboBox_Type is new ComboBox_Public with private;
   type ComboBox_Access is access all Combobox_Type;
   type ComboBox_ClassAccess is access all Combobox_Type'Class;

   overriding
   procedure Free
     (Item : access ComboBox_Type);

   overriding
   procedure Initialize
     (Item   : access ComboBox_Type;
      Parent : Object_ClassAccess);

   procedure SetIndex
     (Item  : access ComboBox_Type;
      Index : Integer);

   function GetIndex
     (Item : access ComboBox_Type)
      return Integer;

   procedure AddEntry
     (Item   : access ComboBox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   function GetSelectedEntryString
     (Item : access ComboBox_Type)
      return Unbounded_String;

   procedure GetSelectedEntry
     (Item   : access ComboBox_Type;
      String : out Unbounded_String;
      Color  : out Canvas.Color_Type);

   function GetEntries
     (Item : access ComboBox_Type)
      return GUI.Basics.StringAndColorList_Pack.List;

   function GetEntryCount
     (Item : access ComboBox_Type)
      return Integer;
   ---------------------------------------------------------------------------

   type ComboBox_Constructor is
     access function
       (Parent : GUI.Object_ClassAccess)
        return Combobox_ClassAccess;

private

   type ComboBox_Type is new ComboBox_Public with
      record
         Choices : GUI.Basics.StringAndColorList_Pack.List;
         Index   : Integer;
      end record;

end GUI.ComboBox;
