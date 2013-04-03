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

with Ada.Text_IO; use Ada.Text_IO;

package body GUI.RadioButton is

   procedure SetChecked
     (Item : access RadioButton_Type) is

      Cursor : RadioButton_ClassAccess;

   begin

      Put_Line("Clear Last");
      Cursor:=Item.LastInGroup;
      while Cursor/=null loop
         Cursor.ClearChecked;
         Cursor:=Cursor.LastInGroup;
      end loop;

      Put_Line("Clear Next");
      Cursor:=Item.NextInGroup;
      while Cursor/=null loop
         Cursor.ClearChecked;
         Cursor:=Cursor.NextInGroup;
      end loop;

   end SetChecked;
   ---------------------------------------------------------------------------

   procedure Link
     (Item        : access RadioButton_Type;
      RadioButton : RadioButton_ClassAccess) is
   begin
      if Item.LastInGroup/=null then
         Item.LastInGroup.NextInGroup:=Item.NextInGroup;
      end if;
      if Item.NextInGroup/=null then
         Item.NextInGroup.LastInGroup:=Item.LastInGroup;
      end if;
      Item.LastInGroup:=RadioButton;
      Item.NextInGroup:=RadioButton.NextInGroup;
      if Item.LastInGroup/=null then
         Item.LastInGroup.NextInGroup:=RadioButton_ClassAccess(Item);
      end if;
      if Item.NextInGroup/=null then
         Item.NextInGroup.LastInGroup:=RadioButton_ClassAccess(Item);
      end if;
   end Link;
   ---------------------------------------------------------------------------

end GUI.RadioButton;
