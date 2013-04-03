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
--   3.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.CheckBox is

   type OnCheckChance_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type CheckBox_Type is abstract new Object_Type with
      record
         OnCheckChange : OnCheckChance_Access;
      end record;
   type CheckBox_Access is access all Checkbox_Type;
   type CheckBox_ClassAccess is access all CheckBox_Type'Class;

   type CheckBox_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return CheckBox_ClassAccess;

   function IsChecked
     (Item : access CheckBox_Type)
      return Boolean is abstract;

   procedure SetChecked
     (Item    : access CheckBox_Type;
      Checked : Boolean) is abstract;

   procedure SetCaption
     (Item    : access Checkbox_Type;
      Caption : Unbounded_String) is abstract;

end GUI.CheckBox;
