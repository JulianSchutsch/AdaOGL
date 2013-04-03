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

package GUI.RadioButton is

   type RadioButton_Type is abstract new GUI.Object_Type with private;
   type RadioButton_Access is access all RadioButton_Type;
   type RadioButton_ClassAccess is access all RadioButton_Type'Class;
   type RadioButton_Constructor is
     access function
       (Parent : GUI.Object_ClassAccess)
        return RadioButton_ClassAccess;

   procedure SetCaption
     (Item    : access RadioButton_Type;
      Caption : Unbounded_String) is abstract;

   procedure SetChecked
     (Item : access RadioButton_Type);

   procedure ClearChecked
     (Item : access RadioButton_Type) is abstract;

   procedure Link
     (Item : access RadioButton_Type;
      RadioButton : RadioButton_ClassAccess);

private
   type RadioButton_Type is abstract new GUI.Object_Type with
      record
         NextInGroup : RadioButton_ClassAccess:=null;
         LastInGroup : RadioButton_ClassAccess:=null;
      end record;

end GUI.RadioButton;
