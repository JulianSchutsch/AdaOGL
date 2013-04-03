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
--   5.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.GroupBox is

   type GroupBox_Type is abstract new Object_Type with null record;
   type GroupBox_Access is access all GroupBox_Type;
   type GroupBox_ClassAccess is access all GroupBox_Type'Class;
   type GroupBox_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return GroupBox_ClassAccess;

   procedure SetCaption
     (Item    : access GroupBox_Type;
      Caption : Unbounded_String) is abstract;

end GUI.GroupBox;
