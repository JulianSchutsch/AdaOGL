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
--   1.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.TabControl is

   type TabControl_Type is abstract new Object_Type with null record;
   type TabControl_Access is access all TabControl_Type;
   type TabControl_ClassAccess is access all TabControl_Type'Class;
   type TabControl_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return TabControl_ClassAccess;

   type Tab_Type is abstract new Object_Type with null record;
   type Tab_Access is access all Tab_Type;
   type Tab_ClassAccess is access all Tab_Type'Class;

   function NewTab
     (Item    : access TabControl_Type;
      Caption : Unbounded_String)
      return Tab_ClassAccess is abstract;

end GUI.TabControl;
