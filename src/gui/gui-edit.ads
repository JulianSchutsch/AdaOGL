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
--   4.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.Edit is

   type Edit_Type is abstract new Object_Type with null record;
   type Edit_Access is access all Edit_Type;
   type Edit_ClassAccess is access all Edit_Type'Class;
   type Edit_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return Edit_ClassAccess;

   procedure SetText
     (Item : access Edit_Type;
      Text : Unbounded_String) is abstract;

   function GetText
     (Item : access Edit_Type)
      return Unbounded_String is abstract;

end GUI.Edit;
