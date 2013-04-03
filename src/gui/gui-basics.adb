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

package body GUI.Basics is

   function ElementAtIndex
     (List : StringAndColorList_Pack.List;
      Index : Integer)
      return StringAndColorList_Pack.Cursor is

      Cursor : StringAndColorList_Pack.Cursor;

   begin

      Cursor:=List.First;
      for i in 1..Index loop
         Cursor:=StringAndColorList_Pack.Next(Cursor);
      end loop;
      return Cursor;

   end ElementAtIndex;
   ---------------------------------------------------------------------------

end GUI.Basics;
