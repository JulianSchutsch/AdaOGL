--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

-- Revision History
--   18.Jun 2012 Julian Schutsch

-- Reasons for implementation
--   Create the means to handle or at least display errors which are not
--   expected.
--   A string representation for errors is introduced to attach error data
--   to exceptions.
--   A database is necessary to decode the given error numbers, but
--   may permit hints on how to fix errors without consultation of a user
--   manual.

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Errors is

   type ErrorParamArray_Type is array(Integer range <>) of Unbounded_String;
   type ErrorParamArray_Access is access all ErrorParamArray_Type;
   type Error_Type is
      record
         Error   : Unbounded_String;
         Params  : ErrorParamArray_Access := null;
      end record;

   type Error_Access is access all Error_Type;

   function StringToParams
     (Str : String)
      return ErrorParamArray_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ErrorParamArray_Type,
      Name   => ErrorParamArray_Access);

end Errors;
