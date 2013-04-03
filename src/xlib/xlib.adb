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

with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

package body Xlib is

   function "+" (Left : XIMStyle_Access; Right : Interfaces.C.size_t)
                 return XIMStyle_Access is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => XIMStyle_Access,
         Target => System.Address);

      function To_Style_Access is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => XIMStyle_Access);

   begin
      return To_Style_Access
        (To_Address(Left)
         +Storage_Offset(Right));
   end "+";
end xlib;
