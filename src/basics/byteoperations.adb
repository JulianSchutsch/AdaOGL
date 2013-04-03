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

with Interfaces.C;

package body ByteOperations is

   function "+"
     (Left  : Byte_Access;
      Right : Integer)
      return Byte_Access is

      use type Interfaces.C.size_t;

      function ByteAccessToSizeT is new Ada.Unchecked_Conversion
        (Source => Byte_Access,
         Target => Interfaces.C.size_t);

      function SizeTToByteAccess is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.size_t,
         Target => Byte_Access);

   begin
      return SizeTToByteAccess
        (ByteAccessToSizeT(Left)+Interfaces.C.size_t(Right));
   end "+";

end ByteOperations;
