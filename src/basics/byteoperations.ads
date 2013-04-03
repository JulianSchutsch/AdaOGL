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
--  16.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

package ByteOperations is

   type Byte_Type is mod 2**8;
   type Byte_Access is access all Byte_Type;
   type ByteArray_Type is array(Integer range <>) of aliased Byte_Type;
   pragma Convention(C,ByteArray_Type);

   type ByteArray_Access is access all ByteArray_Type;

   function AddressToByteAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => Byte_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ByteArray_Type,
      Name   => ByteArray_Access);

   function "+"
     (Left  : Byte_Access;
      Right : Integer)
      return Byte_Access;

end ByteOperations;
