-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
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
with Endianess;
with Interfaces.C;
with Basics; use Basics;
with Ada.Streams;

package Bytes is

   type PtrInt_Type is new Interfaces.C.size_t;
   subtype Int_Type is Interfaces.C.int;
   type Int_Access is access all Int_Type;
   type Int_Array is array(Integer range <>) of aliased Int_Type;
   pragma Convention(C,Int_Array);

   -- Carefull, there may be rare systems where this is not a Byte.
   -- TODO: Check
   type Byte_Type is new Ada.Streams.Stream_Element;
   type Byte_Access is access all Byte_Type;
   type Byte_Array is array(PtrInt_Type range <>) of aliased Byte_Type;
   type Byte_ArrayAccess is access all Byte_Array;
   pragma Convention(C,Byte_Array);

   type LittleEndianCardinal32_Access is access all Endianess.LittleEndianCardinal32;

   function AddressToIntAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => Int_Access);

   function AddressToByteAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => Byte_Access);

   function ByteAccessToLECardinal32Access is new Ada.Unchecked_Conversion
     (Source => Byte_Access,
      Target => LittleEndianCardinal32_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Byte_Array,
      Name   => Byte_ArrayAccess);

   function "-"
     (Left  : Byte_Access;
      Right : Integer)
      return Byte_Access;

   function "+"
     (Left  : Byte_Access;
      Right : Integer)
      return Byte_Access;

end Bytes;
