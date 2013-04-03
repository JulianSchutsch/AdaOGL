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
--   25.Jan 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   Simplify writting correct network protocol implementations.

-- Usage
--   Only use these types for your network protocol and use To and From to
--   obtain native integers of the same size as defined in Types.
--   It is not possible to work directly with the provided types which
--   is also part of the security.

pragma Ada_2005;

with Types; use Types;

with Interfaces;

package Endianess is

   type LittleEndianInteger16 is private;
   type LittleEndianInteger32 is private;
   type LittleEndianInteger64 is private;
   type LittleEndianCardinal16 is private;
   type LittleEndianCardinal32 is private;
   type LittleEndianCardinal64 is private;

   type BigEndianInteger16 is private;
   type BigEndianInteger32 is private;
   type BigEndianInteger64 is private;
   type BigEndianCardinal16 is private;
   type BigEndianCardinal32 is private;
   type BigEndianCardinal64 is private;

   function To(Integer: Integer16) return LittleEndianInteger16;
   function To(Integer: Integer32) return LittleEndianInteger32;
   function To(Integer: Integer64) return LittleEndianInteger64;

   function From(Integer: LittleEndianInteger16) return Integer16;
   function From(Integer: LittleEndianInteger32) return Integer32;
   function From(Integer: LittleEndianInteger64) return Integer64;

   function To(Integer: Integer16) return BigEndianInteger16;
   function To(Integer: Integer32) return BigEndianInteger32;
   function To(Integer: Integer64) return BigEndianInteger64;

   function From(Integer: BigEndianInteger16) return Integer16;
   function From(Integer: BigEndianInteger32) return Integer32;
   function From(Integer: BigEndianInteger64) return Integer64;

private

   type LittleEndianInteger16 is new Interfaces.Integer_16;
   type LittleEndianInteger32 is new Interfaces.Integer_32;
   type LittleEndianInteger64 is new Interfaces.Integer_64;
   type LittleEndianCardinal16 is new Interfaces.Unsigned_16;
   type LittleEndianCardinal32 is new Interfaces.Unsigned_32;
   type LittleEndianCardinal64 is new Interfaces.Unsigned_64;

   type BigEndianInteger16 is new Interfaces.Integer_16;
   type BigEndianInteger32 is new Interfaces.Integer_32;
   type BigEndianInteger64 is new Interfaces.Integer_64;
   type BigEndianCardinal16 is new Interfaces.Unsigned_16;
   type BigEndianCardinal32 is new Interfaces.Unsigned_32;
   type BigEndianCardinal64 is new Interfaces.Unsigned_64;

end Endianess;
