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

with ByteOperations;
with Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;

package Packets is

   PacketOutOfData : Exception;

   type Packet_Type is tagged;
   type Packet_ClassAccess is access all Packet_Type;
   type Packet_Type is tagged
      record
         Content        : ByteOperations.ByteArray_Access := null;
         Position       : Integer            := 0;
         Amount         : Integer            := 0;
         Next           : Packet_ClassAccess := null;
         Last           : Packet_ClassAccess := null;
         CData1         : Interfaces.C.int;
      end record;

   procedure Write
     (Packet : access Packet_Type;
      Item   : Types.Integer32);

   procedure Write
     (Packet : access Packet_Type;
      Item   : Types.Cardinal32);

   procedure Write
     (Packet : access Packet_Type;
      Item   : Unbounded_String);

   function Read
     (Packet : access Packet_Type)
      return Unbounded_String;

   function Read
     (Packet : access Packet_Type)
      return Types.Integer32;

   function Read
     (Packet : access Packet_Type)
      return Types.Cardinal32;

   procedure Debug
     (Packet : access Packet_Type);

   procedure Free
     (Packet : access Packet_Type);

end Packets;
