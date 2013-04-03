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

with Endianess;
with Ada.Unchecked_Deallocation;
--with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Network.Packets is

   PacketSizeIncrease : constant:=1024;

   procedure Debug
     (Packet : access Packet_Type) is

      ElementCount : Integer:=0;

   begin
      Put("Amount:");
      Put(Packet.Amount);
      New_Line;
      for i in Packet.Content'First..Packet.Amount-1 loop
         Put(Integer(Packet.Content(i)),Base => 16);
         ElementCount:=ElementCount+1;
         if ElementCount mod 6=0 then
            New_Line;
         else
            Put(" ");
         end if;
      end loop;
      if ElementCount mod 6/=0 then
         New_Line;
      end if;
   end Debug;
   ---------------------------------------------------------------------------

   procedure AddPacketSize
     (Packet : access Packet_Type;
      Size   : Integer) is
      use type  ByteOperations.ByteArray_Access;

      NewSize : Integer;

   begin
      NewSize:=((Packet.Position+Size)/PacketSizeIncrease)*PacketSizeIncrease;
      if (Packet.Position+Size) mod PacketSizeIncrease/=0 then
         NewSize:=NewSize+PacketSizeIncrease;
      end if;

      -- Allocate initial memory
      if (Packet.Content=null) then
         Packet.Content:=new ByteOperations.ByteArray_Type(0..NewSize-1);
         return;
      end if;

      -- Allocate additional memory
      if (Packet.Position+Size>Packet.Content'Last) then
         declare
            NewContent : ByteOperations.ByteArray_Access;
         begin
            NewContent:=new ByteOperations.ByteArray_Type(0..NewSize-1);
            NewContent(0..Packet.Content'Last):=Packet.Content.all;
            ByteOperations.Free(Packet.Content);
            Packet.Content:=NewContent;
         end;
      end if;

   end AddPacketSize;
   ---------------------------------------------------------------------------

   procedure Write
     (Packet  : access Packet_Type;
      Pointer : ByteOperations.Byte_Access;
      Size    : Integer) is

      use type ByteOperations.Byte_Access;

      Pnt : ByteOperations.Byte_Access:=Pointer;

   begin

      AddPacketSize(Packet,Size);

      for i in 1..Size loop
         Packet.Content(Packet.Position) := Pnt.all;
         Packet.Position := Packet.Position+1;
         Pnt             := Pnt+1;
      end loop;
      Packet.Amount:=Packet.Position;

   end Write;
   ---------------------------------------------------------------------------

   procedure Read
     (Packet  : access Packet_Type;
      Pointer : ByteOperations.Byte_Access;
      Size    : Integer) is

      use type ByteOperations.Byte_Access;

      Pnt : ByteOperations.Byte_Access:=Pointer;

   begin

--      Put("Read :");
--      Put(Size);
--      new_Line;

      if Packet.Position+Size>Packet.Amount then
         raise PacketOutOfData;
      end if;

      for i in 1..Size loop
         Pnt.all         := Packet.Content(Packet.Position);
         Packet.Position := Packet.Position+1;
         Pnt             := Pnt+1;
      end loop;

   end Read;
   ---------------------------------------------------------------------------

   procedure Write
     (Packet : access Packet_Type;
      Item   : Types.Integer32) is

      NetData : aliased Endianess.LittleEndianInteger32:=Endianess.To(Item);

   begin
      Write
        (Packet  => Packet,
         Pointer => ByteOperations.AddressToByteAccess(NetData'Address),
         Size    => NetData'Size/8);
   end Write;
   ---------------------------------------------------------------------------

   procedure Write
     (Packet : access Packet_Type;
      Item   : Unbounded_String) is
   begin

      Packet.Write(Types.Integer32(Length(Item)));
      AddPacketSize(Packet,Length(Item));
      for i in 1..Length(Item) loop
         Packet.Content(Packet.Position) := Character'Pos(Element(Item,i));
         Packet.Position                 := Packet.Position+1;
      end loop;
      Packet.Amount:=Packet.Position;

   end Write;
   ---------------------------------------------------------------------------

   function Read
     (Packet : access Packet_Type)
      return Types.Integer32 is

      NetData: aliased Endianess.LittleEndianInteger32;

   begin

      Read
        (Packet  => Packet,
         Pointer => ByteOperations.AddressToByteAccess(NetData'Address),
         Size    => NetData'Size/8);

      return Endianess.From(NetData);

   end Read;
   ---------------------------------------------------------------------------

   function Read
     (Packet : access Packet_Type)
      return Unbounded_String is

      Length : Types.Integer32;
      Str    : Unbounded_String;

   begin

      Length:=Packet.Read;

      if Packet.Position+Integer(Length)>Packet.Amount then
         raise PacketOutOfData;
      end if;

--      Put("Read Unbounded String");
--      Put(Integer(Length));
--      New_Line;

      -- TODO: Add maximum length check
      -- TODO: Reduce amount of reallocation in this loop!
      for i in 1..Length loop

         Str:=Str&Character'Val(Packet.Content(Packet.Position));
         Packet.Position:=Packet.Position+1;

      end loop;

      return Str;

   end Read;
   ---------------------------------------------------------------------------

   procedure Free
     (Packet : access Packet_Type) is

      PacketVal : Packet_Access;

      procedure InternFree is new Ada.Unchecked_Deallocation
        (Object => Packet_Type,
         Name   => Packet_Access);

   begin
      ByteOperations.Free(Packet.Content);
      PacketVal:=Packet_Access(Packet);
      InternFree(PacketVal);
   end Free;

end Network.Packets;
