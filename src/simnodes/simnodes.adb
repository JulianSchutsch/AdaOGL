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

with Ada.Text_IO; use Ada.Text_IO;
with ProgramArguments;
with Basics; use Basics;
with DistributedSystems; use DistributedSystems;
with Packets;
with Types; use Types;

package body SimNodes is

   type EndType_Enum is
     (EndTypeFirst,
      EndTypeLast);

   DistributedSystemsImpl : DistributedSystems.Implementation_Type;

   NodeTypeIDs : constant array(NodeType_Enum) of Types.Cardinal32:=
     (NodeTypeFront => 0,
      NodeTypeSim   => 1);

   EndTypeIDs : constant array(EndType_Enum) of Types.Cardinal32:=
     (EndTypeFirst => 0,
      EndTypeLast  => 1);

   procedure Initialize
     (NodeType : NodeType_Enum) is

      procedure SendNodeTypePacket
        (Dest   : Node_Type) is

         Packet : Packets.Packet_ClassAccess;

      begin
         Packet:=new Packets.Packet_Type;
         Packet.Write(NodeTypeIDs(NodeType));
         DistributedSystemsImpl.SendMessage
           (Dest   => Dest,
            Packet => Packet);
      end SendNodeTypePacket;
      ------------------------------------------------------------------------

      procedure SendFirstNode
        (Dest : Node_Type) is

         Packet : Packets.Packet_ClassAccess;

      begin
         Put_Line("SF"&Node_Type'Image(ThisNode));
         Packet:=new Packets.Packet_Type;
         Packet.Write(EndTypeIDs(EndTypeFirst));
         Packet.Write(NodeTypeIDs(NodeType));
         Packet.Write(Types.Cardinal32(ThisNode));
         DistributedSystemsImpl.SendMessage
           (Dest   => Dest,
            Packet => Packet);
      end SendFirstNode;
      ------------------------------------------------------------------------

      procedure SendLastNode
        (Dest : Node_Type) is

         Packet : Packets.Packet_ClassAccess;

      begin
         Put_Line("SL"&Node_Type'Image(ThisNode));
         Packet:=new Packets.Packet_Type;
         Packet.Write(EndTypeIDs(EndTypeLast));
         Packet.Write(NodeTypeIDs(NodeType));
         Packet.Write(Types.Cardinal32(ThisNode));
         DistributedSystemsImpl.SendMessage
           (Dest   => Dest,
            Packet => Packet);
      end SendLastNode;
      ------------------------------------------------------------------------

      First           : Boolean := False;
      Last            : Boolean := False;
      ExpectedPackets : Integer := 2;

      procedure ReceiveNeighbourType
        (Source : Node_Type;
         Packet : Packets.Packet_ClassAccess) is

         RecNodeType : Types.Cardinal32;

      begin
         RecNodeType:=Packet.Read;
         if RecNodeType/=NodeTypeIDs(NodeType) then
            if Source=ThisNode-1 then
               First:=True;
            elsif Source=ThisNode+1 then
               Last:=True;
            end if;
         end if;
         ExpectedPackets:=ExpectedPackets-1;
      end ReceiveNeighbourType;
      ------------------------------------------------------------------------

      procedure ReceiveRanges
        (Source : Node_Type;
         Packet : Packets.Packet_ClassAccess) is

         RecEndType  : Types.Cardinal32;
         RecNodeType : Types.Cardinal32;
         RecNode     : Types.Cardinal32;

      begin
         RecEndType  := Packet.Read;
         RecNodeType := Packet.Read;
         RecNode     := Packet.Read;
         if Node_Type(RecNode)/=Source then
            Put_Line("RecNode/=Source in ReceiveRanges");
         end if;

         if RecEndType=EndTypeIDs(EndTypeFirst) then
            if RecNodeType=NodeTypeIDs(NodeTypeFront) then
               FirstFront:=Node_Type(RecNode);
            else
               FirstSim:=Node_Type(RecNode);
            end if;
         else
            if RecNodeType=NodeTypeIDs(NodeTypeFront) then
               LastFront:=Node_Type(RecNode);
            else
               LastSim:=Node_Type(RecNode);
            end if;
         end if;

         ExpectedPackets:=ExpectedPackets-1;
         Put_Line("Remaining Ranges : "&Integer'Image(ExpectedPackets));
      end ReceiveRanges;
      ------------------------------------------------------------------------

   begin

      DistributedSystemsImpl
        := DistributedSystems.Implementations.Find
          (Configuration => ProgramArguments.Configuration,
           Node          => U("Arguments"));
      DistributedSystemsImpl.InitializeNode(Configuration);

      if ThisNode/=FirstNode then
         SendNodeTypePacket(ThisNode-1);
      else
         First:=True;
         ExpectedPackets:=ExpectedPackets-1;
      end if;
      if ThisNode/=LastNode then
         SendNodeTypePacket(ThisNode+1);
      else
         Last:=True;
         ExpectedPackets:=ExpectedPackets-1;
      end if;
      Put_Line("Where "&Node_Type'Image(ThisNode)&" "&Integer'Image(ExpectedPackets));
      while ExpectedPackets>0 loop
         DistributedSystemsImpl.ProcessMessages(ReceiveNeighbourType'Unrestricted_Access);
      end loop;
      Put_Line("First "&Node_Type'Image(ThisNode));
      if First then
         for i in FirstNode..LastNode loop
            if i/=ThisNode then
               SendFirstNode(i);
            end if;
         end loop;
      end if;
      Put_Line("Last "&Node_Type'Image(ThisNode));
      if Last then
         for i in Firstnode..LastNode loop
            if i/=ThisNode then
               SendLastNode(i);
            end if;
         end loop;
      end if;
      ExpectedPackets:=4;
      if First then
         ExpectedPackets:=ExpectedPackets-1;
      end if;
      if Last then
         ExpectedPackets:=ExpectedPackets-1;
      end if;
      Put_Line("Count?"&Node_Type'Image(ThisNode)&"::"&Integer'Image(ExpectedPackets));
      while ExpectedPackets>0 loop
         DistributedSystemsImpl.ProcessMessages(ReceiveRanges'Unrestricted_Access);
      end loop;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      DistributedSystemsImpl.FinalizeNode.all;
   end Finalize;
   ---------------------------------------------------------------------------

end  SimNodes;
