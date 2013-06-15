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
--   19.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Text_IO; use Ada.Text_IO;
with DistributedSystems;
with DistributedSystems.UseImplementations;
with Network.UseImplementations;
with ProgramArguments;
with Basics; use Basics;
with Config;
with Network.Packets;
with Types;
with Ada.Numerics.Float_Random;

procedure Pi is

   use type DistributedSystems.Node_Type;

   Iterations : constant := 100000000;

   Implementation : DistributedSystems.Implementation_Type;
   Configuration  : Config.Config_Type;
   NodeResult     : Integer;
   SumResult      : Integer:=0;

begin

   ProgramArguments.Initialize;

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;

   Implementation:=DistributedSystems.Implementations.Find
     (Configuration => ProgramArguments.Configuration,
      Node          => U("Arguments"));

   Implementation.InitializeNode(Configuration);

   -- Calculate some value
   declare
      use Ada.Numerics.Float_Random;
      Gen : Generator;
      X   : Float;
      Y   : Float;
   begin
      Reset(Gen);
      NodeResult:=0;
      for i in 1..Iterations loop
         X:=Random(Gen);
         Y:=Random(Gen);
         if X*X+Y*Y<=1.0 then
            NodeResult:=NodeResult+1;
         end if;
      end loop;
   end;
   -- If Node=FirstNode then receive from all nodes
   if DistributedSystems.ThisNode=DistributedSystems.FirstNode then

      Put_Line("First Node, enter Receive loop");
      declare

         Results : array(DistributedSystems.FirstNode..DistributedSystems.LastNode) of Integer;
         ReceivedResults : Integer:=1; -- One result is stored for "this" node.

         procedure Receive
           (Source : DistributedSystems.Node_Type;
            Packet : Network.Packets.Packet_Access) is
            Value : Types.Integer32;
         begin
            Put_Line("Receive from "
                       &DistributedSystems.Node_Type'Image(Source));
            Value           := Packet.Read;
            Results(Source) := Integer(Value);
            ReceivedResults := ReceivedResults+1;
         end Receive;
         ---------------------------------------------------------------------

      begin
         Results(DistributedSystems.FirstNode):=NodeResult;
         while ReceivedResults<DistributedSystems.NodeCount loop
            Implementation.ProcessMessages(Receive'Unrestricted_Access);
         end loop;
         for i in Results'Range loop
            SumResult:=SumResult+Results(i);
         end loop;
      end;
      Put_Line(Float'Image(4.0*Float(SumResult)/float(Iterations*DistributedSystems.NodeCount)));

   else
      Put_Line("Send Result : "
               &DistributedSystems.Node_Type'Image(DistributedSystems.ThisNode));
      declare
         Packet : Network.Packets.Packet_Access;
      begin
         Packet:=new Network.Packets.Packet_Type;
         Packet.Write(Types.Integer32(NodeResult));
         Implementation.SendMessage
           (Dest   => DistributedSystems.FirstNode,
            Packet => Packet);
      end;
      Implementation.WaitForSend.all;
   end if;

   Implementation.FinalizeNode.all;

   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

end Pi;
