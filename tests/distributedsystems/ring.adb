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
--   21.Jun 2012 Julian Schutsch
--     - Original version

with Ada.Text_IO; use Ada.Text_IO;

with Basics; use Basics;
with Config;
with ProgramArguments;
with DistributedSystems;
with DistributedSystems.UseImplementations;
with Network.UseImplementations;
with Network.Packets;
with Types;

procedure Ring is

   use type DistributedSystems.Node_Type;

   RingLoops : constant := 1000;

   Implementation : DistributedSystems.Implementation_Type;
   Configuration  : Config.Config_Type;
   NextID         : DistributedSystems.Node_Type;
   LastID         : DistributedSystems.Node_Type;
   Received       : Integer:=0;

begin

   ProgramArguments.Initialize;

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;

   Implementation:=DistributedSystems.Implementations.Find
     (Configuration => ProgramArguments.Configuration,
      Node          => U("Arguments"));

   Implementation.InitializeNode(Configuration);

   NextID:=DistributedSystems.ThisNode+1;
   if NextID>DistributedSystems.LastNode then
      NextID:=DistributedSystems.FirstNode;
   end if;

   LastID:=DistributedSystems.ThisNode-1;
   if LastID<DistributedSystems.FirstNode or
     LastID>DistributedSystems.LastNode then
      LastID:=DistributedSystems.LastNode;
   end if;

   for i in 1..RingLoops loop
      declare
         Packet : Network.Packets.Packet_Access;
      begin
         Packet := new Network.Packets.Packet_Type;
         Packet.Write(Types.Integer32(1));
         Implementation.SendMessage
           (Dest   => NextID,
            Packet => Packet);
      end;
      declare

         procedure Receive
           (Source : DistributedSystems.Node_Type;
            Packet : Network.Packets.Packet_Access) is
            pragma Unreferenced(Packet);
         begin
            if Source/=LastID then
               Put_Line("Wrong Packet Received"
                        &DistributedSystems.Node_Type'Image(Source)
                        &"/="&DistributedSystems.Node_Type'Image(LastID)
                          &"::"&DistributedSystems.Node_Type'Image(DistributedSystems.ThisNode));
            else
               Put_Line("Right Packet Received "&DistributedSystems.Node_Type'Image
                        (DistributedSystems.ThisNode)
                          &" l"&Integer'Image(i));
               Received:=Received+1;
            end if;
         end Receive;
         ---------------------------------------------------------------------

      begin
         loop
            Implementation.ProcessMessages(Receive'Unrestricted_Access);
            exit when Received>=i;
         end loop;
      end;
      Put_Line("LOOP DONE"
               &Integer'Image(i)
                 &" "&DistributedSystems.Node_Type'Image(DistributedSystems.ThisNode));

   end loop;

   Implementation.FinalizeNode.all;

   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

end Ring;
