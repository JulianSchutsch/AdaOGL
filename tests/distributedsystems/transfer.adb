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
--   22.Jun 2012 Julian Schutsch
--     - Original version

with Ada.Text_IO; use Ada.Text_IO;
with Config;
with Types;
with DistributedSystems;
with ProgramArguments;
with Basics; use Basics;
with DistributedSystems.UseImplementations;
with Network.UseImplementations;
with Network.Packets;

procedure Transfer is

   use type DistributedSystems.Node_Type;

   PacketSize : constant:=1024*1024*128;

   Implementation : DistributedSystems.Implementation_Type;
   Configuration  : Config.Config_Type;
   ProgStart : Long_Float;

begin

   ProgramArguments.Initialize;

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;

   Implementation:=DistributedSystems.Implementations.Find
     (Configuration => ProgramArguments.Configuration,
      Node          => U("Arguments"));

   Implementation.InitializeNode(Configuration);

   ProgStart := Implementation.GetTime.all;
   -- All nodes send large chunks of data to node0
   -- Node0 has to verify these packets
   -- Node0 only terminates properly if all packets are received
   --  and contain the right data
   -- Data generated is based on random integers and the
   --  Global ID is used to initialize the seed.
   if DistributedSystems.ThisNode/=DistributedSystems.FirstNode then
      declare
         Packet : Network.Packets.Packet_Access;
      begin
         Put_Line("Fill Start : "&Long_Float'Image(Implementation.GetTime.all-ProgStart));
         Packet:=new Network.Packets.Packet_Type;
         for i in 1..PacketSize loop
            Packet.Write(Types.Integer32(i));
         end loop;
         Put_Line("Send Start : "&Long_Float'Image(Implementation.GetTime.all-ProgStart));
         Implementation.SendMessage(DistributedSystems.FirstNode,Packet);
         Put_Line("Send End : "&Long_Float'Image(Implementation.GetTime.all-ProgStart));
      end;
      Implementation.WaitForSend.all;
   else
      declare
         Received : Integer := 1;
         Invalid  : Integer := 0;

         procedure Receive
           (Source : DistributedSystems.Node_Type;
            Packet : Network.Packets.Packet_Access) is
            pragma Unreferenced(Source);
            use type Types.Integer32;
         begin
            Put_Line("Receive "&Long_FLoat'Image(Implementation.GetTime.all-ProgStart));
            for i in 1..PacketSize loop
               declare
                  Val : constant Types.Integer32:=Packet.Read;
               begin
                  if Val/=Types.Integer32(i) then
                     Put_Line("Invalid Value"&Integer'Image(i)&":"&Types.Integer32'Image(Val));
                     Invalid:=Invalid+1;
                     Put_Line("Invalid Nr."&Integer'Image(Invalid));
                     return;
                  end if;
               end;
            end loop;
            Received:=Received+1;
         end Receive;
         ---------------------------------------------------------------------

      begin
         loop
            Implementation.ProcessMessages(Receive'Unrestricted_Access);
            exit when Received=DistributedSystems.NodeCount;
         end loop;
         Put_Line("Loop done");
      end;
   end if;

   Put_Line("End of "&DistributedSystems.Node_Type'Image(DistributedSystems.ThisNode)
              &" "&Long_Float'Image(Implementation.GetTime.all-ProgStart));

   Implementation.FinalizeNode.all;

   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

end Transfer;
