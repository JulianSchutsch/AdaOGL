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

with Ada.Calendar;
with Network.Streams;
with Network.Packets;
with ProcessLoop;
with Logging;
with AdminProtocol;

package body SimClient.AdminClient is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusWaitForCommand);

   type ClientCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure OnConnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnFailedConnect
     (Item : in out ClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnDisconnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ClientCallBack_Type);
   ---------------------------------------------------------------------------

   StreamImplementation : Network.Streams.Implementation_Type;
   Client               : Network.Streams.Client_ClassAccess;
   LogImplementation    : Logging.Implementation_Type;
   LogContext           : Logging.Context_ClassAccess;
   LogChannel           : Logging.Channel_ClassAccess;
   ReceiveStatus        : ReceiveStatus_Enum;
   ClientCallBack       : aliased ClientCallBack_Type;
   Connected            : Boolean:=False;

   ---------------------------------------------------------------------------

   procedure OnReceive
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);

      PrevPosition : Integer;
      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition:=Client.Position;
         case ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identifier : Unbounded_String;
               begin

                  Identifier:=Client.Read;

                  if Identifier/=AdminProtocol.ServerID then
                     LogChannel.Write
                       (Level => Logging.LevelInvalid,
                        Message => "Identification (Admin) send by the server is invalid.");
                     -- TODO: Set ALL TO INVALID
                     return;
                  else
                     LogChannel.Write
                       (Level   => Logging.LevelEvent,
                        Message => "Identification (Admin) send by the server is valid.");
                     ReceiveStatus := ReceiveStatusWaitForCommand;
                  end if;

               end;
            when ReceiveStatusWaitForCommand =>
               return;
         end case;
      end loop;
   exception
      when Network.Packets.PacketOutOfData =>
         Client.Position:=PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ClientCallBack_Type) is
      pragma Unreferenced(Item);
      Packet : Network.Packets.Packet_Access;
   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Succeded to connect to Admin server.");
      ReceiveStatus := ReceiveStatusWaitForIdentification;

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(AdminProtocol.ClientID);
      Client.SendPacket(Packet);

      Connected:=True;
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnFailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Unreferenced(Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelFailure,
         Message => "Failed to connect to Admin server.");

      Retry:=True;
   end OnFailedConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Connected:=False;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   procedure WaitForCompletion is
   begin
      while not Client.SendBufferEmpty loop
         ProcessLoop.Process;
      end loop;
   end WaitForCompletion;
   ---------------------------------------------------------------------------

   function WaitForConnection
     return Boolean is

      use type Ada.Calendar.Time;

      StartTime : Ada.Calendar.Time;

   begin
      -- Terminate loop after 3 seconds
      StartTime := Ada.Calendar.Clock;
      loop
         ProcessLoop.Process;
         exit when (Ada.Calendar.Clock-StartTime)>15.0;
         exit when Connected;
      end loop;
      return Connected;
   end WaitForConnection;
   ---------------------------------------------------------------------------

   procedure  WaitForDisconnect is

      use type Ada.Calendar.Time;

      StartTime : Ada.Calendar.Time;

   begin
      StartTime:=Ada.Calendar.Clock;
      loop
         ProcessLoop.Process;
         exit when (Ada.Calendar.Clock-StartTime)>15.0;
         exit when not Connected;
      end loop;
   end WaitForDisconnect;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      LogImplementation:=Logging.Implementations.Find
        (Configuration  => Configuration,
         Node           => To_Unbounded_String("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ModuleName => To_Unbounded_String("Admin"));
      LogContext.NewChannel
        (ChannelName => To_Unbounded_String(""),
         Channel     => LogChannel);

      StreamImplementation
        := Network.Streams.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Admin.Network"));

      StreamImplementation.Initialize.all;

      Client
        :=StreamImplementation.NewClient
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Admin.Client.Network"));
      Client.CallBack:=ClientCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure SendShutdown is
      Packet : Network.Packets.Packet_Access;
   begin
      if not Connected then
         raise NotConnected;
      end if;

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(AdminProtocol.ServerCmdShutdown);

      Client.SendPacket(Packet);

   end SendShutdown;
   ---------------------------------------------------------------------------

   procedure SendMessage
     (Message : Unbounded_String) is

      Packet : Network.Packets.Packet_Access;

   begin
      if not Connected then
         raise NotConnected;
      end if;
      if Length(Message)>128 then
         raise FailedSend with "Message too long. Must not exceed 128 characters";
      end if;

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(AdminProtocol.ServerCmdMessage);
      Packet.Write(Message);
      Client.SendPacket(Packet);
   end SendMessage;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeClient
        (Item => Client);
      StreamImplementation.Finalize.all;

      LogImplementation.FreeContext
        (Item => LogContext);
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return Boolean is

   begin
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimClient.AdminClient;
