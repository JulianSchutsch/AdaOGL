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

with Logging;
with Network.Streams;
with Network.Packets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Text_IO; use Ada.Text_IO;
with ControlProtocol;

package body SimRegion.ControlClient is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusWaitForCommand,
      ReceiveStatusInvalid);

   StreamImplementation  : Network.Streams.Implementation_Type;
   Client                : Network.Streams.Client_ClassAccess;
   ReceiveStatus         : ReceiveStatus_Enum;
   LogImplementation     : Logging.Implementation_Type;
   LogContext            : Logging.Context_ClassAccess;
   LogChannel            : Logging.Channel_ClassAccess;
   ConnectTriesLeft      : Natural;
   CurrentCommand        : ControlProtocol.ServerCmd_Type;

   type ClientCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure OnFailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnConnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ClientCallBack_Type);
   ---------------------------------------------------------------------------

   procedure OnReceive
     -- TODO: Check all warnings for reason
     (Item : in out ClientCallBack_Type) is

      pragma Unreferenced(Item);

      PrevPosition : Integer;
      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition := Client.Position;
         case ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identification : Unbounded_String;
               begin
                  Identification:=Client.Read;
                  if Identification/=ControlProtocol.ServerID then
                     LogChannel.Write
                       (Level   => Logging.LevelInvalid,
                        Message => "Identification (Control) send by the server is invalid");
                     ReceiveStatus := ReceiveStatusInvalid;
                     Client.Disconnect;
                     return;
                  else
                     LogChannel.Write
                       (Level   => Logging.LevelEvent,
                        Message => "Identification (Control) send by the server is valid");
                     ReceiveStatus:=ReceiveStatusWaitForCommand;
                  end if;
               end;
            when ReceiveStatusWaitForCommand =>
               CurrentCommand:=Client.Read;
               LogChannel.Write
                 (Level => Logging.LevelCommonEvent,
                  Message => "Received Command :"&
                  ControlProtocol.ServerCmd_Type'Image(CurrentCommand));
               return;
            when ReceiveStatusInvalid =>
               return;
         end case;
      end loop;
   exception
      when Network.Packets.PacketOutOfData =>
         Client.Position := PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnFailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Unreferenced(Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelFailure,
         Message => "Failed to connect to Control network");
      if ConnectTriesLeft>0 then
         ConnecttriesLeft := ConnectTriesLeft-1;
         Retry            := True;
      else
         Retry      := False;
         Terminated := True;
      end if;
   end;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ClientCallBack_Type) is
      pragma Unreferenced(Item);

      Packet : Network.Packets.Packet_Access;

   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Connected to Control network");
      ReceiveStatus := ReceiveStatusWaitForIdentification;
      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(ControlProtocol.ClientID);
      Client.SendPacket(Packet);
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Disconnected from Control network");
   end OnDisconnect;
   ---------------------------------------------------------------------------

   ClientCallBack : aliased ClientCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      LogImplementation
        :=Logging.Implementations.Find
          (Configuration => Configuration,
           Node          => U("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ModuleName    => U("Region.Control"));

      LogContext.NewChannel
        (ChannelName => To_Unbounded_String(""),
         Channel     => LogChannel);

      StreamImplementation
        :=Network.Streams.Implementations.Find
          (Configuration => Configuration,
           Node          => U("Control.Network"));

      StreamImplementation.Initialize.all;

      ConnectTriesLeft:=4;

      Client
        :=StreamImplementation.NewClient
          (Configuration => Configuration,
           Node          => U("Control.Client.Network"));

      Client.CallBack:=ClientCallBack'Access;

   end Initialize;
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

end SimRegion.ControlClient;
