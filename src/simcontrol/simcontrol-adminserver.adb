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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Network.Streams;
with Network.Packets;
with AdminProtocol;
with Logging;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;

package body SimControl.AdminServer is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusWaitForCommand,
      ReceiveStatusProcessCommand,
      ReceiveStatusInvalid);

   ---------------------------------------------------------------------------

   type ServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         ReceiveStatus  : ReceiveStatus_Enum;
         Channel        : Network.Streams.Channel_ClassAccess;
         LogChannel     : Logging.Channel_ClassAccess;
         CurrentCommand : AdminProtocol.ServerCmd_Type;
      end record;

   type ServerChannelCallBack_Access is access ServerChannelCallBack_Type;

   overriding
   procedure Receive
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure Disconnect
     (Item : in out ServerChannelCallBack_Type);
   ---------------------------------------------------------------------------

   type ServerCallBack_Type is
     new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);
   ---------------------------------------------------------------------------

   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess:=null;
   ServerCallBack       : aliased ServerCallBack_Type;
   LogImplementation    : Logging.Implementation_Type;
   LogContext           : Logging.Context_ClassAccess:=null;
   LogMainChannel       : Logging.Channel_ClassAccess:=null;
   ---------------------------------------------------------------------------

   function Cmd_AdminServerMessage
     (Item : in ServerChannelCallBack_Type)
      return Boolean is

      Message       : Unbounded_String;

   begin

      Message:=Item.Channel.Read;
      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => To_String("Message:" & Message));

      return True;

   end Cmd_AdminServerMessage;
   ---------------------------------------------------------------------------

   function Cmd_Shutdown
     (Item : in ServerChannelCallBack_Type)
      return Boolean is

   begin

      Item.LogChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Received shutdown command");
      Terminated:=True;
      return True;

   end Cmd_Shutdown;
   ---------------------------------------------------------------------------

   type CmdArray is array (AdminProtocol.ServerCmd_Type range <>) of
     access function
       (Item : in ServerChannelCallBack_Type) return Boolean;

   Cmds:constant CmdArray:=
     (AdminProtocol.ServerCmdMessage  => Cmd_AdminServerMessage'Access,
      AdminProtocol.ServerCmdShutdown => Cmd_Shutdown'Access);

   procedure Receive
     (Item : in out ServerChannelCallBack_Type) is

      PrevPosition : Integer;

      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition:=Item.Channel.Position;
         case Item.ReceiveStatus is

            when ReceiveStatusWaitForIdentification =>

               if Item.Channel.Read/=AdminProtocol.ClientID then
                  Item.LogChannel.Write
                    (Level   => Logging.LevelInvalid,
                     Message => "Wrong identification of the client for Network Admin");
                  Item.ReceiveStatus:=ReceiveStatusInvalid;
                  Item.Channel.Disconnect;
                  return;
               else
                  Item.LogChannel.Write
                    (Level => Logging.LevelEvent,
                     Message => "Admin Client has valid identification");
               end if;
               Item.ReceiveStatus := ReceiveStatusWaitForCommand;

            when ReceiveStatusWaitForCommand =>

               Item.CurrentCommand:=Item.Channel.Read;

               Item.LogChannel.Write
                 (Level => Logging.LevelCommonEvent,
                  Message => "Received Command :"&
                  AdminProtocol.ServerCmd_Type'Image(Item.CurrentCommand));

               if Item.CurrentCommand not in Cmds'Range then
                  Item.LogChannel.Write
                    (Level => Logging.LevelFailure,
                     Message => "Received Command "&
                     AdminProtocol.ServerCmd_Type'Image(Item.CurrentCommand)&" not in valid range");
                  Item.ReceiveStatus:=ReceiveStatusInvalid;
                  Item.Channel.Disconnect;
                  return;
               end if;

               Item.ReceiveStatus:=ReceiveStatusProcessCommand;

            when ReceiveStatusProcessCommand =>

               Item.LogChannel.Write
                 (Level => Logging.LevelCommonEvent,
                  Message => "Process Command");
               if Cmds(Item.CurrentCommand).all
                 (Item=> Item) then
                  Item.ReceiveStatus:=ReceiveStatusWaitForCommand;
               end if;

            when ReceiveStatusInvalid =>

               return;

         end case;
      end loop;
   exception
      when Network.Packets.PacketOutOfData =>
         Item.Channel.Position := PrevPosition;
   end Receive;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : in out ServerChannelCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Item.LogChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Disconnected client");
      Item.LogChannel.FreeChannel;
      Network.Streams.Free(Item.Channel.CallBack);
   end Disconnect;
   ---------------------------------------------------------------------------

   procedure AAccept
     (Item : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is
      pragma Unreferenced(Item);

      NewCallBack : ServerChannelCallBack_Access;
      Packet      : Network.Packets.Packet_Access;

   begin
      LogMainChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Accept connection for Admin server");
      NewCallBack := new ServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ReceiveStatusWaitForIdentification;
      NewCallBack.Channel       := Channel;
      LogContext.NewChannel
        (ChannelName => ConcatElements
           (Item      => Channel.PeerAddress,
            Separator => To_Unbounded_String(";")),
         Channel     => NewCallBack.LogChannel);
      Channel.CallBack
        := Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(AdminProtocol.ServerID);
      Channel.SendPacket(Packet);
   end AAccept;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      LogImplementation:=
        Logging.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Logging"));

      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ConfigNode    => U("Logging"),
         ModuleName    => U("Control.Admin"));

      LogContext.NewChannel
        (ChannelName => U("Server"),
         Channel     => LogMainChannel);

      StreamImplementation:=
        Network.Streams.Implementations.Find
          (Configuration => Configuration,
           Node          => U("Admin.Network"));

      StreamImplementation.Initialize.all;

      Server
        :=StreamImplementation.NewServer
          (Configuration => Configuration,
           Node          => U("Admin.Server.Network"));
      Server.CallBack:=ServerCallBack'Access;

      Put("Send Server Initialized");
      New_Line;
      LogMainChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Server initialized");

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      StreamImplementation.FreeServer(Server);

      StreamImplementation.Finalize.all;

      LogImplementation.FreeContext
        (Item => LogContext);
   end Finalize;
   ---------------------------------------------------------------------------

end SimControl.AdminServer;
