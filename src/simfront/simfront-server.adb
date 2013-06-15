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

with SimNodes; use SimNodes;
with Network.Streams;
with Packets;
with Basics; use Basics;
with FrontProtocol;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SimFront.Users; use SimFront.Users;
with Authentication;
with Types; use Types;

with Ada.Text_IO; use Ada.Text_IO;

package body SimFront.Server is

   type ReceiveStatus_Enum is
     (ReceiveStatusVerifyProtocol,
      ReceiveStatusWaitForCommand,
      ReceiveStatusProcessCommand);

   type ServerCallBack_Type is new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure AAccept
     (Item : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);

   type ChannelCallBack_Type is new Network.Streams.ChannelCallBack_Type with
      record
         Channel               : Network.Streams.Channel_ClassAccess;
         User                  : User_Type;
         AuthenticationMessage : Unbounded_String;
         LogChannel            : Logging.Channel_ClassAccess;
      end record;
   type ChannelCallBack_Access is access all ChannelCallBack_Type;

   overriding
   procedure Receive
     (Item : in out ChannelCallBack_Type);

   overriding
   procedure Disconnect
     (Item : in out ChannelCallBack_Type);
   ---------------------------------------------------------------------------

   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess:=null;
   ServerCallBack       : aliased ServerCallBack_Type;
   ReceiveStatus        : ReceiveStatus_Enum;
   CurrentCommand       : FrontProtocol.ServerCmd_Type;

   type Command_Access is
     access procedure
       (Item : in out ChannelCallBack_Type);

   procedure CommandPublicKey
     (Item : in out ChannelCallBack_Type) is

      use type Authentication.PublicKey_ClassAccess;

   begin

      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Receiving Public Key");
      if Item.User.PublicKey/=null then
         Item.User.PublicKey.Free;
         Item.User.PublicKey:=null;
      end if;

      Item.User.PublicKey:=AuthenticationImpl.ReadPublicKey
        (Item.Channel.Received);

      declare
         Packet : Packets.Packet_ClassAccess;
      begin
         begin
            Packet:=new Packets.Packet_Type;
            Packet.Write(FrontProtocol.ClientCmdEncryptMessage);
            Item.AuthenticationMessage:=AuthenticationGenerator.GenerateMessage;
            Packet.Write(Item.AuthenticationMessage);
         exception
            when others =>
               Packets.Free(Packet);
               raise;
         end;
         Item.Channel.SendPacket(Packet);
      end;

      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Received Public Key, Send Message to encrypt");

      ReceiveStatus:=ReceiveStatusWaitForCommand;

   end CommandPublicKey;
   ---------------------------------------------------------------------------

   procedure CommandEncryptedMessage
     (Item : in out ChannelCallBack_Type) is

      use type Authentication.PublicKey_ClassAccess;

      EncryptedMessage : Unbounded_String;

   begin

      EncryptedMessage:=Item.Channel.Received.Read;

      if Item.User.PublicKey=null then
         Item.LogChannel.Write
           (Level   => Logging.LevelError,
            Message => "Encrypted Message without Public Key");
         Item.Channel.Disconnect;
      end if;

      if Item.User.PublicKey.Verify
        (Message   => Item.AuthenticationMessage,
         Encrypted => EncryptedMessage) then
         -- Now it is necessary to look up the user and load
         -- the nick and the privileges
         null;
      else
         Item.LogChannel.Write
           (Level => Logging.LevelInvalid,
            Message => "Invalid Encryption for Public Key");
      end if;

      declare
         Packet : Packets.Packet_ClassAccess;
      begin

         Packet:=new Packets.Packet_Type;
         Packet.Write(FrontProtocol.ClientCmdNotifyPrivileges);
         for Privilege in FrontProtocol.Privileges_Enum'Range loop

            if Item.User.Privileges(Privilege) then
               Packet.Write(Integer32(1));
            else
               Packet.Write(Integer32(0));
            end if;

         end loop;

         Item.Channel.SendPacket(Packet);

      end;

      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Authentication valid, send privileges record");

      ReceiveStatus:=ReceiveStatusWaitForCommand;

   end CommandEncryptedMessage;
   ---------------------------------------------------------------------------

   procedure CommandShutdown
     (Item : in out ChannelCallBack_Type) is
   begin

      if Item.User.Privileges(FrontProtocol.PrivilegeAdmin) then
         null;
         -- Do the actual shutdown
      else
         Item.LogChannel.Write
           (Level   => Logging.LevelInvalidAccess,
            Message => "Privilege Admin required for Shutdown");
      end if;

      ReceiveStatus:=ReceiveStatusWaitForCommand;

   end CommandShutdown;
   ---------------------------------------------------------------------------

   Commands : constant array (FrontProtocol.ServerCmd_Type range <>) of Command_Access:=
     (FrontProtocol.ServerCmdPublicKey        => CommandPublicKey'Access,
      FrontProtocol.ServerCmdEncryptedMessage => CommandEncryptedMessage'Access,
      FrontProtocol.ServerCmdShutdown         => CommandShutdown'Access);

   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is

      pragma Unreferenced(Item);

      CallBack : ChannelCallBack_Access;
      Packet   : Packets.Packet_ClassAccess;

   begin

      CallBack         := new ChannelCallBack_Type;
      Channel.CallBack := Network.Streams.ChannelCallBack_ClassAccess(CallBack);
      CallBack.Channel := Channel;
      CallBack.User    := AnonymousUser;

      LogContext.NewChannel
        (ChannelName => ConcatElements(Channel.PeerAddress,U(";")),
         Channel     => CallBack.LogChannel);

      Packet:=new Packets.Packet_Type;
      Packet.Write(FrontProtocol.ServerID);
      Channel.SendPacket(Packet);

   end AAccept;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : in out ChannelCallBack_Type) is
   begin

      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Disconnecting");
      Item.LogChannel.FreeChannel;
      Item.LogChannel:=null;
      Network.Streams.Free(Item.Channel.CallBack);

   end Disconnect;
   ---------------------------------------------------------------------------

   procedure Receive
     (Item : in out ChannelCallBack_Type) is

      PrevPosition : Integer:=0;

   begin

      Put_Line("Receive:::");
      loop

         PrevPosition:=Item.Channel.Received.Position;
         case ReceiveStatus is
            when ReceiveStatusVerifyProtocol =>
               declare
                  ProtocolID : Unbounded_String;
               begin
                  ProtocolID:=Item.Channel.Received.Read;
                  if ProtocolID/=FrontProtocol.ClientID then
                     Item.LogChannel.Write
                       (Level   => Logging.LevelInvalid,
                        Message => "Invalid Front Protocol ID send by Client");
                     Item.Channel.Disconnect;
                     return;
                  end if;
               end;
               ReceiveStatus:=ReceiveStatusWaitForCommand;

            when ReceiveStatusWaitForCommand =>
               CurrentCommand := Item.Channel.Received.Read;
               if CurrentCommand not in Commands'Range then
                  Item.LogChannel.Write
                    (Level   => Logging.LevelInvalid,
                     Message => "Invalid Command called by Client");
                  Item.Channel.Disconnect;
               end if;
               ReceiveStatus  := ReceiveStatusProcessCommand;

            when ReceiveStatusProcessCommand =>
               Commands(CurrentCommand).all(Item);

         end case;

      end loop;

   exception
      when Packets.PacketOutOfData =>
         Put_Line("OutOfData::::");
         Item.Channel.Received.Position:=PrevPosition;

   end Receive;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin

      StreamImplementation:=Network.Streams.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Front.Network"));
      ReceiveStatus:=ReceiveStatusVerifyProtocol;
      Server:=StreamImplementation.NewServer
        (Configuration => Configuration,
         Node          => U("Front.Network"));
      Server.CallBack := ServerCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeServer(Server);
   end Finalize;
   ---------------------------------------------------------------------------

end SimFront.Server;
