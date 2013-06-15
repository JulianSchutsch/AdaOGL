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
with Authentication;
with Packets;
with Network.Streams;
with Basics; use Basics;
with FrontProtocol;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Types; use Types;

package body SimClient.Front is

   type ReceiveStatus_Enum is
     (ReceiveStatusVerifyProtocol,
      ReceiveStatusWaitForCommand,
      ReceiveStatusProcessCommand);

   type ClientCallBack_Type is new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure Connect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure Disconnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure FailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure Receive
     (Item : in out ClientCallBack_Type);
   ---------------------------------------------------------------------------
   StreamImplementation    : Network.Streams.Implementation_Type;
   AuthenticationImpl      : Authentication.Implementation_Type;
   AuthenticationGenerator : Authentication.Generator_ClassAccess:=null;
   Client                  : Network.Streams.Client_ClassAccess:=null;
   ClientCallBack          : aliased ClientCallBack_Type;
   PublicKey               : Authentication.PublicKey_ClassAccess:=null;
   PrivateKey              : Authentication.PrivateKey_ClassAccess:=null;
   ReceiveStatus           : ReceiveStatus_Enum;
   CurrentCommand          : FrontProtocol.ClientCmd_Type;
   Privileges              : FrontProtocol.Privileges_Type;

   type Command_Access is
     access procedure;

   procedure CommandEncryptMessage is

      Message   : Unbounded_String;
      Encrypted : Unbounded_String;
      Packet    : Packets.Packet_ClassAccess;

   begin

      Put_Line("Encrypt");
      Message   := Client.Received.Read;
      Encrypted := PrivateKey.Encrypt(Message);
      Packet:=new Packets.Packet_Type;
      Packet.Write(FrontProtocol.ServerCmdEncryptedMessage);
      Packet.Write(Encrypted);
      Client.SendPacket(Packet);

      ReceiveStatus:=ReceiveStatusWaitForCommand;

   end CommandEncryptMessage;
   ---------------------------------------------------------------------------

   procedure CommandNotifyPrivileges is
   begin

      Put_Line("Notify Priv");

      for Privilege in Frontprotocol.Privileges_Enum'Range loop

         declare
            Val : constant Integer32:=Client.Received.Read;
         begin
            Privileges(Privilege):=Val=1;
         end;

      end loop;

      for Privilege in FrontProtocol.Privileges_Enum'Range loop
         if Privileges(Privilege) then
            Put_Line("Priv..");
         else
            Put_Line("NoPriv");
         end if;
      end loop;

      ReceiveStatus:=ReceiveStatusWaitForCommand;

   end CommandNotifyPrivileges;
   ---------------------------------------------------------------------------

   Commands : constant array (FrontProtocol.ClientCmd_Type range <>) of Command_Access:=
     (FrontProtocol.ClientCmdEncryptMessage => CommandEncryptMessage'Access,
      FrontProtocol.ClientCmdNotifyPrivileges => CommandNotifyPrivileges'Access);

   procedure Receive
     (Item : in out ClientCallBack_Type) is

      pragma Unreferenced(Item);

      PrevPosition : Integer:=0;

   begin

      Put_Line("Receive");

      loop

         PrevPosition:=Client.Received.Position;

         case ReceiveStatus is
            when ReceiveStatusVerifyProtocol =>
               declare
                  ProtocolID : Unbounded_String;
               begin
                  ProtocolID:=Client.Received.Read;
                  if ProtocolID/=FrontProtocol.ServerID then
                     LogMainChannel.Write
                       (Level   => Logging.LevelInvalid,
                        Message => "Invalid Front Server ID");
                     Client.Disconnect;
                     return;
                  end if;
                  ReceiveStatus:=ReceiveStatusWaitForCommand;
               end;

            when ReceiveStatusWaitForCommand =>
               CurrentCommand := Client.Received.Read;
               ReceiveStatus  := ReceiveStatusProcessCommand;

            when ReceiveStatusProcessCommand =>
               Commands(CurrentCommand).all;

         end case;

      end loop;

   exception
      when Packets.PacketOutOfData =>
         Put_Line("OutOfData");
         Client.Received.Position:=PrevPosition;
   end Receive;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : in out ClientCallBack_Type) is

      pragma Unreferenced(Item);

   begin

      Put_Line("aDisconnecting");
      if OnDisconnect/=null then
         OnDisconnect.all;
      end if;
      Put_Line("Disconnect.done");

   end Disconnect;
   ---------------------------------------------------------------------------

   procedure Connect
     (Item : in out ClientCallBack_Type) is

      Pragma Unreferenced(Item);

   begin

      Put_Line("Trivial Connect");
      declare
         Packet : Packets.Packet_ClassAccess;
      begin
         Packet:=new Packets.Packet_Type;
         Packet.Write(FrontProtocol.ClientID);
         Client.SendPacket(Packet);

         Packet:=new Packets.Packet_Type;
         Packet.Write(FrontProtocol.ServerCmdPublicKey);
         PublicKey.WriteToPacket(Packet);
         Client.SendPacket(Packet);
      end;
      Put_Line("Trivial Connect done");

   end Connect;
   ---------------------------------------------------------------------------

   procedure FailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean) is

      pragma Unreferenced(Item);

   begin

      Put_Line("Failed Connect(Client SYS)");
      if OnFailedConnect/=null then
         OnFailedConnect(Retry);
      end if;

   end FailedConnect;
   ---------------------------------------------------------------------------

   procedure Connect
     (Configuration : Config.Config_Type) is
   begin

      ReceiveStatus:=ReceiveStatusVerifyProtocol;

      AuthenticationImpl:=Authentication.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Front.Authentication"));
      AuthenticationGenerator:=AuthenticationImpl.NewGenerator
        (Configuration => Configuration,
         Node          => U("Front.Authentication"));
      AuthenticationGenerator.GenerateKeyPair
        (PublicKey  => PublicKey,
         PrivateKey => PrivateKey);

      StreamImplementation:=Network.Streams.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Front.Network"));
      Client:=StreamImplementation.NewClient
        (Configuration => Configuration,
         Node          => U("Front.Network"));
      Client.CallBack:=ClientCallBack'Access;

   end Connect;
   ---------------------------------------------------------------------------

   procedure Disconnect is
   begin

      Put_Line("Call Disconnect");
      AuthenticationImpl.FreeGenerator(AuthenticationGenerator);
      StreamImplementation.FreeClient(Client);

   end Disconnect;
   ---------------------------------------------------------------------------

end SimClient.Front;
