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

with Network.Streams;
with Network.Packets;
with Ada.Unchecked_Deallocation;
with LoggingProtocol;
with Ada.Text_IO; use Ada.Text_IO;

package body Logging.Client is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusInvalid);

   type Context_Type;
   type Context_Access is access all Context_Type;
   type Channel_Type;
   type Channel_Access is access all Channel_Type;

   type ClientCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         ReceiveStatus : ReceiveStatus_Enum;
         Context       : Context_Access:=null;
         Client        : Network.Streams.Client_ClassAccess:=null;
      end record;

   overriding
   procedure Connect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure FailedConnect
     (Item : in out ClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure Disconnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure Receive
     (Item : in out ClientCallBack_Type);

   type Context_Type is new Logging.Context_Type with
      record
         ModuleName     : Unbounded_String;
         Implementation : Network.Streams.Implementation_Type;
         Client         : Network.Streams.Client_ClassAccess:=null;
         CallBack       : aliased ClientCallBack_Type;
         Channels       : Channel_Access:=null;
      end record;

   overriding
   procedure NewChannel
     (Item        : access Context_Type;
      ChannelName : Unbounded_String;
      Channel     : out Channel_ClassAccess);
   ---------------------------------------------------------------------------

   type Channel_Type is new Logging.Channel_Type with
      record
         ChannelName : Unbounded_String;
         ModuleName  : Unbounded_String;
         Context     : Context_Access:=null;
         NextChannel : Channel_Access:=null;
         LastChannel : Channel_Access:=null;
      end record;

   overriding
   procedure Write
     (Item    : in out Channel_Type;
      Level   : Level_Enum;
      Message : String);

   overriding
   procedure FreeChannel
     (Item : not null access Channel_Type);
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Context_Type,
      Name => Context_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel_Type,
      Name   => Channel_Access);
   ---------------------------------------------------------------------------

   procedure FreeChannel
     (Item : not null access Channel_Type) is

      Channel : Channel_Access;

   begin

      Channel := Channel_Access(Item);
      if Channel.LastChannel/=null then
         Channel.LastChannel.NextChannel:=Channel.NextChannel;
      else
         Channel.Context.Channels:=Channel.NextChannel;
      end if;
      if Channel.NextChannel/=null then
         Channel.NextChannel.LastChannel:=Channel.LastChannel;
      end if;
      Free(Channel);

   end FreeChannel;
   ---------------------------------------------------------------------------

   procedure Write
     (Item    : in out Channel_Type;
      Level   : Level_Enum;
      Message : String) is

      use type Network.Streams.Client_ClassAccess;

      Packet : Network.Packets.Packet_Access;

   begin

      if Item.Context.Client/=null then
         Packet:=new Network.Packets.Packet_Type;
         Packet.Write(LoggingProtocol.ServerCmdLog);
         Packet.Write(LoggingProtocol.LevelInt_Type(Level_Enum'Pos(Level)));
         Packet.Write(Item.ModuleName);
         Packet.Write(Item.ChannelName);
         Packet.Write(To_Unbounded_String(Message));
         Packet.Debug;
         Item.Context.Client.SendPacket(Packet);
      end if;

   end Write;
   ---------------------------------------------------------------------------

   procedure NewChannel
     (Item        : access Context_Type;
      ChannelName : Unbounded_String;
      Channel     : out Channel_ClassAccess) is

      NewChannel : Channel_Access;

   begin

      NewChannel             := new Channel_Type;
      NewChannel.ChannelName := ChannelName;
      NewChannel.ModuleName  := Item.ModuleName;
      NewChannel.Context     := Context_Access(Item);
      NewChannel.NextChannel := Item.Channels;
      if Item.Channels/=null then
         Item.Channels.LastChannel:=NewChannel;
      end if;
      Item.Channels:=NewChannel;
      Channel:=Channel_ClassAccess(NewChannel);

   end NewChannel;
   ---------------------------------------------------------------------------

   procedure FailedConnect
     (Item : in out ClientCallBack_Type;
      Retry : in out Boolean) is

      pragma Unreferenced(Item);

   begin

      Put("Failed Connect!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      New_Line;
      Ada.Text_IO.Flush(Ada.Text_IO.Standard_Output);
      Retry:=True;

   end FailedConnect;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : in out ClientCallBack_Type) is

   begin

      Item.Context.Client:=null;

   end Disconnect;
   ---------------------------------------------------------------------------

   procedure Receive
     (Item : in out ClientCallBack_Type) is

      PrevPosition : Integer;
      pragma Warnings(Off,PrevPosition);

   begin

      loop
         PrevPosition := Item.Client.Position;
         case Item.ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identification : Unbounded_String;
               begin
                  Identification:=Item.Client.Read;
                  if Identification/=LoggingProtocol.ServerID then
                     Item.Client.Disconnect;
                     Item.ReceiveStatus:=ReceiveStatusInvalid;
                     return;
                  end if;
               end;
               -- There is no more data to be expected
               -- for this implementation at the moment
               Item.ReceiveStatus:=ReceiveStatusInvalid;

            when ReceiveStatusInvalid =>
               return;

         end case;

      end loop;

   exception
      when Network.Packets.PacketOutOfData =>
         Item.Client.Position:=PrevPosition;

   end Receive;
   ---------------------------------------------------------------------------

   procedure Connect
     (Item : in out ClientCallBack_Type) is

   begin

      -- Send identification as Logging Client
      Put("Send Identification Packet!!!!!!!!!!!!!!!!!!");
      New_Line;

      Item.ReceiveStatus:=ReceiveStatusWaitForIdentification;

   end Connect;
   ---------------------------------------------------------------------------

   function NewContext
     (Configuration : Config.Config_Type;
      ConfigNode    : Unbounded_String;
      ModuleName    : Unbounded_String)
      return Context_ClassAccess is

      NewCont : Context_Access;

      Packet : Network.Packets.Packet_Access;

   begin

      NewCont:=new Context_Type;
      NewCont.ModuleName := ModuleName;

      Put_Line("Logging.Client.Find Impl");
      NewCont.Implementation:=Network.Streams.Implementations.Find
        (Configuration => Configuration,
         Node          => ConfigNode & ".Network");
      Put_Line("Logging.Client.NewClient");
      NewCont.Implementation.Initialize.all;
      NewCont.Client:=NewCont.Implementation.NewClient
        (Configuration => Configuration,
         Node          => ConfigNode & ".Network");
      Put_Line("Logging.Client...");

      NewCont.CallBack.Context := NewCont;
      NewCont.CallBack.Client  := NewCont.Client;
      NewCont.Client.CallBack  := NewCont.CallBack'Access;

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(LoggingProtocol.ClientID);
      Packet.Debug;
      NewCont.Client.SendPacket(Packet);
      Flush(Standard_Output);

      return Context_ClassAccess(NewCont);

   end NewContext;
   ---------------------------------------------------------------------------

   procedure FreeContext
     (Item : Context_ClassAccess) is

      Context     : Context_Access;
      Channel     : Channel_Access;
      NextChannel : Channel_Access;

   begin

      Context:=Context_Access(Item);

      Channel:=Context.Channels;
      while Channel/=null loop
         NextChannel:=Channel.NextChannel;
         Channel.FreeChannel;
         Channel:=NextChannel;
      end loop;

      Context.Implementation.Finalize.all;

      Free(Context);

   end FreeContext;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access,
      FreeContext => FreeContext'Access);
   Identifier     : constant Unbounded_String:=U("Network");

   procedure Register is
   begin

      Implementations.Register
        (Implementation => Implementation,
         Identifier     => Identifier);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister(Identifier);

   end Unregister;
   ---------------------------------------------------------------------------

end Logging.Client;
