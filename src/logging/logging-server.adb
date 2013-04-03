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
with LoggingProtocol;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Network.Packets;

package body Logging.Server is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusWaitForCommand,
      ReceiveStatusWaitForCommandData,
      ReceiveStatusInvalid);

   type ServerCallBack_Type is
     new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure AAccept
     (Item    : in out ServercallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);
   ---------------------------------------------------------------------------

   type ServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         Channel        : Network.Streams.Channel_ClassAccess:=null;
         ReceiveStatus  : ReceiveStatus_Enum;
         CurrentCommand : LoggingProtocol.ServerCmd_Type;
      end record;
   type ServerChannelCallBack_Access is access all ServerChannelCallBack_Type;

   overriding
   procedure Disconnect
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure Receive
     (Item : in out ServerChannelCallBack_Type);
   ---------------------------------------------------------------------------

   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess:=null;
   ServerCallBack       : aliased ServerCallBack_Type;
   ---------------------------------------------------------------------------

   procedure Cmd_LoggingServerLog
     (Item     : in out ServerChannelCallBack_Type;
      Complete : out Boolean) is

      Level       : Level_Enum;
      LevelInt    : LoggingProtocol.LevelInt_Type;
      ModuleName  : Unbounded_String;
      ChannelName : Unbounded_String;
      Message     : Unbounded_String;

   begin
      LevelInt:=item.Channel.Read;

      if LevelInt not in Level_Enum'Pos(Level_Enum'First)..Level_Enum'Pos(Level_Enum'Last) then
         Put("Invalid message received");
         New_Line;
         Item.ReceiveStatus:=ReceiveStatusInvalid;
         Item.Channel.Disconnect;
         Complete:=False;
      end if;

      Level       := Level_Enum'Val(LevelInt);
      ModuleName  := Item.Channel.Read;
      ChannelName := Item.Channel.Read;
      Message     := Item.Channel.Read;

      if OnLogEvent/=null then
         OnLogEvent
           (Source  => Item.Channel.PeerAddress,
            Level   => Level,
            Module  => ModuleName,
            Channel => ChannelName,
            Message => Message);
      else
         Put("Source  :");
         Put(To_String
           (ConcatElements
                (Item      => (Item.Channel.PeerAddress),
                 Separator =>U(";"))));
         New_Line;
         Put("Level   :");
         Put(Level_Enum'Image(Level));
         New_Line;
         Put("Module  :");
         Put(To_String(ModuleName));
         New_Line;
         Put("Channel :");
         Put(To_String(ChannelName));
         New_Line;
         Put("Message :");
         Put(To_String(Message));
         New_Line;
      end if;

      Complete    := True;

   end Cmd_LoggingServerLog;
   ---------------------------------------------------------------------------

   type CmdArray is array (LoggingProtocol.ServerCmd_Type range <>) of
     access procedure
       (Item     : in out ServerChannelCallBack_Type;
        Complete : out Boolean);

   Cmds:constant CmdArray:=
     (LoggingProtocol.ServerCmdLog  => Cmd_LoggingServerLog'Access);

   procedure Receive
     (Item : in out ServerChannelCallBack_Type) is

      PrevPosition : Integer;
      pragma Warnings(Off,PrevPosition);

   begin

      Put("Receive");
      New_Line;
      Item.Channel.Debug;
      Put(":::");
      New_Line;

      loop

         PrevPosition:=Item.Channel.Position;

         case Item.ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identification : Unbounded_String;
               begin
                  Identification:=Item.Channel.Read;
                  if Identification/=LoggingProtocol.ClientID then
                     Put("Invalid Identification by Client");
                     Put(To_String(Identification));
                     New_Line;
                     Item.ReceiveStatus:=ReceiveStatusInvalid;
                     Item.Channel.Disconnect;
                     return;
                  end if;
               end;

               Put("New Logging Client");
               New_Line;
               Item.ReceiveStatus:=ReceiveStatusWaitForCommand;

            when ReceiveStatusWaitForCommand =>
               Item.CurrentCommand:=Item.Channel.Read;
               if Item.CurrentCommand not in Cmds'Range then
                  Put("Command not in valid range");
                  Put(Integer(Item.CurrentCommand));
                  New_Line;
                  Item.ReceiveStatus:=ReceiveStatusInvalid;
                  Item.Channel.Disconnect;
                  return;
               end if;
               Item.ReceiveStatus:=ReceiveStatusWaitForCommandData;

            when ReceiveStatusWaitForCommandData =>

               declare
                  Complete : Boolean;
               begin
                  Cmds(Item.CurrentCommand).all(Item,Complete);
                  if Complete then
                     Item.ReceiveStatus:=ReceiveStatusWaitForCommand;
                  end if;
               end;

            when ReceiveStatusInvalid =>
               Put("Invalid Status Receive?");
               New_Line;
               return;

         end case;

      end loop;

   exception

      when Network.Packets.PacketOutOfData =>
         Item.Channel.Position:=PrevPosition;

   end Receive;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : in out ServerChannelCallBack_Type) is
   begin

      Put("Disconnect");
      New_Line;
      Network.Streams.Free(Item.Channel.CallBack);
      Put("...");
      New_Line;

   end Disconnect;
   ---------------------------------------------------------------------------

   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is

      pragma Unreferenced(Item);

      NewCallBack : ServerChannelCallBack_Access;
      Packet      : Network.Packets.Packet_Access;

   begin

      Put("Accept Logging Client");
      New_Line;

      NewCallBack:=new ServerChannelCallBack_Type;
      NewCallBack.Channel:=Channel;
      NewCallBack.ReceiveStatus:=ReceiveStatusWaitForIdentification;

      Channel.CallBack
        :=Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(LoggingProtocol.ServerID);
      Channel.SendPacket(Packet);

   end AAccept;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      StreamImplementation:=Network.Streams.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Logging.Network"));

      StreamImplementation.Initialize.all;

      Server:=StreamImplementation.NewServer
        (Configuration => Configuration,
         Node          => U("Logging.Network"));

      Server.CallBack:=ServerCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      StreamImplementation.FreeServer(Server);

      StreamImplementation.Finalize.all;

   end Finalize;
   ---------------------------------------------------------------------------

end Logging.Server;
