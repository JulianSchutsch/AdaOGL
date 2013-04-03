pragma Ada_2005;

with Network.Streams;
with Network.Packets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Logging;
with Basics; use Basics;
with ControlProtocol;

package body SimControl.ControlServer is

  type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusReady,
      ReceiveStatusInvalid);

   type ServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         LogChannel    : Logging.Channel_ClassAccess         := null;
         Channel       : Network.Streams.Channel_ClassAccess := null;
         ReceiveStatus : ReceiveStatus_Enum;
      end record;

   type ServerChannelCallBack_Access is
     access all ServerChannelCallBack_Type;

   overriding
   procedure OnReceive
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ServerChannelCallBack_Type);
   ---------------------------------------------------------------------------

   type ServerCallBack_Type is
     new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure OnAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);
   ---------------------------------------------------------------------------

   ServerCallBack       : aliased ServerCallBack_Type;
   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess := null;
   LogContext           : Logging.Context_ClassAccess        := null;
   LogMainChannel       : Logging.Channel_ClassAccess        := null;
   LogImplementation    : Logging.Implementation_Type;

   procedure OnReceive
     (Item : in out ServerChannelCallBack_Type) is

      PrevPosition : Integer;
      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition := Item.Channel.Position;
         case Item.ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identification : Unbounded_String;
               begin
                  Identification:=Item.Channel.Read;

                  if Identification/=ControlProtocol.ClientID then
                     Item.LogChannel.Write
                       (Level   => Logging.LevelInvalid,
                        Message => "Control-Client identification invalid");
                     -- TODO : Invalidate SendStatus as well
                     Item.ReceiveStatus := ReceiveStatusInvalid;
                     return;
                  else
                     Item.LogChannel.Write
                       (Level   =>  Logging.LevelEvent,
                        Message => "Control-Client identification valid");
                     Item.ReceiveStatus := ReceiveStatusReady;
                  end if;
               end;
            when ReceiveStatusReady =>
               return;
            when ReceiveStatusInvalid =>
               return;
         end case;
      end loop;
   exception
      when Network.Packets.PacketOutOfData =>
         Item.Channel.Position:= PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ServerChannelCallBack_Type) is
   begin
      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Disconnected Client");
      Item.LogChannel.FreeChannel;
      Network.Streams.Free(Item.Channel.CallBack);
   end OnDisconnect;
   ---------------------------------------------------------------------------

   procedure OnAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is
      pragma Unreferenced(Item);

      NewCallBack : ServerChannelCallBack_Access;
      Packet : Network.Packets.Packet_Access;

   begin
      NewCallBack:=new ServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ReceiveStatusWaitForIdentification;
      NewCallBack.Channel       := Channel;

      LogContext.NewChannel
        (ChannelName => ConcatElements
           (Item      => Channel.PeerAddress,
            Separator => To_Unbounded_String(";")),
         Channel     => NewCallBack.LogChannel);

      Channel.CallBack:=Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);
      LogMainChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Accepted Client");
      Put(Channel.PeerAddress);
      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(ControlProtocol.ServerID);
      Channel.SendPacket(Packet);
   end OnAccept;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is

   begin

      LogImplementation
        :=Logging.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Logging"));
      LogContext
        :=LogImplementation.NewContext
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control.Control"));

      LogContext.NewChannel
          (ChannelName => To_Unbounded_String("Server"),
           Channel     => LogMainChannel);

      StreamImplementation
        :=Network.Streams.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Control.Network"));

      StreamImplementation.Initialize.all;

      Server
        :=StreamImplementation.NewServer
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Control.Server.Network"));

      Server.CallBack:=ServerCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      LogImplementation.FreeContext
        (Item => LogContext);

      StreamImplementation.FreeServer
        (Item => Server);

      StreamImplementation.Finalize.all;

   end Finalize;
   ---------------------------------------------------------------------------

end SimControl.ControlServer;
