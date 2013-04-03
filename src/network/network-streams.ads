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
--   3.Feb 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   This modul provides an abstract tagged type for easy replacement
--   of network stream implementations.
--   It also implements stream read and write acting on input and output
--   contents separately.

-- Usage
--   The network stream is available through two tagged types:
--
--    Server  : Accepts incoming connections and creates channels to
--              represent them.
--
--    Channel : Can either be a client side connection or a server side
--              connection. Acts as a stream with input and output separated.
--
--   Both communicate back by use of communication tagged types:
--
--     ServerCallBack
--     ChannelCallBack
--
--   Which are assigned to a callback entry in Server or Channel by the
--   using component (for example when OnAccept is called).
pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Basics; use Basics;
with Config.Implementations;
with Network.Packets;

package Network.Streams is

   IncompleteData   : Exception;
   InvalidData      : Exception;

   type ProcessAccess is
     access procedure;

   type Initialize_Access is
     access procedure;

   type Finalize_Access is
     access procedure;

   type ChannelCallBack_Type;
   type ChannelCallBack_ClassAccess is access all ChannelCallBack_Type'Class;

   type Channel_Type is abstract new Network.Packets.Packet_Type with
      record
         PeerAddress     : StringStringMap_Pack.Map;
         CallBack        : ChannelCallBack_ClassAccess:=null;
      end record;

   type Channel_ClassAccess is access all Channel_Type'Class;
   type Client_ClassAccess is access all Channel_Type'Class;

   type Client_Constructor is access function
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Client_ClassAccess;

   type Client_Destructor is access procedure
     (Item : in out Client_ClassAccess);

   procedure Disconnect
     (Item : access Channel_Type) is null;

   procedure SendPacket
     (Item   : access Channel_Type;
      Packet : Network.Packets.Packet_Access) is abstract;

   function SendBufferEmpty
     (Item : access  Channel_Type)
      return Boolean is abstract;

---------------------------------------------------------------------------

   type ChannelCallBack_Type is tagged limited
      record
         null;
      end record;

   procedure Receive
     (Item   : in out ChannelCallBack_Type) is null;

   procedure Connect
     (Item : in out ChannelCallBack_Type) is null;

   procedure Disconnect
     (Item : in out ChannelCallBack_Type) is null;

   procedure FailedConnect
     (Item  : in out ChannelCallBack_Type;
      Retry : in out Boolean) is null;
   ---------------------------------------------------------------------------

   type ServerCallBack_Type;
   type ServerCallBack_ClassAccess is access all ServerCallBack_Type'Class;

   type Server_Type is abstract tagged
      record
         CallBack : ServerCallBack_ClassAccess:=null;
      end record;
   type Server_ClassAccess is access all Server_Type'Class;

   type Server_Constructor is access function
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Server_ClassAccess;

   type Server_Destructor is access procedure
     (Item : in out Server_ClassAccess);
   ---------------------------------------------------------------------------

   type ServerCallBack_Type is tagged null record;

   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Channel_ClassAccess) is null;
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Server_Type'Class,
      Name   => Server_ClassAccess);
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ChannelCallBack_Type'Class,
      Name   => ChannelCallBack_ClassAccess);
   ---------------------------------------------------------------------------

   type Implementation_Type is
      record
         Initialize : Network.Streams.Initialize_Access  := null;
         Finalize   : Network.Streams.Finalize_Access    := null;
         NewServer  : Network.Streams.Server_Constructor := null;
         FreeServer : Network.Streams.Server_Destructor  := null;
         NewClient  : Network.Streams.Client_Constructor := null;
         FreeClient : Network.Streams.Client_Destructor  := null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => To_Unbounded_String("StreamImplementation"));

end Network.Streams;
