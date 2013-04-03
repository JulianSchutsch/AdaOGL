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
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

package body Logging.StdOut is

   type Context_Type;
   type Context_Access is access all Context_Type;

   type Channel_Type;
   type Channel_Access is access all Channel_Type;
   type Channel_Type is new Logging.Channel_Type with
      record
         ModuleName  : Unbounded_String;
         ChannelName : Unbounded_String;
         Context     : Context_Access;
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

   type Context_Type is new Logging.Context_Type with
      record
         ModuleName : Unbounded_String;
         Debugging  : Boolean;
         Channels   : Channel_Access;
      end record;

   overriding
   procedure NewChannel
     (Item        : access Context_Type;
      ChannelName : Unbounded_String;
      Channel     : out Channel_ClassAccess);
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Context_Type,
      Name   => Context_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel_Type,
      Name   => Channel_Access);
   ---------------------------------------------------------------------------

   procedure Write
     (Item    : in out Channel_Type;
      Level   : Level_Enum;
      Message : String) is
      pragma Warnings(Off,Level); -- TEMP
   begin
      Put("[");
      Put(Head
        (Source => Item.ModuleName,
         Count  => 20));
      Put("][");
      Put(Head
        (Source => Item.ChannelName,
         Count => 20));
      Put("]:");
      Put(Message);
      New_Line;
   end Write;
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

   procedure NewChannel
     (Item        : access Context_Type;
      ChannelName : Unbounded_String;
      Channel     : out Channel_ClassAccess) is

      pragma Warnings(Off,Item);

      NewChannel : Channel_Access;

   begin
      NewChannel := new Channel_Type;
      NewChannel.ChannelName := ChannelName;
      NewChannel.ModuleName  := Item.ModuleName;
      NewChannel.Context     := Context_Access(Item);
      NewChannel.NextChannel := Item.Channels;
      if Item.Channels/=null then
         Item.Channels.LastChannel:=NewChannel;
      end if;
      Item.Channels:=NewChannel;
      Channel:=Channel_ClassAccess(NewChannel);
   end;
   ---------------------------------------------------------------------------

   function NewContext
     (Configuration : Config.Config_Type;
      ConfigNode    : Unbounded_String;
      ModuleName    : Unbounded_String)
      return Context_ClassAccess is

      pragma Unreferenced(Configuration);
      pragma Unreferenced(ConfigNode);

      NewContext : Context_Access;

   begin
      NewContext := new Context_Type;
      NewContext.Debugging := true;
      NewContext.ModuleName := ModuleName;
      return Context_ClassAccess(NewContext);
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

      Free(Context);

   end FreeContext;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access,
      FreeContext => FreeContext'Access);
   Identifier : constant Unbounded_String:=U("StdOut");

   procedure Register is
   begin
      Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);
   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin
      Implementations.Unregister(Identifier);
   end Unregister;
   ---------------------------------------------------------------------------

end Logging.StdOut;
