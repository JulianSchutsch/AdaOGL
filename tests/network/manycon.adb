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
--   2.Jul 2012 Julian Schutsch
--     - Original version

with Network.Streams;
with Network.UseImplementations;
with Config;
with Basics; use Basics;
with ExceptionOutput;
with ProcessLoop;

with Ada.Text_IO; use Ada.Text_IO;

procedure ManyCon is

   ClientCount : constant := 5;
   Accepted : Integer:=0;
   pragma Warnings(Off,Accepted);

   Implementation : Network.Streams.Implementation_Type;
   Server  : Network.Streams.Server_ClassAccess;
   Clients : array(1..ClientCount) of Network.Streams.Client_ClassAccess
     :=(others => null);

   type ServerCallBack_Type is new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);

   type ClientCallBack_Type is new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure Connect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure FailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean);

   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is
      pragma Unreferenced(Item);
      pragma Unreferenced(Channel);
   begin
      Put_Line("Accepting...");
      Accepted:=Accepted+1;
   end AAccept;
   ---------------------------------------------------------------------------

   procedure FailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Unreferenced(Item);
   begin
      Put_Line("Failed Connect");
      Retry:=True;
   end FailedConnect;
   ---------------------------------------------------------------------------

   procedure Connect
     (Item : in out ClientCallBack_Type) is
      pragma Unreferenced(Item);
   begin
      Put_Line("Connected");
   end Connect;
   ---------------------------------------------------------------------------

   Configuration  : Config.Config_Type;
   ServerCallBack : aliased ServerCallBack_Type;
   ClientCallBack : aliased ClientCallBack_Type;
   Counter  : Integer:=0;

begin

   Configuration.Insert(U(".Family"),U("IPv4"));
   Configuration.Insert(U(".BindIP"),U("0.0.0.0"));
   Configuration.Insert(U(".BindPort"),U("50000"));
   Configuration.Insert(U(".RemoteIP"),U("127.0.0.1"));
   Configuration.Insert(U(".RemotePort"),U("50000"));

   Network.UseImplementations.Register;
   Implementation:=Network.Streams.Implementations.FindAny;

   Implementation.Initialize.all;

   Server:=Implementation.NewServer(Configuration,U(""));
   Server.CallBack:=ServerCallBack'Unrestricted_Access;

   for i in Clients'Range loop
      Clients(i):=Implementation.NewClient(Configuration,U(""));
      Clients(i).CallBack:=ClientCallBack'Unrestricted_Access;
   end loop;
   -- Wait for all this to happen

   while Accepted<ClientCount loop
      if Counter=1 then
         Put("*");
         Counter:=0;
      end if;
      Counter:=Counter+1;
      ProcessLoop.Process;
   end loop;

   for i in Clients'Range loop
      Put_Line("Destroy Client "&Integer'Image(i));
      Implementation.FreeClient(Clients(i));
   end loop;

   Implementation.FreeServer(Server);

   Implementation.Finalize.all;

   Network.UseImplementations.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
end ManyCon;
------------------------------------------------------------------------------
