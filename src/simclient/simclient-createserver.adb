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
------------------------------------------------------------------------------

pragma Ada_2005;

with DistributedSystems;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;

package body SimClient.CreateServer is

   use type DistributedSystems.Spawn_ClassAccess;

   Spawn            : DistributedSystems.Spawn_ClassAccess:=null;
   SupplementConfig : Config.Config_Type;

   procedure Execute is
   begin
      Spawn.Execute(SupplementConfig);
   end Execute;
   ---------------------------------------------------------------------------

   procedure ExecuteFailure
     (SupplementConfig : Config.Config_Type) is
   begin
      if OnFailure/=null then
         OnFailure(SupplementConfig);
      end if;
   end ExecuteFailure;
   ---------------------------------------------------------------------------

   procedure ExecuteSuccess is
   begin
      if OnSuccess/=null then
         OnSuccess.all;
      end if;
   end ExecuteSuccess;
   ---------------------------------------------------------------------------

   procedure ExecuteMessage
     (Message : Unbounded_String) is
   begin
      if OnMessage/=null then
         OnMessage(Message);
      end if;
   end ExecuteMessage;
   ---------------------------------------------------------------------------

   procedure Retry
     (SupplementConfig : Config.Config_Type) is
   begin
      Standard.SimClient.CreateServer.SupplementConfig:=SupplementConfig;
      Execute;
   end Retry;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration  : Config.Config_Type) is
      Implementation : DistributedSystems.Implementation_Type;
      Executables    : DistributedSystems.ExecutableArray_Type(1..1);
   begin
      Put_Line("SimClient.CreateServer.initialize");
      if Spawn/=null then
         raise InvalidReinitialize;
      end if;
      SupplementConfig.Clear;
      Implementation:=DistributedSystems.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Distribution"));
--      Executables(1).Executable:=U("front");
--      Executables(1).Amount:=1;
      Executables(1).Executable:=U("node");
      Executables(1).Amount:=1;
      Implementation.CreateSpawnObject(Configuration,Executables,Spawn);
      Spawn.OnMessage:=ExecuteMessage'Access;
      Spawn.OnFailure:=ExecuteFailure'Access;
      Spawn.OnSuccess:=ExecuteSuccess'Access;
      Execute;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      Put_Line("SimClient.CreateServer.finalize");
      if Spawn/=null then
         Spawn.Free;
         Spawn:=null;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

end SimClient.CreateServer;
