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
--   18.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers; use Ada.Containers;
with Network.UseImplementations;
with DistributedSystems;
with DistributedSystems.UseImplementations;
with Config;
with Errors;
with ProcessLoop;
with ProgramArguments;

procedure Spawn is

   SpawnObject      : DistributedSystems.Spawn_ClassAccess;
   Implementation   : DistributedSystems.Implementation_Type;
   Configuration    : Config.Config_Type;
   SupplementConfig : Config.Config_Type;
   Terminated       : Boolean:=False;
   -- Variable Terminated is not modified in the main loop
   pragma Warnings(Off,Terminated);

   procedure SpawnMessage
     (Message : Unbounded_String) is
   begin
      Put_Line("* "&To_String(Message));
   end SpawnMessage;
   ---------------------------------------------------------------------------

   procedure SpawnSuccess is
   begin
      Put_Line("Spawn success.");
   end SpawnSuccess;
   ---------------------------------------------------------------------------

   procedure SpawnFailure
     (Error            : Errors.Error_Type;
      SupplementConfig : Config.Config_Type) is
      pragma Unreferenced(SupplementConfig);
   begin
      Put_Line("Error:"&To_String(Error.Error));
      Put_Line("Spawn failure.");
      Terminated := True;
   end SpawnFailure;
   ---------------------------------------------------------------------------

   procedure SpawnTerminate is
   begin
      Terminated:=True;
   end SpawnTerminate;
   ---------------------------------------------------------------------------

begin

   ProgramArguments.Initialize;

   if ProgramArguments.Parameters.Length=0 then
      Put_Line("Please specify program to run as node");
      return;
   end if;

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;

   Implementation:=DistributedSystems.Implementations.FindAny;
   Implementation.CreateSpawnObject
     (Configuration =>  Configuration,
      Executables   => (0=>(Executable => ProgramArguments.Parameters.Element(0),Amount => 2)),
      SpawnObject   => SpawnObject);
   SpawnObject.OnMessage   := SpawnMessage'Unrestricted_Access;
   SpawnObject.OnSuccess   := SpawnSuccess'Unrestricted_Access;
   SpawnObject.OnFailure   := SpawnFailure'Unrestricted_Access;
   SpawnObject.OnTerminate := SpawnTerminate'Unrestricted_Access;

   SpawnObject.Execute(SupplementConfig);

   loop
      ProcessLoop.Process;
      exit when Terminated;
   end loop;

   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

end Spawn;
