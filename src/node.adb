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
--   13.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with ExceptionOutput;
with DistributedSystems.UseImplementations;
with Network.UseImplementations;
with Logging.Client;
with Logging.StdOut;
with Config;
with ProgramArguments;
with DistributedSystems;
with Basics; use Basics;
with SimNode;
--with Ada.Text_IO; use Ada.Text_IO;

procedure Node is

   Configuration          : Config.Config_Type;
   DistributedSystemsImpl : DistributedSystems.Implementation_Type;

begin
   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;
   Logging.Client.Register;
   Logging.StdOut.Register;

   ProgramArguments.Initialize;

   ProgramArguments.Configuration.Debug;
   DistributedSystemsImpl
     := DistributedSystems.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        Node          => U("Arguments"));

   DistributedSystemsImpl.InitializeNode
     (Configuration => Configuration,
      Group         => 0); -- TODO:Group ID not yet valid
   Configuration.Debug;

   SimNode.Initialize(Configuration);

   loop
      exit when SimNode.Process;
   end loop;

   SimNode.Finalize;

   DistributedSystemsImpl.FinalizeNode.all;

   Logging.StdOut.Unregister;
   Logging.Client.Unregister;
   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
end Node;
