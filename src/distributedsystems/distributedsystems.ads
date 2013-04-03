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
--   20.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Config;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;

package DistributedSystems is

   FailedNodeInitialization : Exception;

   type Group_Type is new Integer;
   type Node_Type is new Integer;

   MyGroup       : Group_Type;
   MyGlobalID    : Node_Type;
   FirstGlobalID : Node_Type;
   LastGlobalID  : Node_Type;
   MyGroupID     : Node_Type;
   FirstGroupID  : Node_Type;
   LastGroupID   : Node_Type;

   type InitializeNode_Access is
     access procedure
       (Configuration : out Config.Config_Type;
        Group         : Group_Type);

   type FinalizeNode_Access is
     access procedure;

   type Executable_Type is
      record
         Executable : Unbounded_String;
         Amount     : Positive;
      end record;

   type ExecutableArray_Type is array (Integer range <>) of Executable_Type;

   type OnMessage_Access is
     access procedure
       (Message : Unbounded_String);

   type OnFailure_Access is
     access procedure
       (SupplementConfig : Config.Config_Type);

   type OnSuccess_Access is
     access procedure;

   type Spawn_Type is new AnyObject_Type with
      record
         OnMessage : OnMessage_Access:=null;
         OnFailure : OnFailure_Access:=null;
         OnSuccess : OnSuccess_Access:=null;
      end record;

   type Spawn_Access is access all Spawn_Type;
   type Spawn_ClassAccess is access all Spawn_Type'Class;

   -- Execution of all the nodes selected when creating the Spawn Object
   -- (by Executables parameter)
   -- The SupplementConfig can be empty if Execute is called first.
   -- In case of failure, the SupplementConfig may contain a list
   -- of necessary options+default values which may enable Execute to
   -- proceed.
   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type);

   procedure Free
     (Item : access Spawn_Type);

   type CreateSpawnObject_Access is
     access procedure
       (Configuration : Config.Config_Type;
        Executables   : ExecutableArray_Type;
        SpawnObject   : out Spawn_ClassAccess);

   type Implementation_Type is
      record
         InitializeNode    : InitializeNode_Access    := null;
         FinalizeNode      : FinalizeNode_Access      := null;
         CreateSpawnObject : CreateSpawnObject_Access := null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       =>
      To_Unbounded_String("DistributedSystemsImplementation"));

end DistributedSystems;
