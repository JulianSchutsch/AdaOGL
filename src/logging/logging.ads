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
--   4.Mar 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   Requirement for a high level logging sytem which can be customized for
--   example to work over network connections.

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config.Implementations;
with Basics; use Basics;

package Logging is

   type Level_Enum is
     (LevelException,
      LevelFailure,
      LevelInvalid,
      LevelDebug,
      LevelEvent,
      LevelRareEvent,
      LevelCommonEvent);

   for Level_Enum use
     (LevelException => 0,
      LevelFailure => 1,
      LevelInvalid => 2,
      LevelDebug => 3,
      LevelEvent => 4,
      LevelRareEvent => 5,
      LevelCommonEvent => 6);

   type Channel_Type is abstract tagged limited null record;
   type Channel_ClassAccess is access all Channel_Type'Class;

   procedure Write
     (Item    : in out Channel_Type;
      Level   : Level_Enum;
      Message : String) is abstract;

   procedure FreeChannel
     (Item : not null access Channel_Type) is abstract;
   ---------------------------------------------------------------------------

   type Context_Type is abstract tagged limited null record;
   type Context_ClassAccess is access all Context_Type'Class;

   procedure NewChannel
     (Item        : access Context_Type;
      ChannelName : Unbounded_String;
      Channel     : out Channel_ClassAccess) is abstract;
   ---------------------------------------------------------------------------

   type Context_Constructor is
     access function
       (Configuration : Config.Config_Type;
        ConfigNode    : Unbounded_String;
        ModuleName    : Unbounded_String)
        return Context_ClassAccess;

   type Context_Destructor is
     access procedure
       (Item : Context_ClassAccess);

   type Implementation_Type is
      record
         NewContext  : Context_Constructor:=null;
         FreeContext : Context_Destructor:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => U("Implementation"));

end Logging;
