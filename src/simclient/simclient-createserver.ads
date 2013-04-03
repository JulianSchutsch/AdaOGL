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

-- Reasons for implementation:
--   A module for all spawn related details for ParallelSim.
--   This is mostly a forwarding module to SimClientGUI.CreateServer,
--   but defines what is executed and how often.
--   (GUI/Implementation separation)

pragma Ada_2005;

with Config;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package SimClient.CreateServer is

   type OnMessage_Access  is
     access procedure
       (Message : Unbounded_String);

   type OnSuccess_Access is
     access procedure;

   type OnFailure_Access is
     access procedure
       (SupplementConfig : Config.Config_Type);

   OnMessage : OnMessage_Access:=null;
   OnSuccess : OnSuccess_Access:=null;
   OnFailure : OnFailure_Access:=null;

   InvalidReinitialize : Exception;

   procedure Retry
     (SupplementConfig : Config.Config_Type);
   procedure Initialize
     (Configuration : Config.Config_Type);
   procedure Finalize;

end SimClient.CreateServer;
