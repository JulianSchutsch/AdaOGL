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
--   21.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Basics; use Basics;
with Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package LoggingProtocol is

   subtype ServerCmd_Type is Types.Integer32;
   subtype LevelInt_Type is Types.Integer32;

   ServerCmdLog : constant ServerCmd_Type:=0;

   ServerID : constant Unbounded_String:=U("ParallelSimLoggingServer");
   ClientID : constant Unbounded_String:=U("ParallelSimLoggingClient");

end LoggingProtocol;
