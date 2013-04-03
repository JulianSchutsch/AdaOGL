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
--   7.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;

package AdminProtocol is

   subtype ServerCmd_Type is Types.Integer32;

   ServerCmdMessage  : constant ServerCmd_Type:=0;
   ServerCmdShutdown : constant ServerCmd_Type:=1;

   ServerCmd_Msg_MaxLength : constant:=128;

   ServerID : constant Unbounded_String:=U("ParallelSimAdminServer");
   ClientID : constant Unbounded_String:=U("ParallelSimAdminClient");

end AdminProtocol;
