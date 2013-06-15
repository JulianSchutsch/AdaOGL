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
--  30.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;

package FrontProtocol is

   type Privileges_Enum is
     (PrivilegePlay,
      PrivilegeTerraform,
      PrivilegeModerator,
      PrivilegeAdmin);

   type Privileges_Type is array(Privileges_Enum) of Boolean;

   subtype ServerCmd_Type is Types.Integer32;
   subtype ClientCmd_Type is Types.Integer32;

   ServerCmdPublicKey        : constant ServerCmd_Type:=0;
   ServerCmdEncryptedMessage : constant ServerCmd_Type:=1;
   ServerCmdShutdown         : constant ServerCmd_Type:=2;

   ClientCmdEncryptMessage   : constant ClientCmd_Type:=0;
   ClientCmdNotifyPrivileges : constant ClientCmd_Type:=1;

   ServerID : constant Unbounded_String:=U("ParallelSimFrontServer");
   ClientID : constant Unbounded_String:=U("ParallelSimFrontClient");

end FrontProtocol;
