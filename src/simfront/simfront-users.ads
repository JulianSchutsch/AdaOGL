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
--   30.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Authentication;
with Basics; use Basics;
with FrontProtocol; use FrontProtocol;

package SimFront.Users is

   type User_Type is
      record
         Nick       : Unbounded_String;
         Privileges : Privileges_Type;
         PublicKey  : Authentication.PublicKey_ClassAccess:=null;
      end record;

   AnonymousUser : User_Type:=
     (Nick       => U("Anonymous"),
      Privileges =>
        (PrivilegePlay         => False,
         PrivilegeTerraform    => False,
         PrivilegeModerator    => False,
         PrivilegeAdmin        => False),
      PublicKey => null);

   AuthenticationGenerator : Authentication.Generator_ClassAccess:=null;
   AuthenticationImpl      : Authentication.Implementation_Type;

   procedure Initialize;
   procedure Finalize;

end SimFront.Users;
