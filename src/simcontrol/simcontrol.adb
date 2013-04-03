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

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ProcessLoop;

with SimControl.AdminServer;
--with SimControl.ControlServer;

package body SimControl is

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      Terminated:=False;
      SimControl.AdminServer.Initialize
        (Configuration => Configuration);
--      SimControl.ControlServer.Initialize
--        (Configuration => Configuration);
   end Initialize;

   procedure Finalize is
   begin
--      SimControl.ControlServer.Finalize;
      SimControl.AdminServer.Finalize;
      Terminated:=True;
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return boolean is
   begin
      ProcessLoop.Process;
      return Terminated;
   end Process;
   ---------------------------------------------------------------------------

end SimControl;
