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
--   1.Jul 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Config;

package SimClient.Front is

   type OnConnect_Access is
     access procedure;
   type OnDisconnect_Access is
     access procedure;
   type OnFailedConnect_Access is
     access procedure
       (Retry : out Boolean);

   OnConnect       : OnConnect_Access:=null;
   OnDisconnect    : OnDisconnect_Access:=null;
   OnFailedConnect : OnFailedConnect_Access:=null;

   procedure Connect
     (Configuration : Config.Config_Type);
   procedure Disconnect;

end SimClient.Front;
