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
--   24.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with ExceptionOutput;
with DistributedSystems.UseImplementations;
with Network.UseImplementations;

procedure Front is
begin

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;

   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
end Front;
