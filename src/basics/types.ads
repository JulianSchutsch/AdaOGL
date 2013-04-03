-------------------------------------------------------------------------------
--   Copyright 2011 Julian Schutsch
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
--   25.Jan 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package Types is

   type Integer8 is range -2**7..2**7-1;
   type Integer16 is range -2**15..2**15-1;
   type Integer32 is range -2**31..2**31-1;
   type Integer64 is range -2**63..2**63-1;

   type Cardinal8 is range 0..2**8-1;
   type Cardinal16 is range 0..2**16-1;
   type Cardinal32 is range 0..2**32-1;
   type Cardinal64 is mod 2**64;

end Types;
