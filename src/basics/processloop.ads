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
--   25.Feb 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   A potentially large number of packages require periodic execution of a
--   procedure. This is usually for a polling process of any kind.

-- Usage
--   Procedures can be removed with Add and Remove and are called in
--   the reverse order they are added.

with Basics; use Basics;

package ProcessLoop is

   type ProcAccess is
     access procedure
       (Object : AnyObject_ClassAccess);

   procedure Process;

   procedure Add
     (Proc   : ProcAccess;
      Object : AnyObject_ClassAccess);

   procedure Remove
     (Proc   : ProcAccess;
      Object : AnyObject_ClassAccess);

end ProcessLoop;
