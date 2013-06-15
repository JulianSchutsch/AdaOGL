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
--   18.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Win32;

package Processes_Plattform is

   CurrentPathPrefix : Unbounded_String:=U("");

   type Processes_Type is
      record
         StdOutPipeIn       : aliased Win32.HANDLE_Type := Win32.NULLHANDLE;
         StdOutPipeOut      : aliased Win32.HANDLE_Type := Win32.NULLHANDLE;
         ProcessHandle      : Win32.HANDLE_Type         := Win32.NULLHANDLE;
      end record;

end Processes_Plattform;
