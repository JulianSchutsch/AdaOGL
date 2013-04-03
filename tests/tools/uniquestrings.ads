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
--   22.Feb 2012 Julian Schutsch
--    - Original version

-- Reasons for implementation
--   Package testing with strings as parameters cannot be done by testing
--   all possible combinations.
--   This unit is meant as a tool to provide a reasonable, random subset.
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package UniqueStrings is

   function AnyRandomString
     (MinimumLength : Positive;
      MaximumLength : Positive)
      return Unbounded_String;

   function UniqueRandomString
     (MinimumLength : Positive;
      MaximumLength : Positive)
      return Unbounded_String;

   procedure ClearUniqueStrings;

   function AnyPreviousRandomString
     return Unbounded_String;

end UniqueStrings;
