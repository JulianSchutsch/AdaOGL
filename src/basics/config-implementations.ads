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
--   5.Mar 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   To simplify selection of implementations using a configuration.
--   This avoids a lot of boiler plate code.

-- Usage
--   Specialise this package using a custom Implementation_Type in the same
--   package as the interface definition.
--   This type usually includes access pointers for initialisation,
--   finalisation of contexts of any kind.
--
--   A new implementation is added to the container using the Register
--   function giving a unique name for each implementation.

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Implementation_Type is private;
   IdentifierKey : Unbounded_String;

package Config.Implementations is

   ImplementationNotFound : Exception;

   procedure Register
     (Identifier     : Unbounded_String;
      Implementation : Implementation_Type);

   procedure Unregister
     (Identifier : Unbounded_String);

   -- Find selects an identifier from the Configuration using ModuleName and
   -- IdentifierKey and returns an implementation with that identifier.
   function Find
     (Configuration : Config_Type;
      Node          : Unbounded_String)
      return Implementation_Type;

   -- Find returns an implementation with ImplementationName as identifier.
   function Find
     (ImplementationName : Unbounded_String)
      return Implementation_Type;

   function FindAny
     return Implementation_Type;

   procedure Debug;

end Config.Implementations;
