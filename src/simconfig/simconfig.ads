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
--   5.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;

package SimConfig is

   SecondSetForElement : Exception;
   MissingOptionsEnd   : Exception;
   MissingSetEnd       : Exception;
   IntegerExpected     : Exception;
   InvalidElementType  : Exception;

   type GameType_Enum is
     (GameTypeSinglePlayer,
      GameTypeMultiPlayerSingleComputer,
      GameTypeMultiPlayerMultiComputer);

   type StringArray_Type is array(Integer range <>) of Unbounded_String;
   type StringArray_Access is access all StringArray_Type;

   type ConfigElem_Enum is
     (ConfigElemString,
      ConfigElemChoice,
      ConfigElemCheck,
      ConfigElemConstantString);

   type ConfigElem_Type;
   type ConfigArray_Type;
   type ConfigArray_Access is access all ConfigArray_Type;

   type ConfigSet_Type is
      record
         Key         : Unbounded_String;
         Description : Unbounded_String;
         Options     : ConfigArray_Access;
      end record;
   type ConfigSetArray_Type is array (Integer range <>) of ConfigSet_Type;
   type ConfigSetArray_Access is access all ConfigSetArray_Type;

   type ConfigElem_Type is
      record
         TType          : ConfigElem_Enum;
         Node           : Unbounded_String;
         Description    : Unbounded_String;
         Set            : ConfigSetArray_Access;
         Default        : Unbounded_String;
         DefaultIndex   : aliased Integer;
      end record;
   type ConfigElem_Access is access all ConfigElem_Type;

   type ConfigArray_Type is array(Integer range <>) of aliased ConfigElem_Type;

   function CreateConfigArrayFromConfiguration
     (Configuration : Config.Config_Type)
      return ConfigArray_Access;

   function LoadConfigArray
     (FileName : Unbounded_String)
      return ConfigArray_Access;

   procedure FreeConfigArray
     (ConfigArray : in out ConfigArray_Access);

   procedure DebugConfigArray
     (ConfigArray : ConfigArray_Access);

end SimConfig;
