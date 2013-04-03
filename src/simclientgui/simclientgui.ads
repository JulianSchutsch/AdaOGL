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

with Config;
with GUI;
with GUI.Themes;

package SimClientGUI is

   ReenabledGUIModule  : Exception;
   RedisabledGUIModule : Exception;

   procedure Initialize
     (Configuration : Config.Config_Type);

   procedure Finalize;

   function Process
     return Boolean;

private

   GUIImplementation   : GUI.Implementation_Type;
   ThemeImplementation : GUI.Themes.Implementation_Type;
   GUIContext          : GUI.Context_ClassAccess:=null;
   Terminated          : Boolean:=false;

end SimClientGUI;
