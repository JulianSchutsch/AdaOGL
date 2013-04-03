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

with GUI;
with GUI.Label;
with GUI.ComboBox;
with GUI.Edit;
with GUI.CheckBox;
with GUI.Themes;
with Basics; use Basics;
with Config;

package SimConfig.Visual is

   InvalidDefaultIndexForCheck : Exception;

   type ElementsPage_Type;
   type ElementsPage_Access is access all ElementsPage_Type;
   type ElementArray_Type;
   type ElementArray_Access is access ElementArray_Type;
   type Element_Type is new Basics.AnyObject_Type with
      record
         Label        : GUI.Label.Label_ClassAccess       := null;
         ComboBox     : GUI.ComboBox.ComboBox_ClassAccess := null;
         Edit         : GUI.Edit.Edit_ClassAccess         := null;
         CheckBox     : GUI.CheckBox.CheckBox_ClassAccess := null;
         Option       : SimConfig.ConfigElem_Access       := null;
         Elements     : ElementArray_Access               := null;
         ElementsPage : ElementsPage_Access               := null;
      end record;
   type Element_Access is access all Element_Type;
   type ElementArray_Type is array(Integer range <>) of aliased Element_Type;

   type ElementsPage_Type is new GUI.Object_Type with
      record
         Elements  : ElementArray_Access:=null;
         Theme     : GUI.Themes.Implementation_Type;
      end record;

   overriding
   procedure Free
     (Item : access ElementsPage_Type);

   procedure GetConfig
     (Item          : access ElementsPage_Type;
      Configuration : in out Config.Config_Type);
   ---------------------------------------------------------------------------

   function CreateElementsPage
     (Parent    : GUI.Object_ClassAccess;
      Options   : ConfigArray_Access;
      Theme     : GUI.Themes.Implementation_Type)
      return ElementsPage_Access;

end SimConfig.Visual;
