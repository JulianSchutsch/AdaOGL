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
--   25.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with GUI.Window;
with GUI.ScrollBar;
with GUI.Console;
with GUI.Button;
with GUI.Combobox;
with GUI.ListBox;
with GUI.TabControl;
with GUI.CheckBox;
with GUI.RadioButton;
with GUI.Edit;
with GUI.GroupBox;
with GUI.Label;

package GUI.Themes is

   type Implementation_Type is
      record
         NewWindow              : GUI.Window.Window_Constructor           := null;
         NewVerticalScrollBar   : GUI.ScrollBar.ScrollBar_Constructor     := null;
         VerticalScrollBarWidth : Integer;
         NewConsole             : GUI.Console.Console_Constructor         := null;
         NewButton              : GUI.Button.Button_Constructor           := null;
         NewCombobox            : GUI.Combobox.Combobox_Constructor       := null;
         NewListBox             : GUI.ListBox.ListBox_Constructor         := null;
         NewTabControl          : GUI.TabControl.TabControl_Constructor   := null;
         NewCheckbox            : GUI.Checkbox.CheckBox_Constructor       := null;
         NewRadioButton         : GUI.RadioButton.RadioButton_Constructor := null;
         NewEdit                : GUI.Edit.Edit_Constructor               := null;
         NewGroupBox            : GUI.GroupBox.GroupBox_Constructor       := null;
         NewLabel               : GUI.Label.Label_Constructor             := null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => To_Unbounded_String("Theme"));

end GUI.Themes;
