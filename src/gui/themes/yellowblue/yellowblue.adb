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

pragma Ada_2005;

with Basics; use Basics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GUI.Themes;

with YellowBlue.Window; use YellowBlue.Window;
with YellowBlue.VerticalScrollBar; use YellowBlue.VerticalScrollBar;
with YellowBlue.Console; use YellowBlue.Console;
with YellowBlue.Button; use YellowBlue.Button;
with YellowBlue.Combobox; use YellowBlue.Combobox;
with YellowBlue.ListBox; use YellowBlue.ListBox;
with YellowBlue.TabControl; use YellowBlue.TabControl;
with YellowBlue.CheckBox; use YellowBlue.CheckBox;
with YellowBlue.RadioButton; use YellowBlue.RadioButton;
with YellowBlue.Edit; use YellowBlue.Edit;
with YellowBlue.GroupBox; use YellowBlue.GroupBox;
with YellowBlue.Label; use YellowBlue.Label;

package body YellowBlue is

   Implementation : constant GUI.Themes.Implementation_Type:=
     (NewWindow              => NewWindow'Access,
      NewVerticalScrollBar   => NewVerticalScrollBar'Access,
      VerticalScrollBarWidth => VerticalScrollBarWidth,
      NewConsole             => NewConsole'Access,
      NewButton              => NewButton'Access,
      NewCombobox            => NewCombobox'Access,
      NewListBox             => NewListBox'Access,
      NewTabControl          => NewTabControl'Access,
      NewCheckBox            => NewCheckBox'Access,
      NewRadioButton         => NewRadioButton'Access,
      NewEdit                => NewEdit'Access,
      NewGroupBox            => NewGroupBox'Access,
      NewLabel               => NewLabel'Access);

   Identifier : constant Unbounded_String:=U("YellowBlue");

   procedure Register is
   begin

      GUI.Themes.Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);

   end Register;
   ---------------------------------------------------------------------------

   procedure UnRegister is
   begin

      GUI.Themes.Implementations.UnRegister
        (Identifier => Identifier);

   end UnRegister;
   ---------------------------------------------------------------------------

end YellowBlue;
