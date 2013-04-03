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
--   3.Mai 2012 Julian Schutsch
--     - Original version

-- Demo
--   Demonstration for radio buttons.

pragma Ada_2005;

with GUI;
with GUI.RadioButton;
with GUI.Themes;
with GUI.UseImplementations;

with YellowBlue;

with Config;
with ProcessLoop;
with Basics; use Basics;

procedure RadioButton is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;
   RadioButton1      : GUI.RadioButton.RadioButton_ClassAccess;
   RadioButton2      : GUI.RadioButton.RadioButton_ClassAccess;
   RadioButton3      : GUI.RadioButton.RadioButton_ClassAccess;

   Terminated        : Boolean:=False;
   pragma Warnings(Off,Terminated); -- Terminated is never changed
                                    -- from GNATs perspective

   procedure ContextClose
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      Terminated:=True;
   end ContextClose;
   ---------------------------------------------------------------------------

begin

   -- GUI.UseImplementations.Register adds all supported implementations
   -- on this plattform to GUI.Implementations.
   -- The GUI.UseImplementations package was created by /buildcfg/configprog
   GUI.UseImplementations.Register;

   -- This is the default theme, now added to GUI.Themes.Implementations
   YellowBlue.Register;

   -- Now select any available GUI implementation
   GUIImplementation := GUI.Implementations.FindAny;

   -- And the only theme we added to GUI.Themes.Implementations
   Theme             := GUI.Themes.Implementations.FindAny;

   -- Often when creating a new context (main window), one may wish
   -- to specify further data in Configuration.
   Context:=GUIImplementation.NewContext
     (Configuration => Configuration,
      Node          => U(""));

   -- Called when the main window's close button is clicked
   Context.OnClose:=ContextClose'Unrestricted_Access;

   RadioButton1:=Theme.NewRadioButton(Context.BasisArea);
   RadioButton1.SetBounds
     (Top     => 10,
      Left    => 10,
      Height  => 30,
      Width   => 150,
      Visible => True);
   RadioButton1.SetCaption(U("Radio 1"));

   RadioButton2:=Theme.NewRadioButton(Context.BasisArea);
   RadioButton2.SetBounds
     (Top     => 45,
      Left    => 10,
      Height  => 30,
      Width   => 150,
      Visible => True);
   RadioButton2.SetCaption(U("Radio 2"));
   RadioButton2.Link(RadioButton1);

   RadioButton3:=Theme.NewRadioButton(Context.BasisArea);
   RadioButton3.SetBounds
     (Top     => 80,
      Left    => 10,
      Height  => 30,
      Width   => 150,
      Visible => True);
   RadioButton3.SetCaption(U("Radio 3"));
   RadioButton3.Link(RadioButton1);

   RadioButton1.SetChecked;

   -- Waiting until Cotext.OnClose is triggered
   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   -- All objects placed in the context are deleted also
   GUI.FreeContext(Context);

   -- Unregister all which has been registered
   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end RadioButton;
