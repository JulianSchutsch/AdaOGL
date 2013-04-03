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
--   1.Mai 2012 Julian Schutsch
--     - Original version

-- Demo
--   Demonstration for setting up a GUI context and a listbox using two
--   buttons to add or remove content from it.

pragma Ada_2005;

with GUI;
with GUI.Themes;
with GUI.ListBox;
with GUI.Button;
with GUI.UseImplementations;

with YellowBlue;

with Config;
with BoundsCalc; use BoundsCalc;
with ProcessLoop;
with Basics; use Basics;

procedure Listbox is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;
   ListBox           : GUI.ListBox.ListBox_ClassAccess;
   AddButton         : GUI.Button.Button_ClassAccess;
   DeleteButton      : GUI.Button.Button_ClassAccess;

   AddCounter        : Integer:=0;

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

   procedure AddClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin

      AddCounter:=AddCounter+1;

      ListBox.AddEntry
        (String => U("Entry Added :"&Integer'Image(AddCounter)),
         Color  => 16#FF000000#);

   end AddClick;
   ---------------------------------------------------------------------------

   procedure DeleteClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin

      if ListBox.GetIndex>=0 then
         ListBox.DeleteEntry(ListBox.GetIndex);
      end if;

   end DeleteClick;
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

   declare
      -- Obtain current main window dimensions
      WindowBounds : constant Bounds_Type:=Context.BasisArea.GetBounds;
   begin
      -- Create a new listbox with WindowArea as parent
      ListBox:=Theme.NewListBox(Context.BasisArea);

      -- The Listbox should fill almost the entire window, but leave some
      -- space for buttons
      ListBox.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => WindowBounds.Height,
         Width   => WindowBounds.Width-120,
         Visible => True);
      ListBox.SetAnchors
        (Top => True,
         Left => True,
         Right => True,
         Bottom => True);

      AddButton:=Theme.NewButton(Context.BasisArea);
      AddButton.SetBounds
        (Top     => 10,
         Left    => WindowBounds.Width-110,
         Height  => 30,
         Width   => 100,
         Visible => True);
      AddButton.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => False);
      AddButton.SetCaption(U("Add"));
      AddButton.OnClick:=AddClick'Unrestricted_Access;

      DeleteButton:=Theme.NewButton(Context.BasisArea);
      DeleteButton.SetBounds
        (Top     => 50,
         Left    => WindowBounds.Width-110,
         Height  => 30,
         Width   => 100,
         Visible => True);
      DeleteButton.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => False);
      DeleteButton.SetCaption(U("Delete"));
      DeleteButton.OnClick:=DeleteClick'Unrestricted_Access;
   end;


   -- Waiting until either Context.OnClose is triggered
   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   -- All objects placed in the context are deleted also
   GUI.FreeContext(Context);

   -- Unregister all which has been registered
   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end Listbox;
