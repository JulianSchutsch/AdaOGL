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

with BoundsCalc; use BoundsCalc;
with Canvas;
with Fonts;
with Basics; use Basics;
with GUI.Listbox;
with GUI.Basics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with YellowBlue.ListBox;
with GUIMouse; use GUIMouse;

package body YellowBlue.Combobox is

   BackgroundColor    : constant Canvas.Color_Type:=16#FFFFFFFF#;
   FrameColor         : constant Canvas.Color_Type:=16#FF000000#;
   TriangleColor      : constant Canvas.Color_Type:=16#FF000000#;
   DownButtonWidth    : constant:=25;
   TriangleDistance   : constant:=4;
   RectangleColor     : constant Canvas.Color_Type:=16#FF000000#;
   RectangleDistance  : constant:=4;
   DropDownListHeight : constant:=100;
   MaxVisibleDropDownEntries : constant:=5;

   type Combobox_Type is new GUI.Combobox.Combobox_Type with
      record
         Canvas       : GUI.Canvas_ClassAccess:=null;
         Font         : Fonts.Font_ClassAccess:=null;
         DropdownList : GUI.ListBox.ListBox_ClassAccess:=null;
      end record;
   type Combobox_Access is access all Combobox_Type;

   overriding
   procedure Resize
     (Item : access Combobox_Type);

   overriding
   procedure SetIndex
     (Item  : access Combobox_Type;
      Index : Integer);

   overriding
   function MouseDown
     (Item   : access Combobox_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure Free
     (Item : access Combobox_Type);
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Combobox_Type) is

      use type Fonts.Font_ClassAccess;

   begin

      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;

      GUI.Combobox.Combobox_Access(Item).Free;

   end Free;
   ---------------------------------------------------------------------------

   procedure DrawCanvas
     (Combobox : access Combobox_Type) is

      use type Fonts.Font_ClassAccess;
      use type GUI.ListBox.ListBox_ClassAccess;

      Bounds : constant Bounds_Type:=Combobox.GetBounds;

   begin

      GUI.FreeCanvas(Combobox.Canvas);
      Combobox.Canvas:=Combobox.NewCanvas
        (Height => Bounds.Height,
         Width  => Bounds.Width);
      Combobox.Canvas.Clear(BackgroundColor);
      Combobox.Canvas.Rectangle
        (X      => 0,
         Y      => 0,
         Height => Bounds.Height,
         Width  => Bounds.Width,
         Color  => FrameColor);
      Combobox.Canvas.VertLine
        (X      => Bounds.Width-DownButtonWidth,
         Y      => 1,
         Height => Bounds.Height-2,
         Color  => FrameColor);
      if Combobox.DropdownList=null then
         Combobox.Canvas.WuLine
           (X1    => float(Bounds.Width-DownButtonWidth+TriangleDistance),
            Y1    => float(TriangleDistance),
            X2    => float(Bounds.Width)-float(DownButtonWidth)/2.0-1.0,
            Y2    => float(Bounds.Height-TriangleDistance-1),
            Color => TriangleColor);
         Combobox.Canvas.WuLine
           (X1    => float(Bounds.Width-TriangleDistance-1),
            Y1    => float(TriangleDistance),
            X2    => float(Bounds.Width)-float(DownButtonWidth)/2.0-1.0,
            Y2    => float(Bounds.Height-TriangleDistance-1),
            Color => TriangleColor);
         Combobox.Canvas.WuLine
           (X1    => float(Bounds.Width-DownButtonWidth+TriangleDistance),
            Y1    => float(TriangleDistance),
            X2    => float(Bounds.Width-TriangleDistance-1),
            Y2    => float(TriangleDistance),
            Color => TriangleColor);
      else
         Combobox.Canvas.HorzLine
           (X => Bounds.Width-DownButtonWidth+RectangleDistance,
            Y => RectangleDistance,
            Width => DownButtonWidth-2*RectangleDistance,
            Color => RectangleColor);
         Combobox.Canvas.HorzLine
           (X     => Bounds.Width-DownButtonWidth+RectangleDistance,
            Y     => Bounds.Height-RectangleDistance-1,
            Width => DownButtonWidth-2*RectangleDistance,
            Color => RectangleColor);
         Combobox.Canvas.VertLine
           (X      => Bounds.Width-DownButtonWidth+RectangleDistance,
            Y      => RectangleDistance,
            Height => Bounds.Height-2*RectangleDistance,
            Color  => RectangleColor);
         Combobox.Canvas.VertLine
           (X      => Bounds.Width-RectangleDistance-1,
            Y      => RectangleDistance,
            Height => Bounds.Height-2*RectangleDistance,
            Color  => RectangleColor);
      end if;

      Combobox.Canvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width,
         Visible => True);

      if Combobox.Font/=null then
         declare
            String : Unbounded_String;
            Color  : Canvas.Color_Type;
         begin
            Combobox.GetSelectedEntry(String,Color);
            Combobox.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(Combobox.Canvas),
               X      => 4.0,
               Y      => 2.0,
               Text   => String,
               Color  => Color);
         end;
      end if;

   end DrawCanvas;
   ---------------------------------------------------------------------------

   procedure ASyncDropDownDelete
     (ItemPnt : GUI.Object_ClassAccess) is
      Item : constant Combobox_Access:=Combobox_Access(ItemPnt);

      use type GUI.ListBox.ListBox_ClassAccess;

   begin
      if Item.DropDownList/=null then
         Put_Line("ASyncDropDownDelete");
         Put(Item.all'Address);
         Item.DropdownList.Free;
         Item.DropdownList:=null;
         DrawCanvas(Item);
         Put_Line("ASyncDrop//");
      end if;
   end ASyncDropDownDelete;
   ---------------------------------------------------------------------------

   procedure DropDownItemSelect
     (CallBackObject : AnyObject_ClassAccess) is

      use type GUI.ListBox.ListBox_ClassAccess;

      Item : constant Combobox_Access:=Combobox_Access(CallBackObject);

   begin
      if Item.DropDownList/=null then
         Put("DropDownItemSelect");
         New_Line;
         Item.SetIndex(Item.DropdownList.GetIndex);
         Item.AddASync(ASyncDropdownDelete'Access);
         Put("Done");
      end if;
   end DropDownItemSelect;
   ---------------------------------------------------------------------------

   procedure ContextAreaClick
     (CallBackObject : AnyObject_ClassAccess) is

      use type GUI.ListBox.ListBox_ClassAccess;

      Item : constant Combobox_Access:=Combobox_Access(CallBackObject);

   begin
      if Item.DropdownList/=null then
         Item.AddASync(ASyncDropdownDelete'Access);
      end if;
   end ContextAreaClick;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access Combobox_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(Y);

      AreaAbsBounds : constant AbsBounds_Type := Item.GetContextArea.GetAbsBounds;
      BoxAbsBounds  : constant AbsBounds_Type := Item.GetAbsBounds;
      BoxBounds     : constant Bounds_Type    := Item.GetBounds;

      YCandidate1 : Integer;
      YCandidate2 : Integer;
      DTop        : Integer;
      DHeight     : Integer;

   begin
      if Button=LeftButton then
         if X>=Item.GetBounds.Width-DownButtonWidth then
            Item.DropdownList
              :=YellowBlue.ListBox.NewListBox(Item.GetContextArea);

            DHeight:=Item.DropdownList.PrecalculateListBoxHeight(Integer'Min(Item.GetEntryCount,MaxVisibleDropDownEntries));

            YCandidate1:=BoxAbsBounds.AbsTop
              -BoxAbsBounds.AbsSubTop
              +BoxBounds.Height;
            YCandidate2:=BoxAbsBounds.AbsTop
              -BoxAbsBounds.AbsSubTop
              -DropDownListHeight;
            if YCandidate1+DropDownListHeight>AreaAbsBounds.AbsHeight then
               if YCandidate2<0 then
                  if AreaAbsBounds.AbsHeight-YCandidate1
                    >BoxAbsBounds.AbsTop-BoxAbsBounds.AbsSubTop then
                     DHeight := AreaAbsBounds.AbsHeight-YCandidate1;
                     DTop    := YCandidate1;
                  else
                     DHeight := BoxAbsBounds.AbsTop-BoxAbsBounds.AbsSubTop;
                     DTop    := 0;
                  end if;
               else
                  DTop:=YCandidate2;
               end if;
            else
               DTop:=YCandidate1;
            end if;
            Put("DropDownList:");
            Put(Dtop);
            Put(DHeight);
            New_Line;
            Item.DropdownList.SetBounds
              (Top     => DTop,
               Left    => BoxAbsBounds.AbsLeft-BoxAbsBounds.AbsSubLeft,
               Height  => DHeight,
               Width   => BoxBounds.Width,
               Visible => True);
            Item.SetContextAreaClick
              (CallBack       => ContextAreaClick'Access,
               CallBackObject => AnyObject_ClassAccess(Item));

            declare
               use GUI.Basics;
               use GUI.Basics.StringAndColorList_Pack;
               Cursor : StringAndColorList_Pack.Cursor;
               Entries : StringAndColorList_Pack.List;
            begin
               Entries:=Item.GetEntries;
               Cursor:=Entries.First;
               while Cursor/=StringAndColorList_Pack.No_Element loop
                  Item.DropdownList.AddEntry
                    (Element(Cursor).String,
                     Element(Cursor).Color);
                  Cursor:=StringAndColorList_Pack.Next(Cursor);
               end loop;
            end;
            Item.DropdownList.SetIndex(GUI.Combobox.Combobox_Access(Item).GetIndex);
            Item.DropdownList.CallBackObject:=AnyObject_ClassAccess(Item);
            Item.DropdownList.OnSelect:=DropDownItemSelect'Access;
            Item.DropdownList.OnReselect:=ContextAreaClick'Access;
            DrawCanvas(Item);
         end if;
      end if;
      return True;
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure SetIndex
     (Item  : access Combobox_Type;
      Index : Integer) is
   begin
      GUI.Combobox.Combobox_Access(Item).SetIndex(Index);
      DrawCanvas(Item);
   end SetIndex;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Combobox_Type) is
   begin
      DrawCanvas(Item);
   end Resize;
   ---------------------------------------------------------------------------

   function NewCombobox
     (Parent : GUI.Object_ClassAccess)
      return GUI.Combobox.Combobox_ClassAccess is

      NewCombobox : Combobox_Access;

   begin

      NewCombobox:=new Combobox_Type;
      NewCombobox.Initialize(Parent);

      NewCombobox.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 16,
         Attributes => Fonts.NoAttributes);

      return Combobox_ClassAccess(NewCombobox);

   end NewCombobox;

end YellowBlue.Combobox;
