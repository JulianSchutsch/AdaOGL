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

with YellowBlue.VerticalScrollBar;
with GUI.ListBasis;
with GUI.ScrollBar;
with Canvas;
with Fonts;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body YellowBlue.ListBox is

   FrameColor  : constant Canvas.Color_Type:=16#FF000000#;
   FrameWidth  : constant Integer:=1;
   BorderWidth : constant Integer:=2;
   FillColor   : constant Canvas.Color_Type:=16#FFFFFFFF#;

   type ListBox_Type is new GUI.ListBox.ListBox_Type with
      record
         ListBasis         : GUI.ListBasis.ListBasis_Access      := null;
         VerticalScrollbar : GUI.ScrollBar.ScrollBar_ClassAccess := null;
         EntryCount        : Natural:=0;
         LeftFrame         : GUI.Canvas_ClassAccess:=null;
         TopFrame          : GUI.Canvas_ClassAccess:=null;
         BottomFrame       : GUI.Canvas_ClassAccess:=null;
         FillCanvas        : GUI.Canvas_ClassAccess:=null;
      end record;
   type ListBox_Access is access all ListBox_Type;

   overriding
   function PrecalculateListBoxHeight
     (Item       : access ListBox_Type;
      EntryCount : Integer)
      return Integer;

   overriding
   procedure AddEntry
     (Item   : access ListBox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   overriding
   procedure DeleteEntry
     (Item  : access ListBox_Type;
      Index : Integer);

   overriding
   procedure ClearEntries
     (Item : access ListBox_Type);

   overriding
   procedure Resize
     (Item : access ListBox_Type);

   overriding
   function GetIndex
     (Item : access ListBox_Type)
      return Integer;

   overriding
   procedure SetIndex
     (Item  : access ListBox_Type;
      Index : Integer);
   ---------------------------------------------------------------------------

   function PrecalculateListBoxHeight
     (Item       : access ListBox_Type;
      EntryCount : Integer)
      return Integer is
   begin
      return Item.ListBasis.PrecalculateListBasisHeight(EntryCount)+2*BorderWidth;
   end PrecalculateListBoxHeight;
   ---------------------------------------------------------------------------

   function GetIndex
     (Item : access ListBox_Type)
      return Integer is
   begin
      return Item.ListBasis.GetIndex;
   end GetIndex;
   ---------------------------------------------------------------------------

   procedure SetIndex
     (Item : access ListBox_Type;
      Index : Integer) is
      use type GUI.ListBox.OnSelect_Access;
   begin
      Item.ListBasis.SetIndex(Index);
   end SetIndex;
   ---------------------------------------------------------------------------

   procedure ListBasisSelect
     (CallBackObject : AnyObject_ClassAccess) is

      use type GUI.ListBox.OnSelect_Access;

      Item : constant ListBox_Access:=ListBox_Access(CallBackObject);

   begin

      if Item.OnSelect/=null then
         Item.OnSelect(Item.CallBackObject);
      end if;

   end ListBasisSelect;
   ---------------------------------------------------------------------------

   procedure ListBasisReselect
     (CallBackOBject : AnyObject_ClassAccess) is

      use type GUI.ListBox.OnSelect_Access;

      Item : constant ListBox_Access:=ListBox_Access(CallBackObject);

   begin

      if Item.OnReselect/=null then
         Item.OnReselect(Item.CallBackObject);
      end if;

   end ListBasisReselect;
   ---------------------------------------------------------------------------

   procedure UpdateScrollBar
     (Item : access ListBox_Type) is

      ReqScroll : Integer;
      Position  : Integer;

   begin
      ReqScroll:=Item.EntryCount-Item.ListBasis.VisibleLineCount;
      if ReqScroll<0 then
         ReqScroll:=0;
      end if;

      Position:=Item.VerticalScrollbar.GetPosition;
      if Position>ReqScroll then
         Position:=ReqScroll;
      end if;

      Item.VerticalScrollbar.SetRange
        (Min      => 0,
         Max      => ReqScroll,
         Position => Position);

   end UpdateScrollBar;
   ---------------------------------------------------------------------------

   procedure ScrollPositionChange
     (CallBackObject : AnyObject_ClassAccess) is

      Item : constant ListBox_Access:=ListBox_Access(CallBackObject);

   begin
      Item.ListBasis.SetTopIndex(Item.VerticalScrollbar.GetPosition);
   end ScrollPositionChange;
   ---------------------------------------------------------------------------


   procedure ClearEntries
     (Item : access ListBox_Type) is
   begin
      Item.ListBasis.ClearEntries;
   end ClearEntries;
   ---------------------------------------------------------------------------

   procedure AddEntry
     (Item   : access ListBox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin
      Item.ListBasis.AddEntry(String,Color);
      Item.EntryCount:=Item.EntryCount+1;
      UpdateScrollBar(Item);
   end AddEntry;
   ---------------------------------------------------------------------------

   procedure DeleteEntry
     (Item  : access ListBox_Type;
      Index : Integer) is
   begin
      Item.ListBasis.DeleteEntry(Index);
      Item.EntryCount:=Item.EntryCount-1;
      UpdateScrollBar(Item);
   end DeleteEntry;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access ListBox_Type) is
   begin
      UpdateScrollBar(Item);
   end Resize;
   ---------------------------------------------------------------------------

   function NewListBox
     (Parent : GUI.Object_ClassAccess)
      return GUI.ListBox.ListBox_ClassAccess is

      NewListBox : ListBox_Access;
      Bounds     : Bounds_Type;

   begin
      NewListBox:=new ListBox_Type;
      GUI.ListBox.ListBox_Access(NewListBox).Initialize(Parent);
      Bounds:=NewListBox.GetBounds;

      NewListBox.ListBasis
        :=new GUI.ListBasis.ListBasis_Type;

      NewListBox.ListBasis.Initialize(GUI.Object_ClassAccess(NewListBox));
      NewListBox.ListBasis.SetBounds
        (Top     => BorderWidth,
         Left    => BorderWidth,
         Height  => Bounds.Height-2*BorderWidth,
         Width   => Bounds.Width-YellowBlue.VerticalScrollBar.VerticalScrollbarWidth-BorderWidth,
         Visible => True);
      NewListBox.ListBasis.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      NewListBox.ListBasis.SetFont
        (Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 25,
         Attributes => Fonts.NoAttributes));
      NewListBox.ListBasis.CallBackObject:=AnyObject_ClassAccess(NewListBox);
      NewListBox.ListBasis.OnSelect:=ListBasisSelect'Access;
      NewListBox.ListBasis.OnReselect:=ListBasisReselect'Access;

      NewListBox.LeftFrame:=NewListBox.NewCanvas
        (Height => 1,
         Width  => 1);
      NewListBox.LeftFrame.Clear(FrameColor);
      NewListBox.LeftFrame.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => FrameWidth,
         Visible => True);
      NewListBox.LeftFrame.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => True);

      NewListBox.TopFrame:=NewListBox.NewCanvas
        (Height => 1,
         Width  => 1);
      NewListBox.TopFrame.Clear(FrameColor);
      NewListBox.TopFrame.SetBounds
        (Top     => 0,
         Left    => FrameWidth,
         Height  => FrameWidth,
         Width   => Bounds.Width-FrameWidth,
         Visible => True);
      NewListBox.TopFrame.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => False);

      NewListBox.BottomFrame:=NewListBox.NewCanvas
        (Height => 1,
         Width  => 1);
      NewListBox.BottomFrame.Clear(FrameColor);
      NewListBox.BottomFrame.SetBounds
        (Top     => Bounds.Height-FrameWidth,
         Left    => FrameWidth,
         Height  => FrameWidth,
         Width   => Bounds.Width-FrameWidth,
         Visible => True);
      NewListBox.BottomFrame.SetAnchors
        (Top    => False,
         Left   => True,
         Right  => True,
         Bottom => True);

      NewListBox.VerticalScrollbar
        :=YellowBlue.VerticalScrollBar.NewVerticalScrollBar
          (GUI.Object_ClassAccess(NewListBox));
      NewListBox.VerticalScrollbar.SetRange
        (Min      => 0,
         Max      => 0,
         Position => 0);
      NewListBox.VerticalScrollbar.SetBounds
        (Top     => 0,
         Left    => Bounds.Width-YellowBlue.VerticalScrollBar.VerticalScrollbarWidth,
         Height  => Bounds.Height,
         Width   => YellowBlue.VerticalScrollBar.VerticalScrollbarWidth,
         Visible => True);
      NewListBox.VerticalScrollbar.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);
      NewListBox.VerticalScrollbar.CallBackObject:=AnyObject_ClassAccess(NewListBox);
      NewListBox.VerticalScrollbar.OnPositionChange:=ScrollPositionChange'Access;

      NewListBox.FillCanvas:=NewListBox.NewCanvas
        (Height => 1,
         Width  => 1);
      NewListBox.FillCanvas.Clear(FillColor);
      NewListBox.FillCanvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width-YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Visible => True);
      NewListBox.FillCanvas.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);

      return GUI.ListBox.ListBox_ClassAccess(NewListBox);

   end NewListBox;
   ---------------------------------------------------------------------------

end YellowBlue.ListBox;
