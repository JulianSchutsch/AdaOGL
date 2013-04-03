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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with Canvas;
with Fonts;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with GUIMouse; use GUIMouse;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body YellowBlue.TabControl is

   TabBorderWidth       : constant Integer:=3;
   TabHorzBorderWidth   : constant Integer:=12;
   NormalFillColor      : constant Canvas.Color_Type:=16#FF7F7F7F#;
   NormalFrameColor     : constant Canvas.Color_Type:=16#FFFFFFFF#;
   NormalCaptionColor   : constant Canvas.Color_Type:=16#FF000000#;
   SelectedFillColor    : constant Canvas.Color_Type:=16#FF00007F#;
   SelectedFrameColor   : constant Canvas.Color_Type:=16#FFFFFFFF#;
   SelectedCaptionColor : constant Canvas.Color_Type:=16#FFFFFFFF#;
   FrameColor           : constant Canvas.Color_Type:=16#FFFFFFFF#;

   type TabControl_Type;
   type TabControl_Access is access all TabControl_Type;

   type Tab_Type is new GUI.TabControl.Tab_Type with
      record
         X          : Integer;
         Y          : Integer;
         Width      : Integer;
         Canvas     : GUI.Canvas_ClassAccess;
         Caption    : Unbounded_String;
         TabControl : TabControl_Access;
      end record;
   type Tab_Access is access all Tab_Type;
   ---------------------------------------------------------------------------

   package TabList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Tab_Access,
      "="          => "=");

   type PatchArray_Type is array(Integer range <>) of GUI.Canvas_ClassAccess;
   type PatchArray_Access is access PatchArray_Type;
   type TabControl_Type is new GUI.TabControl.TabControl_Type with
      record
         Font         : Fonts.Font_ClassAccess:=null;
         Tabs         : TabList_Pack.List;
         CurrentTab   : Tab_Access:=null;
         TabHeight    : Integer;
         LeftFrame    : GUI.Canvas_ClassAccess:=null;
         BottomFrame  : GUI.Canvas_ClassAccess:=null;
         RightFrame   : GUI.Canvas_ClassAccess:=null;
         FramePatches : PatchArray_Access:=null;
      end record;

   overriding
   function NewTab
     (Item    : access TabControl_Type;
      Caption : Unbounded_String)
      return GUI.TabControl.Tab_ClassAccess;

   overriding
   procedure Resize
     (Item : access TabControl_Type);

   overriding
   procedure Free
     (Item : access TabControl_Type);

   overriding
   function Mousedown
     (Item   : access TabControl_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;
   ---------------------------------------------------------------------------

   procedure DrawCanvas
     (Tab : access Tab_Type) is

      FillColor    : Canvas.Color_Type;
      FrameColor   : Canvas.Color_Type;
      CaptionColor : Canvas.Color_Type;

   begin

      Tab.Width := Tab.TabControl.Font.TextWidth(Tab.Caption)+2*TabHorzBorderWidth;

      if Tab=Tab.TabControl.CurrentTab then
         FillColor    := SelectedFillColor;
         FrameColor   := SelectedFrameColor;
         CaptionColor := SelectedCaptionColor;
      else
         FillColor    := NormalFillColor;
         FrameColor   := NormalFrameColor;
         CaptionColor := NormalCaptionColor;
      end if;

      GUI.FreeCanvas(Tab.Canvas);
      Tab.Canvas:=Tab.TabControl.NewCanvas
        (Height => Tab.TabControl.TabHeight,
         Width  => Tab.Width);
      Tab.Canvas.Clear(FillColor);
      Tab.Canvas.VertLine
        (X      => 0,
         Y      => 0,
         Height => Tab.TabControl.TabHeight,
         Color  => FrameColor);
      Tab.Canvas.VertLine
        (X      => Tab.Width-1,
         Y      => 0,
         Height => Tab.TabControl.TabHeight,
         Color  => FrameColor);
      Tab.Canvas.HorzLine
        (X      => 0,
         Y      => 0,
         Width  => Tab.Width,
         Color  => FrameColor);
      Tab.TabControl.Font.TextOut
        (Canvas => Canvas.Canvas_ClassAccess(Tab.Canvas),
         X      => float(TabHorzBorderWidth),
         Y      => float(TabBorderWidth),
         Text   => Tab.Caption,
         Color  => CaptionColor);

   end DrawCanvas;
   ---------------------------------------------------------------------------

   procedure SetFramePatchCount
     (Item  : access TabControl_Type;
      Count : Natural) is

      NewFramePatches : PatchArray_Access;
      CountMin        : Integer;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => PatchArray_Type,
         Name   => PatchArray_Access);

   begin

      if Count/=0 then
         NewFramePatches:=new PatchArray_Type(1..Count);
      end if;

      if (Item.FramePatches/=null) then
         CountMin:=Integer'Min(Count,Item.FramePatches'Last);
         for i in 1..CountMin loop
            NewFramePatches(i):=Item.FramePatches(i);
         end loop;
         for i in CountMin+1..Item.FramePatches'Last loop
            GUI.FreeCanvas(Item.FramePatches(i));
         end loop;
         Free(Item.FramePatches);
      else
         CountMin:=0;
      end if;

      if Count=0 then
         return;
      end if;

      for i in CountMin+1..NewFramePatches'Last loop
         NewFramePatches(i):=Item.NewCanvas
           (Height => 1,
            Width  => 1);
         NewFramePatches(i).Clear(FrameColor);
      end loop;

      Item.FramePatches:=NewFramePatches;

   end SetFramePatchCount;
   ---------------------------------------------------------------------------

   procedure ArrangeTabs
     (Item : access TabControl_Type) is

      use type TabList_Pack.Cursor;

      Cursor    : TabList_Pack.Cursor;
      Tab       : Tab_Access;
      LineCount : Integer:=0;

      Bounds : constant Bounds_Type:=Item.GetBounds;

   begin

      declare
         CurrentX : Integer:=0;
         CurrentY : Integer:=0;
         AnyTabOnline : Boolean:=False;
      begin
         -- Count Lines, hide invisible tab areas
         Cursor:=Item.Tabs.First;
         while Cursor/=TabList_Pack.No_Element loop

            Tab:=TabList_Pack.Element(Cursor);

            if CurrentX+Tab.Width>=Bounds.Width
              and AnyTabOnline then
               CurrentX  := 0;
               CurrentY  := CurrentY+Item.TabHeight;
               LineCount := LineCount+1;
            end if;

            AnyTabOnline:=True;

            Tab.X:=CurrentX;
            Tab.Y:=CurrentY;

            if Tab/=Item.CurrentTab then
               Tab.SetBounds(0,0,0,0,False);
            end if;

            CurrentX:=CurrentX+Tab.Width;

            Cursor:=TabList_Pack.Next(Cursor);

         end loop;

         if CurrentX/=0 then
            CurrentY:=CurrentY+Item.TabHeight;
            LineCount:=LineCount+1;
         end if;

         if Item.CurrentTab/=null then
            Item.CurrentTab.SetBounds
              (Top     => CurrentY,
               Left    => 0,
               Height  => Bounds.Height-CurrentY,
               Width   => Bounds.Width,
               Visible => True);
         end if;
         Item.LeftFrame.SetBounds
           (Top     => CurrentY,
            Left    => 0,
            Height  => Bounds.Height-CurrentY,
            Width   => 1,
            Visible => True);
         Item.BottomFrame.SetBounds
           (Top     => Bounds.Height-1,
            Left    => 1,
            Height  => 1,
            Width   => Bounds.Width-2,
            Visible => True);
      end;

      if LineCount=0 then
         Item.RightFrame.SetBounds
           (Top     => Item.TabHeight,
            Left    => Bounds.Width-1,
            Height  => Bounds.Height-Item.TabHeight,
            Width   => 1,
            Visible => True);
         SetFramePatchCount(Item,1);
         Item.FramePatches(1).SetBounds
           (Top    => 0,
            Left    => 1,
            Width   => Bounds.Width-2,
            Height  => 1,
            Visible => True);
      else
         Item.RightFrame.SetBounds
           (Top     => Item.TabHeight,
            Left    => Bounds.Width-1,
            Height  => Bounds.Height-Item.TabHeight,
            Width   => 1,
            Visible => True);
         SetFramePatchCount(Item,LineCount);
      end if;

      -- Arrange Tab Canvases

      declare
         PreviousX     : Integer:=0;
         PreviousY     : Integer:=0;
         PreviousWidth : Integer:=0;
         PatchPos      : Integer:=1;
      begin

         Cursor:=Item.Tabs.First;
         while Cursor/=TabList_Pack.No_Element loop

            Tab:=TabList_Pack.Element(Cursor);

            Tab.Canvas.SetBounds
              (Top     => Tab.Y,
               Left    => Tab.X,
               Height  => Item.TabHeight,
               Width   => Tab.Width,
               Visible => True);


            if Tab.Y/=PreviousY then
               PreviousY:=Tab.Y;
               Item.FramePatches(PatchPos).SetBounds
                 (Top     => PreviousY,
                  Left    => PreviousX+PreviousWidth-1,
                  Height  => 1,
                  Width   => Bounds.Width-PreviousX-PreviousWidth,
                  Visible => True);
               PatchPos:=PatchPos+1;
            end if;

            PreviousX     := Tab.X;
            PreviousWidth := Tab.Width;

            Cursor:=TabList_Pack.Next(Cursor);

         end loop;

         if LineCount/=0 then
            Item.FramePatches(PatchPos).SetBounds
              (Top     => PreviousY+Item.TabHeight,
               Left    => PreviousX+PreviousWidth-1,
               Height  => 1,
               Width   => Bounds.Width-PreviousX-PreviousWidth,
               Visible => True);
         end if;

      end;

   end ArrangeTabs;
   ---------------------------------------------------------------------------

   procedure ActivateTabAt
     (Item : access TabControl_Type;
      X    : Integer;
      Y    : Integer) is

      use type TabList_Pack.Cursor;

      Cursor  : TabList_Pack.Cursor;
      Tab     : Tab_Access;
      PrevTab : Tab_Access;

   begin

      Put("ActiveTabAt");
      Put(X);
      Put(Y);
      New_Line;
      Cursor:=Item.Tabs.First;
      while Cursor/=TabList_Pack.No_Element loop
         Tab:=TabList_Pack.Element(Cursor);

         if X in Tab.X..Tab.X+Tab.Width-1
           and Y in Tab.Y..Tab.Y+Item.TabHeight-1 then
            PrevTab:=Item.CurrentTab;
            Item.CurrentTab:=Tab;
            Put("New Active Tab");
            Put(To_String(Tab.Caption));
            New_Line;
            DrawCanvas(PrevTab);
            DrawCanvas(Tab);
            ArrangeTabs(Item);
            return;
         end if;

         Cursor:=TabList_Pack.Next(Cursor);
      end loop;

   end ActivateTabAt;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access TabControl_Type) is
   begin
      SetFramePatchCount(Item,0);
      GUI.TabControl.TabControl_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   function Mousedown
     (Item   : access TabControl_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is
   begin
      if Button=LeftButton then
         ActivateTabAt(Item,X,Y);
      end if;
      return True;
   end Mousedown;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access TabControl_Type) is
   begin
      ArrangeTabs(Item);
   end Resize;
   ---------------------------------------------------------------------------

   function NewTab
     (Item    : access TabControl_Type;
      Caption : Unbounded_String)
      return GUI.TabControl.Tab_ClassAccess is

      NewTab : Tab_Access;

   begin

      NewTab:=new Tab_Type;

      NewTab.TabControl := TabControl_Access(Item);
      NewTab.Caption    := Caption;

      GUI.TabControl.Tab_Access(NewTab).Initialize(GUI.Object_ClassAccess(Item));

      Item.Tabs.Append(NewTab);
      if Item.CurrentTab=null then
         Item.CurrentTab:=NewTab;
      end if;

      DrawCanvas(NewTab);

      ArrangeTabs(Item);

      return GUI.TabControl.Tab_ClassAccess(NewTab);
   end NewTab;
   ---------------------------------------------------------------------------

   function NewTabControl
     (Parent : GUI.Object_ClassAccess)
      return GUI.TabControl.TabControl_ClassAccess is

      NewTabC : TabControl_Access;

   begin
      NewTabC:=new TabControl_Type;
      GUI.TabControl.TabControl_Access(NewTabC).Initialize(Parent);
      NewTabC.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);
      NewTabC.TabHeight:=2*TabBorderWidth+NewTabC.Font.Height;

      NewTabC.LeftFrame:=NewTabC.NewCanvas
        (Height => 1,
         Width  => 1);
      NewTabC.LeftFrame.Clear(FrameColor);
      NewTabC.RightFrame:=NewTabC.NewCanvas
        (Height => 1,
         Width  => 1);
      NewTabC.RightFrame.Clear(FrameColor);
      NewTabC.BottomFrame:=NewTabC.NewCanvas
        (Height => 1,
         Width  => 1);
      NewTabC.BottomFrame.Clear(FrameColor);
      ArrangeTabs(NewTabC);
      return GUI.TabControl.TabControl_ClassAccess(NewTabC);

   end NewTabControl;
   ---------------------------------------------------------------------------

end YellowBlue.TabControl;
