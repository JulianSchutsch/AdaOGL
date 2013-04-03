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
with Fonts;
with Canvas;
with BoundsCalc; use BoundsCalc;
with Basics; use Basics;

package body YellowBlue.GroupBox is

   FrameColor   : constant Canvas.Color_Type:=16#FFFFFF00#;
   TextColor    : constant Canvas.Color_Type:=16#FFFFFFFF#;

   BorderWidth  : constant Integer:=3;
   BracketWidth : constant Integer:=3;
   CornerWidth  : constant Integer:=5;
   BracketCaptionDistance : constant Integer:=2;

   type GroupBox_Type is new GUI.GroupBox.GroupBox_Type with
      record
         Caption           : Unbounded_String;
         Font              : Fonts.Font_ClassAccess:=null;
         TopFrameCanvas    : GUI.Canvas_ClassAccess:=null;
         LeftFrameCanvas   : GUI.Canvas_ClassAccess:=null;
         RightFrameCanvas  : GUI.Canvas_ClassAccess:=null;
         BottomFrameCanvas : GUI.Canvas_ClassAccess:=null;
         CaptionCanvas     : GUI.Canvas_ClassAccess:=null;
         Client            : GUI.Object_Access:=null;
      end record;
   type GroupBox_Access is access all GroupBox_Type;

   overriding
   procedure SetCaption
     (Item    : access GroupBox_Type;
      Caption : Unbounded_String);

   overriding
   procedure Free
     (Item : access GroupBox_Type);
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access GroupBox_Type) is
   begin
      Fonts.Release(Item.Font);
      GUI.GroupBox.GroupBox_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   procedure SetAllBounds
     (Item : access GroupBox_Type) is

      use type GUI.Canvas_ClassAccess;

      Bounds : constant Bounds_Type:=Item.GetBounds;

   begin

      if Item.CaptionCanvas=null then
         Item.TopFrameCanvas.SetBounds
           (Top     => 0,
            Left    => 0,
            Height  => 1,
            Width   => Bounds.Width,
            Visible => True);
         Item.LeftFrameCanvas.SetBounds
           (Top     => 1,
            Left    => 0,
            Height  => Bounds.Height-2,
            Width   => 1,
            Visible => True);
         Item.RightFrameCanvas.SetBounds
           (Top     => 0,
            Left    => Bounds.Width-1,
            Height  => Bounds.Height-2,
            Width   => 1,
            Visible => True);
         Item.Client.SetBounds
           (Top     => BorderWidth,
            Left    => BorderWidth,
            Height  => Bounds.Height-2*BorderWidth,
            Width   => Bounds.Width-2*BorderWidth,
            Visible => True);
      else
         Item.TopFrameCanvas.SetBounds
           (Top     => Item.CaptionCanvas.ContentHeight/2-1,
            Left    => Item.CaptionCanvas.ContentWidth,
            Height  => 1,
            Width   => Bounds.Width-Item.CaptionCanvas.ContentWidth,
            Visible => True);
         Item.LeftFrameCanvas.SetBounds
           (Top     => Item.CaptionCanvas.ContentHeight,
            Left    => 0,
            Height  => Bounds.Height-Item.CaptionCanvas.ContentHeight-1,
            Width   => 1,
            Visible => True);
         Item.RightFrameCanvas.SetBounds
           (Top     => Item.CaptionCanvas.ContentHeight/2-1,
            Left    => Bounds.Width-1,
            Height  => Bounds.Height-Item.CaptionCanvas.ContentHeight/2,
            Width   => 1,
            Visible => True);
         Item.Client.SetBounds
           (Top     => Item.CaptionCanvas.ContentHeight+BorderWidth,
            Left    => BorderWidth,
            Height  => Bounds.Height-2*BorderWidth-Item.CaptionCanvas.ContentHeight,
            Width   => Bounds.Width-2*BorderWidth,
            Visible => True);
      end if;
      Item.BottomFrameCanvas.SetBounds
        (Top     => Bounds.Height-1,
         Left    => 0,
         Height  => 1,
         Width   => Bounds.Width,
         Visible => True);

   end SetAllBounds;
   ---------------------------------------------------------------------------

   procedure DrawCaptionCanvas
     (Item : access GroupBox_Type) is

      use type Fonts.Font_ClassAccess;

      TextWidth  : Integer;
      TextHeight : Integer;
      Width      : Integer;

   begin
      GUI.FreeCanvas(Item.CaptionCanvas);
      if Item.Font/=null then
         TextHeight:=Item.Font.Height;
         TextWidth:=Item.Font.TextWidth(Item.Caption);
         Width:=TextWidth+CornerWidth+2*BracketWidth+2*BracketCaptionDistance;
         Item.CaptionCanvas:=Item.NewCanvas
           (Height => TextHeight,
            Width  => Width);
         Item.CaptionCanvas.SetBounds
           (Top     => 0,
            Left    => 0,
            Height  => TextHeight,
            Width   => Width,
            Visible => True);
         Item.CaptionCanvas.Clear(0);
         Item.CaptionCanvas.VertLine
           (X      => 0,
            Y      => TextHeight/2-1,
            Height => TextHeight/2+1,
            Color  => FrameColor);
         Item.CaptionCanvas.HorzLine
           (X      => 1,
            Y      => TextHeight/2-1,
            Width  => CornerWidth-1,
            Color  => FrameColor);
         Item.CaptionCanvas.VertLine
           (X      => CornerWidth,
            Y      => 0,
            Height => TextHeight,
            Color  => FrameColor);
         Item.CaptionCanvas.HorzLine
           (X      => CornerWidth+1,
            Y      => 0,
            Width  => BracketWidth-1,
            Color  => FrameColor);
         Item.CaptionCanvas.HorzLine
           (X      => CornerWidth+1,
            Y      => TextHeight-1,
            Width  => BracketWidth-1,
            Color  => FrameColor);
         Item.Font.TextOut
           (Canvas => Canvas.Canvas_ClassAccess(Item.CaptionCanvas),
            X      => float(CornerWidth+BracketWidth+BracketCaptionDistance),
            Y      => 0.0,
            Text   => Item.Caption,
            Color  => TextColor);
         Item.CaptionCanvas.HorzLine
           (X     => Width-BracketWidth,
            Y     => 0,
            Width => BracketWidth-1,
            Color => FrameColor);
         Item.CaptionCanvas.HorzLine
           (X     => Width-BracketWidth,
            Y     => TextHeight-1,
            Width => BracketWidth-1,
            Color => FrameColor);
         Item.CaptionCanvas.VertLine
           (X      => Width-1,
            Y      => 0,
            Height => TextHeight,
            Color  => FrameColor);
      end if;
      SetAllBounds(Item);
   end DrawCaptionCanvas;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Item    : access GroupBox_Type;
      Caption : Unbounded_String) is
   begin
      Item.Caption:=Caption;
      DrawCaptionCanvas(Item);
   end SetCaption;
   ---------------------------------------------------------------------------

   function NewGroupBox
     (Parent : GUI.Object_ClassAccess)
      return GUI.GroupBox.GroupBox_ClassAccess is

      NewGroupBox : GroupBox_Access;

   begin
      NewGroupBox:=new GroupBox_Type;
      GUI.GroupBox.GroupBox_Access(NewGroupBox).Initialize(Parent);
      NewGroupBox.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);
      NewGroupBox.TopFrameCanvas:=NewGroupBox.NewCanvas(1,1);
      NewGroupBox.TopFrameCanvas.Clear(FrameColor);
      NewGroupBox.TopFrameCanvas.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => False);
      NewGroupBox.LeftFrameCanvas:=NewGroupBox.NewCanvas(1,1);
      NewGroupBox.LeftFrameCanvas.Clear(FrameColor);
      NewGroupBox.LeftFrameCanvas.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => True);
      NewGroupBox.RightFrameCanvas:=NewGroupBox.NewCanvas(1,1);
      NewGroupBox.RightFrameCanvas.Clear(FrameColor);
      NewGroupBox.RightFrameCanvas.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);
      NewGroupBox.BottomFrameCanvas:=NewGroupBox.NewCanvas(1,1);
      NewGroupBox.BottomFrameCanvas.Clear(FrameColor);
      NewGroupBox.BottomFrameCanvas.SetAnchors
        (Top    => False,
         Left   => True,
         Right  => True,
         Bottom => True);

      NewGroupBox.Client:=new GUI.Object_Type;
      NewGroupBox.Client.Initialize(GUI.Object_ClassAccess(NewGroupBox));
      NewGroupBox.Client.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      NewGroupBox.SetClient(GUI.Object_ClassAccess(NewGroupBox.Client));

      SetAllBounds(NewGroupBox);

      return GUI.GroupBox.GroupBox_ClassAccess(NewGroupBox);
   end NewGroupBox;
   ---------------------------------------------------------------------------

end YellowBlue.GroupBox;
