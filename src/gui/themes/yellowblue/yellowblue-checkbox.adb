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
with GUIMouse; use GUIMouse;

package body YellowBlue.CheckBox is

   CheckCaptionDistance : constant Integer := 4;
   CheckSize            : constant Integer := 19;
   CheckFillColor       : constant Canvas.Color_Type := 16#FF000000#;
   CheckFrameColor      : constant Canvas.Color_Type := 16#FFFFFF00#;
   CheckMarkColor       : constant Canvas.Color_Type := 16#FFFFFF00#;
   TextColor            : constant Canvas.Color_Type := 16#FFFFFFFF#;
   MarkX1               : constant Integer := 3;
   MarkY1               : constant Integer := CheckSize/2-1;
   MarkX2               : constant Integer := CheckSize/2-1;
   MarkY2               : constant Integer := CheckSize-3-1;
   MarkX3               : constant Integer := CheckSize-3-1;
   MarkY3               : constant Integer := 3;

   type CheckBox_Type is new GUI.Checkbox.CheckBox_Type with
      record
         CheckCanvas   : GUI.Canvas_ClassAccess:=null;
         CaptionCanvas : GUI.Canvas_ClassAccess:=null;
         Caption       : Unbounded_String;
         Font          : Fonts.Font_ClassAccess:=null;
         Checked       : Boolean:=False;
      end record;
   type CheckBox_Access is access all Checkbox_Type;

   overriding
   procedure SetCaption
     (Item    : access CheckBox_Type;
      Caption : Unbounded_String);

   overriding
   procedure Resize
     (Item : access CheckBox_Type);

   overriding
   function MouseDown
     (Item   : access CheckBox_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   function IsChecked
     (Item : access CheckBox_Type)
      return Boolean;

   overriding
   procedure SetChecked
     (Item    : access CheckBox_Type;
      Checked : Boolean);
   ---------------------------------------------------------------------------

   function IsChecked
     (Item : access CheckBox_Type)
      return Boolean is
   begin
      return Item.Checked;
   end IsChecked;
   ---------------------------------------------------------------------------

   procedure DrawCheckCanvas
     (Item : access CheckBox_Type) is

   begin

      GUI.FreeCanvas(Item.CheckCanvas);
      Item.CheckCanvas:=Item.NewCanvas
        (Height => CheckSize,
         Width  => CheckSize);
      Item.CheckCanvas.Clear(CheckFillColor);
      Item.CheckCanvas.Rectangle
        (X      => 0,
         Y      => 0,
         Height => CheckSize,
         Width  => CheckSize,
         Color  => CheckFrameColor);
      if Item.Checked then
         Item.CheckCanvas.WuLine
           (X1    => float(MarkX1),
            Y1    => float(MarkY1),
            X2    => float(MarkX2),
            Y2    => float(MarkY2),
            Color => CheckMarkColor);
         Item.CheckCanvas.WuLine
           (X1    => float(MarkX2),
            Y1    => float(MarkY2),
            X2    => float(MarkX3),
            Y2    => float(MarkY3),
            Color => CheckMarkColor);
      end if;

   end DrawCheckCanvas;
   ---------------------------------------------------------------------------

   procedure DrawCaptionCanvas
     (Item : access Checkbox_Type) is

      use type Fonts.Font_ClassAccess;

      TextHeight : constant Integer:=Item.Font.Height;
      TextWidth  : constant Integer:=Item.Font.TextWidth(Item.Caption);

   begin

      GUI.FreeCanvas(Item.CaptionCanvas);
      if Item.Font/=null then
         Item.CaptionCanvas:=Item.NewCanvas
           (Height => TextHeight,
            Width  => TextWidth);
         Item.Font.TextOut
           (Canvas => Canvas.Canvas_ClassAccess(Item.CaptionCanvas),
            X      => 0.0,
            Y      => 0.0,
            Text   => Item.Caption,
            Color  => TextColor);
      end if;

   end DrawCaptionCanvas;
   ---------------------------------------------------------------------------

   procedure SetChecked
     (Item    : access CheckBox_Type;
      Checked : Boolean) is
   begin
      if Item.Checked/=Checked then
         Item.Checked:=Checked;
         DrawCheckCanvas(Item);
         Item.Resize;
      end if;
   end SetChecked;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access CheckBox_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      use type GUI.CheckBox.OnCheckChance_Access;

   begin
      if Button=LeftButton then
         if TestInsideBounds(Item.CheckCanvas.GetBounds,X,Y) then
            Item.Checked:=not Item.Checked;
            DrawCheckCanvas(Item);
            Resize(Item);
            if Item.OnCheckChange/=null then
               Item.OnCheckChange(Item.CallBackObject);
            end if;
         end if;
      end if;
      return True;
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Checkbox_Type) is

      use type GUI.Canvas_ClassAccess;

      Bounds      : constant Bounds_Type:=Item.GetBounds;
      CaptionLeft : Integer:=0;

   begin

      if Item.CheckCanvas/=null then
         Item.CheckCanvas.SetBounds
           (Top     => (Bounds.Height-Item.CheckCanvas.ContentHeight)/2,
            Left    => 0,
            Height  => Item.CheckCanvas.ContentHeight,
            Width   => Item.CheckCanvas.ContentWidth,
            Visible => True);
         CaptionLeft:=Item.CheckCanvas.ContentWidth+CheckCaptionDistance;
      end if;

      if Item.CaptionCanvas/=null then
         Item.CaptionCanvas.SetBounds
           (Top     => (Bounds.Height-Item.CaptionCanvas.ContentHeight)/2,
            Left    => CaptionLeft,
            Height  => Item.CaptionCanvas.ContentHeight,
            Width   => Item.CaptionCanvas.ContentWidth,
            Visible => True);
      end if;

   end Resize;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Item    : access CheckBox_Type;
      Caption : Unbounded_String) is
   begin

      Item.Caption:=Caption;
      DrawCaptionCanvas(Item);
      Item.Resize;

   end SetCaption;
   ---------------------------------------------------------------------------

   function NewCheckBox
     (Parent : GUI.Object_ClassAccess)
      return GUI.Checkbox.Checkbox_ClassAccess is

      NewCheckbox : CheckBox_Access;

   begin
      NewCheckBox:=new Checkbox_Type;
      GUI.CheckBox.Checkbox_Access(NewCheckbox).Initialize(Parent);

      NewCheckBox.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);

      DrawCheckCanvas(NewCheckBox);
      return GUI.Checkbox.CheckBox_ClassAccess(NewCheckBox);
   end NewCheckbox;
   ---------------------------------------------------------------------------

end YellowBlue.Checkbox;
