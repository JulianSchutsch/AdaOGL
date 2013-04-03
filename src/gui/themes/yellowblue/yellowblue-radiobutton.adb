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
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with GUIMouse; use GUIMouse;

--with Ada.Text_IO; use Ada.Text_IO;

package body YellowBlue.RadioButton is

   CheckCaptionDistance : constant Integer := 4;
   CheckSize            : constant Integer := 19;
   CheckMarkRadius      : constant Integer := CheckSize/2-4;
   CheckFillColor       : constant Canvas.Color_Type := 16#FF000000#;
   CheckFrameColor      : constant Canvas.Color_Type := 16#FFFFFF00#;
   CheckMarkColor       : constant Canvas.Color_Type := 16#FFFFFF00#;
   TextColor            : constant Canvas.Color_Type := 16#FFFFFFFF#;

   type RadioButton_Type is new GUI.RadioButton.RadioButton_Type with
      record
         CheckCanvas   : GUI.Canvas_ClassAccess:=null;
         CaptionCanvas : GUI.Canvas_ClassAccess:=null;
         Font          : Fonts.Font_ClassAccess:=null;
         Caption       : Unbounded_String;
         Checked       : Boolean:=False;
      end record;
   type RadioButton_Access is access all RadioButton_Type;

   overriding
   procedure SetCaption
     (Item    : access RadioButton_Type;
      Caption : Unbounded_String);

   overriding
   procedure Resize
     (Item : access RadioButton_Type);

   overriding
   procedure SetChecked
     (Item : access RadioButton_Type);

   overriding
   procedure ClearChecked
     (Item : access RadioButton_Type);

   overriding
   function MouseDown
     (Item   : access RadioButton_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;
   ---------------------------------------------------------------------------

   procedure DrawCheckCanvas
     (Item : access RadioButton_Type) is
   begin
      GUI.FreeCanvas(Item.CheckCanvas);
      Item.CheckCanvas:=Item.NewCanvas
        (Height => CheckSize,
         Width  => CheckSize);
      Item.CheckCanvas.Clear(0);
      Item.CheckCanvas.FilledCircle
        (X      => CheckSize/2,
         Y      => Checksize/2,
         Radius => CheckSize/2,
         Color  => CheckFillColor);
      if Item.Checked then
         Item.CheckCanvas.FilledCircle
           (X      => CheckSize/2,
            Y      => CheckSize/2,
            Radius => CheckMarkRadius,
            Color  => CheckMarkColor);
      end if;
      Item.CheckCanvas.Circle
        (X      => CheckSize/2,
         Y      => CheckSize/2,
         Radius => CheckSize/2,
         Color  => CheckFrameColor);
   end DrawCheckCanvas;
   ---------------------------------------------------------------------------

   procedure DrawCaptionCanvas
     (Item : access RadioButton_Type) is

      use type Fonts.Font_ClassAccess;

      TextHeight : Integer;
      TextWidth  : Integer;

   begin

      GUI.FreeCanvas(Item.CaptionCanvas);
      if Item.Font/=null then

         TextHeight := Item.Font.Height;
         TextWidth  := Item.Font.TextWidth(Item.Caption);

         Item.CaptionCanvas:=Item.NewCanvas
           (Height => TextHeight,
            Width  => TextWidth);
         Item.CaptionCanvas.Clear(0);
         Item.Font.TextOut
           (Canvas => Canvas.Canvas_ClassAccess(Item.CaptionCanvas),
            X      => 0.0,
            Y      => 0.0,
            Text   => Item.Caption,
            Color  => TextColor);

      end if;

   end DrawCaptionCanvas;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access RadioButton_Type) is

      use type GUI.Canvas_ClassAccess;

      Bounds : constant Bounds_Type:=Item.GetBounds;
      CaptionLeft : Integer:=0;

   begin

      if Item.CheckCanvas/=null then
         Item.CheckCanvas.SetBounds
           (Top     => (Bounds.Height-Item.CheckCanvas.ContentHeight)/2-1,
            Left    => 0,
            Height  => Item.CheckCanvas.ContentHeight,
            Width   => Item.CheckCanvas.ContentWidth,
            Visible => True);
         CaptionLeft:=Item.CheckCanvas.ContentWidth+CheckCaptionDistance;
      end if;

      if Item.CaptionCanvas/=null then
         Item.CaptionCanvas.SetBounds
           (Top     => (Bounds.Height-Item.CaptionCanvas.ContentHeight)/2-1,
            Left    => CaptionLeft,
            Height  => Item.CaptionCanvas.ContentHeight,
            Width   => Item.CaptionCanvas.ContentWidth,
            Visible => True);
      end if;

   end Resize;
   ---------------------------------------------------------------------------

   procedure SetChecked
     (Item : access RadioButton_Type) is
   begin
      if Item.Checked then
         return;
      end if;
      GUI.RadioButton.RadioButton_Access(Item).SetChecked;
      Item.Checked:=True;
      DrawCheckCanvas(Item);
      Item.Resize;
   end SetChecked;
   ---------------------------------------------------------------------------

   procedure ClearChecked
     (Item : access RadioButton_Type) is
   begin
      if not Item.Checked then
         return;
      end if;
      GUI.RadioButton.RadioButton_Access(Item).SetChecked;
      Item.Checked:=False;
      DrawCheckCanvas(Item);
      Item.Resize;
   end ClearChecked;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access RadioButton_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

   begin

      if Button=LeftButton then
         if (X-CheckSize/2)**2+(Y-CheckSize/2)<=(CheckSize/2)**2 then
            Item.SetChecked;
         end if;
      end if;

      return True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Item    : access RadioButton_Type;
      Caption : Unbounded_String) is
   begin
      Item.Caption:=Caption;
      DrawCaptionCanvas(Item);
      Item.Resize;
   end SetCaption;
   ---------------------------------------------------------------------------

   function NewRadioButton
     (Parent : GUI.Object_ClassAccess)
      return GUI.RadioButton.RadioButton_ClassAccess is
      NewRadioButton : RadioButton_Access;
   begin

      NewRadioButton:=new RadioButton_Type;
      GUI.RadioButton.RadioButton_Access(NewRadioButton).Initialize(Parent);

      NewRadioButton.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);

      DrawCheckCanvas(NewRadioButton);

      return GUI.RadioButton.RadioButton_ClassAccess(NewRadioButton);
   end NewRadioButton;
   ---------------------------------------------------------------------------

end YellowBlue.RadioButton;
