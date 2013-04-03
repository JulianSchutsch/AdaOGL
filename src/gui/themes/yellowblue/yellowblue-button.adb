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
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Fonts;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Canvas;
with GUIMouse; use GUIMouse;

package body YellowBlue.Button is

   type Button_Type is new GUI.Button.Button_Type with
      record
         Font          : Fonts.Font_ClassAccess:=null;
         Canvas        : Canvas_ClassAccess:=null;
         Pressed       : Boolean:=False;
         PressedWithin : Boolean:=False;
      end record;
   type Button_Access is access Button_Type;

   overriding
   procedure SetCaption
     (Item    : access Button_Type;
      Caption : Unbounded_String);

   overriding
   procedure Resize
     (Item : access Button_Type);

   overriding
   procedure Free
     (Item : access Button_Type);

   overriding
   procedure MouseMove
     (Item : access Button_Type;
      X    : Integer;
      Y    : Integer);

   overriding
   procedure MouseUp
     (Item   : access Button_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer);

   overriding
   function MouseDown
     (Item   : access Button_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Button_Type) is

      use type Fonts.Font_ClassAccess;

   begin
      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;
      GUI.Button.Button_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   procedure DrawCanvas
     (Item : access Button_Type) is

      use type Fonts.Font_ClassAccess;

      Bounds : constant Bounds_Type:=Item.GetBounds;

      BackgroundColor : Canvas.Color_Type;
      FrameColor      : Canvas.Color_Type;
      FontColor       : Canvas.Color_Type;

   begin

      if Item.Canvas/=null then
         FreeCanvas(Item.Canvas);
      end if;
      if (Bounds.Height<=0)
        or (Bounds.Width<=0) then
         return;
      end if;

      Item.Canvas:=Item.NewCanvas
        (Height => Bounds.Height,
         Width  => Bounds.Width);

      if Item.PressedWithin then
         BackgroundColor := 16#FF00007F#;
         FrameColor      := 16#FF000000#;
         FontColor       := 16#FFFFFFFF#;
      else
         BackgroundColor := 16#FF7F7F7F#;
         FrameColor      := 16#FF000000#;
         FontColor       := 16#FF000000#;
      end if;

      Item.Canvas.Clear(BackgroundColor);
      Item.Canvas.Rectangle
        (X => 0,
         Y => 0,
         Height => Bounds.Height,
         Width => Bounds.Width,
         Color => FrameColor);

      Item.Canvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width,
         Visible => True);

      if Item.Font/=null then

         declare
            Width  : constant Integer:=Item.Font.TextWidth(Item.Caption);
            Height : constant Integer:=Item.Font.Height;
         begin
            Item.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(Item.Canvas),
               X => Float((Bounds.Width-Width)/2-1),
               Y => Float((Bounds.Height-Height)/2-1),
               Text => Item.Caption,
               Color => FontColor);
         end;

      end if;

   end DrawCanvas;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access Button_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      Bounds : constant Bounds_Type:=Item.GetBounds;

   begin
      if Button/=LeftButton then
         return True;
      end if;

      Item.PressedWithin := X in 0..Bounds.Width-1
        and Y in 0..Bounds.Height-1;
      Item.Pressed       := True;
      DrawCanvas(Item);

      return True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (Item : access Button_Type;
      X    : Integer;
      Y    : Integer) is
      NewPressedWithin : Boolean;

      Bounds : constant Bounds_Type:=Item.GetBounds;

   begin

      if Item.Pressed then
         NewPressedWithin := X in 0..Bounds.Width-1
           and Y in 0..Bounds.Height-1;
         if NewPressedWithin/=Item.PressedWithin then
            Item.PressedWithin:=NewPressedWithin;
            DrawCanvas(Item);
         end if;
      end if;

   end MouseMove;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Item   : access Button_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is
      pragma Unreferenced(X);
      pragma Unreferenced(Y);
   begin

      if Button=LeftButton and
        Item.Pressed then
         if Item.PressedWithin and
           Item.OnClick/=null then
            Item.OnClick(Item.CallBackObject);
         end if;
         Item.Pressed:=False;
         Item.PressedWithin:=False;
         DrawCanvas(Item);
      end if;

   end MouseUp;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Item    : access Button_Type;
      Caption : Unbounded_String) is
   begin
      GUI.Button.Button_Access(Item).SetCaption(Caption);
      DrawCanvas(Item);
   end SetCaption;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Button_Type) is
   begin
      DrawCanvas(Item);
   end Resize;
   ---------------------------------------------------------------------------

   function NewButton
     (Parent : Object_ClassAccess)
      return Button_ClassAccess is

      NewButton : Button_Access;

   begin
      NewButton:=new Button_Type;
      GUI.Button.Button_Access(NewButton).Initialize(Parent);
      NewButton.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 25,
         Attributes => Fonts.NoAttributes);
      return Button_ClassAccess(NewButton);
   end NewButton;
   ---------------------------------------------------------------------------

end YellowBlue.Button;
