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

with Fonts;
with Fonts.ColorStrings;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Canvas;
with GUIDefinitions; use GUIDefinitions;
with GUIMouse; use GUIMouse;
with GUIKeys; use GUIKeys;
With Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Text_IO; use Ada.Text_IO;

package body YellowBlue.Edit is

   FillColor   : constant Canvas.Color_Type:=16#FFFFFFFF#;
   FrameColor  : constant Canvas.Color_Type:=16#FF000000#;
   TextColor   : constant Canvas.Color_Type:=16#FF000000#;
   CursorColor : constant Canvas.Color_Type:=16#FF000000#;
   LeftTextDistance  : constant Integer:=3;
   RightTextDistance : constant Integer:=4;

   type Edit_Type is new GUI.Edit.Edit_Type with
      record
         Canvas         : GUI.Canvas_ClassAccess:=null;
         CursorCanvas   : GUI.Canvas_ClassAccess:=null;
         Font           : Fonts.Font_ClassAccess:=null;
         Text           : aliased Fonts.ColorStrings.ColorString_Type;
         RenderOffset   : Integer:=0;
         CursorPosition : Integer:=1;
      end record;
   type Edit_Access is access all Edit_Type;

   overriding
   procedure Resize
     (Item : access Edit_Type);

   overriding
   function CharacterInput
     (Item  : access Edit_Type;
      Chars : Unbounded_String)
      return Boolean;

   overriding
   function KeyDown
     (Item : access Edit_Type;
      Key  : Key_Enum)
      return Boolean;

   overriding
   function MouseDown
     (Item   : access Edit_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure SetText
     (Item : access Edit_Type;
      Text : Unbounded_String);

   overriding
   function GetText
     (Item : access Edit_Type)
      return Unbounded_String;

   overriding
   procedure Free
     (Item : access Edit_Type);
   ---------------------------------------------------------------------------

   procedure DrawCanvases
     (Item : access Edit_Type) is
      Bounds : constant Bounds_Type:=Item.GetBounds;
   begin
      GUI.FreeCanvas(Item.Canvas);
      Item.Canvas:=Item.NewCanvas
        (Height => Bounds.Height,
         Width  => Bounds.Width);
      Item.Canvas.Clear(FillColor);
      Item.Canvas.Rectangle
        (X      => 0,
         Y      => 0,
         Height => Bounds.Height,
         Width  => Bounds.Width,
         Color  => FrameColor);
      Item.Text.Render
        (Canvas => Canvas.Canvas_ClassAccess(Item.Canvas),
         X      => -Item.RenderOffset+LeftTextDistance,
         Y      => (Bounds.Height-Item.Font.Height)/2-1);
      Item.Canvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width,
         Visible => True);

      Item.CursorCanvas.SetBounds
        (Top     => (Bounds.Height-Item.Font.Height)/2-1,
         Left    =>
           Integer(Item.Text.GetAccumulatedWidth(Item.CursorPosition-1))
          -Item.RenderOffset+LeftTextDistance,
         Height  => Item.Font.Height,
         Width   => 1,
         Visible => True);

   end DrawCanvases;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Edit_Type) is
   begin
      Item.Text.Clear;
      Fonts.Release(Item.Font);
      GUI.Edit.Edit_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access Edit_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is
      pragma Unreferenced(Item,Button,X,Y);
   begin
      return True;
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MakeCursorVisible
     (Item : access Edit_Type) is

      Bounds     : constant Bounds_Type:=Item.GetBounds;
      AccumWidth : constant Integer:=Integer(Item.Text.GetAccumulatedWidth(Item.CursorPosition-1));

   begin

      if AccumWidth-Item.RenderOffset+LeftTextDistance>=Bounds.Width-RightTextDistance then
         Item.RenderOffset:=AccumWidth+LeftTextDistance+RightTextDistance-Bounds.Width;
      end if;
      if AccumWidth-Item.RenderOffset<LeftTextDistance then
         Item.RenderOffset:=AccumWidth-LeftTextDistance;
      end if;

   end MakeCursorVisible;
   ---------------------------------------------------------------------------

   function KeyDown
     (Item : access Edit_Type;
      Key  : Key_Enum)
      return Boolean is
   begin
      case Key is
         when KeyBackspace =>
            if Item.CursorPosition>1 then
               Item.Text.Delete
                 (Position => Item.CursorPosition-1,
                  Length   => 1);
               Item.CursorPosition:=Item.CursorPosition-1;
               MakeCursorVisible(Item);
               DrawCanvases(Item);
            end if;
         when KeyDelete =>
            if Item.CursorPosition<=Item.Text.Length then
               Item.Text.Delete
                 (Position => Item.CursorPosition,
                  Length   => 1);
               DrawCanvases(Item);
            end if;
         when KeyLeft =>
            if Item.CursorPosition>1 then
               Item.CursorPosition:=Item.CursorPosition-1;
               MakeCursorVisible(Item);
               DrawCanvases(Item);
            end if;
         when KeyRight =>
            if Item.CursorPosition<=Item.Text.Length then
               Item.CursorPosition:=Item.CursorPosition+1;
               MakeCursorVisible(Item);
               DrawCanvases(Item);
            end if;
         when others =>
            return False;
      end case;
      return True;
   end KeyDown;
   ---------------------------------------------------------------------------

   function CharacterInput
     (Item  : access Edit_Type;
      Chars : Unbounded_String)
      return Boolean is
   begin
      Put_Line("CharacterInput");
      Item.CursorPosition:=Item.CursorPosition
        +Item.Text.Insert
        (Position => Item.CursorPosition,
         String   => Chars,
         Color    => TextColor);
      MakeCursorVisible(Item);
      DrawCanvases(Item);
      return True;
   end CharacterInput;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Edit_Type) is
   begin
      DrawCanvases(Item);
   end Resize;
   ---------------------------------------------------------------------------

   procedure SetText
     (Item : access Edit_Type;
      Text : Unbounded_String) is
   begin
      Item.Text.Initialize
        (String => Text,
         Color  => TextColor,
         Font   => Item.Font);
      Item.CursorPosition:=1;
      MakeCursorVisible(Item);
      DrawCanvases(Item);
   end SetText;
   ---------------------------------------------------------------------------

   function GetText
     (Item : access Edit_Type)
      return Unbounded_String is
   begin
      return Item.Text.GetString;
   end GetText;
   ---------------------------------------------------------------------------

   function NewEdit
     (Parent : GUI.Object_ClassAccess)
      return GUI.Edit.Edit_ClassAccess is

      NewEdit : Edit_Access;

   begin
      NewEdit:=new Edit_Type;
      GUI.Edit.Edit_Access(NewEdit).Initialize(Parent);
      NewEdit.FocusStyle:=FocusStyleAccept;
      NewEdit.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);
      NewEdit.Text.Initialize
        (String => U(""),
         Color  => TextColor,
         Font   => NewEdit.Font);
      NewEdit.CursorCanvas:=NewEdit.NewCanvas
        (Height => 1,
         Width  => 1);
      NewEdit.CursorCanvas.Clear(CursorColor);
      return GUI.Edit.Edit_ClassAccess(NewEdit);
   end NewEdit;
   ---------------------------------------------------------------------------

end YellowBlue.Edit;
