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

with Fonts;
with Basics; use Basics;
with Canvas;

package body YellowBlue.Label is

   TextColor : constant Canvas.Color_Type:=16#FF000000#;

   type Label_Type is new GUI.Label.Label_Type with
      record
         Canvas  : GUI.Canvas_ClassAccess;
         Caption : Unbounded_String;
         Font    : Fonts.Font_ClassAccess:=null;
      end record;
   type Label_Access is access all Label_Type;

   overriding
   procedure SetCaption
     (Item    : access Label_Type;
      Caption : Unbounded_String);

   overriding
   procedure Free
     (Item : access Label_Type);
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Label_Type) is
   begin
      Fonts.Release(Item.Font);
      GUI.Label.Label_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   procedure DrawCanvas
     (Item : access Label_Type) is

      use type Fonts.Font_ClassAccess;

      TextWidth  : Integer;
      TextHeight : Integer;

   begin

      GUI.FreeCanvas(Item.Canvas);
      if Item.Font/=null then
         TextHeight:=Item.Font.Height;
         TextWidth:=Item.Font.TextWidth(Item.Caption);
         Item.Canvas:=Item.NewCanvas
           (Height => TextHeight,
            Width  => TextWidth);
         Item.Canvas.Clear(0);
         Item.Font.TextOut
           (Canvas => Canvas.Canvas_ClassAccess(Item.Canvas),
            X      => 0.0,
            Y      => 0.0,
            Text   => Item.Caption,
            Color  => TextColor);
         Item.Canvas.SetBounds
           (Top     => 0,
            Left    => 0,
            Height  => TextHeight,
            Width   => TextWidth,
            Visible => True);
      end if;

   end DrawCanvas;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Item    : access Label_Type;
      Caption : Unbounded_String) is
   begin
      Item.Caption:=Caption;
      DrawCanvas(Item);
   end SetCaption;
   ---------------------------------------------------------------------------

   function NewLabel
     (Parent : GUI.Object_ClassAccess)
      return GUI.Label.Label_ClassAccess is

      NewLabel : Label_Access;

   begin
      NewLabel:=new Label_Type;
      GUI.Label.Label_Access(NewLabel).Initialize(Parent);
      NewLabel.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);
      return GUI.Label.Label_ClassAccess(NewLabel);
   end NewLabel;
   ---------------------------------------------------------------------------

end YellowBlue.Label;
