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

with BitmapFonts.Data; use BitmapFonts.Data;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Fonts;
with Basics; use Basics;
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BitmapFonts is

   type Font_Type is new Fonts.Font_Type with
      record
         Chars : CharArray_Access:=null;
      end record;
   type Font_Access is access Font_Type;

   overriding
   procedure CharacterOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : in out Float;
      Y      : in out Float;
      Char   : Wide_Wide_Character;
      Color  : Standard.Canvas.Color_Type);

   overriding
   function CharacterAdvance
     (Font : access Font_Type;
      Char : Wide_Wide_Character)
      return Float;

   overriding
   function Height
     (Font : access Font_Type)
      return Integer;

   overriding
   function CharacterWidth
     (Font : access Font_Type;
      Char : Wide_Wide_Character)
      return Float;

   overriding
   function Kerning
     (Font       : access Font_Type;
      FirstChar  : Wide_Wide_Character;
      SecondChar : Wide_Wide_Character)
      return Float;
   ---------------------------------------------------------------------------

   procedure CharacterOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : in out Float;
      Y      : in out Float;
      Char   : Wide_Wide_Character;
      Color  : Standard.Canvas.Color_Type) is

      use type Standard.Canvas.Image_Access;

      CharPos : constant Integer:=Wide_Wide_Character'Pos(Char)+Font.Chars'First;
      Glyph   : CharImage_Access;

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;

      Width         : Integer;
      Height        : Integer;
      SourceOriginX : Integer:=0;
      SourceOriginY : Integer:=0;
      Gray          : Integer;

   begin
      if CharPos not in Font.Chars'Range
        or Canvas.Image=null then
         return;
      end if;

      Glyph:=Font.Chars(CharPos).Image;
      Height := Glyph'Length(1);
      Width  := Glyph'Length(2);
      -- Data in Bitmap.Bitmap.buffer
      -- Advance = Glyph.advance.x div 1024 div 64
      -- Top = lbaseline-Bitmap.Top
      -- Left = Bitmap.Left
      X1 := Integer(Float'Rounding(X));
      Y1 := Integer(Float'Rounding(Y));
      X2 := X1+Width-1;
      Y2 := Y1+Height-1;
      if (X2>=0)
        and (X1<Canvas.ContentWidth)
        and (Y2>=0)
        and (Y1<Canvas.ContentHeight) then

         SourceOriginX:=Glyph'First(2);
         SourceOriginY:=Glyph'First(1);
         if X1<0 then
            SourceOriginX:=SourceOriginX-X1;
            X1:=0;
         end if;

         if Y1<0 then
            SourceOriginY:=SourceOriginY-Y1;
            Y1:=0;
         end if;

         if X2>=Canvas.ContentWidth then
            X2:=Canvas.ContentWidth-1;
         end if;

         if Y2>=Canvas.ContentHeight then
            Y2:=Canvas.ContentHeight-1;
         end if;

         for n in Y1..Y2 loop
            for i in X1..X2 loop
               Gray:=Integer(Glyph(n-Y1+SourceOriginY,i-X1+SourceOriginX));
               if Gray/=0 then
                  Canvas.Image(n,i):=Standard.Canvas.PreBlendMix
                    (BackgroundColor => Canvas.Image(n,i),
                     ForegroundColor => Standard.Canvas.MultiplyAlpha(Color,Gray));
               end if;

            end loop;
         end loop;

      end if;

      X:=X+float(Width);

   end CharacterOut;
   ---------------------------------------------------------------------------

   function Height
     (Font : access Font_Type)
      return Integer is
   begin
      return Font.Chars(Font.Chars'First).Image'Length(1);
   end Height;
   ---------------------------------------------------------------------------

   function CharacterWidth
     (Font : access Font_Type;
      Char : Wide_Wide_Character)
      return Float is
      CharPos : constant Integer:=Wide_Wide_Character'Pos(Char)+Font.Chars'First;
   begin
      if CharPos in Font.Chars'Range then
         return float(Font.Chars(CharPos).Image'Length(2));
      else
         return 0.0;
      end if;
   end CharacterWidth;
      ---------------------------------------------------------------------------

   function CharacterAdvance
     (Font : access Font_Type;
      Char : Wide_Wide_Character)
      return Float is
      CharPos : constant Integer:=Wide_Wide_Character'Pos(Char)+Font.Chars'First;
   begin
      if CharPos in Font.Chars'Range then
         return float(Font.Chars(CharPos).Image'Length(2));
      else
         return 0.0;
      end if;
   end CharacterAdvance;


   function Kerning
     (Font       : access Font_Type;
      FirstChar  : Wide_Wide_Character;
      SecondChar : Wide_Wide_Character)
      return Float is

      pragma Unreferenced(Font);
      pragma Unreferenced(FirstChar);
      pragma Unreferenced(SecondChar);

   begin
      return 0.0;
   end Kerning;
   ---------------------------------------------------------------------------

   function Load
     (Name       : Unbounded_String;
      Size       : Natural;
      Attributes : Fonts.Attributes_Type)
      return Fonts.Font_ClassAccess is

      pragma Unreferenced(Attributes);

      NewFont         : Font_Access;
      MinimumDistance : Integer:=Integer'Last;
      BestEntry       : Integer:=0;
      AnyEntry        : Boolean:=False;

   begin

      for i in ConstFonts'Range loop
         if ConstFonts(i).Name=Name then
            if abs(ConstFonts(i).Size-Size)<MinimumDistance then
               MinimumDistance := abs(ConstFonts(i).Size-Size);
               BestEntry       := i;
               AnyEntry        := True;
            end if;
         end if;
      end loop;

      if not AnyEntry then
         return null;
      end if;

      NewFont:=new Font_Type;
      NewFont.Chars:=ConstFonts(BestEntry).Chars;
      return Fonts.Font_ClassAccess(NewFont);

   end Load;
   ---------------------------------------------------------------------------

   procedure Unload
     (Font : Fonts.Font_ClassAccess) is
   begin
      null;
   end Unload;
   ---------------------------------------------------------------------------

   ImplementationName : constant Unbounded_String:=U("Bitmap");

   procedure Register is
   begin

      Fonts.Register
        (Name => ImplementationName,
         Load => Load'Access,
         Unload => Unload'Access);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Fonts.Unregister
        (ImplementationName);

   end Unregister;
   ---------------------------------------------------------------------------

end BitmapFonts;
