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

with Interfaces.C;

package body Fonts.FreeType.Large is

   procedure SelectGlyph
     (Font      : access LargeFont_Type;
      Character : FT_UInt_Type;
      Mode      : FT_ULong_Type) is

      Error : FT_Error_Type;

   begin

      if Node/=null then
         FTC_Node_Unref(Node,Manager);
         Node:=null;
      end if;

      Error:=FTC_ImageCache_LookupScaler
        (cache      => ImageCache,
         scaler     => Font.Scaler'Access,
         load_flags => Mode,
         gindex     => Character,
         aglyph     => Glyph'Access,
         anode      => Node'Access);
      if Error/=0 then
         raise FailedRendering
           with "Failed call to FTC_ImageCache_LookupScaler, exit code:"
             &FT_Error_Type'Image(Error);
      end if;

   end SelectGlyph;
   ---------------------------------------------------------------------------

   procedure GlyphOut
     (Font : access LargeFont_Type;
      Canvas     : Standard.Canvas.Canvas_ClassAccess;
      X          : in out Float;
      Y          : in out Float;
      Color      : Standard.Canvas.Color_Type) is

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;

      Bitmap        : FT_BitmapGlyph_Access;
      Width         : Integer;
      Height        : Integer;
      SourceOrigin  : Integer:=0;
      SourceAdd     : Integer:=0;
      Gray          : Integer;
      SourcePointer : GrayValue_Access;

   begin

      Bitmap:=Convert(Glyph);
      Height := Integer(Bitmap.bitmap.rows);
      Width  := Integer(Bitmap.bitmap.width);
      if (Height<0) or (Width<0) then

         raise FailedRendering
           with "Encountered Bitmap with negative Height or Width";
      end if;
      -- Data in Bitmap.Bitmap.buffer
      -- Advance = Glyph.advance.x div 1024 div 64
      -- Top = lbaseline-Bitmap.Top
      -- Left = Bitmap.Left
      X1 := Integer(Float'Rounding(X)+float(Bitmap.left)/64.0);
      Y1 := Integer(Float'Rounding(Y))+Font.BaseLine-Integer(Bitmap.top);
      X2 := X1+Width-1;
      Y2 := Y1+Height-1;
      if (X2>=0)
        and (X1<Canvas.ContentWidth)
        and (Y2>=0)
        and (Y1<Canvas.ContentHeight) then

         if X1<0 then
            SourceOrigin := -X1;
            SourceAdd    := -X1;
            X1:=0;
         end if;
         if Y1<0 then
            SourceOrigin := SourceOrigin-Y1*Width;
            Y1:=0;
         end if;

         if X2>=Canvas.ContentWidth then
            SourceAdd:=SourceAdd+X2-Canvas.ContentWidth+1;
            X2:=Canvas.ContentWidth-1;
         end if;

         if Y2>=Canvas.ContentHeight then
            Y2:=Canvas.ContentHeight-1;
         end if;

         SourcePointer:=Bitmap.bitmap.buffer+Interfaces.C.size_t(SourceOrigin);
         for n in Y1..Y2 loop
            for i in X1..X2 loop
               Gray:=Integer(SourcePointer.all);
               if Gray/=0 then
                  Canvas.Image(n,i):=Standard.Canvas.PreBlendMix
                    (BackgroundColor => Canvas.Image(n,i),
                     ForegroundColor => Standard.Canvas.MultiplyAlpha(Color,Gray));
               end if;
               SourcePointer:=SourcePointer+1;

            end loop;
            SourcePointer:=SourcePointer+Interfaces.C.size_t(SourceAdd);
         end loop;

      end if;

      X:=X+Float(Glyph.advance.x)/(64.0*1024.0);
      Y:=Y+FLoat(Glyph.advance.y)/(64.0*1024.0);

   end GlyphOut;
   ---------------------------------------------------------------------------

   function CharacterAdvance
     (Font : access LargeFont_Type;
      Char : Wide_Wide_Character)
      return Float is
      CharGlyph : FT_UInt_Type;
   begin
      CharGlyph:=FTC_CMapCache_Lookup
        (cache => CMapCache,
         face_id => FTC_FaceID_Type(Font.all'Address),
         cmap_index => -1,
         char_code => Wide_Wide_Character'Pos(Char));
      SelectGlyph(Font,CharGlyph,FT_LOAD_DEFAULT);
      return Float(Glyph.advance.x)/(64.0*1024.0);
   end CharacterAdvance;
   ---------------------------------------------------------------------------

   function CharacterWidth
     (Font : access LargeFont_Type;
      Char : Wide_Wide_Character)
      return Float is
      GlyphIndex : FT_UInt_Type;
      Bitmap : FT_BitmapGlyph_Access;
   begin
      GlyphIndex:=FTC_CMapCache_Lookup
        (cache => CMapCache,
         face_id => FTC_FaceID_Type(Font.all'Address),
         cmap_index => -1,
         char_code => Wide_Wide_Character'Pos(Char));

      SelectGlyph(Font,GlyphIndex,FT_LOAD_RENDER);
      Bitmap:=Convert(Glyph);
      return Float(Bitmap.left)/64.0+Float(Bitmap.bitmap.width);
   end CharacterWidth;
   ---------------------------------------------------------------------------

   procedure CharacterOut
     (Font   : access LargeFont_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : in out Float;
      Y      : in out Float;
      Char   : Wide_Wide_Character;
      Color  : Standard.Canvas.Color_Type) is

      GlyphIndex : FT_UInt_Type;

   begin

      GlyphIndex:=FTC_CMapCache_Lookup
        (cache => CMapCache,
         face_id => FTC_FaceID_Type(Font.all'Address),
         cmap_index => -1,
         char_code => Wide_Wide_Character'Pos(Char));

      SelectGlyph(Font,GlyphIndex,FT_LOAD_RENDER);

      GlyphOut
        (Font       => Font,
         Canvas     => Canvas,
         X          => X,
         Y          => Y,
         Color      => Color);
   end;
   ---------------------------------------------------------------------------

end Fonts.FreeType.Large;
