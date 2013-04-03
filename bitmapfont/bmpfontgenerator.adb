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

-- Revision History
--   28.Apr 2012 Julian Schutsch
--     - Original version

-- TODO: add proper licence link for the Font read here

pragma Ada_2005;

with Canvas;
with Basics; use Basics;
with Fonts;
with Fonts.Freetype;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;

procedure BMPFontGenerator is

   File      : File_Type;
   Font      : Fonts.Font_ClassAccess;
   Directory : String:="../src/canvas";
   FileName  : String:=Directory&"/bitmapfonts-data.ads";
   CurrentSize : Integer;
   CurrentName : Unbounded_String;

   procedure WriteCopyright is
   begin
      Put_Line(File,"-------------------------------------------------------------------------------");
      Put_Line(File,"--   Copyright 2012 Julian Schutsch");
      Put_Line(File,"--");
      Put_Line(File,"--   This file is part of ParallelSim");
      Put_Line(File,"--");
      Put_Line(File,"--   ParallelSim is free software: you can redistribute it and/or modify");
      Put_Line(File,"--   it under the terms of the GNU Affero General Public License as published");
      Put_Line(File,"--   by the Free Software Foundation, either version 3 of the License, or");
      Put_Line(File,"--   (at your option) any later version.");
      Put_Line(File,"--");
      Put_Line(File,"--   ParallelSim is distributed in the hope that it will be useful,");
      Put_Line(File,"--   but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line(File,"--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line(File,"--   GNU Affero General Public License for more details.");
      Put_Line(File,"--");
      Put_Line(File,"--   You should have received a copy of the GNU Affero General Public License");
      Put_Line(File,"--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.");
      Put_Line(File,"------------------------------------------------------------------------------- ");
      Put_Line(File,"");
   end WriteCopyright;
   ---------------------------------------------------------------------------

   procedure WriteHeader is
   begin
      Put_Line(File,"-- This file was created automatically by bmpfontgenerator.");
      Put_Line(File,"");
      Put_Line(File,"with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;");
      Put_Line(File,"");
      Put_Line(File,"package BitmapFonts.Data is");
      Put_Line(File,"");
      Put_Line(File,"   type Gray_Type is range 0..255;");
      Put_Line(File,"   type CharImage_Type is");
      Put_Line(File,"      array(Integer range <>,Integer range <>) of Gray_Type;");
      Put_Line(File,"   type CharImage_Access is access all CharImage_Type;");
      Put_Line(File,"");
      Put_Line(File,"   type Char_Type is");
      Put_Line(File,"      record");
      Put_Line(File,"         Image   : CharImage_Access;");
      Put_Line(File,"         Advance : Integer;");
      Put_Line(File,"      end record;");
      Put_Line(File,"");
      Put_Line(File,"   type CharArray_Type is array(Integer range <>) of Char_Type;");
      Put_Line(File,"   type CharArray_Access is access all CharArray_Type;");
      Put_Line(File,"");
      Put_Line(File,"   type Font_Type is");
      Put_Line(File,"      record");
      Put_Line(File,"         Size  : Integer;");
      Put_Line(File,"         Chars : CharArray_Access;");
      Put_Line(File,"         Name  : Unbounded_String;");
      Put_Line(File,"      end record;");
      Put_Line(File,"   type FontArray_Type is array(Integer range <>) of Font_Type;");
      Put_Line(File,"");
   end WriteHeader;
   ---------------------------------------------------------------------------

   procedure WriteFooter is
   begin
      Put_Line(File,"");
      Put_Line(File,"end BitmapFonts.Data;");
   end WriteFooter;
   ---------------------------------------------------------------------------

   procedure WriteFont is
      Canv : Canvas.Canvas_ClassAccess;

      use type Canvas.Color_Type;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Canvas.Canvas_Type'Class,
         Name   => Canvas.Canvas_ClassAccess);

      CharWidth  : Natural;
      CharHeight : Natural;

      procedure PutInt
        (Int : Integer) is
      begin
         Put(File,To_String(Trim(U(Integer'Image(Int)),Side => Left)));
      end PutInt;
      ------------------------------------------------------------------------

      procedure PutCharName
        (Char : Integer) is
      begin
         Put(File,"Char_"&To_String(CurrentName)&"_");
         PutInt(CurrentSize);
         Put(File,"_");
         PutInt(Char);
      end PutCharName;
      ------------------------------------------------------------------------

      type Advance_Array is array(0..255) of Float;

--      Advances : Advance_Array;

   begin

      Font:=Fonts.Lookup
        (Name       => U("./Vera.ttf"),
         Size       => CurrentSize,
         Attributes => Fonts.NoAttributes);

      for Char in 0..127 loop

         CharHeight := Natural(Font.Height);
         CharWidth  := Natural(Float'Floor(Font.CharacterWidth(Wide_Wide_Character'Val(Char))));

           Canv:=new Canvas.Canvas_Type;
         -- TODO: Check for zero pictures (height=0, width=0)
         Canv.Initialize
           (Height => CharHeight,
            Width  => CharWidth);
         Canv.Clear(16#FF000000#);

         declare
            X : Float:=0.0;
            Y : Float:=0.0;
         begin
            Font.CharacterOut
              (Canvas => Canv,
               X      => X,
               Y      => Y,
               Char   => Wide_Wide_Character'Val(Char),
               Color  => 16#FFFFFFFF#);
         end;

         Put(File,"Char_"&To_String(CurrentName)&"_");
         PutInt(CurrentSize);
         Put(File,"_");
         Put(File,To_String(Trim(U(Integer'Image(Char)),Side => Left)));
         Put(File,": aliased CharImage_Type");
--         Put(File,"(");
--                  &"0..");
--         PutInt(CharHeight-1);
--         Put(File,",0..");
--         PutInt(CharWidth-1);
--         Put(File,")");
         Put_Line(File,":=(");

         for y in Canv.Image'Range(1) loop
            Put(File,"  (");
            for x in Canv.Image'Range(2) loop
               PutInt(Integer(Canv.Image(y,x) and 16#FF#));
               if x/=Canv.Image'Last(2) then
                  Put(File,",");
               else
                  if y/=Canv.Image'Last(1) then
                     Put_Line(File,"),");
                  else
                     Put_Line(File,"));");
                  end if;
               end if;
            end loop;
         end loop;
         Put_Line(File,"");
         Canv.Finalize;
         Free(Canv);

      end loop;

      Put(File,"Chars_"&To_String(CurrentName)&"_");
      PutInt(CurrentSize);
      Put_Line(File,": aliased CharArray_Type:=(");
      for Char in 0..127 loop

         Put(File,"  (Image => ");
         PutCharName(Char);
         Put(File,"'Access)");
         if Char/=127 then
            Put_Line(File,",");
         else
            Put_Line(File,");");
         end if;
      end loop;
      Put_Line(File,"");

      Fonts.Release(Font);

   end WriteFont;

begin

   Fonts.Freetype.Register;

   if Exists(FileName) then
      Delete_File(FileName);
   end if;
   if not Exists(Directory) then
      Create_Directory(Directory);
   end if;

   Create
     (File => File,
      Mode => Out_File,
      Name => FileName);
   WriteCopyright;
   WriteHeader;

   CurrentName:=U("Vera");
   CurrentSize:=12;
   WriteFont;

   CurrentName:=U("Vera");
   CurrentSize:=18;
   WriteFont;

   Put_Line(File," ConstFonts : FontArray_Type:=(");
   Put_Line(File,"  (Size => 12, Chars => Chars_Vera_12'Access, Name => To_Unbounded_String(""Vera"")),");
   Put_Line(File,"  (Size => 18, Chars => Chars_Vera_18'Access, Name => To_Unbounded_String(""Vera"")));");

   WriteFooter;

   Close(File);

   Fonts.FreeType.Unregister;

end BMPFontGenerator;
