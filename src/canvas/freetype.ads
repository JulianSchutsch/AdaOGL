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
--   30.Mar 2012 Julian Schutsch
--     - Original version
--   26.Apr 2012 Julian Schutsch
--     - change FT_Char_Type from Unsigned_8 to Integer_8

pragma Ada_2005;

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Ada.Unchecked_Conversion;

package Freetype is

   type FT_Library_Opaque is null record;
   type FT_Library_Access is access FT_Library_Opaque;
   type FT_Int_Type is new Interfaces.C.int;
   type FT_UInt_Type is new Interfaces.C.unsigned;
   type FT_ULong_Type is new Interfaces.C.unsigned_long;
   subtype FT_Long_Type is Interfaces.C.long;
   type FT_Error_Type is new Interfaces.C.int;
   type FT_UShort_Type is new Interfaces.C.unsigned_short;
   type FT_Fixed_Type is new Interfaces.C.long;
   type FT_Pos_Type is new Interfaces.C.long;
   type FT_UInt32_Type is new Interfaces.Unsigned_32;
   type FT_Byte_Type is new Interfaces.C.unsigned_char;
   type FT_Char_Type is new Interfaces.Integer_8;
   type FT_Short_Type is new Interfaces.C.short;
   type FT_Int32_Type is new Interfaces.Integer_32;

   type FT_GlyphSlot_Opaque is null record;
   type FT_GlyphSlot_Access is access FT_GlyphSlot_Opaque;

   type FTC_Node_Opaque is null record;
   type FTC_Node_Access is access FTC_Node_Opaque;
   type FTC_Manager_Opaque is null record;
   type FTC_Manager_Access is access all FTC_Manager_Opaque;
   type FTC_SBitCache_Opaque is null record;
   type FTC_SBitCache_Access is access all FTC_SBitCache_Opaque;
   type FTC_ImageCache_Opaque is null record;
   type FTC_ImageCache_Access is access all FTC_ImageCache_Opaque;
   type FTC_CMapCache_Opaque is null record;
   type FTC_CMapCache_Access is access all FTC_CMapCache_Opaque;
   subtype FT_Pointer_Type is System.Address;
   type FTC_FaceID_Type is new FT_Pointer_Type;
   type FT_Generic_Finalizer_Access is
     access procedure
       (object : System.Address);
   pragma Convention(C,FT_Generic_Finalizer_Access);
   type FT_Glyph_Format_Type is new Interfaces.C.long;

   type FT_Render_Mode_Enum is
     (FT_RENDER_MODE_NORMAL,
      FT_RENDER_MODE_LIGHT,
      FT_RENDER_MODE_MONO,
      FT_RENDER_MODE_LCD,
      FT_RENDER_MODE_LCD_V,
      FT_RENDER_MODE_MAX);
   pragma Convention(C,FT_Render_Mode_Enum);

   type FT_Encoding_Enum is
     (FT_ENCODING_UNICODE);

   for FT_Encoding_Enum use
     (FT_ENCODING_UNICODE =>
        Character'Pos('c')
      +Character'Pos('i')*2**8
      +Character'Pos('n')*2**16
      +Character'Pos('u')*2**24);
   pragma Convention(C,FT_Encoding_Enum);

   type FT_Vector_Type is
      record
         x : FT_Pos_Type;
         y : FT_Pos_Type;
      end record;
   pragma Convention(C,FT_Vector_Type);

   type FT_Glyph_Type is
      record
         library : FT_Library_Access;
         clazz   : System.Address; -- FT_Glyph_Class not documented.
         format  : FT_Glyph_Format_Type;
         advance : FT_Vector_Type;
      end record;
   pragma Convention(C,FT_Glyph_Type);
   type FT_Glyph_Access is access FT_Glyph_Type;

   type FT_Generic_Type is
      record
         data      : System.Address;
         finalizer : FT_Generic_Finalizer_Access;
      end record;
   pragma Convention(C,FT_Generic_Type);

   type FT_BBox_Type is
      record
         xMin : FT_Pos_Type;
         yMin : FT_Pos_Type;
         xMax : FT_Pos_Type;
         yMax : FT_Pos_Type;
      end record;

   FT_FACE_FLAG_KERNING : constant:=2**6;

   type FT_Face_Type is
      record
         num_faces           : FT_Long_Type;
         face_index          : FT_Long_Type;
         -- The type is against the definition given by freetype,
         -- but compatible both to the definition and bit wise operations
         face_flags          : Interfaces.C.unsigned_long;
         style_flags         : FT_Long_Type;
         num_glyphs          : FT_Long_Type;
         family_name         : System.Address;
         style_name          : System.Address;
         num_fixed_sizes     : FT_Int_Type;
         available_sizes     : System.Address;
         num_charmaps        : FT_Int_Type;
         charmaps            : System.Address;
         ggeneric            : FT_Generic_Type;
         bbox                : FT_BBox_Type;
         units_per_EM        : FT_UShort_Type;
         ascender            : FT_Short_Type;
         descender           : FT_Short_Type;
         height              : FT_Short_Type;
         max_advance_width   : FT_Short_Type;
         max_advance_height  : FT_Short_Type;
         underline_position  : FT_Short_Type;
         underline_thickness : FT_Short_Type;
         glyph               : FT_GlyphSlot_Access;
         size                : System.Address;
         charmap             : System.Address;
         -- Some private stuff follows, but is not mentioned here
         -- since we never allocate the memory for it.
      end record;
   pragma Convention(C,FT_Face_Type);
   type FT_Face_Access is access all FT_Face_Type;

   type FT_Size_Metrics_Type is
      record
         x_ppem      : FT_UShort_Type;
         y_ppem      : FT_UShort_Type;
         x_scale     : FT_Fixed_Type;
         y_scale     : FT_Fixed_Type;
         ascender    : FT_Pos_Type;
         descender   : FT_Pos_Type;
         height      : FT_Pos_Type;
         max_advance : FT_Pos_Type;
      end record;
   pragma Convention(C,FT_Size_Metrics_Type);

   -- This type is incomplete on purpose. Internal stuff has been left out!
   type FT_Size_Type is
      record
         face     : FT_Face_Access;
         ggeneric : FT_Generic_Type;
         metrics  : FT_Size_Metrics_Type;
      end record;
   pragma Convention(C,FT_Size_Type);
   type FT_Size_Access is access all FT_Size_Type;

   type FTC_Face_Requester_Access is
     access function
       (face_id      : FTC_FaceID_Type;
        library      : FT_Library_Access;
        request_data : FT_Pointer_Type;
        aface        : access FT_Face_Access)
        return FT_Error_Type;
   pragma Convention(C,FTC_Face_Requester_Access);

   type FTC_Scaler_Type is
      record
         face_id : FTC_FaceID_Type;
         width   : FT_UInt_Type;
         height  : FT_UInt_Type;
         pixel   : FT_Int_Type;
         x_res   : FT_UInt_Type;
         y_res   : FT_UInt_Type;
      end record;
   pragma Convention(C,FTC_Scaler_Type);

   type GrayValue_Type is new Interfaces.Unsigned_8;
   type GrayValue_Access is access all GrayValue_Type;

   function "+" (Left : GrayValue_Access; Right : Interfaces.C.size_t)
                 return GrayValue_Access;
   pragma Inline("+");

   type FT_Bitmap_Type is
      record
         rows         : Interfaces.C.int;
         width        : Interfaces.C.int;
         pitch        : Interfaces.C.int;
         buffer       : GrayValue_Access;
         num_grays    : Interfaces.C.short;
         pixel_mode   : Interfaces.C.char;
         palette_mode : Interfaces.C.char;
         palette      : System.Address;
      end record;
   pragma Convention(C,FT_Bitmap_Type);

   type FT_BitmapGlyph_Type is
      record
         root   : FT_Glyph_Type;
         left   : FT_Int_Type;
         top    : FT_Int_Type;
         bitmap : FT_Bitmap_Type;
      end record;
   pragma Convention(C,FT_BitmapGlyph_Type);
   type FT_BitmapGlyph_Access is access FT_BitmapGlyph_Type;

   type FTC_SBit_Type is
      record
         width     : FT_Byte_Type;
         height    : FT_Byte_Type;
         left      : FT_Char_Type;
         top       : FT_Char_Type;
         format    : FT_Byte_Type;
         max_grays : FT_Byte_Type;
         pitch     : FT_Short_Type;
         xadvance  : FT_Char_Type;
         yadvance  : FT_Char_Type;
         buffer    : GrayValue_Access;
      end record;
   pragma Convention(C,FTC_SBit_Type);
   type FTC_SBit_Access is access FTC_SBit_Type;

   ---------------------------------------------------------------------------
   FT_LOAD_DEFAULT : constant:=0;
   FT_LOAD_RENDER  : constant:=4;

   ---------------------------------------------------------------------------

   function FT_Init_FreeType
     (Library : access FT_Library_Access)
      return FT_Error_Type;
   pragma Import(C,FT_Init_FreeType,"FT_Init_FreeType");

   procedure FT_Library_Version
     (Library : FT_Library_Access;
      Major   : access FT_Int_Type;
      Minor   : access FT_Int_Type;
      Patch   : access FT_Int_Type);
   pragma Import(C,FT_Library_Version,"FT_Library_Version");

   procedure FT_Done_FreeType
     (Library : FT_Library_Access);
   pragma Import(C,FT_Done_FreeType,"FT_Done_FreeType");

   function FTC_Manager_New
     (library   : FT_Library_Access;
      max_faces : FT_UInt_Type;
      max_Sizes : FT_UInt_Type;
      max_bytes : FT_ULong_Type;
      requester : FTC_Face_Requester_Access;
      req_data  : FT_Pointer_Type;
      amanager  : access FTC_Manager_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_Manager_New,"FTC_Manager_New");

   function FTC_SBitCache_New
     (manager : FTC_Manager_Access;
      acache  : access FTC_SBitCache_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_SBitCache_New,"FTC_SBitCache_New");

   function FTC_ImageCache_New
     (manager : FTC_Manager_Access;
      acache  : access FTC_ImageCache_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_ImageCache_New,"FTC_ImageCache_New");

   function FTC_CMapCache_New
     (manager : FTC_Manager_Access;
      acache  : access FTC_CMapCache_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_CMapCache_New,"FTC_CMapCache_New");

   procedure FTC_Manager_Done
     (manager : FTC_Manager_Access);
   pragma Import(C,FTC_Manager_Done,"FTC_Manager_Done");

   function FT_New_Face
     (library      : FT_Library_Access;
      filepathname : Interfaces.C.Strings.chars_ptr;
      face_index   : FT_Long_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type;
   pragma Import(C,FT_New_Face,"FT_New_Face");

   function FTC_Manager_LookupFace
     (manager : FTC_Manager_Access;
      face_id : FTC_FaceID_Type;
      aface   : access FT_Face_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_Manager_LookupFace,"FTC_Manager_LookupFace");

   function FTC_Manager_LookupSize
     (manager : FTC_Manager_Access;
      scaler  : access FTC_Scaler_Type;
      asize   : access FT_Size_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_Manager_LookupSize,"FTC_Manager_LookupSize");

   function FTC_CMapCache_Lookup
     (cache      : FTC_CMapCache_Access;
      face_id    : FTC_FaceID_Type;
      cmap_index : FT_Int_Type;
      char_code  : FT_UInt32_Type)
      return FT_UInt_Type;
   pragma Import(C,FTC_CMapCache_Lookup,"FTC_CMapCache_Lookup");

   function FTC_ImageCache_LookupScaler
     (cache      : FTC_ImageCache_Access;
      scaler     : access FTC_Scaler_Type;
      load_Flags : FT_ULong_Type;
      gindex     : FT_UInt_Type;
      aglyph     : access FT_Glyph_Access;
      anode      : access FTC_Node_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_ImageCache_LookupScaler,"FTC_ImageCache_LookupScaler");

   procedure FTC_Node_Unref
     (node    : FTC_Node_Access;
      manager : FTC_Manager_Access);
   pragma Import(C,FTC_Node_Unref,"FTC_Node_Unref");

   function FT_Glyph_To_Bitmap
     (the_glyph   : access FT_Glyph_Access;
      render_mode : FT_Render_Mode_Enum;
      origin      : access FT_Vector_Type;
      destroy     : Interfaces.C.unsigned_char)
      return FT_Error_Type;
   pragma Import(C,FT_Glyph_To_Bitmap,"FT_Glyph_To_Bitmap");

   procedure FT_Done_Glyph
     (glyph : FT_Glyph_Access);
   pragma Import(C,FT_Done_Glyph,"FT_Done_Glyph");

   function FTC_SBitCache_LookupScaler
     (cache      : FTC_SBitCache_Access;
      scaler     : access FTC_Scaler_Type;
      load_flags : FT_ULong_Type;
      gindex     : FT_UInt_Type;
      sbit       : access FTC_SBit_Access;
      anode      : access FTC_Node_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_SBitCache_LookupScaler,"FTC_SBitCache_LookupScaler");

   procedure FTC_Manager_RemoveFaceID
     (manager : FTC_Manager_Access;
      face_id : FTC_FaceID_Type);
   pragma Import(C,FTC_Manager_RemoveFaceID,"FTC_Manager_RemoveFaceID");

   function FT_Get_Kerning
     (face        : FT_Face_Access;
      left_glyph  : FT_UInt_Type;
      right_glyph : FT_UInt_Type;
      kern_mode   : FT_Uint_Type;
      akerning    : access FT_Vector_Type)
      return FT_Error_Type;
   pragma Import(C,FT_Get_Kerning,"FT_Get_Kerning");

   function FT_Get_Char_Index
     (face     : FT_Face_Access;
      charcode : FT_ULong_Type)
      return FT_UInt_Type;
   pragma Import(C,FT_Get_Char_Index,"FT_Get_Char_Index");

   function FT_Get_Glyph
     (slot   : FT_GlyphSlot_Access;
      aglyph : access FT_Glyph_Access)
      return FT_Error_Type;
   pragma Import(C,FT_Get_Glyph, "FT_Get_Glyph");

   function FT_Load_Glyph
     (face        : FT_Face_Access;
      glyph_index : FT_UInt_Type;
      load_flags  : FT_Int32_Type)
      return FT_Error_Type;
   pragma Import(C,FT_Load_Glyph,"FT_Load_Glyph");

   function FT_Select_Charmap
     (face     : FT_Face_Access;
      encoding : FT_Encoding_Enum)
      return FT_Error_Type;
   pragma Import(C,FT_Select_Charmap,"FT_Select_Charmap");

   function FT_HAS_KERNING
     (face : FT_Face_Access)
      return Boolean;

   function Convert is new Ada.Unchecked_Conversion
     (Source => FT_Glyph_Access,
      Target => FT_BitmapGlyph_Access);

end Freetype;
