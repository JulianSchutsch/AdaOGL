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
--   29.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Freetype; use Freetype;

package Fonts.Freetype is

   type FreeTypeFont_Type is abstract new Fonts.Font_Type with private;
   type FreeTypeFont_Access is access all FreeTypeFont_Type;

   procedure Register;
   procedure Unregister;

private

   Library    : aliased FT_Library_Access     := null;
   Manager    : aliased FTC_Manager_Access    := null;
   SBitCache  : aliased FTC_SBitCache_Access  := null;
   ImageCache : aliased FTC_ImageCache_Access := null;
   CMapCache  : aliased FTC_CMapCache_Access  := null;

   VersionMajor : aliased FT_Int_Type;
   VersionMinor : aliased FT_Int_Type;
   VersionPatch : aliased FT_Int_Type;

   Node         : aliased FTC_Node_Access;
   Glyph        : aliased FT_Glyph_Access;
   SBit         : aliased FTC_SBit_Access;

   type FreeTypeFont_ClassAccess is access all FreeTypeFont_Type'Class;
   type FreeTypeFont_Type is abstract new Fonts.Font_Type with
      record
         Filename   : Unbounded_String;
         Index      : FT_Long_Type;
         FaceHandle : aliased FT_Face_Access;
         Scaler     : aliased FTC_Scaler_Type;
         BaseLine   : Integer;
         LineHeight : Integer;
         Kerning    : Boolean;
      end record;

   overriding
   function Height
     (Font : access FreeTypeFont_Type)
      return Integer;

   overriding
   function Kerning
     (Font       : access FreeTypeFont_Type;
      FirstChar  : Wide_Wide_Character;
      SecondChar : Wide_Wide_Character)
      return Float;

   ---------------------------------------------------------------------------

end Fonts.FreeType;
