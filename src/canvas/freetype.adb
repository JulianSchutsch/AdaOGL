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
with System.Storage_Elements; use System.Storage_Elements;

package body FreeType is

   function FT_HAS_KERNING
     (face : FT_Face_Access)
      return Boolean is
      use type Interfaces.C.unsigned_long;
   begin
      return (face.face_flags and FT_FACE_FLAG_KERNING)/=0;
   end FT_HAS_KERNING;
   ---------------------------------------------------------------------------

   function "+" (Left : GrayValue_Access; Right : Interfaces.C.size_t)
                 return GrayValue_Access is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => GrayValue_Access,
         Target => System.Address);

      function To_GrayValue_Access is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => GrayValue_Access);

   begin
      return To_GrayValue_Access
        (To_Address(Left)
         +Storage_Offset(Right));
   end "+";
   ---------------------------------------------------------------------------

end FreeType;
