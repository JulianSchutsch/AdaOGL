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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Canvas;

package Fonts is

   FailedFontImplementationInitialization : Exception;
   FailedRendering                        : Exception;

   type Font_Type is abstract tagged private;

   procedure CharacterOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : in out Float;
      Y      : in out Float;
      Char   : Wide_Wide_Character;
      Color  : Standard.Canvas.Color_Type) is abstract;

   function Height
     (Font : access Font_Type)
      return Integer is abstract;

   function CharacterAdvance
     (Font : access Font_Type;
      Char : Wide_Wide_Character)
      return Float is abstract;

   function CharacterWidth
     (Font : access Font_Type;
      Char : Wide_Wide_Character)
      return Float is abstract;

   function Kerning
     (Font       : access Font_Type;
      FirstChar  : Wide_Wide_Character;
      SecondChar : Wide_Wide_Character)
      return Float is abstract;

   function TextWidth
     (Font : access Font_Type'Class;
      Text : Unbounded_String)
      return Integer;

   procedure TextOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : Float;
      Y      : Float;
      Text   : Unbounded_String;
      Color  : Standard.Canvas.Color_Type);

   type Font_ClassAccess is access all Font_Type'Class;

   type Attributes_Type is array(Natural range <>) of Unbounded_String;
   type Attributes_Access is access Attributes_Type;

   NoAttributes : Attributes_Type(0..-1);

   procedure Release
     (Font : in out Font_ClassAccess);

   function Lookup
     (Name       : Unbounded_String;
      Size       : Natural;
      Attributes : Attributes_Type)
      return Font_ClassAccess;

   type Load_Access is
     access function
       (Name       : Unbounded_String;
        Size       : Natural;
        Attributes : Attributes_Type)
        return Font_ClassAccess;

   type Unload_Access is
     access procedure
       (Font : Font_ClassAccess);

   procedure Register
     (Name   : Unbounded_String;
      Load   : Load_Access;
      Unload : Unload_Access);

   procedure Unregister
     (Name   : Unbounded_String);

private

   FontNotFound : Exception;

   type Font_Type is abstract tagged
      record
         ReferenceCount : Natural := 1;
         Name           : Unbounded_String;
         Size           : Natural;
         Attributes     : Attributes_Access;
         Next           : Font_ClassAccess;
         Last           : Font_ClassAccess;
         Unload         : Unload_Access;
      end record;

end Fonts;
