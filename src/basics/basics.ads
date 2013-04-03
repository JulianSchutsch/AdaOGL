--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

-- Revision History
--   2.Feb 2012 Julian Schutsch
--     - Original version
--  29.Feb 2012 Julian Schutsch
--     - Fixed range
--   5.Feb 2012 Julian Schutsch
--     - Added Put and ConcatElements function for StringStringMap

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Numerics.Generic_Elementary_Functions;
with System;

package Basics is

   UTF8Exception : Exception;

   type AnyObject_Type is tagged null record;
   type AnyObject_ClassAccess is access all AnyObject_Type'Class;

   package StringList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Unbounded_String,
      "="          => "=");

   package StringStringMap_Pack is new Ada.Containers.Hashed_maps
     (Key_Type => Unbounded_String,
      Element_Type => Unbounded_String,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package StringVector is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Unbounded_String,
      "=" => "=");

   type StringArray is array (Positive range <>) of Unbounded_String;

   procedure Put
     (Item : StringStringMap_Pack.Map);

   procedure Put
     (Address : System.Address);

   function ConcatElements
     (Item      : StringStringMap_Pack.Map;
      Separator : Unbounded_String)
      return Unbounded_String;

   function RoundUpPowerOf2
     (Value : Natural)
      return Natural;

   function UTF8ToUCS4
     (String : Unbounded_String)
      return Unbounded_Wide_Wide_String;

   function UCS2ToUTF8
     (Char : Wide_Character)
      return Unbounded_String;

   function UCS4ToUTF8
     (Char : Wide_Wide_Character)
      return Unbounded_String;

   procedure Swap
     (Value1 : in out Float;
      Value2 : in out Float);
   pragma Inline(Swap);

   function TryStringToInteger
     (String : Unbounded_String;
      Value  : access Integer)
      return Boolean;

   function U
     (Source : String)
      return Unbounded_String renames To_Unbounded_String;

   package FloatNumeric is new Ada.Numerics.Generic_Elementary_Functions
     (Float_Type => Float);

   type CharacterBuffer_Type(Count : Integer) is tagged private;

   procedure AddCharacter
     (Item : in out CharacterBuffer_Type;
      Char : Character);

   procedure ReadString
     (Item : in out CharacterBuffer_Type;
      Str  : out Unbounded_String);

private

   type CharacterBuffer_Type(Count : Integer) is tagged
      record
         Buffer   : String(1..Count);
         Position : Integer:=1;
      end record;

end Basics;
