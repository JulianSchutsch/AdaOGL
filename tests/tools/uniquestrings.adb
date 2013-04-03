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
--   22.Feb 2012 Julian Schutsch
--     - Original version
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body UniqueStrings is

  package NameList is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type  => Unbounded_String,
      "="           => "=");
   use type NameList.Cursor;

   Gen : Generator;
   RandomNameList : NameList.List;

   procedure ClearUniqueStrings is
   begin
      RandomNameList.Clear;
   end ClearUniqueStrings;

   function AnyPreviousRandomString
     return Unbounded_String is

      Index  : Natural;
      Cursor : NameList.Cursor;

   begin
      Index:=Natural(Float'Rounding(Float(Random(Gen)*Float(NameList.Length(RandomNameList)))));
      Cursor := RandomNameList.First;
      for i in 2..Index loop
         Cursor:=NameList.Next(Cursor);
      end loop;
      return NameList.Element(Cursor);
   end AnyPreviousRandomString;


   function AnyRandomString
     (MinimumLength : Positive;
      MaximumLength : Positive)
     return Unbounded_String is

      Chars     : Integer;
      Str       : Unbounded_String;

   begin
      Chars := Positive(Float'Rounding(Float(Random(Gen))*Float(MaximumLength-MinimumLength)))+MinimumLength;
      for i in 1..Chars loop
         Append
           (Source   => Str,
            New_Item => Character'Val(Integer(Float(Random(Gen))*24.0)+65));
      end loop;

      return Str;
   end AnyRandomString;

   function UniqueRandomString
     (MinimumLength : Positive;
      MaximumLength : Positive)
     return Unbounded_String is
      Name   : Unbounded_String;
      Cursor : NameList.Cursor;
   begin
      loop
         Name:=AnyRandomString
           (MinimumLength => MinimumLength,
            MaximumLength => MaximumLength);
         Cursor:=NameList.Find
           (Container => RandomNameList,
            Item      => Name);

         exit when Cursor=NameList.No_Element;
      end loop;
      RandomNameList.Append
        (New_Item => Name);
      return Name;
   end UniqueRandomString;

end UniqueStrings;
