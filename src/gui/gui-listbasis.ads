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
--   27.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with GUI.Basics; use GUI.Basics;
with Canvas;
with Fonts;

package GUI.ListBasis is

   IndexOutOfRange    : Exception;
   TopIndexOutOfRange : Exception;

   type OnSelect_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type ListBasis_Public is new Object_Type with
      record
         OnSelect   : OnSelect_Access:=null;
         OnReselect : OnSelect_Access:=null;
      end record;
   type ListBasis_Type is new ListBasis_Public with private;
   type ListBasis_Access is access all ListBasis_Type;

   overriding
   procedure Free
     (Item : access ListBasis_Type);

   overriding
   procedure Resize
     (Item : access ListBasis_Type);

   overriding
   function MouseDown
     (Item   : access ListBasis_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure MouseMove
     (Item   : access ListBasis_Type;
      X      : Integer;
      Y      : Integer);

   overriding
   procedure MouseUp
     (Item   : access ListBasis_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer);

   procedure AddEntry
     (Item   : access ListBasis_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   procedure DeleteEntry
     (Item  : access ListBasis_Type;
      Index : Integer);

   procedure ClearEntries
     (Item : access ListBasis_Type);

   procedure SetFont
     (Item : access ListBasis_Type;
      Font : Fonts.Font_ClassAccess);

   function VisibleLineCount
     (Item : access ListBasis_Type)
      return Integer;

   procedure SetIndex
     (Item  : access ListBasis_Type;
      Index : Integer);

   function GetIndex
     (Item : access ListBasis_Type)
      return Integer;

   procedure SetTopIndex
     (Item     : access ListBasis_Type;
      TopIndex : Integer);

   function PrecalculateListBasisHeight
     (Item       : access ListBasis_Type;
      EntryCount : Integer)
      return Integer;

private

   type ListBasisCanvas_Type;
   type ListBasisCanvas_Access is access ListBasisCanvas_Type;
   type ListBasisCanvas_Type is
      record
         Canvas     : GUI.Canvas_ClassAccess := null;
         LineNumber : Integer;
         Next       : ListBasisCanvas_Access := null;
         Last       : ListBasisCanvas_Access := null;
      end record;

   type ListBasis_Type is new ListBasis_Public with
      record
         Entries       : StringAndColorList_Pack.List;
         TopIndex      : Integer:=0;
         SelectedIndex : Integer:=-1;
         Pressed       : Boolean;
         Font          : Fonts.Font_ClassAccess := null;
         CanvasLines   : ListBasisCanvas_Access := null;
      end record;

end GUI.ListBasis;
