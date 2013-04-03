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
--   1.Apr 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   A basis component for several similar tasks like
--    * Console
--    * Editor
--    * Text View

pragma Ada_2005;

private with Fonts.ColorStrings;
with Fonts;

package GUI.TextBasis is

   NoFontSelected : Exception;

   type OnVisualChange_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type OnInputEnter_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess;
        Input          : Unbounded_String);

   type TextBasis_Public is new Object_Type with
      record
         OnVisualChange      : OnVisualChange_Access:=null;
         OnInputEnter        : OnInputEnter_Access:=null;
      end record;

   type TextBasis_Type is new TextBasis_Public with private;

   type TextBasis_Access is access all TextBasis_Type;
   type TextBasis_ClassAccess is access all TextBasis_Type'Class;

   type Line_Type is private;
   type Line_Access is access Line_Type;

   procedure Initialize
     (Item   : access TextBasis_Type;
      Parent : Object_ClassAccess);

   overriding
   procedure Free
     (Item : access TextBasis_Type);

   overriding
   procedure Resize
     (Item : access TextBasis_Type);

   overriding
   function MouseDown
     (Item   : access TextBasis_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   procedure WriteLine
     (Item   : access TextBasis_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   procedure InsertBefore
     (Item       : access TextBasis_Type;
      LineNumber : Natural;
      String     : Unbounded_String;
      Color      : Canvas.Color_Type);

   procedure Clear
     (Item : access TextBasis_Type);

   procedure SetFont
     (Item : access TextBasis_Type;
      Font : Fonts.Font_ClassAccess);

   function CharacterInput
     (Item  : access TextBasis_Type;
      Chars : Unbounded_String)
      return Boolean;

   function KeyDown
     (Item : access TextBasis_Type;
      Key : Key_Enum)
      return Boolean;

   procedure EnableInput
     (Item       : access TextBasis_Type;
      LineNumber : Natural;
      Prompt     : Unbounded_String);

   function VisibleLineCount
     (Item : access TextBasis_Type)
      return Integer;

   function WrappedLineCount
     (Item : access TextBasis_Type)
      return Integer;

   function GetWrappedLineIndex
     (Item : access TextBasis_Type)
      return Integer;

   procedure SetWrappedLineIndex
     (TextBasis : access TextBasis_Type;
      Index     : Integer);

private

   use Fonts.ColorStrings;

   type Line_Type is new Fonts.ColorStrings.ColorString_Type with
      record
         Next     : Line_Access:=null;
         Last     : Line_Access:=null;
      end record;

   type CanvasLine_Type;
   type CanvasLine_Access is access CanvasLine_Type;
   type CanvasLine_Type is
      record
         WrappedLine : Natural;
         Line        : Line_Access        := null;
         Canvas      : Canvas_ClassAccess := null;
         Next        : CanvasLine_Access  := null;
         Last        : CanvasLine_Access  := null;
      end record;

   type TextBasis_Type is new TextBasis_Public with
      record
         FirstLine              : Line_Access            := null;
         LastLine               : Line_Access            := null;
         CanvasLines            : CanvasLine_Access      := null;
         Font                   : Fonts.Font_ClassAccess := null;
         SpaceCharWidth         : Integer;
         LineHeight             : Integer;
         WrappedLineIndex       : Natural := 0;
         InWrappedLineIndex     : Natural := 0;
         EditLine               : Line_Access:=null;
         EditPos                : Integer:=0;
         MinimumEditPos         : Integer:=0;
         CursorCanvas           : Canvas_ClassAccess:=null;
      end record;

end GUI.TextBasis;
