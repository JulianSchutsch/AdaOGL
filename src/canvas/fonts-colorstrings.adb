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

pragma Ada_2005;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Basics; use Basics;
with Ada.Text_IO; use Ada.Text_IO;

package body Fonts.ColorStrings is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ColorStringArray_Type,
      Name   => ColorStringArray_Access);
   ---------------------------------------------------------------------------

   function Length
     (ColorString : access ColorString_Type)
      return Integer is
   begin
      return ColorString.Content'Length;
   end Length;
   ---------------------------------------------------------------------------

   function GetAccumulatedWidth
     (ColorString : access ColorString_Type;
      Position    : Integer)
      return Float is
   begin
      if Position=0 then
         return 0.0;
      end if;
      -- TODO: The AccumWidth should actually be a float but isn't yet
      return float(ColorString.Content(Position).AccumWidth);
   end GetAccumulatedWidth;
   ---------------------------------------------------------------------------

   function GetStringSlice
     (ColorString : access ColorString_Type;
      Start       : Integer;
      Stop        : Integer)
      return Unbounded_String is

      Result : Unbounded_String;

   begin

      if (Stop<ColorString.Content'First) or
        (Start>ColorString.Content'Last) then
         raise IndexOutOfRange;
      end if;

      for i in Start..Stop loop
         Result:=Result&UCS4ToUTF8(ColorString.Content(i).Char);
      end loop;
      return Result;

   end GetStringSlice;
   ---------------------------------------------------------------------------

   function GetString
     (ColorString : access ColorString_Type)
      return Unbounded_String is

      Result : Unbounded_String;

   begin

      for i in ColorString.Content'First..ColorString.Content'Last loop
         Result:=Result&UCS4ToUTF8(ColorString.Content(i).Char);
      end loop;
      return Result;

   end GetString;
   ---------------------------------------------------------------------------

   procedure DecodePosition
     (ColorString : access ColorString_Type;
      Position    : Natural;
      WrappedLine : out Natural;
      Offset      : out Integer) is

      LinePosition     : Integer;
      LineNumber       : Integer:=0;
      LastLinePosition : Integer;
      LastLineNumber   : Integer:=0;

      function CalculateOffset
        return Integer is

         Start : Integer;

      begin

         if ColorString.Content'Length=0 then
            return 0;
         end if;

         if LinePosition-1>=ColorString.Content'First then
            Start:=Colorstring.Content(LinePosition-1).AccumWidth;
         else
            Start:=0;
         end if;

         if Position>ColorString.Content'Last then
            return ColorString.Content(ColorString.Content'Last).AccumWidth
              -Start;
         else
            return ColorString.Content(Position-1).AccumWidth
              -Start;
         end if;

      end CalculateOffset;

   begin

      if ColorString.Content=null then
         WrappedLine := 0;
         Offset      := 0;
         return;
      end if;

      LinePosition     := ColorString.Content'First;
      LastLinePosition := LinePosition;

      while LinePosition<=ColorString.Content'Last loop

         if Position in LinePosition..ColorString.Content(LinePosition).NextLine-1 then
            WrappedLine := LineNumber;
            Offset      := CalculateOffset;
            return;
         end if;

         LastLinePosition := LinePosition;
         LinePosition     := ColorString.Content(LinePosition).NextLine;
         LastLineNumber   := LineNumber;
         LineNumber       := LineNumber+1;

      end loop;

      LinePosition := LastLinePosition;
      WrappedLine  := LastLineNumber;
      Offset       := CalculateOffset;
   end DecodePosition;
   ---------------------------------------------------------------------------

   function GetWrappedLine
     (ColorString : access ColorString_Type;
      Position    : Integer)
      return Integer is

      LinePosition : Integer;
      LineNumber   : Integer:=0;

   begin

      if ColorString.Content=null then
         return 0;
      end if;

      LinePosition:=ColorString.Content'First;

      while LinePosition<=ColorString.Content'Last loop

         if Position in LinePosition..ColorString.Content(LinePosition).NextLine-1 then
            return LineNumber;
         end if;

         LinePosition := ColorString.Content(LinePosition).NextLine;
         LineNumber   := LineNumber+1;

      end loop;

      return LineNumber;

   end GetWrappedLine;
   ---------------------------------------------------------------------------

   procedure RenderWrapped
     (ColorString     : in out Colorstring_Type;
      Canvas          : Standard.Canvas.Canvas_ClassAccess;
      X               : Integer;
      Y               : Integer) is

      XPosition : Integer:=X;
      YPosition : Integer:=Y;

   begin

      if ColorString.Font=null
        or ColorString.Content=null then
         raise FontNotAssigned;
      end if;

      if ColorString.CurrentPosition>ColorString.Content'Last then
         return;
      end if;

      for i in ColorString.CurrentPosition
        ..ColorString.Content(ColorString.CurrentPosition).NextLine-1 loop

         ColorString.Font.CharacterOut
           (Canvas => Canvas,
            X      => Float(XPosition),
            Y      => Float(YPosition),
            Char   => ColorString.Content(i).Char,
            Color  => ColorString.Content(i).Color);

      end loop;

   end RenderWrapped;
   ---------------------------------------------------------------------------

   procedure Render
     (ColorString     : in out Colorstring_Type;
      Canvas          : Standard.Canvas.Canvas_ClassAccess;
      X               : Integer;
      Y               : Integer) is

      XPosition : Integer:=X;
      YPosition : Integer:=Y;

   begin

      if ColorString.Font=null then
         raise FontNotAssigned;
      end if;

      if ColorString.Content=null then
         return;
      end if;

      for i in ColorString.Content'Range loop

         ColorString.Font.CharacterOut
           (Canvas => Canvas,
            X      => Float(XPosition),
            Y      => Float(YPosition),
            Char   => ColorString.Content(i).Char,
            Color  => ColorString.Content(i).Color);

      end loop;

   end Render;
   ---------------------------------------------------------------------------

   procedure GreedyWrapping
     (ColorString : in out ColorString_Type;
      Width       : Integer) is

      InWord              : Boolean:=False;
      WordStart           : Integer:=0;
      LineAccumWidth      : Integer:=0;
      LineStart           : Natural;
      LineNext            : Natural;
      LineWordCount       : Integer:=0;
      Position            : Natural;
      Overflow            : Boolean;

      procedure NewLine is
      begin

         ColorString.WrappedLineCount:=ColorString.WrappedLineCount+1;

         if ColorString.Content(LineStart).NextLine/=Position then

            ColorString.Content(LineStart).Modified  := True;
            ColorString.Content(LineStart).NextLine  := Position;
            ColorString.Content(LineStart).LineWidth
              := ColorString.Content(Position-1).AccumWidth-LineAccumWidth;

         end if;


         LineStart      := Position;
         LineAccumWidth := ColorString.Content(Position-1).AccumWidth;
         LineWordCount  := 0;
         InWord         := False;

      end NewLine;
      ------------------------------------------------------------------------

      procedure StopLine is
      begin

         if Position/=LineStart then
            NewLine;
         end if;

         if ColorString.WrappedLineCount=0 then
            ColorString.WrappedLineCount:=1;
         end if;

      end StopLine;
      ------------------------------------------------------------------------

   begin

      if ColorString.Content=null then
         return;
      end if;

      LineStart := ColorString.Content'First;
      Position  := ColorString.Content'First;

      ColorString.WrappedLineCount:=0;

      while Position<=ColorString.Content'Last loop

         Overflow
           :=(ColorString.Content(Position).AccumWidth
              -LineAccumWidth>Width);

         if not InWord then

            if ColorString.Content(Position).Char=' ' then

               if (Position/=LineStart)
                 and Overflow then
                  NewLine;
               end if;

            else

               if (Position/=LineStart)
                 and Overflow then
                  Position:=Position-1;
                  NewLine;
               end if;

               InWord        := True;
               LineWordCount := LineWordCount+1;
               WordStart     := Position;

            end if;

         else

            if ColorString.Content(Position).Char=' ' then

               InWord := False;

               if Overflow then
                  NewLine;
               end if;

            else

               if Overflow then

                  if LineWordCount>1 then
                     Position:=WordStart;
                     InWord := True;
                  end if;

                  NewLine;

               end if;
            end if;

         end if;

         Position:=Position+1;

      end loop;

      StopLine;

      -- TODO: Check if this is even possible (Range /=0)
      if (ColorString.Content=null)
        or (ColorString.Content'First>ColorString.Content'Last) then
         return;
      end if;

      Position:=ColorString.Content'First+1;
      LineNext:=ColorString.Content(ColorString.Content'First).NextLine;

      while Position<=ColorString.Content'Last loop
         if Position/=LineNext then
            ColorString.Content(Position).NextLine:=0;
         else
            LineNext:=ColorString.Content(Position).NextLine;
         end if;
         Position:=Position+1;
      end loop;

   end GreedyWrapping;
   ---------------------------------------------------------------------------

   function FirstWrappedLine
     (ColorString : access ColorString_Type)
      return Boolean is
   begin

      if (ColorString.Content/=null) then

         ColorString.CurrentPosition    := ColorString.Content'First;
         ColorString.CurrentWrappedLine := 0;
         if ColorString.CurrentPosition<=ColorString.Content'Last then
            ColorString.CurrentWidth
              := ColorString.Content(ColorString.CurrentPosition).LineWidth;
         else
            ColorString.CurrentWidth:=0;
         end if;
         return True;

      end if;

      return False;

   end;
   ---------------------------------------------------------------------------

   function NextWrappedLine
     (ColorString : access ColorString_Type)
      return Boolean is
   begin

      if ColorString.Content=null then
         return False;
      end if;

      ColorString.CurrentWrappedLine:=ColorString.CurrentWrappedLine+1;

      if ColorString.CurrentWrappedLine=ColorString.WrappedLineCount then
         ColorString.CurrentWrappedLine := Integer'Last;
         ColorString.CurrentPosition    := Integer'Last;
         return False;
      end if;

      ColorString.CurrentPosition
        :=ColorString.Content(ColorString.CurrentPosition).NextLine;
      ColorString.CurrentWidth
        := ColorString.Content(ColorString.CurrentPosition).LineWidth;

      return True;

   end NextWrappedLine;
   ---------------------------------------------------------------------------

   procedure SelectWrappedLine
     (ColorString : in out ColorString_Type;
      WrappedLine : Natural) is

   begin

      if ColorString.Content=null then
         ColorString.CurrentPosition:=0;
         return;
      end if;

      if WrappedLine>=ColorString.WrappedLineCount then
         raise InvalidWrappedLine;
      end if;

      ColorString.CurrentPosition:=ColorString.Content'First;

      for i in 1..WrappedLine loop
         ColorString.CurrentPosition
           :=ColorString.Content(ColorString.CurrentPosition).NextLine;
      end loop;

      ColorString.CurrentWrappedLine:=WrappedLine;
      if ColorString.CurrentPosition<=ColorString.Content'Last then
         ColorString.CurrentWidth
           := ColorString.Content(ColorString.CurrentPosition).LineWidth;
      else
         ColorString.CurrentWidth:=0;
      end if;

   end SelectWrappedLine;
   ---------------------------------------------------------------------------


   procedure CalculateDimensions
     (ColorString : in out ColorString_Type) is

      AccumWidth   : Integer:=0;
      PreviousChar : Wide_Wide_Character:=Wide_Wide_Character'Val(0);
      ThisChar     : Wide_Wide_Character;

   begin

      if ColorString.Font=null then
         return;
      end if;

      for i in ColorString.Content'Range loop

         ThisChar             := ColorString.Content(i).Char;

         AccumWidth
           := AccumWidth+Integer(ColorString.Font.CharacterAdvance(ThisChar));

         ColorString.Content(i).AccumWidth := AccumWidth;
         ColorString.Content(i).NextLine := 0; -- Reset this to reset wrapping

         AccumWidth
           := AccumWidth+Integer(ColorString.Font.Kerning(PreviousChar,ThisChar));

         PreviousChar:=ThisChar;


      end loop;

   end CalculateDimensions;
   ---------------------------------------------------------------------------

   procedure Clear
     (ColorString : in out ColorString_Type) is
   begin

      if ColorString.Content/=null then
         Free(ColorString.Content);
      end if;

   end Clear;
   ---------------------------------------------------------------------------

   procedure Reinitialize
     (ColorString : in out ColorString_Type;
      Font        : Font_ClassAccess) is
   begin
      ColorString.Font:=Font;
      CalculateDimensions(ColorString);
   end Reinitialize;
   ---------------------------------------------------------------------------

   procedure Delete
     (ColorString : access ColorString_Type;
      Position    : Integer;
      Length      : Positive) is

      NewContent : ColorStringArray_Access;

   begin

      if (Position<ColorString.Content'First)
        or (Position+Length-1>ColorString.Content'Last) then
         raise IndexOutOfRange;
      end if;

      NewContent:=new ColorStringArray_Type
        (1..ColorString.Content'Last-Length);

      for i in 1..Position-1 loop
         NewContent(i):=ColorString.Content(i);
      end loop;

      for i in Position..NewContent'Last loop
         NewContent(i):=ColorString.Content(i+Length);
      end loop;

      Free(ColorString.Content);
      ColorString.Content:=NewContent;

      CalculateDimensions(ColorString.all);

   end Delete;
   ---------------------------------------------------------------------------

   function Insert
     (ColorString : access ColorString_Type;
      Position    : Integer;
      String      : Unbounded_String;
      Color       : Canvas.Color_Type)
      return Integer is

      UCS4       : Unbounded_Wide_Wide_String;
      NewContent : ColorStringArray_Access;

   begin

      Put_Line("COnvert String"&TO_String(String));
      UCS4:=UTF8ToUCS4(String);
      Put_Line("DOne");

      if ColorString.Content=null then
         if Position=1 then
            -- Simply create the entire thing

            ColorString.Initialize
              (String => String,
               Color  => Color,
               Font   => ColorString.Font);

            return Length(UCS4);

         end if;
         raise IndexOutOfRange;
      end if;

      if Position not in ColorString.Content'First..ColorString.Content'Last+1 then
         raise IndexOutOfRange;
      end if;

      NewContent:=new ColorStringArray_Type
        (1..ColorString.Content'Last+Length(UCS4));

      for i in 1..Position-1 loop
         NewContent(i):=ColorString.Content(i);
      end loop;

      for i in 1..Length(UCS4) loop
         NewContent(Position+i-1).Char  := Element(UCS4,i);
         NewContent(Position+i-1).Color := Color;
      end loop;

      for i in Position..ColorString.Content'Last loop
         NewContent(i+Length(UCS4)):=ColorString.Content(i);
      end loop;

      Free(ColorString.Content);

      ColorString.Content:=NewContent;

      CalculateDimensions(ColorString.all);

      return Length(UCS4);

   end Insert;
   ---------------------------------------------------------------------------

   procedure Initialize
     (ColorString : in out ColorString_Type;
      String      : Unbounded_String;
      Color       : Canvas.Color_Type;
      Font        : Font_ClassAccess) is

      UCS4 : Unbounded_Wide_Wide_String;

   begin

      ColorString.Font:=Font;

      UCS4 := UTF8ToUCS4(String);

      if ColorString.Content/=null then
         Free(Colorstring.Content);
      end if;

      ColorString.Content := new ColorStringArray_Type
        (1..Length(UCS4));

      for i in 1..Length(UCS4) loop
         ColorString.Content(i).Color := Color;
         Colorstring.Content(i).Char  := Element(UCS4,i);
      end loop;

      CalculateDimensions(ColorString);

   end Initialize;
   ---------------------------------------------------------------------------

end Fonts.Colorstrings;
