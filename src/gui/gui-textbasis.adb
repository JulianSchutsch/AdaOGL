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
with Canvas;
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body GUI.TextBasis is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Line_Type,
      Name   => Line_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => CanvasLine_Type,
      Name   => CanvasLine_Access);

   function MouseDown
     (Item   : access TextBasis_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(X);
      pragma Unreferenced(Y);
      pragma Unreferenced(Button);

   begin
      return True;
   end MouseDown;
   ---------------------------------------------------------------------------

   function VisibleLineCount
     (Item : access TextBasis_Type)
      return Integer is
   begin
      if Item.LineHeight/=0 then
         return Item.GetBounds.Height/Item.LineHeight;
      else
         return 0;
      end if;
   end VisibleLineCount;
   ---------------------------------------------------------------------------

   function WrappedLineCount
     (Item : access TextBasis_Type)
      return Integer is

      Line  : Line_Access;
      Count : Integer:=0;

   begin

      Line:=Item.FirstLine;
      while Line/=null loop
         Count := Count+Line.WrappedLineCount;
         Line  := Line.Next;
      end loop;

      return Count;

   end WrappedLineCount;
   ---------------------------------------------------------------------------

   procedure FreeCanvasLine
     (Item       : access TextBasis_Type;
      CanvasLine : in out CanvasLine_Access) is
   begin

      if CanvasLine.Canvas/=null then
         FreeCanvas(CanvasLine.Canvas);
      end if;

      Free(CanvasLine);

   end FreeCanvasLine;
   ---------------------------------------------------------------------------

   function GetFirstWrappedLine
     (TextView : access TextBasis_Type;
      Line     : Line_Access)
      return Integer is

      pragma Unreferenced(TextView);

      Cursor       : Line_Access:=Line.Last;
      WrappedLines : Integer:=0;

   begin

      while Cursor/=null loop
         WrappedLines:=WrappedLines+Cursor.WrappedLineCount;
         Cursor:=Cursor.Last;
      end loop;

      return WrappedLines;

   end GetFirstWrappedLine;
   ---------------------------------------------------------------------------

   procedure HideCursor
     (TextBasis : access TextBasis_Type) is
   begin
      TextBasis.CursorCanvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => 0,
         Width   => 0,
         Visible => False);
   end HideCursor;
   ---------------------------------------------------------------------------

   procedure UpdateCursor
     (TextBasis : access TextBasis_Type) is

      WrappedLine : Integer;
      Offset      : Integer;
      VisibleLineCount : Integer;

   begin

      if TextBasis.EditLine=null then
         HideCursor(TextBasis);
         return;
      end if;

      TextBasis.EditLine.DecodePosition
        (Position    => TextBasis.EditPos,
         WrappedLine => WrappedLine,
         Offset      => Offset);
      WrappedLine:=WrappedLine+GetFirstWrappedLine(TextBasis,TextBasis.EditLine);
      if TextBasis.LineHeight/=0 then
         VisibleLineCount:=TextBasis.GetBounds.Height/TextBasis.LineHeight;
      else
         VisibleLineCount:=0;
      end if;
      if (WrappedLine>=TextBasis.WrappedLineIndex) and
        (WrappedLine<=TextBasis.WrappedLineIndex+VisibleLineCount) then
         TextBasis.CursorCanvas.SetBounds
           (Top     => (WrappedLine-TextBasis.WrappedLineIndex)*TextBasis.LineHeight,
            Left    => Offset,
            Height  => TextBasis.LineHeight,
            Width   => 1,
            Visible => True);
      else
         HideCursor(TextBasis);
      end if;

   end UpdateCursor;
   ---------------------------------------------------------------------------

   procedure ClearCanvasLines
     (Item     : access TextBasis_Type) is

      Line     : CanvasLine_Access;
      NextLine : CanvasLine_Access;

   begin
      Line:=Item.CanvasLines;
      while Line/=null loop

         NextLine:=Line.Next;

         FreeCanvasLine
           (Item       => Item,
            CanvasLine => Line);

         Line:=NextLine;

      end loop;

      Item.CanvasLines:=null;

   end ClearCanvasLines;
   ---------------------------------------------------------------------------

   function SelectLine
     (TextView   : access TextBasis_Type;
      LineNumber : Natural)
      return Line_Access is

      Cursor         : Line_Access;
      RemainingLines : Natural:=LineNumber;

   begin
      Cursor:=TextView.FirstLine;
      while Cursor/=null loop

         if RemainingLines=0 then
            return Cursor;
         end if;

         RemainingLines:=RemainingLines-1;
         Cursor:=Cursor.Next;
      end loop;
      return Cursor;
   end SelectLine;
   ---------------------------------------------------------------------------

   procedure SelectWrappedLine
     (Item        : access TextBasis_Type;
      WrappedLine : Natural;
      Line        : out Line_Access) is

      CurrentLine        : Line_Access;
      CurrentWrappedLine : Integer:=0;

   begin

      CurrentLine:=Item.FirstLine;

      while CurrentLine/=null loop

         if WrappedLine in
           CurrentWrappedLine..CurrentWrappedLine+CurrentLine.WrappedLineCount-1 then

            CurrentLine.SelectWrappedLine(WrappedLine-CurrentWrappedLine);
            Line:=CurrentLine;
            return;

         end if;

         CurrentWrappedLine := CurrentWrappedLine+CurrentLine.WrappedLineCount;
         CurrentLine        := CurrentLine.Next;

      end loop;

      Line := null;

   end SelectWrappedLine;
   ---------------------------------------------------------------------------

   procedure RenderCanvasLine
     (Item       : access TextBasis_Type;
      CanvasLine : CanvasLine_Access) is

      use type Fonts.Font_ClassAccess;

   begin
      if Item.Font=null then
         return;
      end if;

      Item.Context.NewCanvas
        (Object => Object_ClassAccess(Item),
         Height => Item.LineHeight,
         Width  => CanvasLine.Line.CurrentWidth,
         Canvas => CanvasLine.Canvas);

      CanvasLine.Canvas.Clear
        (Color => 0);-- 16#FFFFFFFF#);

      CanvasLine.Line.RenderWrapped
        (Canvas => Canvas.Canvas_ClassAccess(CanvasLine.Canvas),
         X      => 0,
         Y      => 0);

      CanvasLine.Canvas.SetBounds
        (Top     => (CanvasLine.WrappedLine-Item.WrappedLineIndex)*Item.LineHeight,
         Left    => 0,
         Height  => Item.LineHeight,
         Width   => CanvasLine.Line.CurrentWidth,
         Visible => True);

   end;
   ---------------------------------------------------------------------------

   function InsertCanvasLine
     (Item         : access TextBasis_Type;
      CanvasLine   : CanvasLine_Access;
      WrappedLine  : Natural;
      Line         : Line_Access)
      return CanvasLine_Access is

      NewCanvasLine : CanvasLine_Access;

   begin

      if Line.CurrentWidth<=0 then
         return CanvasLine;
      end if;

      NewCanvasLine:=new CanvasLine_Type;

      NewCanvasLine.WrappedLine := WrappedLine;
      NewCanvasLine.Last        := CanvasLine;
      NewCanvasLine.Line        := Line;

      if CanvasLine/=null then

         NewCanvasLine.Next      := CanvasLine.Next;
         if NewCanvasLine.Next/=null then
            NewCanvasLine.Next.Last:=NewCanvasLine;
         end if;

         CanvasLine.Next := NewCanvasLine;

      else

         Item.CanvasLines := NewCanvasLine;

      end if;

      RenderCanvasLine
        (Item       => Item,
         CanvasLine => NewCanvasLine);

      return NewCanvasLine;

   end InsertCanvasLine;
   ---------------------------------------------------------------------------

   procedure RenderCanvasLines
     (Item : access TextBasis_Type) is

      CurrentWrappedLine : Natural;
      Line               : Line_Access;
      CanvasLine         : CanvasLine_Access;

      function FindCanvasLine
        return Boolean is

         Cursor : CanvasLine_Access;

      begin

         Cursor:=CanvasLine;
         while Cursor/=null
           and then Cursor.WrappedLine<=CurrentWrappedLine loop
            CanvasLine:=Cursor;
            Cursor:=Cursor.Next;
         end loop;

         return (CanvasLine/=null)
           and then (CanvasLine.WrappedLine=CurrentWrappedLine);

      end FindCanvasLine;
      ------------------------------------------------------------------------

   begin

      CurrentWrappedLine:=Item.WrappedLineIndex;

      SelectWrappedLine
        (Item        => Item,
         WrappedLine => CurrentWrappedLine,
         Line        => Line);

      if Line=null then
         return;
      end if;

      WrappedLineLoop1:
      loop

         CanvasLine:=InsertCanvasLine
           (Item         => Item,
            CanvasLine   => CanvasLine,
            WrappedLine  => CurrentWrappedLine,
            Line         => Line);

         CurrentWrappedLine:=CurrentWrappedLine+1;

         if (CurrentWrappedLine-Item.WrappedLineIndex)*Item.LineHeight>Item.Bounds.Height then
            return;
         end if;
         exit WrappedLineLoop1 when not Line.NextWrappedLine;

      end loop WrappedLineLoop1;

      Line:=Line.Next;

      CanvasLine := Item.CanvasLines;

      LogicalLineLoop:
      while Line/=null loop

         if Line.FirstWrappedLine then

            WrappedLineLoop:
            loop

               if not FindCanvasLine then

                  CanvasLine:=InsertCanvasLine
                    (Item         => Item,
                     CanvasLine   => CanvasLine,
                     WrappedLine  => CurrentWrappedLine,
                     Line         => Line);

               end if;

               CurrentWrappedLine:=CurrentWrappedLine+1;

               exit LogicalLineLoop when (CurrentWrappedLine-Item.WrappedLineIndex)*Item.LineHeight>Item.GetBounds.Height;
               exit WrappedLineLoop when not Line.NextWrappedLine;

            end loop WrappedLineLoop;

         end if;

         Line:=Line.Next;

      end loop LogicalLineLoop;

   end RenderCanvasLines;
   ---------------------------------------------------------------------------

   procedure UpdateCanvasLines
     (TextBasis : access TextBasis_Type) is
   begin
      ClearCanvasLines(TextBasis);
      RenderCanvasLines(TextBasis);
   end UpdateCanvasLines;
   ---------------------------------------------------------------------------

   function GetWrappedLineIndex
     (Item : access TextBasis_Type)
      return Integer is
   begin
      return Item.WrappedLineIndex;
   end GetWrappedLineIndex;
   ---------------------------------------------------------------------------

   procedure WrapAllLines
     (TextBasis : access TextBasis_Type) is

      Line : Line_Access;

   begin

      Line:=TextBasis.FirstLine;
      while Line/=null loop
         Line.GreedyWrapping
           (Width => TextBasis.Bounds.Width);
         Line:=Line.Next;
      end loop;

   end WrapAllLines;
   ---------------------------------------------------------------------------

   procedure SetWrappedLineIndex
     (TextBasis  : access TextBasis_Type;
      Index      : Integer) is
   begin
      --TODO: Make it faster (different call)
      TextBasis.WrappedLineIndex:=Index;

      UpdateCanvasLines(TextBasis);
      UpdateCursor(TextBasis);

   end SetWrappedLineIndex;
   ---------------------------------------------------------------------------

   procedure MakeVisible
     (TextBasis : access TextBasis_Type;
      Line      : Line_Access;
      Position  : Integer) is

      WrappedLine  : Integer;
      VisLines : Integer;

   begin

      if (Line=null)
        or TextBasis.LineHeight<=0 then
         return;
      end if;

      VisLines := VisibleLineCount(TextBasis);
      WrappedLine:=Line.GetWrappedLine(Position)
        +GetFirstWrappedLine(TextBasis,Line);

      if WrappedLine<TextBasis.WrappedLineIndex then
         SetWrappedLineIndex(TextBasis,WrappedLine);
         return;
      end if;

      if WrappedLine-TextBasis.WrappedLineIndex>VisLines then
         SetWrappedLineIndex(TextBasis,WrappedLine-VisLines);
      end if;

      UpdateCanvasLines(TextBasis);
      UpdateCursor(TextBasis);

      if TextBasis.OnVisualChange/=null then
         TextBasis.OnVisualChange.all(TextBasis.CallBackObject);
      end if;

   end MakeVisible;
   ---------------------------------------------------------------------------

   procedure UpdateAfterContentChange
     (TextBasis         : access TextBasis_Type;
      Line              : Line_Access;
      KeepCursorVisible : Boolean) is
   begin

      Line.GreedyWrapping
        (Width => TextBasis.Bounds.Width);

      if KeepCursorVisible then
         MakeVisible
           (TextBasis => TextBasis,
            Line      => TextBasis.EditLine,
            Position  => TextBasis.EditPos);
         return;
      end if;

      UpdateCanvasLines(TextBasis);
      UpdateCursor(TextBasis);

      if TextBasis.OnVisualChange/=null then
         TextBasis.OnVisualChange.all(TextBasis.CallBackObject);
      end if;

   end UpdateAfterContentChange;
   ---------------------------------------------------------------------------

   procedure Clear
     (Item : access TextBasis_Type) is

      Line     : Line_Access;
      NextLine : Line_Access;

   begin

      Line:=Item.FirstLine;
      while Line/=null loop
         NextLine := Line.Next;
         Line.Clear;
         Free(Line);
         Line     := NextLine;
      end loop;

      Item.FirstLine:=null;

      ClearCanvasLines(Item);

   end Clear;
   ---------------------------------------------------------------------------

   -- TODO: Visible Lines change easily with this, update them!
   -- TEMPFIX : Clearing all
   procedure InsertBefore
     (Item       : access TextBasis_Type;
      LineNumber : Natural;
      String     : Unbounded_String;
      Color      : Canvas.Color_Type) is

      NewLine : Line_Access;
      Line    : constant Line_Access:=SelectLine(Item,LineNumber);

   begin
      NewLine:=new Line_Type;
      NewLine.Initialize
        (String      => String,
         Color       => Color,
         Font        => Item.Font);

      NewLine.Next:=Line;
      if Line/=null then
         NewLine.Last:=Line.Last;
         Line.Last:=NewLine;
      end if;

      if NewLine.Last/=null then
         NewLine.Last.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;

      UpdateAfterContentChange
        (TextBasis         => Item,
         Line              => NewLine,
         KeepCursorVisible => True);

   end InsertBefore;
   ---------------------------------------------------------------------------

   function NewLine
     (Item : access TextBasis_Type;
      String : Unbounded_String;
      Color : Canvas.Color_Type)
      return Line_Access is

      NewLine : Line_Access;

   begin

      NewLine:=new Line_Type;
      NewLine.Initialize
        (String      => String,
         Color       => Color,
         Font        => Item.Font);

      NewLine.Last:=Item.LastLine;
      if Item.LastLine/=null then
         Item.LastLine.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;
      Item.LastLine:=NewLine;

      UpdateAfterContentChange
        (TextBasis => Item,
         Line      => NewLine,
         KeepCursorVisible => True);

      return NewLine;

   end NewLine;
   ---------------------------------------------------------------------------

   procedure WriteLine
     (Item   : access TextBasis_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is

      NewLine : Line_Access;
   begin

      NewLine:=new Line_Type;
      NewLine.Initialize
        (String      => String,
         Color       => Color,
         Font        => Item.Font);

      NewLine.Last:=Item.LastLine;
      if Item.LastLine/=null then
         Item.LastLine.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;
      Item.LastLine:=NewLine;

      NewLine.GreedyWrapping(Item.Bounds.Width);
      RenderCanvasLines(Item);

   end WriteLine;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access TextBasis_Type) is
   begin

      if (Item.Bounds.Height=Item.PrevBounds.Height)
        and (Item.Bounds.Width=Item.PrevBounds.Width) then
         return;
      end if;

      if Item.Bounds.Width/=Item.PrevBounds.Width then
         WrapAllLines(Item);
      end if;

      ClearCanvasLines(Item);
      UpdateCanvasLines(Item);
      UpdateCursor(Item);

      if Item.OnVisualChange/=null then
         Item.OnVisualChange.all(Item.CallBackObject);
      end if;

   end Resize;
   ---------------------------------------------------------------------------

   procedure Reinitialize
     (Item : access TextBasis_Type) is

      Line : Line_Access;

   begin

      Line:=Item.FirstLine;
      while Line/=null loop
         Line.Reinitialize(Item.Font);
         Line:=Line.Next;
      end loop;

   end Reinitialize;
   ---------------------------------------------------------------------------

   procedure UpdateAll
     (TextBasis : access TextBasis_Type) is
   begin
      WrapAllLines(TextBasis);
      UpdateCanvasLines(TextBasis);
      UpdateCursor(TextBasis);
   end UpdateAll;
   ---------------------------------------------------------------------------

   procedure SetFont
     (Item : access TextBasis_Type;
      Font : Fonts.Font_ClassAccess) is
   begin
      -- TODO: Calculate Font Specific things.
      Item.Font           := Font;
      Item.SpaceCharWidth := Font.TextWidth(To_Unbounded_String(" "));
      Item.LineHeight     := Font.Height;

      Reinitialize(Item);
      UpdateAll(Item);

   end SetFont;
   ---------------------------------------------------------------------------

   procedure EnableInput
     (Item       : access TextBasis_Type;
      LineNumber : Natural;
      Prompt     : Unbounded_String) is
   begin

      Item.EditLine := SelectLine(Item,LineNumber);

      if Item.EditLine=null then
         Item.EditLine:=NewLine(Item,To_Unbounded_String(""),16#FFFFFFFF#);
      end if;

      Item.EditPos:=1+Item.EditLine.Insert
        (Position => 1,
         String   => Prompt,
         Color    => 16#FF00FF00#);

      Item.MinimumEditPos:=Item.EditPos;

      UpdateAfterContentChange
        (TextBasis         => Item,
         Line              => Item.EditLine,
         KeepCursorVisible => True);

   end EnableInput;
   ---------------------------------------------------------------------------

   function KeyDown
     (Item : access TextBasis_Type;
      Key  : Key_Enum)
      return Boolean is

   begin
      case Key is
         when KeyBackspace =>
            if (Item.EditLine/=null) and (Item.EditPos>Item.MinimumEditPos) then
               Item.EditPos:=Item.EditPos-1;
               Item.EditLine.Delete
                 (Position => Item.EditPos,
                  Length   => 1);
               UpdateAfterContentChange
                 (TextBasis         => Item,
                  Line              => Item.EditLine,
                  KeepCursorVisible => True);
            end if;
            return True;

         when KeyReturn =>

            if Item.EditLine/=null then

               if Item.OnInputEnter/=null then
                  if Item.EditLine.Length+1>Item.MinimumEditPos then
                     Item.OnInputEnter.all
                       (CallBackObject => Item.CallBackObject,
                        Input          => Item.EditLine.GetStringSlice
                          (Start => Item.MinimumEditPos,
                           Stop => Item.EditLine.Length));
                  else
                     Item.OnInputEnter.all
                       (CallBackObject => Item.CallBackObject,
                        Input          => To_Unbounded_String(""));
                  end if;
               end if;

               Item.EditPos:=Item.MinimumEditPos;

               if Item.EditLine.Length+1>Item.MinimumEditPos then
                  Item.EditLine.Delete
                    (Position => Item.MinimumEditPos,
                     Length   => Item.EditLine.Length-Item.MinimumEditPos+1);
                  UpdateAfterContentChange
                    (TextBasis         => Item,
                     Line              => Item.EditLine,
                     KeepCursorVisible => True);
               end if;

            end if;

            return True;

         when KeyLeft =>
            if Item.EditLine/=null then
               if Item.EditPos>Item.MinimumEditPos then
                  Item.EditPos:=Item.EditPos-1;
                  UpdateAfterContentChange
                    (TextBasis         => Item,
                     Line              => Item.EditLine,
                     KeepCursorVisible => True);
               end if;
            end if;
            return True;

         when KeyRight =>
            if Item.EditLine/=null then
               if Item.EditPos<=Item.EditLine.Length then
                  Item.EditPos:=Item.EditPos+1;
                  UpdateAfterContentChange
                    (TextBasis         => Item,
                     Line              => Item.EditLine,
                     KeepCursorVisible => True);
               end if;
            end if;
            return True;

         when KeyHome =>
            if Item.EditLine/=null then
               if Item.EditPos/=Item.MinimumEditPos then
                  Item.EditPos:=Item.MinimumEditPos;
                  UpdateAfterContentChange
                    (TextBasis         => Item,
                     Line              => Item.EditLine,
                     KeepCursorVisible => True);
               end if;
            end if;
            return True;

         when KeyEnd =>
            if Item.EditLine/=null then
               if Item.EditPos/=Item.EditLine.Length+1 then
                  Item.EditPos:=Item.EditLine.Length+1;
                  UpdateAfterContentChange
                    (TextBasis         => Item,
                     Line              => Item.EditLine,
                     KeepCursorVisible => True);
               end if;
            end if;
            return True;

         when KeyDelete =>
            if Item.EditLine/=null then
               if Item.EditPos<=Item.EditLine.Length then
                  Item.EditLine.Delete
                    (Position => Item.EditPos,
                     Length   => 1);
                  UpdateAfterContentChange
                    (TextBasis         => Item,
                     Line              => Item.EditLine,
                     KeepCursorVisible => True);
               end if;
            end if;
            return True;

         when others =>
            return False;
      end case;
   end KeyDown;
   ---------------------------------------------------------------------------

   function CharacterInput
     (Item  : access TextBasis_Type;
      Chars : Unbounded_String)
      return Boolean is
   begin

      if Item.EditLine=null then
         return False;
      end if;
      Item.EditPos:=Item.EditPos+Item.EditLine.Insert
        (Position => Item.EditPos,
         String   => Chars,
         Color    => 16#FFFFFFFF#);

      UpdateAfterContentChange
        (TextBasis         => Item,
         Line              => Item.EditLine,
         KeepCursorVisible => True);

      return True;

   end CharacterInput;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item   : access TextBasis_Type;
      Parent : Object_ClassAccess) is
   begin

      GUI.Initialize
        (Item   => Object_Access(Item),
         Parent => Parent);

      Item.FocusStyle:=FocusStyleAccept;

      Item.Context.NewCanvas
        (Object => Object_ClassAccess(Item),
         Height => 1,
         Width  => 1,
         Canvas => Item.CursorCanvas);

      Item.CursorCanvas.Clear
        (Color => 16#FFFFFFFF#);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access TextBasis_Type) is

      use type Fonts.Font_ClassAccess;

   begin

      Clear(Item);

      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;

      GUI.Object_Access(Item).Free;

   end Free;
   ---------------------------------------------------------------------------

end GUI.TextBasis;
