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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body GUI.ListBasis is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ListBasisCanvas_Type,
      Name   => ListBasisCanvas_Access);

   function VisibleLineCount
     (Item : access ListBasis_Type)
      return Integer is

   begin
      return Item.GetBounds.Height/Item.Font.Height;
   end VisibleLineCount;
   ---------------------------------------------------------------------------

   function PrecalculateListBasisHeight
     (Item       : access ListBasis_Type;
      EntryCount : Integer)
      return Integer is
   begin
      return EntryCount*Item.Font.Height;
   end PrecalculateListBasisHeight;

   procedure ClearCanvasse
     (Item : access ListBasis_Type) is

      Cursor     : ListBasisCanvas_Access;
      NextCursor : ListBasisCanvas_Access;

   begin

      Cursor:=Item.CanvasLines;
      while Cursor/=null loop
         NextCursor:=Cursor.Next;
         GUI.FreeCanvas(Cursor.Canvas);
         Free(Cursor);
         Cursor:=NextCursor;
      end loop;
      Item.CanvasLines:=null;

   end ClearCanvasse;
   ---------------------------------------------------------------------------

   procedure DrawCanvasse
     (Item : access ListBasis_Type) is

      use type StringandColorList_Pack.Cursor;
      use type Fonts.Font_ClassAccess;

      Entr           : StringAndColorList_Pack.Cursor;
      PreviousCanvas : ListBasisCanvas_Access:=null;
      NewCanvas      : ListBasisCanvas_Access;
      StringAndColor : StringAndColor_Type;
      yPosition      : Integer;
      FontHeight     : Integer;
      TextWidth      : Integer;
      CanvasWidth    : Integer;
      LineNumber     : Integer;
      ObjectWidth    : constant Integer:=Item.GetBounds.Width;

   begin

      ClearCanvasse(Item);
      if Item.Font=null then
         return;
      end if;
      Entr:=Item.Entries.First;

      for i in 1..Item.TopIndex loop
         if Entr=StringAndColorList_Pack.No_Element then
            return;
         end if;
         Entr:=StringAndColorList_Pack.Next(Entr);
      end loop;

      yPosition:=0;
      FontHeight := Item.Font.Height;

      LineNumber:=Item.TopIndex;

      while Entr/=StringAndColorList_Pack.No_Element loop

         StringAndColor:=StringAndColorList_Pack.Element(Entr);

         NewCanvas:=new ListBasisCanvas_Type;
         NewCanvas.LineNumber:=LineNumber;
         TextWidth:=Item.Font.TextWidth(StringAndColor.String);

         if LineNumber/=Item.SelectedIndex then
            CanvasWidth:=Integer'Min(TextWidth,ObjectWidth);
         else
            CanvasWidth := ObjectWidth;
         end if;

         NewCanvas.Canvas:=Item.NewCanvas
           (Height => FontHeight,
            Width  => CanvasWidth);

         declare
            Color      : Canvas.Color_Type;
            Background : Canvas.Color_Type;
         begin

            if LineNumber/=Item.SelectedIndex then
               Background:=0;
               Color:=StringAndColor.Color;
            else
               Background:=16#FF0000FF#;
               Color:=16#FFFFFFFF#;
            end if;

            NewCanvas.Canvas.Clear(Background);
            Item.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(NewCanvas.Canvas),
               X      => 0.0,
               Y      => 0.0,
               Text   => StringAndColor.String,
               Color  => Color);
         end;

         NewCanvas.Canvas.SetBounds
           (Top     => yPosition,
            Left    => 0,
            Height  => FontHeight,
            Width   => CanvasWidth,
            Visible => True);

         NewCanvas.Last:=PreviousCanvas;
         if PreviousCanvas/=null then
            PreviousCanvas.Next:=NewCanvas;
         else
            Item.CanvasLines:=NewCanvas;
         end if;

         PreviousCanvas:=NewCanvas;

         yPosition:=yPosition+FontHeight;
         if YPosition>=Item.GetBounds.Height then
            exit;
         end if;

         Entr       := StringAndColorList_Pack.Next(Entr);
         LineNumber := LineNumber+1;

      end loop;

   end DrawCanvasse;
   ---------------------------------------------------------------------------

   procedure SetTopIndex
     (Item     : access ListBasis_Type;
      TopIndex : Integer) is
   begin

      if (TopIndex<0)
        or (TopIndex>=Integer(Item.Entries.Length)) then
         raise IndexOutOfRange;
      end if;

      if Item.TopIndex/=TopIndex then
         Item.TopIndex:=TopIndex;
         DrawCanvasse(Item);
      end if;
   end SetTopIndex;
   ---------------------------------------------------------------------------

   procedure SetIndex
     (Item  : access ListBasis_Type;
      Index : Integer) is
   begin

      Put("ListBasis.SetIndex");
      New_Line;
      if (Index<-1)
        or (Index>=Integer(Item.Entries.Length)) then
         raise IndexOutOfRange;
      end if;

      if Item.SelectedIndex/=Index then
         Item.SelectedIndex:=Index;
         if Item.OnSelect/=null then
            Item.OnSelect(Item.CallBackObject);
         end if;
         DrawCanvasse(Item);
      else
         if Item.OnReselect/=null then
            Item.OnReselect(Item.CallBackObject);
         end if;
      end if;
      Put("ListBasis.SetIndex.//");

   end SetIndex;
   ---------------------------------------------------------------------------

   function GetIndex
     (Item : access ListBasis_Type)
      return Integer is
   begin
      return Item.SelectedIndex;
   end GetIndex;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access ListBasis_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(X);

      NewSelected : Integer;

   begin
      if Button=LeftButton then
         NewSelected:=Y/Item.Font.Height+Item.TopIndex;
         if NewSelected<0 then
            NewSelected:=0;
         end if;
         if NewSelected>=Integer(Item.Entries.Length) then
            NewSelected:=Integer(Item.Entries.Length)-1;
         end if;
         Item.Pressed:=True;
         Item.SetIndex(NewSelected);
      end if;
      return True;
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (Item   : access ListBasis_Type;
      X      : Integer;
      Y      : Integer) is
   begin
      null;
   end MouseMove;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Item   : access ListBasis_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is
   begin
      null;
   end MouseUp;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access ListBasis_Type) is
   begin

      GUI.Object_Access(Item).Free;

   end Free;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access ListBasis_Type) is
   begin

      DrawCanvasse(Item);

   end Resize;
   ---------------------------------------------------------------------------

   procedure SetFont
     (Item : access ListBasis_Type;
      Font : Fonts.Font_ClassAccess) is

      use type Fonts.Font_ClassAccess;

   begin
      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;
      Item.Font:=Font;
      DrawCanvasse(Item);
   end SetFont;
   ---------------------------------------------------------------------------

   procedure DeleteEntry
     (Item  : access ListBasis_Type;
      Index : Integer) is

      Cursor : StringAndColorList_Pack.Cursor;

   begin
      if Index<0
        or Index>=Integer(Item.Entries.Length) then
         raise IndexOutOfRange;
      end if;
      Cursor:=ElementAtIndex(Item.Entries,Index);
      Item.Entries.Delete(Cursor);
      if Item.SelectedIndex=Index then
         Item.SelectedIndex:=-1;
      elsif Item.SelectedIndex>Index then
         Item.SelectedIndex:=Item.SelectedIndex-1;
      end if;
      DrawCanvasse(Item);
   end DeleteEntry;
   ---------------------------------------------------------------------------

   procedure AddEntry
     (Item   : access ListBasis_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin

      Item.Entries.Append((String,Color));
      DrawCanvasse(Item);

   end AddEntry;
   ---------------------------------------------------------------------------

   procedure ClearEntries
     (Item : access ListBasis_Type) is
   begin

      Item.Entries.Clear;
      DrawCanvasse(Item);

   end ClearEntries;
   ---------------------------------------------------------------------------

end GUI.ListBasis;
