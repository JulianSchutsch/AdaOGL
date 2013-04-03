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

with GUI.TextBasis;
with GUI.ScrollBar;
with YellowBlue.VerticalScrollBar;
with Fonts;
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Canvas;
with Basics; use Basics;

with GUIDefinitions; use GUIDefinitions;

package body YellowBlue.Console is

   type Console_Type is new GUI.Console.Console_Type with
      record
         TextBasis         : GUI.TextBasis.TextBasis_ClassAccess:=null;
         VerticalScrollBar : GUI.ScrollBar.ScrollBar_ClassAccess:=null;
         EditLineNumber    : Natural := 0;
      end record;
   type Console_Access is access all Console_Type;

   overriding
   procedure WriteLine
     (Item   : access Console_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   overriding
   procedure SetFont
     (Item : access Console_Type;
      Font : Fonts.Font_ClassAccess);

   overriding
   procedure Resize
     (Item : access Console_Type);
   ---------------------------------------------------------------------------

   procedure SetFont
     (Item   : access Console_Type;
      Font   : Fonts.Font_ClassAccess) is
   begin
      Item.TextBasis.SetFont
        (Font => Font);
   end SetFont;
   ---------------------------------------------------------------------------

   procedure WriteLine
     (Item   : access Console_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin
      Item.TextBasis.InsertBefore
        (LineNumber => Item.EditLineNumber,
         String     => String,
         Color      => Color);
      Item.EditLineNumber:=Item.EditLineNumber+1;
   end WriteLine;
   ---------------------------------------------------------------------------

   procedure ScrollPositionChange
     (CallBackObject : AnyObject_ClassAccess) is

      use type GUI.TextBasis.TextBasis_ClassAccess;

      Console : constant Console_Access:=Console_Access(CallbackObject);

   begin
      if Console.TextBasis=null then
         return;
      end if;

      Console.TextBasis.SetWrappedLineIndex
        (Index => Console.VerticalScrollBar.GetPosition);

   end ScrollPositionChange;
   ---------------------------------------------------------------------------

   procedure VisualChange
     (CallbackObject : Basics.AnyObject_ClassAccess) is

      Console : constant Console_Access:=Console_Access(CallbackObject);

      use type GUI.ScrollBar.ScrollBar_ClassAccess;

      ScrollRange    : Integer;
      ScrollPosition : Integer;

   begin

      if Console.VerticalScrollBar=null then
         return;
      end if;
      ScrollRange    := Console.TextBasis.WrappedLineCount-Console.TextBasis.VisibleLineCount;
      ScrollPosition := Console.TextBasis.GetWrappedLineIndex;
      if ScrollRange<0 then
         ScrollRange:=0;
      end if;
      if ScrollPosition>=ScrollRange then
         ScrollPosition:=ScrollRange;
      end if;
      if ScrollPosition<0 then
         ScrollPosition:=0;
      end if;

      Console.VerticalScrollBar.SetRange
        (Min      => 0,
         Max      => ScrollRange,
         Position => ScrollPosition);

   end VisualChange;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Console_Type) is
   begin
      if Item.CallBackObject/=null then
         VisualChange(Item.CallBackObject);
      end if;
   end Resize;
   ---------------------------------------------------------------------------

   procedure InputEnter
     (CallBackObject : Basics.AnyObject_ClassAccess;
      Input          : Unbounded_String) is

      use type GUI.TextBasis.OnInputEnter_Access;

      Console : constant Console_Access:=Console_Access(CallbackObject);

   begin

      if Console.OnInputEnter/=null then
         Console.OnInputEnter.all
           (CallBackObject => Console.CallBackObject,
            Input          => Input);
      end if;

   end InputEnter;
   ---------------------------------------------------------------------------

   function NewConsole
     (Parent : Object_ClassAccess)
      return GUI.Console.Console_ClassAccess is

      NewConsole : Console_Access;

   begin

      NewConsole := new Console_Type;

      GUI.Console.Initialize
        (Item   => GUI.Console.Console_Access(NewConsole),
         Parent => Parent);

      NewConsole.TextBasis := new GUI.TextBasis.TextBasis_Type;

      GUI.TextBasis.Initialize
        (Item   => GUI.TextBasis.TextBasis_Access(NewConsole.TextBasis),
         Parent => Object_ClassAccess(NewConsole));

      NewConsole.SetFont
        (Fonts.Lookup
           (Name       => U("Vera"),
            Size       => 18,
            Attributes => Fonts.NoAttributes));

      NewConsole.TextBasis.EnableInput(0,U(">"));
      NewConsole.TextBasis.CallBackObject := AnyObject_ClassAccess(NewConsole);
      NewConsole.TextBasis.OnVisualChange := VisualChange'Access;
      NewConsole.TextBasis.OnInputEnter   := InputEnter'Access;

      NewConsole.VerticalScrollBar
        :=YellowBlue.VerticalScrollBar.NewVerticalScrollBar(Object_ClassAccess(NewConsole));
      NewConsole.VerticalScrollBar.CallBackObject:=AnyObject_ClassAccess(NewConsole);
      NewConsole.VerticalScrollBar.OnPositionChange:=ScrollPositionChange'Access;

      NewConsole.TextBasis.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => 0,
         Width   => -YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Visible => True);
      NewConsole.TextBasis.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      NewConsole.VerticalScrollBar.SetBounds
        (Top     => 0,
         Left    => -YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Height  => 0,
         Width   => YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Visible => True);
      NewConsole.VerticalScrollBar.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);

      NewConsole.FocusStyle  := FocusStyleRedirect;
      NewConsole.SetFocusObject(Object_ClassAccess(NewConsole.TextBasis));

      NewConsole.VerticalScrollBar.FocusStyle:=FocusStyleRedirect;
      NewConsole.VerticalScrollBar.SetFocusObject
        (Object_ClassAccess(NewConsole.TextBasis));


      return GUI.Console.Console_ClassAccess(NewConsole);

   end NewConsole;
   ---------------------------------------------------------------------------

end YellowBlue.Console;
