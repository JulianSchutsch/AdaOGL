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

--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Canvas;
with GUIMouse; use GUIMouse;

package body YellowBlue.VerticalScrollBar is

   ButtonHeight           : constant Float:=20.0;
   TriangleBorderDistance : constant Float:=4.0;
   HandleBorderDistance   : constant Integer:=3;
   MinimumBarHeight       : constant Integer:=9;
   BarBorderDistance      : constant Integer:=2;
   BarButtonDistance      : constant Integer:=1;
   SlideTop               : constant Integer
     := Integer(ButtonHeight)+BarButtonDistance;

   ButtonBackgroundColor  : constant Canvas.Color_Type:=16#FF7F7F7F#;
   ButtonTriangleColor    : constant Canvas.Color_Type:=16#FFFFFFFF#;
   ButtonRectangleColor   : constant Canvas.Color_Type:=16#FF000000#;

   SlideFrameColor        : constant Canvas.Color_Type:=16#FF000000#;
   SlideFaceColor         : constant Canvas.Color_Type:=16#00000000#;
   BarFrameColor          : constant Canvas.Color_Type:=16#FF7F7F7F#;
   BarFaceColor           : constant Canvas.Color_Type:=16#FF3F3F3F#;
   BarHandleColor         : constant Canvas.Color_Type:=16#FFFFFFFF#;

   type BarMode_Enum is
     (BarModeNull,
      BarModeNormal);

   type MouseMode_Enum is
     (MouseModeNull,
      MouseModeUpButton,
      MouseModeUpSlide,
      MouseModeBar,
      MouseModeDownSlide,
      MouseModeDownButton);

   type ScrollBar_Type is new GUI.ScrollBar.ScrollBar_Type with
      record
         UpButtonCanvas        : Canvas_ClassAccess:=null;
         DownButtonCanvas      : Canvas_ClassAccess:=null;
         SlideCanvas           : Canvas_ClassAccess:=null;
         BarTopBorderCanvas    : Canvas_ClassAccess:=null;
         BarTopCanvas          : Canvas_ClassAccess:=null;
         BarMiddleCanvas       : Canvas_ClassAccess:=null;
         BarBottomCanvas       : Canvas_ClassAccess:=null;
         BarBottomBorderCanvas : Canvas_ClassAccess:=null;
         BarMode               : BarMode_Enum:=BarModeNull;
         MouseMode             : MouseMode_Enum:=MouseModeNull;
         BarPosition           : Integer:=0;
         BarHeight             : Integer:=0;
         MouseDelta            : Integer:=0;
      end record;
   type ScrollBar_Access is access ScrollBar_Type;

   overriding
   procedure Resize
     (Item : access ScrollBar_Type);

   overriding
   procedure UpdateBarPosition
     (ScrollBar : access ScrollBar_Type);

   overriding
   function MouseDown
     (Item   : access ScrollBar_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure MouseUp
     (Item   : access ScrollBar_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer);

   overriding
   procedure MouseMove
     (Item : access ScrollBar_Type;
      X    : Integer;
      Y    : Integer);

   ---------------------------------------------------------------------------

   procedure MouseMove
     (Item : access ScrollBar_Type;
      X    : Integer;
      Y    : Integer) is

      pragma Unreferenced(X);

      SlideLength : constant Integer
        :=Item.GetBounds.Height-2*Integer(ButtonHeight)
        -2*BarButtonDistance;

      NewPos : Integer;

   begin

      if Item.MouseMode=MouseModeBar then

         NewPos:=((Y-SlideTop-Item.MouseDelta)*(Item.GetMax-Item.GetMin))
           /(SlideLength-Item.BarHeight)+Item.GetMin;

         if NewPos<Item.GetMin then
            NewPos:=Item.GetMin;
         end if;
         if NewPos>Item.GetMax then
            NewPos:=Item.GetMax;
         end if;

         Item.SetPosition(NewPos);

      end if;

   end MouseMove;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Item   : access ScrollBar_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is
      pragma Unreferenced(X);
      pragma Unreferenced(Y);

   begin
      if (Button/=LeftButton) then
         return;
      end if;

      case Item.MouseMode is
         when MouseModeNull =>
            null;
         when MouseModeUpButton =>
            if Item.GetPosition>Item.GetMin then
               Item.SetPosition(Item.GetPosition-1);
            end if;
         when MouseModeUpSlide =>
            null;
         when MouseModeBar =>
            null;
         when MouseModeDownSlide =>
            null;
         when MouseModeDownButton =>
            if Item.GetPosition<Item.GetMax then
               Item.SetPosition(Item.GetPosition+1);
            end if;
      end case;

      Item.MouseMode:=MouseModeNull;

   end MouseUp;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access ScrollBar_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      SlideLength : constant Integer
        :=Item.GetBounds.Height-2*Integer(ButtonHeight)
        -2*BarButtonDistance;


   begin

      if (Item.MouseMode/=MouseModeNull) or (Button/=LeftButton) then
         return True;
      end if;

      if Y<Integer(ButtonHeight) then
         Item.MouseMode:=MouseModeUpButton;
         return True;
      end if;

      if Y>=Item.GetBounds.Height-Integer(ButtonHeight) then
         Item.MouseMode:=MouseModeDownButton;
         return True;
      end if;

      if Item.BarMode=BarModeNull then
         return True;
      end if;

      if (X<BarBorderDistance) or
        (X>=Item.GetBounds.Width-BarBorderDistance) then
         return True;
      end if;

      if (Y<SlideTop)
        or (Y>=SlideTop+SlideLength) then
         return True;
      end if;

      if Y<SlideTop+Item.BarPosition then
         Item.MouseMode:=MouseModeUpSlide;
         return True;
      end if;

      if Y<SlideTop+Item.BarPosition+Item.BarHeight then
         Item.MouseDelta:=Y-SlideTop-Item.BarPosition;
         Item.MouseMode:=MouseModeBar;
         return True;
      end if;

      Item.MouseMode:=MouseModeDownSlide;
      return True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure DrawBarCanvasse
     (ScrollBar : access ScrollBar_Type) is

      BarWidth : constant Integer
        :=ScrollBar.GetBounds.Width-2*BarBorderDistance;

   begin
      if ScrollBar.BarTopBorderCanvas/=null then
         FreeCanvas(ScrollBar.BarTopBorderCanvas);
      end if;
      ScrollBar.BarTopBorderCanvas:=ScrollBar.NewCanvas
        (Height => 1,
         Width  => BarWidth);
      ScrollBar.BarTopBorderCanvas.Clear
        (Color => BarFrameColor);
      ------------------------------------------------------------------------

      if ScrollBar.BarTopCanvas/=null then
         FreeCanvas(ScrollBar.BarTopCanvas);
      end if;
      ScrollBar.BarTopCanvas:=ScrollBar.NewCanvas
        (Height => 1,
         Width  => BarWidth);
      ScrollBar.BarTopCanvas.Clear
        (Color => BarFaceColor);
      ScrollBar.BarTopCanvas.Image(0,0):=BarFrameColor;
      ScrollBar.BarTopCanvas.Image(0,BarWidth-1):=BarFrameColor;
      ------------------------------------------------------------------------

      if ScrollBar.BarMiddleCanvas/=null then
         FreeCanvas(ScrollBar.BarMiddleCanvas);
      end if;
      Scrollbar.BarMiddleCanvas:=ScrollBar.NewCanvas
        (Height => 5,
         Width  => BarWidth);
      ScrollBar.BarMiddleCanvas.Clear
        (Color => BarFaceColor);
      ScrollBar.BarMiddleCanvas.VertLine
        (X      => 0,
         Y      => 0,
         Height => 5,
         Color  => BarFrameColor);
      ScrollBar.BarMiddleCanvas.VertLine
        (X      => BarWidth-1,
         Y      => 0,
         Height => 5,
         Color  => BarFrameColor);
      ScrollBar.BarMiddleCanvas.HorzLine
        (X      => HandleBorderDistance,
         Y      => 0,
         Width  => BarWidth-2*HandleBorderDistance,
         Color  => BarHandleColor);
      ScrollBar.BarMiddleCanvas.HorzLine
        (X      => HandleBorderDistance,
         Y      => 2,
         Width  => BarWidth-2*HandleBorderDistance,
         Color  => BarHandleColor);
      ScrollBar.BarMiddleCanvas.HorzLine
        (X      => HandleBorderDistance,
         Y      => 4,
         Width  => BarWidth-2*HandleBorderDistance,
         Color  => BarHandleColor);
      ------------------------------------------------------------------------

      if ScrollBar.BarBottomCanvas/=null then
         FreeCanvas(ScrollBar.BarBottomCanvas);
      end if;
      ScrollBar.BarBottomCanvas:=ScrollBar.NewCanvas
        (Height => 1,
         Width  => BarWidth);
      ScrollBar.BarBottomCanvas.Clear
        (Color => BarFaceColor);
      ScrollBar.BarBottomCanvas.Image(0,0):=BarFrameColor;
      ScrollBar.BarBottomCanvas.Image(0,BarWidth-1):=BarFrameColor;
      ------------------------------------------------------------------------

      if ScrollBar.BarBottomBorderCanvas/=null then
         FreeCanvas(Scrollbar.BarBottomBorderCanvas);
      end if;
      ScrollBar.BarBottomBorderCanvas:=ScrollBar.NewCanvas
        (Height => 1,
         Width  => BarWidth);
      ScrollBar.BarBottomBorderCanvas.Clear
        (Color => BarFrameColor);
      ------------------------------------------------------------------------
   end DrawBarCanvasse;
   ---------------------------------------------------------------------------

   procedure FreeBarCanvasse
     (ScrollBar : access ScrollBar_Type) is
   begin
      if ScrollBar.BarTopBorderCanvas/=null then
         FreeCanvas(ScrollBar.BarTopBorderCanvas);
      end if;
      if ScrollBar.BarTopCanvas/=null then
         FreeCanvas(ScrollBar.BarTopCanvas);
      end if;
      if ScrollBar.BarMiddleCanvas/=null then
         FreeCanvas(ScrollBar.BarMiddleCanvas);
      end if;
      if ScrollBar.BarBottomCanvas/=null then
         FreeCanvas(ScrollBar.BarBottomCanvas);
      end if;
      if ScrollBar.BarBottomBorderCanvas/=null then
         FreeCanvas(ScrollBar.BarBottomBorderCanvas);
      end if;
   end;
   ---------------------------------------------------------------------------

   procedure UpdateBarPosition
     (ScrollBar : access ScrollBar_Type) is

      BarWidth : constant Integer
        := ScrollBar.GetBounds.Width-2*BarBorderDistance;

      SlideLength : constant Integer
        :=ScrollBar.GetBounds.Height-2*Integer(ButtonHeight)
        -2*BarButtonDistance;

      RequestedTicks : Integer;
      PossibleTicks  : Integer;

      BarPosition  : Integer;
      BarHeight    : Integer;

   begin

      RequestedTicks := ScrollBar.GetMax-ScrollBar.GetMin;
      if (RequestedTicks<=0) or (SlideLength<MinimumBarHeight) then
         if ScrollBar.BarMode/=BarModeNull then
            FreeBarCanvasse(ScrollBar);
            ScrollBar.BarMode:=BarModeNull;
         end if;
         return;
      end if;

      if ScrollBar.BarMode/=BarModeNormal then
         DrawBarCanvasse(ScrollBar);
         ScrollBar.BarMode:=BarModeNormal;
      end if;

      if SlideLength-RequestedTicks>=MinimumBarHeight then
         PossibleTicks := RequestedTicks;
         BarHeight     := SlideLength-RequestedTicks;
      else
         PossibleTicks := SlideLength-MinimumBarHeight;
         BarHeight     := MinimumBarHeight;
      end if;

      BarPosition:=((ScrollBar.GetPosition-ScrollBar.GetMin)*PossibleTicks)
        /RequestedTicks;

      ScrollBar.BarPosition := BarPosition;
      ScrollBar.BarHeight   := BarHeight;

      ScrollBar.BarTopBorderCanvas.SetBounds
        (Top     => SlideTop+BarPosition,
         Left    => BarBorderDistance,
         Height  => 1,
         Width   => BarWidth,
         Visible => True);

      ScrollBar.BarTopCanvas.SetBounds
        (Top     => SlideTop+BarPosition+1,
         Left    => BarBorderDistance,
         Height  => (BarHeight-7)/2,
         Width   => BarWidth,
         Visible => True);

      ScrollBar.BarMiddleCanvas.SetBounds
        (Top     => SlideTop+BarPosition+(BarHeight-7)/2+1,
         Left    => BarBorderDistance,
         Height  => 5,
         Width   => BarWidth,
         Visible => True);

      ScrollBar.BarBottomCanvas.SetBounds
        (Top     => SlideTop+BarPosition+(BarHeight-7)/2+6,
         Left    => BarBorderDistance,
         Height  => BarHeight-7-(BarHeight-7)/2,
         Width   => BarWidth,
         Visible => True);

      ScrollBar.BarBottomBorderCanvas.SetBounds
        (Top     => SlideTop+BarPosition+BarHeight-1,
         Left    => BarBorderDistance,
         Height  => 1,
         Width   => BarWidth,
         Visible => True);

   end UpdateBarPosition;
   ---------------------------------------------------------------------------

   procedure DrawButtons
     (ScrollBar : access ScrollBar_Type) is

      MiddleX : Float;

   begin
      MiddleX := Float(ScrollBar.GetBounds.Width-1)/2.0;

      if ScrollBar.UpButtonCanvas/=null then
         FreeCanvas(ScrollBar.UpButtonCanvas);
      end if;

      ScrollBar.UpButtonCanvas:=ScrollBar.NewCanvas
        (Height => Integer(ButtonHeight),
         Width  => ScrollBar.GetBounds.Width);

      ScrollBar.UpButtonCanvas.Clear
        (Color => ButtonBackgroundColor);
      ScrollBar.UpButtonCanvas.WuLine
        (X1    => MiddleX,
         Y1    => TriangleBorderDistance,
         X2    => Float(ScrollBar.GetBounds.Width)-TriangleBorderDistance-1.0,
         Y2    => ButtonHeight-TriangleBorderDistance-1.0,
         Color => ButtonTriangleColor);
      ScrollBar.UpButtonCanvas.WuLine
        (X1    => MiddleX,
         Y1    => TriangleBorderDistance,
         X2    => TriangleBorderDistance,
         Y2    => ButtonHeight-TriangleBorderDistance-1.0,
         Color => ButtonTriangleColor);
      ScrollBar.UpButtonCanvas.WuLine
        (X1    => TriangleBorderDistance,
         Y1    => ButtonHeight-TriangleBorderDistance-1.0,
         X2    => Float(ScrollBar.GetBounds.Width)-TriangleBorderDistance-1.0,
         Y2    => ButtonHeight-TriangleBorderDistance-1.0,
         Color => ButtonTriangleColor);

      ScrollBar.UpButtonCanvas.Rectangle
        (X      => 0,
         Y      => 0,
         Height => Integer(ButtonHeight),
         Width  => ScrollBar.GetBounds.Width,
         Color  => ButtonRectangleColor);

      ScrollBar.UpButtonCanvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Integer(ButtonHeight),
         Width   => ScrollBar.GetBounds.Width,
         Visible => True);

      if ScrollBar.DownButtonCanvas/=null then
         FreeCanvas(Scrollbar.DownButtonCanvas);
      end if;

      ScrollBar.DownButtonCanvas:=ScrollBar.NewCanvas
        (Height => Integer(ButtonHeight),
         Width  => ScrollBar.GetBounds.Width);

      ScrollBar.DownButtonCanvas.Clear
        (Color => ButtonBackgroundColor);
      ScrollBar.DownButtonCanvas.WuLine
        (X1    => TriangleBorderDistance,
         Y1    => TriangleBorderDistance,
         X2    => Float(ScrollBar.GetBounds.Width)-TriangleBorderDistance-1.0,
         Y2    => TriangleBorderDistance,
         Color => ButtonTriangleColor);
      ScrollBar.DownButtonCanvas.WuLine
        (X1    => TriangleBorderDistance,
         Y1    => TriangleBorderDistance,
         X2    => MiddleX,
         Y2    => ButtonHeight-TriangleBorderDistance-1.0,
         Color => ButtonTriangleColor);
      ScrollBar.DownButtonCanvas.WuLine
        (X1    => Float(ScrollBar.GetBounds.Width)-TriangleBorderDistance-1.0,
         Y1    => TriangleBorderDistance,
         X2    => MiddleX,
         Y2    => ButtonHeight-TriangleBorderDistance-1.0,
         Color => ButtonTriangleColor);

      ScrollBar.DownButtonCanvas.Rectangle
        (X      => 0,
         Y      => 0,
         Height => Integer(ButtonHeight),
         Width  => ScrollBar.GetBounds.Width,
         Color  => ButtonRectangleColor);

      ScrollBar.DownButtonCanvas.SetBounds
        (Top     => ScrollBar.GetBounds.Height-Integer(ButtonHeight),
         Left    => 0,
         Height  => Integer(ButtonHeight),
         Width   => ScrollBar.GetBounds.Width,
         Visible => True);
      ScrollBar.DownButtonCanvas.SetAnchors
        (Top    => False,
         Left   => True,
         Right  => False,
         Bottom => True);

   end DrawButtons;
   ---------------------------------------------------------------------------

   procedure DrawCanvasse
     (ScrollBar : access ScrollBar_Type) is

   begin

      DrawButtons(ScrollBar);

      if ScrollBar.SlideCanvas/=null then
         Freecanvas(ScrollBar.SlideCanvas);
      end if;
      ScrollBar.SlideCanvas:=ScrollBar.NewCanvas
        (Height => 1,
         Width  => ScrollBar.GetBounds.Width);
      ScrollBar.SlideCanvas.Clear
        (Color => SlideFaceColor);
      ScrollBar.SlideCanvas.Image(0,0):=SlideFrameColor;
      ScrollBar.SlideCanvas.Image(0,ScrollBar.GetBounds.Width-1):=SlideFrameColor;

      ScrollBar.SlideCanvas.SetBounds
        (Top     => Integer(ButtonHeight),
         Left    => 0,
         Height  => ScrollBar.GetBounds.Height-2*Integer(ButtonHeight),
         Width   => ScrollBar.GetBounds.Width,
         Visible => True);
      ScrollBar.SlideCanvas.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);

   end DrawCanvasse;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access ScrollBar_Type) is
   begin

      if Item.GetBounds.Width/=Item.GetPrevBounds.Width then
         DrawCanvasse(Item);
      end if;
      UpdateBarPosition(Item);

   end Resize;
   ---------------------------------------------------------------------------

   function NewVerticalScrollBar
     (Parent : Object_ClassAccess)
      return GUI.ScrollBar.ScrollBar_ClassAccess is

      NewScrollBar : ScrollBar_Access;

   begin

      NewScrollbar := new ScrollBar_Type;

      GUI.ScrollBar.Initialize
        (Item => GUI.ScrollBar.ScrollBar_Access(NewScrollBar),
         Parent => Parent);

      return GUI.Scrollbar.ScrollBar_ClassAccess(NewScrollBar);

   end NewVerticalScrollBar;
   ---------------------------------------------------------------------------

end YellowBlue.VerticalScrollbar;
