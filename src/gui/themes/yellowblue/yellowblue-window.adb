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

with Fonts;
with Canvas;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with GUIMouse; use GUIMouse;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body YellowBlue.Window is

   TitleBarHeight   : constant Integer := 24;
   LineWidth        : constant Integer := 1;
   BorderSpaceWidth : constant Integer := 4;
   BorderWidth      : constant Integer := LineWidth*2+BorderSpaceWidth;
   CornerSize       : constant Integer := 2*BorderWidth;
   TopBarHeight     : constant Integer := TitleBarHeight+2*BorderWidth;
   ButtonWidth      : constant Integer := 18;
   ButtonHeight     : constant Integer := 18;
   ButtonSpace      : constant Integer := 4;

   ButtonFrameColor  : constant Canvas.Color_Type := 16#FFFFFFFF#;
   PButtonFrameColor : constant Canvas.Color_Type := 16#FFFF0000#;

   NormalBackgroundColor     : constant Canvas.Color_Type := 16#EF3F3F3F#;
   NormalBorderLineColor     : constant Canvas.Color_Type := 16#FFCFCFCF#;
   NormalBorderEdgeLineColor : constant Canvas.Color_Type := 16#FF7F7F7F#;
   NormalClientColor         : constant Canvas.Color_Type := 16#EF7F7F7F#;
   NormalTitleBarColor       : constant Canvas.Color_Type := 16#EF00007F#;

   FocussedBackgroundColor     : constant Canvas.Color_Type := 16#EF7F7F7F#;
   FocussedBorderLineColor     : constant Canvas.Color_Type := 16#FFFFFFFF#;
   FocussedBorderEdgeLineColor : constant Canvas.Color_Type := 16#FF7F7F7F#;
   FocussedClientColor         : constant Canvas.Color_Type := 16#EF7F7F7F#;
   FocussedTitleBarColor       : constant Canvas.Color_Type := 16#EF007F00#;

   type ButtonX_Array is array(GUI.Window.WindowButtons_Enum) of Integer;

   type Window_Type is new GUI.Window.Window_Type with
      record
         TopLeftCorner     : GUI.Canvas_ClassAccess;
         TopBar            : GUI.Canvas_ClassAccess;
         TopRightCorner    : GUI.Canvas_ClassAccess;
         LeftBar           : GUI.Canvas_ClassAccess;
         RightBar          : GUI.Canvas_ClassAccess;
         BottomLeftCorner  : GUI.Canvas_ClassAccess;
         BottomRightCorner : GUI.Canvas_ClassAccess;
         BottomBar         : GUI.Canvas_ClassAccess;
         ClientArea        : GUI.Canvas_ClassAccess;
         TitleCanvas       : GUI.Canvas_ClassAccess;
         Font              : Fonts.Font_ClassAccess;
         ButtonCanvas      : GUI.Canvas_ClassAccess;
         ButtonPressed     : Boolean:=False;
         PressedButton     : GUI.Window.WindowButtons_Enum;
         ButtonX           : ButtonX_Array;
      end record;
   type Window_Access is access all Window_Type;

   overriding
   function MouseDown
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure MouseUp
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer);

   overriding
   procedure MouseMove
     (Item : access Window_Type;
      X    : Integer;
      Y    : Integer);

   overriding
   procedure Free
     (Item : access Window_Type);

   overriding
   procedure Focus
     (Item : access Window_Type);

   overriding
   procedure Defocus
     (Item : access Window_Type);

   overriding
   procedure SetCaption
     (Window  : access Window_Type;
      Caption : Unbounded_String);

   overriding
   procedure SetButtons
     (Window  : access Window_Type;
      Buttons : GUI.Window.WindowButtons_Set);
   ---------------------------------------------------------------------------

   procedure DrawButtons
     (Window : access Window_Type) is

      use type GUI.Window.WindowButtons_Enum;

      Count   : Natural:=0;
      Buttons : constant GUI.Window.WindowButtons_Set:=Window.GetButtons;
      Width   : Integer;
      X       : Integer:=0;

   begin

      if Window.ButtonCanvas/=null then
         FreeCanvas(Window.ButtonCanvas);
      end if;

      for i in Buttons'Range loop
         if Buttons(i) then
            Count:=Count+1;
         end if;
      end loop;

      if Count/=0 then
         Width:=Count*ButtonWidth+(Count-1)*ButtonSpace;
      else
         return;
      end if;

      Window.ButtonCanvas:=Window.NewCanvas
        (Height => ButtonHeight,
         Width  => Width);

      Window.ButtonCanvas.Clear(0);
      if Buttons(GUI.Window.WindowButtonClose) then
         Window.ButtonX(GUI.Window.WindowButtonClose):=X;
         if Window.ButtonPressed and Window.PressedButton=GUI.Window.WindowButtonClose then
            Window.ButtonCanvas.Rectangle
              (X => X,
               Y => 0,
               Height => ButtonHeight,
               Width => ButtonWidth,
               Color => PButtonFrameColor);
         else
            Window.ButtonCanvas.Rectangle
              (X => X,
               Y => 0,
               Height => ButtonHeight,
               Width => ButtonWidth,
               Color => ButtonFrameColor);
         end if;
      end if;

      Window.ButtonCanvas.SetBounds
        (Top     => 9,
         Left    => Window.GetBounds.Width-Width-9,
         Width   => Width,
         Height  => ButtonHeight,
         Visible => True);
      Window.ButtonCanvas.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => False);
      Window.ButtonCanvas.BringToFront;

   end DrawButtons;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Window_Type) is
   begin

      Fonts.Release(Item.Font);
      GUI.Window.Free
        (Item => GUI.Window.Window_Access(Item));

   end Free;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

   begin

      If Button=LeftButton then

         if Item.ButtonCanvas/=null then
            if TestInsideBounds(Item.ButtonCanvas.GetBounds,X,Y) then
               declare
                  use type GUI.Window.OnCloseWindow_Access;
                  Buttons : GUI.Window.WindowButtons_Set:=Item.GetButtons;
                  RX      : Integer:=X-Item.ButtonCanvas.GetBounds.Left;
               begin
                  if Buttons(GUI.Window.WindowButtonClose) and
                    RX>=Item.ButtonX(GUI.Window.WindowButtonClose) and
                    RX<Item.ButtonX(GUI.Window.WindowButtonClose)+ButtonWidth then
                     Item.ButtonPressed := True;
                     Item.PressedButton := GUI.Window.WindowButtonClose;

                  end if;
               end;
               if Item.ButtonPressed then
                  DrawButtons(Item);
                  return True;
               end if;
            end if;
         end if;

         if Y<BorderWidth then

            if X<CornerSize then
               Item.StartChange
                 (RefX => X,
                  RefY => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopLeft);
               return True;
            end if;

            if X>=Item.GetBounds.Width-CornerSize then
               Item.StartChange
                 (RefX => X,
                  RefY => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopRight);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeTop);
            return True;

         end if;

         if Y>=Item.GetBounds.Height-BorderWidth then

            if X<CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomLeft);
               return True;
            end if;

            if X>=Item.GetBounds.Width-CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomRight);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeBottom);
            return True;

         end if;

         if X<BorderWidth then

            if Y<CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopLeft);
               return True;
            end if;

            if Y>=Item.GetBounds.Height-CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomLeft);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeLeft);
            return True;

         end if;

         if X>=Item.GetBounds.Width-BorderWidth then

            if Y<CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopRight);
               return True;
            end if;

            if Y>=Item.GetBounds.Height-CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomRight);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeRight);
            return True;

         end if;

         if Y<BorderWidth+TitleBarHeight then
            Item.StartChange
              (Refx => X,
               Refy => Y,
               Mode => GUI.Window.WindowChangeModeMove);
            return True;
         end if;

      end if;

      return True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is
      pragma Unreferenced(Button);
   begin
      Item.ApplyChange
        (Refx => X,
         Refy => Y);
      Item.StopChange;
      if Item.ButtonCanvas/=null then
         if Item.ButtonPressed and
           TestInsideBounds(Item.ButtonCanvas.GetBounds,X,Y) then
            declare
               use type GUI.Window.WindowButtons_Enum;
               use type GUI.Window.OnCloseWindow_Access;
               RX : Integer:=X-Item.ButtonCanvas.GetBounds.Left;
            begin
               if Item.GetButtons(Item.PressedButton) and
                 RX>=Item.ButtonX(Item.PressedButton) and
                 RX<Item.ButtonX(Item.PressedButton)+ButtonWidth then

                  if Item.PressedButton=GUI.Window.WindowButtonClose and
                    Item.OnCloseWindow/=null then
                     Item.OnCloseWindow(Item.CallBackObject);
                  end if;

               end if;
            end;
         end if;
      end if;

      Item.ButtonPressed := False;
      DrawButtons(Item);
   end MouseUp;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (Item : access Window_Type;
      X    : Integer;
      Y    : Integer) is
   begin
      Item.ApplyChange
        (Refx => X,
         Refy => Y);
   end MouseMove;
   ---------------------------------------------------------------------------
   procedure DrawTitleCanvas
     (Window : access Window_Type) is

      use type Fonts.Font_ClassAccess;

   begin
      if Window.TitleCanvas/=null then
         FreeCanvas(Window.TitleCanvas);
      end if;

      if Window.Font/=null then

         declare
            Caption   : constant Unbounded_String := Window.GetCaption;
            TextWidth : constant Integer := Window.Font.TextWidth(Caption);
         begin

            Window.TitleCanvas:=Window.NewCanvas
              (Height => TitleBarHeight,
               Width  => TextWidth);
            Window.TitleCanvas.Clear
              (Color => 16#00000000#);

            Window.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(Window.TitleCanvas),
               X => 0.0,
               Y => 0.0,
               Text => Caption,
               Color => 16#FFFFFFFF#);

            Window.TitleCanvas.SetBounds
              (Top     => BorderWidth+1,
               Left    => BorderWidth+2,
               Height  => TitleBarHeight,
               Width   => TextWidth,
               Visible => True);

            Window.TitleCanvas.SetAnchors
              (Top    => True,
               Left   => True,
               Right  => False,
               Bottom => False);
            Window.TitleCanvas.BringToFront;

         end;
      end if;

   end;
   ---------------------------------------------------------------------------

   procedure DrawCanvasse
     (Window : access Window_Type) is

      TitleBarColor       : Canvas.Color_Type;
      BorderLineColor     : Canvas.Color_Type;
      BorderEdgeLineColor : Canvas.Color_Type;
      BackgroundColor     : Canvas.Color_Type;
      ClientColor         : Canvas.Color_Type;

   begin

      if Window.Focussed then
         TitleBarColor       := FocussedTitleBarColor;
         BorderLineColor     := FocussedBorderLineColor;
         BorderEdgeLineColor := FocussedBorderEdgeLineColor;
         BackgroundColor     := FocussedBackgroundColor;
         ClientColor         := FocussedClientColor;
      else
         TitleBarColor       := NormalTitleBarColor;
         BorderLineColor     := NormalBorderLineColor;
         BorderEdgeLineColor := NormalBorderEdgeLineColor;
         BackgroundColor     := NormalBackgroundColor;
         ClientColor         := NormalClientColor;
      end if;

      DrawTitleCanvas(Window);

      if Window.TopLeftCorner/=null then
         FreeCanvas(Window.TopLeftCorner);
      end if;

      Window.TopLeftCorner:=Window.NewCanvas
        (Height => TopBarHeight,
         Width  => CornerSize);

      Window.TopLeftCorner.Clear
        (Color => BackgroundColor);
      Window.TopLeftCorner.HorzLine
        (X     => 0,
         Y     => 0,
         Width => CornerSize,
         Color => BorderLineColor);
      Window.TopLeftCorner.VertLine
        (X      => 0,
         Y      => 1,
         Height => TopBarHeight-1,
         Color  => BorderLineColor);

      Window.TopLeftCorner.HorzLine
        (X      => LineWidth+BorderSpaceWidth,
         Y      => LineWidth+BorderSpaceWidth,
         Width  => CornerSize-LineWidth-BorderSpaceWidth,
         Color  => BorderLineColor);
      Window.TopLeftCorner.VertLine
        (X      => LineWidth+BorderSpaceWidth,
         Y      => LineWidth+BorderSpaceWidth+1,
         Height => TopbarHeight-LineWidth-BorderSpaceWidth-BorderWidth,
         Color  => BorderLineColor);

      Window.TopLeftCorner.HorzLine
        (X      => LineWidth+BorderSpaceWidth+1,
         Y      => TopBarHeight-BorderWidth,
         Width  => CornerSize-LineWidth-BorderSpaceWidth-1,
         Color  => BorderLineColor);
      Window.TopLeftCorner.HorzLine
        (X      => LineWidth+BorderSpaceWidth,
         Y      => TopBarHeight-1,
         Width  => CornerSize-LineWidth-BorderSpaceWidth,
         Color  => BorderLineColor);
      Window.TopLeftCorner.HorzLine
        (X     => 1,
         Y     => CornerSize-1,
         Width => BorderSpaceWidth,
         Color => BorderEdgeLineColor);
      Window.TopLeftCorner.VertLine
        (X      => CornerSize-1,
         Y      => 1,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.TopLeftCorner.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => TopBarHeight,
         Width   => CornerSize,
         Visible => True);
      Window.TopLeftCorner.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => False);
      ------------------------------------------------------------------------

      if Window.TopBar/=null then
         FreeCanvas(Window.TopBar);
      end if;

      Window.TopBar:=Window.NewCanvas
        (Height => TopBarHeight,
         Width  => 1);

      Window.TopBar.Clear
        (Color => BackgroundColor);
      Window.TopBar.VertLine
        (Y      => 0,
         X      => BorderWidth,
         Height => TitleBarHeight,
         Color  => TitleBarColor);
      Window.TopBar.Image(0,0)
        :=BorderLineColor;
      Window.TopBar.Image(LineWidth+BorderSpaceWidth,0)
        :=BorderLineColor;
      Window.TopBar.Image(TopBarHeight-BorderSpaceWidth-LineWidth-1,0)
        :=BorderLineColor;
      Window.TopBar.Image(TopBarHeight-1,0)
        :=BorderLineColor;
      Window.TopBar.SetBounds
        (Top     => 0,
         Left    => CornerSize,
         Height  => TopBarHeight,
         Width   => Window.GetBounds.Width-2*CornerSize,
         Visible => True);
      Window.Topbar.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => False);
      ------------------------------------------------------------------------

      if Window.TopRightCorner/=null then
         FreeCanvas(Window.TopRightCorner);
      end if;

      Window.TopRightCorner:=Window.NewCanvas
        (Height => TopBarHeight,
         Width  => CornerSize);

      Window.TopRightCorner.Clear
        (Color => BackgroundColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => 0,
         Width  => CornerSize,
         Color  => BorderLineColor);
      Window.TopRightCorner.VertLine
        (X      => CornerSize-1,
         Y      => 1,
         Height => TopbarHeight-1,
         Color  => BorderLineColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => LineWidth+BorderSpaceWidth,
         Width  => CornerSize-BorderWidth,
         Color  => BorderLineColor);
      Window.TopRightCorner.VertLine
        (X      => CornerSize-BorderWidth,
         Y      => LineWidth+BorderSpaceWidth,
         Height => TopbarHeight-2*BorderWidth+1,
         Color  => BorderLineColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => TopBarHeight-1,
         Width  => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => TopBarHeight-BorderWidth,
         Width  => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.TopRightCorner.VertLine
        (X      => 0,
         Y      => LineWidth,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.TopRightCorner.HorzLine
        (X      => CornerSize-LineWidth-BorderSpaceWidth,
         Y      => CornerSize-1,
         Width  => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.TopRightCorner.SetBounds
        (Top     => 0,
         Left    => Window.GetBounds.Width-CornerSize,
         Height  => TopBarHeight,
         Width   => CornerSize,
         Visible => True);
      Window.TopRightCorner.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => False);
      ------------------------------------------------------------------------

      if Window.LeftBar/=null then
         FreeCanvas(Window.LeftBar);
      end if;

      Window.LeftBar:=Window.NewCanvas
        (Height => 1,
         Width  => BorderWidth);
      Window.LeftBar.Clear
        (Color => BackgroundColor);
      Window.LeftBar.Image(0,0):=BorderLineColor;
      Window.LeftBar.Image(0,BorderWidth-1):=BorderLineColor;
      Window.LeftBar.SetBounds
        (Top     => TopBarHeight,
         Left    => 0,
         Height  => Window.GetBounds.Height-TopBarHeight-CornerSize,
         Width   => BorderWidth,
         Visible => True);
      Window.LeftBar.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.RightBar/=null then
         FreeCanvas(Window.RightBar);
      end if;

      Window.RightBar:=Window.NewCanvas
        (Height => 1,
         Width  => BorderWidth);
      Window.RightBar.Clear
        (Color => BackgroundColor);
      Window.RightBar.Image(0,0):=BorderLineColor;
      Window.RightBar.Image(0,BorderWidth-1):=BorderLineColor;
      Window.RightBar.SetBounds
        (Top     => TopBarHeight,
         Left    => Window.GetBounds.Width-BorderWidth,
         Height  => Window.GetBounds.Height-TopBarHeight-CornerSize,
         Width   => BorderWidth,
         Visible => True);
      GUI.SetAnchors
        (Canvas => Window.RightBar,
         Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.BottomLeftCorner/=null then
         FreeCanvas(Window.BottomLeftCorner);
      end if;

      Window.BottomLeftCorner:=Window.NewCanvas
        (Height => CornerSize,
         Width  => CornerSize);
      Window.BottomLeftCorner.Clear
        (Color => BackgroundColor);
      Window.BottomLeftCorner.VertLine
        (X      => 0,
         Y      => 0,
         Height => CornerSize,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.HorzLine
        (X      => 1,
         Y      => CornerSize-1,
         Width  => CornerSize-1,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.VertLine
        (X      => BorderWidth-1,
         Y      => 0,
         Height => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.HorzLine
        (X      => BorderWidth,
         Y      => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.HorzLine
        (X      => LineWidth,
         Y      => 0,
         Width  => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomLeftCorner.VertLine
        (X      => CornerSize-1,
         Y      => CornerSize-LineWidth-BorderSpaceWidth,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomLeftCorner.Bar
        (X      => BorderWidth,
         Y      => 0,
         Height => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => 0);
      Window.BottomLeftCorner.SetBounds
        (Top     => Window.GetBounds.Height-CornerSize,
         Left    => 0,
         Height  => CornerSize,
         Width   => CornerSize,
         Visible => True);
      Window.BottomLeftCorner.SetAnchors
        (Top    => False,
         Left   => True,
         Right  => False,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.BottomBar/=null then
         FreeCanvas(Window.BottomBar);
      end if;

      Window.BottomBar:=Window.NewCanvas
        (Height => BorderWidth,
         Width  => 1);
      Window.BottomBar.Clear
        (Color => BackgroundColor);

      Window.BottomBar.Image(0,0)             := BorderLineColor;
      Window.BottomBar.Image(BorderWidth-1,0) := BorderLineColor;

      Window.BottomBar.SetBounds
        (Top     => Window.GetBounds.Height-BorderWidth,
         Left    => CornerSize,
         Height  => BorderWidth,
         Width   => Window.GetBounds.Width-2*CornerSize,
         Visible => True);
      GUI.SetAnchors
        (Canvas => Window.BottomBar,
         Top    => False,
         Left   => True,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.BottomRightCorner/=null then
         FreeCanvas(Window.BottomRightCorner);
      end if;

      Window.BottomRightCorner:=Window.NewCanvas
        (Height => CornerSize,
         Width  => CornerSize);
      Window.BottomRightCorner.Clear
        (Color => BackgroundColor);
      Window.BottomRightCorner.VertLine
        (X      => CornerSize-1,
         Y      => 0,
         Height => CornerSize,
         Color  => BorderLineColor);
      Window.BottomRightCorner.HorzLine
        (X      => 0,
         Y      => CornerSize-1,
         Width  => CornerSize-1,
         Color  => BorderLineColor);
      Window.BottomRightCorner.VertLine
        (X      => CornerSize-BorderWidth,
         Y      => 0,
         Height => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.BottomRightCorner.HorzLine
        (X      => 0,
         Y      => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => BorderLineColor);
      Window.BottomRightCorner.HorzLine
        (X      => CornerSize-LineWidth-BorderSpaceWidth,
         Y      => 0,
         Width  => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomRightCorner.VertLine
        (X      => 0,
         Y      => CornerSize-LineWidth-BorderSpaceWidth,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomRightCorner.Bar
        (X      => 0,
         Y      => 0,
         Height => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => 0);
      Window.BottomRightCorner.SetBounds
        (Top     => Window.GetBounds.Height-CornerSize,
         Left    => Window.GetBounds.Width-CornerSize,
         Height  => CornerSize,
         Width   => CornerSize,
         Visible => True);
      Window.BottomRightCorner.SetAnchors
        (Top    => False,
         Left   => False,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.ClientArea/=null then
         FreeCanvas(Window.ClientArea);
      end if;

      Window.ClientArea:=Window.NewCanvas
        (Height => 1,
         Width  => 1);
      Window.ClientArea.Clear
        (Color => ClientColor);
      Window.ClientArea.SetBounds
        (Top     => TopBarHeight,
         Left    => BorderWidth,
         Height  => Window.GetBounds.Height-TopBarHeight-BorderWidth,
         Width   => Window.GetBounds.Width-2*BorderWidth,
         Visible => True);
      Window.ClientArea.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

   end DrawCanvasse;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Window  : access Window_Type;
      Caption : Unbounded_String) is
   begin
      GUI.Window.Window_Access(Window).SetCaption(Caption);
      DrawTitleCanvas(Window);
   end SetCaption;
   ---------------------------------------------------------------------------

   procedure SetButtons
     (Window  : access Window_Type;
      Buttons : GUI.Window.WindowButtons_Set) is
   begin
      GUI.Window.Window_Access(Window).SetButtons(Buttons);
      DrawButtons(Window);
   end SetButtons;
   ---------------------------------------------------------------------------

   procedure Focus
     (Item : access Window_Type) is
   begin
      DrawCanvasse(Item);
      GUI.Window.Window_Access(Item).Focus;
   end Focus;
   ---------------------------------------------------------------------------

   procedure Defocus
     (Item : access Window_Type) is
   begin
      DrawCanvasse(Item);
   end Defocus;
   ---------------------------------------------------------------------------

   function NewWindow
     (Parent : Object_ClassAccess)
      return GUI.Window.Window_ClassAccess is

      NewWindow : Window_Access;

   begin
      NewWindow:=new Window_Type;

      GUI.Window.Initialize
        (Item   => GUI.Window.Window_Access(NewWindow),
         Parent => Parent);

      NewWindow.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);
      ------------------------------------------------------------------------
      DrawCanvasse(NewWindow);

      NewWindow.TopHeightConstraint:=
        (MinValueConstraint => ConstraintConstant,
         MinValueConstant   => 0,
         MaxValueConstraint => ConstraintUsingParentSize,
         MaxValueConstant   => 10,
         MinSizeConstraint  => ConstraintConstant,
         MinSizeConstant    => 100,
         MaxSizeConstraint  => ConstraintUsingParentSize,
         MaxSizeConstant    => 0);
      NewWindow.LeftWidthConstraint:=
        (MinValueConstraint => ConstraintUsingSize,
         MinValueConstant   => 10,
         MaxValueConstraint => ConstraintUsingParentSize,
         MaxValueConstant   => 10,
         MinSizeConstraint  => ConstraintConstant,
         MinSizeConstant    => 100,
         MaxSizeConstraint  => ConstraintUsingParentSize,
         MaxSizeConstant    => 0);
      ------------------------------------------------------------------------
      declare
         Client : GUI.Object_Access;
      begin
         Client:=new GUI.Object_Type;
         GUI.Initialize
           (Item   => Client,
            Parent => Object_ClassAccess(NewWindow));
         NewWindow.SetClient(Object_ClassAccess(Client));

         Client.SetBounds
           (Top     => TopBarHeight,
            Left    => BorderWidth,
            Height  => -TopBarHeight-BorderWidth,
            Width   => -2*BorderWidth,
            Visible => True);

         Client.SetAnchors
           (Top    => True,
            Left   => True,
            Right  => True,
           Bottom => True);
      end;

      return GUI.Window.Window_ClassAccess(NewWindow);

   end NewWindow;
   ---------------------------------------------------------------------------

end YellowBlue.Window;
