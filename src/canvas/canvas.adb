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

with Basics; use Basics;
with Ada.Unchecked_Deallocation;

package body Canvas is

   procedure Initialize
     (Canvas : access Canvas_Type;
      Height : Integer;
      Width  : Integer) is
   begin

      if (Height>0) and (Width>0) then
         Canvas.Image:=new Image_Type
           (0..Height-1,
            0..Width-1);
      end if;

      Canvas.ContentHeight := Height;
      Canvas.ContentWidth  := Width;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Canvas : access Canvas_Type) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Image_Type,
         Name   => Image_Access);

   begin

      if Canvas.Image/=null then
         Free(Canvas.Image);
         Canvas.ContentHeight := 0;
         Canvas.ContentWidth  := 0;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Circle
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Radius : Integer;
      Color  : Color_Type) is

      use FloatNumeric;

      Value : Integer;

   begin
      for i in 0..Radius loop
         Value:=Integer(Float'Rounding(Sqrt(Float(Radius*Radius)-Float(i*i))));
         Canvas.SetPixel(X+i,Y+Value,Color);
         Canvas.SetPixel(X-i,Y+Value,Color);
         Canvas.SetPixel(X+i,Y-Value,Color);
         Canvas.SetPixel(X-i,Y-Value,Color);
         Canvas.SetPixel(X+Value,X+i,Color);
         Canvas.SetPixel(X-Value,X+i,Color);
         Canvas.SetPixel(X+Value,X-i,Color);
         Canvas.SetPixel(X-Value,X-i,Color);
      end loop;
   end Circle;
   ---------------------------------------------------------------------------

   procedure FilledCircle
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Radius : Integer;
      Color  : Color_Type) is

      use FloatNumeric;

      Value : Integer;

   begin
      for i in 0..Radius loop
         Value:=Integer(Float'Rounding(Sqrt(Float(Radius*Radius)-Float(i*i))));
         Canvas.VertLine
           (X      => X-i,
            Y      => Y-Value,
            Height => 2*Value+1,
            Color  => Color);
         Canvas.VertLine
           (X      => X+i,
            Y      => Y-Value,
            Height => 2*Value+1,
            Color  => Color);
         Canvas.HorzLine
           (X      => X-Value,
            Y      => Y-i,
            Width  => 2*Value+1,
            Color  => Color);
         Canvas.HorzLine
           (X      => X-Value,
            Y      => Y+i,
            Width  => 2*Value+1,
            Color  => Color);
      end loop;
   end FilledCircle;
   ---------------------------------------------------------------------------

   procedure Rectangle
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Width  : Integer;
      Color  : Color_Type) is
   begin

      Canvas.HorzLine
        (X     => X,
         Y     => Y,
         Width => Width,
         Color => Color);

      Canvas.HorzLine
        (X     => X,
         Y     => Y+Height-1,
         Width => Width,
         Color => Color);

      Canvas.VertLine
        (X      => X,
         Y      => Y+1,
         Height => Height-2,
         Color  => Color);

      Canvas.VertLine
        (X      => X+Width-1,
         Y      => Y+1,
         Height => Height-2,
         Color  => Color);

   end Rectangle;

   procedure GetPixel
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : out Color_Type) is
   begin
      if Canvas.Image/=null and then
        ((X in Canvas.Image'Range(2))
        and (Y in Canvas.Image'Range(1))) then
         Color:=Canvas.Image(Y,X);
      else
         Color:=0;
      end if;
   end GetPixel;
   ---------------------------------------------------------------------------

   procedure SetPixel
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : Color_Type) is
   begin

      if Canvas.Image/=null and then
        ((X in Canvas.Image'Range(2))
        and (Y in Canvas.Image'Range(1))) then
         Canvas.Image(Y,X):=Color;
      end if;

   end SetPixel;
   ---------------------------------------------------------------------------

   -- Implementation of Xiaolin Wu's line algorithm
   -- Source : http://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm
   procedure WuLine
     (Canvas : in out Canvas_Type;
      X1     : Float;
      Y1     : Float;
      X2     : Float;
      Y2     : Float;
      Color  : Color_Type) is

      XPos1    : Float:=X1;
      XPos2    : Float:=X2;
      YPos1    : Float:=Y1;
      YPos2    : Float:=Y2;
      DeltaX   : Float:=X2-X1;
      DeltaY   : Float:=Y2-Y1;
      Gradient : Float;
      XEnd     : Float;
      YEnd     : Float;
      XGap     : Float;
      XPXL1    : Integer;
      YPXL1    : Integer;
      Intery   : Float;
      XPXL2    : Integer;
      YPXL2    : Integer;
      Order    : Boolean:=False;

      procedure SPixel
        (X     : Integer;
         Y     : Integer;
         Alpha : Float) is

         BackgroundColor : Color_Type;

         RX : Integer;
         RY : Integer;

      begin
         if Order then
            RX:=Y;
            RY:=X;
         else
            RX:=X;
            RY:=Y;
         end if;

         Canvas.GetPixel
           (X     => RX,
            Y     => RY,
            Color => BackgroundColor);

         Canvas.SetPixel
           (X => RX,
            Y => RY,
            Color => PreBlendMix
              (BackgroundColor => BackgroundColor,
               ForegroundColor => MultiplyAlpha
                 (Color => Color,
                  Alpha => Alpha)));

      end SPixel;

      function frac
        (x : Float)
         return Float is
      begin
         return x-Float'Floor(x);
      end frac;

   begin

      if abs(DeltaX)<abs(DeltaY) then
         Swap(XPos1,YPos1);
         Swap(XPos2,YPos2);
         Swap(DeltaX,DeltaY);
         Order:=True;
      end if;
      if XPos2<XPos1 then
         Swap(XPos1,XPos2);
         Swap(YPos1,YPos2);
      end if;

      Gradient:=DeltaY/DeltaX;

      XEnd:=Float'Rounding(XPos1);
      YEnd:=YPos1+Gradient*(XEnd-XPos1);

      -- First Endpoint
      XGap:=(1.0-frac(XPos1+0.5));

      XPXL1:=Integer(XEnd);
      YPXL1:=Integer(Float'Floor(YEnd));
      SPixel
        (X     => XPXL1,
         Y     => YPXL1,
         Alpha => (1.0-frac(YEnd))*XGap);

      SPixel
        (X     => XPXL1,
         Y     => YPXL1+1,
         Alpha => frac(YEnd)*XGap);
      Intery:=YEnd+Gradient;

      -- Second Endpoint
      XEnd:=Float'Rounding(XPos2);
      YEnd:=YPos2+Gradient*(XEnd-XPos2);

      XGap:=frac(XPos2+0.5);

      XPXL2 := Integer(XEnd);
      YPXL2 := Integer(Float'Floor(YEnd));

      SPixel
        (X     => XPXL2,
         Y     => YPXL2,
         Alpha => (1.0-frac(YEnd))*XGap);
      SPixel
        (X     => XPXL2,
         Y     => YPXL2+1,
         Alpha => frac(YEnd)*XGap);

      -- Main Loop
      for i in XPXL1+1..XPXL2-1 loop

         SPixel
           (X     => i,
            Y     => Integer(Intery),
            Alpha => 1.0-frac(Intery));

         SPixel
           (X     => i,
            Y     => Integer(Intery),
            Alpha => frac(Intery));

         intery:=intery+gradient;

      end loop;

   end WuLine;
   ---------------------------------------------------------------------------

   procedure Bar
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Width  : Integer;
      Color  : Color_Type) is

      DrawHeight : Integer;
      DrawWidth  : Integer;
      DrawX      : Integer;
      DrawY       : Integer;

   begin
      DrawHeight := Height;
      DrawY      := Y;

      if DrawY<0 then
         DrawHeight:=DrawHeight+DrawY;
         DrawY:=0;
      end if;

      if DrawY+DrawHeight>Canvas.ContentHeight then
         DrawHeight:=Canvas.ContentHeight-DrawY;
      end if;

      if DrawHeight<=0 then
         return;
      end if;
      ------------------------------------------------------------------------

      DrawWidth := Width;
      DrawX     := X;

      if DrawX<0 then
         DrawWidth:=DrawWidth+DrawX;
         DrawX:=0;
      end if;

      if DrawX+DrawWidth>Canvas.ContentWidth then
         DrawWidth:=Canvas.ContentWidth-DrawX;
      end if;

      if DrawWidth<=0 then
         return;
      end if;
      ------------------------------------------------------------------------

      for i in DrawY..DrawY+DrawHeight-1 loop
         for n in DrawX..DrawX+DrawWidth-1 loop
            Canvas.Image(i,n):=Color;
         end loop;
      end loop;
   end Bar;
   ---------------------------------------------------------------------------

   procedure HorzLine
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Color  : Color_Type) is

      DrawWidth : Integer;
      DrawX     : Integer;

   begin
      if (Y<0)
        or (Y>=Canvas.ContentHeight) then
         return;
      end if;

      DrawWidth := Width;
      DrawX     := X;

      if DrawX<0 then
         DrawWidth:=DrawWidth+DrawX;
         DrawX:=0;
      end if;

      if DrawX+DrawWidth>Canvas.ContentWidth then
         DrawWidth:=Canvas.ContentWidth-DrawX;
      end if;

      if DrawWidth<=0 then
         return;
      end if;

      for i in DrawX..DrawX+DrawWidth-1 loop
         Canvas.Image(Y,i):=Color;
      end loop;

   end HorzLine;
   ---------------------------------------------------------------------------

   procedure VertLine
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Color  : Color_Type) is

      DrawHeight : Integer;
      DrawY      : Integer;

   begin
      if (Canvas.Image=null)
        or (X<0)
        or (X>=Canvas.ContentWidth) then
         return;
      end if;

      DrawHeight := Height;
      DrawY      := Y;

      if DrawY<0 then
         DrawHeight:=DrawHeight+DrawY;
         DrawY:=0;
      end if;

      if DrawY+DrawHeight>Canvas.ContentHeight then
         DrawHeight:=Canvas.ContentHeight-DrawY;
      end if;

      if DrawHeight<=0 then
         return;
      end if;

      for i in DrawY..DrawY+DrawHeight-1 loop
         Canvas.Image(i,X):=Color;
      end loop;

   end VertLine;

   procedure Clear
     (Canvas : in out Canvas_Type;
      Color  : Color_Type) is
   begin
      if Canvas.Image=null then
         return;
      end if;
      for y in Canvas.Image'Range(1) loop
         for x in Canvas.Image'Range(2) loop
            Canvas.Image(y,x):=Color;
         end loop;
      end loop;
      Canvas.Modified := True;
   end;
   ---------------------------------------------------------------------------

   function MultiplyAlpha
     (Color : Color_Type;
      Alpha : Float)
      return Color_Type is

   begin
      return Color and 16#FFFFFF#+
        Shift_Left(Color_Type(Float(Shift_Right(Color,24))*Alpha),24);
   end MultiplyAlpha;
   ---------------------------------------------------------------------------

   function MultiplyAlpha
     (Color : Color_Type;
      Alpha : Integer)
      return Color_Type is

   begin
      return Color and 16#FFFFFF#+
        Shift_Left(Shift_Right(Color,24)*Color_Type(Alpha)/255,24);

   end MultiplyAlpha;
   ---------------------------------------------------------------------------

   function PreBlendMix
     (BackgroundColor : Color_Type;
      ForegroundColor : Color_Type)
      return Color_Type is

      use Interfaces;

      Alpha1    : constant Color_Type:=Shift_Right(BackgroundColor,24);
      Alpha2    : constant Color_Type:=Shift_Right(ForegroundColor,24);
      Red1      : constant Color_Type:=Shift_Right(BackgroundColor,16) and 16#FF#;
      Red2      : constant Color_Type:=Shift_Right(ForegroundColor,16) and 16#FF#;
      Green1    : constant Color_Type:=Shift_Right(BackgroundColor,8) and 16#FF#;
      Green2    : constant Color_Type:=Shift_Right(ForegroundColor,8) and 16#FF#;
      Blue1     : constant Color_Type:=BackgroundColor and 16#FF#;
      Blue2     : constant Color_Type:=ForegroundColor and 16#FF#;

      NormAlpha : Color_Type;
      NewRed    : Color_Type;
      NewGreen  : Color_Type;
      NewBlue   : Color_Type;
      NewAlpha  : Color_Type;

   begin

      NormAlpha := (255*255-(255-Alpha1)*(255-Alpha2));

      if NormAlpha/=0 then
         NewRed   := (255*Alpha2*Red2+(255-Alpha2)*Alpha1*Red1)/NormAlpha;
         NewGreen := (255*Alpha2*Green2+(255-Alpha2)*Alpha1*Green1)/NormAlpha;
         NewBlue  := (255*Alpha2*Blue2+(255-Alpha2)*Alpha1*Blue1)/NormAlpha;
         NewAlpha := NormAlpha/255;

         return NewBlue
           +Shift_Left(NewGreen,8)
           +Shift_Left(NewRed,16)
           +Shift_Left(NewAlpha,24);
      else
         return 0;
      end if;

   end;

end Canvas;
