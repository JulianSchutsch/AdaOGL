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
--   26.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces;

package Canvas is

   type Color_Type is new Interfaces.Unsigned_32;

   type Image_Type is array(Natural range <>,Natural range <>) of Color_Type;
   pragma Convention(C,Image_Type);

   type Image_Access is access all Image_Type;

   type Canvas_Type is tagged limited
      record
         Image         : Image_Access := null;
         Modified      : Boolean      := True;
         ContentHeight : Integer;
         ContentWidth  : Integer;
      end record;
   type Canvas_Access is access all Canvas_Type;
   type Canvas_ClassAccess is access all Canvas_Type'Class;

   procedure Initialize
     (Canvas : access Canvas_Type;
      Height : Integer;
      Width  : Integer);

   procedure Finalize
     (Canvas : access Canvas_Type);

   procedure GetPixel
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : out Color_Type);

   procedure SetPixel
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : Color_Type);

   procedure Clear
     (Canvas : in out Canvas_Type;
      Color  : Color_Type);

   procedure VertLine
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Color  : Color_Type);

   procedure HorzLine
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Color  : Color_Type);

   procedure WuLine
     (Canvas : in out Canvas_Type;
      X1     : Float;
      Y1     : Float;
      X2     : Float;
      Y2     : Float;
      Color  : Color_Type);

   procedure Rectangle
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Width  : Integer;
      Color  : Color_Type);

   procedure Bar
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Width  : Integer;
      Color  : Color_Type);

   procedure Circle
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Radius : Integer;
      Color  : Color_Type);

   procedure FilledCircle
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Radius : Integer;
      Color  : Color_Type);

   function MultiplyAlpha
     (Color : Color_Type;
      Alpha : Float)      -- Value must be in 0..1
      return Color_Type;
   pragma Inline(MultiplyAlpha);

   function MultiplyAlpha
     (Color : Color_Type;
      Alpha : Integer)    -- Value must be in 0..255
      return Color_Type;
   pragma Inline(MultiplyAlpha);

   function PreBlendMix
     (BackgroundColor : Color_Type;
      ForegroundColor : Color_Type)
      return Color_Type;

end Canvas;
