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
--   11.Mar 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   This unit simplifies working with nested rectangles including visibility
--   if rectangles are clipped by their parents boundaries.

-- Usage
--   The two main types in this package are
--    * Bounds_Type
--    * AbsBounds_Type
--
--   Bounds_Type represents a rectangular region including visibility.
--   AbsBounds_Type is a rectangle represented in relative coordinates,
--   relative to a root rectangle. The fields AbsTop..AbsWidth are the
--   visible portion of the rectangle in root coordinates if the rectangle
--   is visible at all. AbsSubTop and AbsSubLeft give the number of pixels
--   the content in the rectangle is hidden behind the top left corner
--   of any of the parents rectangles.
--
--   The Anchors_Type can be used to keep bounds of an rectangular region
--   constant relative to the parents bounds, thereby ensuring constant border
--   width, if the border booleans (Top, Left, Right, Bottom) are set.
--   The border widths are extracted using StoreAnchors with a current
--   configuration of both client and parent bounds.
--   They are restored with RestoreAnchors.
--
--   The Constraint_Type is used on a single dimension of bounds limiting
--   size and position(value) either in horizontal or vertical direction.
--   The possible situations for constraints are listed below giving
--   the configuration and its subsequent effect in a simple formula.
--   The constant is always the constant belonging to the constraint part,
--   eg. MaxSizeConstraint : Constant=MaxSizeConstant.
--
--   * MaxSizeConstraint
--     * ConstraintUsingParentSize
--        Size <= ParentSize - Constant
--
--   * MinValueConstraint
--     * ConstraintConstant
--       Value >= Constant
--         if triggered, Size is set to
--         OldSize - (Value - OldValue)
--
--     * ConstraintUsingSize
--       Value >= Constant - Size
--
--     * ConstraintUsingParentSize
--       Value >= Constant - ParentSize
--
--   * MaxValueConstraint
--     * ConstraintConstant
--       Value <= Constant
--
--     * ConstraintUsingSize
--       Value <= Size - Constant
--
--     * ConstraintUsingParentSize
--       Value <= ParentSize - Constant
--
--  * MinSizeConstraint
--    * ConstraintConstant
--      Size >= Constant
--        if trigged, Value is set to
--        OldValue + OldSize - Constant
--
--  It should be noted that the constraints are applied in the order described
--  here. The occasionaly used OldValue and OldSize should give the Size and
--  Value before any change to the rectangle happened. It is used heavily by
--  the GUI to implement a sane behaviour with nested objects moving and
--  at the same time keeping constraints.

package BoundsCalc is

   type Constraint_Enum is
     (ConstraintNone,
      ConstraintConstant,
      ConstraintUsingSize,
      ConstraintUsingParentSize);

   type Bounds_Type is
      record
         Top     : Integer:=0;
         Left    : Integer:=0;
         Height  : Integer:=0;
         Width   : Integer:=0;
         Visible : Boolean:=False;
      end record;

   function TestInsideBounds
     (Bounds : Bounds_Type;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   type AbsBounds_Type is
      record
         AbsTop     : Integer:=0;
         AbsLeft    : Integer:=0;
         AbsHeight  : Integer:=0;
         AbsWidth   : Integer:=0;
         AbsSubTop  : Integer:=0;
         AbsSubLeft : Integer:=0;
         AbsVisible : Boolean:=False;
      end record;

   type Anchors_Type is
      record
         Top          : Boolean:=True;
         Left         : Boolean:=True;
         Right        : Boolean:=False;
         Bottom       : Boolean:=False;
         TopBorder    : Integer:=0;
         LeftBorder   : Integer:=0;
         RightBorder  : Integer:=0;
         BottomBorder : Integer:=0;
      end record;

   type Constraint_Type is
      record
         MinValueConstraint : Constraint_Enum:=ConstraintNone;
         MinValueConstant   : Integer;
         MaxValueConstraint : Constraint_Enum:=ConstraintNone;
         MaxValueConstant   : Integer;
         MinSizeConstraint  : Constraint_Enum:=ConstraintNone;
         MinSizeConstant    : Integer;
         MaxSizeConstraint  : Constraint_Enum:=ConstraintNone;
         MaxSizeConstant    : Integer;
      end record;

   procedure NestBounds
     (ParentAbsBounds : AbsBounds_Type;
      RectBounds      : Bounds_Type;
      ResultBounds    : out AbsBounds_Type);

   procedure ApplyConstraint
     (Constraint : Constraint_Type;
      Value      : in out Integer;
      Size       : in out Integer;
      OldValue   : Integer;
      OldSize    : Integer;
      ParentSize : Integer);

   procedure StoreAnchors
     (Anchors      : in out Anchors_Type;
      ClientBounds : Bounds_Type;
      ParentBounds : Bounds_Type);

   procedure RestoreAnchors
     (Anchors      : Anchors_Type;
      ClientBounds : in out Bounds_Type;
      ParentBounds : Bounds_Type);

   function TestInsideAbsBounds
     (AbsBounds : AbsBounds_Type;
      AbsX      : Integer;
      AbsY      : Integer)
      return Boolean;

   procedure Put
     (Item : Bounds_Type);

   procedure Put
     (Item : AbsBounds_Type);

   procedure Put
     (Item : Anchors_Type);

end BoundsCalc;
