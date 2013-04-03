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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BoundsCalc is

   function TestInsideBounds
     (Bounds : Bounds_Type;
      X      : Integer;
      Y      : Integer)
      return Boolean is
   begin

      return
        Bounds.Visible
        and (Y>=Bounds.Top)
        and (Y<Bounds.Top+Bounds.Height)
        and (X>=Bounds.Left)
        and (X<Bounds.Left+Bounds.Width);

   end TestInsideBounds;
   ---------------------------------------------------------------------------

   function TestInsideAbsBounds
     (AbsBounds : AbsBounds_Type;
      AbsX      : Integer;
      AbsY      : Integer)
      return Boolean is
   begin

      return
        AbsBounds.AbsVisible
        and (AbsY>=AbsBounds.AbsTop)
        and (AbsY<AbsBounds.AbsTop+AbsBounds.AbsHeight)
        and (AbsX>=AbsBounds.AbsLeft)
        and (AbsX<AbsBounds.AbsLeft+AbsBounds.AbsWidth);

   end TestInsideAbsBounds;
   ---------------------------------------------------------------------------

   procedure Put
     (Item : Anchors_Type) is
   begin
      Put("Anchors:");
      New_Line;
      Put(" TopBorder    :");
      Put(Item.TopBorder);
      New_Line;
      Put(" LeftBorder   :");
      Put(Item.LeftBorder);
      New_Line;
      Put(" RightBorder  :");
      Put(Item.RightBorder);
      New_Line;
      Put(" BottomBorder :");
      Put(Item.BottomBorder);
      New_Line;
   end Put;
   ---------------------------------------------------------------------------

   procedure Put
     (Item : Bounds_Type) is
   begin
      Put("Bounds:");
      New_Line;
      Put(" Top     : ");
      Put(Item.Top);
      New_Line;
      Put(" Left    : ");
      Put(Item.Left);
      New_Line;
      Put(" Height  : ");
      Put(Item.Height);
      New_Line;
      Put(" Width   : ");
      Put(Item.Width);
      New_Line;
      if Item.Visible then
         Put(" Visible : True");
      else
         Put(" Visible : False");
      end if;
      New_Line;

   end Put;
   ---------------------------------------------------------------------------

   procedure Put
     (Item : AbsBounds_Type) is
   begin
      Put("AbsBounds:");
      New_Line;
      Put(" AbsTop     : ");
      Put(Item.AbsTop);
      New_Line;
      Put(" AbsLeft    : ");
      Put(Item.AbsLeft);
      New_Line;
      Put(" AbsHeight  : ");
      Put(Item.AbsHeight);
      New_Line;
      Put(" AbsWidth   : ");
      Put(Item.AbsWidth);
      New_Line;
      if Item.AbsVisible then
         Put(" AbsVisible : True");
      else
         Put(" AbsVisible : False");
      end if;
      New_Line;

   end Put;
   ---------------------------------------------------------------------------

   procedure NestBounds
     (ParentAbsBounds : AbsBounds_Type;
      RectBounds      : Bounds_Type;
      ResultBounds    : out AbsBounds_Type) is

   begin
      ResultBounds.AbsTop     := ParentAbsBounds.AbsTop;
      ResultBounds.AbsLeft    := ParentAbsBounds.AbsLeft;
      ResultBounds.AbsSubTop  := ParentAbsBounds.AbsSubTop;
      ResultBounds.AbsSubLeft := ParentAbsBounds.AbsSubleft;
      ResultBounds.AbsVisible := ParentAbsBounds.AbsVisible and
        RectBounds.Visible;

      declare
         x     : Integer;
         Width : Integer;
      begin

         x := RectBounds.Left;
         if x<=ParentAbsBounds.AbsSubLeft then
            ResultBounds.AbsSubLeft := ResultBounds.AbsSubLeft - x;
            x := 0;
         else
            x := x - ParentAbsBounds.AbsSubleft;
            ResultBounds.AbsLeft    := ResultBounds.AbsLeft + x;
            ResultBounds.AbsSubLeft := 0;
         end if;

         Width := RectBounds.Width - ResultBounds.AbsSubLeft;
         if x+Width>ParentAbsBounds.AbsWidth then
            Width := ParentAbsBounds.AbsWidth - x;
         end if;
         ResultBounds.AbsVisible := ResultBounds.AbsVisible and (Width>0);
         ResultBounds.AbsWidth   := Width;
      end;

      declare
         y      : Integer;
         Height : Integer;
      begin

         y:=RectBounds.Top;
         if y<=ParentAbsBounds.AbsSubTop then
            ResultBounds.AbsSubTop := ResultBounds.AbsSubTop - y;
            y := 0;
         else
            y := y - ParentAbsBounds.AbsSubTop;
            ResultBounds.AbsTop    := ResultBounds.AbsTop + y;
            ResultBounds.AbsSubTop := 0;
         end if;

         Height := RectBounds.Height - ResultBounds.AbsSubTop;
         if y+Height>ParentAbsBounds.AbsHeight then
            Height := ParentAbsBounds.AbsHeight - y;
         end if;
         ResultBounds.AbsVisible := ResultBounds.AbsVisible and (Height>0);
         ResultBounds.AbsHeight  := Height;
      end;
   end NestBounds;
   ---------------------------------------------------------------------------

   procedure ApplyConstraint
     (Constraint : Constraint_Type;
      Value      : in out Integer;
      Size       : in out Integer;
      OldValue   : Integer;
      OldSize    : Integer;
      ParentSize : Integer) is
   begin

      if Constraint.MaxSizeConstraint=ConstraintUsingParentSize then
         if Size > ParentSize - Constraint.MaxSizeConstant then
            Size := ParentSize - Constraint.MaxSizeConstant;
            if Value < OldValue then
               Value := OldValue + OldSize - Size;
            end if;
         end if;
      end if;

      case Constraint.MinValueConstraint is
         when ConstraintNone =>
            null;

         when ConstraintConstant =>
            if Value < Constraint.MinValueConstant then
               Value := Constraint.MinValueConstant;
               if Size>OldSize then
                  Size := Oldsize - (Value - OldValue);
               end if;
            end if;

         when ConstraintUsingSize =>
            if Value < Constraint.MinValueConstant - Size then
               Value := Constraint.MinValueConstant - Size;
            end if;

         when ConstraintusingParentSize =>
            if Value < Constraint.MinValueConstant - ParentSize then
               Value := Constraint.MinValueConstant - ParentSize;
            end if;

      end case;

      case Constraint.MaxValueConstraint is
         when ConstraintNone =>
            null;

         when ConstraintConstant =>
            if Value > Constraint.MaxValueConstant then
               Value:=Constraint.MaxValueConstant;
            end if;

         when ConstraintUsingSize =>
            if Value > Size - Constraint.MaxValueConstant then
               Value := Size - Constraint.MaxValueConstant;
            end if;

         when ConstraintUsingParentSize =>
            if Value > ParentSize - Constraint.MaxValueConstant then
               Value := ParentSize - Constraint.MaxValueConstant;
            end if;

      end case;

      if Constraint.MinSizeConstraint=ConstraintConstant then
         if Size < Constraint.MinSizeConstant then
            if Value > OldValue then
               Value := OldValue + OldSize - Constraint.MinSizeConstant;
            end if;
            Size:=Constraint.MinSizeConstant;
         end if;
      end if;

   end ApplyConstraint;
   ---------------------------------------------------------------------------

   procedure StoreAnchors
     (Anchors      : in out Anchors_Type;
      ClientBounds : Bounds_Type;
      ParentBounds : Bounds_Type) is
   begin

      Anchors.TopBorder    := ClientBounds.Top;

      Anchors.LeftBorder   := ClientBounds.Left;

      Anchors.RightBorder  := ParentBounds.Width
        - ClientBounds.Left - ClientBounds.Width;

      Anchors.BottomBorder := ParentBounds.Height
        - ClientBounds.Top - ClientBounds.Height;

   end StoreAnchors;
   ---------------------------------------------------------------------------

   procedure RestoreAnchors
     (Anchors      : Anchors_Type;
      ClientBounds : in out Bounds_Type;
      ParentBounds : Bounds_Type) is
   begin
      if Anchors.Left then
         if Anchors.Right then
            ClientBounds.Width := ParentBounds.Width
              - ClientBounds.Left - Anchors.RightBorder;
         end if;
      else
         if Anchors.Right then
            ClientBounds.Left := ParentBounds.Width
              - ClientBounds.Width - Anchors.RightBorder;
         end if;
      end if;
      if Anchors.Top then
         if Anchors.Bottom then
            ClientBounds.Height := ParentBounds.Height
              - ClientBounds.Top - Anchors.BottomBorder;
         end if;
      else
         if Anchors.Bottom then
            ClientBounds.Top := ParentBounds.Height
              - ClientBounds.Height - Anchors.BottomBorder;
         end if;
      end if;
   end RestoreAnchors;
   ---------------------------------------------------------------------------

end BoundsCalc;
