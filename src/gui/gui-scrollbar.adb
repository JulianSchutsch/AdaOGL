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

package body GUI.ScrollBar is

   function GetMin
     (Item : access ScrollBar_Type)
      return Integer is
   begin
      return Item.Min;
   end GetMin;
   ---------------------------------------------------------------------------

   function GetMax
     (Item : access ScrollBar_Type)
      return Integer is
   begin
      return Item.Max;
   end GetMax;
   ---------------------------------------------------------------------------

   function GetPosition
     (Item : access ScrollBar_Type)
      return Integer is
   begin
      return Item.Position;
   end GetPosition;
   ---------------------------------------------------------------------------

   procedure SetPosition
     (Item     : access ScrollBar_Type;
      Position : Integer) is

   begin

      if (Position<Item.Min) or
        (Position>Item.Max) then
         raise InvalidScrollBarPosition
           with Integer'Image(Item.Min)&".."
           &Integer'Image(Item.Max)&" does not contain "
           &Integer'Image(Position);
      end if;

      if (Item.Position/=Position) then

         Item.Position:=Position;
         ScrollBar_ClassAccess(Item).UpdateBarPosition;

         if Item.OnPositionChange/=null then
            Item.OnPositionChange(Item.CallBackObject);
         end if;

      end if;

   end SetPosition;
   ---------------------------------------------------------------------------

   procedure SetRange
     (Item     : access ScrollBar_Type;
      Min      : Integer;
      Max      : Integer;
      Position : Integer) is
   begin

      if Max<Min then
         raise InvalidScrollBarRange;
      end if;
      if (Position<Min) or
        (Position>Max) then
         raise InvalidScrollBarPosition;
      end if;

      if (Item.Min/=Min) or (Item.Max/=Max) then

         Item.Min:=Min;
         Item.Max:=Max;
         ScrollBar_ClassAccess(Item).UpdateBarPosition;

         if Item.Position/=Position then
            Item.Position:=Position;
            if Item.OnPositionChange/=null then
               Item.OnPositionChange(Item.CallBackObject);
            end if;
         end if;

         return;

      end if;

      SetPosition(Item,Position);

   end SetRange;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item    : access ScrollBar_Type;
      Parent  : Object_ClassAccess) is
   begin

      GUI.Initialize
        (Item   => Object_Access(Item),
         Parent => Parent);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access ScrollBar_Type) is
   begin

      GUI.Free
        (Item => Object_Access(Item));

   end Free;
   ---------------------------------------------------------------------------

end GUI.ScrollBar;
