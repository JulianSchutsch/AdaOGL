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

package body TaskQueue is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Task_Type'Class,
      Name   => Task_ClassAccess);

   function Execute
     (Item      : access Task_Type)
      return Boolean is
      pragma Warnings(Off,Item);
   begin
      return True;
   end Execute;
   ---------------------------------------------------------------------------

   procedure AddTask
     (Queue : Queue_Access;
      Item  : Task_ClassAccess) is
   begin
      if Item.Queue/=null then
         raise TaskAllreadyInList;
      end if;
      Item.Queue:=Queue;
      Item.Next:=null;
      Item.Last:=Queue.LastTask;
      if Item.Last/=null then
         Item.Last.Next:=Item;
      else
         Queue.Tasks:=Item;
      end if;
      Queue.LastTask := Item;
   end AddTask;
   ---------------------------------------------------------------------------

   procedure FreeTask
     (Item : Task_ClassAccess) is

      ItemVal : Task_ClassAccess;

   begin
      if Item.Queue/=null then
         if Item.Next/=null then
            Item.Next.Last:=Item.Last;
         else
            Item.Queue.LastTask:=Item.Last;
         end if;
         if Item.Last/=null then
            Item.Last.Next:=Item.Next;
         else
            Item.Queue.Tasks:=Item.Next;
         end if;
         -- Security
         Item.Queue:=null;
      end if;
      ItemVal := Item;
      Free(ItemVal);
   end FreeTask;
   ---------------------------------------------------------------------------

   procedure First
     (Queue   : in out Queue_Type;
      Element : out Task_ClassAccess) is

   begin
      Element:=Queue.Tasks;
      if Element=null then
         return;
      end if;
      if Element.Next/=null then
         Element.Next.Last:=null;
      else
         Queue.LastTask:=null;
      end if;
      Queue.Tasks   := Element.Next;
      Element.Queue := null;
      -- Added for security
      Element.Next:=null;
      Element.Last:=null;
   end First;
   ---------------------------------------------------------------------------

   function Empty
     (Queue : in Queue_Type)
      return Boolean is
   begin
      return (Queue.Tasks=null);
   end Empty;
   ---------------------------------------------------------------------------

end TaskQueue;
