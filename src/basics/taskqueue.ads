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
--   7.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package TaskQueue is

   TaskAllreadyInList : Exception;

   type Task_Type is tagged private;
   type Task_ClassAccess is access all Task_Type'Class;
   type Queue_Type is limited private;
   type Queue_Access is access all Queue_Type;

   function Execute
     (Item      : access Task_Type)
      return Boolean;

   procedure AddTask
     (Queue : Queue_Access;
      Item  : Task_ClassAccess);

   procedure FreeTask
     (Item : Task_ClassAccess);

   procedure First
     (Queue   : in out Queue_Type;
      Element : out Task_ClassAccess);

   function Empty
     (Queue : in Queue_Type)
      return Boolean;

private

   type Task_Type is tagged
      record
         Next  : Task_ClassAccess:=null;
         Last  : Task_ClassAccess:=null;
         Queue : Queue_Access;
      end record;

   type Queue_Type is limited
      record
         Tasks    : Task_ClassAccess:=null;
         LastTask : Task_ClassAccess:=null;
      end record;

end TaskQueue;
