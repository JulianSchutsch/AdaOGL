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

package body DistributedSystems is

   procedure Free
     (Item : access Spawn_Type) is

      procedure FreeMemory is new Ada.Unchecked_Deallocation
        (Object => Spawn_Type'Class,
         Name   => Spawn_ClassAccess);

      ItemVal : Spawn_ClassAccess:=Spawn_ClassAccess(Item);

   begin
      FreeMemory(ItemVal);
   end Free;
   ---------------------------------------------------------------------------

   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type) is

   begin

      if Item.OnFailure/=null then
         Item.OnFailure(SupplementConfig);
      end if;

   end Execute;
   ---------------------------------------------------------------------------

end DistributedSystems;
