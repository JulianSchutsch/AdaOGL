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
--   15.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with DistributedSystems;
with Config;

package SimNodes is

   type NodeType_Enum is
     (NodeTypeFront,
      NodeTypeSim);

   FrontGroup    : DistributedSystems.Group_Type;
   SimGroup      : DistributedSystems.Group_Type;
   Configuration : Config.Config_Type;
   FirstFront    : DistributedSystems.Node_Type;
   FirstSim      : DistributedSystems.Node_Type;
   LastFront     : DistributedSystems.Node_Type;
   LastSim       : DistributedSystems.Node_Type;

   procedure Initialize
     (NodeType : NodeType_Enum);
   procedure Finalize;

end SimNodes;
