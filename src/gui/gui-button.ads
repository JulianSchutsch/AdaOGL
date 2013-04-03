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
--   24.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.Button is

   type OnClick_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type Button_Public is abstract new Object_Type with
      record
         OnClick : OnClick_Access:=null;
         -- Read only
         Caption : Unbounded_String;
      end record;

   type Button_Type is new Button_Public with private;
   type Button_Access is access all Button_Type;
   type Button_ClassAccess is access all Button_Type'Class;

   procedure SetCaption
     (Item    : access Button_Type;
      Caption : Unbounded_String);

   type Button_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return Button_ClassAccess;

private

   type Button_Type is new Button_Public with
      record
         null;
      end record;

end GUI.Button;
