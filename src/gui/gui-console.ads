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
--   5.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Canvas;
with Fonts;
with GUI.TextBasis;

package GUI.Console is

   type Console_Public is abstract new Object_Type with
      record
         OnInputEnter : GUI.TextBasis.OnInputEnter_Access;
      end record;

   type Console_Type is abstract new Console_Public with private;
   type Console_Access is access all Console_Type;
   type Console_ClassAccess is access all Console_Type'Class;

   type Console_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return Console_ClassAccess;

   overriding
   procedure Free
     (Item : access Console_Type);

   procedure WriteLine
     (Item   : access Console_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is abstract;

   procedure SetFont
     (Item   : access Console_Type;
      Font   : Fonts.Font_ClassAccess) is abstract;

   procedure Initialize
     (Item   : access Console_Type;
      Parent : Object_ClassAccess);

private

   type Console_Type is abstract new Console_Public with
      record
         null;
      end record;

end GUI.Console;
