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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;

package body Config.Implementations is

   type ImplementationDesc_Type is
      record
         Identifier     : Unbounded_String;
         Implementation : Implementation_Type;
      end record;

   package List_Pack is
     new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ImplementationDesc_Type,
      "=" => "=");

   List : List_Pack.List;

   procedure Unregister
     (Identifier : Unbounded_String) is

      use type List_Pack.Cursor;

      Cursor             : List_Pack.Cursor;
      ImplementationDesc : ImplementationDesc_Type;

   begin

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         ImplementationDesc:=List_Pack.Element(Cursor);
         if ImplementationDesc.Identifier=Identifier then
            List.Delete(Cursor);
            return;
         end if;
         Cursor:=List_Pack.Next(Cursor);
      end loop;

      raise ImplementationNotFound
        with "UnRegister failed";

   end;
   ---------------------------------------------------------------------------

   procedure Register
     (Identifier     : Unbounded_String;
      Implementation : Implementation_Type) is

   begin

      List.Append
        (New_Item =>
           (Identifier     => Identifier,
            Implementation => Implementation));

   end Register;
   ---------------------------------------------------------------------------

   procedure Debug is

      use type List_Pack.Cursor;

      Cursor             : List_Pack.Cursor;
      ImplementationDesc : ImplementationDesc_Type;

   begin

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         ImplementationDesc:=List_Pack.Element(Cursor);
         Put(To_String(ImplementationDesc.Identifier));
         New_Line;
         Cursor:=List_Pack.Next(Cursor);
      end loop;

   end Debug;
   ---------------------------------------------------------------------------

   function Find
     (ImplementationName : Unbounded_String)
      return Implementation_Type is

      use type List_Pack.Cursor;

      Cursor             : List_Pack.Cursor;
      ImplementationDesc : ImplementationDesc_Type;

   begin

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         ImplementationDesc:=List_Pack.Element(Cursor);
         if ImplementationDesc.Identifier=ImplementationName then
            return ImplementationDesc.Implementation;
         end if;
         Cursor :=List_Pack.Next(Cursor);
      end loop;

      Put_Line("Available Implementations:");
      Debug;
      raise ImplementationNotFound
        with To_String(ImplementationName);

   end Find;
   ---------------------------------------------------------------------------

   function FindAny
     return Implementation_Type is

      use type Ada.Containers.Count_Type;

   begin

      if List.Length=0 then
         raise ImplementationNotFound;
      end if;
      return List_Pack.Element(List.First).Implementation;

   end FindAny;
   ---------------------------------------------------------------------------

   function Find
     (Configuration : Config_Type;
      Node          : Unbounded_String)
      return Implementation_Type is

   begin

      return Find
        (ImplementationName => Configuration.Element
           (Key => Node&"."&IdentifierKey));

   end Find;
   ---------------------------------------------------------------------------

end Config.Implementations;
