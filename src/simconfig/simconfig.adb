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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with Expressions;
with NodeInfo;
with Basics; use Basics;

package body SimConfig is

   package ConfigElemList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ConfigElem_Type,
      "="          => "=");

   package ConfigSetList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ConfigSet_Type,
      "="          => "=");

   procedure DebugConfigArray
     (ConfigArray : ConfigArray_Access) is

      procedure Debug
        (Space       : String;
         ConfigArray : ConfigArray_Access);

      procedure Debug
        (Space          : String;
         ConfigSetArray : ConfigSetArray_Access) is
      begin
         if ConfigSetArray=null then
            return;
         end if;
         for i in ConfigSetArray'Range loop
            Put(Space);
            Put("Key :");
            Put_Line(ConfigSetArray(i).Key);
            Put(Space);
            Put("Description : ");
            Put_Line(ConfigSetArray(i).Description);
            Debug(Space&" ",ConfigSetArray(i).Options);
         end loop;
      end Debug;
      ------------------------------------------------------------------------

      procedure Debug
        (Space       : String;
         ConfigArray : ConfigArray_Access) is
      begin
         if ConfigArray=null then
            return;
         end if;
         for i in ConfigArray'Range loop
            Put(Space);
            Put("Type         : ");
            case ConfigArray(i).TType is
               when ConfigElemString =>
                  Put_Line("String");
               when ConfigElemChoice =>
                  Put_Line("Choice");
               when ConfigElemCheck =>
                  Put_Line("Check");
               when ConfigElemConstantString =>
                  Put_Line("Constant String");
            end case;
            Put(Space);
            Put("Node         : ");
            Put_Line(ConfigArray(i).Node);
            Put(Space);
            Put("Description  : ");
            Put_Line(ConfigArray(i).Description);
            Put(Space);
            Put("Default      : ");
            Put_Line(ConfigArray(i).Default);
            Put(Space);
            Put("DefaultIndex : ");
            Put(ConfigArray(i).DefaultIndex);
            New_Line;
            Debug(Space&" ",ConfigArray(i).Set);
         end loop;
      end Debug;
      ------------------------------------------------------------------------

   begin
      Debug("",ConfigArray);
   end DebugConfigArray;
   ---------------------------------------------------------------------------

   procedure FreeSetArray
     (ConfigSetArray : in out ConfigSetArray_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => ConfigSetArray_Type,
         Name   => ConfigSetArray_Access);
   begin
      for i in ConfigSetArray'Range loop
         if ConfigSetArray(i).Options/=null then
            FreeConfigArray(ConfigSetArray(i).Options);
         end if;
      end loop;
      Free(ConfigSetArray);
   end FreeSetArray;
   ---------------------------------------------------------------------------

   procedure FreeConfigArray
     (ConfigArray : in out ConfigArray_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => ConfigArray_Type,
         Name   => ConfigArray_Access);
   begin
      for i in ConfigArray'Range loop
         if ConfigArray(i).Set/=null then
            FreeSetArray(ConfigArray(i).Set);
         end if;
      end loop;
      Free(ConfigArray);
   end FreeConfigArray;
   ---------------------------------------------------------------------------

   function CreateConfigArrayFromConfiguration
     (Configuration : Config.Config_Type)
      return ConfigArray_Access is

      Config : ConfigArray_Access;
      Cursor : StringStringMap_Pack.Cursor;

   begin
      Config:=new ConfigArray_Type
        (1..Integer(Configuration.Length));
      Cursor:=Configuration.First;
      for i in Config'Range loop
         Config(i).TType       := ConfigElemString;
         Config(i).Node        := StringStringMap_Pack.Key(Cursor);
         Config(i).Description := Config(i).Node;
         Config(i).Default     := StringStringMap_Pack.Element(Cursor);
         Cursor:=StringStringMap_Pack.Next(Cursor);
      end loop;
      return Config;
   end CreateConfigArrayFromConfiguration;
   ---------------------------------------------------------------------------

   function LoadConfigArray
     (FileName : Unbounded_String)
      return ConfigArray_Access is

      Config : ConfigArray_Access;
      File : File_Type;

      Command  : Unbounded_String;
      Param    : Unbounded_String;

      function GetCommand
        return Boolean is

         Line     : Unbounded_String;
         Position : Integer;

      begin
         Line:=U("");
         while not End_of_File(File) loop
            Get_Line(File,Line);
            Line:=Trim(Line,Both);
            if Line/="" then
               exit;
            end if;
         end loop;
         if Line="" then
            return False;
         end if;
         Position:=Index
           (Source => Line,
            Pattern => " ");
         if Position/=0 then
            Command:=Unbounded_Slice
              (Source => Line,
               Low    => 1,
               High   => Position-1);
            Param:=Unbounded_Slice
              (Source => Line,
               Low    => Position+1,
               High   => Length(Line));
            Trim(Param,Both);
         else
            Command:=Line;
            Param:=U("");
         end if;
         return True;
      end GetCommand;
      ------------------------------------------------------------------------

      procedure LoadOptions
        (Config : in out ConfigArray_Access);

      procedure LoadSet
        (Set : in out ConfigSetArray_Access) is

         List           : ConfigSetList_Pack.List;
         CurrentElement : ConfigSet_Type;
         ProcElement    : Boolean:=False;
         Cursor         : ConfigSetList_Pack.Cursor;

      begin
         while GetCommand loop
            if Command="key" then
               if ProcElement then
                  List.Append(CurrentElement);
               end if;
               CurrentElement.Key:=Expressions.Process(Param,NodeInfo.Variables);
               CurrentElement.Description:=U("");
               CurrentElement.Options:=null;
               ProcElement:=True;
            elsif Command="description" then
               CurrentElement.Description:=Expressions.Process(Param,NodeInfo.Variables);
            elsif Command="options" then
               LoadOptions(CurrentElement.Options);
               if (Command/="end")
                 or (Param/="options") then
                  raise MissingOptionsEnd;
               end if;
            else
               exit;
            end if;

         end loop;
         if ProcElement then
            List.Append(CurrentElement);
         end if;

         Set:=new ConfigSetArray_Type
           (1..Integer(List.Length));
         Cursor:=List.First;
         for i in Set'Range loop
            Set(i) := ConfigSetList_Pack.Element(Cursor);
            Cursor := ConfigSetList_Pack.Next(Cursor);
         end loop;

      end LoadSet;
      ------------------------------------------------------------------------

      procedure LoadOptions
        (Config : in out ConfigArray_Access) is

         use type ConfigElemList_Pack.Cursor;

         List           : ConfigElemList_Pack.List;
         CurrentElement : ConfigElem_Type;
         pragma Warnings(Off,CurrentElement);
         ProcElement    : Boolean:=False;
         Cursor         : ConfigElemList_Pack.Cursor;

      begin
         while GetCommand loop

            if Command="element" then
               if ProcElement then
                  List.Append(CurrentElement);
               end if;
               CurrentElement.Node         := U("");
               CurrentElement.Description  := U("");
               CurrentElement.Set          := null;
               CurrentElement.Default      := U("");
               CurrentElement.DefaultIndex := 0;
               ProcElement:=True;
               if Param="string" then
                  CurrentElement.TType:=ConfigElemString;
               elsif Param="choice" then
                  CurrentElement.TType:=ConfigElemChoice;
               elsif Param="check" then
                  CurrentElement.TType:=ConfigElemCheck;
               elsif Param="constantstring" then
                  CurrentElement.TType:=ConfigElemConstantString;
               else
                  raise InvalidElementType;
               end if;
            elsif Command="node" then
               CurrentElement.Node:=Expressions.Process(Param,NodeInfo.Variables);
            elsif Command="description" then
               CurrentElement.Description:=Expressions.Process(Param,NodeInfo.Variables);
            elsif Command="default" then
               CurrentElement.Default:=Expressions.Process(Param,NodeInfo.Variables);
            elsif Command="defaultindex" then
               if not TryStringToInteger
                 (Expressions.Process(Param,NodeInfo.Variables),
                  CurrentElement.DefaultIndex'Access) then
                  raise IntegerExpected;
               end if;
            elsif Command="set" then
               if CurrentElement.Set/=null then
                  raise SecondSetForElement;
               end if;
               LoadSet(CurrentElement.Set);
               if (Command/="end") or
                 (Param/="set") then
                  raise MissingSetEnd with "XX"&To_String(Command);
               end if;
            else
               exit;
            end if;

         end loop;
         if ProcElement then
            List.Append(CurrentElement);
         end if;

         Config:=new ConfigArray_Type
           (1..Integer(List.Length));
         Cursor:=List.First;
         for i in Config'Range loop
            Config(i) := ConfigElemList_Pack.Element(Cursor);
            Cursor    := ConfigElemList_Pack.Next(Cursor);
         end loop;

      end LoadOptions;
      ------------------------------------------------------------------------

   begin
      Open
        (File => File,
         Mode => In_File,
         Name => To_String(FileName));
      LoadOptions(Config);
      Close(File);
      return Config;
   end LoadConfigArray;
   ---------------------------------------------------------------------------

end SimConfig;
