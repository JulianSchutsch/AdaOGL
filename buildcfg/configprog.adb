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
--   20.Apr 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   Each program in ParallelSim has different requirements including
--   different requirements on different plattforms.
--   This program generates the necessary .ads and .adb files
--   for each kind of program and adds register and unregister calls
--   unified behind a single register and unregister call.
--   It also determines the current plattform and checks if the picked
--   implementations are compatible.
--   A config.gpr is written to make this information available
--   to all gpr files.

pragma Ada_2005;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;

with Plattform; use Plattform;
with Implementations; use Implementations;
with Common; use Common;

procedure ConfigProg is

   ImplementationNotPlattformCompatible : Exception;

   ImplementationsActive : array(Implementation_Enum) of Boolean:=(others => false);

   type ModuleArray_Type is array(Module_Enum) of ImplementationList_Pack.List;

   Modules : ModuleArray_Type;

   function ImplementationInModulePresent
     (Module         : Module_Enum;
      Implementation : Implementation_Enum)
      return Boolean is

      use type ImplementationList_Pack.Cursor;

   begin
      return Modules(Module).Find(Implementation)
        /=ImplementationList_Pack.No_Element;
   end ImplementationInModulePresent;
   ---------------------------------------------------------------------------

   procedure AddImplementation
     (Name : Unbounded_String) is

      Module         : Module_Enum;
      Implementation : Implementation_Enum;

   begin
      FindImplementation
        (Name           => Name,
         Module         => Module,
         Implementation => Implementation);

      if not Compatible(Implementation,Detected) then
         raise ImplementationNotPlattformCompatible
           with To_String(Name);
      end if;

      if not ImplementationsActive(Implementation) then
         ImplementationsActive(Implementation):=True;
         Modules(Module).Append(Implementation);
      end if;

   end AddImplementation;
   ---------------------------------------------------------------------------

   procedure RemoveImplementation
     (Name : Unbounded_String) is

      use type ImplementationList_Pack.Cursor;

      Module         : Module_Enum;
      Implementation : Implementation_Enum;
      Cursor         : ImplementationList_Pack.Cursor;

   begin
      FindImplementation
        (Name => Name,
         Module => Module,
         Implementation => Implementation);

      if ImplementationsActive(Implementation) then
         ImplementationsActive(Implementation):=False;
         Cursor:=Modules(Module).Find(Implementation);
         if Cursor/=ImplementationList_Pack.No_Element then
            Modules(Module).Delete(Cursor);
         end if;
      end if;

   end RemoveImplementation;

   procedure AddDefaultImplementations is
   begin

      for i in Implementation_Enum'Range loop
         if Default(i)
           and not ImplementationsActive(i) then
            ImplementationsActive(i):=True;
            Modules(Implementations.Implementations(i).Module).Append(i);
         end if;
      end loop;

   end AddDefaultImplementations;
   ---------------------------------------------------------------------------

   procedure ProcessArguments is

      Argument       : Unbounded_String;

   begin

      for i in 1..Ada.Command_Line.Argument_Count loop

         Argument:=To_Unbounded_String(Ada.Command_Line.Argument(i));
         Put("Argument:");
         Put(To_String(Argument));
         New_Line;
         if Element(Argument,1)='!' then
            RemoveImplementation(Unbounded_Slice(Argument,2,Length(Argument)));
         else
            if Argument/="default" then
               AddImplementation(Argument);
            else
               AddDefaultImplementations;
            end if;
         end if;

      end loop;

   end ProcessArguments;
   ---------------------------------------------------------------------------

   procedure WriteCopyright
     (File : in out File_Type) is
   begin
      Put_Line(File,"-------------------------------------------------------------------------------");
      Put_Line(File,"--   Copyright 2012 Julian Schutsch");
      Put_Line(File,"--");
      Put_Line(File,"--   This file is part of ParallelSim");
      Put_Line(File,"--");
      Put_Line(File,"--   ParallelSim is free software: you can redistribute it and/or modify");
      Put_Line(File,"--   it under the terms of the GNU Affero General Public License as published");
      Put_Line(File,"--   by the Free Software Foundation, either version 3 of the License, or");
      Put_Line(File,"--   (at your option) any later version.");
      Put_Line(File,"--");
      Put_Line(File,"--   ParallelSim is distributed in the hope that it will be useful,");
      Put_Line(File,"--   but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line(File,"--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line(File,"--   GNU Affero General Public License for more details.");
      Put_Line(File,"--");
      Put_Line(File,"--   You should have received a copy of the GNU Affero General Public License");
      Put_Line(File,"--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.");
      Put_Line(File,"------------------------------------------------------------------------------- ");
      Put_Line(File,"");
   end WriteCopyright;
   ---------------------------------------------------------------------------

   procedure WriteModuleInterface
     (Module : Module_Enum) is

      File : File_Type;

   begin
      begin
         Delete_File(Name => To_String(BasePath)&To_String(PackageFileName(Module)&".ads"));
      exception
         when others =>
            null;
      end;

      Create
        (File => File,
         Mode => Out_File,
         Name => To_String(BasePath)&To_String(PackageFileName(Module)&".ads"));
      WriteCopyright(File);
      Put_Line(File,"-- This file was automatically created by /autosrc/useimplementations.");
      Put_Line(File,"");
      Put_Line(File,"pragma Ada_2005;");
      Put_Line(File,"");
      Put_Line(File,"package "&To_String(PackageName(Module))& " is");
      Put_Line(File,"");
      Put_Line(File,"   procedure Register;");
      Put_Line(File,"   procedure Unregister;");
      Put_Line(File,"");
      Put_Line(File,"end "&To_String(PackageName(Module))&";");
      Close(File);
   end WriteModuleInterface;
   ---------------------------------------------------------------------------

   procedure WriteModuleBody
     (Module : Module_Enum) is

      use type ImplementationList_Pack.Cursor;

      File           : File_Type;
      Cursor         : ImplementationList_Pack.Cursor;
      Implementation : Implementation_Enum;

      procedure WriteWithStatements is
      begin

         Cursor:=Modules(Module).First;
         while Cursor/=ImplementationList_Pack.No_Element loop

            Implementation := ImplementationList_Pack.Element(Cursor);
            for i in ImplementationPackages(Implementation)'Range loop
               Put_Line(File,To_String("with "&ImplementationPackages(Implementation)(i)&";"));
            end loop;
            Cursor         := ImplementationList_Pack.Next(Cursor);

         end loop;

      end WriteWithStatements;
      ------------------------------------------------------------------------

      procedure WriteRegisterStatements is
      begin

         Cursor:=Modules(Module).First;
         if Cursor=ImplementationList_Pack.No_Element then
            Put_Line(File,"      null;");
            return;
         end if;
         while Cursor/=ImplementationList_Pack.No_Element loop

            Implementation := ImplementationList_Pack.Element(Cursor);
            for i in ImplementationPackages(Implementation)'Range loop
               Put_Line(File,To_String("      "&ImplementationPackages(Implementation)(i)&".Register;"));
            end loop;
            Cursor         := ImplementationList_Pack.Next(Cursor);

         end loop;

      end WriteRegisterStatements;
      ------------------------------------------------------------------------

      procedure WriteUnregisterStatements is
      begin

         Cursor:=Modules(Module).First;
         if Cursor=ImplementationList_Pack.No_Element then
            Put_Line(File,"      null;");
            return;
         end if;
         while Cursor/=ImplementationList_Pack.No_Element loop

            Implementation := ImplementationList_Pack.Element(Cursor);
            for i in ImplementationPackages(Implementation)'Range loop
               Put_Line(File,To_String("      "&ImplementationPackages(Implementation)(i)&".Unregister;"));
            end loop;
            Cursor         := ImplementationList_Pack.Next(Cursor);

         end loop;

      end WriteUnregisterStatements;
      ------------------------------------------------------------------------

   begin
      begin
         Delete_File(To_String(BasePath)&To_String(PackageFileName(Module)&".adb"));
      exception
         when others =>
            null;
      end;
      Create
        (File => File,
         Mode => Out_File,
         Name => To_String(BasePath)&To_String(PackageFileName(Module)&".adb"));
      WriteCopyright(File);
      Put_Line(File,"pragma Ada_2005;");
      Put_Line(File,"");
      WriteWithStatements;
      Put_Line(File,"package body "&To_string(PackageName(Module))&" is");
      Put_Line(File,"");
      Put_Line(File,"   procedure Register is");
      Put_Line(File,"   begin");
      WriteRegisterStatements;
      Put_Line(File,"   end Register;");
      Put_Line(File,"   ---------------------------------------------------------------------------");
      Put_Line(File,"");
      Put_Line(File,"   procedure Unregister is");
      Put_Line(File,"   begin");
      WriteUnregisterStatements;
      Put_Line(File,"   end Unregister;");
      Put_Line(File,"   ---------------------------------------------------------------------------");
      Put_Line(File,"");
      Put_Line(File,"end "&To_String(PackageName(Module))&";");
      Close(File);
   end WriteModuleBody;
   ---------------------------------------------------------------------------

   procedure WriteModules is

      File : File_Type;

   begin

      for Module in Modules'Range loop
         WriteModuleInterface(Module);
         WriteModuleBody(Module);
      end loop;

   end WriteModules;
   ---------------------------------------------------------------------------

   procedure WriteConfigGpr is

      File : File_Type;

   begin
      begin
         Delete_File(To_String(BasePath)&"../gpr/config.gpr");
      exception
         when others =>
            null;
      end;
      Create
        (File => File,
         Mode => Out_File,
         Name => To_String(BasePath)&"../gpr/config.gpr");
      WriteCopyRight(File);
      Put_Line(File,"-- This project file was created automatically and is only valid for:");
      Put_Line(File,"--   "&To_String(PlattformIdentifier(Detected)));
      Put_Line(File,"");
      Put_Line(File,"abstract project Config is");
      Put_Line(File,"");
      Put_Line(File,"   type Plattform_Type is");

      for i in Plattform_Enum'Range loop
         if i=Plattform_Enum'First then
            Put(File,"     (");
         else
            Put(File,"      ");
         end if;
         Put(File,""""&To_String(PlattformIdentifier(i))&"""");
         if i=Plattform_Enum'Last then
            Put_Line(File,");");
         else
            Put_Line(File,",");
         end if;
      end loop;

      Put_Line(File,"");
      Put_Line(File,"   Detected : Plattform_Type:="""
               &To_String(PlattformIdentifier(Detected))&""";");
      Put_Line(File,"");
      Put_Line(File,"   type Active_Type is (""On"",""Off"");");
      Put_Line(File,"");

      for i in Implementation_Enum'Range loop
         Put(File,"   "
             &To_String(Head
             (Source => Implementations.Implementations(i).Name,
              Count  => 20))
             &" : Active_Type:=");
         if ImplementationsActive(i) then
            Put_Line(File,"""On"";");
         else
            Put_Line(File,"""Off"";");
         end if;
      end loop;

      Put_Line(File,"");
      declare
         use type StringList_Pack.Cursor;
         Cursor : StringList_Pack.Cursor;
      begin
         Cursor:=AdditionalConfigLines.First;
         while Cursor/=StringList_Pack.No_Element loop
            Put_Line(File,To_String(StringList_Pack.Element(Cursor)));
            Cursor:=StringList_Pack.Next(Cursor);
         end loop;
      end;
      Put_Line(File,"");

      Put_Line(File,"end Config;");
      Close(File);

   end WriteConfigGpr;
   ---------------------------------------------------------------------------

   procedure ImplementationsInitialize is
   begin

      for i in Implementation_Enum'Range loop
         if ImplementationsActive(i) then
            if ImplementationInitialize(i)/=null then
               ImplementationInitialize(i).all;
            end if;
         end if;
      end loop;

   end ImplementationsInitialize;
   ---------------------------------------------------------------------------

begin

   if Ada.Command_Line.Argument_Count=0 then
      Put_Line("This program is responsible for initializing a build of ParallelSim");
      Put_Line("To select a number of implementations please any combination of:");
      New_Line;
      for i in Implementation_Enum'Range loop
         Put_Line(" "&To_String(Implementations.Implementations(i).Name));
      end loop;
      New_Line;
      Put_Line("Specifying ""default"" selects all compatible implementations for a plattform.");
      Put_Line("Implementations are checked for compatibility with the current plattform.");
      Put_Line("To exclude an implementation, prefix it with '!' .");
      Put_Line("All parameters are processed in the order they are given.");
      return;
   end if;
   Plattform.Initialize;
   Implementations.Initialize;
   Put_Line("Plattform : "&To_String(PlattformName(Detected)));
   ProcessArguments;
   Put_Line("Selected implementations :");
   New_Line;
   for i in Implementation_Enum loop
      if ImplementationsActive(i) then
         Put_Line(" "&To_String(Implementations.Implementations(i).Name));
      end if;
   end loop;
   New_Line;
   Put_Line("Write packages...");
   WriteModules;
   Put_Line("Initialize implementations (e.g. plattform specific constants)");
   ImplementationsInitialize;
   Put_Line("Write Config.gpr...");
   WriteConfigGpr;
   Put_Line("Done.");

exception
   when E:ImplementationNotFound =>
      Put_Line("Implementation unknown : "&Ada.Exceptions.Exception_Message(E));

   when E:ImplementationNotPlattformCompatible =>
      Put_Line("Implementation not compatible with this plattform : "
               &Ada.Exceptions.Exception_Message(E));

   when E:ImplementationInitializeFailed =>
      Put_Line("Unable to complete initialization of an implementation :");
      Put_Line(" "&Ada.Exceptions.Exception_Message(E));

end ConfigProg;
