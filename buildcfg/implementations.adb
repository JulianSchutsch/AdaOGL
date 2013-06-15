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
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib;
with GNAT.Strings;
with Plattform; use Plattform;

package body Implementations is

   -- MPICH2 must find the location of the libmpi and mpi.h first.
   -- On Windows this is done by finding the base path though
   -- locating mpiexec over the path environment variable.
   -- On Linux it is assumed that only a /mpich2 must be added for
   -- the mpi.h and the .so is found without any further knowledge.
   procedure MPICH2_Initialize is
      File : File_Type;
      MPIBasePath : Unbounded_String;

      function CleanFileName
        (Name : String)
         return String is
      begin
         if Name'Length=0 then
            return Name;
         end if;
         if Name(Name'First)='"' then
            return Name(Name'First+1..Name'Last-1);
         else
            return Name;
         end if;
      end CleanFileName;
      ------------------------------------------------------------------------

      procedure GetWindowsMPICH2BasePath is

         use type GNAT.Strings.String_Access;

         ExecPath : GNAT.OS_Lib.String_Access;

      begin
         ExecPath:=GNAT.OS_Lib.Locate_Exec_On_Path
           (Exec_Name => "mpiexec.exe");
         if ExecPath=null then
            raise ImplementationInitializeFailed
              with "Unable to locate mpiexec.exe";
         end if;
         MPIBasePath:=U(ExecPath.all(ExecPath.all'First..ExecPath.all'Last-16));
         GNAT.Strings.Free(ExecPath);
      end GetWindowsMPICH2BasePath;
      ------------------------------------------------------------------------

   begin

      if not Exists(To_String(BasePath)&"mpiconstants.c") then
         raise ImplementationInitializeFailed
           with "mpiconstant.c not found, maybe execution path is not in ./buildcfg";
      end if;

      begin
         Delete_File(To_String(BasePath)&"mpiconstants.h");
      exception
         when others =>
            null;
      end;

      Create
        (File => File,
         Mode => Out_File,
         Name => To_String(BasePath)&"mpiconstants.h");
      case Detected is
         when PlattformLinux =>
            Put_Line(File,"#include ""mpi.h""");
            AdditionalConfigLines.Append(U("   MPICH2LIB:=""."";"));

         when PlattformWindowsNT =>
            GetWindowsMPICH2BasePath;
            Put("MPICH2 Base Path :"&To_String(MPIBasePath));
            New_Line;
            if not Exists(To_String(MPIBasePath&"/include/mpi.h")) then
               Close(File);
               raise ImplementationInitializeFailed
                 with "mpi.h not found. Please set MPIBASE environment variable.";
            end if;
            Put_Line(File,"#include """&To_String(MPIBasePath)&"/include/mpi.h""");
            AdditionalConfigLines.Append("   MPICH2LIB:="""&MPIBasePath&"/lib"";");

         when others =>
            Close(File);
            raise ImplementationInitializeFailed
              with "Uncertain where to look for mpi.h, please add to implementations.adb";

      end case;

      Close(File);

      declare

         use type GNAT.OS_Lib.String_Access;

         ExecPath   : GNAT.OS_Lib.String_Access;
         Arguments  : GNAT.OS_Lib.String_List_Access;
         Success    : Boolean;
      begin
         ExecPath := GNAT.OS_Lib.Locate_Exec_On_Path
           (Exec_Name => "gcc");
         if ExecPath=null then
            raise ImplementationInitializeFailed
              with "Unable to locate gcc";
         end if;
         Arguments:=GNAT.OS_Lib.Argument_String_To_List
           (To_String(BasePath)&"mpiconstants.c -o "
            &To_String(BasePath)&"mpiconstants"&To_String(ExecutableSuffix)
            &" -I/usr/include/mpich2 -I/usr/include/mpich2-i386");
         GNAT.OS_Lib.Spawn
           (Program_Name => ExecPath.all,
            Args         => Arguments.all,
            Success      => Success);
         GNAT.Strings.Free(Arguments);
         GNAT.Strings.Free(ExecPath);

         if not Success then
            raise ImplementationInitializeFailed
              with "Unable to compile mpiconstants.c for mpich2 module";
         end if;
      end;

      declare

         use type GNAT.OS_Lib.String_Access;

         Arguments  : GNAT.OS_Lib.String_List_Access;
         Success    : Boolean;

      begin

         Arguments:=GNAT.OS_Lib.Argument_String_To_List
           (To_String(BasePath)&"mpiconstants.c -o "
            &To_String(BasePath)&"mpiconstants"&To_String(ExecutableSuffix));
         GNAT.OS_Lib.Spawn
           (Program_Name => To_String(BasePath)&"mpiconstants"&To_String(ExecutableSuffix),
            Args         => Arguments.all,
            Success      => Success);
         GNAT.Strings.Free(Arguments);

         if not Success then
            raise ImplementationInitializeFailed
              with "Unable to execute mpiconstants for mpich2";
         end if;
      end;

      Copy_File
        (Source_Name => "mpiconstants.ads",
         Target_Name => To_String(BasePath)&"../src/distributedsystems/mpi/mpiconstants.ads");
      Delete_File("mpiconstants.ads");

   end MPICH2_Initialize;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin

      case Detected is
         when PlattformUnknown =>
            null;
         when PlattformLinux =>
            Default(ImplementationBSDSockets) := True;
            Default(ImplementationXlib)       := True;
            Default(ImplementationMPICH2)     := True;
            -- Default(ImplementationFreeType)   := True;
         when PlattformWindowsNT =>
            Default(ImplementationBSDSockets) := True;
            Default(ImplementationWGL)        := True;
            Default(ImplementationMPICH2)     := True;
            -- Default(ImplementationFreeType)   := True;
      end case;
      Default(ImplementationBitmapFonts) := True;

   end;
   ---------------------------------------------------------------------------

   procedure FindImplementation
     (Name           : Unbounded_String;
      Module         : out Module_Enum;
      Implementation : out Implementation_Enum) is
   begin

      for i in Implementations'Range loop

         if Implementations(i).Name=Name then
            Module         := Implementations(i).Module;
            Implementation := i;
            return;
         end if;

      end loop;

      raise ImplementationNotFound with To_String(Name);

   end FindImplementation;
   ---------------------------------------------------------------------------

end Implementations;
