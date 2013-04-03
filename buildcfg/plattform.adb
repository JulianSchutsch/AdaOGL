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

with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Directories;

package body Plattform is

   procedure Initialize is

      ReturnCode : Integer;
      Success    : Boolean;

   begin

      -- uname plattform detection, if possible
      declare

         use type GNAT.Strings.String_Access;

         Arguments : GNAT.OS_Lib.String_List_Access;
         ExecPath  : GNAT.OS_Lib.String_Access;
      begin
         ExecPath := GNAT.OS_Lib.Locate_Exec_On_Path
           (Exec_Name => "uname");
         if ExecPath/=null then
            Arguments:=GNAT.OS_Lib.Argument_String_To_List("-s");
            GNAT.OS_Lib.Spawn
              (Program_Name => ExecPath.all,
               Args         => Arguments.all,
               Output_File  => "uname.out",
               Success      => Success,
               Return_Code  => ReturnCode,
               Err_To_Out   => True);
            GNAT.Strings.Free(Arguments);
         end if;
         GNAT.Strings.Free(ExecPath);
      end;

      if (ReturnCode=0) and Success then
         declare
            File : File_Type;
         begin
            Open
              (File => File,
               Mode => In_File,
               Name => "uname.out");
            begin
               declare
                  Content : String:=Get_Line(File);
               begin
                  Close(File);
                  Ada.Directories.Delete_File("uname.out");
                  if GNAT.Regpat.Match
                    (Expression => "MINGW",
                     Data       => Content) or
                    GNAT.Regpat.Match
                      (Expression => "CYGWIN",
                       Data => Content) then
                     ExecutableSuffix:=U(".exe");
                     Detected:=PlattformWindowsNT;
                     return;
                  end if;
                  if GNAT.Regpat.Match
                    (Expression => "Linux",
                     Data       => Content) then
                     Detected:=PlattformLinux;
                     return;
                  end if;
               end;
            exception
               when Ada.IO_Exceptions.End_Error =>
                  null;
            end;
         end;

         Detected:=PlattformUnknown;

      end if;

   end Initialize;
   ---------------------------------------------------------------------------

end Plattform;
