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

with Interfaces.C;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with GNAT.Strings;
with Ada.Directories; use Ada.Directories;
with ProcessLoop;
with Unix;

with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package body Processes is

   PathDirectories       : StringList_Pack.List;
   PathDirectoriesLoaded : Boolean:=False;

   procedure LoadPathDirectories is

      PathEnv : GNAT.OS_Lib.String_Access;
      Start   : Integer;

   begin

      if PathDirectoriesLoaded then
         return;
      end if;

      PathEnv := GNAT.OS_Lib.Getenv("PATH");
      Start   := PathEnv.all'First;
      Put_Line("PATH:"&PathEnv.all);
      for i in PathEnv.all'Range loop
         if PathEnv.all(i)=':' then
            -- Take String Start..i-1 if i-1-start>0 and add it to the list
            if i-Start>=1 then
               PathDirectories.Append(U(PathEnv.all(Start..i-1)));
            end if;
            Start:=i+1;
         end if;
      end loop;
      if Start<=PathEnv.all'Last then
         PathDirectories.Append(U(PathEnv.all(Start..PathEnv.all'Last)));
      end if;
      PathDirectoriesLoaded:=True;
      GNAT.Strings.Free(PathEnv);

   end LoadPathDirectories;
   ---------------------------------------------------------------------------

   procedure FindInPathDirectories
     (FileName : Unbounded_String;
      FullName : out Unbounded_String;
      Success  : out Boolean) is

      use type StringList_Pack.Cursor;

      Cursor : StringList_Pack.Cursor;

   begin
      LoadPathDirectories;
      Cursor:=PathDirectories.First;
      while Cursor/=StringList_Pack.No_Element loop
         declare
            Search   : Search_Type;
            DirEntry : Directory_Entry_Type;
         begin
            Start_Search
              (Search    => Search,
               Directory => To_String(StringList_Pack.Element(Cursor)),
               Pattern   => To_String(FileName),
               Filter    => (Ordinary_File => True, others => False));
            if More_Entries(Search) then
               Get_Next_Entry
                 (Search          => Search,
                  Directory_Entry => DirEntry);
               FullName:=U(Full_Name(DirEntry));
               End_Search(Search);
               Success:=True;
               return;
            end if;
            End_Search(Search);
         exception
            when Ada.IO_Exceptions.Name_Error =>
               null;
         end;
         Cursor:=StringList_Pack.Next(Cursor);
      end loop;
      Success:=False;
      return;
   end FindInPathDirectories;
   ---------------------------------------------------------------------------

   procedure ProcessQueue
     (Object : AnyObject_ClassAccess) is

      use type Interfaces.C.int;

      Process  : constant Process_Access:=Process_Access(Object);
      CharCode : aliased ByteOperations.ByteArray_Type:=(0 => 0);
      Result : Interfaces.C.int;

   begin

      declare

         use type Unix.pid_t_Type;

         Status : aliased  Interfaces.C.int;
         pid    : Unix.pid_t_Type;
      begin
         pid:=Unix.waitpid
           (pid     => Process.P.pid,
            status  => Status'Access,
            options => Unix.WNOHANG);
         if pid=-1 then
            Put_Line("Failed waitpid");
         end if;
         if pid=Process.P.pid then
            Put_Line("Process terminated");
            if Unix.close(Process.P.Pipe(0))/=0 then
               null;
            end if;
            ProcessLoop.Remove
              (Proc   => ProcessQueue'Access,
               Object => Object);
            if Process.OnTerminate/=null then
               Process.OnTerminate(Process.CallBackObject);
            end if;
            return;
         end if;
      end;

      loop
         Result:=Unix.read
           (FileDescriptor => Process.P.Pipe(0),
            Buffer         => CharCode(0)'Unchecked_Access,
            Count          => 1);
         if Result<=0 then
            if Unix.errno=Unix.EAGAIN then
               return;
            end if;
            Put_Line("Failed Read from Pipe");
            Put_Line(Interfaces.C.int'Image(Unix.errno));
            -- TODO: Handle error cases...some may require closing the pipe
            return;
         end if;
         case CharCode(0) is
            when 10 =>
               declare
                  Str : Unbounded_String;
               begin
                  Process.CharacterBuffer.ReadString(Str);
                  if Process.OnMessage/=null then
                     Put_Line("*"&To_String(Str));
                     Process.OnMessage(Process.CallBackObject,Str);
                  end if;
               end;
            when 13 =>
               null;
            when others =>
               Process.CharacterBuffer.AddCharacter(Character'Val(CharCode(0)));
         end case;

      end loop;

   end ProcessQueue;
   ---------------------------------------------------------------------------

   procedure Kill
     (Item : access Process_Type) is
   begin
      null;
   end Kill;
   ---------------------------------------------------------------------------

   procedure Execute
     (Item        : access Process_Type;
      ProgramName : Unbounded_String;
      Arguments   : Unbounded_String) is

      use type Interfaces.C.int;
      use type Unix.pid_t_Type;

      CProgram        : Interfaces.C.Strings.chars_ptr;
      CParameters     : Interfaces.C.Strings.chars_ptr;
      FullProgramName : Unbounded_String;

   begin

      declare
         Success : Boolean;
      begin
         FindInPathDirectories
           (FileName => ProgramName,
            FullName => FullProgramName,
            Success  => Success);
         if not Success then
            raise ExecutableNotFound with To_String(ProgramName);
         end if;
      end;

      if Unix.pipe(Item.P.Pipe'Access)<0 then
         raise FailedToCreatePipe with "call to pipe failed with "
           &Interfaces.C.int'Image(Unix.errno);
      end if;
      declare
         pid : Unix.pid_t_Type;
      begin
         pid:=Unix.fork;
         if pid<0 then
            if Unix.close(Item.P.Pipe(0))<0 then
               null;
            end if;
            if Unix.close(Item.P.Pipe(1))<0 then
               null;
            end if;
            raise FailedExecute with "Call to fork failed with "
              &Interfaces.C.int'Image(Unix.errno);
         end if;
         if pid/=0 then
            -- Parent process
            Item.P.pid:=pid;

            -- PipeArray
            --  0 : Read
            --  1 : Write
            -- Close writing end of the pipe
            if Unix.close(Item.P.Pipe(1))<0 then
               raise FailedExecute with "Call to close (writting end of the pipe) failed with "
                 &Interfaces.C.int'Image(Unix.errno);
            end if;

            Unix.SetNonBlocking(Item.P.Pipe(0));

         else
            -- Child process

            Put_Line("Child PROCESS");
            -- Close reading end of the pipe
            if Unix.close(Item.P.Pipe(0))<0 then
               Put_Line("Call to close (reading end of the pipe) failed");
            end if;

            -- Close ordinary stdout
            if Unix.close(1)<0 then
               Put_Line(Standard_Error,"Call to close(1) failed");
            end if;
            if Unix.close(2)<0 then
               null;
            end if;

            -- Reassign pipes writting end to stdout
            if Unix.dup2(Item.P.Pipe(1),1)<0 then
               Put_Line(Standard_Error,"Failed dup2");
               Put_Line(Standard_Error,Interfaces.C.int'Image(Unix.errno));
            end if;
            if Unix.dup2(Item.P.Pipe(1),2)<0 then
               null;
            end if;
            CProgram    := Interfaces.C.Strings.New_String(To_String(FullProgramName));
            CParameters := Interfaces.C.Strings.New_String(To_String(Arguments));
            if Unix.Exec
              (ProgramName => CProgram,
               Arguments   => CParameters)<0 then
               Put_Line(Standard_Error,"Failed Exec"
                       &Interfaces.C.int'Image(Unix.errno));
               Unix.eexit(1);
            end if;
            Interfaces.C.Strings.Free(CProgram);
            Interfaces.C.Strings.Free(CParameters);
         end if;
      end;

      ProcessLoop.Add(ProcessQueue'Access,AnyObject_ClassAccess(Item));

   end Execute;

end Processes;
