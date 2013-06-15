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
--   21.Mai 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces.C.Strings;
with ByteOperations;
with UnixConstants;

package Unix is

   -- TODO: This type may not be portable, but should work on all GNU
   --       plattforms. The process of determining the correct type
   --       is complicated! (Lots of ugly c hacks)
   type pid_t_Type is new Interfaces.C.int;

   EAGAIN  : constant := UnixConstants.EAGAIN;
   WNOHANG : constant := UnixConstants.WNOHANG;

   function errno
     return Interfaces.C.int;
   pragma Import(C,errno,"geterrno");

   type FileDescriptor_Type is new Interfaces.C.int;

   type PipeFiles_Type is array(0..1) of FileDescriptor_Type;
   pragma Convention(C,PipeFiles_Type);

   function pipe
     (PipeFiles : access PipeFiles_Type)
      return Interfaces.C.int;
   pragma Import(C,pipe,"pipe");

   function read
     (FileDescriptor : FileDescriptor_Type;
      Buffer         : ByteOperations.Byte_Access;
      Count          : Interfaces.C.size_t)
      return Interfaces.C.int;
   pragma Import(C,read,"read");

   function close
     (FileDescriptor : FileDescriptor_Type)
      return Interfaces.C.int;
   pragma Import(C,close,"close");

   function fork
     return pid_t_Type;
   pragma Import(C,fork,"fork");

   function dup2
     (OldFileDescriptor : FileDescriptor_Type;
      NewFileDescriptor : FileDescriptor_Type)
      return Interfaces.C.int;
   pragma Import(C,dup2,"dup2");

   -- C procedure which tries to make this portable
   procedure SetNonBlocking
     (FileDescriptor : FileDescriptor_Type);
   pragma Import(C,SetNonBlocking,"SetNonBlocking");

   -- Wrapper for vararg execl call
   function Exec
     (ProgramName : Interfaces.C.Strings.chars_ptr;
      Arguments   : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;
   pragma Import(C,Exec,"_exec");

   procedure eexit
     (status : Interfaces.C.int);
   pragma Import(C,eexit,"exit");

   function kill
     (pid : pid_t_Type;
      sig : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,kill,"kill");

   function waitpid
     (pid     : pid_t_Type;
      status  : access Interfaces.C.int;
      options : Interfaces.C.int)
      return pid_t_Type;
   pragma Import(C,waitpid,"waitpid");

end Unix;
