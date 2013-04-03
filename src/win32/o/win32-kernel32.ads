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
--   18.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with ByteOperations; use ByteOperations;

package Win32.Kernel32 is

   function CreateProcess
     (lpApplicationName    : Interfaces.C.Strings.chars_ptr;
      lpCommandLine        : Interfaces.C.Strings.chars_ptr;
      lpProcessAttributes  : access SECURITY_ATTRIBUTES_Type:=null;
      lpThreadAttributes   : access SECURITY_ATTRIBUTES_Type:=null;
      bInheritHandles      : Interfaces.C.int;
      dwCreationFlags      : Interfaces.Unsigned_32;
      lpEnvironment        : System.Address;
      lpCurrentDirectory   : Interfaces.C.Strings.chars_ptr;
      lpStartupInfo        : access STARTUPINFO_Type:=null;
      lpProcessInformation : access PROCESS_INFORMATION_Type)
      return Interfaces.C.int;
   pragma Import(StdCall,CreateProcess,"CreateProcessA");

   function CloseHandle
     (hObject : HANDLE_Type)
      return Interfaces.C.int;
   pragma Import(Stdcall,CloseHandle,"CloseHandle");

   function GetModuleHandle
     (lpModuleName : LPCTSTR_Type)
      return HInstance_Type;
   pragma Import(StdCall,GetModuleHandle,"GetModuleHandleA");

   function CreatePipe
     (hReadPipe        : access Handle_Type;
      hWritePipe       : access Handle_Type;
      lpPipeAttributes : access SECURITY_ATTRIBUTES_Type;
      nSize            : DWORD_Type)
      return Boolean;
   pragma Import(StdCall,CreatePipe,"CreatePipe");

   function SetHandleInformation
     (hObject : Handle_Type;
      dwMask  : DWORD_Type;
      dwFlags : DWORD_Type)
      return Boolean;
   pragma Import(StdCall,SetHandleInformation,"SetHandleInformation");

   function GetExitCodeProcess
     (hProcess   : HANDLE_Type;
      lpExitCode : access DWORD_Type)
      return Boolean;
   pragma Import(StdCall,GetExitCodeProcess,"GetExitCodeProcess");

   function ReadFile
     (hFile                : HANDLE_Type;
      lpBuffer             : access ByteArray_Type;
      nNumberOfBytesToRead : DWORD_Type;
      lpNumberOfBytesRead  : access DWORD_Type;
      lpOverlapped         : access OVERLAPPED_Type)
      return Boolean;
   pragma Import(StdCall,ReadFile,"ReadFile");

   function SetCommTimeouts
     (hFile          : HANDLE_Type;
      lpCommTimeouts : access COMMTIMEOUTS_Type)
      return Boolean;
   pragma Import(StdCall,SetCommTimeouts,"SetCommTimeouts");

   function PeekNamedPipe
     (hNamedPipe             : HANDLE_Type;
      lpBuffer               : access ByteArray_Type;
      nBufferSize            : DWORD_Type;
      lpBytesRead            : access DWORD_Type;
      lpTotalBytesAvail      : access DWORD_Type;
      lpBytesLeftThisMessage : access DWORD_Type)
      return Boolean;
   pragma Import(StdCall,PeekNamedPipe,"PeekNamedPipe");

   function TerminateProcess
     (hProcess : HANDLE_Type;
      uExitCode : UINT_Type)
      return Boolean;
   pragma Import(StdCall,TerminateProcess,"TerminateProcess");

end Win32.Kernel32;
