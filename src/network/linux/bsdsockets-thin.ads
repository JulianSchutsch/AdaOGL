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
--   27.Jan 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package BSDSockets.Thin is

   SOL_SOCKET   : constant Interfaces.C.int:=1;
   SO_REUSEADDR : constant Interfaces.C.int:=2;

   function Error return Interfaces.C.int;

   FD_SETSIZE: constant Natural:=1024;

   -- WARNING : This packet array may not be portable between different
   --           compilers
   type fd_set_element is mod 2**32;
   for fd_set_element'Size use 32;

   type fd_set_struct is array (0..FD_SETSIZE/32-1) of fd_set_element;
   for fd_set_struct'Alignment use 32;

   type TimeVal is
      record
         tv_sec  : Interfaces.C.long;
         tv_usec : Interfaces.C.long;
      end record;
   pragma Convention(C,TimeVal);

   function Socket
     (AddressFamily : Interfaces.C.int;
      SocketType    : Interfaces.C.int;
      ProtocolType  : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Socket,"socket");

   function Bind
     (Socket  : Interfaces.C.int;
      Name    : access SockAddr_In6;
      NameLen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Bind,"bind");

   function Listen
     (Socket  : Interfaces.C.int;
      Backlog : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Listen,"listen");

   function SetSockOpt
     (Socket : Interfaces.C.int;
      Level  : Interfaces.C.int;
      OptName : Interfaces.C.int;
      OptVal  : System.Address;
      OptLen  : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,SetSockOpt,"setsockopt");

   function INET_PTON
     (AddressFamily : Interfaces.C.int;
      AddrString    : Interfaces.C.Strings.chars_ptr;
      Buffer        : access In_Addr6)
      return Interfaces.C.int;
   pragma Import(C,INET_PTON,"inet_pton");

   function AddressToString
     (Addr    : access SockAddr_In6;
      AddrLen : Natural)
      return Unbounded_String;

   function HTONS
     (HostShort : Interfaces.C.unsigned_short)
      return Interfaces.C.unsigned_short;
   pragma Import(C,HTONS,"htons");

   function NTOHS
     (NetShort : Interfaces.C.unsigned_short)
      return Interfaces.C.unsigned_short;
   pragma Import(C,NTOHS,"ntohs");

   function Recv
     (Socket : Interfaces.C.int;
      Buf    : System.Address;
      Len    : Interfaces.C.int;
      Flags  : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Recv,"recv");

   function Send
     (Socket : Interfaces.C.int;
      Data   : System.Address;
      Length : Interfaces.C.int;
      Flags  : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Send,"send");

   function CloseSocket
     (Socket : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,CloseSocket,"close");

   function ShutDown
     (Socket : Interfaces.C.int;
      Method : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,ShutDown,"shutdown");

   function AAccept
     (Socket  : Interfaces.C.int;
      Addr    : access SockAddr_In6;
      AddrLen : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,AAccept,"accept");

   function SSelect
     (NumberOfSockets : Interfaces.C.int;
      ReadSet        : access fd_set_struct;
      WriteSet       : access fd_set_struct;
      ExceptSet      : access fd_set_struct;
      TimeOut        : access TimeVal)
      return Interfaces.C.int;
   pragma Import(C,SSelect,"select");

   function GetAddrInfo
     (pNodeName    : Interfaces.C.Strings.chars_ptr;
      pServiceName : Interfaces.C.Strings.chars_ptr;
      pHints       : access AddrInfo;
      ppResult     : access AddrInfoAccess)
      return Interfaces.C.int;
   pragma Import(C,GetAddrInfo,"getaddrinfo");

   function Connect
     (Socket  : Interfaces.C.int;
      Name    : SockAddrAccess;
      NameLen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Connect,"connect");

   procedure FreeAddrInfo
     (pAddrInfo : access AddrInfo);
   pragma Import(C,FreeAddrInfo,"freeaddrinfo");

   procedure FD_SET
     (Socket : SocketID;
      Set    : access fd_set_struct);

   procedure FD_CLR
     (Socket : SocketID;
      Set    : access fd_set_struct);

   function FD_ISSET
     (Socket : SocketID;
      Set    : access fd_set_struct)
      return Interfaces.C.int;

   procedure FD_ZERO
     (Set : access fd_set_struct);

   function DecypherAddressFamily
     (AddressFamily : AddressFamilyEnum)
      return Interfaces.C.int;

   procedure Initialize is null;
   procedure Finalize is null;

private


end BSDSockets.Thin;
