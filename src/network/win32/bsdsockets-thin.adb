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
--with Ada.Strings;

package body BSDSockets.Thin is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;

   function DecypherAddressFamily
     (AddressFamily : AddressFamilyEnum)
      return Interfaces.C.int is
   begin
      case AddressFamily is
         when AF_INET =>
            return 2;
         when AF_INET6 =>
            return 23;
      end case;
   end DecypherAddressFamily;

   type WSAData is
      record
         wVersion       : Interfaces.Unsigned_16;
         wHighVersion   : Interfaces.Unsigned_16;
         szDescription  : Interfaces.C.char_array(0..256);
         szSystemStatus : Interfaces.C.char_array(0..128);
         iMaxSockets    : Interfaces.Integer_16;
         iMaxUdpDg      : Interfaces.Integer_16;
         lpVendorInfo   : Interfaces.C.Strings.chars_ptr;
      end record;
   pragma Convention(C,WSADATA);
   ---------------------------------------------------------------------------

   function WSAStartup
     (Version : Interfaces.C.unsigned_short;
      Data    : access WSAData)
      return Interfaces.C.int;
   pragma Import(StdCall,WSAStartup,"WSAStartup");
   ---------------------------------------------------------------------------

   function WSACleanup
     return Interfaces.C.int;
   pragma Import(StdCall,WSACleanup,"WSACleanup");
   ---------------------------------------------------------------------------

   function WSAGetLastError
     return Interfaces.C.int;
   pragma Import(StdCall,WSAGetLastError,"WSAGetLastError");
   ---------------------------------------------------------------------------

   function WSAStringToAddressA
     (AddressString   : Interfaces.C.Strings.chars_ptr;
      AddressFamily   : Interfaces.C.int;
      lpProtocolInfo  : System.Address;
      lpAddress       : access SockAddr_In6;
      lpAddressLength : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(StdCall,WSAStringToAddressA,"WSAStringToAddressA");
   ---------------------------------------------------------------------------

   function WSAAddressToStringA
     (lpsaAddress             : access SockAddr_In6;
      dwAddressLength         : Interfaces.Unsigned_32;
      lpProtocolInfo          : System.Address;
      lpszAddressString       : Interfaces.C.Strings.chars_ptr;
      lpdwAddressStringLength : access Interfaces.Unsigned_32)
      return Interfaces.C.int;
   pragma Import(StdCall,WSAAddressToStringA,"WSAAddressToStringA");
   ---------------------------------------------------------------------------

   function AddressToString
     (Addr    : access SockAddr_In6;
      AddrLen : Natural)
      return Unbounded_String is

      use type Interfaces.Unsigned_32;

      ReturnVal  : Interfaces.C.int;
      AddrString : String(1..47):=(1..47 => ' ');
      pragma Warnings(Off,AddrString);
      AddrPtr    : Interfaces.C.Strings.chars_ptr;
      Len        : aliased Interfaces.Unsigned_32
        :=Interfaces.Unsigned_32(AddrString'Last);

      Result : Unbounded_String;

   begin
      AddrPtr:=Interfaces.C.Strings.New_String(AddrString);
      ReturnVal:=WSAAddressToStringA
        (lpsaAddress             => Addr,
         dwAddressLength         => Interfaces.Unsigned_32(AddrLen),
         lpProtocolInfo          => System.Null_Address,
         lpszAddressString       => AddrPtr,
         lpdwAddressStringLength => Len'Access);

      if ReturnVal=0 then
         Result:=To_Unbounded_String(Interfaces.C.Strings.Value(AddrPtr));

         -- The result includes a remove port which has to be removed
         Result:=Head
           (Source=>Result,
            Count =>Index
              (Source=>Result,
               Pattern=>":",
               From=>Length(Result),
               Going=>Ada.Strings.Backward));
         -- For IPv6 Windows puts the address in square brackets which need
         -- to be removed.
         if Result/="" and then (Element(Result,1)='[') then
            Result:=Unbounded_Slice
              (Source => Result,
               Low    => 2,
               High   => Index
                 (Source  => Result,
                  Pattern => "]",
                  From    =>Length(Result),
                  Going   =>Ada.Strings.Backward)-1);
         end if;
      else
         Result:=To_Unbounded_String("");
      end if;
      Interfaces.C.Strings.Free
        (Item => AddrPtr);
      return Result;
   end AddressToString;
   ---------------------------------------------------------------------------

   function INET_PTON
     (AddressFamily : Interfaces.C.int;
      AddrString    : Interfaces.C.Strings.chars_ptr;
      Buffer        : access In_Addr6)
      return Interfaces.C.int is

      Addr   : aliased SockAddr_In6;
      Result : Interfaces.C.int;
      Len    : aliased Interfaces.C.int;

   begin
      Len := SockAddr_In6'Size*8;
      Result:=WSAStringToAddressA(AddressString   => AddrString,
                                  AddressFamily   => AddressFamily,
                                  lpProtocolInfo  => System.Null_Address,
                                  lpAddress       => Addr'Access,
                                  lpAddressLength => Len'Access);
      Buffer.all := Addr.sin6_addr;
      if Result=0 then
         return 1;
      else
         return 0;
      end if;
   end INET_PTON;
   ---------------------------------------------------------------------------

   function Error
     return Interfaces.C.int is
   begin
      return WSAGetLastError;
   end Error;
   ---------------------------------------------------------------------------

   procedure FD_SET
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      Set.fd_array(Integer(Set.fd_count)) := Socket;
      Set.fd_count := Set.fd_count+1;
   end FD_SET;
   ---------------------------------------------------------------------------

   procedure FD_CLR
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      for i in Set.fd_array'Range loop
         if Set.fd_array(i)=Socket then
            for j in i..Set.fd_array'Last-1 loop
               Set.fd_array(j):=Set.fd_array(j+1);
            end loop;
            exit;
         end if;
      end loop;
   end FD_CLR;
   ---------------------------------------------------------------------------

   function FD_ISSET
     (Socket : SocketID;
      Set    : access fd_set_struct)
      return Interfaces.C.int is
   begin
      for i in 0..Integer(Set.fd_count)-1 loop
         if Set.fd_array(i)=Socket then
            return 1;
         end if;
      end loop;
      return 0;
   end FD_ISSET;

   procedure FD_ZERO
     (Set : access fd_set_struct) is
   begin
      Set.fd_count:=0;
   end FD_ZERO;
   ---------------------------------------------------------------------------

   -- Stores information obtained during initialization
   WSAInfo : aliased WSAData;

   procedure Initialize is
   begin
      if WSAStartup(16#0202#,WSAInfo'Access)/=0 then
         raise FailedNetworkAPIInitialization;
      end if;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      if WSACleanup/=0 then
         Put("Why is WSACleanup failing?");
         New_Line;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

end BSDSockets.Thin;
