-------------------------------------------------------------------------------
--   Copyright 2011 Julian Schutsch
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

with Linux;

package body BSDSockets.Thin is
   use type Interfaces.C.int;

   c_AF_INET : Interfaces.C.int;
   pragma Import(C,c_AF_INET,"c_AF_INET");
   c_AF_INET6:Interfaces.C.int;
   pragma Import(C,c_AF_INET6,"c_AF_INET6");

   function DecypherAddressFamily
     (AddressFamily : AddressFamilyEnum)
      return Interfaces.C.int is
   begin
      case AddressFamily is
         when AF_INET =>
            return c_AF_INET;
         when AF_INET6 =>
            return c_AF_INET6;
      end case;
   end DecypherAddressFamily;

   function INET_PTON
     (AddressFamily : Interfaces.C.int;
      Addr          : access In_Addr6;
      AddrString    : Interfaces.C.Strings.chars_ptr;
      AddrLength    : Interfaces.C.size_t)
      return Interfaces.C.int;
   pragma Import(C,INET_PTON,"inet_pton");

   function AddressToString
     (Addr    : access SockAddr_In6;
      AddrLen : Natural)
      return Unbounded_String is

      pragma Warnings(Off,AddrLen);

      ReturnVal : Interfaces.C.int;
      AddrString : Interfaces.C.char_array(0..46);
      pragma Warnings(Off,AddrString);
      AddrPtr    : Interfaces.C.Strings.chars_ptr;

      Result : Unbounded_String;

   begin
      AddrPtr:=Interfaces.C.Strings.New_Char_Array(AddrString);

      ReturnVal:=INET_PTON
        (AddressFamily => Interfaces.C.int(Addr.sin6_family),
         Addr          => Addr.sin6_addr'Access,
         AddrString    => AddrPtr,
         AddrLength    => 47);

      if ReturnVal=1 then
         Result :=To_Unbounded_String(Interfaces.C.To_Ada
           (Interfaces.C.Strings.Value
              (Item => AddrPtr)));
      else
         Result:=To_Unbounded_String("");
      end if;
      Interfaces.C.Strings.Free
        (Item => AddrPtr);
      return Result;
   end AddressToString;

   function Error return Interfaces.C.int is
   begin
      return Linux.errno;
   end Error;
   ---------------------------------------------------------------------------

   procedure FD_SET
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      Set(Integer(Socket)/32):=Set(Integer(Socket)/32) or 2**Integer(Socket);
   end FD_SET;
   ---------------------------------------------------------------------------

   procedure FD_CLR
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      Set(Integer(Socket)/32):=Set(Integer(Socket)/32) and not (2**Integer(Socket));
   end FD_CLR;
   ---------------------------------------------------------------------------

   function FD_ISSET
     (Socket : SocketID;
      Set    : access fd_set_struct)
      return Interfaces.C.int is
   begin
      return Interfaces.C.int(Set(Integer(Socket)/32) and (2**Integer(Socket)));
   end FD_ISSET;
   ---------------------------------------------------------------------------

   procedure FD_ZERO
     (Set : access fd_set_struct) is
   begin
      for i in Set'Range loop
         Set(i):=0;
      end loop;
   end FD_ZERO;
   ---------------------------------------------------------------------------

end BSDSockets.Thin;
