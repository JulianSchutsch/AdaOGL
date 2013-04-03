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

with Interfaces.C.Strings;
with BSDSockets.Thin;
with System;
with ProcessLoop;
with Ada.Unchecked_Conversion;
with Basics; use Basics;

package body BSDSockets is
   use type Interfaces.C.int;

   function SocketTypeToInt is new Ada.Unchecked_Conversion
     (Source => SocketTypeEnum,
      Target => Interfaces.C.int);

   function ProtocolToInt is new Ada.Unchecked_Conversion
     (Source => ProtocolEnum,
      Target => Interfaces.C.int);

   function ShutdownMethodToInt is new Ada.Unchecked_Conversion
     (Source => ShutdownMethodEnum,
      Target => Interfaces.C.int);

   function SendEnumToInt is new Ada.Unchecked_Conversion
     (Source => SendEnum,
      Target => Interfaces.C.int);

   function AddressToSockAddrAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => SockAddrAccess);

   function AddressToAddrInfoAccess is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => AddrInfoAccess);
   ---------------------------------------------------------------------------

   procedure Recv
     (Socket : SocketID;
      Data   : in out ByteOperations.ByteArray_Type;
      Flags  : SendEnum;
      Read   : out Integer) is

      Result : Interfaces.C.int;

   begin
      Result:=BSDSockets.Thin.Recv
        (Socket => Interfaces.C.int(Socket),
         Buf    => Data'Address,
         Len    => Data'Length,
         Flags  => SendEnumToInt(Flags));

      if Result<=0 then
         raise FailedRecv;
      end if;

      Read := Integer(Result);
   end Recv;
   ---------------------------------------------------------------------------

   procedure Send
     (Socket : SocketID;
      Data   : ByteOperations.ByteArray_Type;
      Flags  : SendEnum;
      Send   : out Integer) is

      Result : Interfaces.C.int;

   begin
      Result:=BSDSockets.Thin.Send
        (Socket => Interfaces.C.int(Socket),
         Data   => Data'Address,
         Length => Data'Length,
         Flags  => SendEnumToInt(Flags));

      if Result<0 then
         raise FailedSend;
      end if;

      Send := Integer(Result);
   end;
   ---------------------------------------------------------------------------

   procedure CloseSocket
     (Socket : SocketID) is

      Result : Interfaces.C.int;

   begin
      Result:=BSDSockets.Thin.CloseSocket
        (Socket => Interfaces.C.int(Socket));
      if Result/=0 then
         raise FailedCloseSocket;
      end if;
   end CloseSocket;
   ---------------------------------------------------------------------------

   procedure Shutdown(Socket : SocketID;
                      Method : ShutdownMethodEnum) is

      Result : Interfaces.C.int;

   begin
      Result:=BSDSockets.Thin.Shutdown
        (Socket => Interfaces.C.int(Socket),
         Method => ShutdownMethodToInt(Method));
      if Result/=0 then
         raise FailedShutdown with "Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;
   end Shutdown;
   ---------------------------------------------------------------------------

   procedure AddEntry(List: access SelectList;
                      Entr: access SelectEntry) is
   begin
      if Entr.Priv.List/=null then
         raise EntryAddedToTwoLists;
      end if;
      Entr.Priv.Next := List.FirstEntry;
      Entr.Priv.Last := null;
      if Entr.Priv.Next/=null then
         Entr.Priv.Next.Priv.Last:=Entr;
      end if;
      Entr.Priv.List:=List;
      List.FirstEntry:=Entr;
   end AddEntry;
   ---------------------------------------------------------------------------

   procedure RemoveEntry(Entr: access SelectEntry) is
   begin
      if Entr.Priv.List=null then
         raise EntryNotAddedToAnyList;
      end if;
      if Entr.Priv.Next/=null then
         Entr.Priv.Next.Priv.Last:=Entr.Priv.Last;
      end if;
      if Entr.Priv.Last/=null then
         Entr.Priv.Last.Priv.Next:=Entr.Priv.Next;
      else
         Entr.Priv.List.FirstEntry:=Entr.Priv.Next;
      end if;
      Entr.Priv.List := null;
   end RemoveEntry;
   ---------------------------------------------------------------------------

   function ToString(Port : PortID) return String is
   begin
      return Integer'Image(Integer(Port));
   end ToString;
   ---------------------------------------------------------------------------

   function ToString(Socket : SocketID) return String is
   begin
      return Integer'Image(Integer(Socket));
   end ToString;
   ---------------------------------------------------------------------------

   procedure AAccept
     (Socket    : SocketID;
      Host      : out Unbounded_String;
      Port      : out PortID;
      NewSocket : out SocketID) is

      Result  : Interfaces.C.int;
      Addr    : aliased SockAddr_In6;
      AddrLen : aliased Interfaces.C.int;

   begin
      AddrLen := SockAddr_In6'Size/8;
      Result  := BSDSockets.Thin.AAccept
        (Socket  => Interfaces.C.int(Socket),
         Addr    => Addr'Access,
         AddrLen => AddrLen'Access);
      Host := BSDSockets.Thin.AddressToString
        (Addr    => Addr'Access,
         AddrLen => Positive(AddrLen));
      Port := PortID(BSDSockets.Thin.NTOHS(Addr.sin6_port));
      if Result=-1 then
         raise FailedAccept with "Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;
      NewSocket := SocketID(Result);
   end AAccept;
   ---------------------------------------------------------------------------

   procedure SSelect(Sockets : in out SelectList) is
      Fill        : Integer:=0;
      ReadSet     : aliased BSDSockets.Thin.fd_set_struct;
      WriteSet    : aliased BSDSockets.Thin.fd_set_struct;
      TimeVal     : aliased BSDSockets.Thin.TimeVal;
      Result      : Interfaces.C.int;
      MainCursor  : access SelectEntry:=Sockets.FirstEntry;
      StartCursor : access SelectEntry:=Sockets.FirstEntry;
      MaxSocket   : Interfaces.C.int;

   begin
      BSDSockets.Thin.FD_ZERO(ReadSet'Access);
      BSDSockets.Thin.FD_ZERO(WriteSet'Access);
      TimeVal.tv_sec  := 0;
      TimeVal.tv_usec := 0;
      MaxSocket := 1023; --TODO: TEMP HACK
      while MainCursor/=null loop
         if Interfaces.C.int(MainCursor.Socket)>MaxSocket then
            MaxSocket:=Interfaces.C.int(MainCursor.Socket);
         end if;
         BSDSockets.Thin.FD_SET(MainCursor.Socket,ReadSet'Access);
         BSDSockets.Thin.FD_SET(MainCursor.Socket,WriteSet'Access);
         Fill := Fill+1;
         if Fill=BSDSockets.Thin.FD_SETSIZE then
            Result := BSDSockets.Thin.SSelect
              (NumberOfSockets => MaxSocket+1,
               ReadSet         => ReadSet'Access,
               WriteSet        => WriteSet'Access,
               ExceptSet       => null,
               TimeOut         => TimeVal'Access);

            if Result/=0 then
               raise FailedSelect;
            end if;

            MaxSocket := 1023; -- TODO: TEMP HACK

            while Fill/=0 loop

               StartCursor.Readable
                 := BSDSockets.Thin.FD_ISSET
                   (StartCursor.Socket,ReadSet'Access)/=0;
               StartCursor.Writeable
                 := BSDSockets.Thin.FD_ISSET
                   (StartCursor.Socket,WriteSet'Access)/=0;

               StartCursor := StartCursor.Priv.Next;
               Fill := Fill - 1;
            end loop;
            BSDSockets.Thin.FD_ZERO(ReadSet'Access);
            BSDSockets.Thin.FD_ZERO(WriteSet'Access);
         end if;
         MainCursor := MainCursor.Priv.Next;
      end loop;

      if Fill /= 0 then
         Result := BSDSockets.Thin.SSelect
           (NumberOfSockets => MaxSocket+1,
            ReadSet         => ReadSet'Access,
            WriteSet        => WriteSet'Access,
            ExceptSet       => null,
            TimeOut         => TimeVal'Access);

         while Fill /= 0 loop
            StartCursor.Readable := BSDSockets.Thin.FD_ISSET
              (Socket => StartCursor.Socket,
               Set    => ReadSet'Access)/=0;

            StartCursor.Writeable := BSDSockets.Thin.FD_ISSET
              (Socket => StartCursor.Socket,
               Set    => WriteSet'Access)/=0;

            StartCursor := StartCursor.Priv.Next;
            Fill := Fill - 1;
         end loop;
      end if;
   end SSelect;
   ---------------------------------------------------------------------------

   procedure FreeAddrInfo(AddrInfo: not null AddrInfoAccess) is
   begin
      BSDSockets.Thin.FreeAddrInfo(pAddrInfo => AddrInfo);
   end FreeAddrInfo;
   ---------------------------------------------------------------------------

   function AddrInfo_Next(AddrInfo: AddrInfoAccess) return AddrInfoAccess is
   begin
      return AddressToAddrInfoAccess(AddrInfo.ai_next);
   end AddrInfo_Next;
   ---------------------------------------------------------------------------

   function GetAddrInfo(AddressFamily : AddressFamilyEnum;
                        SocketType    : SocketTypeEnum;
                        Protocol      : ProtocolEnum;
                        Host          : String) return AddrInfoAccess is
      Result          : Interfaces.C.int;
      AddrInfoPointer : aliased AddrInfoAccess;
      HostPtr         : Interfaces.C.Strings.chars_ptr;
      Hints           : aliased AddrInfo;
   begin
      Hints.ai_flags     := 0;
      Hints.ai_family    := BSDSockets.Thin.DecypherAddressFamily(AddressFamily);
      Hints.ai_socktype  := SocketTypeToInt(SocketType);
      Hints.ai_protocol  := ProtocolToInt(Protocol);
      Hints.ai_addrlen   := 0;
      Hints.ai_canonname := Interfaces.C.Strings.Null_Ptr;
      Hints.ai_addr      := System.Null_Address;
      Hints.ai_next      := System.Null_Address;
      HostPtr := Interfaces.C.Strings.New_String(Str => Host);

      Result:=BSDSockets.Thin.GetAddrInfo
        (pNodeName    => HostPtr,
         pServiceName => Interfaces.C.Strings.Null_Ptr,
         pHints       => Hints'Access,
         ppResult     => AddrInfoPointer'Access);

      if Result/=0 then
         raise FailedGetAddrInfo with "Error Code"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;
      return AddrInfoPointer;
   end GetAddrInfo;
   ---------------------------------------------------------------------------

   procedure Listen
     (Socket  : SocketID;
      Backlog : Integer:=0) is

      Result : Interfaces.C.int;

   begin
      Result:=BSDSockets.Thin.Listen(Socket  => Interfaces.C.int(Socket),
                                     Backlog => Interfaces.C.int(Backlog));
      if Result/=0 then
         raise FailedListen with "Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;
   end;
   ---------------------------------------------------------------------------

   procedure Bind
     (Socket : SocketID;
      Port   : PortID;
      Family : AddressFamilyEnum;
      Host   : String := "") is

      Addr    : aliased SockAddr_In6;
      Result  : Interfaces.C.int;
      HostPtr : aliased Interfaces.C.Strings.chars_ptr;
--      OptTrue : aliased Interfaces.C.int:=1;

   begin
      for i in 0..15 loop
         Addr.sin6_addr.s6_addr(i):=0;
      end loop;

--      Result:=BSDSockets.Thin.SetSockOpt
--        (Socket => Interfaces.C.int(Socket),
--         Level  => BSDSockets.Thin.SOL_SOCKET,
--         OptName => BSDSockets.Thin.SO_REUSEADDR,
--         OptVal  => OptTrue'Address,
--         OptLen => Interfaces.C.int'Size/8);
--      if Result/=0 then
--         Put(Integer(BSDSockets.Thin.Error));
--         New_Line;
--         raise FAILEDBIND;
--      end if;

      HostPtr:=Interfaces.C.Strings.New_String(Host);

      Result:=BSDSockets.Thin.INET_PTON
        (AddressFamily => BSDSockets.Thin.DecypherAddressFamily(Family),
         AddrString    => HostPtr,
         Buffer        => Addr.sin6_addr'Access);
--      for i in 0..15 loop
--         Put(Integer(Addr.sin6_addr.s6_addr(i)));
--      end loop;
--      New_Line;

      Interfaces.C.Strings.Free(Item=>HostPtr);

      if Result/=1 then
         raise FailedBind with "Failed Address translation with Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;

--      Put("::::");
--      Put(Integer(BSDSockets.Thin.DecypherAddressFamily(Family)));
--      Put(Integer(Port));
--      Put(Integer(SockAddr_In6'Size/8));
--      Put(Integer(In_Addr6'Size/8));
--      New_Line;

      Addr.sin6_family   := Interfaces.C.short(BSDSockets.Thin.DecypherAddressFamily(Family));
      Addr.sin6_port     := BSDSockets.Thin.HTONS
        (Interfaces.C.unsigned_short(Port));
      Addr.sin6_flowinfo := 0;
      Addr.sin6_scope_id := 0;

      Result:=BSDSockets.Thin.Bind(Socket  => Interfaces.C.int(Socket),
                                   Name    => Addr'Access,
                                   NameLen => Addr'Size/8);
      if Result/=0 then
         raise FailedBind with "Failed Bind with Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;
   end Bind;
   ---------------------------------------------------------------------------

   procedure Connect
     (Socket   : SocketID;
      AddrInfo : not null AddrInfoAccess;
      Port     : PortID) is
      use type System.Address; -- HACK

      -- This is a dangerous conversion since it converts between different
      -- sized data types.
      -- It is assumed size_t'Size>int'Size
      pragma Warnings(Off);
      function SizeTToInt is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.size_t,
         Target => Interfaces.C.int);
      pragma Warnings(On);

      Result : Interfaces.C.int;

   begin
      -- CAUTION : ai_addr might have an invalid address, propably 0
      AddressToSockAddrAccess(AddrInfo.ai_addr).sa_port := BSDSockets.Thin.HTONS
        (Interfaces.C.unsigned_short(Port));

--      Put(Integer(AddrInfo.ai_flags));
--      Put(Integer(AddrInfo.ai_family));
--      Put(Integer(AddrInfo.ai_socktype));
--      Put(Integer(AddrInfo.ai_protocol));
--      Put(Integer(SizeTToInt(AddrInfo.ai_addrlen)));
--      New_Line;
      Result:=BSDSockets.Thin.Connect(Interfaces.C.int(Socket),
                                      AddressToSockAddrAccess(AddrInfo.ai_addr),
                                      SizeTToInt(AddrInfo.ai_addrlen));

      if Result/=0 then
         raise FailedConnect with "Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;

   end Connect;
   ---------------------------------------------------------------------------

   function Socket
     (AddrInfo: AddrInfoAccess) return SocketID is

      Value : Interfaces.C.int;

   begin
      Value:=BSDSockets.Thin.Socket(AddrInfo.ai_family,
                                    AddrInfo.ai_socktype,
                                    AddrInfo.ai_protocol);
      if Value=-1 then
         raise FailedSocket;
      end if;
      return SocketID(Value);
   end Socket;
   ---------------------------------------------------------------------------

   function Socket
     (AddressFamily : AddressFamilyEnum;
      SocketType    : SocketTypeEnum;
      Protocol      : ProtocolEnum) return SocketID is

      Value         : Interfaces.C.int;

   begin
      Value:=BSDSockets.Thin.Socket(BSDSockets.Thin.DecypherAddressFamily(AddressFamily),
                                    SocketTypeToInt(SocketType),
                                    ProtocolToInt(Protocol));
      if Value=-1 then
         raise FailedSocket with "Error Code:"
           &Interfaces.C.int'Image(BSDSockets.Thin.Error);
      end if;
      return SocketID(Value);
   end Socket;
   ---------------------------------------------------------------------------

   procedure DoProcess
     (Object : AnyObject_ClassAccess) is
      pragma Unreferenced(Object);
   begin
      SSelect
        (Sockets => DefaultSelectList);
   end DoProcess;

   procedure Initialize is
   begin
      BSDSockets.Thin.Initialize;
      ProcessLoop.Add(DoProcess'Access,null);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ProcessLoop.Remove(DoProcess'Access,null);
      BSDSockets.Thin.Finalize;
   end Finalize;
   ---------------------------------------------------------------------------

end BSDSockets;
