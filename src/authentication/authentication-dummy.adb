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

with Ada.Unchecked_Deallocation;
with Types; use Types;

package body Authentication.Dummy is

   DummyKey : constant Integer32:=-10144;

   type PublicKey_Type is new Authentication.PublicKey_Type with null record;
   type PublicKey_Access is access all PublicKey_Type;

   overriding
   function Verify
     (PublicKey : access PublicKey_Type;
      Message   : Unbounded_String;
      Encrypted : Unbounded_String)
      return Boolean;

   overriding
   procedure Free
     (PublicKey : access PublicKey_Type);

   overriding
   procedure WriteToPacket
     (PublicKey : access PublicKey_Type;
      Packet    : Packets.Packet_ClassAccess);
   ---------------------------------------------------------------------------

   type PrivateKey_Type is new Authentication.PrivateKey_Type with null record;
   type PrivateKey_Access is access all PrivateKey_Type;

   overriding
   function Encrypt
     (PrivateKey : access PrivateKey_Type;
      Message    : Unbounded_String)
      return Unbounded_String;

   overriding
   procedure Free
     (PrivateKey : access PrivateKey_Type);
   ---------------------------------------------------------------------------

   type Generator_Type is new Authentication.Generator_Type with null record;
   type Generator_Access is access all Generator_Type;

   overriding
   procedure GenerateKeyPair
     (Generator  : access Generator_Type;
      PublicKey  : out PublicKey_ClassAccess;
      PrivateKey : out PrivateKey_ClassAccess);

   overriding
   function GenerateMessage
     (Generator : access Generator_Type)
      return Unbounded_String;
   ---------------------------------------------------------------------------

   function GenerateMessage
     (Generator : access Generator_Type)
      return Unbounded_String is

      pragma Unreferenced(Generator);

   begin
      return U("Dummy-Message");
   end GenerateMessage;
   ---------------------------------------------------------------------------

   procedure WriteToPacket
     (PublicKey : access PublicKey_Type;
      Packet    : Packets.Packet_ClassAccess) is

      pragma Unreferenced(PublicKey);

   begin

      Packet.Write(DummyKey);

   end WriteToPacket;
   ---------------------------------------------------------------------------

   function ReadPublicKey
     (Packet    : Packets.Packet_ClassAccess)
      return PublicKey_ClassAccess is

      Key       : Integer32;
      PublicKey : PublicKey_Access;

   begin

      Key:=Packet.Read;
      if Key/=DummyKey then
         raise InvalidKey with "Dummy key";
      end if;
      PublicKey:=new PublicKey_Type;
      return PublicKey_ClassAccess(PublicKey);

   end ReadPublicKey;
   ---------------------------------------------------------------------------

   procedure GenerateKeyPair
     (Generator  : access Generator_Type;
      PublicKey  : out PublicKey_ClassAccess;
      PrivateKey : out PrivateKey_ClassAccess) is

      pragma Unreferenced(Generator);

      NewPublicKey  : PublicKey_Access;
      NewPrivateKey : PrivateKey_Access;

   begin
      NewPublicKey  := new PublicKey_Type;
      NewPrivateKey := new PrivateKey_Type;
      PublicKey  := PublicKey_ClassAccess(NewPublicKey);
      PrivateKey := PrivateKey_ClassAccess(NewPrivateKey);
   end GenerateKeyPair;
   ---------------------------------------------------------------------------

   function Encrypt
     (PrivateKey : access PrivateKey_Type;
      Message    : Unbounded_String)
      return Unbounded_String is

      pragma Unreferenced(PrivateKey);

   begin
      return Message;
   end Encrypt;
   ---------------------------------------------------------------------------

   procedure Free
     (PrivateKey : access PrivateKey_Type) is

      procedure FreeIt is new Ada.Unchecked_Deallocation
        (Object => PrivateKey_Type,
         Name   => PrivateKey_Access);

      Val : PrivateKey_Access:=PrivateKey_Access(PrivateKey);
   begin
      FreeIt(Val);
   end Free;
   ---------------------------------------------------------------------------

   function Verify
     (PublicKey : access PublicKey_Type;
      Message   : Unbounded_String;
      Encrypted : Unbounded_String)
      return Boolean is

      pragma Unreferenced(PublicKey);

   begin
      return Message=Encrypted;
   end Verify;
   ---------------------------------------------------------------------------

   procedure Free
     (PublicKey : access PublicKey_Type) is

      procedure FreeIt is new Ada.Unchecked_Deallocation
        (Object => PublicKey_Type,
         Name   => PublicKey_Access);

      Val : PublicKey_Access:=PublicKey_Access(PublicKey);

   begin
      FreeIt(Val);
   end Free;
   ---------------------------------------------------------------------------

   function NewGenerator
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Generator_ClassAccess is

      pragma Unreferenced(Configuration);
      pragma Unreferenced(Node);

      NewGenerator : Generator_Access;

   begin

      NewGenerator:=new Generator_Type;
      return Generator_ClassAccess(NewGenerator);

   end NewGenerator;
   ---------------------------------------------------------------------------

   procedure FreeGenerator
     (Item : in out Generator_ClassAccess) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Generator_Type,
         Name   => Generator_Access);

      ItemVal : Generator_Access:=Generator_Access(Item);

   begin

      Free(ItemVal);
      Item:=null;

   end FreeGenerator;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewGenerator  => NewGenerator'Access,
      FreeGenerator => FreeGenerator'Access,
      ReadPublicKey => ReadPublicKey'Access);
   Identifier :constant Unbounded_String := U("Dummy");

   procedure Register is
   begin

      Implementations.Register
        (Implementation =>  Implementation,
         Identifier     => Identifier);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister(Identifier);

   end Unregister;
   ---------------------------------------------------------------------------

end Authentication.Dummy;
