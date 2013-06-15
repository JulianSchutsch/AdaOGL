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
--   30.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;
with Config.Implementations;
with Basics; use Basics;
with Packets;

package Authentication is

   InvalidKey : Exception;

   type PublicKey_Type is abstract tagged null record;
   type PublicKey_ClassAccess is access all PublicKey_Type'Class;

   function Verify
     (PublicKey  : access PublicKey_Type;
      Message    : Unbounded_String;
      Encrypted  : Unbounded_String)
      return Boolean is abstract;

   procedure Free
     (PublicKey : access PublicKey_Type) is abstract;

   procedure WriteToPacket
     (PublicKey : access PublicKey_Type;
      Packet    : Packets.Packet_ClassAccess) is abstract;

   type PrivateKey_Type is abstract tagged null record;
   type PrivateKey_ClassAccess is access all PrivateKey_Type'Class;

   function Encrypt
     (PrivateKey : access PrivateKey_Type;
      Message    : Unbounded_String)
      return Unbounded_String is abstract;

   procedure Free
     (PrivateKey : access PrivateKey_Type) is abstract;
   ---------------------------------------------------------------------------

   type Generator_Type is abstract tagged null record;
   type Generator_ClassAccess is access all Generator_Type'Class;

   procedure GenerateKeyPair
     (System     : access Generator_Type;
      PublicKey  : out PublicKey_ClassAccess;
      PrivateKey : out PrivateKey_ClassAccess) is abstract;

   function GenerateMessage
     (System : access Generator_Type)
      return Unbounded_String is abstract;
   ---------------------------------------------------------------------------

   type Generator_Constructor is
     access function
       (Configuration : Config.Config_Type;
        Node          : Unbounded_String)
        return Generator_ClassAccess;

   type Generator_Destructor is
     access procedure
       (Item : in out Generator_ClassAccess);

   type ReadPublicKey_Access is
     access function
       (Packet : Packets.Packet_ClassAccess)
        return PublicKey_ClassAccess;

   type Implementation_Type is
      record
         NewGenerator  : Generator_Constructor:=null;
         FreeGenerator : Generator_Destructor:=null;
         ReadPublicKey : ReadPublicKey_Access:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => U("Implementation"));

end Authentication;
