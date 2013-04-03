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

with MPI; use MPI;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;
with Interfaces.C;
with Config;
with DistributedSystems; use DistributedSystems;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Processes;
with GNAT.Regpat;

package body MPI.Node is

   type Spawn_Type is new DistributedSystems.Spawn_Type with
      record
         Configuration       : Config.Config_Type;
         ExecutableArguments : Unbounded_String;
         Process             : aliased Processes.Process_Type;
      end record;
   type Spawn_Access is access all Spawn_Type;

   overriding
   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type);

   overriding
   procedure Free
     (Item : access Spawn_Type);
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Spawn_Type) is
   begin
      Item.Process.Kill;
      DistributedSystems.Spawn_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   procedure ProcessMessage
     (CallBackObject : AnyObject_ClassAccess;
      Message        : Unbounded_String) is

      Spawn            : constant Spawn_Access:=Spawn_Access(CallBackObject);
      SupplementConfig : Config.Config_Type;

   begin
      if Spawn.OnMessage/=null then
         Spawn.OnMessage(Message);
      end if;
      if GNAT.Regpat.Match
        (Expression => "Credentials required to connect",
         Data       => To_String(Message)) then
         SupplementConfig.Include
           (Key      => U("Account"),
            New_Item => U(""));
         SupplementConfig.Include
           (Key      => U("Password"),
            New_Item => U(""));
         if Spawn.OnFailure/=null then
            Spawn.OnFailure(SupplementConfig);
         end if;
         Spawn.Process.Kill;
      end if;
      if GNAT.Regpat.Match
        (Expression => "Aborting",
         Data       => To_String(Message)) then
         if Spawn.OnFailure/=null then
            Spawn.OnFailure(SupplementConfig);
         end if;
         Spawn.Process.Kill;
      end if;
      if GNAT.Regpat.Match
        (Expression => "launch failed",
         Data       => To_String(Message)) then
         if Spawn.OnFailure/=null then
            Spawn.OnFailure(SupplementConfig);
         end if;
         Spawn.Process.Kill;
      end if;
      if GNAT.Regpat.Match
        (Expression => "MPI-Node initialized",
         Data       =>To_String(Message)) then
         if Spawn.OnSuccess/=null then
            Spawn.OnSuccess.all;
         end if;
      end if;
   end ProcessMessage;
   ---------------------------------------------------------------------------

   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type) is

      use type StringStringMap_Pack.Cursor;

      Arguments      : Unbounded_String;
      Cursor         : StringStringMap_Pack.Cursor;

   begin

      Item.Configuration.SaveToFile("_MyConfig");

      Arguments := U("-noprompt ");

      Cursor:=SupplementConfig.Find(U("Account"));
      if Cursor/=StringStringMap_Pack.No_Element then
         declare
            Account  : constant Unbounded_String:=StringStringMap_Pack.Element(Cursor);
            Password : Unbounded_String;
            File     : File_Type;
         begin
            Cursor:=SupplementConfig.Find(U("Password"));
            if Cursor/=StringStringMap_Pack.No_Element then
               Password:=StringStringMap_Pack.Element(Cursor);
               begin
                  Delete_File("MPIEXEC.PWD");
               exception
                  when others =>
                     null;
               end;
               Create
                 (File => File,
                  Mode => Out_File,
                  Name => "MPIEXEC.PWD");
               Put_Line(File,To_String(Account));
               Put_Line(File,To_String(Password));
               Close(File);
               Arguments:=Arguments&"-pwdfile MPIEXEC.PWD ";
            end if;
         end;
      end if;

      Arguments := Arguments&Item.ExecutableArguments;

      if Item.OnMessage/=null then
         Item.OnMessage("Running mpiexec with "&Arguments);
      end if;

      Item.Process.CallBackObject:=AnyObject_ClassAccess(Item);
      Item.Process.OnMessage:=ProcessMessage'Access;
      if not Item.Process.Execute
        (ProgramName => U("mpiexec"),
         Arguments   => Arguments) then
         if Item.OnFailure/=null then
            Item.OnFailure(SupplementConfig);
         end if;
      end if;

      if Item.OnMessage/=null then
         Item.OnMessage(U("Output from mpiexec:"));
      end if;

   end Execute;
   ---------------------------------------------------------------------------

   procedure CreateSpawnObject
     (Configuration : Config.Config_Type;
      Executables   : ExecutableArray_Type;
      SpawnObject   : out Spawn_ClassAccess) is

      Spawn : Spawn_Access;

   begin

      Spawn:=new Spawn_Type;
      Spawn.Configuration:=Configuration;

      for i in Executables'Range loop
         Spawn.ExecutableArguments:=Spawn.ExecutableArguments& "-n"
           &Positive'Image(Executables(i).Amount)&" "
           &Processes.CurrentPathPrefix&Executables(i).Executable
           &" -DistributedSystemsImplementation=MPI";
         if i/=Executables'Last then
            Spawn.ExecutableArguments:=Spawn.ExecutableArguments&" : ";
         end if;

      end loop;
      SpawnObject:=Spawn_ClassAccess(Spawn);

   end CreateSpawnObject;
   ---------------------------------------------------------------------------

   procedure InitializeNode
     (Configuration : out Config.Config_Type;
      Group         : DistributedSystems.Group_Type) is
      pragma Unreferenced(Group);

      use type Interfaces.C.int;

      WorldRank : aliased Interfaces.C.int;
      WorldSize : aliased Interfaces.C.int;
      Result    : Interfaces.C.int;

   begin

      Configuration.LoadFromFile
        (FileName => "_MyConfig");
      MPI_Init
        (Argc => null,
         Args => null);

      Result:=MPI_Comm_Rank
        (comm => MPI_COMM_WORLD,
         rank => WorldRank'Access);

      if Result/=MPI_SUCCESS then
         raise FailedNodeInitialization
           with "Call to MPI_Comm_Rank failed with "
             &ReturnValueToString(Result);
      end if;

      Result:=MPI_Comm_Size
        (comm => MPI_COMM_WORLD,
         size => WorldSize'Access);

      if Result/=MPI_Success then
         raise FailedNodeInitialization
           with "Call to MPI_COMM_Size failed with "
             &ReturnValueToString(Result);
      end if;

      MyGlobalID    := Node_Type(WorldRank);
      FirstGlobalID := 0;
      LastGlobalID  := Node_Type(WorldSize-1);

      Put_Line("MPI-Node initialized "&Node_Type'Image(MyGlobalID));

   end InitializeNode;
   ---------------------------------------------------------------------------

   procedure FinalizeNode is
   begin
      MPI_Finalize;
   end FinalizeNode;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (InitializeNode    => InitializeNode'Access,
      FinalizeNode      => FinalizeNode'Access,
      CreateSpawnObject => CreateSpawnObject'Access);
   Identifier : constant Unbounded_String:=U("MPI");

   procedure Register is
   begin

      Implementations.Register
        (Implementation => Implementation,
         Identifier     => Identifier);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister
        (Identifier => Identifier);

   end Unregister;
   ---------------------------------------------------------------------------

end MPI.Node;
