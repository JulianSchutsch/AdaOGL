pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;
with DistributedSystems;
with DistributedSystems.UseImplementations;
with ProgramArguments;
with Ada.Directories;

with Config;
with Basics; use Basics;

with Logging.StdOut;
with Logging.Client;

with ExceptionOutput;

with Ada.Text_IO; use Ada.Text_IO;

procedure Ps is

   Configuration           : Config.Config_Type;
   ProcessesImplementation : DistributedSystems.Implementation_Type;

   Family            : constant String:="IPv6";
   BindHost          : constant String:="::";
   RemoteHost        : constant String:="::1";
   AdminServerPort   : constant String:="10002";

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   DistributedSystems.UseImplementations.Register;
   Logging.StdOut.Register;
   Logging.Client.Register;

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Network.StreamImplementation"),
      New_Item  => U("BSDSockets"));

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Server.Network.Family"),
      New_Item  => U(Family));

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Server.Network.Host"),
      New_Item  => U(BindHost));

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Server.Network.Port"),
      New_Item  => U(AdminServerPort));

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Client.Network.Family"),
      New_Item  => U("Family"));

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Client.Network.Host"),
      New_Item  => U(RemoteHost));

   Config.Insert
     (Container => Configuration,
      Key       => U("Admin.Client.Network.Port"),
      New_Item  => U(AdminServerPort));

   Config.Insert
     (Container => Configuration,
      Key       => U("Logging.Implementation"),
      New_Item  => U("Network"));

   Configuration.Insert
     (Key      => U("Logging.Network.StreamImplementation"),
      New_Item => U("BSDSockets"));
   Configuration.Insert
     (Key      => U("Logging.Client.Network.Family"),
      New_Item => U("IPv6"));
   Configuration.Insert
     (Key      => U("Logging.Client.Network.Host"),
      New_Item => U("::1"));
   Configuration.Insert
     (Key      => U("Logging.Client.Network.Port"),
      New_Item => U("20000"));

   ProcessesImplementation:=DistributedSystems.Implementations.Find
     (ImplementationName => To_Unbounded_String("MPI"));
   -- The network for spawning isn't necessarily the same as the Control
   -- This should be a different module in the future.

   Config.Debug
     (Item => Configuration);

   Put("SpawnNodes:");
   New_Line;
   ProcessesImplementation.SpawnNodes
     (Configuration => Configuration,
      Executables =>
        ((Executable => To_Unbounded_String(Ada.Directories.Full_Name("simctr")),
          Amount     => 1),
         (Executable => To_Unbounded_String(Ada.Directories.Full_name("simreg")),
          Amount     => 1)));
   Put("Done");
   New_Line;

   Logging.Client.Unregister;
   DistributedSystems.UseImplementations.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);

end Ps;
