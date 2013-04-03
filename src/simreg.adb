with Config;

with BSDSockets.Streams;

with ProgramArguments;
with SimRegion;
with DistributedSystems.UseImplementations;
with DistributedSystems;

with ExceptionOutput;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Logging.StdOut;
with Logging.Client;

--with Ada.Text_IO; use Ada.Text_IO;

procedure simreg is

   Configuration : Config.Config_Type;
   DistributedSystemsImpl : DistributedSystems.Implementation_Type;

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Logging.StdOut.Register;
   Logging.Client.Register;
   DistributedSystems.UseImplementations.Register;

   ProgramArguments.Debug;

   DistributedSystemsImpl
     :=DistributedSystems.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        Node          => To_Unbounded_String("Arguments"));

   DistributedSystemsImpl.InitializeNode
     (Configuration => Configuration,
      Group         => 0); -- TODO: Not yet valid GroupID

   SimRegion.Initialize
     (Configuration => Configuration);

   loop
      exit when SimRegion.Process;
   end loop;

   SimRegion.Finalize;

   Logging.Client.Unregister;
   DistributedSystemsImpl.FinalizeNode.all;

   DistributedSystems.UseImplementations.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
      loop
         null;
      end loop;
end simreg;
