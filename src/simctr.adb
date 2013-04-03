with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;

with Config;
with ProgramArguments;
with SimControl;
with Logging.StdOut;
with Logging.Client;
with ExceptionOutput;
with DistributedSystems;
with DistributedSystems.UseImplementations;

--with Ada.Text_IO; use Ada.Text_IO;

procedure SimCtr is

   Configuration           : Config.Config_Type;
   DistributedSystemsImpl  : DistributedSystems.Implementation_Type;

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   DistributedSystems.UseImplementations.Register;
   Logging.StdOut.Register;
   Logging.Client.Register;

   DistributedSystemsImpl
     := DistributedSystems.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        Node          => To_Unbounded_String("Arguments"));

   DistributedSystemsImpl.InitializeNode
     (Configuration => Configuration,
      Group         => 0); -- TODO:Group ID not yet valid

   SimControl.Initialize
     (Configuration =>  Configuration);

   loop
      exit when SimControl.Process;
   end loop;

   Logging.Client.Unregister;
   DistributedSystemsImpl.FinalizeNode.all;
   SimControl.Finalize;
   DistributedSystems.UseImplementations.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
      loop
         null;
      end loop;
end SimCtr;
