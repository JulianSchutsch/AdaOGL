pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ProgramArguments; use ProgramArguments;
with BSDSockets.Streams;
With ExceptionOutput;
with Logging.StdOut;

with SimAdmin;
with Config;

procedure SimDown is

   Configuration : Config.Config_Type;

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Logging.StdOut.Register;
   Configuration.LoadFromFile("_MyConfig");

   -- TEMP : Assuming Network.Processes=Local, not necessary for the
   --        future, but since this is only a temp help program ok.

   Config.Debug
     (Item => Configuration);

   SimAdmin.Initialize
     (Configuration => Configuration);

   if SimAdmin.WaitForConnection then
      SimAdmin.SendMessage
        (Message => To_Unbounded_String("AAA"));
      SimAdmin.SendShutdown;

      SimAdmin.WaitForDisconnect;
   end if;

   SimAdmin.Finalize;

exception
   when E:others =>
      ExceptionOutPut.Put(E);
end SimDown;
