pragma Ada_2005;

with Logging.Server;
with Config;
with ProcessLoop;
with Basics; use Basics;
with BSDSockets.Streams;
with Ada.Text_IO; use Ada.Text_IO;

procedure Logserver is

   Configuration : Config.Config_Type;
   Char : Character;
   Available : Boolean;

begin

   BSDSockets.Streams.Register;

   Configuration.Insert
     (Key      => U("Logging.Network.StreamImplementation"),
      New_Item => U("BSDSockets"));
   Configuration.Insert
     (Key      => U("Logging.Server.Network.Family"),
      New_Item => U("IPv6"));
   Configuration.Insert
     (Key      => U("Logging.Server.Network.Host"),
      New_Item => U("::"));
   Configuration.Insert
     (Key      => U("Logging.Server.Network.Port"),
      New_Item => U("20000"));

   Logging.Server.Initialize
     (Configuration => Configuration);

   Put("Press Escape key to leave program.");
   New_Line;

   loop

      Get_Immediate
        (Item => Char,
         Available => Available);

      if Available then
         case Char is
            when Character'Val(27) =>
               exit;

            when others =>
               null;

         end case;

      end if;

      ProcessLoop.Process;
   end loop;

   Logging.Server.Finalize;
   BSDSockets.Streams.Unregister;

end Logserver;
