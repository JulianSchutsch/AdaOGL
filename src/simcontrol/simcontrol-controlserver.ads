pragma Ada_2005;

with Config;

package SimControl.ControlServer is

   procedure Initialize
      (Configuration : Config.Config_Type);

   procedure Finalize;

end SimControl.ControlServer;
