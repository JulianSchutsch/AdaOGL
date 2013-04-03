package VersionParser is

   InvalidVersionString : Exception;

   type Version_Type is array(Natural range <>) of Natural;

   -- Takes a String formatted as:
   --  Version=Number {"." Version}
   -- where Number is a small integer number with a maximum of 5 decimal
   -- digits.
   --
   -- The Limit parameter limits the number of Numbers accepted by the parser.
   -- Any content the version string is ignored if this limit is reached.
   -- A Limit=0 indicates no limit at all.
   --
   -- For incorrect version strings InvalidVersionString is raised.
   function Parse
     (StringData : String;
      Limit      : Natural:=0)
      return Version_Type;

end VersionParser;
