package body Basics is

   function ReturnSomeString
      return Unbounded_String is
   begin
      return To_Unbounded_String("TestString");
   end ReturnSomeString;
   ---------------------------------------------------------------------------

end Basics;
