with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;

package body ExceptionOutput is

   procedure Put(Occurence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Put("Exception Name : " & Ada.Exceptions.Exception_Name(Occurence));
      New_Line;
      Put("Message : " & Ada.Exceptions.Exception_Message(Occurence));
      New_Line;
      Put("Traceback      :");
      New_Line;
      Put(GNAT.Traceback.Symbolic.Symbolic_TraceBack(Occurence));
      New_Line;
   end Put;

end ExceptionOutput;
