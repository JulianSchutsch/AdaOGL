with Ada.Text_IO; use Ada.Text_IO;

package body ExceptionOutput is

   procedure Put(Occurence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Put("Exception Name : " & Ada.Exceptions.Exception_Name(Occurence));
      New_Line;
      Put("Message : " & Ada.Exceptions.Exception_Message(Occurence));
      New_Line;
   end Put;

end ExceptionOutput;
