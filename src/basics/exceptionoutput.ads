with Ada.Exceptions;

package ExceptionOutput is
   procedure Put(Occurence : Ada.Exceptions.Exception_Occurrence);
end ExceptionOutput;
