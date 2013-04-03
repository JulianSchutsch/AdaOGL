pragma Ada_2012;

private with DynamicLibraryHandle;
with Ada.Finalization;
with System;

package DynamicLibraries is

   HandleReuse     : Exception;
   HandleNotOpen   : Exception;
   InvalidFunction : Exception;

   type Handle_Type is new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Initialize
     (Handle : in out Handle_Type);

   overriding
   procedure Finalize
     (Handle : in out Handle_Type);

   procedure Open
     (Handle : in out Handle_Type;
      Name   : String);

   procedure Close
     (Handle : in out Handle_Type);

   function GetSymbol
     (Handle : in out Handle_Type;
      Name   : String)
      return System.Address;

private

   type Handle_Type is new Ada.Finalization.Limited_Controlled with
      record
         Internal : DynamicLibraryHandle.Handle_Internal;
      end record;

end DynamicLibraries;
