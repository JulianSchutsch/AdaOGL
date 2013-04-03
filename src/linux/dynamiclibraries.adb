pragma Ada_2012;

with Interfaces.C;

package body DynamicLibraries is

   use DynamicLibraryHandle;

   function dlopen
     (filename : access Interfaces.C.char;
      flag     : Interfaces.C.int)
      return DynamicLibraryHandle.Handle_Internal;
   pragma Import(C,dlopen,"dlopen");

   function dlsym
     (handle : DynamicLibraryHandle.Handle_Internal;
      symbol : access Interfaces.C.char)
      return System.Address;
   pragma Import(C,dlsym,"dlsym");

   function dlclose
     (handle : DynamicLibraryHandle.Handle_Internal)
      return Interfaces.C.int;
   pragma Import(C,dlclose,"dlclose");

   procedure Initialize
     (Handle : in out Handle_Type) is
   begin
      Handle.Internal:=null;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Handle : in out Handle_Type) is
   begin
      if Handle.Internal/=null then
         Handle.Close;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Open
     (Handle : in out Handle_Type;
      Name   : String) is

      CName : Interfaces.C.char_array:=Interfaces.C.To_C(Name);

   begin

      if Handle.Internal/=null then
         raise HandleReuse;
      end if;
      Handle.Internal:=dlopen
        (filename => CName(CName'First)'Access,
         flag     => 1); -- TODO: Understand this flag

   end Open;
   ---------------------------------------------------------------------------

   procedure Close
     (Handle : in out Handle_Type) is

      Error : Interfaces.C.int;
      pragma Unreferenced(Error);

   begin

      if Handle.Internal=null then
         raise HandleNotOpen;
      end if;
      Error:=dlclose(Handle.Internal);
      Handle.Internal:=null;

   end Close;
   ---------------------------------------------------------------------------

   function GetSymbol
     (Handle : in out Handle_Type;
      Name   : String)
      return System.Address is
      use type Interfaces.C.char;
      CName : Interfaces.C.char_array:=Interfaces.C.To_C(Name);
   begin
      pragma Assert(CName(CName'Last)=Interfaces.C.char'Val(0));

      if Handle.Internal=null then
         raise HandleNotOpen;
      end if;
      return dlsym
        (handle => Handle.Internal,
         symbol => CName(CName'First)'Access);

   end GetSymbol;
   ---------------------------------------------------------------------------

end DynamicLibraries;
