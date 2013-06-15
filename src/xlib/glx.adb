with Ada.Text_IO; use Ada.Text_IO;
package body glX is

   function Conv is new Ada.Unchecked_Conversion(System.Address,GLXGetProcAddressARB_Access);

   function GetProcAddressARB
     (Name : String)
      return System.Address is
      use type Interfaces.C.char;
      CName : Interfaces.C.char_array:=Interfaces.C.To_C(Name);
   begin
      pragma Assert(CName(CName'Last)=Interfaces.C.char'Val(0));
      return glXGetProcAddressARB(CName(CName'First)'Access);
   end GetProcAddressARB;
   ---------------------------------------------------------------------------

   function GetProcAddress
     (Name : String)
      return System.Address is
      use type Interfaces.C.char;
      CName : interfaces.C.char_array:=Interfaces.C.To_C(Name);
   begin
      pragma Assert(CName(CName'Last)=Interfaces.C.char'Val(0));
      return glxGetProcAddress(CName(CName'First)'Access);
   end GetProcAddress;
   ---------------------------------------------------------------------------

   function QueryExtensionsString
     (Display : Display_Access;
      Screen  : Interfaces.C.int)
      return String is
   begin
      return Interfaces.C.Strings.Value(glXQueryExtensionsString
        (dpy    => Display,
         screen => Screen));
   end QueryExtensionsString;
   ---------------------------------------------------------------------------

   procedure LoadGLX
     (Display : Display_Access) is

      use type Interfaces.C.int;

   begin

      if glXQueryVersion
        (dpy   => Display,
         major => VersionMajor'Access,
         minor => VersionMinor'Access)=0 then
         raise FailedGLXLoading with "call to glXQueryVersion failed";
      end if;

      if (VersionMajor>=2) or ((VersionMajor=1) and (VersionMinor>=4)) then

         Put_Line("GetProcAddressARB");
         glXGetProcAddressARB:=Conv(GetProcAddress("glXGetProcAddressARB"&Character'Val(0)));
         if glXGetProcAddressARB=null then
            raise FailedGLXLoading with "glXGetProcAddressARB=null with glX>=1.4";
         end if;

      end if;

   end LoadGLX;
   ---------------------------------------------------------------------------

end glX;
