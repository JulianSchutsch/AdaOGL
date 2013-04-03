package body Win32.OpenGL32 is

   function wglCreateContextAttribsARB
     (hdc           : HDC_Type;
      hShareContext : HGLRC_Type;
      attriblist    : Int_Access)
      return HGLRC_Type is

      type Proc_Access is
        access function
          (hdc           : HDC_Type;
           hShareContext : HGLRC_Type;
           attriblist    : Int_Access)
           return HGLRC_Type;

      pragma Convention(StdCall,Proc_Access);

      function Conv is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Proc_Access);

      ProcName : Interfaces.C.char_array:=Interfaces.C.To_C("wglCreateContextAttribsARB");

      Proc : constant Proc_Access:=Conv(wglGetProcAddress(ProcName(ProcName'First)'Access));

   begin

      return Proc(hdc,hShareContext,attriblist);

   end wglCreateContextAttribsARB;
   ---------------------------------------------------------------------------

end Win32.OpenGL32;
