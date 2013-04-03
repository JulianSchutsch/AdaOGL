package Win32.Ole32 is

   function CoCreateGuid
     (pguid : access GUID_Type)
      return HRESULT_Type;
   pragma Import(StdCall,CoCreateGuid,"CoCreateGuid");

end Win32.Ole32;
