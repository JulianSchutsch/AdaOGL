package DynamicLibraryHandle is

   type Handle_InternalRec is null record;
   type Handle_Internal is access all Handle_InternalRec;

end DynamicLibraryHandle;
