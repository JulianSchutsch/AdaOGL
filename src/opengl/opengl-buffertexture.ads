package OpenGL.BufferTexture is

   FailedMap   : Exception;
   FailedUnmap : Exception;

   type BufferTexture_Type is private;

   procedure Create
     (Tex       : in out BufferTexture_Type;
      Size      : GLsizeiptr_Type;
      BasicType : GLenum_Type;
      UseHint   : GLenum_Type);

   procedure Activate
     (Tex : BufferTexture_Type;
      Unit : Integer);

   function MapWriteOnly(Tex: BufferTexture_Type) return System.Address;
   procedure Unmap(Tex: BufferTexture_Type);
   procedure Destroy(Tex: in out BufferTexture_Type);

private

   type BufferTexture_Type is
      record
         PixelBufferObject : aliased GLUInt_Type;
         Texture           : aliased GLUInt_Type;
      end record;

end OpenGL.BufferTexture;
