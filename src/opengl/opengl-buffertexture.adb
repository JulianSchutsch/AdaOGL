with Ada.Text_IO; use Ada.Text_IO;

package body OpenGL.BufferTexture is

   procedure Create(Tex       : in out BufferTexture_Type;
                    Size      : GLsizeiptr_Type;
                    BasicType : GLenum_Type;
                    UseHint   : GLenum_Type) is
   begin

      Put_Line("Size : "&GLsizeiptr_Type'Image(Size));
      glGenBuffers(1,Tex.PixelBufferObject'Access);
      BindTextureBuffer(Tex.PixelBufferObject);
      glBufferData(GL_TEXTURE_BUFFER,Size,System.Null_Address,UseHint);
      AssertError("glBufferData, BufferTexture.Create");

      glGenTextures(1,Tex.Texture'Access);
      glBindTexture(GL_TEXTURE_BUFFER, Tex.Texture);
      AssertError("BindTexture, BufferTexture.Create");
      glTexBuffer
        (target         => GL_TEXTURE_BUFFER,
         internalformat => BasicType,
         buffer         => Tex.PixelBufferObject);
      AssertError("glTexBuffer, BufferTexture.Create"&GLenum_Type'Image(BasicType)&":"&GLsizeiptr_Type'Image(Size));

   end Create;
   ---------------------------------------------------------------------------

   procedure Activate(Tex  : BufferTexture_Type;
                      Unit : Integer) is
   begin
      BindTexture
        (Target  => GL_TEXTURE_BUFFER,
         Unit    => Unit,
         Texture => Tex.Texture);
   end Activate;
   ---------------------------------------------------------------------------

   function MapWriteOnly(Tex : BufferTexture_Type) return System.Address is
      Result : System.Address;
      use type System.Address;
   begin
      BindTextureBuffer(Tex.PixelBufferObject);
      AssertError("BindTextureBuffer for MapWriteOnly");
      Result := glMapBuffer
        (target  => GL_TEXTURE_BUFFER,
         aaccess => GL_WRITE_ONLY);
      if Result=System.Null_Address then
         raise FailedMap with "GLError:"&GLenum_Type'Image(glGetError.all);
      end if;
      AssertError("MapWriteOnly, Post Map");
      return Result;
   end MapWriteOnly;
   ---------------------------------------------------------------------------

   procedure Destroy(Tex : in out BufferTexture_Type) is
   begin
      glDeleteTextures(1,Tex.Texture'Access);
      glDeleteBuffers(1,Tex.PixelBufferObject'Access);
   end Destroy;
   ---------------------------------------------------------------------------

   procedure Unmap(Tex : BufferTexture_Type) is
   begin
      if glUnmapBuffer(GL_TEXTURE_BUFFER)=0 then
         raise FailedUnmap;
      end if;
      BindTextureBuffer(0);
      AssertError("Unmap");
   end Unmap;
   ----------------------------------------------------------------------------

end OpenGL.BufferTexture;
