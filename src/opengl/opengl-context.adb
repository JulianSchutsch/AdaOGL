-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

pragma Ada_2005;

with System;
with Canvas;
with Boundscalc; use Boundscalc;
with GUIDefinitions; use GUIDefinitions;
with Ada.Text_IO; use Ada.Text_IO;

package body OpenGL.Context is

   procedure NewCanvas
     (Context : in out Context_Type;
      Object  : Object_ClassAccess;
      Height  : Integer;
      Width   : Integer;
      Canvas  : out Canvas_ClassAccess) is
      pragma Unreferenced(Context);

      NewCanv : Canvas_Access;
   begin

      NewCanv        := new Canvas_Type;
      Canvas:=Canvas_ClassAccess(NewCanv);

      if (Height<=0)
        or (Width<=0) then
         return;
      end if;

      NewCanv.Initialize
        (Height => Height,
         Width  => Width);

      glGentextures
        (n => 1,
         textures => NewCanv.TextureID'Access);
      glBindTexture
        (target  => GL_TEXTURE_2D,
         texture => NewCanv.TextureID);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_MIN_FILTER,
         param  => GL_NEAREST);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_MAG_FILTER,
         param  => GL_NEAREST);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_WRAP_S,
         param  => GL_CLAMP);
      glTexParameteri
        (target => GL_TEXTURE_2D,
         pname  => GL_TEXTURE_WRAP_T,
         param  => GL_CLAMP);

      glTexImage2D
        (target => GL_TEXTURE_2D,
         level  => 0,
         internalFormat => GL_RGBA,
         width  => GLsizei_Type(NewCanv.ContentWidth),
         height => GLsizei_Type(NewCanv.ContentHeight),
         border => 0,
         format => GL_BGRA,
         ttype  => GL_UNSIGNED_BYTE,
         data   => System.Null_Address);

      Standard.OpenGL.AssertError("NewCanvas");

      GUI.AddCanvasByContext
        (Object => Object,
         Canvas => Canvas_ClassAccess(NewCanv));

      NewCanv.Initialized:=True;

   end NewCanvas;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Canvas : access Canvas_Type) is
   begin

      if Canvas.Initialized then

         glDeleteTextures
           (n        => 1,
            textures => Canvas.TextureID'Access);

         GUI.Canvas_Access(Canvas).Finalize;

      end if;

      GUI.Canvas_Access(Canvas).Finalize;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Paint
     (Context : in out Context_Type) is

      procedure SetupView is
      begin
         glViewPort
           (x      => 0,
            y      => 0,
            width  => GLsizei_Type(Context.Bounds.Width),
            height => GLsizei_Type(Context.Bounds.Height));

         glMatrixMode(GL_PROJECTION);
         glLoadIdentity;
         glOrtho
           (left    => 0.0,
            right   => GLdouble_Type(Context.Bounds.Width),
            bottom  => GLdouble_Type(Context.Bounds.Height),
            top     => 0.0,
            nearVal => -1.0,
            farVal  => 1.0);

         glMatrixMode(GL_MODELVIEW);
         glLoadIdentity;

         glDisable(GL_SCISSOR_TEST);
         glDisable(GL_DEPTH_TEST);
         glEnable(GL_TEXTURE_2D);
         glBlendFunc
           (sfactor => GL_SRC_ALPHA,
            dfactor => GL_ONE_MINUS_SRC_ALPHA);
         glAlphaFunc
           (func => GL_GREATER,
            ref  => 0.1);
         glEnable(GL_BLEND);
         glColor4f(1.0,1.0,1.0,1.0);
      end SetupView;
      ------------------------------------------------------------------------

      procedure ProcessTree
        (Object : Object_ClassAccess) is

         use type Canvas.Image_Access;

         p : Object_ClassAccess;

         ObjectAbsBounds : AbsBounds_Type;
         CanvasAbsBounds : AbsBounds_Type;
         CanvasBounds    : Bounds_Type;
         CanvasCursor    : Canvas_Access;

      begin

         p := Object;

         ObjectLoop:
         while p/=null loop

            case p.Render is

               when RenderCanvasse =>

                  ObjectAbsBounds:=p.AbsBounds;

                  CanvasCursor:=Canvas_Access(p.GetFirstCanvas);

                  CanvasLoop:
                  while CanvasCursor/=null loop

                     glBindTexture
                       (target  => GL_TEXTURE_2D,
                        texture => CanvasCursor.TextureID);

                     if CanvasCursor.Modified
                       and CanvasCursor.Image/=null then

                        CanvasCursor.Modified:=False;

                        -- TODO: Replace by SubTexImage
                        glTexImage2D
                          (target => GL_TEXTURE_2D,
                           level  => 0,
                           internalFormat => GL_RGBA,
                           width  => GLsizei_Type(CanvasCursor.ContentWidth),
                           height => GLsizei_Type(CanvasCursor.ContentHeight),
                           border => 0,
                           format => GL_BGRA,
                           ttype  => GL_UNSIGNED_BYTE,
                           data   => CanvasCursor.Image(0,0)'Address);
                           Standard.OpenGL.AssertError("Upload Image");

                     end if;

                     CanvasBounds:=CanvasCursor.GetBounds;
                     NestBounds
                       (ParentAbsBounds => ObjectAbsBounds,
                        RectBounds      => CanvasBounds,
                        ResultBounds    => CanvasAbsBounds);

                     if CanvasAbsBounds.AbsVisible then

                        declare
                           Texx1 : constant GLfloat_Type
                             :=GLfloat_Type(CanvasAbsBounds.AbsSubLeft)
                             /GLfloat_Type(CanvasBounds.Width);

                           Texx2 : constant GLfloat_Type
                             :=(GLfloat_Type(CanvasAbsBounds.AbsSubLeft)
                                +GLfloat_Type(CanvasAbsBounds.AbsWidth))
                             /GLfloat_Type(CanvasBounds.Width);

                           Texy1 : constant GLfloat_Type
                             :=GLfloat_Type(CanvasAbsBounds.AbsSubTop)
                             /GLfloat_Type(CanvasBounds.Height);

                           Texy2 : constant GLfloat_Type
                             :=(GLfloat_Type(CanvasAbsBounds.AbsSubTop)
                                +GLfloat_Type(CanvasAbsBounds.AbsHeight))
                             /GLfloat_Type(CanvasBounds.Height);

                        begin
                           glBegin(GL_QUADS);
                           glTexCoord2f(Texx1,Texy1);
                           glVertex2f
                             (GLfloat_Type(CanvasAbsBounds.AbsLeft),
                              GLfloat_Type(CanvasAbsBounds.AbsTop));
                           glTexCoord2f(Texx1,Texy2);
                           glVertex2f
                             (GLfloat_Type(CanvasAbsBounds.AbsLeft),
                              GLfloat_Type(CanvasAbsBounds.AbsTop+CanvasAbsBounds.AbsHeight));
                           glTexCoord2f(Texx2,Texy2);
                           glVertex2f
                             (GLfloat_Type(CanvasAbsBounds.AbsLeft+CanvasAbsBounds.AbsWidth),
                              GLfloat_Type(CanvasAbsBounds.AbsTop+CanvasAbsBounds.AbsHeight));
                           glTexCoord2f(Texx2,Texy1);
                           glVertex2f
                             (GLfloat_Type(CanvasAbsBounds.AbsLeft+CanvasAbsBounds.AbsWidth),
                              GLfloat_Type(canvasAbsBounds.AbsTop));
                           glEnd;
                        end;

                     end if;

                     CanvasCursor:=Canvas_Access
                       (GUI.Canvas_ClassAccess(CanvasCursor).GetNextCanvas);

                  end loop CanvasLoop;
                  ------------------------------------------------------------

               when RenderCustom =>

                  p.RenderCustom;
                  SetupView;
                  ------------------------------------------------------------

            end case;

            p:=p.GetLastTreeObject;

         end loop ObjectLoop;

      end ProcessTree;
      ------------------------------------------------------------------------

   begin
      if (Context.Bounds.Width<=0)
        or (Context.Bounds.Height<=0) then
         return;
      end if;

      SetupView;

      glClearColor
        (red   => 0.0,
         green => 0.0,
         blue  => 0.0,
         alpha => 1.0);

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      ProcessTree(Context.BasisArea);
      ProcessTree(Context.WindowArea);
      ProcessTree(Context.ModalArea);
      ProcessTree(Context.ContextArea);
      Standard.OpenGL.AssertError("Paint");
   end Paint;
   ---------------------------------------------------------------------------

end OpenGL.Context;
