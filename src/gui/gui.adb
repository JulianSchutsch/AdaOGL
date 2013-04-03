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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body GUI is

   procedure FreeObject is new Ada.Unchecked_Deallocation
     (Object => Object_Type'Class,
      Name   => Object_ClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Canvas_Type'Class,
      Name   => Canvas_ClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Context_Type'Class,
      Name   => Context_ClassAccess);
   ---------------------------------------------------------------------------

   procedure AddASync
     (Item   : access Object_Type;
      Method : OnASync_Access) is
   begin
      Item.Context.Priv.ASyncCalls.Append
        (New_Item =>
           (Method => Method,
            Object => Object_ClassAccess(Item)));
   end AddASync;
   ---------------------------------------------------------------------------

   procedure CallAsyncs
     (Context : Context_ClassAccess) is

      use type AsyncList_Pack.Cursor;

      Cursor : ASyncList_Pack.Cursor;
      Method : ASync_Type;

   begin

      Cursor:=Context.Priv.ASyncCalls.First;
      while Cursor/=ASyncList_Pack.No_Element loop
         Method:=ASyncList_Pack.Element(Cursor);
         Method.Method(Method.Object);
         Cursor:=ASyncList_Pack.Next(Cursor);
      end loop;
      Context.Priv.ASyncCalls.Clear;

   end CallAsyncs;
   ---------------------------------------------------------------------------

   type ModalArea_Type is new Object_Type with
      record
         null;
      end record;

   overriding
   function MouseDown
     (Item   : access ModalArea_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access ModalArea_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is
      pragma Unreferenced(Button);
      pragma Unreferenced(X);
      pragma Unreferenced(Y);
   begin
      return Item.FirstChild/=null;
   end MouseDown;
   ---------------------------------------------------------------------------

   type ContextArea_Type is new Object_Type with
      record
         null;
      end record;

   overriding
   function MouseDown
     (Item   : access ContextArea_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item : access ContextArea_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(Button);
      pragma Unreferenced(X);
      pragma Unreferenced(Y);

   begin
      -- Check if Context Mode is active
      if Item.FirstChild/=null then
         if Item.Context.OnContextAreaClick/=null then
            Item.Context.OnContextAreaClick(Item.CallBackObject);
            Item.Context.OnContextAreaClick:=null;
         end if;
         return True;
      end if;
      return False;
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure SetContextAreaClick
     (Item           : access Object_Type;
      CallBack       : OnContextAreaClick_Access;
      CallBackObject : AnyObject_ClassAccess) is
   begin
      Item.Context.OnContextAreaClick         := CallBack;
      Item.Context.ContextArea.CallBackObject := CallBackObject;
   end SetContextAreaClick;
   ---------------------------------------------------------------------------

   function GetContextArea
     (Item : access Object_Type)
      return Object_ClassAccess is
   begin
      return Item.Context.ContextArea;
   end GetContextArea;
   ---------------------------------------------------------------------------

   function GetCanvasCount
     (Item : access Object_Type)
      return Integer is

      Cursor : Canvas_ClassAccess;
      Count  : Integer:=0;

   begin
      Cursor:=Item.Canvasse;
      while Cursor/=null loop
         Count  := Count+1;
         Cursor := Cursor.Next;
      end loop;
      return Count;
   end GetCanvasCount;
   ---------------------------------------------------------------------------

   function NewCanvas
     (Item   : access Object_Type;
      Height : Integer;
      Width  : Integer)
      return Canvas_ClassAccess is

      NewCanvas : Canvas_ClassAccess;

   begin

      Item.Context.NewCanvas
        (Object => Object_ClassAccess(Item),
         Height => Height,
         Width  => Width,
         Canvas => NewCanvas);

      return NewCanvas;

   end NewCanvas;
   ---------------------------------------------------------------------------

   procedure FreeContext
     (Context : in out Context_ClassAccess) is
   begin

      if Context/=null then
         Context.Finalize;
         Free(Context);
      end if;

   end FreeContext;
   ---------------------------------------------------------------------------

   procedure FreeCanvas
     (Canvas : in out Canvas_ClassAccess) is
   begin

      if Canvas/=null then
         Canvas.Finalize;
         Free(Canvas);
      end if;

   end FreeCanvas;
   ---------------------------------------------------------------------------

   function GetFirstCanvas
     (Item : access Object_Type)
      return Canvas_ClassAccess is
   begin
      return Item.Canvasse;
   end GetFirstCanvas;
   ---------------------------------------------------------------------------

   function GetBounds
     (Canvas : access Canvas_Type)
      return Bounds_Type is
   begin
      return Canvas.Bounds;
   end GetBounds;
   ---------------------------------------------------------------------------

   function GetLastTreeObject
     (Item : access Object_Type)
      return Object_ClassAccess is
      Result : Object_ClassAccess;

   begin
      if Item.LastChild/=null then
         return Item.LastChild;
      else
         Result:=Object_ClassAccess(Item);
         while (Result/=null) and then (Result.Last=null) loop
            Result:=Result.Parent;
         end loop;
         if Result/=null then
            Result:=Result.Last;
         end if;
         return Result;
      end if;
   end GetLastTreeObject;
   ---------------------------------------------------------------------------

   function GetNextCanvas
     (Canvas : access Canvas_Type)
      return Canvas_ClassAccess is
   begin
      return Canvas.Next;
   end GetNextCanvas;
   ---------------------------------------------------------------------------

   procedure SetFocusObject
     (Item   : access Object_Type;
      Object : Object_ClassAccess) is
   begin
      Item.FocusObject:=Object;
   end SetFocusObject;
   ---------------------------------------------------------------------------

   procedure BringToFront
     (Canvas : access Canvas_Type) is

      P : Canvas_ClassAccess;

   begin
      if Canvas.Next=null then
         return;
      end if;

      P:=Canvas_ClassAccess(Canvas);
      while P.Next/=null loop
         P := P.Next;
      end loop;

      if Canvas.Last/=null then
         Canvas.Last.Next:=Canvas.Next;
      else
         Canvas.Object.Canvasse:=Canvas.Next;
      end if;

      Canvas.Next.Last := Canvas.Last;

      Canvas.Next := null;
      Canvas.Last := P;
      Canvas.Last.Next := Canvas_ClassAccess(Canvas);

   end BringToFront;
   ---------------------------------------------------------------------------

   procedure BringToFront
     (Item : Object_ClassAccess) is
   begin
      if Item.Last=null then
         return;
      end if;

      if Item.Next/=null then
         Item.Next.Last:=Item.Last;
      else
         Item.Parent.LastChild:=Item.Last;
      end if;

      Item.Last.Next := Item.Next;

      Item.Next      := Item.Parent.FirstChild;
      Item.Last      := null;
      Item.Next.Last := Item;
      Item.Parent.FirstChild := Item;
   end BringToFront;
   ---------------------------------------------------------------------------

   procedure SetClient
     (Object : access Object_Type;
      Client : Object_ClassAccess) is
   begin
      Object.Client:=Client;
   end SetClient;
   ---------------------------------------------------------------------------

   function GetClient
     (Object : access Object_Type)
      return Object_ClassAccess is
   begin
      return Object.Client;
   end GetClient;
   ---------------------------------------------------------------------------

   procedure ContextCharacterInput
     (Context : Context_ClassAccess;
      Chars   : Unbounded_String) is

   begin

      Put_Line("CharacterSend:"&Integer'Image(Length(Chars)));
      Put_Line(To_String(Chars));

      if Context.Priv.FocusObject/=null then
         if not Context.Priv.FocusObject.CharacterInput(Chars) then
            Put("Character rejected:");
            Put(To_String(Chars));
            New_Line;
         end if;
      end if;

      CallAsyncs(Context);

   end ContextCharacterInput;
   ---------------------------------------------------------------------------

   procedure ContextKeyDown
     (Context : Context_ClassAccess;
      Key     : Key_Enum) is
   begin

      if Context.Priv.FocusObject/=null then
         if not Context.Priv.FocusObject.KeyDown(Key) then
            Put("Key rejected");
            New_Line;
         end if;
      end if;

      CallAsyncs(Context);

   end ContextKeyDown;
   ---------------------------------------------------------------------------

   procedure ContextKeyUp
     (Context : Context_ClassAccess;
      Key     : Key_Enum) is
   begin

      if Context.Priv.FocusObject/=null then
         if not Context.Priv.FocusObject.KeyUp(Key) then
            Put("Key up rejected");
            New_Line;
         end if;
      end if;

      CallAsyncs(Context);

   end ContextKeyUp;
   ---------------------------------------------------------------------------

   procedure ClearFocusTree
     (Object : Object_ClassAccess) is

      Cursor : Object_ClassAccess;

   begin

      Cursor:=Object.Context.Priv.FocusObject;

      if Cursor/=null then
         Put_Line("ClearFocusTree");
      end if;

      while Cursor/=null loop

         if (Cursor.FocusStyle=FocusStyleContainer) and
           (Cursor.FocusObject=Object.Context.Priv.FocusObject) then

            Cursor.FocusObject:=null;

         end if;

         Cursor.Focussed:=False;
         Cursor.Defocus;

         Cursor:=Cursor.Parent;

      end loop;

      Object.Context.Priv.FocusObject:=null;

   end ClearFocusTree;
   ---------------------------------------------------------------------------

   procedure SetFocusTree
     (Object : Object_ClassAccess) is

         Cursor : Object_ClassAccess;

   begin

      ClearFocusTree(Object);

      Object.Context.Priv.FocusObject:=Object;
      Cursor:=Object;

      while Cursor/=null loop

         if (Cursor.FocusStyle=FocusStyleContainer)
           and (Cursor/=Object) then

            Cursor.FocusObject:=Object;

         end if;

         Cursor.Focussed:=True;
         Cursor.Focus;

         Cursor:=Cursor.Parent;

      end loop;

   end;
   ---------------------------------------------------------------------------

   procedure SetFocus
     (Item : access Object_Type) is

      Cursor : Object_ClassAccess;

   begin

      Cursor:=Object_ClassAccess(Item);

      while (Cursor/=null) loop
         Put(Cursor.all'Address);
         New_Line;

         case Cursor.FocusStyle is

            when FocusStyleNone =>
               Cursor:=Cursor.Parent;

            when FocusStyleAccept =>
               SetFocusTree(Cursor);
               return;

            when FocusStyleContainer =>

               if Cursor.FocusObject/=null then
                  Cursor:=Cursor.FocusObject;
               else
                  SetFocusTree(Cursor);
                  return;
               end if;

            when FocusStyleRedirect =>
               if Cursor.FocusObject=null then
                  raise FocusRedirectionToNull;
               end if;
               SetFocusTree(Cursor.FocusObject);
               return;

         end case;

      end loop;

   end;
   ---------------------------------------------------------------------------

   function KeyDown
     (Item : access Object_Type;
      Key  : Key_Enum)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Key);

   begin
      return True;
   end KeyDown;
   ---------------------------------------------------------------------------

   function KeyUp
     (Item : access Object_Type;
      Key  : Key_Enum)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Key);

   begin
      return True;
   end KeyUp;
   ---------------------------------------------------------------------------

   function CharacterInput
     (Item  : access Object_Type;
      Chars : Unbounded_String)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Chars);

   begin
      return False;
   end CharacterInput;

   function MouseDown
     (Item   : access Object_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Button);
      pragma Unreferenced(X);
      pragma Unreferenced(Y);

   begin
      return False;
   end MouseDown;
   ---------------------------------------------------------------------------

   -- Global Mousedown procedure which either delivers the mouse signal to a
   -- previously selected object (mouse select) or finds a candidate to be
   -- selected.
   procedure ContextMouseDown
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is

      function SubMouseDown
        (Object : Object_ClassAccess)
         return Boolean is

         ObjectCursor    : Object_ClassAccess;
         MouseEventTaken : Boolean;

      begin

         if TestInsideAbsBounds
           (AbsBounds => Object.AbsBounds,
            AbsX      => AbsX,
            AbsY      => AbsY) then

            -- Test if any nested object wants the mouse signal
            ObjectCursor:=Object.FirstChild;

            while ObjectCursor/=null loop
               if SubMouseDown(ObjectCursor) then
                  return True;
               end if;
               ObjectCursor:=ObjectCursor.Next;
            end loop;

            -- Test if this object wants the mouse signal
            declare
               Bounds : AbsBounds_Type renames
                 Object.AbsBounds;
            begin
               MouseEventTaken:=Object.MouseDown
                 (Button => MouseButton,
                  X      => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
                  Y      => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
            end;

            if MouseEventTaken then

               Context.Priv.MouseSelection := Object;
               return True;

            end if;

         end if;

         return False;

      end SubMouseDown;
      ------------------------------------------------------------------------

   begin

      if Context.Priv.MouseButtonsPressed=NoMouseButtons then

         Context.Priv.MouseSelection:=null;

         declare
            Result : Boolean;
            pragma Unreferenced(Result);
         begin
            if not SubMouseDown(Context.ContextArea) then
               if not SubMouseDown(Context.ModalArea) then
                  if not SubMouseDown(Context.WindowArea) then
                     Result:=SubMouseDown(Context.BasisArea);
                  end if;
               end if;
            end if;
         end;

         if Context.Priv.MouseSelection/=null then
            Context.Priv.MouseSelection.SetFocus;
         end if;

      else

         if Context.Priv.MouseSelection/=null then

            declare
               MouseEventTaken : Boolean;
               pragma Unreferenced(MouseEventTaken);
            begin

               declare
                  Bounds : AbsBounds_Type renames
                    Context.Priv.MouseSelection.AbsBounds;
               begin
                  MouseEventTaken:=Context.Priv.MouseSelection.MouseDown
                    (Button => MouseButton,
                     X      => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
                     Y      => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
               end;

            end;

         end if;

      end if;

      Context.Priv.MouseButtonsPressed(MouseButton):=True;

      CallAsyncs(Context);

   end ContextMouseDown;
   ---------------------------------------------------------------------------

   procedure ContextMouseUp
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is

   begin

      if Context.Priv.MouseSelection/=null then

         declare
            Bounds : AbsBounds_Type renames
              Context.Priv.MouseSelection.AbsBounds;
         begin
            Context.Priv.MouseSelection.MouseUp
              (Button => MouseButton,
               X      => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
               Y      => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
         end;
      end if;

      Context.Priv.MouseButtonsPressed(MouseButton):=False;
      if Context.Priv.MouseButtonsPressed=NoMouseButtons then
         Context.Priv.MouseSelection:=null;
      end if;

      CallAsyncs(Context);

   end ContextMouseUp;
   ---------------------------------------------------------------------------

   procedure ContextMouseMove
     (Context : Context_ClassAccess;
      AbsX    : Integer;
      AbsY    : Integer) is
   begin

      if Context.Priv.MouseSelection/=null then

         declare
            Bounds : AbsBounds_Type renames
              Context.Priv.MouseSelection.AbsBounds;
         begin
            Context.Priv.MouseSelection.MouseMove
              (X => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
               Y => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
         end;

      end if;

      CallAsyncs(Context);

   end ContextMouseMove;
   ---------------------------------------------------------------------------

   function GetAbsBounds
     (Object : access Object_Type)
      return AbsBounds_Type is
   begin
      return Object.AbsBounds;
   end GetAbsBounds;
   ---------------------------------------------------------------------------

   procedure ResizeTree
     (Object : Object_ClassAccess) is

      ObjectCursor : Object_ClassAccess;
      CanvasCursor : Canvas_ClassAccess;

   begin

      if Object.Parent/=null then

         RestoreAnchors
           (Anchors      => Object.Anchors,
            ClientBounds => Object.Bounds,
            ParentBounds => Object.Parent.Bounds);

         ApplyConstraint
           (Constraint => Object.TopHeightConstraint,
            Value      => Object.Bounds.Top,
            Size       => Object.Bounds.Height,
            OldValue   => Object.PrevBounds.Top,
            OldSize    => Object.PrevBounds.Height,
            ParentSize => Object.Parent.Bounds.Height);
         ApplyConstraint
           (Constraint => Object.LeftWidthConstraint,
            Value      => Object.Bounds.Left,
            Size       => Object.Bounds.Width,
            OldValue   => Object.PrevBounds.Left,
            OldSize    => Object.PrevBounds.Width,
            ParentSize => Object.Parent.Bounds.Width);

         NestBounds
           (ParentAbsBounds => Object.Parent.AbsBounds,
            RectBounds      => Object.Bounds,
            ResultBounds    => Object.AbsBounds);
      else
         Object.AbsBounds:=
           (AbsTop     => Object.Bounds.Top,
            AbsLeft    => Object.Bounds.Left,
            AbsHeight  => Object.Bounds.Height,
            AbsWidth   => Object.Bounds.Width,
            AbsSubTop  => 0,
            AbsSubLeft => 0,
            AbsVisible => Object.Bounds.Visible);
      end if;
      ------------------------------------------------------------------------

      CanvasCursor:=Object.Canvasse;

      while CanvasCursor/=null loop
         RestoreAnchors
           (Anchors      => CanvasCursor.Anchors,
            ClientBounds => CanvasCursor.Bounds,
            ParentBounds => Object.Bounds);
         CanvasCursor:=CanvasCursor.Next;
      end loop;
      ------------------------------------------------------------------------

      ObjectCursor:=Object.FirstChild;

      while ObjectCursor/=null loop
         ResizeTree(ObjectCursor);
         ObjectCursor:=ObjectCursor.Next;
      end loop;
      ------------------------------------------------------------------------

      if (Object.Bounds.Height/=Object.PrevBounds.Height)
        or (Object.Bounds.Width/=Object.PrevBounds.Width) then

         Object.Resize;
         if Object.OnResize/=null then
            Object.OnResize(Object.CallBackObject);
         end if;

      end if;

      Object.PrevBounds := Object.Bounds;

   end ResizeTree;
   ---------------------------------------------------------------------------

   procedure PropagateContextResize
     (Context : Context_ClassAccess;
      Height  : Integer;
      Width   : Integer) is
   begin

      Context.Bounds:=
        (Top     => 0,
         Left    => 0,
         Height  => Height,
         Width   => Width,
         Visible => True);

      if Context.BasisArea/=null then
         Context.BasisArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;
      if Context.WindowArea/=null then
         Context.WindowArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;
      if Context.ModalArea/=null then
         Context.ModalArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;
      if Context.ContextArea/=null then
         Context.ContextArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;

   end PropagateContextResize;
   ---------------------------------------------------------------------------

   procedure SetAnchors
     (Object : access Object_Type;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean) is
   begin
      Object.Anchors.Top    := Top;
      Object.Anchors.Left   := Left;
      Object.Anchors.Right  := Right;
      Object.Anchors.Bottom := Bottom;

      if Object.Parent/=null then
         StoreAnchors
           (Anchors      => Object.Anchors,
            ClientBounds => Object.Bounds,
            ParentBounds => Object.Parent.Bounds);
      end if;

   end SetAnchors;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Canvas  : access Canvas_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean) is

   begin

      if Canvas.Object=null then
         return;
      end if;

      Canvas.Bounds:=
        (Top     => Top,
         Left    => Left,
         Height  => Height,
         Width   => Width,
         Visible => Visible);

      StoreAnchors
        (Anchors => Canvas.Anchors,
         ClientBounds => Canvas.Bounds,
         ParentBounds => Canvas.Object.Bounds);

   end SetBounds;
   ---------------------------------------------------------------------------

   procedure SetAnchors
     (Canvas : access Canvas_Type;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean) is

   begin

      if Canvas.Object=null then
         return;
      end if;

      Canvas.Anchors.Top    := Top;
      Canvas.Anchors.Left   := Left;
      Canvas.Anchors.Right  := Right;
      Canvas.Anchors.Bottom := Bottom;

      StoreAnchors
        (Anchors      => Canvas.Anchors,
         ClientBounds => Canvas.Bounds,
         ParentBounds => Canvas.Object.Bounds);

   end SetAnchors;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Object  : access Object_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean) is
   begin

      Object.Bounds:=
        (Top     => Top,
         Left    => Left,
         Height  => Height,
         Width   => Width,
         Visible => Visible);

      ResizeTree(Object_ClassAccess(Object));

   end SetBounds;
   ---------------------------------------------------------------------------

   function GetClientBounds
     (Object : access Object_Type)
      return Bounds_Type is

      Obj : Object_ClassAccess:=Object_ClassAccess(Object);

   begin
      while Obj.Client/=null loop
         Obj:=Obj.Client;
      end loop;
      return Obj.Bounds;
   end GetClientBounds;
   ---------------------------------------------------------------------------

   function GetPrevBounds
     (Object : access Object_Type)
      return Bounds_Type is
   begin
      return Object.PrevBounds;
   end GetPrevBounds;
   ---------------------------------------------------------------------------

   function GetBounds
     (Object : access Object_Type)
      return Bounds_Type is
   begin
      return Object.Bounds;
   end GetBounds;
   ---------------------------------------------------------------------------

   procedure AddCanvasByContext
     (Object : Object_ClassAccess;
      Canvas : Canvas_ClassAccess) is
   begin

      Canvas.Next:=Object.Canvasse;
      if Canvas.Next/=null then
         Canvas.Next.Last:=Canvas;
      end if;
      Object.Canvasse:=Canvas;

      Canvas.Object:=Object;

   end AddCanvasByContext;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Canvas : access Canvas_Type) is

   begin

      -- This case may happen if some asks for a "dummy" canvas with
      -- zero or negative dimensions
      if Canvas.Object=null then
         return;
      end if;

      if Canvas.Next/=null then
         Canvas.Next.Last:=Canvas.Last;
      end if;
      if Canvas.Last/=null then
         Canvas.Last.Next:=Canvas.Next;
      else
         Canvas.Object.Canvasse:=Canvas.Next;
      end if;

      Standard.Canvas.Canvas_Access(Canvas).Finalize;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item   : access Object_Type;
      Parent : Object_ClassAccess) is

     Par : Object_ClassAccess;

   begin
      Par := Parent;
      if Par=null then
         return;
      end if;

      while Par.Client/=null loop
         Par:=Par.Client;
      end loop;

      Item.Parent:=Par;

      Item.Context := Par.Context;

      Item.Next:=Par.FirstChild;

      if Item.Next/=null then
         Item.Next.Last:=Object_ClassAccess(Item);
      else
         Par.LastChild:=Object_ClassAccess(Item);
      end if;

      Par.FirstChild:=Object_ClassAccess(Item);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Object_Type) is

      ItemVal : Object_ClassAccess:=Object_ClassAccess(Item);

   begin

      -- TODO: This is not enough, there can be referenced through containers
      -- be left which MUST be removed too
      if Item.Context.Priv.FocusObject=Object_ClassAccess(Item) then
         ClearFocusTree(Object_ClassAccess(Item));
      end if;
      if Item.Context.Priv.MouseSelection=ItemVal then
         Item.Context.Priv.MouseSelection:=null;
      end if;

      -- Finalize all Childs
      declare
         Object     : Object_ClassAccess;
         NextObject : Object_ClassAccess;
      begin
         Object:=Item.FirstChild;

         while Object/=null loop
            NextObject:=Object.Next;
            Object.Free;
            Object:=NextObject;
         end loop;
      end;

      -- Free all Canvases
      declare
         Canvas     : Canvas_ClassAccess;
         NextCanvas : Canvas_ClassAccess;
      begin
         Canvas := Item.Canvasse;

         while Canvas/=null loop
            NextCanvas:=Canvas.Next;
            FreeCanvas(Canvas);
            Canvas:=NextCanvas;
         end loop;
      end;

      -- Remove Object from the tree
      if Item.Parent/=null then
         if Item.Next/=null then
            Item.Next.Last:=Item.Last;
         else
            Item.Parent.LastChild:=Item.Last;
         end if;

         if Item.Last/=null then
            Item.Last.Next:=Item.Next;
         else
            Item.Parent.FirstChild:=Item.Next;
         end if;
      end if;

      FreeObject(ItemVal);

   end Free;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Context : access Context_Type) is
   begin
      -- TODO: Use specialized objects
      Context.BasisArea           := new Object_Type;
      Context.WindowArea          := new Object_Type;
      Context.ModalArea           := new ModalArea_Type;
      Context.ContextArea         := new ContextArea_Type;
      Context.BasisArea.Context   := Context_ClassAccess(Context);
      Context.WindowArea.Context  := Context_ClassAccess(Context);
      Context.ModalArea.Context   := Context_ClassAccess(Context);
      Context.ContextArea.Context := Context_ClassAccess(Context);
      PropagateContextResize
        (Context => Context_ClassAccess(Context),
         Height  => Context.Bounds.Height,
         Width   => Context.Bounds.Width);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Context : in out Context_Type) is
   begin
      Context.BasisArea.Free;
      Context.WindowArea.Free;
      Context.ModalArea.Free;
      Context.ContextArea.Free;
   end;
   ---------------------------------------------------------------------------

end GUI;
