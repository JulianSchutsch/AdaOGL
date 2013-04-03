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

-- Revision History
--   11.Mar 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   Provide a basis for a simple GUI framework solving essential tasks as
--   nested rectangle(object) management, mouse and keyboard signal delievery
--   and canvasse.
--   Actual implementation is part of child packages.

-- Usage
--   Objects in this package should be treated as abstract.
--   Before usage, a context implementation must be selected using the
--   Implementations sub-package.
--   With the implementation a new (render) context can be created.
--
--   Each context has three root objects:
--     * WindowArea   : For ordinary objects, lowest layer
--     * ModalArea    : Similar to WindowArea, but blocks all signals if
--                      activated
--     * ContextArea  : Highest layer for special context specific tasks
--                      like for example context menus or combo box
--                      lists.
--   Objects are a combination of a basic functionality and a theme.
--   To actually create an object a theme has to be selected in GUI.Themes.
--   As a parent any of the three root objects or any other object can be
--   chosen.

-- TODO: issue : there is a set of empty canvases not added anywhere
--               they should be freed. Add special handling for empty canvases

pragma Ada_2005;

with BoundsCalc; use BoundsCalc;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Canvas;
with GUIDefinitions; use GUIDefinitions;
with GUIKeys; use GUIKeys;
with GUIMouse; use GUIMouse;
with Ada.Containers.Doubly_Linked_Lists;

package GUI is

   FocusRedirectionToNull : Exception;
   FailedToCreateContext  : Exception;
   InvalidContext         : Exception;
   FailedToDestroyContext : Exception;

   type OnResize_Access is
     access procedure
       (CallbackObject : AnyObject_ClassAccess);

   type OnCloseContext_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type OnContextAreaClick_Access is
     access procedure
       (CallBackObject : AnyObject_ClassAccess);

   type Context_Type;
   type Context_ClassAccess is access all Context_Type'Class;
   ---------------------------------------------------------------------------

   type Canvas_Type is abstract new Canvas.Canvas_Type with private;
   type Canvas_Access is access all Canvas_Type;
   type Canvas_ClassAccess is access all Canvas_Type'Class;

   procedure SetBounds
     (Canvas  : access Canvas_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean);

   procedure SetAnchors
     (Canvas : access Canvas_Type;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean);

   function GetNextCanvas
     (Canvas : access Canvas_Type)
      return Canvas_ClassAccess;

   function GetBounds
     (Canvas : access Canvas_Type)
      return Bounds_Type;

   procedure BringToFront
     (Canvas : access Canvas_Type);

   -- CALL ONLY BY INHERITED
   procedure Finalize
     (Canvas : access Canvas_Type);
   ---------------------------------------------------------------------------

   procedure FreeCanvas
     (Canvas : in out Canvas_ClassAccess);
   ---------------------------------------------------------------------------

   type Object_Public is new AnyObject_Type with
      record
         Render              : Render_Enum:=RenderCanvasse;
         CallBackObject      : AnyObject_ClassAccess;
         FocusStyle          : FocusStyle_Enum:=FocusStyleNone;
         Focussed            : Boolean:=False;
         TopHeightConstraint : Constraint_Type;
         LeftWidthConstraint : Constraint_Type;
         -- Read only
         AbsBounds           : AbsBounds_Type;
         OnResize            : OnResize_Access:=null;
      end record;

   type Object_Type is new Object_Public with private;
   type Object_Access is access all Object_Type;
   type Object_ClassAccess is access all Object_Type'Class;

   type OnASync_Access is
     access procedure
       (Item : Object_ClassAccess);

   procedure RenderCustom
     (Object : access Object_Type) is null;

   procedure Initialize
     (Item   : access Object_Type;
      Parent : Object_ClassAccess);

   function NewCanvas
     (Item   : access Object_Type;
      Height : Integer;
      Width  : Integer)
      return Canvas_ClassAccess;

   procedure Focus
     (Item : access Object_Type) is null;

   procedure Defocus
     (Item : access Object_Type) is null;

   procedure Free
     (Item : access Object_Type);

   function CharacterInput
     (Item  : access Object_Type;
      Chars : Unbounded_String)
      return Boolean;

   function KeyDown
     (Item : access Object_Type;
      Key  : Key_Enum)
      return Boolean;

   function KeyUp
     (Item : access Object_Type;
      Key  : Key_Enum)
      return Boolean;

   function MouseDown
     (Item   : access Object_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   procedure MouseUp
     (Item   : access Object_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is null;

   procedure MouseMove
     (Item   : access Object_Type;
      X      : Integer;
      Y      : Integer) is null;

   procedure SetBounds
     (Object  : access Object_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean);

   function GetBounds
     (Object : access Object_Type)
      return Bounds_Type;

   function GetClientBounds
     (Object : access Object_Type)
      return Bounds_Type;

   function GetAbsBounds
     (Object : access Object_Type)
      return AbsBounds_Type;

   function GetPrevBounds
     (Object : access Object_Type)
      return Bounds_Type;

   procedure SetAnchors
     (Object : access Object_Type;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean);

   function GetClient
     (Object : access Object_Type)
      return Object_ClassAccess;

   procedure SetClient
     (Object : access Object_Type;
      Client : Object_ClassAccess);

   procedure Resize
     (Item : access Object_Type) is null;

   procedure SetFocus
     (Item : access Object_Type);

   procedure BringToFront
     (Item : Object_ClassAccess);

   procedure SetFocusObject
     (Item   : access Object_Type;
      Object : Object_ClassAccess);

   function GetLastTreeObject
     (Item : access Object_Type)
      return Object_ClassAccess;

   function GetFirstCanvas
     (Item : access Object_Type)
      return Canvas_ClassAccess;

   function GetCanvasCount
     (Item : access Object_Type)
      return Integer;

   function GetContextArea
     (Item : access Object_Type)
      return Object_ClassAccess;

   procedure SetContextAreaClick
     (Item           : access Object_Type;
      CallBack       : OnContextAreaClick_Access;
      CallBackObject : AnyObject_ClassAccess);

   procedure AddASync
     (Item   : access Object_Type;
      Method : OnASync_Access);
   ---------------------------------------------------------------------------

   type Context_Private is private;

   type Context_Type is abstract tagged limited
      record
         CallBackObject     : AnyObject_ClassAccess     := null;
         OnClose            : OnCloseContext_Access     := null;
         OnContextAreaClick : OnContextAreaClick_Access := null;
         BasisArea          : Object_ClassAccess        := null;
         WindowArea         : Object_ClassAccess        := null;
         ModalArea          : Object_ClassAccess        := null;
         ContextArea        : Object_ClassAccess        := null;
         -- Read only
         Bounds         : Bounds_Type;
         Priv           : Context_Private;
      end record;

   procedure NewCanvas
     (Context : in out Context_Type;
      Object  : Object_ClassAccess;
      Height  : Integer;
      Width   : Integer;
      Canvas  : out Canvas_ClassAccess) is abstract;

   procedure ContextMouseDown
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer);

   procedure ContextMouseUp
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer);

   procedure ContextMouseMove
     (Context : Context_ClassAccess;
      AbsX    : Integer;
      AbsY    : Integer);

   procedure ContextCharacterInput
     (Context : Context_ClassAccess;
      Chars   : Unbounded_String);

   procedure ContextKeyDown
     (Context : Context_ClassAccess;
      Key     : Key_Enum);

   procedure ContextKeyUp
     (Context : Context_ClassAccess;
      Key     : Key_Enum);

   procedure PropagateContextResize
     (Context : Context_ClassAccess;
      Height  : Integer;
      Width   : Integer);

   -- CALL ONLY BY INHERITED
   procedure Initialize
     (Context : access Context_Type);

   -- CALL ONLY BY INHERITED, CALL FreeContext
   procedure Finalize
     (Context : in out Context_Type);

   procedure FreeContext
     (Context : in out Context_ClassAccess);

   ---------------------------------------------------------------------------
   type Context_Constructor is
     access function
       (Configuration : Config.Config_Type;
        Node          : Unbounded_String)
        return Context_ClassAccess;
   ---------------------------------------------------------------------------

   type Implementation_Type is
      record
         NewContext : Context_Constructor:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => To_Unbounded_String("GUIImplementation"));

   -- These functions are special and expect fully initialized contexts
   -- Therefore they should only be called by Context implementations.
   procedure AddCanvasByContext
     (Object : Object_ClassAccess;
      Canvas : Canvas_ClassAccess);

private

   type ASync_Type is
      record
         Method : OnAsync_Access     := null;
         Object : Object_ClassAccess := null;
      end record;

   package ASyncList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ASync_Type,
      "=" => "=");

   type Context_Private is
      record
         MouseButtonsPressed : MouseButton_Array  := NoMouseButtons;
         MouseSelection      : Object_ClassAccess := null;
         FocusObject         : Object_ClassAccess := null;
         ASyncCalls          : ASyncList_Pack.List;
      end record;

   type Object_Type is new Object_Public with
      record
         Context             : Context_ClassAccess;
         Bounds              : Bounds_Type;
         PrevBounds          : Bounds_Type;
         Anchors             : Anchors_Type;
         Next                : Object_ClassAccess:=null;
         Last                : Object_ClassAccess:=null;
         Parent              : Object_ClassAccess:=null;
         FirstChild          : Object_ClassAccess:=null;
         LastChild           : Object_ClassAccess:=null;
         Client              : Object_ClassAccess:=null;
         FocusObject         : Object_ClassAccess:=null;
         Canvasse            : Canvas_ClassAccess:=null;
      end record;

   ---------------------------------------------------------------------------

   type Canvas_Type is abstract new Canvas.Canvas_Type with
      record
         Bounds : Bounds_Type;
         Next    : Canvas_ClassAccess;
         Last    : Canvas_ClassAccess;
         Anchors : Anchors_Type;
         Object  : Object_ClassAccess;
      end record;

end GUI;
