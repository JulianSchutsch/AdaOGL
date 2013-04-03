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
--   23.Apr 2012 Julian Schutsch
--     - Original version

-- Usage
--   Each implementation has a unique entry in Implementation_Enum.
--   There is relation with schema:
--     Implementation_Enum Name:String Module_Enum

--   Where the Module describes a ParallelSim-Modul or package group.
--   The implementations for one module should match the modules common
--   interface since the program never refers to implementations directly.
--   The relation is in the array Implementations and used by
--   FindImplementation.

--   Each Implementation may have any number of packages which should be
--   added in ImplementationsPackages.

--   Compatiblity to each plattform must be added for each implementation in
--   the Compatible array.

pragma Ada_2005;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Common; use Common;
with Plattform; use Plattform;

package Implementations is

   ImplementationNotFound         : Exception;
   ImplementationInitializeFailed : Exception;

   type Implementation_Enum is
     (ImplementationBSDSockets,
      ImplementationMPICH2,
      ImplementationWGL,
      ImplementationXlib,
      ImplementationFreeType,
      ImplementationBitmapFonts);

   Default : array(Implementation_Enum) of Boolean:=(others=>False);

   procedure MPICH2_Initialize;

   type InitializeProc_Access is access procedure;

   ImplementationInitialize : array(Implementation_Enum) of InitializeProc_Access:=
     (ImplementationBSDSockets  => null,
      ImplementationMPICH2      => MPICH2_Initialize'Access,
      ImplementationWGL         => null,
      ImplementationXlib        => null,
      ImplementationFreeType    => null,
      ImplementationBitmapFonts => null);

   -- This package is used to store the Implementations selected for each
   -- module.
   package ImplementationList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Implementation_Enum,
      "="          => "=");

   type Module_Enum is
     (ModuleNetwork,
      ModuleGUI,
      ModuleDistributedSystems);

   -- Implementation_Enum Name:String Module_Enum relation
   type ImplementationEntry_Type is
      record
         Name           : Unbounded_String;
         Module         : Module_Enum;
      end record;

   Implementations : array(Implementation_Enum) of ImplementationEntry_Type:=
     (ImplementationBSDSockets =>
        (Name   => U("bsdsockets"),
         Module => ModuleNetwork),
      ImplementationMPICH2 =>
        (Name   => U("mpich2"),
         Module => ModuleDistributedSystems),
      ImplementationWGL =>
        (Name   => U("wgl"),
         Module => ModuleGUI),
      ImplementationXlib =>
        (Name   => U("xlib"),
         Module => ModuleGUI),
      ImplementationFreeType =>
        (Name   => U("freetype"),
         Module => ModuleGUI),
      ImplementationBitmapFonts =>
        (Name   => U("bitmapfonts"),
         Module => ModuleGUI));

   -- Packages for each module
   ImplementationListBSDSockets : aliased StringArray_Type:=
     (0 => U("BSDSockets.Streams"));
   ImplementationListMPICH2 : aliased StringArray_Type:=
     (0 => U("MPI.Node"));
   ImplementationListWGL : aliased StringArray_Type:=
     (0 => U("OpenGL.Context.Win32"));
   ImplementationListXlib : aliased StringArray_Type:=
     (0 => U("OpenGL.Context.Xlib"));
   ImplementationListFreeType : aliased StringArray_Type:=
     (0 => U("Fonts.FreeType"));
   ImplementationListBitmapFonts : aliased StringArray_Type:=
     (0 => U("BitmapFonts"));

   ImplementationPackages : array(Implementation_Enum) of StringArray_Access:=
     (ImplementationBSDSockets  => ImplementationListBSDSockets'Access,
      ImplementationMPICH2      => ImplementationListMPICH2'Access,
      ImplementationWGL         => ImplementationListWGL'Access,
      ImplementationXlib        => ImplementationListXLib'Access,
      ImplementationFreeType    => ImplementationListFreeType'Access,
      ImplementationBitmapFonts => ImplementationListBitmapFonts'Access);

   Compatible : array(Implementation_Enum,Plattform_Enum) of Boolean:=
     (ImplementationBSDSockets =>
        (PlattformLinux     => True,
         PlattformWindowsNT => True,
         others             => False),

      ImplementationMPICH2 =>
        (PlattformLinux     => True,
         PlattformWindowsNT => True,
         others             => False),

      ImplementationWGL =>
        (PlattformWindowsNT => True,
         others             => False),

      ImplementationXlib =>
        (PlattformLinux     => True,
         Others             => False),

      ImplementationFreeType =>
        (PlattformLinux     => True,
         PlattformWindowsNT => True,
         others             => False),

      ImplementationBitmapFonts =>
        (others => True));

   -- For automatic source generation to find the filename and name for
   -- a module.
   PackageFileName : array(Module_Enum) of Unbounded_String:=
     (ModuleNetwork            => U("../src/network/network-useimplementations"),
      ModuleGUI                => U("../src/gui/gui-useimplementations"),
      ModuleDistributedSystems => U("../src/distributedsystems/distributedsystems-useimplementations"));

   PackageName : array(Module_Enum) of Unbounded_String:=
     (ModuleNetwork            => U("Network.UseImplementations"),
      ModuleGUI                => U("GUI.UseImplementations"),
      ModuleDistributedSystems => U("DistributedSystems.UseImplementations"));

   procedure FindImplementation
     (Name           : Unbounded_String;
      Module         : out Module_Enum;
      Implementation : out Implementation_Enum);

   -- Must be called only after Plattform.Initialize
   procedure Initialize;

   package StringList_Pack is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Unbounded_String,
         "="          => "=");
   AdditionalConfigLines : StringList_Pack.List;

end Implementations;
