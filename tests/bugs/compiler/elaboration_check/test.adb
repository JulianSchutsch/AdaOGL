------------------------------------------------------------------------------
-- Elaboration_Check test proogram
------------------------------------------------------------------------------

-- Reason for test:
--   Weird behaviour of Unbounded_String was noticed in combination with
--   elaboration checks.
--   This program is the smallest program so far which reproduces this.

-- Noticed not to work with:
--   1. Plattform : Windows 7
--      Compiler  : GNAT GPL 2011 (20110428)

-- Noticed to work with:
--   1. Plattform : Debian
--      Compiler  : GNAT GPL 2011 (20110419)
--   2. Plattform : Windows 7
--      Compiler  : GNAT GPL 2010 (20100603)

-- Settings used:
--   Please read test.gpr

-- What happens:
--   This program causes an unexpected exception in TestSub
--   Debugging shows an Identifier.Reference = 0x00

-- Debugging so far:
--   The ReturnSomeString procedure seems to return the unbounded_string
--   properly, but it gets lost before calling TestSub.

-- "Workarounds":
--   1. Move ReturnSomeString to test program
--   2. Store result of ReturnSomeString in local variable before passing it
--      to TestSub
--   3. pragma Suppress(Elaboration_Checks)
--   4. Compile without -gnatE
--   5. Compile with -gnatp

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Some separate package to trigger elaboration_checks when calling the
-- one procedure included.
with Basics; use Basics;

procedure Test is

   procedure TestSub
     (Identifier : Unbounded_String) is

   begin

      -- This causes an exception because Identifier.Reference = 0x00
      Put(Identifier);

   end TestSub;
   ---------------------------------------------------------------------------

   procedure Test is

   begin

      TestSub
        (Identifier => Basics.ReturnSomeString);

   end Test;
   ---------------------------------------------------------------------------

begin

   Test;

exception

   when E:others =>
      Put("Exception Name : " & Exception_Name(E));
      New_Line;
      Put("Message : " & Exception_Message(E));
      New_Line;
      Put("Traceback      :");
      New_Line;
      Put(Symbolic_TraceBack(E));
      New_Line;

end Test;
