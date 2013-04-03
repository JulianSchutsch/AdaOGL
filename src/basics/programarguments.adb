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

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
With Ada.Text_IO; use Ada.Text_IO;

package body ProgramArguments is

   procedure Debug is

      use type StringStringMap_Pack.Cursor;
      use type StringVector.Cursor;

   begin

      Put("Program Parameters:");
      New_Line;
      for i in Parameters.First_Index..Parameters.Last_Index loop
         Put(" ");
         Put(To_String(Parameters.Element(i)));
         New_Line;
      end loop;

      Config.Debug(Configuration);

   end Debug;
   ---------------------------------------------------------------------------

   procedure Initialize is

      Argument : Unbounded_String;
      EqualPos : Natural;

   begin

      for i in 1..Ada.Command_Line.Argument_Count loop
         Argument:=To_Unbounded_String
           (Ada.Command_Line.Argument(i));
         if Element(Argument,1)='-' then
            EqualPos:=Index
              (Source  => Argument,
               Pattern => "=",
               From    => 1);
            if EqualPos/=0 then
               -- Exceptions are ignored to avoid trouble with
               -- garbage arguments (calling with mpiexec for example)
               begin
                  Configuration.Insert
                    ("Arguments."&Unbounded_Slice
                       (Source => Argument,
                        Low    => 2,
                        High   => EqualPos-1),
                     Unbounded_Slice
                       (Source => Argument,
                        Low    => EqualPos+1,
                        High   => Length(Argument)));
               exception
                  when Others =>
                     null;
               end;
            else
               Configuration.Insert
                 ("Arguments."&Unbounded_Slice
                    (Source => Argument,
                     Low    => 2,
                     High   => Length(Argument)),
                  To_Unbounded_String(""));
            end if;
         else
            Parameters.Append
              (New_Item => Argument);
         end if;
      end loop;

   end Initialize;
   ---------------------------------------------------------------------------

end ProgramArguments;
