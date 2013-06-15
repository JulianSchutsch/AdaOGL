--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

pragma Ada_2005;

with Basics; use Basics;

package body Errors  is

   function StringToParams
     (Str : String)
      return ErrorParamArray_Access is

      Count    : Integer:=0;
      Empty    : Boolean:=True;
      Result   : ErrorParamArray_Access;
      Start    : Integer;

   begin

      for i in Str'Range loop
         if Str(i)=';' then
            Count:=Count+1;
            Empty:=True;
         else
            Empty:=False;
         end if;
      end loop;

      if not Empty then
         Count:=Count+1;
      end if;

      Result := new ErrorParamArray_Type(1..Count);
      Count  := Result'First;
      Start  := Str'First;
      Empty  := True;

      for i in Str'Range loop
         if Str(i)=';' then
            Result(Count):=U(Str(Start..i-1));
            Count:=Count+1;
            Start:=i+1;
            Empty:=True;
         else
            Empty:=False;
         end if;
      end loop;
      if not Empty then
         Result(Count):=U(Str(Start..Str'Last));
      end if;
      return Result;
   end StringToParams;
   ---------------------------------------------------------------------------

end Errors;
