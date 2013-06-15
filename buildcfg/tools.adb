pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat;
with GNAT.Strings;
with GNAT.OS_Lib;
with Ada.Directories;

package body Tools is

   TempOutFile : constant String:="quickexectemp.out";

   function ExtractArgsFromOutput
     (Args : StringArray_Type)
      return StringList_Pack.List is

      use type StringList_Pack.Cursor;

      Cursor : StringList_Pack.Cursor;
      Part   : Unbounded_String;
      Result : StringList_Pack.List;

      procedure Test is
      begin
         for Arg in Args'Range loop
            if Length(Part)>=Length(Args(Arg)) and then
              Args(Arg)=Slice(Part,1,Length(Args(Arg))) then
               Result.Append(Part);
               Part:=U("");
               return;
            end if;
         end loop;
         Part:=U("");
      end Test;
      ------------------------------------------------------------------------

   begin
      Cursor:=Output.First;
      while Cursor/=StringList_Pack.No_Element loop
         declare
            OutElement : String:=To_String(StringList_Pack.Element(Cursor));
         begin
            for i in OutElement'Range loop
               if OutElement(i)=' ' then
                  Test;
               else
                  Part:=Part&OutElement(i);
               end if;
            end loop;
            Test;
         end;
         Cursor:=StringList_Pack.Next(Cursor);
      end loop;
      return Result;
   end ExtractArgsFromOutput;
   ---------------------------------------------------------------------------

   function StringInEnvironment
     (Str : String;
      Env : String)
      return Boolean is

      use type  GNAT.Strings.String_Access;

      EnvString : GNAT.Strings.String_Access;
      Result    : Boolean:=False;

   begin
      EnvString:=GNAT.OS_Lib.Getenv(Env);
      if EnvString/=null then
         if GNAT.Regpat.Match
           (Expression => Str,
            Data       => EnvString.all) then
            Result:=True;
         end if;
         GNAT.Strings.Free(EnvString);
      end if;
      return Result;
   end StringInEnvironment;
   ---------------------------------------------------------------------------

   function StringInOutput
     (Str : String)
      return Boolean is

      use type StringList_Pack.Cursor;

      Cursor : StringList_Pack.Cursor;

   begin
      Cursor:=Output.First;
      while Cursor/=StringList_Pack.No_Element loop
         if GNAT.Regpat.Match
           (Expression => Str,
            Data       => To_String(StringList_Pack.Element(Cursor))) then
            return True;
         end if;
         Cursor:=StringList_Pack.Next(Cursor);
      end loop;
      return False;
   end StringInOutput;
   ---------------------------------------------------------------------------

   procedure QuickExec
     (Command  : String;
      Argument : String) is

      use type GNAT.Strings.String_Access;

      Arguments  : GNAT.OS_Lib.String_List_Access;
      ExecPath   : GNAT.OS_Lib.String_Access;
      ReturnCode : Integer;
      File       : File_Type;

   begin
      Output.Clear;
      ExecPath := GNAT.OS_Lib.Locate_Exec_On_Path(Command);
      if ExecPath=null then
         Success:=False;
         return;
      end if;
      Arguments:=GNAT.OS_Lib.Argument_String_To_List(Argument);
      GNAT.OS_Lib.Spawn
        (Program_Name => ExecPath.all,
         Args         => Arguments.all,
         Output_File  => TempOutFile,
         Success      => Success,
         Return_Code  => ReturnCode,
         Err_To_Out   => True);
      GNAT.Strings.Free(ExecPath);
      GNAT.Strings.Free(Arguments);
      if (ReturnCode/=0) then
         Success:=False;
         return;
      end if;
      Open
        (File => File,
         Mode => In_File,
         Name => TempOutFile);
      while not End_Of_File(File) loop
         Output.Append(U(Get_Line(File)));
      end loop;
      Close(File);
      Ada.Directories.Delete_File(TempOutFile);
   end QuickExec;
   ---------------------------------------------------------------------------

   procedure DeleteFile
     (Name : String) is
   begin
      Ada.Directories.Delete_File(Name);
   exception
      when others =>
         null;
   end DeleteFile;
   ---------------------------------------------------------------------------

   function StringListToGprList
     (List : StringList_Pack.List)
      return Unbounded_String is

      use type StringList_Pack.Cursor;

      Cursor : StringList_Pack.Cursor;
      Result : Unbounded_String;

   begin
      Result:=U("(");
      Cursor:=List.First;
      while Cursor/=StringList_Pack.No_Element loop
         if Cursor/=List.First then
            Result:=Result&",";
         end if;
         Result:=Result&""""&StringList_Pack.Element(Cursor)&"""";
         Cursor:=StringList_Pack.Next(Cursor);
      end loop;
      Result:=Result&")";
      return Result;
   end StringListToGprList;
   ---------------------------------------------------------------------------

end Tools;
