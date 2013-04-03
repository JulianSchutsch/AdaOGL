package body VersionParser is

   DigitLimit : constant Natural:=5;

   function Parse
     (StringData : String;
      Limit      : Natural:=0)
      return Version_Type is

      Pos   : Integer := StringData'First;
      Depth : Integer := 0;

      function Current
        return Character is
      begin

         if Pos<=StringData'Last then
            return StringData(Pos);
         else
            return Character'Val(0);
         end if;

      end Current;
      ------------------------------------------------------------------------

      procedure Consume is
      begin
         pragma Assert(Pos<=StringData'Last);
         Pos:=Pos+1;
      end Consume;
      ------------------------------------------------------------------------

      function EndOfString
        return Boolean is
      begin
         return Pos>StringData'Last;
      end EndOfString;
      ------------------------------------------------------------------------

      procedure ParseNumber is
      begin

         while Current=' ' loop
            Consume;
         end loop;

         if EndOfString then
            raise InvalidVersionString with "Unexpected end at "&Natural'Image(Pos);
         end if;

         if Current not in '0'..'9' then
            raise InvalidVersionString with "Expected number at "&Natural'Image(Pos);
         end if;
         Consume;

         declare
            DigitCount : Natural:=1;
         begin
            while Current in '0'..'9' loop
               DigitCount:=DigitCount+1;
               if DigitCount>DigitLimit then
                  raise InvalidVersionString with "More than 5 digits for a version number part";
               end if;
               Consume;
            end loop;
         end;

         Depth:=Depth+1;

         while Current=' ' loop
            Consume;
         end loop;

         if EndOfString then
            return;
         end if;

         if (Limit/=0) and (Depth>=Limit) then
            return;
         end if;

         if Current/='.' then
            raise InvalidVersionString with "Missing dot at "&Natural'Image(Pos);
         end if;
         Consume;

         ParseNumber;

      end ParseNumber;
      ------------------------------------------------------------------------

      procedure ReadNumber(Result: in out Version_Type) is

         Number : Natural:=0;

      begin

         while Current=' ' loop
            Consume;
         end loop;

         pragma Assert(not EndofString);

         while Current in '0'..'9' loop
            Number:=Character'Pos(Current)-Character'Pos('0')+number*10;
            Consume;
         end loop;

         Result(Result'First):=Number;

         while Current=' ' loop
            Consume;
         end loop;

         if EndOfString then
            return;
         end if;

         if Result'Length=1 then
            return;
         end if;
         pragma Assert(Current='.');
         Consume;

         ReadNumber(Result(Result'First+1..Result'Last));

      end ReadNumber;

   begin

      ParseNumber;
      Pos:=StringData'First;
      return Result:Version_Type(1..Depth) do
         if Depth/=0 then
            ReadNumber(Result);
         end if;
      end return;

   end Parse;
   ---------------------------------------------------------------------------

end VersionParser;
