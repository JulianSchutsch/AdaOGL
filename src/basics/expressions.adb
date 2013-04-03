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
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Expressions is

   function Process
     (String    : Unbounded_String;
      Variables : StringStringMap_Pack.Map)
      return Unbounded_String is

      StringPosition : Integer:=1;
      PrevStringPosition : Integer:=-1;

      type Element_Enum is
        (ElementInteger,
         ElementString);

      type Element_Type is
         record
            Typ         : Element_Enum;
            IntValue    : aliased Integer;
            StringValue : Unbounded_String;
         end record;

      type Token_Enum is
        (TokenSymbol,
         TokenInteger,
         TokenString,
         TokenAdd,
         TokenSub,
         TokenMul,
         TokenDiv);

      Token       : Token_Enum;
      IntValue    : Integer;
      StringValue : Unbounded_String;

      function NextToken
        return Boolean is

         type TokenMode_Enum is
           (TokenModeNone,
            TokenModeInteger,
            TokenModeString,
            TokenModeSymbol);

         TokenMode : TokenMode_Enum:=TokenModeNone;
         Char      : Character;

      begin

         PrevStringPosition:=StringPosition;

         loop

            if StringPosition>Length(String) then
               exit;
            end if;

            Char:=Element(String,StringPosition);
            StringPosition:=StringPosition+1;

            case TokenMode is
               when TokenModeNone =>
                  case Char is
                     when '+' =>
                        Token:=TokenAdd;
                        return True;
                     when '-' =>
                        Token:=TokenSub;
                        return True;
                     when '*' =>
                        Token:=TokenMul;
                        return True;
                     when '/' =>
                        Token:=TokenDiv;
                        return True;
                     when '0'..'9' =>
                        IntValue:=Character'Pos(Char)-Character'Pos('0');
                        TokenMode:=TokenModeInteger;
                     when '"' =>
                        StringValue:=To_Unbounded_String("");
                        TokenMode:=TokenModeString;
                     when ' ' =>
                        null;
                     when others =>
                        StringValue:=To_Unbounded_String("");
                        TokenMode:=TokenModeSymbol;
                  end case;

               when TokenModeInteger =>
                  case Char is
                     when '0'..'9' =>
                        IntValue:=IntValue*10+Character'Pos(Char)-Character'Pos('0');
                     when others =>
                        StringPosition:=StringPosition-1;
                        Token:=TokenInteger;
                        return True;
                  end case;

               when TokenModeString =>
                  case Char is
                     when '"' =>
                        Token:=TokenString;
                        return True;
                     when others =>
                        StringValue:=StringValue&Char;
                  end case;

               when TokenModeSymbol =>
                  case Char is
                     when ' ' =>
                        Token:=TokenSymbol;
                        return True;
                     when others =>
                        StringValue:=StringValue&Char;
                  end case;

            end case;

         end loop;

         case TokenMode is
            when TokenModeInteger =>
               Token:=TokenInteger;
               return True;
            when TokenModeString =>
               Token:=TokenString;
               return True;
            when TokenModeSymbol =>
               Token:=TokenSymbol;
               return True;
            when TokenModeNone =>
               return False;
         end case;

      end NextToken;
      ------------------------------------------------------------------------

      -- Goes one token back
      -- Only works once
      procedure PrevToken is
      begin
         StringPosition:=PrevStringPosition;
      end PrevToken;
      ------------------------------------------------------------------------

      function GetElement
        (Result : access Element_Type)
         return Boolean is

      begin
         if not NextToken then
            return False;
         end if;
         case Token is
            when TokenInteger =>
               Result.Typ:=ElementInteger;
               Result.IntValue:=IntValue;
               return True;

            when TokenString =>
               Result.Typ:=ElementString;
               Result.StringValue:=StringValue;
               Return True;

            when TokenSymbol =>
               declare
                  VarValue : Unbounded_String;
               begin
                  VarValue:=Variables.Element(StringValue);
                  Result.Typ:=ElementInteger;
                  if not TryStringToInteger(VarValue,Result.IntValue'Access) then
                     Result.Typ:=ElementString;
                     Result.StringValue:=VarValue;
                  end if;
                  return True;
               end;
            when others =>
               raise ElementExpected;

         end case;

      end GetElement;
      ------------------------------------------------------------------------

      function MulExpression
        (Result : access Element_Type)
         return Boolean is

         Operation : Token_Enum;
         Operand   : aliased Element_Type;

      begin

         if not GetElement(Result) then
            return False;
         end if;

         loop

            if not NextToken then
               return True;
            end if;
            Operation:=Token;

            if (Operation/=TokenMul)
              and (Operation/=TokenDiv) then
               PrevToken;
               return True;
            end if;

            if not GetElement(Operand'Access) then
               raise ElementExpected;
            end if;

            case Operation is
               when TokenMul =>
                  case Result.Typ is
                     when ElementInteger =>
                        if Operand.Typ/=ElementInteger then
                           raise IncompatibleOperands;
                        end if;
                        Result.IntValue:=Result.IntValue*Operand.IntValue;
                     when ElementString =>
                        raise IncompatibleOperands;
                  end case;

               when TokenDiv =>
                  case Result.Typ is
                     when ElementInteger =>
                        if Operand.Typ/=ElementInteger then
                           raise IncompatibleOperands;
                        end if;
                        Result.IntValue:=Result.IntValue/Operand.IntValue;
                     when ElementString =>
                        raise IncompatibleOperands;
                  end case;

               when others =>
                  raise OperationExpected;

            end case;

         end loop;
      end MulExpression;

      function AddExpression
        (Result : access Element_Type)
         return Boolean is

         Operand   : aliased Element_Type;
         Operation : Token_Enum;

      begin
         if not MulExpression(Result) then
            return False;
         end if;
         loop
            if not NextToken then
               return True;
            end if;
            Operation:=Token;

            if (Operation/=TokenAdd)
              and (Operation/=TokenSub) then
               return True;
            end if;

            if not MulExpression(Operand'Access) then
               raise ElementExpected;
            end if;

            case Operation is
               when TokenAdd =>
                  case Result.Typ is
                     when ElementInteger =>
                        if Operand.Typ/=ElementInteger then
                           raise IncompatibleOperands;
                        end if;
                        Result.IntValue:=Result.IntValue+Operand.IntValue;
                     when ElementString =>
                        if Operand.Typ/=ElementString then
                           raise IncompatibleOperands;
                        end if;
                        Result.StringValue:=Result.StringValue&Operand.StringValue;
                  end case;

               when TokenSub =>
                  case Result.Typ is
                     when ElementInteger =>
                        if Operand.Typ/=ElementInteger then
                           raise IncompatibleOperands;
                        end if;
                        Result.IntValue:=Result.IntValue-Operand.IntValue;
                     when ElementString =>
                        raise IncompatibleOperands;
                  end case;

               when others =>
                  raise OperationExpected;

            end case;
         end loop;

      end AddExpression;
      ------------------------------------------------------------------------

      function Expression
        (Result : access Element_Type)
         return Boolean is

      begin
         return AddExpression(Result);
      end Expression;

      Result : aliased Element_Type;
   begin

      if not Expression(Result'Access) then
         return To_Unbounded_String("");
      end if;

      case Result.Typ is
         when ElementInteger =>
            return To_Unbounded_String(Integer'Image(Result.IntValue));
         when ElementString =>
            return Result.StringValue;
      end case;

   end Process;
   ---------------------------------------------------------------------------

end Expressions;
