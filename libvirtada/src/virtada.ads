-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2018 Andrea Cervetti                      --
--               Copyright (C) 2018 Homebrew Internet s.r.l.               --
--                                                                         --
-- This library is free software: you can redistribute it and/or modify    --
-- it under the terms of the GNU General Public License as published by    --
-- the Free Software Foundation, either version 3 of the License, or       --
-- (at your option) any later version.                                     --
--                                                                         --
-- As a special exception, if other files instantiate generics from        --
-- this unit, or you link this unit with other files to produce an         --
-- executable, this unit does not by itself cause the resulting executable --
-- to be covered by the GNU General Public License. This exception does    --
-- not however invalidate any other reasons why the executable file might  --
-- be covered by the GNU Public License.                                   --
--                                                                         --
-- This library is distributed in the hope that it will be useful,         --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of          --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
-- GNU General Public License for more details.                            --
--                                                                         --
-- You should have received a copy of the GNU General Public License       --
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                         --
-----------------------------------------------------------------------------
pragma Ada_2005;
with Interfaces.C;
with Interfaces.C.Extensions;

with Ada.Finalization;

with Interfaces.C.Strings;

package Virtada is

   subtype unsigned is  Interfaces.C.unsigned;
   subtype Unsigned_Long is Interfaces.C.unsigned_long;
   subtype Unsigned_Char is Interfaces.C.unsigned_char;

   type Unsigned_Long_Long is new Interfaces.C.Extensions.unsigned_long_long;


   Domain_Error : exception;

   Invalid_Argument, Not_Found : exception;

   ----------------------------------
   -- libvirt_common_api interface --
   ----------------------------------
   type Typed_Parameter  is tagged private;
   type Typed_Params_List is array (Natural range <>) of Typed_Parameter;

   function Exists (List : Typed_Params_List; Name : String)
                    return Boolean;
   -- Test if typed parameter exists in list

   -- overloaded functions
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Boolean;
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Long_Float;
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Integer;
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Long_Long_Integer;
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return String;
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Unsigned;
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Unsigned_Long_Long;

   function Type_Image (Param : Typed_Parameter) return String;
   function Get_Name   (Param : Typed_Parameter) return String;

private
   type Param_Type is (TP_INT, TP_UINT, TP_LLONG, TP_ULLONG,
                       TP_DOUBLE, TP_BOOLEAN, TP_STRING, TP_DUMMY);

   type Param_Value (Discr : Param_Type := TP_DUMMY) is
      record
       case Discr  is
         when TP_INT =>
            i : aliased Interfaces.C.int;
         when TP_UINT =>
            ui : aliased unsigned;
         when TP_LLONG =>
            l : aliased Long_Long_Integer;
         when TP_ULLONG =>
            ul : aliased Unsigned_Long_Long;
         when TP_DOUBLE =>
            d : aliased Interfaces.C.double;
         when TP_BOOLEAN =>
            b : aliased Interfaces.C.char;
         when TP_STRING =>
            s : Interfaces.C.Strings.chars_ptr;
         when TP_DUMMY => -- This is just to have the type unconstrained
            null;         -- and not limited
         end case;
      end record;

   type Typed_Parameter is new Ada.Finalization.Controlled with
      record
         Name : Interfaces.C.char_array (0 .. 79);
         Value : Param_Value;
      end record;

   overriding procedure Adjust   (Object : in out Typed_Parameter);
   overriding procedure Finalize (Object : in out Typed_Parameter);

end Virtada;
