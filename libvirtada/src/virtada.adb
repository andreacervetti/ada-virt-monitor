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
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Virtada is
   use Interfaces.C;

   ------------
   -- Exists --
   ------------
   function Exists (List : Typed_Params_List; Name : String)
                    return Boolean is
   begin
      for I in List'Range loop
         if Name = To_Ada (List (I).Name)
         then
            return True;
         end if;
      end loop;
      return False;
   end Exists;

   --------------------------------
   -- overloaded Typed_Param_Get --
   --------------------------------
   -- Common --
   function Typed_Param_Get_Any (List : Typed_Params_List; Name : String)
                                return Typed_Parameter is
   begin
      for I in List'Range loop
         if Name = To_Ada (List (I).Name) then
            return List (I);
         end if;
      end loop;
      raise Not_Found;
   end Typed_Param_Get_Any;

   -- Boolean --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Boolean
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_BOOLEAN then
         if Param.Value.b = Char'Val(0) then
            return False;
         else
            return True;
         end if;
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- Boolean

   -- Long_Float --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Long_Float
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_DOUBLE then
         return Long_Float (Param.Value.d);
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- Long_Float

   -- Integer --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Integer
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_INT then
         return Integer (Param.Value.i);
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- Integer

   -- Long_Long_Integer --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Long_Long_Integer
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_LLONG then
         return Param.Value.l;
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- Long_Long_Integer

   -- String --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return String
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_STRING then
         return Value (Param.Value.s);
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- String

   -- Unsigned --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Unsigned
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_UINT then
         return Param.Value.ui;
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- Unsigned

   -- Unsigned_Long_Long --
   function Typed_Param_Get (List : Typed_Params_List; Name : String)
                             return Unsigned_Long_Long
   is
      Param : Typed_Parameter := Typed_Param_Get_Any (List, Name);
   begin
      if Param.Value.Discr = TP_ULLONG then
         return Unsigned_Long_Long (Param.Value.ul);
      end if;
      raise Invalid_Argument;
   end Typed_Param_Get; -- Unsigned_Long_Long


   function Type_Image (Param : Typed_Parameter)
                        return String is
   begin
      return Param_Type'Image (Param.Value.Discr);
   end Type_Image;

   function Get_Name (Param : Typed_Parameter)
                      return String is
   begin
      return To_Ada (Param.Name);
   end Get_Name;

   overriding procedure Adjust (Object : in out Typed_Parameter) is
   begin
      if Object.Value.Discr = TP_STRING then
         Object.Value.s := New_Char_Array (Value (Object.Value.s));
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Typed_Parameter) is
   begin
      if Object.Value.Discr = TP_STRING then
         Free (Object.Value.s);
      end if;
   end Finalize;

end Virtada;
