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
with Interfaces.C.Strings; use Interfaces.C.Strings;
use Interfaces.C;

with System.Memory;
with System.Address_To_Access_Conversions;

package body Virtada.Host.Interfaces is

   overriding procedure Adjust (Object : in out Interface_Type) is
   begin
      if Object.Ptr /=
        virInterfacePtr (Null_Address)
      then -- the object has been initialized
         if virInterfaceRef (Object.Ptr) = -1 then
            raise Domain_Error;
         end if;
         Object.Initialized := True;
      end if;
   end Adjust;

   function Get_Name (Object : Interface_Type) return String is
   begin
      return To_String (Object.Name);
   end Get_Name;

   overriding procedure Finalize (Object : in out Interface_Type) is
   begin
      if Object.Initialized then
         if virInterfaceFree (Object.Ptr) = -1 then
            raise Domain_Error;
         end if;
      end if;
   end Finalize;

   type Interface_Ptr_Array is
     array (size_t range <>) of aliased virInterfacePtr;

   function List_Interfaces
     (Connection : Connect_Type'Class;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Interface_Array
   is
      Flags     : virConnectListAllInterfacesFlags := 0;
      List_Addr : System.Address;
      Num       : Integer;
      Empty     : Interface_Array (1 .. 0);
   begin
      if Active then
         Flags := Flags or VIR_CONNECT_LIST_INTERFACES_ACTIVE;
      end if;

      if Inactive then
         Flags := Flags or VIR_CONNECT_LIST_INTERFACES_INACTIVE;
      end if;

      Num :=
        Integer
          (virConnectListAllInterfaces
             (Connection.Link.Ptr,
              List_Addr'Address,
              Flags));
      if Num = -1 then
         return Empty;
      end if;

      declare
         subtype Constr_IPA is Interface_Ptr_Array (0 .. size_t (Num));
         package P2A is new System.Address_To_Access_Conversions (Constr_IPA);
         List_Ptr       : P2A.Object_Pointer;
         Interface_List : Interface_Array (1 .. Num);
         X              : size_t;
      begin

         List_Ptr := P2A.To_Pointer (List_Addr);

         for I in Interface_List'Range loop
            X := size_t (I - 1);
            -- set interface_ptr
            Interface_List (I).Ptr         := List_Ptr (X);
            Interface_List (I).Initialized := True;
            -- Get name
            Interface_List (I).Name :=
              To_Unbounded_String (Value (virInterfaceGetName (List_Ptr (X))));
         end loop;

         System.Memory.Free (List_Addr);
         return Interface_List;

      end;

   end List_Interfaces;

end Virtada.Host.Interfaces;
