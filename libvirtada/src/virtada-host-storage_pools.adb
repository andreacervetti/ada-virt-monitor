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
with Interfaces.C; use Interfaces.C;
with System.Memory;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Virtada.Host.Storage_Pools is
   
   type Storage_Poll_Ptr_Array is array (size_t range <>) of virStoragePoolPtr;

   ------------------------
   -- List_Storage_Pools --
   ------------------------
   function List_Storage_Pools
     (Connection : Connect_Type'Class;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Storage_Pool_Array
   is
      Flags     : virConnectListAllStoragePoolsFlags := 0;
      List_Addr : System.Address;
      Num       : int;
   begin
      if Active then
         Flags := Flags or VIR_CONNECT_LIST_STORAGE_POOLS_ACTIVE;
      end if;
      
      if Inactive then
         Flags := Flags or VIR_CONNECT_LIST_STORAGE_POOLS_INACTIVE;
      end if;
      
      Num := virConnectListAllStoragePools (Connection.Link.Ptr,
                                            List_Addr'Address,
                                            Flags);
      if Num = -1 then raise
           Storage_Pool_Error;
      end if;
      
      declare
         Ptr_List : Storage_Poll_Ptr_Array (1 .. size_t (Num));
         for Ptr_List'Address use List_Addr;
         
         Storage_Pool_List : Storage_Pool_Array (1 .. Integer (Num));
      begin
         for I in Storage_Pool_List'Range loop
            Storage_Pool_List (I) := (Ada.Finalization.Controlled with
                                      Ptr => Ptr_List (size_t (I)));
         end loop;
         System.Memory.Free (List_Addr);
         return Storage_Pool_List;
      end;
   end List_Storage_Pools;

   ---------------------
   -- Get_Volume_Info --
   ---------------------
   function Get_Volume_Info (Volume : Volume_Type)
                             return Volume_Info
   is
      Info : aliased virStorageVolInfo;
      Num : int;
   begin
      Num := virStorageVolGetInfo (Volume.Ptr, Info'Unchecked_Access);
      if Num = -1 then
         raise Volume_Error;
      end if;
      return (Vol_Type'Val (Info.c_type),
              Unsigned_Long_Long (Info.capacity),
              Unsigned_Long_Long (Info.allocation));
   end Get_Volume_Info;

   ------------------------
   -- Get_Volume_By_Path --
   ------------------------
   function Get_Volume_By_Path (Connection : Connect_Type'Class;
                                Path       : String)
                                return Volume_Type
   is
      Ptr : virStorageVolPtr;
      Path_Ptr : chars_ptr;
   begin
      Path_Ptr := New_String (Path);
      Ptr      := virStorageVolLookupByPath (Connection.Link.Ptr, Path_Ptr);
      Free (Path_Ptr);
      if Ptr = virStorageVolPtr (Null_Address) then
         raise Volume_Error;
      else
         return Volume : Volume_Type do
            Volume.Ptr := Ptr;
         end return;
      end if;
   end Get_Volume_By_Path;


   -----------------------------------------
   -- Storage Pool controlling procedures --
   -----------------------------------------
   overriding procedure Adjust (Object : in out Storage_Pool_Type) is
   begin
      if Object.Ptr /= virStoragePoolPtr (Null_Address) then
         if virStoragePoolRef (Object.Ptr) = -1 then
            raise Storage_Pool_Error;
         end if;
      end if;
   end Adjust;
   
   overriding procedure Finalize (Object : in out Storage_Pool_Type) is
   begin
      if Object.Ptr /= virStoragePoolPtr (Null_Address) then
         if virStoragePoolFree (Object.Ptr) = -1 then
            raise Storage_Pool_Error;
         end if;
      end if;
   end Finalize;

   -----------------------------------------
   -- Volume controlling procedures --
   -----------------------------------------
   overriding procedure Adjust (Object : in out Volume_Type) is
   begin
      if Object.Ptr /= virStorageVolPtr (Null_Address) then
         if virStorageVolRef (Object.Ptr) = -1 then
            raise Volume_Error;
         end if;
      end if;
   end Adjust;
   
   overriding procedure Finalize (Object : in out Volume_Type) is
   begin
      if Object.Ptr /= virStorageVolPtr (Null_Address) then
         if virStorageVolFree (Object.Ptr) = -1 then
            raise Volume_Error;
         end if;
      end if;
   end Finalize;

end Virtada.Host.Storage_Pools;
