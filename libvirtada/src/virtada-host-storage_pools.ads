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
with Ada.Finalization;
with Libvirt_Storage_Api; use Libvirt_Storage_Api;

package Virtada.Host.Storage_Pools is

   -------------------
   -- Storage Pools --
   -------------------
   Storage_Pool_Error : exception;

   type Storage_Pool_Type is tagged private;

   type Storage_Pool_Array is array (Positive range <>) of Storage_Pool_Type;

   function List_Storage_Pools
     (Connection : Connect_Type'Class;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Storage_Pool_Array;

   -------------
   -- Volumes --
   -------------
   Volume_Error : exception;

   type Volume_Type is tagged private;

   type Vol_Type is (File, Block, Dir, Network, Netdir, Ploop);

   type Volume_Info is
      record
         CType      : Vol_Type;
         Capacity   : Unsigned_Long_Long;
         Allocation : Unsigned_Long_Long;
      end record;

   function Get_Volume_Info (Volume : Volume_Type) return Volume_Info;

   function Get_Volume_By_Path (Connection : Connect_Type'Class;
                                Path       : String)
                                return Volume_Type;

private

   type Storage_Pool_Type is new Ada.Finalization.Controlled with
      record
         Ptr : virStoragePoolPtr;
      end record;

   overriding procedure Adjust (Object : in out Storage_Pool_Type);
   overriding procedure Finalize (Object : in out Storage_Pool_Type);

   type Volume_Type is new Ada.Finalization.Controlled with
      record
         Ptr : virStorageVolPtr := virStorageVolPtr (Null_Address);
      end record;

   overriding procedure Adjust (Object : in out Volume_Type);
   overriding procedure Finalize (Object : in out Volume_Type);

end Virtada.Host.Storage_Pools;
