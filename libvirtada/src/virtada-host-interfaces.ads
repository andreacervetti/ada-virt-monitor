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
with Virtada.Host;       use Virtada.Host;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Libvirt_Interface_Api; use Libvirt_Interface_Api;

package Virtada.Host.Interfaces is

   --------------------
   -- Interface
   --------------------
   type Interface_Type is new Ada.Finalization.Controlled with private;

   type Interface_Array is array (Positive range <>) of Interface_Type;

   function List_Interfaces
     (Connection : Connect_Type'Class;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Interface_Array;

   function Get_Name (Object : Interface_Type) return String;

private

   type Interface_Type is new Ada.Finalization.Controlled with record
      Name        : Unbounded_String;
      Initialized : Boolean         := False;
      Ptr         : virInterfacePtr := virInterfacePtr (Null_Address);
   end record;

   overriding procedure Adjust (Object : in out Interface_Type);
   overriding procedure Finalize (Object : in out Interface_Type);

end Virtada.Host.Interfaces;
