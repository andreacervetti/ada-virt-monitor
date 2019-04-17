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
with Virterror; use Virterror;

package Virtada.Errors is

   ---------------------------
   -- log functions
   ---------------------------
   type Libvirt_Error is tagged private;
   -- struct holding error info
   function Code (Error : Libvirt_Error) return String;
   function Code (Error : Libvirt_Error) return Integer;
   function Domain (Error : Libvirt_Error) return String;
   function Domain (Error : Libvirt_Error) return Integer;
   function Message (Error : Libvirt_Error) return String;

   type Error_Handler_Type is access procedure (Err : Libvirt_Error);

   procedure Reset_Default_Log;
   -- set the log output to the default libvirt function
   procedure Set_Error_Function (Callback : Error_Handler_Type);

   procedure Suspend;

   procedure Resume;

   function Log_Active return Boolean;

private

   type Libvirt_Error is tagged record
      Data : u_virError;
   end record;

end Virtada.Errors;
