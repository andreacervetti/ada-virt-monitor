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

with System;                  use System;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
                              use Interfaces.C;

with System.Address_To_Access_Conversions;

package body Virtada.errors is

   Active : Boolean := True;

   --- log functions
   function Code (Error : Libvirt_Error)
                  return String
   is
   begin
      return "Error Code: " &
        virErrorNumber'Image (virErrorNumber'Val (Error.Data.code));
   end Code;

   function Code (Error : Libvirt_Error)
                  return Integer
   is
   begin
      return Integer (Error.Data.code);
   end Code;

   function Domain (Error : Libvirt_Error)
                    return String
   is
   begin
      return "Error Domain: " &
        virErrorDomain'Image (virErrorDomain'Val (Error.Data.domain));
   end Domain;

   function Domain (Error : Libvirt_Error)
                    return Integer
   is
   begin
      return Integer (Error.Data.domain);
   end Domain;

   function Message (Error : Libvirt_Error)
                     return String
   is
   begin
      return "message: " & Value (Error.Data.message);
   end Message;

   procedure Reset_Default_Log is
   begin
      virSetErrorFunc (Null_Address, null);
   end Reset_Default_Log;


   Error_Handler : Error_Handler_Type;

   procedure Basic_Log_Handler
     (UserData : System.Address;
      Error    : virErrorPtr);
   pragma Convention (C, Basic_Log_Handler);

   procedure Basic_Log_Handler
     (UserData : System.Address;
      Error    : virErrorPtr)
   is
      pragma Unreferenced (UserData);
      package A2P is new System.Address_To_Access_Conversions (u_virError);
   begin
      Error_Handler
        (Libvirt_Error'(Data => A2P.To_Pointer (System.Address (Error)).all));
   end Basic_Log_Handler;

   procedure Set_Error_Function (Callback : Error_Handler_Type) is
   begin
      virSetErrorFunc (Null_Address, Basic_Log_Handler'Access);
      Error_Handler := Callback;
   end Set_Error_Function;

   procedure Suspend is
   begin
      Active := False;
   end Suspend;

   procedure Resume is
   begin
      Active := True;
   end Resume;

   function Log_Active
     return Boolean
   is
   begin
      return Active;
   end Log_Active;

end Virtada.errors;
