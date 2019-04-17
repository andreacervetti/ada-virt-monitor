-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2018 Andrea Cervetti                      --
--               Copyright (C) 2018 Homebrew Internet s.r.l.               --
--                                                                         --
-- This program is free software: you can redistribute it and/or modify    --
-- it under the terms of the GNU General Public License as published by    --
-- the Free Software Foundation, either version 3 of the License, or       --
-- (at your option) any later version.                                     --
--                                                                         --
-- This program is distributed in the hope that it will be useful,         --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of          --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
-- GNU General Public License for more details.                            --
--                                                                         --
-- You should have received a copy of the GNU General Public License       --
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                         --
-----------------------------------------------------------------------------
with Ada.Text_IO;
with Virtada.Errors; use Virtada.Errors;

package body Monitors.Logger is

   procedure Default_Logger (Str : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Str);
   end Default_Logger;

   type Logger_Pointer is access procedure (Str : String);

   protected Logger is
      procedure Display (Str : String);
      procedure Set_Logger (Callback : Logger_Pointer);
   private
      Cb_Pointer : Logger_Pointer := Default_Logger'Access;
   end Logger;

   protected body Logger is
      procedure Display (Str : String) is
      begin
         Cb_Pointer (Str);
      end Display;

      procedure Set_Logger (Callback : Logger_Pointer) is
      begin
         Cb_Pointer := Callback;
      end Set_Logger;
   end Logger;

   procedure Set_Logger (Callback : access procedure (Str : String)) is
   begin
      Logger.Set_Logger (Callback);
   end Set_Logger;

   procedure Display_Message (Message : String) is
   begin
      Logger.Display (Message);
   end Display_Message;

   procedure Libvirt_Error_Handler (Error : Libvirt_Error) is
   begin
      if Log_Active then
         Display_Message (Error.Code);
         Display_Message (Error.Domain);
         Display_Message (Error.Message);
      end if;
   end Libvirt_Error_Handler;

begin

   Set_Error_Function (Libvirt_Error_Handler'Access);

end Monitors.Logger;
