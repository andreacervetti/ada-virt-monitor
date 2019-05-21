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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libvirt_Host_Api;
with System; use System;

package Virtada.Host is

   ---------------
   -- host
   ---------------
   Connect_Failed, Close_Failed, Connect_Error : exception;

   type Connect_Type is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (Object : in out Connect_Type);

   function Create (Uri : String) return Connect_Type;
   -- create an empty (not connected) connection from a uri

   procedure Connect (Connection : in out Connect_Type;
                      Uri        : String := "");
--     procedure Connect (Connection : in out Connect_Type);
   -- connect a connection object with the uri passed
   -- or the uri contained in the object

   procedure Disconnect (Connection : in out Connect_Type);

   function Uri (Connection : Connect_Type) return String;
   --
   function Host_Name (Connection : Connect_Type) return String;
   -- Return the system hostname of the server running the hypervisor
   -- calling gethostname
   function Get_Type (Connection : Connect_Type) return String;
   --
   function Model (Connection : Connect_Type) return String;
   --
   function CPUs (Connection : Connect_Type) return Unsigned;
   --
   function Memory (Connection : Connect_Type) return Unsigned_Long;
   --
   function Get_Max_VCpus
     (Connection : Connect_Type; Type_Str : String := "") return Integer;
   --
   function Get_SysInfo (Connection : Connect_Type) return String;
   --
   function Is_Connected (Connection : Connect_Type) return Boolean;
   --
   function Is_Alive (Connection : Connect_Type) return Boolean;
   -- check if the connection is active
   function Get_Capabilities (Connection : Connect_Type) return String;
   --
   function Get_Domain_Capabilities (Connection : Connect_Type;
                                     Emulator : String;
                                     Arch : String;
                                     Machine : String;
                                     Virt_Type : String)
                                     return String;

   function Num_Of_Domains
     (Connection : Connect_Type;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Integer;
   -- returns the number of domain on the system

   type Ask_Callback_Ptr is access function (Prompt: String) return String;

   procedure Set_Ask_User (Func : Ask_Callback_Ptr);

   procedure Set_Ask_Password (Func : Ask_Callback_Ptr);

private
   use Libvirt_Host_Api;

   type Connect_Status is (Connected, Not_Connected);

   type Link_Type (Status : Connect_Status := Not_Connected) is record
      case Status is
         when Not_Connected =>
            null;
         when Connected =>
            Ptr      : virConnectPtr := virConnectPtr (Null_Address);
            HVType   : Unbounded_String;
            CPUs     : Interfaces.C.Unsigned;
            CPU_Type : Unbounded_String;
            Memory   : Unsigned_Long;
      end case;
   end record;

   type Connect_Type is new Ada.Finalization.Limited_Controlled with record
      Uri  : Unbounded_String := Null_Unbounded_String;
      Link : Link_Type;
   end record;

end Virtada.Host;

