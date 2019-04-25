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

with Ada.Text_IO; use Ada.Text_IO;

with Libvirt_Domain_Api; use Libvirt_Domain_Api;


package body Virtada.Host is

   type Credential_Array is
     array (Interfaces.C.unsigned range <>) of aliased virConnectCredential;
   pragma Convention (C, Credential_Array);

   function Auth_Callback (Cred   : virConnectCredentialPtr;
                           Ncred  : Interfaces.C.unsigned;
                           Cbdata : System.Address)
                           return int;
   pragma Convention (C, Auth_Callback);
   -- this routine is a translation of the default callbak in libvirt.c
   -- The difference is that the routines asking for username
   -- and password are external, so the user can replace them.

   function Ask_User_Default (Prompt : String)
                              return String
   is
   begin
      Put (Prompt & ": ");
      return Get_Line;
   end Ask_User_Default;

   function Ask_Password_Default (Prompt : String)
                                  return String
   is
      C      : Character;
      Buffer : String (1..1024);
      Len    : Natural := 0;
   begin
      Put (Prompt & ": ");
      loop
         Get_Immediate (C);
         exit when C = ASCII.CR;
         exit when C = ASCII.LF;
         if C = ASCII.BS then
            if Len > 0 then
               Len := Len -1;
            end if;
         else
            Len := Len + 1;
            Buffer (Len) := C;
         end if;
      end loop;
      New_Line;
      return Buffer (1..Len);
   end Ask_Password_Default;


   Ask_User     : Ask_Callback_Ptr := Ask_User_Default'Access;
   Ask_Password : Ask_Callback_Ptr := Ask_Password_Default'Access;

   procedure Set_Ask_User (Func : Ask_Callback_Ptr) is
   begin
      Ask_User := Func;
   end Set_Ask_User;

   procedure Set_Ask_Password (Func : Ask_Callback_Ptr) is
   begin
      Ask_Password := Func;
   end Set_Ask_Password;

   function Auth_Callback (Cred   : virConnectCredentialPtr;
                           Ncred  : Interfaces.C.unsigned;
                           Cbdata : System.Address)
                           return int
   is
      pragma Unreferenced (Cbdata);
      C_Type   : virConnectCredentialType;
      Cred_Arr : Credential_Array (1 .. Ncred);
      pragma Import (C, Cred_Arr);
      for Cred_Arr'Address use Cred.all'Address;
   begin

      for I in Cred_Arr'Range loop
         declare
            type Str_Ptr is access String;
            for Str_Ptr'Storage_Size use 1024;
            Buffer : Str_Ptr;
         begin
            C_Type := virConnectCredentialType'Val (Cred_Arr(I).C_Type);

            case C_Type is
            when VIR_CRED_EXTERNAL =>

               if String'(Value (Cred_Arr (I).challenge)) /= "PolicyKit" then
                  return -1;
               end if;

            when VIR_CRED_USERNAME | VIR_CRED_AUTHNAME |
                 VIR_CRED_ECHOPROMPT | VIR_CRED_REALM =>

               Buffer := new String'(Ask_User (Value (Cred_Arr(I).prompt)));

            when VIR_CRED_PASSPHRASE | VIR_CRED_NOECHOPROMPT =>

               Buffer := new String'(Ask_Password (Value(Cred_Arr(I).prompt)));

            when others =>
               return -1;
            end case;

            if C_Type /= VIR_CRED_EXTERNAL then
               if Buffer.all = ""
                 and then Cred_Arr (I).defresult /= Null_Ptr then
                  Cred_Arr (I).result
                    := New_Char_Array (Value (Cred_Arr (I).defresult));
               else
                  Cred_Arr (I).result := New_String (Buffer.all);
               end if;
               Cred_Arr (I).Resultlen
                 := Interfaces.C.Unsigned (Strlen (Cred_Arr (I).result));
            end if;

         exception
            when others => return -1;
         end;
      end loop;
      return 0;
   end Auth_Callback;

   Cred_Types     : aliased array (Unsigned range 1 .. 6)
     of aliased Int := (2, 6, 8, 5, 7, 9);
   pragma Convention (C, Cred_Types);

   Connect_Auth : aliased VirConnectAuth
     := (Cred_Types(1)'Access  , 6, Auth_Callback'Access, Null_Address);

   ------------------
   -- Connect_Open --
   ------------------
   function Connect_Open
     (Uri       : String;
      Read_Only : Boolean := False)
      return virConnectPtr
   is

      Host : chars_ptr;
      Temp : virConnectPtr := virConnectPtr (System.Null_Address);
      Flag : Interfaces.C.unsigned;
   begin
      Host := New_String (Uri);

      if Read_Only then
         Flag := 1;
      else
         Flag := 0;
      end if;

      Temp := virConnectOpenAuth (Host, Connect_Auth'Access, Flag);

      Free (Host);

      if Temp = virConnectPtr (System.Null_Address) then
         raise Connect_Failed;
      end if;

      return Temp;
   end Connect_Open;

   -------------
   -- Connect --
   -------------
   procedure Connect (Connection : in out Connect_Type;
                      Uri        : String := "")
   is
      Type_Ptr : chars_ptr;
      Info     : aliased virNodeInfo;
      Info_Ptr : virNodeInfoPtr := Info'Unchecked_Access;
      Num      : int;
      Link     : Link_Type;
   begin
      if Uri /= "" then
         Connection.Uri := To_Unbounded_String (Uri);
      end if;

      if Connection.Uri = Null_Unbounded_String
      then
         Connection.Link := (Status => Not_Connected);
         return;
      end if;

      Link := (Status   => Connected,
               Ptr      => Connect_Open (To_String (Connection.Uri)),
               HVType   => Null_Unbounded_String,
               CPUs     => 0,
               CPU_Type => Null_Unbounded_String,
               Memory   => 0);

      Connection.Link := Link;

      Type_Ptr := virConnectGetType (Connection.Link.Ptr);
      if Type_Ptr /= Null_Ptr then
         Connection.Link.HVType := To_Unbounded_String (Value (Type_Ptr));
      end if;

      Num := virNodeGetInfo (Connection.Link.Ptr, Info_Ptr);
      if Num = 0 then
         Connection.Link.CPU_Type
           := To_Unbounded_String (To_Ada (Info_Ptr.model));
         Connection.Link.CPUs   := Info_Ptr.cpus;
         Connection.Link.Memory := Info_Ptr.memory;
      end if;
   exception
      when Connect_Failed =>
         Connection.Link := (Status => Not_Connected);
   end Connect;

   ----------------
   -- Disconnect --
   ----------------
   procedure Disconnect (Connection : in out Connect_Type)
   is
      Ret_Code : int;
   begin
      if Connection.Link.Status = Connected then
         Ret_Code := virConnectClose (Connection.Link.Ptr);
         if Ret_Code = -1 then
            raise Close_Failed;
         end if;
         if Ret_Code > 0 then
            while Connection.Is_Alive loop
               exit when virConnectClose (Connection.Link.Ptr) < 1;
            end loop;
         end if;
         Connection.Link := Link_Type'(Status => Not_Connected);
      end if;
   end Disconnect;

   ---------------
   -- Host_Name --
   ---------------
   function Host_Name (Connection : Connect_Type)
                       return String
   is
      Hostname_Ptr : Chars_Ptr;
   begin
      if Connection.Link.Status = Connected then
         Hostname_Ptr := virConnectGetHostname (Connection.Link.Ptr);
         if Hostname_Ptr /= Null_Ptr then
            return Name : String := Value (Hostname_Ptr) do
               Free (Hostname_Ptr);
            end return;
         end if;
      end if;
      return "";
   end Host_Name;

   --------------
   -- Get_Type --
   --------------
   function Get_Type (Connection : Connect_Type)
                      return String
   is
   begin
      if Connection.Link.Status = Connected then
         return To_String (Connection.Link.HVType);
      else
         return "";
      end if;
   end Get_Type;

   -----------
   -- Model --
   -----------
   function Model (Connection : Connect_Type)
                   return String
   is
   begin
      if Connection.Link.Status = Connected then
         return To_String (Connection.Link.CPU_Type);
      else
         return "";
      end if;
   end Model;

   ----------
   -- CPUs --
   ----------
   function CPUs (Connection : Connect_Type)
                  return Unsigned
   is
   begin
      if Connection.Link.Status = Connected then
         return Unsigned (Connection.Link.CPUs);
      else
         return 0;
      end if;
   end CPUs;

   ------------
   -- Memory --
   ------------
   function Memory (Connection : Connect_Type)
                    return Unsigned_Long
   is
   begin
      if Connection.Link.Status = Connected then
         return Connection.Link.Memory;
      else
         return 0;
      end if;
   end Memory;

   -------------------
   -- Get_Max_VCpus --
   -------------------
   Function Get_Max_VCpus (Connection : Connect_Type;
                           Type_Str   : String := "")
                           return Integer
   is
      Type_Ptr : Chars_Ptr := Null_Ptr;
      Ret_Num : Integer := 0;
   Begin
      If Connection.Link.Status = Connected Then
         If Type_Str /= "" Then
            Type_Ptr := New_String (Type_Str);
         End If;
         Ret_Num := Integer(virConnectGetMaxVcpus(Connection.Link.Ptr, Type_Ptr));
         If Type_Ptr /= Null_Ptr Then
            Free (Type_Ptr);
         End If;
      End If;
      Return Ret_Num;
   End Get_Max_VCpus;

   -----------------
   -- Get_SysInfo --
   -----------------
   function Get_SysInfo (Connection : Connect_Type)
                         return String
   is
      Sys_Info : chars_ptr;
   begin
      if Connection.Link.Status = Connected then
         Sys_Info := virConnectGetSysinfo(Connection.Link.Ptr, 0);
         if Sys_Info /= Null_Ptr then
            return Str : String := Value (Sys_Info) do
               Free (Sys_Info);
            end return;
         end if;
      end if;
      return "";
   end Get_SysInfo;

   ------------------
   -- Is_Connected --
   ------------------
   function Is_Connected (Connection : Connect_Type)
                          return Boolean
   is
   begin
      return Connection.Link.Status = Connected;
   end Is_Connected;

   --------------
   -- Is_Alive --
   --------------
   function Is_Alive (Connection : Connect_Type)
                      return Boolean
   is
   begin
      if Connection.Link.Status = Not_Connected then
         return False;
      end if;
      -- why this is possible?????
      if Connection.Link.Ptr = virConnectPtr (Null_Address) then
         return False;
      end if;
      case virConnectIsAlive (Connection.Link.Ptr) is
         when 1 =>
            return True;
         when 0 | -1 =>
            return False;
         when others =>
            raise Connect_Failed; -- shouldn't happen
      end case;
   end Is_Alive;

   ---------
   -- Uri --
   ---------
   function Uri (Connection : Connect_Type)
                 return String
   is
   begin
      return To_String (Connection.Uri);
   end Uri;

   ----------------------
   -- Get_Capabilities --
   ----------------------
   function Get_Capabilities (Connection : Connect_Type)
                              return String
   is
      Capabilities : chars_ptr;
   begin
      if Connection.Link.Status = Connected then
         Capabilities := virConnectGetCapabilities (Connection.Link.Ptr);
         if Capabilities /= Null_Ptr then
            return Str : String := Value (Capabilities) do
               Free (Capabilities);
            end return;
         end if;
      end if;
      return "";
   end Get_Capabilities;

   --------------------------
   -- Get_Domain_Capabilities
   --------------------------
   function Get_Domain_Capabilities (Connection : Connect_Type;
                                     Emulator   : String;
                                     Arch       : String;
                                     Machine    : String;
                                     Virt_Type  : String)
                                     return String
   is
      Capabilities : chars_ptr;
      E, A, M, V   : chars_ptr;
   begin
      if Connection.Link.Status = Connected then

         E := New_String (Emulator);
         A := New_String (Arch);
         M := New_String (Machine);
         V := New_String (Virt_Type);

         Capabilities :=
           virConnectGetDomainCapabilities (Connection.Link.Ptr,
                                            E, A, M, V, 0);
         Free (E);
         Free (A);
         Free (M);
         Free (V);
         if Capabilities /= Null_Ptr then
            return Str : String := Value (Capabilities) do
               Free (Capabilities);
            end return;
         end if;
      end if;
      return "";
   end Get_Domain_Capabilities;

   --------------------
   -- Num_Of_Domains --
   --------------------
   function Num_Of_Domains (Connection : Connect_Type;
                            Active     : Boolean := True;
                            Inactive   : Boolean := True)
                            return Integer
   is
      Sum : Integer := 0;
      RC  : Int;
   begin
      if not Active and not Inactive then
         return 0;
      end if;
      if Connection.Link.Status = Connected then
         if Active then
            RC := virConnectNumOfDomains (Connection.Link.Ptr);
            if RC = -1 then
               raise Connect_Error;
            end if;
            Sum := Sum + Integer(RC);
         end if;
         if Connection.Link.Status = Connected then
            if Active then
               RC := virConnectNumOfDefinedDomains (Connection.Link.Ptr);
               if RC = -1 then
                  raise Connect_Error;
               end if;
               Sum := Sum + Integer (RC);
            end if;
         end if;
         return Sum;
      end if;
      return 0;
   end Num_Of_Domains;

   ------------
   -- Create --
   ------------
   function Create (Uri : String)
                    return Connect_Type
   is
   begin
      return (Ada.Finalization.Limited_Controlled with
         Uri  => To_Unbounded_String (Uri),
         Link => (Status => Not_Connected));
   end Create;

   overriding procedure Finalize (Object : in out Connect_Type) is
   begin
      if Object.Link.Status = Connected then
         while Object.Is_Alive loop
            exit when virConnectClose (Object.Link.Ptr) < 1;
         end loop;
      end if;
   end Finalize;

end Virtada.host;
