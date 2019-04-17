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

with Interfaces.C.Strings;    use Interfaces.C.Strings;
use Interfaces.C;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with System.Memory;
with Libvirt_Domain_Api; use Libvirt_Domain_Api;
with Libvirt_Common_Api; use Libvirt_Common_Api;
with Libvirt_Host_Api;   use Libvirt_Host_Api;
with Virtada.Errors;     use Virtada.Errors;

package body Virtada.Host.Domain is

   type Domain_Ptr_Array is array (size_t range <>) of aliased virDomainPtr;

   ----------
   -- Reset--
   ----------
   procedure Reset (Domain : in out Domain_Type; Found : out Boolean)
   is
      UUID           : Chars_Ptr;
      New_Domain_Ptr : virDomainPtr;
      Conn_Ptr       : virConnectPtr;
   begin
      Conn_Ptr := virDomainGetConnect (Domain.Ptr);
      if Conn_Ptr /= virConnectPtr (Null_Address) then
         UUID := New_String (Domain.UUID);
         -- No annoying error messages
         Suspend;
         New_Domain_Ptr := virDomainLookupByUUIDString (Conn_Ptr, UUID);
         Resume;
         Free (UUID);
         if New_Domain_Ptr /= virDomainPtr (Null_Address) then
            if virDomainFree (Domain.Ptr) /= -1 then
               Domain.Ptr := New_Domain_Ptr;
               Found :=  True;
               return;
            end if;
         end if;
      end if;
      Found := False;
   end Reset;

   ------------------
   -- List_Domains --
   ------------------
   function List_Domains
     (Connection : Connect_Type'Class;
      Active     : Boolean := True;
      Inactive   : Boolean := True)
      return Domain_Array
   is

      Flags      : virConnectListAllDomainsFlags := 0;
      List_Addr  : System.Address;
      Domain_Num : int;

   begin
      if Active then
         Flags := Flags or VIR_CONNECT_LIST_DOMAINS_ACTIVE;
      end if;

      if Inactive then
         Flags := Flags or VIR_CONNECT_LIST_DOMAINS_INACTIVE;
      end if;

      Domain_Num := virConnectListAllDomains (Connection.Link.Ptr,
                                              List_Addr'Address,
                                              Flags);

      if Domain_Num = -1 then
         raise Domain_Error;
      end if;

      declare
         Ptr_List : Domain_Ptr_Array (1 .. size_t (Domain_Num));
         for Ptr_List'Address use List_Addr;

         Domain_List : Domain_Array (1 .. Integer (Domain_Num));
         X           : size_t;
         UUID_Ptr    : chars_ptr;
      begin
         for I in Domain_List'Range loop
            X := size_t (I);
            -- get UUID
            UUID_Ptr := New_String (VIR_UUID_STRING_BUFLEN * " ");
            if virDomainGetUUIDString (Ptr_List (X), UUID_Ptr) = -1 then
               Free (UUID_Ptr);
               raise Domain_Error;
            end if;
            -- set domain
            Domain_List (I) := (Ada.Finalization.Controlled with
                                Ptr => Ptr_list (X),
                                Initialized => True,
                                UUID_String => Value (UUID_Ptr));
            Free (UUID_Ptr);
         end loop;

         System.Memory.Free (List_Addr);
         return Domain_List;
      exception
         when Domain_Error =>
            -- something bad happened. Release the memory allocated by
            -- virConnectListAllDomains before raising the exception again
            System.Memory.Free (List_Addr);
            raise;
      end;
   end List_Domains;

   ---------------------
   -- Get_Domain_Info --
   ---------------------
   function Get_Domain_Info (Domain : Domain_Type)
                             return virDomainInfo
   is
      Info     : aliased virDomainInfo;
      Info_Ptr : virDomainInfoPtr := Info'Unchecked_Access;
   begin
      if virDomainGetInfo (Domain.Ptr, Info_Ptr) = -1 then
         raise Domain_Error;
      end if;
      return Info;
   end Get_Domain_Info;

   ----------
   -- Name --
   ----------
   function Name (Domain : Domain_Type)
                  return String
   is
      Name_Ptr : chars_ptr;
   begin
      Name_Ptr := virDomainGetName (Domain.Ptr);
      if Name_Ptr = Null_Ptr then
         raise Domain_Error;
      else
         return Value (Name_Ptr);
      end if;
   end Name;

   ----------
   -- UUID --
   ----------
   function UUID (Domain : Domain_Type)
                  return String
   is
   begin
      return Domain.UUID_String;
   end UUID;

   --------
   -- ID --
   --------
   function ID (Domain : Domain_Type)
                return Integer
   is
      U_Tmp : Interfaces.C.unsigned;
   begin
      U_Tmp := virDomainGetID (Domain.Ptr);
      if U_Tmp > Interfaces.C.Unsigned (Integer'Last) then
         return -Integer ((U_Tmp xor -1) + 1);
      else
         return Integer (U_Tmp);
      end if;
   end ID;

   --------------
   -- Get_Info --
   --------------
   function Get_Info (Domain : Domain_Type)
                      return Domain_Info
   is
      Info  : virDomainInfo;
      State : Domain_State;
   begin
      Info := Get_Domain_Info (Domain);

      -- get state
      if Info.state in
        virDomainState'Pos (virDomainState'First) ..
        virDomainState'Pos (virDomainState'Last)
      then
         case virDomainState'Val (Info.state) is
            when VIR_DOMAIN_NOSTATE =>
               State := No_State;
            when VIR_DOMAIN_RUNNING =>
               State := Running;
            when VIR_DOMAIN_BLOCKED =>
               State := Blocked;
            when VIR_DOMAIN_PAUSED =>
               State := Paused;
            when VIR_DOMAIN_SHUTDOWN =>
               State := Shutdown;
            when VIR_DOMAIN_SHUTOFF =>
               State := Shutoff;
            when VIR_DOMAIN_CRASHED =>
               State := Crashed;
            when VIR_DOMAIN_PMSUSPENDED =>
               State := Pm_Suspended;
         end case;
      else
         State := Unknown;
      end if;

      return (State    => State,
              Max_Mem  => Info.maxMem,
              Memory   => Info.memory,
              Num_CPUs => Integer (Info.nrVirtCpu),
              -- Here we convert an unsigned long long to a Long_Long_Integer
              -- (signed). This means that there is a risk of constraint error.
              -- This is unlikely (a machine should have a CPU time greater
              -- than 68 years), but it should be fixed for formal stability.
              Cpu_Time => Long_Long_Integer
                (Info.CpuTime));
   end Get_Info;

   ------------------
   -- Get_Job_Info --
   ------------------
   function Get_Job_Info (Domain : Domain_Type)
                          return Domain_Job_Info
   is
      Info     : aliased virDomainJobInfo;
      Info_Ptr : virDomainJobInfoPtr := virDomainJobInfoPtr (Info'Address);
      C_Type   : Domain_Job_Type;
   begin
      if virDomainGetJobInfo (Domain.Ptr, Info_Ptr) = -1 then
         raise Domain_Error;
      end if;

      if Info.c_type in
        virDomainJobType'Pos (virDomainJobType'First) ..
        virDomainJobType'Pos (virDomainJobType'Last)
      then
         case virDomainJobType'Val (Info.c_type) is
            when VIR_DOMAIN_JOB_NONE =>
               C_Type := Job_None;
            when VIR_DOMAIN_JOB_BOUNDED =>
               C_Type := Job_Bounded;
            when VIR_DOMAIN_JOB_UNBOUNDED =>
               C_Type := Job_Unbounded;
            when VIR_DOMAIN_JOB_COMPLETED =>
               C_Type := Job_Completed;
            when VIR_DOMAIN_JOB_FAILED =>
               C_Type := Job_Failed;
            when VIR_DOMAIN_JOB_CANCELLED =>
               C_Type := Job_Cancelled;
         end case;
      else
         C_Type := Unknown;
      end if;

      return (C_Type         => C_Type,
              Time_Elapsed   => Unsigned_Long_Long(Info.timeElapsed),
              Time_Remaining => Unsigned_Long_Long(Info.timeRemaining),
              Data_Total     => Unsigned_Long_Long(Info.dataTotal),
              Data_Processed => Unsigned_Long_Long(Info.dataProcessed),
              Data_Remaining => Unsigned_Long_Long(Info.dataRemaining),
              Mem_Total      => Unsigned_Long_Long(Info.memTotal),
              Mem_Processed  => Unsigned_Long_Long(Info.memProcessed),
              Mem_Remaining  => Unsigned_Long_Long(Info.memRemaining),
              File_Total     => Unsigned_Long_Long(Info.fileTotal),
              File_Processed => Unsigned_Long_Long(Info.fileProcessed),
              File_Remaining => Unsigned_Long_Long(Info.fileRemaining) );

   end Get_Job_Info;

   ---------------------
   -- Get_Block_Stats --
   ---------------------
   function Get_Block_Stats (Domain : Domain_Type;
                             Disk   : String := "")
                             return Domain_Block_Stats
   is
      Stats     : aliased virDomainBlockStatsStruct;
      Disk_Ptr  : chars_ptr;
      Num       : int;
   begin
      if Disk = "" then
         Disk_Ptr := Null_Ptr;
      else
         Disk_Ptr := New_String (Disk);
      end if;

      Num := virDomainBlockStats (Domain.Ptr,
                                  Disk_Ptr,
                                  Stats'Unchecked_Access,
                                  Stats'Size / 8);
      if Disk_Ptr /= Null_Ptr then
         Free (Disk_Ptr);
      end if;

      if Num = -1 then
         raise Domain_Error;
      end if;

      return (Read_Req    => Stats.rd_req,
              Read_Bytes  => Stats.rd_bytes,
              Write_Req   => Stats.wr_req,
              Write_Bytes => Stats.wr_bytes,
              Errs        => Stats.errs);
   end Get_Block_Stats;

   ---------------------------
   -- "+" (block statistics --
   ---------------------------
   function "+" (Left, Right : Domain_Block_Stats)
                 return Domain_Block_Stats
   is
   begin
      return (Read_Req    => Left.Read_Req    + Right.Read_Req,
              Read_Bytes  => Left.Read_Bytes  + Right.Read_Bytes,
              Write_Req   => Left.Write_Req   + Right.Write_Req,
              Write_Bytes => Left.Write_Bytes + Right.Write_Bytes,
              Errs        => Left.Errs        + Right.Errs);
   end "+";
   ------------
   -- Create --
   ------------
   function Create (Domain : Domain_Type)
                    return Boolean
   is
   -- Start a VM
   begin
      case virDomainCreate (Domain.Ptr) is
         when 0 =>
            return True;
         when -1 =>
            return False;
         when others =>
            raise Domain_Error;
      end case;
   end Create;

   --------------
   -- Shutdown --
   --------------
   function Shutdown (VM : Domain_Type)
                      return Boolean
   is
   -- Shutdown a VM in a controlled manner
   begin
      case virDomainShutdown (VM.Ptr) is
         when 0 =>
            return True;
         when -1 =>
            return False;
         when others =>
            raise Domain_Error;
      end case;
   end Shutdown;

   -------------
   -- Suspend --
   -------------
   function Suspend (Domain : Domain_Type)
                     return Boolean
   is
   begin
      case virDomainSuspend (Domain.Ptr) is
         when 0 =>
            return True;
         when -1 =>
            return False;
         when others =>
            raise Domain_Error;
      end case;
   end Suspend;

   ------------
   -- Resume --
   ------------
   function Resume (Domain : Domain_Type)
                    return Boolean
   is
   begin
      case virDomainResume (Domain.Ptr) is
         when 0 =>
            return True;
         when -1 =>
            return False;
         when others =>
            raise Domain_Error;
      end case;
   end Resume;

   ---------------
   -- Is_Active --
   ---------------
   function Is_Active (Domain : Domain_Type)
                       return Boolean
   is
      Ret : int;
   begin
      -- Get active Flag
      Suspend;
      Ret := virDomainIsActive (Domain.Ptr);
      Resume;
      case Ret is
         when 0 =>
            return False;
         when 1 =>
            return True;
         when others =>
            raise Domain_Error;
      end case;
   end Is_Active;

   ------------
   -- Adjust --
   ------------
   overriding procedure Adjust (Object : in out Domain_Type) is
   begin
      if Object.Initialized
      then -- the object has been initialized
         if virDomainRef (Object.Ptr) = -1 then
            raise Domain_Error;
         end if;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (Object : in out Domain_Type) is
   begin
      if Object.Initialized then
         if virDomainFree (Object.Ptr) = -1 then
            raise Domain_Error;
         end if;
      end if;
   end Finalize;

   ------------------
   -- Get_XML_Desc --
   ------------------
   function Get_XML_Desc
     (Domain   : Domain_Type;
      Secure   : Boolean := True;
      Inactive : Boolean := False)
      return String
   is
      Desc  : chars_ptr;
      Flags : virDomainXMLFlags := 0;
   begin
      if Secure then
         Flags := Flags or VIR_DOMAIN_XML_SECURE;
      end if;
      if Inactive then
         Flags := Flags or VIR_DOMAIN_XML_INACTIVE;
      end if;

      Desc := virDomainGetXMLDesc (Domain.Ptr, Flags);

      if Desc = Null_Ptr then
         return "";
      else
         return Str : String := Value (Desc) do
            Free (Desc);
         end return;
      end if;
   end Get_XML_Desc;

   -----------------
   -- Get_OS_Type --
   -----------------
   function Get_OS_Type (Domain   : Domain_Type)
                         return String
   is
      Os_Type : chars_ptr;
   begin
      Os_Type := virDomainGetOSType (Domain.Ptr);
      if Os_Type = Null_Ptr then
         return "";
      end if;

      return Str : String := Value (Os_Type) do
         Free (Os_Type);
      end return;
   end Get_OS_Type;

   -------------
   -- Migrate --
   -------------
   -- These procedures are drafts
   procedure Migrate (Domain  : Domain_Type;
                      To_Host : Connect_Type;
                      To_Uri  : String := "")
   is
      Flags             : Unsigned
        := Unsigned (VIR_MIGRATE_LIVE or VIR_MIGRATE_PERSIST_DEST);
      Params            : virTypedParameterPtr := null;
      URI_Ptr           : chars_ptr := Null_Ptr;
      Nparams           : aliased int := 0;
      Max_Params        : aliased int := 0;
      Migrate_Param_Uri : chars_ptr := Null_Ptr;
      RC                : int;
      New_Domain        : virDomainPtr;

   begin
      if not Domain.Is_Active then
         raise Migration_Error;
      end if;

      if To_Uri /= "" then
         URI_Ptr := New_String (To_Uri);
         Migrate_Param_Uri := New_String (VIR_MIGRATE_PARAM_URI);
         Nparams := 1;
         RC := virTypedParamsAddString (Params'Address,
                                        Nparams'Access,
                                        Max_Params'Access,
                                        Migrate_Param_Uri,
                                        URI_Ptr);
         Free (Migrate_Param_Uri);
         Free (URI_Ptr);
      end if;

      New_Domain := virDomainMigrate3 (Domain.Ptr,
                                       To_Host.Link.Ptr,
                                       Params,
                                       Interfaces.C.Unsigned (Nparams),
                                       Flags);

      if Nparams = 1 then
         virTypedParamsFree (Params, 1);
      end if;

      if New_Domain /= VirDomainPtr (Null_Address) then
         if VirDomainFree (New_Domain) = 0 then
            null;
         end if;
      else
         raise Migration_Error;
      end if;

   end Migrate;

   procedure Migrate
     (Domain : Domain_Type; To_Uri : String)
   is
      URI_Ptr : chars_ptr;
      RC      : int;
      Flags   : Unsigned_Long
        := Unsigned_Long (VIR_MIGRATE_LIVE
                          or VIR_MIGRATE_PEER2PEER
                          or VIR_MIGRATE_PERSIST_DEST);
   begin
      if not Domain.Is_Active then
         raise Migration_Error;
      end if;

      URI_Ptr := New_String (To_Uri);

      RC := virDomainMigrateToURI (Domain.Ptr, URI_Ptr, Flags, Null_Ptr, 0);

      Free (URI_Ptr);

      if RC = -1 then
         raise Migration_Error;
      end if;
   end Migrate;

   ----------------------
   -- Get_Domain_Stats --
   ----------------------
   function Get_Domain_Stats (Domain : Domain_Type)
                              return Typed_Params_List
   is
      Domain_List : array (1..2) of VirDomainPtr
        := (Domain.Ptr, VirDomainPtr (Null_Address));
      pragma Convention (C, Domain_List);
      Stats       : Unsigned;
      Flags       : Unsigned;
      Count       : int;
      Retstats    : System.Address;
      DSR_Addr    : System.Address;

   begin
      -- These should be extended or parameterized
      Stats := VIR_DOMAIN_STATS_INTERFACE or VIR_DOMAIN_STATS_BLOCK;
--        Flags := VIR_CONNECT_GET_ALL_DOMAINS_STATS_ACTIVE
--          or VIR_CONNECT_GET_ALL_DOMAINS_STATS_INACTIVE;
      Flags := VIR_CONNECT_GET_ALL_DOMAINS_STATS_RUNNING;

      -- Call virDomainListGetStats with a list of one domain
      Count := virDomainListGetStats (Domain_List'Address,
                                      Stats,
                                      Retstats'Address,
                                      Flags);
      if Count = -1 then
         raise Domain_Error;
      end if;
      if Count = 0 then
         -- return an empty array
         declare
            Ret_Arr : Typed_Params_List (1 .. 0);
         begin
            return Ret_Arr;
         end;
      end if;
      declare
         type DSRL is array (int range  <>) of virDomainStatsRecordPtr;
         pragma Convention (C, DSRL);
         Ret_Stat_Array : DSRL (1 .. Count);
         for Ret_Stat_Array'Address use Retstats;
      begin
         DSR_Addr := System.Address (Ret_Stat_Array (1));
      end;
      declare
         Domain_Stat_Record : virDomainStatsRecord;
         pragma Import (C, Domain_Stat_Record);
         for Domain_Stat_Record'Address use Dsr_Addr;
         Params_Array : array (1 .. Domain_Stat_Record.nparams)
           of virTypedParameter;
         pragma Import (C, Params_Array);
         for Params_Array'Address use Domain_Stat_Record.params.all'Address;
         Params_List : Typed_Params_List
           (1 .. Integer (Domain_Stat_Record.nparams));
      begin
         for I in Params_Array'Range loop
            Params_List (Integer(I)).Name := Params_Array (I).Field;
            case Params_Array (I).c_type is
               when int (VIR_TYPED_PARAM_INT) =>
                  Params_List (Integer(I)).Value
                    := (TP_INT,  Params_Array (I).value.i);
               when int (VIR_TYPED_PARAM_UINT) =>
                  Params_List (Integer(I)).Value
                    := (TP_UINT, Params_Array (I).value.ui);
               when int (VIR_TYPED_PARAM_LLONG) =>
                  Params_List (Integer(I)).Value
                    := (TP_LLONG, Params_Array (I).value.l);
               when int (VIR_TYPED_PARAM_ULLONG) =>
                  Params_List (Integer(I)).Value
                    := (TP_ULLONG, Unsigned_Long_Long (Params_Array (I).value.ul));
               when int (VIR_TYPED_PARAM_DOUBLE) =>
                  Params_List (Integer(I)).Value
                    := (TP_DOUBLE, Params_Array (I).value.d);
               when int (VIR_TYPED_PARAM_BOOLEAN) =>
                  Params_List (Integer(I)).Value
                    := (TP_BOOLEAN, Params_Array (I).value.b);
               when int (VIR_TYPED_PARAM_STRING) =>
                  Params_List (Integer(I)).Value
                    := (TP_STRING, New_Char_Array (Value (Params_Array (I).value.s)));
               when others =>
                  null;
            end case;
         end loop;
         return Params_List;
      end;
   end Get_Domain_Stats;

end Virtada.Host.Domain;
