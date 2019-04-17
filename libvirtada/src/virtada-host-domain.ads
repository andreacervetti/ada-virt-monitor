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
with Libvirt_Domain_Api; use Libvirt_Domain_Api;

package Virtada.Host.Domain is

   -----------------
   -- Domain
   -----------------
   Migration_Error : exception;

   type Domain_State is (No_State, Running, Blocked, Paused, Shutdown,
                         Shutoff, Crashed, Pm_Suspended, Unknown);

   type Domain_Type is tagged private;

   type Domain_Array is array (Positive range <>) of Domain_Type;

   function List_Domains
     (Connection : Connect_Type'Class;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Domain_Array;

   procedure Reset (Domain : in out Domain_Type; Found : out Boolean);
   -- Get a new virDomainPtr for the object
   -- Apparently virDomain object are not designed to be persistent
   -- Strange things happen trying to reuse it (e.g. the ID does not change
   -- when migrating a domain). So we have to refresh it

   function Create (Domain : Domain_Type) return Boolean;
   -- Start a VM

   function Shutdown (VM : Domain_Type) return Boolean;
   -- Controlled shutdown of a vm

   function Suspend (Domain : Domain_Type) return Boolean;
   -- Pause a VM

   function Resume (Domain : Domain_Type) return Boolean;
   -- restart a suspended VM

   function ID (Domain : Domain_Type) return Integer;
   -- Return the domain id as Integer

   function Is_Active (Domain : Domain_Type) return Boolean;

   function Name         (Domain   : Domain_Type) return String;

   function UUID         (Domain   : Domain_Type) return String;

   function Get_XML_Desc (Domain   : Domain_Type;
                          Secure   : Boolean := True;
                          Inactive : Boolean := False) return String;

   function Get_OS_Type (Domain   : Domain_Type) return String;

   procedure Migrate (Domain  : Domain_Type;
                      To_Host : Connect_Type;
                      To_Uri  : String := "");

   procedure Migrate
     (Domain : Domain_Type; To_Uri : String);
   -- Draft procedure: connection to virDomainMigrateToURI

   function Get_Domain_Stats (Domain : Domain_Type) return Typed_Params_List;

   type Domain_Info is record
      State    : Domain_State;
      Max_Mem  : Unsigned_Long;
      Memory   : Unsigned_Long;
      Num_CPUs : Integer;
      Cpu_Time : Long_Long_Integer;
   end record;

   function Get_Info (Domain : Domain_Type) return Domain_Info;

   type Domain_Job_Type is (Job_None, Job_Bounded, Job_Unbounded, Job_Completed,
                            Job_Failed, Job_Cancelled, Unknown);

   type Domain_Job_Info is record
      C_Type         : Domain_Job_Type;
      Time_Elapsed   : Unsigned_Long_Long;
      Time_Remaining : Unsigned_Long_Long;
      Data_Total     : Unsigned_Long_Long;
      Data_Processed : Unsigned_Long_Long;
      Data_Remaining : Unsigned_Long_Long;
      Mem_Total      : Unsigned_Long_Long;
      Mem_Processed  : Unsigned_Long_Long;
      Mem_Remaining  : Unsigned_Long_Long;
      File_Total     : Unsigned_Long_Long;
      File_Processed : Unsigned_Long_Long;
      File_Remaining : Unsigned_Long_Long;
   end record;

   function Get_Job_Info (Domain : Domain_Type) return Domain_Job_Info;

   type Domain_Block_Stats is record
      Read_Req    : Long_Long_Integer := 0;
      Read_Bytes  : Long_Long_Integer := 0;
      Write_Req   : Long_Long_Integer := 0;
      Write_Bytes : Long_Long_Integer := 0;
      Errs        : Long_Long_Integer := 0;
   end record;

   function "+" (Left, Right : Domain_Block_Stats) return Domain_Block_Stats;

   function Get_Block_Stats (Domain : Domain_Type; Disk : String := "")
                             return Domain_Block_Stats;

private

   type Domain_Type is new Ada.Finalization.Controlled with record
      Ptr         : VirDomainPtr;
      Initialized : Boolean := False;
      UUID_String : String (1 .. VIR_UUID_BUFLEN * 2 + 4);
   end record;

   overriding procedure Adjust (Object : in out Domain_Type);
   overriding procedure Finalize (Object : in out Domain_Type);

end Virtada.Host.Domain;
