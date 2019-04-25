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
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with Ada.Interrupts.Names;                   use Ada.Interrupts.Names;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps; use Ada.Containers;
with Ada.Strings.Fixed.Less_Case_Insensitive;

with Virtada.Host;            use Virtada.Host;
with Virtada.Host.Domain;     use Virtada.Host.Domain;
with Virtada.Host.Interfaces;
with Virtada.Host.Storage_Pools;
use Virtada;


with Monitors.XMLTrees;       use Monitors.XMLTrees;

package Monitors.Structures is

   -- This package provides the Hypervisors static structure used to store the
   -- status of servers and the subroutines visibile to clients.
   -- Also it defines the Updater task that keeps the structure up to date

   subtype Unsigned_Long is Virtada.Unsigned_Long;
   subtype unsigned is Virtada.unsigned;
   subtype Domain_State is Virtada.Host.Domain.Domain_State;

   procedure Close_Monitors;
   -- Terminate all monitoring tasks. The UI must call Close_Monitors before
   -- ending the program

   function Monitor_Is_Active return Boolean;
   -- Check if the updater task is callable

   type Group_Type is limited private;

   procedure Groups_Iterate
     (Callback : not null access procedure (Name : String));

   procedure Create_Group (Group_Name : String);
   -- Create a new empty group
   -- exceptions: Group_Name_Error, Duplicate_Name

   function Get_Group (Group_Name : String) return Group_Type;
   -- return an object of type group_type from the name (case insentive)

   function System_Group (Group_Name : String) return Boolean;
   -- check if the group is  system group (i.e. default)

   function Is_Group (Group_Name : String) return Boolean;
   -- return true if exists a group with this name

   procedure Delete_Group (Group_Name : String);

   --
   type Hypervisor_Record is new Connect_Type with private;
   type Hypervisor is access all Hypervisor_Record;

   function Group (Server : not null access Hypervisor_Record) return String;

   type Hypervisor_Array is array (Positive range <>) of Hypervisor;

   function List_Servers (Element : Group_Type) return Hypervisor_Array;

   subtype Ask_Callback_Ptr is Virtada.Host.Ask_Callback_Ptr;

   procedure Set_Ask_User (Func : Ask_Callback_Ptr);

   procedure Set_Ask_Password (Func : Ask_Callback_Ptr);

   function Connect_Server (Server_Name : String) return Boolean;

   function Add_Server
     (Uri   : String;
      Group : String := "Default") return Boolean;
   function Disconnect_Server (Server_Name : String) return Boolean;

   function Server_Name (Server : not null access Hypervisor_Record) return String;
   procedure Remove_Host (Server_Name : String);
   function Get_Host (Server_Name : String) return Hypervisor;

   overriding procedure Disconnect (Server : in out Hypervisor_Record);


   type VM_Type is new Domain_Type with private;
   procedure Sync_Info (VM : in out VM_Type);

   type VM_Array is array (Positive range <>) of VM_Type;

   function List_VMs (Server : Hypervisor) return VM_Array;

   function Get_Domain
     (Server_Name : String;
      Domain_Name : String) return VM_Type;

   function Get_Domain_Tree (VM : VM_Type) return XMLTree.Tree;

   function State         (VM : VM_Type) return Domain_State;
   function State_Image   (VM : VM_Type) return String;
   function CPU_Usage     (VM : VM_Type) return Integer;
   function Max_Mem       (VM : VM_Type) return Unsigned_Long;
   function CPU_Time      (VM : VM_Type) return Long_Long_Integer;
   function Memory        (VM : VM_Type) return Unsigned_Long;
   function Disk_Read_PS  (VM : VM_Type) return Unsigned_Long_Long;
   function Disk_Write_PS (VM : VM_Type) return Unsigned_Long_Long;
   function Net_Rx_PS     (VM : VM_Type) return Unsigned_Long_Long;
   function Net_Tx_PS     (VM : VM_Type) return Unsigned_Long_Long;
   function Server        (VM : VM_Type) return String;
   function Start         (VM : VM_Type) return Boolean;

   function Migrate     (VM : VM_Type; To : Hypervisor)
                         return Positive;

   function Get_Job_Progress (VM : VM_Type) return Long_Float;
   -- return a value between 0.0 and 1.0

   function Is_Running (Job_Number : Positive) return Boolean;

   type Interface_Type is new Virtada.Host.Interfaces.Interface_Type with null record;
   type Interface_Array is array (Positive range <>) of Interface_Type;

   function Get_Interface_List
     (Connection : Hypervisor;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Interface_Array;

   type Storage_Pool_Type is new Virtada.Host.Storage_Pools.Storage_Pool_Type
   with null record;

   type Storage_Pool_Array is array (Positive range <>) of Storage_Pool_Type;

   type Volume_Type is new Virtada.Host.Storage_Pools.Volume_Type
   with null record;

   overriding function Get_Volume_By_Path (Connection : Connect_Type'Class;
                                           Path       : String)
                                           return Volume_Type;

   function Volume_Capacity (Volume : Volume_Type)
                             return Unsigned_Long_Long;

private
   protected Sighandle is
      procedure Handle_Sighup;
      pragma Attach_Handler (Handle_Sighup, SIGHUP);
      -- set a null handler for sighup otherwise one single dropped
      -- connection could kill the application
   end Sighandle;

   ------------------------------------------------
   -- Semaphore - avoid concurrent access to
   --             hypervisors structure
   ------------------------------------------------
   protected type Semaphore is
      procedure Release;
      entry Secure;
      entry Secure_Write;
   private
      Locked : Boolean := False;
      Count  : Natural := 0;
   end Semaphore;

   function Less (Left, Right : String) return Boolean renames
     Ada.Strings.Fixed.Less_Case_Insensitive;
   -- Function for case insensitive string comparation.

   type VM_Type is new Domain_Type with record
      Cpu_Usage    : Integer          := 0;
      Server       : Unbounded_String := Null_Unbounded_String;
      Info         : Domain_Info;
      Block_Reads  : Unsigned_Long_Long := 0;
      Block_Writes : Unsigned_Long_Long := 0;
      Block_RPS    : Unsigned_Long_Long := 0;
      Block_WPS    : Unsigned_Long_Long := 0;
      Net_Rx_Nr    : Unsigned_Long_Long := 0;
      Net_Tx_Nr    : Unsigned_Long_Long := 0;
      Net_Rx_PS    : Unsigned_Long_Long := 0;
      Net_Tx_PS    : Unsigned_Long_Long := 0;
      Poll_Time    : Long_Long_Integer  := 0;
   end record;

   package VM_Lists is new Ada.Containers.Doubly_Linked_Lists (VM_Type);

   type Hypervisor_Record is new Connect_Type with record
      Name     : Unbounded_String;
      Protocol : Unbounded_String;
      User     : Unbounded_String;
      Domains  : VM_Lists.List;
      Group    : Unbounded_String := Null_Unbounded_String;
      Mutex    : Semaphore;
   end record;

   overriding procedure Finalize (Object : in out Hypervisor_Record);

   overriding function Create (Uri : String) return Hypervisor_Record;

   package Hypervisor_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Hypervisor, Less);

   package Hypervisor_Ref_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Hypervisor_Map.Cursor, Less, Hypervisor_Map."=");

   type Group_Type is record
      Servers_Count : Natural                := 0;
      Servers_Ref   : Hypervisor_Ref_Map.Map := Hypervisor_Ref_Map.Empty_Map;
   end record;

   package Group_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String,
      Group_Type,
      Less);

   ---------------------------------
   -- Static data
   ---------------------------------
   Hypervisors : Hypervisor_Map.Map := Hypervisor_Map.Empty_Map;
   Groups      : Group_Maps.Map     := Group_Maps.Empty_Map;
   Hypervisors_Mutex : Semaphore;

   procedure Get_RW_Stats (VM : Domain_Type'Class;
                           BK_Read  : out Unsigned_Long_Long;
                           BK_Write : out Unsigned_Long_Long;
                           IF_RX    : out Unsigned_Long_Long;
                           IF_TX    : out Unsigned_Long_Long);

end Monitors.Structures;
