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
with GNAT.Sockets;
with GNAT.Regpat;

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;
with Ada.Unchecked_Deallocation;

with Interfaces.C; use Interfaces.C;

with Monitors.Structures.Updaters;

with Monitors.Logger;   use Monitors.Logger;
with Monitors.XMLTrees; use Monitors.XMLTrees;

with Get_Monotonic_Time;

--  with Interfaces.C; use Interfaces.C;

package body Monitors.Structures is

   procedure Free is new Ada.Unchecked_Deallocation
     (Hypervisor_Record,
      Hypervisor);

   protected body Sighandle is
      procedure Handle_Sighup is
      begin
         null;
      end Handle_Sighup;
   end Sighandle;

   ----------------------------------------------------------------------------
   Major_Version : constant Integer := 0;
   Minor_Version : constant Integer := 1;
   ----------------------------------------------------------------------------
   Updater : access Monitors.Structures.Updaters.Updater;
   ----------------------------------------------------------------------------

   -- set the save file name.
   -- the path is set to the user home (see Initialize)
   -- TODO: lock the file
   Save_File_Full_Name : String :=
     Ada.Environment_Variables.Value ("HOME") & "/.ada-virt-monitor.sav";

   type Save_Hypervisor is record
      Name  : Unbounded_String;
      Uri   : Unbounded_String;
      Group : Unbounded_String;
   end record;

   package Server_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Save_Hypervisor);

   function Monitor_Is_Active return Boolean is
   begin
      return Updater.all'Callable;
   end Monitor_Is_Active;

   procedure Save is
      -- write data structures to save file
      use Group_Maps;
      use Hypervisor_Map;
      Save_File   : Ada.Streams.Stream_IO.File_Type;
      Save_Stream : Ada.Streams.Stream_IO.Stream_Access;
      CH          : Hypervisor_Map.Cursor := Hypervisors.First;
      CG          : Group_Maps.Cursor     := Groups.First;
      Server_List : Server_Lists.List;
      Save_Groups : Group_Maps.Map;
   begin
      Ada.Streams.Stream_IO.Create (Save_File, Name => Save_File_Full_Name);
      Save_Stream := Ada.Streams.Stream_IO.Stream (Save_File);

      Integer'Write (Save_Stream, Major_Version);
      Integer'Write (Save_Stream, Minor_Version);

      while Has_Element (CH) loop
         Server_List.Append
           (New_Item =>
              (Name  => Element (CH).Name,
               Uri   => To_Unbounded_String (Element (CH).Uri),
               Group => Element (CH).Group));
         Next (CH);
      end loop;
      while Has_Element (CG) loop
         Save_Groups.Insert
           (Key      => Group_Maps.Key (CG),
            New_Item => (0, Hypervisor_Ref_Map.Empty_Map));
         Next (CG);
      end loop;
      Group_Maps.Map'Write (Save_Stream, Save_Groups);
      Server_Lists.List'Write (Save_Stream, Server_List);

      Ada.Streams.Stream_IO.Close (Save_File);
   end Save;

   procedure Initialize is
      -- Initialize data structures with data loaded from the save file.
      -- Create default group if file does not exist or if versions
      -- are different.
      -- Automatically called at startup.
      use Ada.IO_Exceptions;
      use Server_Lists;
      use Group_Maps;
      Save_File   : Ada.Streams.Stream_IO.File_Type;
      Save_Stream : Ada.Streams.Stream_IO.Stream_Access;
      Major       : Integer := 0;
      Minor       : Integer := 0;
      Server_List : Server_Lists.List;
      C           : Server_Lists.Cursor;
      Server      : Hypervisor;
      Position    : Hypervisor_Map.Cursor;
      Inserted    : Boolean;
      Gc          : Group_Maps.Cursor;

   begin
      Ada.Streams.Stream_IO.Open
        (Save_File,
         Ada.Streams.Stream_IO.In_File,
         Save_File_Full_Name);
      Save_Stream := Ada.Streams.Stream_IO.Stream (Save_File);
      Integer'Read (Save_Stream, Major);
      Integer'Read (Save_Stream, Minor);
      -- Ignore if version is different, create default group
      if Major /= Major_Version or else Minor /= Minor_Version then
         Groups.Insert ("Default", (0, Hypervisor_Ref_Map.Empty_Map));
      else
         Group_Maps.Map'Read (Save_Stream, Groups);
         Server_Lists.List'Read (Save_Stream, Server_List);
         C := Server_List.First;
         while Has_Element (C) loop
            Server :=
              new Hypervisor_Record'(Create (To_String (Element (C).Uri)));
            Server.Group := Element (C).Group;
            Hypervisors.Insert
              (Key      => To_String (Element (C).Name),
               New_Item => Server,
               Position => Position,
               Inserted => Inserted);
            if Inserted then -- this should ever be true
               Gc := Groups.Find (To_String (Server.Group));
               if Has_Element (Gc) then
                  Groups (Gc).Servers_Ref.Insert
                    (Server_Name (Server), Position);
               end if;
            end if;

            Next (C);
         end loop;
      end if;

      Ada.Streams.Stream_IO.Close (Save_File);

   exception
         -- if cannot open file create default group
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Groups.Insert ("Default", (0, Hypervisor_Ref_Map.Empty_Map));
   end Initialize;

   ------------------------------------------------
   -- Semaphore - avoid concurrent access to
   --             hypervisors structure
   ------------------------------------------------
   protected body Semaphore is
      procedure Release is
      begin
         Locked := False;
         Count := Count - 1;
      end Release;

      entry Secure when not Locked is
      begin
         Count := Count + 1;
      end Secure;

      entry Secure_Write when not Locked and Count = 0 is
      begin
         Locked := True;
         Count := Count + 1;
      end Secure_Write;
   end Semaphore;

   -----------------
   -- Close_Monitors
   -----------------
   procedure Close_Monitors is
   begin
      select
         Updater.Close;
      or
         delay 10.0;
      end select;
   exception
      when Tasking_Error =>
         -- the task has already completed execution
         null;
   end Close_Monitors;

   ---------------------------------------
   -- Group Subroutines
   ---------------------------------------
   function Get_Group (Group_Name : String) return Group_Type is
   begin
      return Groups (Group_Name);
   end Get_Group;

   ---------------------------------------
   -- Hypervisor Subroutines
   ---------------------------------------
   ----------------
   -- Disconnect --
   ----------------
   overriding procedure Disconnect (Server : in out Hypervisor_Record) is
   begin
      if Server.Is_Connected then
         Server.Domains := VM_Lists.Empty_List;
      end if;
      Disconnect (Connect_Type (Server));
   end Disconnect;

   ------------------------
   -- Check_Connectivity --
   ------------------------
   procedure Check_Connectivity (Host : String; Protocol : String) is
      -- Check the availability of the server before trying to connect
      -- because the default timeout is too long
      use Ada.Strings.Fixed;
      use Ada.Characters.Handling;
      use GNAT.Sockets;
      use GNAT;
      Port      : Sockets.Port_Type;
      Server    : Sockets.Sock_Addr_Type;
      Socket    : Sockets.Socket_Type;
      Status    : Sockets.Selector_Status;
   begin
      if To_Lower (Protocol) = "qemu" then
         return;
      end if;
      if To_Lower (Protocol) = "esx" then
         Port := 443;
      elsif To_Lower (Protocol) = "qemu+ssh" then
         Port := 22;
      else
         raise Url_Error;
      end if;
      -- try as IP addr
      begin
         Server.Addr := Inet_Addr (Host);
      exception
         when Socket_Error =>
            -- try resolving as name
            Server.Addr := Addresses (Get_Host_By_Name (Host));
      end;
      Server.Port := Port;
      Create_Socket (Socket);
      Connect_Socket (Socket, Server, 5.0, Status => Status);
      case Status is
         when Completed => -- connection OK, do nothing
            null;
         when Expired =>
            raise Host_Unreachable;
         when Aborted =>
            raise Host_Unreachable;
      end case;
   exception
      when Host_Error =>
         raise Host_Not_Found;
   end Check_Connectivity;

   ----------------
   -- Add_Server --
   ----------------
   function Add_Server
     (Uri   : String;
      Group : String := "Default") return Boolean
   is
      Server : Hypervisor := null;
   begin
      -- search group
      if not Groups.Contains (Group) then
         raise Group_Not_Found;
      end if;

      Server := new Hypervisor_Record'(Create (Uri));

      -- check for duplicate connection
      if Hypervisors.Contains (Server_Name (Server)) then
         raise Duplicate_Name;
      end if;

      Check_Connectivity
        (Host     => To_String (Server.Name),
         Protocol => To_String (Server.Protocol));

      Server.Connect;

      if Server.Is_Connected then
         declare
            Position : Hypervisor_Map.Cursor;
            Inserted : Boolean;
            Gc       : Group_Maps.Cursor;
         begin
            Hypervisors_Mutex.Secure_Write;
            Server.Group := To_Unbounded_String (Group);
            Hypervisors.Insert
              (Key      => Server_Name (Server),
               New_Item => Server,
               Position => Position,
               Inserted => Inserted);

            if Inserted then -- this should ever be true
               Gc := Groups.Find (Group);
               Groups (Gc).Servers_Ref.Insert (Server_Name (Server), Position);
            end if;
            Hypervisors_Mutex.Release;
            Save;
         end;
      end if;
      return Server.Is_Connected;
   exception
      when Host_Unreachable | Host_Not_Found =>
         Free (Server);
         return False;
      when others =>
         if Server /= null then
            Free (Server);
         end if;
         raise;
   end Add_Server;

   -------------------------------------
   -- Set_Ask_User / Set_Ask_Password --
   -------------------------------------
   procedure Set_Ask_User (Func : Ask_Callback_Ptr) is
   begin
      Virtada.Host.Set_Ask_User (Func);
   end Set_Ask_User;

   procedure Set_Ask_Password (Func : Ask_Callback_Ptr) is
   begin
      Virtada.Host.Set_Ask_Password (Func);
   end Set_Ask_Password;

   --------------------
   -- Connect_Server --
   --------------------
   function Connect_Server (Server_Name : String)
                            return Boolean is
      use Hypervisor_Map;

      Pos : Hypervisor_Map.Cursor := Hypervisors.Find (Server_Name);
   begin
      if Pos = No_Element then
         raise Host_Not_Found;
      end if;
      if Element (Pos).Is_Connected then
         -- the server is connected - returns OK (for now)
         return True;
      end if;

      Check_Connectivity
        (Host     => To_String (Hypervisors (Pos).Name),
         Protocol => To_String (Hypervisors (Pos).Protocol));
--        Hypervisors_Mutex.Wait;
      Element (Pos).Connect;
--        Hypervisors_Mutex.Signal;
      if not Element (Pos).Is_Connected then
         return False;
      end if;
      declare
         use VM_Lists;
         VMs           : VM_Lists.List;
         Domains       : Domain_Array := List_Domains (Hypervisors(Pos).all);
         Poll_Time     : Long_Long_Integer;
         Info          : Domain_Info;
         Bytes_Read    : Unsigned_Long_Long := 0;
         Bytes_Written : Unsigned_Long_Long := 0;
         Bytes_RX      : Unsigned_Long_Long := 0;
         Bytes_TX      : Unsigned_Long_Long := 0;
      begin
         for I in Domains'Range loop
            Info := Domains(I).Get_Info;
            Poll_Time := Get_Monotonic_Time;
            if Domains (I).Is_Active then
               Get_RW_Stats (Domains (I),
                             Bytes_Read,
                             Bytes_Written,
                             Bytes_RX,
                             Bytes_TX);
            end if;
            VMs.Append
              ((Domain_Type'(Domains (I)) with
               Info         => Info,
               Block_Reads  => Bytes_Read,
               Block_Writes => Bytes_Written,
               Block_Rps    => <>,
               Block_Wps    => <>,
               Net_Rx_Nr    => Bytes_RX,
               Net_Tx_Nr    => Bytes_TX,
               Net_Rx_PS    => <>,
               Net_Tx_PS    => <>,
               Poll_Time    => Poll_Time,
               Cpu_Usage    => 0,
               Server       => Hypervisors (Pos).Name));
         end loop;
         Hypervisors (Pos).Mutex.Secure_Write;
         Hypervisors (Pos).Domains := VMs;
         Hypervisors (Pos).Mutex.Release;
      end;
      return True;
   end Connect_Server;

   -----------------------
   -- Disconnect_Server --
   -----------------------
   function Disconnect_Server (Server_Name : String) return Boolean is
      use Hypervisor_Map;
      Pos : Hypervisor_Map.Cursor := Hypervisors.Find (Server_Name);
   begin
      if Pos = No_Element then
         raise Host_Not_Found;
      end if;
      if not Element (Pos).Is_Connected then
         -- the server is disconnected - returns OK (for now)
         return True;
      end if;
      begin
         Hypervisors_Mutex.Secure_Write;
         -- This is strange. The implicit dereference
         -- (Hypervisors (Pos).Disconnect) does not work.
         Hypervisors (Pos).all.Disconnect;
         Hypervisors_Mutex.Release;
         return True;
      exception
         when Close_Failed =>
            Hypervisors_Mutex.Release;
            return False;
      end;
   end Disconnect_Server;

   --------------------
   -- Groups_Iterate --
   --------------------
   procedure Groups_Iterate
     (Callback : not null access procedure (Name : String))
   is
      procedure Call_Callback (Position : Group_Maps.Cursor) is
      begin
         Callback (Group_Maps.Key (Position));
      end Call_Callback;

   begin
      Groups.Iterate (Call_Callback'Access);
   end Groups_Iterate;

   --------------
   -- Is_Group --
   --------------
   function Is_Group (Group_Name : String) return Boolean is
   begin
      return Groups.Contains (Group_Name);
   end Is_Group;

   ------------------
   -- Create_Group --
   ------------------
   procedure Create_Group (Group_Name : String) is
      use Group_Maps;
      use Ada.Strings.Maps;
      use Ada.Strings.Fixed;
      use Ada.Characters.Handling;

      Allowed_Chars : constant Character_Set :=
        To_Set (Span => ('a', 'z')) or
        To_Set (Span => ('A', 'Z')) or
        To_Set (Span => ('0', '9')) or
        To_Set (Sequence => "_-.");

   begin
      -- check characters in entry
      if Group_Name = ""
        or else not Is_Letter (Group_Name (Group_Name'First))
        or else Index (Group_Name, Allowed_Chars, Test => Outside) /= 0
      then
         raise Group_Name_Error;
      end if;
      -- Check for duplicate name
      if Groups.Contains (Group_Name) then
         raise Duplicate_Name;
      end if;
      Groups.Insert (Group_Name, (0, Hypervisor_Ref_Map.Empty_Map));
      Save;
   end Create_Group;

   ------------------
   -- System_Group --
   ------------------
   function System_Group (Group_Name : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      if Groups.Contains (Group_Name) then
         return To_Upper (Group_Name) = "DEFAULT";
      else
         raise Group_Not_Found;
      end if;
   end System_Group;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (Group : Group_Type) return Boolean is
      use Hypervisor_Ref_Map;
   begin
      if Length (Group.Servers_Ref) > 0 then
         return False;
      else
         return True;
      end if;
   end Is_Empty;

   ------------------
   -- Delete_Group --
   ------------------
   procedure Delete_Group (Group_Name : String) is
      use Ada.Characters.Handling;
   begin
      if To_Upper (Group_Name) = "DEFAULT" then
         raise Cannot_Delete;
      end if;
      if not Is_Empty (Groups.Element (Group_Name)) then
         raise Cannot_Delete;
      end if;
      Groups.Delete (Group_Name);
      Save;
   exception
      when Constraint_Error =>
         raise Group_Not_Found;
   end Delete_Group;

   ------------------
   -- List_Servers --
   ------------------
   function List_Servers (Element : Group_Type) return Hypervisor_Array is
      use Hypervisor_Ref_Map;
      C  : Hypervisor_Ref_Map.Cursor := Element.Servers_Ref.First;
      CH : Hypervisor_Map.Cursor;
      R  : Hypervisor_Array (1 .. Integer (Element.Servers_Ref.Length));
   begin
      Hypervisors_Mutex.Secure;
      for I in R'Range loop
         exit when C = No_Element; -- just to be sure
         CH    := Hypervisor_Ref_Map.Element (C);
         R (I) := Hypervisor_Map.Element (CH);
         Next (C);
      end loop;
      Hypervisors_Mutex.Release;
      return R;
   end List_Servers;

   -----------------
   -- Server_Name --
   -----------------
   function Server_Name (Server : not null access Hypervisor_Record)
                         return String is
   begin
      return To_String (Server.Name);
   end Server_Name;

   --------------
   -- Get_Host --
   --------------
   function Get_Host (Server_Name : String) return Hypervisor is
   begin
      if not Hypervisors.Contains (Server_Name) then
         raise Host_Not_Found;
      end if;
      return Hypervisors.Element (Server_Name);
   end Get_Host;

   -----------------
   -- Remove_Host --
   -----------------
   procedure Remove_Host (Server_Name : String) is
      use Group_Maps;
   begin
      if not Hypervisors.Contains (Server_Name) then
         raise Host_Not_Found;
      end if;
      if Hypervisors (Server_Name).all.Is_Connected then
         raise Cannot_Delete;
      end if;
      Hypervisors_Mutex.Secure_Write;
      declare
         Group_Name : String := To_String (Hypervisors (Server_Name).Group);
         C          : Group_Maps.Cursor;
      begin
         C := Groups.Find (Group_Name);
         if Has_Element (C) then
            Groups (C).Servers_Ref.Delete (Server_Name);
         end if;
      end;
      Free (Hypervisors (Server_Name));
      Hypervisors.Delete (Server_Name);
      Save;
      Hypervisors_Mutex.Release;
   end Remove_Host;

   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (Object : in out Hypervisor_Record) is
   begin
      if Object.Is_Connected then
         Object.Domains := VM_Lists.Empty_List;
      end if;
      Finalize (Connect_Type (Object));
   end Finalize;

   -------------------
   -- Decompose_URI --
   -------------------
   procedure Decompose_URI
     (Uri      : String;
      Host     : out Unbounded_String;
      User     : out Unbounded_String;
      Protocol : out Unbounded_String)
   is
      -- decompose the URI using a regular expression
      use GNAT.Regpat;
      Pattern : String :=
      -- Begin of string
        "^" &
      -- scheme
        "([a-z][a-z0-9+.-]+):" &
      -- //
        "\/\/" &
      -- authority
        "(([a-zA-Z0-9-._~!$&'()*+,;=:]*)@)?" &
        "(" &
      -- hostname
        "([a-zA-Z](([a-zA-Z0-9-])?[a-zA-Z0-9]){0,31}" &
        "(\.[a-zA-Z](([a-zA-Z0-9-])?[a-zA-Z0-9]){0,31})*)" &
      -- or IPv4
        "|((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)" &
        "(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3})" &
        ")?" &
      -- path (unchecked)
        "(/([a-zA-Z0-9-])*)?" &
      -- parameters (unchecked)
        "(\?(.*))?" &
      -- End of string
        "$";
      Matcher : Pattern_Matcher := Compile (Pattern);
      Matches : Match_Array (0 .. 20);

      function "+" (Source : String) return Unbounded_String
                    renames To_Unbounded_String;
   begin

      Match (Matcher, Uri, Matches);
      if Matches (0) = No_Match then
         raise Url_Error;
      end if;
      if Matches (1) /= No_Match then
         Protocol := +(Uri (Matches (1).First .. Matches (1).Last));
      end if;
      if Matches (4) /= No_Match then
         Host := +(Uri (Matches (4).First .. Matches (4).Last));
      end if;
      if Matches (3) /= No_Match then
         User := +(Uri (Matches (3).First .. Matches (3).Last));
      end if;
   end Decompose_URI;

   ------------
   -- Create --
   ------------
   overriding function Create (Uri : String) return Hypervisor_Record is

      Host, User, Protocol : Unbounded_String;

   begin

      begin
         Decompose_URI (Uri, Host, User, Protocol);
      exception
         when Url_Error =>
            Display_Message ("Url_error in Create");
      end;

      return (Connect_Type'(Create (Uri)) with
                Name     => Host,
                Protocol => Protocol,
                User     => User,
                Group    => <>,
                Domains  => VM_Lists.Empty_List,
                Mutex => <>);
   end Create;

   --------------
   -- List_VMs --
   --------------
   function List_VMs (Server : Hypervisor) return VM_Array is
      use VM_Lists;
   begin
      Server.Mutex.Secure;
      declare
      C : VM_Lists.Cursor := First (Server.Domains);
      R : VM_Array (1 .. Integer (Length (Server.Domains)));
      begin
         for I in R'Range loop
            exit when C = No_Element; -- just to be sure
            R (I) := Server.Domains (C);
            Next (C);
         end loop;
         Server.Mutex.Release;
         return R;
      end;
   end List_VMs;

   ------------------------
   -- Get_Interface_List --
   ------------------------
   function Get_Interface_List
     (Connection : Hypervisor;
      Active     : Boolean := True;
      Inactive   : Boolean := True) return Netface_Array
   is
      I_Array : Interface_Array :=
        List_Interfaces (Connection.all, Active, Inactive);
      N_Array : Netface_Array (I_Array'Range);
   begin
      for I in I_Array'Range loop
         N_Array (I) := (I_Array (I) with null record);
      end loop;
      return N_Array;
   end Get_Interface_List;

   ----------------
   -- Get_Domain --
   ----------------
   function Get_Domain
     (Server_Name : String;
      Domain_Name : String) return VM_Type
   is
      use VM_Lists;
      C : VM_Lists.Cursor;
   begin
      if not Hypervisors.Contains (Server_Name) then
         raise Host_Not_Found;
      end if;
      Hypervisors (Server_Name).Mutex.Secure;
      C := First (Hypervisors (Server_Name).Domains);
      while Has_Element (C) loop
         -- Well, this is a little verbose
         if not Less (Name (Element (C)), Domain_Name)
           and then not Less (Domain_Name, Name (Element (C)))
         then
            Hypervisors (Server_Name).Mutex.Release;
            return Element (C);
         end if;
         Next (C);
      end loop;
      Hypervisors (Server_Name).Mutex.Release;
      raise Domain_Not_Found;
   end Get_Domain;

   -----------
   -- Group --
   -----------
   function Group (Server : not null access Hypervisor_Record) return String is
   begin
      return To_String (Server.Group);
   end Group;

   ------------------------------------------------
   -- VM subprograms
   ------------------------------------------------
   -----------------
   -- State_Image --
   -----------------
   function State_Image (State : Domain_State) return String is
   begin
      case State is
         when No_State =>
            return "no state";
         when Running =>
            return "running";
         when Blocked =>
            return "blocked";
         when Paused =>
            return "paused";
         when Shutdown =>
            return "shutdown";
         when Shutoff =>
            return "shutoff";
         when Crashed =>
            return "crashed";
         when Pm_Suspended =>
            return "pmsuspended";
         when Unknown =>
            return "unknown state";
      end case;
   end State_Image;

   ------------------
   -- Get_RW_Stats --
   ------------------
   procedure Get_RW_Stats (VM : Domain_Type'Class;
                           BK_Read  : out Unsigned_Long_Long;
                           BK_Write : out Unsigned_Long_Long;
                           IF_RX    : out Unsigned_Long_Long;
                           IF_TX    : out Unsigned_Long_Long)
   is
      Count         : Unsigned;
   begin
      BK_Read  := 0;
      BK_Write := 0;
      IF_RX    := 0;
      IF_TX    := 0;
      declare
         Params_List   : Typed_Params_List := VM.Get_Domain_Stats;
      begin
         -- Just for debugging....
         --              for I in Params_List'Range loop
         --                 Display_Message(Get_Name (Params_List (I)) & " " &
         --                                   Type_Image (Params_List (I)));
         --              end loop;
         -- Get disks count
         Count := Typed_Param_Get (Params_List, "block.count");
         -- Summarize disks usage
         for I in 0 .. Count - 1 loop
            BK_Read := BK_Read +
              Typed_Param_Get (Params_List, "block." &
                                 Trim (Unsigned'Image (I), Left) &
                                 ".rd.bytes");
            BK_Write := BK_Write +
              Typed_Param_Get (Params_List, "block." &
                                 Trim (Unsigned'Image (I), Left) &
                                 ".wr.bytes");
         end loop;
         -- get net interfaces count
         Count := Typed_Param_Get (Params_List, "net.count");
         -- summarize
         for I in 0 .. Count - 1 loop
            IF_RX := IF_RX +
              Typed_Param_Get (Params_List, "net." &
                                 Trim (Unsigned'Image (I), Left) &
                                 ".rx.bytes");
            IF_TX := IF_TX +
              Typed_Param_Get (Params_List, "net." &
                                 Trim (Unsigned'Image (I), Left) &
                                 ".tx.bytes");
         end loop;
      end;
   exception
         when Not_Found => return;
   end Get_RW_Stats;

   ---------------
   -- Sync_Info --
   ---------------
   procedure Sync_Info (VM : in out VM_Type)
   is
      use Monitors.XMLTrees.XMLTree;

      Usage         : Integer;
      Info          : Domain_Info;
      Poll_Time     : Long_Long_Integer;
      Bytes_Read    : Unsigned_Long_Long;
      Bytes_Written : Unsigned_Long_Long;
      Bytes_RX      : Unsigned_Long_Long;
      Bytes_TX      : Unsigned_Long_Long;
   begin
--        VM.Reset(Found);
      -- Get disks statistics;
      if VM.Is_Active then
         -- collect statistics
         Get_RW_Stats (VM,Bytes_Read, Bytes_Written, Bytes_RX, Bytes_TX);
         -- This computation is wrong (or at least imprecise)
         -- We should compute byte per second not byte per iteration
         if Bytes_Read /= 0 then
            VM.Block_RPS    := Bytes_Read - VM.Block_Reads;
            VM.Block_Reads  := Bytes_Read;
         end if;
         if Bytes_Written /= 0 then
            VM.Block_WPS    := Bytes_Written - VM.Block_Writes;
            VM.Block_Writes := Bytes_Written;
         end if;
         if Bytes_RX /= 0 then
            VM.Net_Rx_PS    := Bytes_RX - VM.Net_Rx_Nr;
            VM.Net_Rx_Nr    := Bytes_RX;
         end if;
         if Bytes_TX /= 0 then
            VM.Net_Tx_PS    := Bytes_TX - VM.Net_Tx_Nr;
            VM.Net_Tx_Nr    := Bytes_TX;
         end if;
      end if;

      Info      := VM.Get_Info;

      Poll_Time := Get_Monotonic_Time;

      if VM.Is_Active and then VM.Info.Cpu_Time > 0 then
         -- Compute the CPU usage. Usage is percentage * 100
         -- TODO check for correctness
         Usage := Integer
           ((((Info.Cpu_Time - VM.Info.Cpu_Time) * 10_000) /
            (Poll_Time - VM.Poll_Time)) /
              Long_Long_Integer (Info.Num_CPUs));
      else
         Usage := 0;
      end if;
      if VM.Info.State /= Info.State then
         Display_Message ("virtual machine " & VM.Name &
                            " changed status: " & VM.State_Image &
                            " -> " & State_Image (Info.State));
      end if;
      VM.Info := Info;
      VM.Poll_Time := Poll_Time;

      if Usage > 10_000 then
         VM.Cpu_Usage := 10_000;
      else
         VM.Cpu_Usage := Usage;
      end if;
   end Sync_Info;

   ---------------------
   -- Get_Domain_Tree --
   ---------------------
   function Get_Domain_Tree (VM : VM_Type) return XMLTree.Tree is
   begin
      return XML_To_Tree (VM.Get_XML_Desc);
   end Get_Domain_Tree;

   -----------
   -- State --
   -----------
   function State (VM : VM_Type) return Domain_State
   is
   begin
      return VM.Info.State;
   end State;

   -----------------
   -- State_Image --
   -----------------
   function State_Image (VM : VM_Type) return String is
      begin
         return State_Image (VM.Info.State);
      end State_Image;

   -------------
   -- Max_Mem --
   -------------
   function Max_Mem (VM : VM_Type) return Unsigned_Long is
   begin
      return VM.Info.Max_Mem;
   end Max_Mem;

   ------------
   -- Memory --
   ------------
   function Memory (VM : VM_Type) return Unsigned_Long is
   begin
      return VM.Info.Memory;
   end Memory;

   ---------------
   -- CPU_Usage --
   ---------------
   function CPU_Usage (VM : VM_Type) return Integer is
   begin
      return VM.Cpu_Usage;
   end CPU_Usage;

   --------------
   -- CPU_Time --
   --------------
   function CPU_Time (VM : VM_Type) return Long_Long_Integer is
   begin
      return VM.Info.Cpu_Time;
   end CPU_Time;

   ------------
   -- Server --
   ------------
   function Server (VM : VM_Type) return String is
   begin
      return To_String (VM.Server);
   end Server;

   -----------
   -- Start --
   -----------
   function Start (VM : VM_Type) return Boolean is
   begin
      return Create (VM);
   end Start;

   ------------------
   -- Disk_Read_PS --
   ------------------
   function Disk_Read_PS  (VM : VM_Type) return Unsigned_Long_Long is
   begin
      return VM.Block_RPS;
   end Disk_Read_PS;

   -------------------
   -- Disk_Write_PS --
   -------------------
   function Disk_Write_PS (VM : VM_Type) return Unsigned_Long_Long is
   begin
      return VM.Block_WPS;
   end Disk_Write_PS;

   ---------------
   -- Net_Rx_PS --
   ---------------
   function Net_Rx_PS  (VM : VM_Type) return Unsigned_Long_Long is
   begin
      return VM.Net_Rx_PS;
   end Net_Rx_PS;

   ---------------
   -- Net_Tx_PS --
   ---------------
   function Net_TX_PS  (VM : VM_Type) return Unsigned_Long_Long is
   begin
      return VM.Net_TX_PS;
   end Net_TX_PS;

   -------------
   -- Migrate --
   -------------
   function Migrate (VM : VM_Type; To : Hypervisor) return Positive
   is
   begin
      -- simply connect to function in Updaters
      return Updaters.Start_Migration_Job
        (Domain_Type (VM), Connect_Type (To.all));
   end Migrate;

   ----------------
   -- Is_Running --
   ----------------
   function Is_Running (Job_Number : Positive) return Boolean is
   begin
      return Updaters.Is_Running (Job_Number);
   end Is_Running;

   ----------------------
   -- Get_Job_Progress --
   ----------------------
   function Get_Job_Progress (VM : VM_Type) return Long_Float is
      Job_Info : Domain_Job_Info;
      Progress : Unsigned_Long_Long;
   begin
      if VM.Is_Active then
         Job_Info := VM.Get_Job_Info;
         if Job_Info.C_Type not in Job_Bounded..Job_Unbounded
           or else Job_Info.Data_Total = 0 then
            return 0.0;
         end if;
         Progress := Job_Info.Data_Total - Job_Info.Data_Remaining;
         return (Long_Float (Progress) / Long_Float (Job_Info.Data_Total));
      else
         return 0.0;
      end if;
   exception
      when others =>
         return 0.0;
   end Get_Job_Progress;


begin

   Initialize;
   -- start updater task
   Updater := new Monitors.Structures.Updaters.Updater;

end Monitors.Structures;
