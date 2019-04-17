-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2018 Andrea Cervetti                      --
--               Copyright (C) 2018 Homebrew Internet s.r.l.               --
--                                                                         --
-- This program is free software: you can redistribute it and/or modify    --
-- it under the terms of the GNU General Public License as published by    --
-- the Free Software Foundation, either version 3 of the License, or       --
-- (at your option) any later version.                                     --
--                                  secure                                       --
-- This program is distributed in the hope that it will be useful,         --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of          --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
-- GNU General Public License for more details.                            --
--                                                                         --
-- You should have received a copy of the GNU General Public License       --
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                         --
-----------------------------------------------------------------------------
with Ada.Exceptions;  use Ada.Exceptions;
with Monitors.Logger; use Monitors.Logger;
with Get_Monotonic_Time;
with Ada.Containers.Ordered_Maps;

package body Monitors.Structures.Updaters is
   -----------------------
   -- migration objects --
   -----------------------
   -- We build a map indexed by a prossive number containing a pointer
   -- to the worker task calling the migration API.
   -- Many migration jobs can run concurrently
   -- TODO: Decide how and when clean up the map
   protected Last_Job is
      procedure Get_New (New_Number : out Positive);
   private
      Last : Natural := 0;
   end Last_Job;

   protected body Last_Job is
      procedure Get_New (New_Number : out Positive) is
      begin
         Last := Last + 1;
         New_Number := Last;
      end Get_New;
   end Last_Job;

   task type Migration_Task (Position : Positive)
   is
      entry Migrate
        (Domain  : Domain_Type;
         To_Host : Connect_Type;
         To_Uri  : String);
   end Migration_Task;

   type Migration_Ptr is access Migration_Task;

   type Migration_Job_Record is
      record
         Worker : Migration_Ptr;
         Ok     : Boolean;
      end record;

   package Migration_Jobs_Map is new Ada.Containers.Ordered_Maps
     (Positive, Migration_Job_Record);

   Migration_Jobs : Migration_Jobs_Map.Map;

   task body Migration_Task is
      type Str_Ptr is access String;
      for Str_Ptr'Storage_Size use 1024;
      Dom : Domain_Type;
      Conn : access constant Connect_Type;
      Uri_Ptr : Str_Ptr;
   begin
      accept Migrate
        (Domain : Domain_Type; To_Host : Connect_Type; To_Uri : String) do
         Dom := Domain;
         Conn := To_Host'Unchecked_Access;
         if To_Uri = "" then
            Uri_Ptr := new String'("");
         else
            Uri_Ptr := new String'("tcp:" & To_Uri & ":49152");
         end if;
      end;
      begin
         Dom.Migrate (Conn.all, Uri_Ptr.all);
         Migration_Jobs (Position).Ok := True;
      exception
         when Migration_Error =>
            Migration_Jobs (Position).Ok := False;
      end;
   end Migration_Task;

   function Start_Migration_Job
     (Domain : Domain_Type; To_Host : Connect_Type; To_Uri : String := "")
      return Positive
   is
      Number : Positive;
   begin
      Last_Job.Get_New (Number);
      Migration_Jobs.Insert (Number,
                             (Worker => new Migration_Task (Number),
                              Ok     => False));
      Migration_Jobs (Number).Worker.Migrate
        (Domain, To_Host, To_Uri);
      return Number;
   end Start_Migration_Job;

   function Is_Running (Job_Number : Positive) return Boolean
   is
   begin
      return Migration_Jobs (Job_Number).Worker'Callable;
   end Is_Running;

   -------------
   -- Updater --
   -------------

   task body Updater is
      Exit_Request : Boolean := False;
      Dc           : VM_Lists.Cursor;
      use Hypervisor_Map;
      use VM_Lists;
   begin
      -- main loop.
      loop
         -- Periodically check if termination requested
         select
            accept Close do
               Exit_Request := True;
            end Close;
         or
            delay 1.0;
         end select;
         exit when Exit_Request;
         -- Loop through the list of hypervisor and upgrade status informations
         Hypervisors_Mutex.Secure;

         for Hc in Hypervisors.Iterate loop
            if Hypervisors (Hc).all.Is_Connected then
               if not Hypervisors (Hc).all.Is_Alive then
            -- the connection has been dropped, notify user and reset element
                  Display_Message
                    ("Server " & Key (Hc) & " closed the connection");
                  Hypervisors (Hc).all.Disconnect;
               else
                  begin
                     declare
                        VMs       : VM_Lists.List;
                        Domains   : Domain_Array :=
                          List_Domains (Hypervisors (Hc).all);
                        Found     : Boolean;
                        Info      : Domain_Info;
                        Poll_Time : Long_Long_Integer;
                        Next_Cursor     : VM_Lists.Cursor;
                        Bytes_Read      : Unsigned_Long_Long := 0;
                        Bytes_Written   : Unsigned_Long_Long := 0;
                        Bytes_RX        : Unsigned_Long_Long := 0;
                        Bytes_TX        : Unsigned_Long_Long := 0;
                     begin
                        -- Get a copy of the list of domains.
                        -- To avoid keeping the object locked we release the
                        -- lock and work on the copy
                        Hypervisors (Hc).Mutex.Secure;
                        VMs := Hypervisors (Hc).Domains;
                        Hypervisors (Hc).Mutex.Release;
                        -- reset domain pointers
                        -- remove disappeared domains from the list
                        Dc := VMs.First;
                        while Has_Element (Dc) loop
                           Next_Cursor := Next (Dc);
                           Reset (VMs (Dc), Found);
                           if not Found then
                              VMs.Delete (Dc);
                           end if;
                           Dc := Next_Cursor;
                        end loop;
                        -- upgrade domains and add new domains to the list
                        -- Lists are not ordered so we have to perform
                        -- a linear search
                        for I in Domains'Range loop
                           Dc    := VMs.First;
                           Found := False;
                           while Has_Element (Dc) loop
                              if UUID (VMs (Dc)) = UUID (Domains (I))
                              then
                                 Found := True;
                                 -- upgrade
                                 VMs (Dc).Sync_Info;
                                 exit;
                              end if;
                              Next (Dc);
                           end loop;
                           if not Found then
                              Bytes_Read := 0;
                              Bytes_Written := 0;
                              Bytes_RX := 0;
                              Bytes_TX := 0;
                              if Domains (I).Is_Active then
                                 Get_RW_Stats (Domains (I),
                                               Bytes_Read,
                                               Bytes_Written,
                                               Bytes_RX,
                                               Bytes_TX);
                              end if;
                              Info := Domains(I).Get_Info;
                              Poll_Time := Get_Monotonic_Time;
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
                                 Poll_Time   => Poll_Time,
                                 Cpu_Usage   => 0,
                                 Server      => Hypervisors (Hc).Name));
                           end if;
                        end loop;
                        Hypervisors (Hc).Mutex.Secure_Write;
                        Hypervisors (Hc).Domains := VMs;
                        Hypervisors (Hc).Mutex.Release;
                     end;
                  exception
                     when Error : Domain_Error =>
                        -- Something went wrong with the connection.
                        -- Maybe libvirtd stopped working. This will be
                        -- notified at next loop
                        Display_Message
                          ("Exception => " &
                             Exception_Name (Error) &
                             " " &
                             Exception_Message (Error));
                  end;
               end if; -- Alive
            end if; -- Connected
         end loop;
         Hypervisors_Mutex.Release;
      end loop;
   exception
      -- Something really bad happened
      -- we don't do anything to recover (like release the mutex) because
      -- we ever must discover the cause and fix the program
      when Error : others =>
         Display_Message
           ("Hypervisor updater task - Exception: " &
            Exception_Name (Error) &
            " " &
            Exception_Message (Error));
         Display_Message (Exception_Information (Error));

   end Updater;

end Monitors.Structures.Updaters;
