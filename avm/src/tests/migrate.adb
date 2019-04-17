with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Directories;  use Ada.Directories;

with Virtada.Host;        use Virtada.Host;
with Virtada.Host.Domain; use Virtada.Host.Domain;
use Virtada;

---------------------------
-- test for migration api
---------------------------

procedure Migrate is
   
   Connection : Connect_Type;
   Connection2 : Connect_Type;

   Domain : Domain_Type;
   --------------------
   -- Migration_Task --
   --------------------
   task type Migration_Task is
      entry Migrate
        (Domain  : Domain_Type;
         To_Host : Connect_Type;
         To_Uri  : String);
      entry Status (Ok : out Boolean);
   end Migration_Task;
   
   task body Migration_Task is
      type Str_Ptr is access String;
      for Str_Ptr'Storage_Size use 1024;
      Dom : Domain_Type;
      Conn : access constant Connect_Type;
      Uri_Ptr : Str_Ptr;
      Completed : Boolean := True;
   begin
      accept Migrate
        (Domain : Domain_Type; To_Host : Connect_Type; To_Uri : String) do
         Dom := Domain;
         Conn := To_Host'Unchecked_Access;
         Uri_Ptr := new String'(To_Uri);
      end;
      begin
         Dom.Migrate (Conn.all, Uri_Ptr.all);
      exception
         when others =>
            Completed := False;
      end;
      select
         accept Status (Ok : out Boolean) do
            Ok := Completed;
         end;
      or
         terminate;
      end select;         
   end Migration_Task;

   ------------------
   -- Monitor_Task --
   ------------------
   task type Monitor_Task is
      entry Start  (Domain : Domain_Type);
      entry Report (Total, Progress : out Unsigned_Long_Long);
   end Monitor_Task;
   
   task body Monitor_Task is
      Dom        : Domain_Type;
      Job_Info   : Domain_Job_Info := (Job_None, 0,0,0,0,0,0,0,0,0,0,0);
   begin
      select
         accept Start (Domain : Domain_Type) do
            Dom := Domain;
         end;
      or
         terminate;
      end select;
      delay 1.0;
      loop 
         if Dom.Is_Active then
            select 
               accept Report (Total, Progress : out Unsigned_Long_Long)
               do
                  Total := Job_Info.Data_Total;
                  Progress := Job_Info.Data_Total - Job_Info.Data_Remaining;
               end;
            else
               Job_Info := Get_Job_Info (Domain);
               exit when Job_Info.C_Type not in Job_Bounded..Job_Unbounded;
               delay 0.5;
            end select;
         else
            exit;
         end if;
      end loop;
      select
         accept Report (Total, Progress : out Unsigned_Long_Long)
         do
            Total := Job_Info.Data_Total;
            Progress := Job_Info.Data_Total - Job_Info.Data_Remaining;
         end;
      or
         terminate;
      end select;
   exception
      when Domain_Error => null;
   end Monitor_Task;      
      
begin
   if Argument_Count /= 4 then
      Set_Exit_Status (Failure);
      Put_Line
        (Standard_Error,
         "usage: " & Simple_Name (Command_Name) & " domain source target altname");
      return;
   end if;
   
   -- connect to source server
   declare
      Uri : String := "qemu+ssh://root@" & Argument (2) & "/system";
   begin
      Connection.Connect (Uri);
      if not Is_Connected (Connection) then
         raise Connect_Failed;
      end if;
   end;
   -- connect to target server
   declare
      Uri : String := "qemu+ssh://root@" & Argument (3) & "/system";
   begin
      Connection2.Connect (Uri);
      if not Is_Connected (Connection2) then
         raise Connect_Failed;
      end if;
   end;

   -- search domain
   declare
      Domains : Domain_Array := List_Domains (Connection);
   begin
      for I in Domains'Range loop
         if Name (Domains (I)) = Argument (1) then
            Domain := Domains (I);
            goto Migration;
         end if;
      end loop;
   end;
   -- not found
   Put_Line (Standard_Error, "Domain not found");
   return;
   
   <<Migration>>
   if not Is_Active (Domain) then
      Put_Line (Standard_Error, "Domain is not running");
      return;
   end if;
   declare
--        Uri : String := "qemu+ssh://root@" & Argument (3) & "/system";
      Uri : String := "tcp:" & Argument (4) & ":49152";
--        Info : Domain_Job_Info;
      Total, Progress : Unsigned_Long_Long;
      Migration : Migration_Task;
      Monitor   : Monitor_Task;
      Ok : Boolean := False;
   begin
      Migration.Migrate (Domain, Connection2, Uri);
      Monitor.Start (Domain);
      Put_Line ("Migrating..");
      delay 2.0;
      Check : loop
         select
            Migration.Status (Ok);
            exit Check;
         else
            Monitor.Report (Total, Progress);
            if Total > 0 then
               Put (Unsigned_Long_Long'Image ((Progress * 100) / Total) &
                      "%  " & ASCII.CR);
            end if;
            delay 0.5;
         end select;
      end loop Check;
      if Ok then
--           Put_Line ("100%");
         New_Line;
      else
         New_Line;
         Put_Line ("Error");
      end if;
   end;

end Migrate;
