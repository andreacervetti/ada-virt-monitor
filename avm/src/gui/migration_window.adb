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
with Gtk.Window;         use Gtk.Window;
with Gtk.Box;            use Gtk.Box;
with Gtk.Grid;           use Gtk.Grid;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Button_Box;     use Gtk.Button_Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Label;          use Gtk.Label;
with Gtk.Progress_Bar;   use Gtk.Progress_Bar;

with Glib.Object;
with Glib.Main;
use Glib.Main;

with Gtkada.Dialogs;      use Gtkada.Dialogs;


with Simple_Callbacks; use Simple_Callbacks;
with Frame_Helpers;    use Frame_Helpers;

with Monitors.Structures; use Monitors.Structures;
use Monitors;

with Switch_To_Window;
with Main_Icon;
with Glib; use Glib;

package body Migration_Window is
   
   package Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean);

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk_Widget_Record);

   
   procedure Close_Window (Win : access Gtk_Widget_Record'Class) is
   begin
      Win.Destroy;
   end Close_Window;
      
   type Migration_Data_Record is
      record
         Source_Host : Gtk_Label;
         Domain      : Gtk_Label;
         Target      : Gtk_Combo_Box_Text;
      end record;
   
   package User_Migration_Data is
     new Glib.Object.User_Data (Migration_Data_Record);
   use User_Migration_Data;
   Migration_Data : constant String := "migration_data";
   
   package User_Domain_Data is
     new Glib.Object.User_Data (VM_Type);
   use User_Domain_Data;
   Domain_Data : constant String := "domain";
   
   package User_Job_Data is
     new Glib.Object.User_Data (Positive);
   use User_Job_Data;
   Job_Data : constant String := "jobnumber";
   
   package Update_Progress is new Glib.Main.Generic_Sources
     (Gtk_Progress_Bar);
   use Update_Progress;
   
   function Timed_Progress_Updater (Progress : Gtk_Progress_Bar)
                                    return Boolean is
      Win         : Gtk_Window;
      Job_Number  : Positive;
      Domain      : VM_Type;
      
   begin
      Win := Gtk_Window (Progress.Get_Ancestor (Gtk.Window.Get_Type));
      Job_Number := User_Job_Data.Get (Win, Job_Data);
      Domain := User_Domain_Data.Get (Win, Domain_Data);
      -- check if job is active
      if Is_Running (Job_Number) then
         Progress.Set_Fraction (Gdouble (Domain.Get_Job_Progress));
         return True;
      else
         Win.Destroy;
         return False;
      end if;
   end Timed_Progress_Updater;
   
   procedure Start_Migration (Object : access Gtk_Widget_Record'Class) is

      Win         : Gtk_Window;
      Box         : Gtk_Box;
      Label       : Gtk_Label;
      Progress    : Gtk_Progress_Bar;
      Data        : Migration_Data_Record;
      Domain      : VM_Type;
      To_Host     : Hypervisor;
      Job_Number  : Positive;
      Timeout_Id  : G_Source_Id;
   begin
      -- Retrieve migration parameters stored as user_data in the
      -- window object
      Win := Gtk_Window (Object.Get_Ancestor (Gtk.Window.Get_Type));

      Data := User_Migration_Data.Get (Win, Migration_Data);
      --no error control yet
      Domain := Get_Domain (Data.Source_Host.Get_Text,
                            Data.Domain.Get_Text);
      To_Host := Get_Host (Data.Target.Get_Active_Text);

      -- Start migration
      Job_Number := Migrate (Domain, To_Host);

      -- Store the job Number end domain as User_data in the window
      User_Job_Data.Set (Win, Job_Number, Job_Data);
      User_Domain_Data.Set (Win, Domain, Domain_Data);
      
      -- Set a label with the migration message
      -- we must set it before raplacing the content of the window
      Gtk.Label.Gtk_New (Label);
      Label.Set_Text ("Migrating " & Data.Domain.Get_Text);

      -- get the box container of the top level window
      Box := Gtk_Box (Win.Get_Child);
      
      Box.Destroy;
      -- create a new box an put in it a progress bar
      Gtk.Box.Gtk_New_Vbox (Box);
      Win.Add (Box);
      Box.Pack_Start (Label);
      
      Gtk.Progress_Bar.Gtk_New (Progress);
      Progress.Set_Show_Text (True);
      Box.Pack_Start (Progress);

      Timeout_Id := Timeout_Add (1000, Timed_Progress_Updater'Access, Progress);
      
      Win.Show_All;
   end Start_Migration;
      

   procedure Create_Migration_Window
     (Server_Name : String; Domain_Name : String) is
     
      Window         : Gtk_Window;
      Box            : Gtk_Box;
      Grid           : Gtk_Grid;
      Label          : Gtk_Label;
      Buttons        : Gtk_Button_Box;
      Migrate_Button : Gtk_Button;
      Cancel_Button  : Gtk_Button;
      
      Data           : Migration_Data_Record;
      
      Unique_Name    : String := 
        "domain:" & Server_Name & "/" & Domain_Name & "/migration";
            
      Dummy          : Message_Dialog_Buttons;

   begin
      -- check if a window with same name is open
      if Switch_To_Window (Unique_Name) then
         return;
      end if;

      declare
         -- This one liner sucks. TODO: write single function
         Host_List : Hypervisor_Array :=
           List_Servers (Get_Group (Group (Get_Host (Server_Name))));
         Count : Natural := 0;
      begin
         Gtk.Combo_Box_Text.Gtk_New (Data.Target);
         for I in Host_List'Range loop
            if Host_List (I).Server_Name /= Server_Name
              and then Host_List (I).Is_Connected then
               Data.Target.Append_Text (Host_List (I).Server_Name);
               Count := Count + 1;
            end if;
         end loop;
         -- if there are no connected servers return
         if Count = 0 then
            Dummy :=
                 Message_Dialog
                   ("There are no others connected server in group",
                    Dialog_Type => Error,
                    Buttons     => Button_OK);
            return;
         end if;
         Data.Target.Set_Active (0);
      end;

      Gtk.Window.Gtk_New (Window);
      Window.Set_Name (Unique_Name);
--        Window.Set_Default_Size (400, 300);
      Window.Set_Title ("Migrate " & Domain_Name);
      Window.Set_Icon (Main_Icon.Icon);

      Return_Callback.Connect (Window, "delete_event", Delete'Access);

      Gtk.Box.Gtk_New_Vbox (Box);
      Window.Add (Box);
      
      Grid := Create_List_Grid;
      Box.Add (Grid);
            
      Gtk.Label.Gtk_New (Label, "Migrate Domain :");
      Label.Set_Halign (Align_End);
      Gtk.Label.Gtk_New (Data.Domain, Domain_Name);
      Data.Domain.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Data.Domain, Label, Pos_Right);

      Gtk.Label.Gtk_New (Label, "From Host :");
      Label.Set_Halign (Align_End);
      Gtk.Label.Gtk_New (Data.Source_Host, Server_Name);
      Data.Source_Host.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Data.Source_Host, Label, Pos_Right);
      
      Gtk.Label.Gtk_New (Label, "To Host :");
      Label.Set_Halign (Align_End);
      Data.Target.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Data.Target, Label, Pos_Right);
      
      
      Gtk.Button_Box.Gtk_New (Buttons, Orientation_Horizontal);
      Buttons.Set_Layout (Buttonbox_End);
      
      Gtk.Button.Gtk_New_From_Stock (Cancel_Button, "gtk-cancel");
      Widget_Callback.Object_Connect 
        (Cancel_Button,
         "clicked",
         Widget_Callback.To_Marshaller(Close_Window'Access),
         Window);
      
      Gtk.Button.Gtk_New_With_Mnemonic (Migrate_Button, "Migrate");
      Widget_Callback.Connect
        (Migrate_Button,
         "clicked",
         Widget_Callback.To_Marshaller (Start_Migration'Access));
      
      -- Store the migration parameter in the main window as User_Data
      User_Migration_Data.Set (Window, Data, Migration_Data);

      Buttons.Add (Cancel_Button);
      Buttons.Add (Migrate_Button);
      Box.Add (Buttons);
      
      Window.Show_All;

   end Create_Migration_Window;

end Migration_Window;
