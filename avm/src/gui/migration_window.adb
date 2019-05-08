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
with Gtk.Spin_Button;    use Gtk.Spin_Button;
with Gtk.Button_Box;     use Gtk.Button_Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Label;          use Gtk.Label;
with Gtk.Progress_Bar;   use Gtk.Progress_Bar;

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
   
   -- We derive a new gtk_window type with migration data used
   -- by the callbacks. We cannot use global variables because
   -- different migration windows can be open at the same time
   type My_Window_Record is new Gtk_Window_Record with
      record
         Source_Host : Gtk_Label;
         Domain      : Gtk_Label;
         Target      : Gtk_Combo_Box_Text;
         VM          : VM_Type;
         Job         : Positive;
         Timeout     : Gtk_Spin_Button;
      end record;
   
   type My_Window is access all My_Window_Record'Class;
   
   procedure Gtk_New
      (Window   : out My_Window;
       The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel)
   is
   begin
      Window := new My_Window_Record;
      Gtk.Window.Initialize (Window, The_Type);
   end Gtk_New;

   package Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean);

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk_Widget_Record);

   
   procedure Close_Window (Win : access Gtk_Widget_Record'Class) is
   begin
      Win.Destroy;
   end Close_Window;
                  
   package Update_Progress is new Glib.Main.Generic_Sources
     (Gtk_Progress_Bar);
   use Update_Progress;
   
   function Timed_Progress_Updater (Progress : Gtk_Progress_Bar)
                                    return Boolean is
      Win         : My_Window;
      
   begin
      Win := My_Window (Progress.Get_Ancestor (Gtk.Window.Get_Type));

      -- check if job is active
      if Is_Running (Win.Job) then
         Progress.Set_Fraction (Gdouble (Win.VM.Get_Job_Progress));
         return True;
      else
         Win.Destroy;
         return False;
      end if;
   end Timed_Progress_Updater;
   
   procedure Start_Migration (Object : access Gtk_Widget_Record'Class) is

      Win         : My_Window;
      Box         : Gtk_Box;
      Label       : Gtk_Label;
      Progress    : Gtk_Progress_Bar;
      To_Host     : Hypervisor;
      Timed_Id    : G_Source_Id;
      Timeout     : Natural;
   begin
      -- Retrieve migration parameters stored in the window object
      Win := My_Window (Object.Get_Ancestor (Gtk.Window.Get_Type));

      --no error control yet
      Win.VM  := Get_Domain (Win.Source_Host.Get_Text,
                             Win.Domain.Get_Text);
      To_Host := Get_Host (Win.Target.Get_Active_Text);
      
      Timeout := Natural (Win.Timeout.Get_Value_As_Int);

      -- Start migration
      Win.Job := Migrate (Win.VM, To_Host, Timeout);
      
      -- Set a label with the migration message
      -- we must set it before raplacing the content of the window
      Gtk.Label.Gtk_New (Label);
      Label.Set_Text ("Migrating " & Win.Domain.Get_Text);

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

      Timed_Id := Timeout_Add (1000, Timed_Progress_Updater'Access, Progress);
      
      Win.Show_All;
   end Start_Migration;
      

   procedure Create_Migration_Window
     (Server_Name : String; Domain_Name : String) is
     
      Window         : My_Window;
      Box            : Gtk_Box;
      Grid           : Gtk_Grid;
      Label          : Gtk_Label;
      Buttons        : Gtk_Button_Box;
      Migrate_Button : Gtk_Button;
      Cancel_Button  : Gtk_Button;
      
      Target         : Gtk_Combo_Box_Text;
            
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
         Gtk.Combo_Box_Text.Gtk_New (Target);
         for I in Host_List'Range loop
            if Host_List (I).Server_Name /= Server_Name
              and then Host_List (I).Is_Connected then
               Target.Append_Text (Host_List (I).Server_Name);
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
         Target.Set_Active (0);
      end;

      Gtk_New (Window);
      Window.Target := Target;
      Window.Set_Name (Unique_Name);
--        Window.Set_Default_Size (400, 300);
      Window.Set_Title ("Migrate " & Domain_Name);
      Window.Set_Icon (Main_Icon.Icon);

      Return_Callback.Connect (Window, "delete_event", Delete'Access);

      Gtk.Box.Gtk_New_Vbox (Box);
      Window.Add (Box);
      
      Grid := Create_List_Grid;
      Box.Add (Grid);
            
      -- Domain to migrate
      Gtk.Label.Gtk_New (Label, "Migrate Domain :");
      Label.Set_Halign (Align_End);
      Gtk.Label.Gtk_New (Window.Domain, Domain_Name);
      Window.Domain.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Window.Domain, Label, Pos_Right);

      -- Source Host
      Gtk.Label.Gtk_New (Label, "From Host :");
      Label.Set_Halign (Align_End);
      Gtk.Label.Gtk_New (Window.Source_Host, Server_Name);
      Window.Source_Host.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Window.Source_Host, Label, Pos_Right);

      -- Target Host
      Gtk.Label.Gtk_New (Label, "To Host :");
      Label.Set_Halign (Align_End);
      Window.Target.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Window.Target, Label, Pos_Right);

      -- Timeout
      Gtk.Label.Gtk_New (Label, "Timeout :");
      Label.Set_Halign (Align_End);
      Gtk.Spin_Button.Gtk_New (Window.Timeout, 0.0, 3600.0, 30.0);
      Window.Timeout.Set_Halign (Align_Start);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Window.Timeout, Label, Pos_Right);

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
      
      Buttons.Add (Cancel_Button);
      Buttons.Add (Migrate_Button);
      Box.Add (Buttons);
      
      Window.Show_All;

   end Create_Migration_Window;

end Migration_Window;
