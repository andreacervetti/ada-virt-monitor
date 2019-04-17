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
with Gtk.Window;
with Gtk.Scrolled_Window;
with Gtk.Frame;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Image_Menu_Item;
with Gtk.Menu;
with Gtk.Notebook;
with Gtk.Paned;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.Handlers; use Gtkada;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Dialog;      use Gtk.Dialog;
with Gtk.GEntry;      use Gtk.GEntry;
with Gtk.Label;       use Gtk.Label;
with Gtk.Stock;

with Simple_Callbacks;   use Simple_Callbacks;
with Viewport_Callbacks; use Viewport_Callbacks;
with Viewport;           use Viewport;
with Message_Window;     use Message_Window;
with Main_Icon;

with Monitors.Logger; use Monitors.Logger;
with Monitors.Structures; use Monitors.Structures;

package body Main_Window is

   Window        : Gtk.Window.Gtk_Window;

   -- These procedure are here so they have visibility to the main window
   function Ask_User (Prompt : String) return String
   is
      use Gtk.Stock;

      Dialog        : Gtk_Dialog;
      Ok_Button     : Gtk_Widget;
      Cancel_Button : Gtk_Widget;
      Label         : Gtk_Label;
      Gentry        : Gtk_GEntry;
      Response      : Gtk_Response_Type;

begin
      Gtk.Dialog.Gtk_New
        (Dialog => Dialog,
         Title  => "Enter User",
         Parent => Window,
         Flags  => 0);
      Cancel_Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Ok_Button     := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      Dialog.Set_Default (Ok_Button);

      Gtk.Label.Gtk_New (Label, Prompt);
      Pack_Start (Get_Content_Area (Dialog), Label);

      Gtk.GEntry.Gtk_New (GEntry);
      Gentry.Set_Activates_Default (True);
      Pack_Start (Get_Content_Area (Dialog), GEntry);

      Show_All (Dialog);

      Response := Dialog.Run;
      declare
         Ret : String := GEntry.Get_Text;
      begin
         Dialog.Destroy;

         if Response = Gtk_Response_OK then
            return Ret;
         else
            return "";
         end if;
      end;
   end Ask_User;

   function Ask_Password (Prompt : String) return String
   is
      use Gtk.Stock;

      Dialog        : Gtk_Dialog;
      Ok_Button     : Gtk_Widget;
      Cancel_Button : Gtk_Widget;
      Label         : Gtk_Label;
      Gentry        : Gtk_GEntry;
      Response      : Gtk_Response_Type;

begin
      Gtk.Dialog.Gtk_New
        (Dialog => Dialog,
         Title  => "Enter Password",
         Parent => Window,
         Flags  => 0);
      Cancel_Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Ok_Button     := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      Dialog.Set_Default (Ok_Button);

      Gtk.Label.Gtk_New (Label, Prompt);
      Pack_Start (Get_Content_Area (Dialog), Label);

      Gtk.GEntry.Gtk_New (GEntry);
      Gentry.Set_Activates_Default (True);
      GEntry.Set_Visibility (False);
      Pack_Start (Get_Content_Area (Dialog), GEntry);

      Show_All (Dialog);

      Response := Dialog.Run;
      declare
         Ret : String := GEntry.Get_Text;
      begin
         Dialog.Destroy;

         if Response = Gtk_Response_OK then
            return Ret;
         else
            return "";
         end if;
      end;
   end Ask_Password;

   procedure Create_Main_Window is

      Box           : Gtk.Box.Gtk_Box;
      Paned         : Gtk.Paned.Gtk_Paned;
      Menu_Bar      : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu_Item     : Gtk.Menu_Item.Gtk_Menu_Item;
      Image_Item    : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Menu          : Gtk.Menu.Gtk_Menu;
      Main_View     : Gtk.Notebook.Gtk_Notebook;
      Message_Frame : Gtk.Frame.Gtk_Frame;
      Messages      : Gtk.Scrolled_Window.Gtk_Scrolled_Window;

   begin
      Gtk.Window.Gtk_New (Window);
      Window.Set_Title ("AVM");
      Window.Set_Default_Size (800, 600);

      Window.Set_Icon (Main_Icon.Icon);
      -- connect to standard handlers in Simple_Callbacks
      Handlers.Return_Callback.Connect (Window, "delete_event", Delete'Access);
      Handlers.Widget_Callback.Connect (Window, "destroy", Destroy'Access);

      -- main work area
      Gtk.Box.Gtk_New_Vbox (Box);
      Window.Add (Box);

      -- main menu
      Gtk.Menu_Bar.Gtk_New (Menu_Bar);
      Box.Pack_Start (Menu_Bar, False);
      -- menu "File"
      Gtk.Menu_Item.Gtk_New (Menu_Item, "File");
      Menu_Bar.Append (Menu_Item);
      Gtk.Menu.Gtk_New (Menu);
      Menu_Item.Set_Submenu (Menu);
      --      File -> new Group
      Gtk.Image_Menu_Item.Gtk_New_From_Stock (Image_Item, "gtk-add", null);
      Image_Item.Set_Label ("New Group");
      Handlers.Widget_Callback.Connect
        (Image_Item,
         "activate",
         New_Group'Access);
      Menu.Append (Image_Item);

      --      File -> Quit
      Gtk.Image_Menu_Item.Gtk_New_From_Stock (Image_Item, "gtk-quit", null);
      Handlers.Widget_Callback.Connect
        (Image_Item,
         "activate",
         Destroy'Access);
      Menu.Append (Image_Item);
      ---------------------------
      -- Add sliding horizontal paned
      ---------------------------
      Gtk.Paned.Gtk_New_Vpaned (Paned);
      Box.Add (Paned);

      ---------------------------
      -- Create the main wiewport
      ---------------------------
      Gtk.Notebook.Gtk_New (Main_View);
      Viewport.Initialize (Main_View);

      ----------------------------
      -- message window
      ----------------------------
      Gtk.Frame.Gtk_New (Message_Frame);
      Message_Frame.Set_Shadow_Type (Shadow_In);
      Gtk.Scrolled_Window.Gtk_New (Messages);
      Set_Message_View (Messages);
      Message_Frame.Add (Messages);

      Paned.Add1 (Main_View);
      Paned.Add2 (Message_Frame);
      Paned.Set_Position (400);

      -- display window
      Window.Show_All;
      -- set dialogs for login
      Set_Ask_User (Ask_User'Access);
      Set_Ask_Password (Ask_Password'Access);

      -- Greet and check if monitor is active
      Display_Message ("Ada VirtMonitor started.");
      if Monitor_Is_Active then
         Display_Message ("Ok.");
      else
         Display_Message ("ERROR! Monitoring task is in abnormal state");
      end if;

   end Create_Main_Window;
end Main_Window;
