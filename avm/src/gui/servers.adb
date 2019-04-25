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
with Gtk.Box;
with Gtk.Label;
with Gtk.Notebook;
with Gtk.Grid;
with Gtkada.Handlers; use Gtkada;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;

with Formats;
with Frame_Helpers; use Frame_Helpers;

with Simple_Callbacks; use Simple_Callbacks;
with Switch_To_Window;
with Main_Icon;

with Monitors.Structures; use Monitors.Structures;
use type Monitors.Structures.Unsigned_Long;

package body Servers is

   procedure Create_Server_Window (Server_Name : String) is

      Server      : Hypervisor;
      Window      : Gtk.Window.Gtk_Window;
      VBox        : Gtk.Box.Gtk_Vbox;
      Grid        : Gtk.Grid.Gtk_Grid;
      Unique_Name : String := "server:" & Server_Name;

      Notebook : Gtk.Notebook.Gtk_Notebook;
      Title    : Gtk.Label.Gtk_Label;

   begin
      -- check if a window with same name is open
      if Switch_To_Window (Unique_Name) then
         return;
      end if;

      -- get the server
      Server := Get_Host (Server_Name);

      -- create window
      Gtk.Window.Gtk_New (Window);
      Window.Set_Name (Unique_Name);
      Window.Set_Title ("Server: " & Server_Name);
      Window.Set_Icon (Main_Icon.Icon);

      Handlers.Return_Callback.Connect (Window, "delete_event", Delete'Access);

      -- main notebook
      Gtk.Notebook.Gtk_New (Notebook);
      Window.Add (Notebook);
      -- general informations box
      Gtk.Box.Gtk_New_Vbox (VBox, Homogeneous => False);
      Gtk.Label.Gtk_New (Title);
      Title.Set_Text ("General");
      Notebook.Append_Page (VBox, Title);

      Grid := Create_List_Grid;
      VBox.Add (Grid);

      Add_Info_Row (Grid, "Group:", Group (Server));
      Add_Info_Row (Grid, "URI:", Server.Uri);
      Add_Info_Row (Grid, "Hostname:", Server.Host_Name);
      Add_Info_Row (Grid, "Type:", Server.Get_Type);
      Add_Info_Row (Grid, "CPUs:", Trim (Unsigned'Image (Server.CPUs), Left));
      Add_Info_Row (Grid, "Memory:", Formats.Memory_Size (Server.Memory));
      Add_Info_Row (Grid, "Model:", Server.Model);

      declare
         Int_Array : Interface_Array := Get_Interface_List (Server);
      begin
         -- Interfaces box
         Gtk.Box.Gtk_New_Vbox (VBox, Homogeneous => False);
         Gtk.Label.Gtk_New (Title);
         Title.Set_Text ("Interfaces");
         Notebook.Append_Page (VBox, Title);

         Grid := Create_List_Grid;
         VBox.Add (Grid);

         for I in Int_Array'Range loop
            Add_Info_Row
              (Grid,
               "Interface" & Integer'Image (I) & ": ",
               Int_Array (I).Get_Name);
         end loop;
      end;
      Window.Show_All;

   end Create_Server_Window;

end Servers;
