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
with Gtk.Window;          use Gtk.Window;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Enums;           use Gtk.Enums;

with Gtkada.Handlers; use Gtkada;

with Simple_Callbacks; use Simple_Callbacks;
with Main_Icon;   use Main_Icon;
with Switch_To_Window;

package body XML_Window is

   -------------------
   -- Create_Window --
   -------------------
   procedure Create_Window
     (Unique_Name :     String;
      Title       :     String;
      Window      : out Gtk_Window;
      Text_View   : out Gtk_Text_View)
   is
      Scrolled_Win : Gtk_Scrolled_Window;
   begin
      Gtk.Window.Gtk_New (Window);
      Window.Set_Name (Unique_Name);

      Window.Set_Title (Title);

      Window.Set_Default_Size (800, 600);

      Window.Set_Icon (Main_Icon.Icon);

      Handlers.Return_Callback.Connect (Window, "delete_event", Delete'Access);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Win);
      Scrolled_Win.Set_Shadow_Type (Shadow_In);

      Gtk.Text_View.Gtk_New (Text_View);
      Text_View.Set_Editable (False);

      Window.Add (Scrolled_Win);
      Scrolled_Win.Add (Text_View);
   end Create_Window;

   procedure Create_Sysinfo_XML_Window (Host : Hypervisor) is

      Unique_Name : String := "server:" & Host.Server_Name & "/Sysinfo";
      Title       : String := "SysInfo Host: " & Host.Server_Name;
      Window      : Gtk_Window;
      Text_View   : Gtk_Text_View;

   begin
      -- check if a window with same name is open
      if Switch_To_Window (Unique_Name) then
         return;
      end if;

      Create_Window (Unique_Name, Title, Window, Text_View);
      -- Get the XML Description from the domain
      Text_View.Get_Buffer.Set_Text (Host.Get_SysInfo);

      Window.Show_All;

   end Create_Sysinfo_XML_Window; -- Domain

   procedure Create_Domain_XML_Window (Domain : VM_Type) is

      Unique_Name : String :=
        "domain:" & Domain.Server & "/" & Domain.Name & "/XML";
      Title : String :=
        " XML Domain: " & Domain.Name & " Hypervisor: " & Domain.Server;
      Window    : Gtk_Window;
      Text_View : Gtk_Text_View;

   begin
      -- check if a window with same name is open
      if Switch_To_Window (Unique_Name) then
         return;
      end if;

      Create_Window (Unique_Name, Title, Window, Text_View);
      -- Get the XML Description from the domain
      Text_View.Get_Buffer.Set_Text (Domain.Get_XML_Desc);

      Window.Show_All;

   end Create_Domain_XML_Window; -- Domain

   procedure Create_Capabilities_XML_Window (Host : Hypervisor) is

      Unique_Name : String := "server:" & Host.Server_Name & "/Capabilities";
      Title       : String := "Capabilities Host: " & Host.Server_Name;
      Window      : Gtk_Window;
      Text_View   : Gtk_Text_View;

   begin
      -- check if a window with same name is open
      if Switch_To_Window (Unique_Name) then
         return;
      end if;

      Create_Window (Unique_Name, Title, Window, Text_View);
      -- Get the XML Description from the domain
      Text_View.Get_Buffer.Set_Text (Host.Get_Capabilities);

      Window.Show_All;
   end Create_Capabilities_XML_Window;

end XML_Window;
