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
with Gtk.Widget; use Gtk.Widget.Widget_List;
with Gtk.Window; use Gtk.Window;

function Switch_To_Window (Unique_Name : String) return Boolean is
   List : Gtk.Widget.Widget_List.Glist;
begin
   List := Gtk.Window.List_Toplevels;
   while List /= Null_List loop
      if Get_Data (List).Get_Name = Unique_Name then
         -- found: switch to the existing window
         Gtk_Window (Get_Data (List)).Present;
         return True;
      end if;
      List := Next (List);
   end loop;
   return False;
end Switch_To_Window;
