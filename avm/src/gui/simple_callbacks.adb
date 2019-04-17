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
with Gtk.Main;
with Monitors.Structures; use Monitors.Structures;

package body Simple_Callbacks is

   function Delete (Object : access Gtk_Widget_Record'Class) return Boolean is
      pragma Unreferenced (Object);
   begin
      return False;
   end Delete;

   procedure Destroy (Object : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Close_Monitors;
      Gtk.Main.Main_Quit;
   end Destroy;

end Simple_Callbacks;
