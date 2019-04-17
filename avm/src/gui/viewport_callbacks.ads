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
with Gtk.Widget;     use Gtk.Widget;
with Gtk.Tree_View;  use Gtk.Tree_View;
with Gdk.Event;      use Gdk.Event;
with Gtk.Menu_Item;  use Gtk.Menu_Item;
with Gtk.Tree_Model; use Gtk.Tree_Model;

package Viewport_Callbacks is

   procedure New_Connection
     (Item       : access Gtk_Menu_Item_Record'Class;
      Group_Name : String);

   procedure New_Group (Parent : access Gtk_Widget_Record'Class);

   procedure On_Row_Activated
     (Tree : access Gtk_Tree_View_Record'Class;
      Path : Gtk_Tree_Path);

   function Tree_View_Popup
     (Object : access Gtk_Tree_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;

end Viewport_Callbacks;
