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
with Gtk.Grid;  use Gtk.Grid;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Combo_Box; use Gtk.Combo_Box;

package Frame_Helpers is

   function Create_Labeled_Frame (Title : String) return Gtk_Frame;

   function Create_List_Grid return Gtk_Grid;

   procedure Add_Entry_Row
     (Grid    : Gtk_Grid;
      Title    : String;
      Default : String  := "";
      Info    : String := "";
      View    : Boolean := False);

   procedure Add_Info_Row
     (Grid        : Gtk_Grid;
      Title        : String;
      Information  : String;
      Name         : String := "");

   procedure Add_Spinbutton_Row
     (Grid    : Gtk_Grid;
      Title    : String;
      Value   : Long_Float);

   procedure Add_Combo_Row
     (Grid     : Gtk_Grid;
      Title    : String;
      Combo    : access Gtk_Combo_Box_Record'Class;
      Name     : String := "");

end Frame_Helpers;
