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
with Gtk.Notebook; use Gtk.Notebook;

package Viewport is

   Name_Col          : constant := 0;
   Icon_Col          : constant := 1;
   Status_Col        : constant := 2;
   ID_Col            : constant := 3;
   CPU_Time_Col      : constant := 4;
   CPU_Col           : constant := 5;
   Disk_Read_Col     : constant := 6;
   Disk_Write_Col    : constant := 7;
   Net_Rx_Col        : constant := 8;
   Net_Tx_Col        : constant := 9;
   Group_Col         : constant := 10;
   Hidden_Status_Col : constant := 11;

   procedure Initialize (Main_Viewport : in out Gtk_Notebook);
   procedure Create_Group (Group_Name : String);

end Viewport;
