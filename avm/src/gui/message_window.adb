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
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues; use Ada.Containers;
with Ada.Strings.Unbounded;                        use Ada.Strings.Unbounded;
with Gtk.Text_View;                                use Gtk.Text_View;
with Gtk.Text_Buffer;                              use Gtk.Text_Buffer;
with Gtk.Text_Iter;                                use Gtk.Text_Iter;
with Glib;                                         use Glib;
with Glib.Main;

with Monitors.Structures; use Monitors.Structures;
with Monitors.Logger;     use Monitors.Logger;

package body Message_Window is

   package U_String_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Unbounded_String);

   package Message_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (U_String_Interface);
   use Message_Queues;

   Message_Queue : Queue;

   View   : Gtk_Text_View;
   Buffer : Gtk_Text_Buffer;

   Timeout_Func : Glib.Main.G_Source_Id;

   procedure Add_Line (Line : String) is
   begin
      Message_Queue.Enqueue (To_Unbounded_String (Line));
   end Add_Line;

   function Display_Line return Boolean is
      Line : Unbounded_String;
      Iter : Gtk_Text_Iter;
   begin
      if Message_Queue.Current_Use > 0 then
         for I in 1 .. Message_Queue.Current_Use loop
            Message_Queue.Dequeue (Line);
            Get_Iter_At_Offset (Buffer, Iter, -1);
            Insert (Buffer, Iter, To_String (Line) & ASCII.LF);
         end loop;
         Get_Iter_At_Offset (Buffer, Iter, -1);
         if Scroll_To_Iter (View, Iter, 0.0, False, 1.0, 1.0) then
            null;
         end if;
      end if;
      return True;
   end Display_Line;

   procedure Set_Message_View (Window : Gtk_Scrolled_Window) is
   begin
      Gtk_New (Buffer);
      Gtk_New (View, Buffer);
      Set_Editable (View, False);
      Set_Cursor_Visible (View, False);
      Add (Window, View);

      Set_Logger (Add_Line'Access);

      Timeout_Func := Glib.Main.Timeout_Add (500, Display_Line'Access);

   end Set_Message_View;

end Message_Window;
