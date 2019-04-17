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
with Terminal_Interface.Curses;         use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Text_IO; use Terminal_Interface.Curses.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
use Ada.Strings;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

-- with Monitors; use Monitors;
with Monitors.Structures; use Monitors.Structures;
with Monitors.Logger;     use Monitors.Logger;
with Formats;
use Monitors;

procedure Linemonitor is

   type Ragged_Array is array (Positive range <>) of Unbounded_String;

   type Status_Type is (Starting, Normal, Ending);

   Status : Status_Type := Starting;

   procedure Add_Group (Params : Ragged_Array);
   procedure Change_Current (Params : Ragged_Array);
   procedure Connect_Host (Params : Ragged_Array);
   procedure Reconnect (Params : Ragged_Array);
   procedure Disconnect (Params : Ragged_Array);
   procedure Show_Host (Params : Ragged_Array);
   procedure Group_Delete (Params : Ragged_Array);
   procedure Remove_Host (Params : Ragged_Array);
   procedure Help (Params : Ragged_Array);
   procedure List_Groups (Params : Ragged_Array);
   procedure Quit (Params : Ragged_Array);

   type Callback_Ptr is access procedure (Params : Ragged_Array);

   type Command_Record is record
      Command      : Unbounded_String;
      Help_Message : Unbounded_String;
      Callback     : Callback_Ptr;
   end record;

   function Tus
     (Source : String) return Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   type Command_List_Array is array (Positive range <>) of Command_Record;

   Command_List : Command_List_Array :=
     ((Tus ("add"), Tus (" groupname - add a new group"), Add_Group'Access),
      (Tus ("cg"), Tus (" - change current group"), Change_Current'Access),
      (Tus ("connect"), Tus (" url - connect new host"), Connect_Host'Access),
      (Tus ("reconnect"),
       Tus (" hostname - reconnect host in current group"),
       Reconnect'Access),
      (Tus ("disconnect"),
       Tus (" hostname - disconnect host in current group"),
       Disconnect'Access),(Tus ("show"),
       Tus (" hostname - show information about hostname"),
       Show_Host'Access),
      (Tus ("delete"), Tus (" groupname - delete group"), Group_Delete'Access),
      (Tus ("remove"), Tus (" hostname - remove host"), Remove_Host'Access),
      (Tus ("help"), Tus (" - print help (this message)"), Help'Access),
      (Tus ("list"), Tus (" - list groups"), List_Groups'Access),
      (Tus ("quit"), Tus (" - exit program"), Quit'Access));

   Out_Window : Window;
   In_Window  : Window;

   Current_Group : Unbounded_String := To_Unbounded_String ("Default");

   procedure Message_Logger (Str : String) is
   begin
      New_Line (Out_Window);
      Put (Out_Window, Str);
      Refresh (Out_Window);
      Refresh (In_Window);
   end Message_Logger;

   procedure Help (Params : Ragged_Array) is
      pragma Unreferenced (Params);
   begin
      Display_Message ("Commands:");
      for I in Command_List'Range loop
         Display_Message
           (To_String
              (Command_List (I).Command & Command_List (I).Help_Message));
      end loop;
   end Help;

   procedure List_Groups (Params : Ragged_Array) is
      pragma Unreferenced (Params);

      procedure Display (Group : String) is
      begin
         if To_Upper (To_String (Current_Group)) = To_Upper (Group) then
            Display_Message (Group & " <- current");
         else
            Display_Message (Group);
         end if;
         declare
            L : Hypervisor_Array := List_Servers (Get_Group (Group));
         begin
            for I in L'Range loop
               if L (I).Is_Connected then
                  Display_Message
                    (Integer'Image (I) &
                     " " &
                     Server_Name (L (I)) &
                     " " &
                     "connected");
                  Display_Message (" " & "Hostname: " & Server_Name (L (I)));
                  -- list domains
                  declare
                     Domains : VM_Array := List_VMs (L (I));
                  begin
                     if Domains'Length > 0 then
                        Display_Message ("    Domains:");
                        for J in Domains'Range loop
                           if Is_Active (Domains (J)) then
                              Display_Message
                                ("    " &
                                 Integer'Image (ID (Domains (J))) &
                                 " " &
                                 Name (Domains (J)) &
                                 " running");
                           else
                              Display_Message
                                ("    " &
                                 Integer'Image (ID (Domains (J))) &
                                 " " &
                                 Name (Domains (J)) &
                                 " not running");
                           end if;
                        end loop;
                     end if;
                  end;
               else
                  Display_Message
                    (Integer'Image (I) &
                     " " &
                     Server_Name (L (I)) &
                     " " &
                     "not connected");
               end if;
            end loop;
         end;
      end Display;

   begin
      Display_Message ("Groups:");
      Groups_Iterate (Display'Access);
   end List_Groups;

   procedure Add_Group (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: add groupname");
         return;
      end if;
      declare
         Group_Name : String := To_String (Params (Params'First));
      begin
         Create_Group (Group_Name);
         Display_Message ("OK");
      exception
         when Group_Name_Error =>
            Display_Message
              ("Group name error!" &
               LF &
               "A group name should begin with an " &
               "alphabetic character and be composed" &
               LF &
               "only by letters (a-z, A-Z), " &
               "numbers (0-9) and by the characters " &
               """_-."".");
         when Duplicate_Name =>
            Display_Message ("Error: Group exists");
      end;
   end Add_Group;

   procedure Group_Delete (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: delete groupname");
         return;
      end if;
      declare
         Group_Name : String := To_String (Params (Params'First));
      begin
         if To_Upper (Group_Name) = To_Upper (To_String (Current_Group)) then
            Display_Message ("Cannot delete current group " & Group_Name);
         elsif System_Group (Group_Name) then
            Display_Message ("Cannot delete system group " & Group_Name);
         else
            Delete_Group (Group_Name);
            Display_Message ("OK");
         end if;
      exception
         when Cannot_Delete =>
            Display_Message ("Cannot delete " & Group_Name);
         when Group_Not_Found =>
            Display_Message ("Group " & Group_Name & " not found");
      end;
   end Group_Delete;

   procedure Remove_Host (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: remove hostname");
      end if;
      Remove_Host (To_String (Params (Params'First)));
      Display_Message ("OK");
   exception
      when Cannot_Delete =>
         Display_Message
           ("Host " &
            To_String (Params (Params'First)) &
            " connected. Cannot remove");
      when Host_Not_Found =>
         Display_Message
           ("Host " & To_String (Params (Params'First)) & " not found");
   end Remove_Host;

   procedure Change_Current (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: cg groupname");
         return;
      end if;
      declare
         Group_Name : String := To_String (Params (Params'First));
      begin
         if not Is_Group (Group_Name) then
            Display_Message ("Group " & Group_Name & " not found");
         else
            Current_Group := To_Unbounded_String (Group_Name);
            Display_Message ("OK");
         end if;
      end;
   end Change_Current;

   procedure Connect_Host (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 and then Params'Length /= 2 then
         Display_Message ("Usage: connect url [group]");
         return;
      end if;
      declare
         OK : Boolean;
      begin
         Save_Curses_Mode (Curses);
         End_Windows;
         if Params'Length = 1 then
            OK := Add_Server (To_String (Params (Params'First)));
         elsif Params'Length = 2 then
            OK :=
              Add_Server
                (To_String (Params (Params'First)),
                 To_String (Params (Params'First + 1)));
         end if;
         if OK then
            Display_Message ("OK");
         else
            Display_Message ("Connection_failed");
         end if;
      exception
         when Group_Not_Found =>
            Display_Message ("Error (group not found)");
         when Duplicate_Name =>
            Display_Message ("Server exists");
         when Url_Error =>
            Display_Message ("Url Error");
      end;
      Reset_Curses_Mode (Curses);
   end Connect_Host;

   procedure Reconnect (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: reconnect hostname [password]");
         return;
      end if;
      begin
         Save_Curses_Mode (Curses);
         End_Windows;
            if not Connect_Server (To_String (Params (Params'First))) then
               Display_Message ("Connection_failed");
            else
               Display_Message ("OK");
            end if;

      exception
         when Group_Not_Found =>
            Display_Message ("Error (current group)");
         when Host_Not_Found =>
            Display_Message ("Host not found in current group");
         when Host_Unreachable =>
            Display_Message ("Host unreachable!");
      end;
      Reset_Curses_Mode (Curses);
   end Reconnect;

   procedure Disconnect (Params : Ragged_Array) is
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: disconnect hostname");
         return;
      end if;
      begin
         Save_Curses_Mode (Curses);
         End_Windows;
         if not Disconnect_Server (To_String (Params (Params'First))) then
            Display_Message ("Disconnection failed");
         else
            Display_Message ("OK");
         end if;
      exception
         when Host_Not_Found =>
            Display_Message ("Host not found in current group");
      end;
      Reset_Curses_Mode (Curses);
   end Disconnect;

   procedure Show_Host (Params : Ragged_Array) is
      Server : Hypervisor;
   begin
      if Params'Length /= 1 then
         Display_Message ("Usage: show hostname");
         return;
      end if;
      Server := Get_Host (To_String (Params (Params'First)));
      Display_Message ("URI : " & Server.Uri);
      if  Server.Is_Connected then
         Display_Message ("hostname : " & Server.Host_Name);
         Display_Message ("Type     : " & Server.Get_Type);
         Display_Message ("CPUs     :" & Unsigned'Image (Server.CPUs));
         Display_Message ("Memory   : " & Formats.Memory_Size (Server.Memory));
         Display_Message ("Model    : " & Server.Model);
         Display_Message ("Status : connected");
      else
         Display_Message ("Status : not connected");
      end if;
   exception
      when Group_Not_Found =>
         Display_Message ("Error (current group)");
      when Host_Not_Found =>
         Display_Message ("Host not found in current group");
   end Show_Host;

   procedure Quit (Params : Ragged_Array) is
      pragma Unreferenced (Params);
   begin
      Status := Ending;
      Close_Monitors;
   end Quit;

   function Split (Str : String) return Ragged_Array is
      First      : Positive := Str'First;
      Null_Array : Ragged_Array (1 .. 0);
   begin
      for I in Str'Range loop
         if Str (I) = ' ' then
            First := First + 1;
         else
            exit;
         end if;
      end loop;
      if First > Str'Last then
         return Null_Array;
      end if;
      for I in First .. Str'Last loop
         if Str (I) = ' ' then
            return To_Unbounded_String (Str (First .. I - 1)) &
              Split (Str (I .. Str'Last));
         end if;
      end loop;
      return Null_Array & To_Unbounded_String (Str (First .. Str'Last));
   end Split;

   procedure Parse_Command (Str : String) is
      Line : Ragged_Array := Split (Str);
   begin
      if Line'Length = 0 then
         Display_Message ("");
         return;
      end if;

      declare
         Command : String  := To_Upper (To_String (Line (Line'First)));
         Found   : Natural := 0;
      begin
         for I in Command_List'Range loop
            if Command'Length <= Length (Command_List (I).Command) then
               if Command =
                 To_Upper
                   (Slice
                      (Command_List (I).Command,
                       Command'First,
                       Command'Last))
               then
                  if Found /= 0 then
                     Display_Message
                       ("Command ambiguous: " & To_String (Line (Line'First)));
                     return;
                  else
                     Found := I;
                  end if;
               end if;
            end if;
         end loop;
         if Found = 0 then
            Display_Message
              ("Command unknown: " & To_String (Line (Line'First)));
            return;
         end if;
         -- echo command
         Display_Message (Str);
         Command_List (Found).Callback (Line (Line'First + 1 .. Line'Last));
      end;
   end Parse_Command;

   Str : String (1 .. 79);

begin
   Init_Screen;
   Out_Window := Create (Lines - 1, Columns, 0, 0);
   Allow_Scrolling (Out_Window, True);
   Move_Cursor (Out_Window, Lines - 2, 0);
   In_Window := New_Window (1, Columns, Lines - 1, 0);
   Set_Logger (Message_Logger'Access);
   loop
      case Status is
         when Ending =>
            exit;
         when Starting =>
            Display_Message
              ("Line monitor ready. type 'help' for command list");
            Status := Normal;
         when Normal =>
            Put (In_Window, "=> ");
            Get (In_Window, Str);
            Parse_Command (Str);
            Clear (In_Window);
      end case;
   end loop;
   Close_Monitors;
   End_Screen;
exception
   -- for any unhandled exception close the ncurses system
   -- before propagating the exception
   when others =>
      Status := Ending;
      Close_Monitors;
      End_Screen;
      raise;
end Linemonitor;
