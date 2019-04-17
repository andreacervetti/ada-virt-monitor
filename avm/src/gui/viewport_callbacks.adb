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
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Window;          use Gtk.Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Box;             use Gtk.Box;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Label;           use Gtk.Label;
with Gtk.Stock;
with Gtk.Handlers;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Tree_Model;      use Gtk.Tree_Model;
with Gtk.Tree_View_Column;
with Gtk.Grid;            use Gtk.Grid;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Main;

with Glib.Object;
use Glib;

with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.Types;        use Gtkada.Types;

with Monitors.Structures; use Monitors.Structures;
with Monitors.Logger;     use Monitors.Logger;
use Monitors;

with Servers;
with Viewport;            use Viewport;
with Domain_Window;
with Migration_Window; use Migration_Window;
with XML_Window;          use XML_Window;

with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
use Ada.Strings;
with Virtada.Host.Domain; use Virtada.Host.Domain;

package body Viewport_Callbacks is

   -- Definition of Handlers for various types
   type Row_Ref is record
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   end record;

   package Changed_Cb is new Gtk.Handlers.Callback (Gtk_Widget_Record);

   package Menu_User_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record,
      Row_Ref);

   --------------------
   -- Try_Connection --
   --------------------
   procedure Try_Connection (Server : String) is
      Dummy : Message_Dialog_Buttons;
   begin
      if not Connect_Server (Server) then
         -- null for now. Add a message error if connection fails
         null;
      end if;
   exception
      when Url_Error =>
         Dummy :=
           Message_Dialog
             ("Wrong URL!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
      when Host_Unreachable =>
         Dummy :=
           Message_Dialog
             ("Host unreachable!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
      when Host_Not_Found =>
         Dummy :=
           Message_Dialog
             ("Host not found!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
   end Try_Connection;

   ---------------
   -- Build_Uri --
   ---------------
   function Build_Uri
     (Protocol : String;
      Host     : String;
      User     : String) return String
   is
      Suffix1 : aliased String := "/system";
      Suffix2 : aliased String := "?no_verify=1";
      Suffix : access String;
   begin
      if Protocol = "esx" then
         Suffix := Suffix2'Access;
      else
         Suffix := Suffix1'Access;
      end if;
      if User = "" then
         return Protocol & "://" & Host & Suffix.all;
      else
         return Protocol & "://" & User & "@" & Host & Suffix.all;
      end if;
   end Build_Uri;

   type Connection_Data_Record is
      record
         Protocol     : Gtk_Combo_Box_Text;
         User         : Gtk_Entry;
         Composed_Uri : Gtk_Label;
         Host         : Gtk_Entry;
      end record;

   package User_Connection_Data Is
     new Glib.Object.User_Data (Connection_Data_Record);

   use User_Connection_Data;

   Connection_Data : constant String := "connection_data";

   QEMU_Protocol       : aliased constant String := "qemu";
   QEMU_SSH_Protocol   : aliased constant String := "qemu+ssh";
   VMWare_Esx_Protocol : aliased constant String := "esx";

   Protocols : constant array (Gint range 0..2) of access constant String :=
     (QEMU_SSH_Protocol'Access,
      QEMU_Protocol'Access,
      VMWare_Esx_Protocol'Access);

   ------------------
   -- Text_Changed --
   ------------------
   procedure Text_Changed (Object : access Gtk_Widget_Record'Class) is
      Grid : Gtk_Grid;
      Data : Connection_Data_Record;
   begin
      Grid := Gtk_Grid (Object.Get_Ancestor (Gtk.Grid.Get_Type));
      Data := User_Connection_Data.Get (Grid, Connection_Data);
      Data.Composed_Uri.Set_Text
      (Build_Uri
         (Protocol => Protocols (Data.Protocol.Get_Active).all,
          Host     => Data.Host.Get_Text,
          User     => Data.User.Get_Text));
   exception -- This should never happen
      when Data_Error =>
         Display_Message ("ERROR: data unavailable - cannot compose URI");
   end Text_Changed;

   --------------------
   -- New_Connection --
   --------------------
   procedure New_Connection
     (Item       : access Gtk_Menu_Item_Record'Class;
      Group_Name : String)
   is
      use Gtk.Stock;

      Ok_Button     : Gtk_Widget;
      Dialog        : Gtk_Dialog;
      Cancel_Button : Gtk_Widget;
      Response      : Gtk_Response_Type;
      Grid           :Gtk_Grid;
      Label         : Gtk_Label;
      Dummy         : Message_Dialog_Buttons;

      Data          : Connection_Data_Record;

   begin
      Gtk.Dialog.Gtk_New
        (Dialog => Dialog,
         Title  => "New connection",
         Parent => Gtk_Window (Item.Get_Toplevel),
         Flags  => 0);
      Cancel_Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Ok_Button     := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      Dialog.Set_Default (Ok_Button);

      Gtk.Grid.Gtk_New (Grid);
      Pack_Start (Get_Content_Area (Dialog), Grid, Padding => 10);

      Gtk.Label.Gtk_New (Label, "Protocol :");
      Grid.Attach_Next_To (Label, null, Pos_Bottom);

      Gtk.Combo_Box_Text.Gtk_New (Data.Protocol);
      Data.Protocol.Append_Text ("QEMU/SSH");
      Data.Protocol.Append_Text ("QEMU");
      Data.Protocol.Append_Text ("VMWare ESX");
    Data.Protocol.Set_Active (0);
      Changed_Cb.Connect
        (Data.Protocol,
         "changed",
         Changed_Cb.To_Marshaller (Text_Changed'Access));
      Grid.Attach_Next_To (Data.Protocol, Label, Pos_Right);

      Gtk.Label.Gtk_New (Label, "Hypervisor :");
      Grid.Attach_Next_To (Label, null, Pos_Bottom);

      Gtk.GEntry.Gtk_New (Data.Host);
      Data.Host.Set_Activates_Default (True);
      Changed_Cb.Connect
        (Data.Host,
         "changed",
         Changed_Cb.To_Marshaller (Text_Changed'Access));
      Grid.Attach_Next_To (Data.Host, Label, Pos_Right);

      Gtk.Label.Gtk_New (Label, "User:");
      Grid.Attach_Next_To (Label, null, Pos_Bottom);

      Gtk.GEntry.Gtk_New (Data.User);
      Data.User.Set_Text ("root");
      Data.User.Set_Activates_Default (True);
      Changed_Cb.Connect
        (Data.User,
         "changed",
         Changed_Cb.To_Marshaller (Text_Changed'Access));
      Grid.Attach_Next_To (Data.User, Label, Pos_Right);

      Gtk.Label.Gtk_New (Label, "URI :");
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Gtk.Label.Gtk_New
        (Data.Composed_Uri,
         Build_Uri
           (Protocol => Protocols (Data.Protocol.Get_Active).all,
            Host     => "",
            User     => Data.User.Get_Text));
      Grid.Attach_Next_To (Data.Composed_Uri, Label, Pos_Right);

      User_Connection_Data.Set (Grid, Data, Connection_Data);

      Show_All (Dialog);
      <<Retry>>
      Response := Dialog.Run;
      if Response = Gtk_Response_OK then
         begin
            if not Add_Server (Data.Composed_Uri.Get_Text, Group_Name) then
               Dummy :=
                 Message_Dialog ("connect failed!", Buttons => Button_OK);
               goto Retry;
            end if;
         exception
            when Duplicate_Name =>
               Dummy :=
                 Message_Dialog
                   ("Connection exists!",
                    Dialog_Type => Error,
                    Buttons     => Button_OK);
               goto Retry;
            when Url_Error =>
               Dummy :=
                 Message_Dialog
                   ("Wrong URL!",
                    Dialog_Type => Error,
                    Buttons     => Button_OK);
               goto Retry;
            when Host_Unreachable =>
               Dummy :=
                 Message_Dialog
                   ("Host unreachable!",
                    Dialog_Type => Error,
                    Buttons     => Button_OK);
               goto Retry;
            when Host_Not_Found =>
               Dummy :=
                 Message_Dialog
                   ("Host not found!",
                    Dialog_Type => Error,
                    Buttons     => Button_OK);
               goto Retry;
            when Group_Not_Found => -- this shouldn't happen
               Dummy :=
                 Message_Dialog
                   ("Group not Found!",
                    Dialog_Type => Error,
                    Buttons     => Button_OK);
               goto Retry;
         end;
      end if;
      Destroy (Dialog);
   end New_Connection;

   ---------------
   -- New_Group --
   ---------------
   procedure New_Group (Parent : access Gtk_Widget_Record'Class) is

      use Gtk.Stock;

      Ok_Button     : Gtk_Widget;
      Dialog        : Gtk_Dialog;
      Cancel_Button : Gtk_Widget;
      Response      : Gtk_Response_Type;
      Box           : Gtk_Box;
      Label         : Gtk_Label;
      Dummy         : Message_Dialog_Buttons;
      Group         : Gtk_Entry;
   begin
      Gtk.Dialog.Gtk_New
        (Dialog => Dialog,
         Title  => "New group",
         Parent => Gtk_Window (Parent.Get_Toplevel),
         Flags  => 0);
      Dialog.Set_Title ("New group");
      Cancel_Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Ok_Button     := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      Dialog.Set_Default (Ok_Button);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Padding => 10);

      Gtk_New (Label, "Group");
      Pack_Start (Box, Label, Padding => 10);
      Gtk_New (Group);
      Group.Set_Activates_Default (True);
      Pack_Start (Box, Group, Padding => 10);

      Show_All (Dialog);

      <<Retry>>
      Response := Dialog.Run;
      if Response = Gtk_Response_OK then
         declare
            Group_Name : String := Trim (Group.Get_Text, Side => Both);
         begin
            -- Create the new group
            Viewport.Create_Group (Group_Name);

         exception
            when Group_Name_Error =>
               Dummy :=
                 Message_Dialog
                   ("Group name error!",
                    Help_Msg =>
                      "A group name should begin with an " &
                      "alphabetic character and be composed" &
                      ASCII.LF &
                      "only by letters (a-z, A-Z), " &
                      "numbers (0-9) and by the characters " &
                      """_-."".",
                    Parent   => Gtk_Window (Parent.Get_Toplevel));
               goto Retry;
            when Duplicate_Name =>
               Dummy := Message_Dialog
                      ("Group exists!",
                       Buttons => Button_OK,
                       Parent  => Gtk_Window (Parent.Get_Toplevel));
               goto Retry;
         end;
      end if;
      Destroy (Dialog);
   end New_Group;

   ----------------------
   -- Reconnect_Server --
   ----------------------
   procedure Reconnect_Server
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      Server : String := Get_String (Row.Model, Row.Iter, Name_Col);
      Dummy  : Message_Dialog_Buttons;
   begin
      Try_Connection (Server);
   end Reconnect_Server;

   -----------------------
   -- Disconnect_Server --
   -----------------------
   procedure Disconnect_Server
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);

      Server : String := Get_String (Row.Model, Row.Iter, Name_Col);
--        Group  : String := Row.Model.Get_String (Row.Iter, Hidden_Group_Col);
   begin
      if not Monitors.Structures.Disconnect_Server (Server) then
         -- null for now. Add a message error if disconnection fails
         null;
      end if;
   end Disconnect_Server;

   -------------------
   -- Remove_Server --
   -------------------
   procedure Remove_Server
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      Server : String := Get_String (Row.Model, Row.Iter, Name_Col);
      Button : Message_Dialog_Buttons;
      Host   : Hypervisor;
   begin
      Host := Get_Host (Server);
      -- ask for confirmation
      Button :=
        Message_Dialog
          ("You are about to remove server " &  ASCII.LF &
           Host.Uri & ASCII.LF &
           ASCII.LF &
           "Confirm?",
           Dialog_Type    => Confirmation,
           Buttons        => Button_No or Button_Yes,
           Default_Button => Button_No);
      if Button = Button_Yes then
         Remove_Host (Server);
      end if;

   exception -- these should never happen
      when Group_Not_Found =>
         Button :=
           Message_Dialog
             ("Error: Group not found!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
      when Host_Not_Found =>
         Button :=
           Message_Dialog
             ("Error: Host not found!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
      when Cannot_Delete =>
         Button :=
           Message_Dialog
             ("Error: cannot delete!" & ASCII.LF & "Host is connected",
              Dialog_Type => Error,
              Buttons     => Button_OK);
   end Remove_Server;

   --------------
   -- Get_Host --
   --------------
   procedure Get_Host
     (Row    :        Row_Ref;
      Server : in out Hypervisor;
      Result : in out Boolean)
   is
      Host   : String := Get_String (Row.Model, Row.Iter, Name_Col);
      Button : Message_Dialog_Buttons;
   begin
      begin
         Result := False;
         Server := Get_Host (Host);
         Result := True;
      exception -- these should never happen
         when Host_Not_Found =>
            Button :=
              Message_Dialog
                ("Error: Host not found!",
                 Dialog_Type => Error,
                 Buttons     => Button_OK);
      end;
   end Get_Host;

   ------------
   -- Get_VM --
   ------------
   procedure Get_VM
     (Row    :        Row_Ref;
      VM     : in out VM_Type;
      Result :    out Boolean)
   is
      Domain : String := Get_String (Row.Model, Row.Iter, Name_Col);
      Host   : String :=
        Get_String (Row.Model, Parent (Row.Model, Row.Iter), Name_Col);
      Button : Message_Dialog_Buttons;
   begin
      Result := False;
      VM     := Get_Domain (Host, Domain);
      Result := True;
   exception -- these should never happen
      when Host_Not_Found =>
         Button :=
           Message_Dialog
             ("Error: Host not found!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
      when Domain_Not_Found =>
         Button :=
           Message_Dialog
             ("Error: Domain not found!",
              Dialog_Type => Error,
              Buttons     => Button_OK);
   end Get_VM;

   ----------------
   -- Run_Domain --
   ----------------
   procedure Run_Domain
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      VM    : VM_Type;
      Found : Boolean := False;
   begin
      Get_VM (Row, VM, Found);
      if Found then
         if not Start (VM) then
            null;
         end if;
      end if;
   end Run_Domain;

   ---------------------
   -- Shutdown_Domain --
   ---------------------
   procedure Shutdown_Domain
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      VM    : VM_Type;
      Found : Boolean := False;
   begin
      Get_VM (Row, VM, Found);
      if Found then
         if not Shutdown (VM) then
            null;
         end if;
      end if;
   end Shutdown_Domain;

   ---------------------
   -- Pause_Domain --
   ---------------------
   procedure Pause_Domain
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      VM    : VM_Type;
      Found : Boolean := False;
   begin
      Get_VM (Row, VM, Found);
      if Found then
         if not Suspend (VM) then
            null;
         end if;
      end if;
   end Pause_Domain;

   ---------------------
   -- Resume_Domain --
   ---------------------
   procedure Resume_Domain
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      VM    : VM_Type;
      Found : Boolean := False;
   begin
      Get_VM (Row, VM, Found);
      if Found then
         if not Resume (VM) then
            null;
         end if;
      end if;
   end Resume_Domain;

   --------------------
   -- Migrate_Domain --
   --------------------
   procedure Migrate_Domain
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);

      Domain : String := Get_String (Row.Model, Row.Iter, Name_Col);
      Host   : String :=
        Get_String (Row.Model, Parent (Row.Model, Row.Iter), Name_Col);

   begin
         Create_Migration_Window (Host, Domain);
   end Migrate_Domain;

   ------------------
   -- Open_Sysinfo --
   ------------------
   procedure Open_Sysinfo
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      Server : Hypervisor;
      Found  : Boolean := False;
   begin
      Get_Host (Row, Server, Found);
      if Found then
         Create_Sysinfo_XML_Window (Server);
      end if;
   end Open_Sysinfo;

   --------------
   -- Open_XML --
   --------------
   procedure Open_XML
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      VM    : VM_Type;
      Found : Boolean := False;
   begin
      Get_VM (Row, VM, Found);
      if Found then
         Create_Domain_XML_Window (VM);
      end if;
   end Open_XML;

   -----------------------
   -- Open_Capabilities --
   -----------------------
   procedure Open_Capabilities
     (Item : access Gtk_Menu_Item_Record'Class;
      Row  : Row_Ref)
   is
      pragma Unreferenced (Item);
      Server : Hypervisor;
      Found  : Boolean := False;
   begin
      Get_Host (Row, Server, Found);
      if Found then
         Create_Capabilities_XML_Window (Server);
      end if;
   end Open_Capabilities;

   -------------------
   -- On_Row_Activated
   -------------------
   procedure On_Row_Activated
     (Tree : access Gtk_Tree_View_Record'Class;
      Path : Gtk_Tree_Path)
   is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      Model := Tree.Get_Model;
      Iter  := Get_Iter (Model, Path);
      if Get_Depth (Path) = 2 then
         -- it's a Domain column
         declare
            Domain : String := Get_String (Model, Iter, Name_Col);
            Server : String :=
              Get_String (Model, Parent (Model, Iter), Name_Col);
         begin
            Domain_Window.Create_Domain_Window (Server, Domain);
         end;
      elsif Get_Depth (Path) = 1 then
         -- it's a hypervisor column
         declare
            Server : String := Get_String (Model, Iter, Name_Col);
         begin
            if Get_Host (Server).Is_Connected then
               -- show server window
               Servers.Create_Server_Window (Server);
            else
               Try_Connection (Server);
            end if;
         end;
      end if;
   end On_Row_Activated;

   -----------------------
   -- Domain_Popup_Menu --
   -----------------------
   procedure Domain_Popup_Menu
     (Row    : Row_Ref;
      Menu   : Gtk.Menu.Gtk_Menu)
   is
      Run_Item      : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Shutdown_Item : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Pause_Item    : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Migrate_Item  : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      XML_Item      : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      VM            : VM_Type;
      Found         : Boolean;
   begin

      Get_VM (Row, VM, Found);
      if not Found then
         return;
      end if;

      -- Run
      Gtk_New_From_Stock (Run_Item, "gtk-media-play", null);
      Run_Item.Set_Label ("Run");
      -- Shutdown
      Gtk_New_From_Stock (Shutdown_Item, "gtk-stop", null);
      Shutdown_Item.Set_Label ("Shutdown");
      if VM.Is_Active then
         Run_Item.Set_Sensitive (False);
         Menu_User_Cb.Connect
           (Shutdown_Item,
            "activate",
            Menu_User_Cb.To_Marshaller (Shutdown_Domain'Access),
            Row);
         Shutdown_Item.Set_Sensitive (True);
      else
         Menu_User_Cb.Connect
           (Run_Item,
            "activate",
            Menu_User_Cb.To_Marshaller (Run_Domain'Access),
            Row);
         Run_Item.Set_Sensitive (True);
         Shutdown_Item.Set_Sensitive (False);
      end if;
      -- Pause
      Gtk_New_From_Stock (Pause_Item, "gtk-media-pause", null);
      if Vm.State = Paused then
         Pause_Item.Set_Label ("Resume");
         Menu_User_Cb.Connect
           (Pause_Item,
            "activate",
            Menu_User_Cb.To_Marshaller (Resume_Domain'Access),
            Row);
         Pause_Item.Set_Sensitive (True);
      else
         Pause_Item.Set_Label ("Pause");
         if VM.State = Running then
            Menu_User_Cb.Connect
              (Pause_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Pause_Domain'Access),
               Row);
            Pause_Item.Set_Sensitive (True);
         else
            Pause_Item.Set_Sensitive (False);
         end if;
      end if;
      -- migrate
      Gtk.Image_Menu_Item.Gtk_New (Migrate_Item, "Migrate");
      if VM.State = Running or else VM.State = Paused then
            Menu_User_Cb.Connect
              (Migrate_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Migrate_Domain'Access),
               Row);
            Migrate_Item.Set_Sensitive (True);
         else
            Migrate_Item.Set_Sensitive (False);
         end if;
      -- open XML
      Gtk_New_From_Stock (XML_Item, "gtk-open", null);
      XML_Item.Set_Label ("Open XML");
      Menu_User_Cb.Connect
        (XML_Item,
         "activate",
         Menu_User_Cb.To_Marshaller (Open_XML'Access),
         Row);
      XML_Item.Set_Sensitive (True);

      Menu.Append (Run_Item);
      Menu.Append (Shutdown_Item);
      Menu.Append (Pause_Item);
      Menu.Append (Migrate_Item);
      Menu.Append (XML_Item);
   end Domain_Popup_Menu;

   ---------------------
   -- Host_Popup_Menu --
   ---------------------
   procedure Host_Popup_Menu
     (Row    : Row_Ref;
      Menu   : Gtk.Menu.Gtk_Menu;
      Active : Boolean)
   is
   begin
      declare
         Conn_Item         : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
         Remove_Item       : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
         XML_Item          : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
         Capabilities_Item : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;

      begin
         Gtk_New_From_Stock (Remove_Item, "gtk-remove", null);
         Gtk_New_From_Stock (XML_Item, "gtk-open", null);
         Gtk_New_From_Stock (Capabilities_Item, "gtk-open", null);

         XML_Item.Set_Label ("Open SysInfo");
         Capabilities_Item.Set_Label ("Open Capabilities");
         if not Active then
            Gtk_New_From_Stock (Conn_Item, "gtk-connect", null);
            Menu_User_Cb.Connect
              (Conn_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Reconnect_Server'Access),
               Row);
            Menu_User_Cb.Connect
              (Remove_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Remove_Server'Access),
               Row);
            Remove_Item.Set_Sensitive (True);
            XML_Item.Set_Sensitive (False);
            Capabilities_Item.Set_Sensitive (False);
         else
            Gtk_New_From_Stock (Conn_Item, "gtk-disconnect", null);
            Conn_Item.Set_Label ("Disconnect");
            Menu_User_Cb.Connect
              (Conn_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Disconnect_Server'Access),
               Row);
            Remove_Item.Set_Sensitive (False);
            Menu_User_Cb.Connect
              (XML_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Open_Sysinfo'Access),
               Row);
            XML_Item.Set_Sensitive (True);
            Menu_User_Cb.Connect
              (Capabilities_Item,
               "activate",
               Menu_User_Cb.To_Marshaller (Open_Capabilities'Access),
               Row);
            Capabilities_Item.Set_Sensitive (True);
         end if;
         Menu.Append (Conn_Item);
         Menu.Append (Remove_Item);
         Menu.Append (XML_Item);
         Menu.Append (Capabilities_Item);
      end;
   end Host_Popup_Menu;

   ---------------------
   -- Tree_View_Popup --
   ---------------------
   function Tree_View_Popup
     (Object : access Gtk_Tree_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      use Gtk.Image_Menu_Item;
      Menu      : Gtk.Menu.Gtk_Menu;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell_X    : Gint;
      Cell_Y    : Gint;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Row_Found : Boolean;
      Row       : Row_Ref;
      X_Win     : Gdouble;
      Y_Win     : Gdouble;
      Active    : Boolean;

   begin
      if Get_Event_Type (Event) /= Button_Press
        or else Get_Button (Event) /= 3
      then
         return False;
      end if;
      Get_Coords (Event, X_Win, Y_Win);
      Object.Get_Path_At_Pos
      (Gint (X_Win), Gint (Y_Win), Path, Column, Cell_X, Cell_Y, Row_Found);
      if not Row_Found then
         return False;
      end if;

      if Get_Depth (Path) = 1 or else Get_Depth (Path) = 2 then

         Row.Model := Object.Get_Model;
         Row.Iter  := Get_Iter (Row.Model, Path);

         Active := Get_Boolean (Row.Model, Row.Iter, Hidden_Status_Col);

         Gtk_New (Menu);

         if Get_Depth (Path) = 1 then
            -- Mouse button clicked on a hypervisor row
            Host_Popup_Menu (Row, Menu, Active);
         elsif Get_Depth (Path) = 2 then
            -- Mouse button clicked on a domain row
            Domain_Popup_Menu (Row, Menu);
         end if;

         Menu.Show_All;
         Menu.Popup (Activate_Time => Gtk.Main.Get_Current_Event_Time);
         return True;
      end if;

      return False;
   end Tree_View_Popup;

end Viewport_Callbacks;
