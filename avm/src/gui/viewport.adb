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
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Box;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Image_Menu_Item;      use Gtk.Image_Menu_Item;
with Gtk.Menu;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Widget;               use Gtk.Widget.Widget_List;
with Gtk.Window;

with Glib.Properties; use Glib.Properties;
with Glib.Main;       use Glib;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Monitors.Structures; use Monitors.Structures;
with Monitors.Logger;     use Monitors.Logger;
use Monitors;

with Viewport_Callbacks; use Viewport_Callbacks;
with Formats; use Formats;
with Virtada; use Virtada;

package body Viewport is

   Update_Time : constant := 2000;

   type Percentage is delta 0.01 range 0.0 .. 100.0;
   ---------------------
   -- Callbacks
   ---------------------
   package Tree_Return_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Tree_View_Record,
      Boolean);
   package Tree_Cb is new Gtk.Handlers.Callback (Gtk_Tree_View_Record);

   package Menu_User_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record,
      String);

   -- Static local variables
   type Group_View is record
      Delete_Item : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Store       : Gtk.Tree_Store.Gtk_Tree_Store;
      Tree        : Gtk.Tree_View.Gtk_Tree_View;
   end record;

   package View_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String,
      Group_View);

   Local_Viewport : Gtk.Notebook.Gtk_Notebook;
   Stores         : View_Map.Map;
   Timeout_Func   : Glib.Main.G_Source_Id;

   procedure Close_Server_Related_Windows (Server_Name : String)
      -- Close all windows related to a disconnected server
   is
      List              : Gtk.Widget.Widget_List.Glist;
      Server_String     : String := "server:" & Server_Name;
      Server_String_Ext : String := Server_String & "/";
      Domain_String     : String := "domain:" & Server_Name & "/";
   begin
      List := Gtk.Window.List_Toplevels;
      while List /= Null_List loop
         if Get_Data (List).Get_Name = Server_String
           or else
             Head (Get_Data (List).Get_Name, Server_String_Ext'Length) =
             Server_String_Ext
            -- found a server window
           or else
             Head (Get_Data (List).Get_Name, Domain_String'Length) =
             Domain_String
            -- found a domain window
         then
            Gtk.Window.Gtk_Window (Get_Data (List)).Close;
         end if;
         List := Next (List);
      end loop;
   end Close_Server_Related_Windows;

   ------------------
   -- Update_Viewport
   ------------------
   function Update_Viewport return Boolean is
      -- timed function
      procedure Update_Store (Cursor : View_Map.Cursor) is
         Group_Name   : String           := View_Map.Key (Cursor);
         Store        : Gtk_Tree_Store   := Stores (Cursor).Store;
         Tree         : Gtk_Tree_View    := Stores (Cursor).Tree;
         Iter         : Gtk_Tree_Iter    := Store.Get_Iter_First;
         Iter1, Child : Gtk_Tree_Iter;
         Group        : Group_Type       := Get_Group (Group_Name);
         List         : Hypervisor_Array := List_Servers (Group);
         Index        : Positive         := List'First;

         procedure Update_Domains
           (Parent : Gtk_Tree_Iter;
            Server : Hypervisor)
         is
            List    : VM_Array      := List_VMs (Server);
            Iter    : Gtk_Tree_Iter := Store.Nth_Child (Parent, 0);
            Percent : Percentage;
         begin
            for Index in List'Range loop
               begin
                  if Iter = Null_Iter then
                     Store.Append (Iter, Parent);
                  end if;
                  -- Name
                  Store.Set (Iter, Name_Col, Name (List (Index)));
                  Store.Set (Iter, Group_Col, Group_Name);
                  if Is_Active (List (Index)) then
                     -- Status
                     Store.Set (Iter, Status_Col, State_Image (List (Index)));
                     -- Icon
                     Store.Set (Iter, Icon_Col, "system-run");
                     -- ID
                     Store.Set (Iter, ID_Col, Integer'Image((List (Index).ID)));
                     -- Running time
                     Store.Set
                       (Iter, CPU_Time_Col, Format_Time(List (Index).CPU_Time));
                     -- CPU %
                     Percent :=
                       Percentage
                         (Float (CPU_Usage (List (Index))) / 100.0);
                     Store.Set (Iter, CPU_Col, Percentage'Image (Percent));
                     -- Disk read bytes
                     Store.Set
                       (Iter,
                        Disk_Read_Col,
                        Memory_Size_Simple (List (Index).Disk_Read_PS));
                     -- Disk written bytes
                     Store.Set
                       (Iter,
                        Disk_Write_Col,
                        Memory_Size_Simple (List (Index).Disk_Write_PS));
                     -- Network received bytes
                     Store.Set
                       (Iter,
                        Net_Rx_Col,
                        Memory_Size_Simple (List (Index).Net_Rx_PS));
                     -- Network transmitted bytes
                     Store.Set
                       (Iter,
                        Net_Tx_Col,
                        Memory_Size_Simple (List (Index).Net_Tx_PS));
                     Store.Set (Iter, Hidden_Status_Col, True);
                  else
                     Store.Set (Iter, Status_Col, "not running");
                     Store.Set (Iter, Icon_Col, "computer");
                     Store.Set (Iter, ID_Col, "");
                     Store.Set (Iter, CPU_Time_Col, "");
                     Store.Set (Iter, CPU_Col, "");
                     Store.Set (Iter, Disk_Read_Col, "");
                     Store.Set (Iter, Disk_Write_Col, "");
                     Store.Set (Iter, Net_Rx_Col, "");
                     Store.Set (Iter, Net_Tx_Col, "");
                     Store.Set (Iter, Hidden_Status_Col, False);
                  end if;
                  Store.Next (Iter);
               exception
                  when Domain_Error =>
                     -- Domain disappeared while we were updating the row
                     -- remove from the list
                     Store.Remove (Iter);
               end;
            end loop;

            while Iter /= Null_Iter loop
               Store.Remove (Iter);
            end loop;
         exception
            when Error : others =>
               Display_Message
                 ("Exception (Update_Domains): " &
                  Exception_Name (Error) &
                  " " &
                  Exception_Message (Error));
               raise;
         end Update_Domains;

      begin -- Update_Store
         while (Iter /= Null_Iter or Index in List'Range) loop
            if Iter = Null_Iter
              or else
              (Index in List'Range
               and then
                 Store.Get_String (Iter, Name_Col) >
                 Server_Name (List (Index)))
            then
               -- new server: add
               Store.Insert_Before (Iter1, Null_Iter, Iter);
               Store.Set (Iter1, Name_Col, Server_Name (List (Index)));
               Store.Set (Iter1, Group_Col, Group_Name);
               if not List (Index).Is_Connected then
                  Store.Set (Iter1, Icon_Col, "network-server");
                  Store.Set (Iter1, Status_Col, "not connected");
                  Store.Set (Iter1, Hidden_Status_Col, False);
               else
                  Store.Set (Iter1, Icon_Col, "network-transmit-receive");
                  Store.Set (Iter1, Status_Col, "connected");
                  Store.Set (Iter1, Hidden_Status_Col, True);
                  -- server is connected: add domains
                  Display_Message ("Adding");
                  Update_Domains (Iter1, List (Index));
                  if Tree.Expand_Row (Store.Get_Path (Iter1), True) then
                     null;
                  end if;

               end if;
               Index := Index + 1;
            elsif Index not in List'Range
              or else
                Store.Get_String (Iter, Name_Col) <
                Server_Name (List (Index))
            then
               -- server doesn't exist: delete
               -- the documentation is not clear. I don't understand: if the
               -- iter is the last is it set to the null_iter? So I save it
               -- before deleting the row
               -- TODO : check if children are deleted
               Iter1 := Iter;
               Store.Next (Iter1);
               Store.Remove (Iter);
               Iter := Iter1;
            else
               -- server exists: if changed upgrade
               if List (Index).Is_Connected then
                  Update_Domains (Iter, List (Index));
                  if not Store.Get_Boolean (Iter, Hidden_Status_Col) then
                     Store.Set (Iter, Icon_Col, "network-transmit-receive");
                     Store.Set (Iter, Status_Col, "connected");
                     Store.Set (Iter, Hidden_Status_Col, True);
                     if Tree.Expand_Row (Store.Get_Path (Iter), True) then
                        null;
                     end if;
                  end if;
               else
                  if Store.Get_Boolean (Iter, Hidden_Status_Col) then
                     -- server is no more connected
                     Store.Set (Iter, Icon_Col, "network-server");
                     Store.Set (Iter, Status_Col, "not connected");
                     Store.Set (Iter, Hidden_Status_Col, False);
                     -- delete all children
                     Child := Store.Nth_Child (Iter, 0);
                     while Child /= Null_Iter loop
                        -- see above
                        Iter1 := Child;
                        Store.Next (Iter1);
                        Store.Remove (Child);
                        Child := Iter1;
                     end loop;
                     Close_Server_Related_Windows
                       (Store.Get_String (Iter, Name_Col));
                  end if;
               end if;
               Store.Next (Iter);
               Index := Index + 1;
            end if;
         end loop;
         -- if this is not a system group and is empty. Activate
         -- the "delete group" option
         -- TODO it makes no sense check this at each iteration
         -- find a better place where do this check
         if Store.Get_Iter_First /= Null_Iter
           or else System_Group (Group_Name)
         then
            View_Map.Element (Cursor).Delete_Item.Set_Sensitive (False);
         else
            View_Map.Element (Cursor).Delete_Item.Set_Sensitive (True);
         end if;
      exception
         when Error : others =>
            Display_Message
              ("Exception (Update_store): " &
               Exception_Name (Error) &
               " " &
               Exception_Message (Error));
            raise;
      end Update_Store;

   begin -- Update_Viewport
      Stores.Iterate (Update_Store'Access);
      return True;
   exception
      -- running as a GTK-controlled thread, exception are not propagated
      -- so we need to intercept any uncaught exception here
      when Error : others =>
         Display_Message
           ("Exception: " &
            Exception_Name (Error) &
            " " &
            Exception_Message (Error));
         return True;
   end Update_Viewport;

   ------------------
   -- On_Group_Delete
   ------------------
   procedure On_Group_Delete
     (Item       : access Gtk_Menu_Item_Record'Class;
      Group_Name : String)
   is
      pragma Unreferenced (Item);
      Page_Num : Gint;
   begin
      -- stop the timed function
      Glib.Main.Remove (Timeout_Func);
      -- wait for complection
      delay 0.2;
      -- Delete group
      Delete_Group (Group_Name);
      -- delete model
      Stores (Group_Name).Element.Store.Unref;
      Stores.Delete (Group_Name);
      -- delete page
      Page_Num := Local_Viewport.Get_Current_Page;
      Local_Viewport.Get_Nth_Page (Page_Num).Destroy;
      -- re start the timed function
      Local_Viewport.Show_All;
      Timeout_Func := Glib.Main.Timeout_Add (Update_Time, Update_Viewport'Access);
   exception
      when others =>
         -- something went wrong creating the group
         -- restart the timed function and propagate the exception
         Timeout_Func := Glib.Main.Timeout_Add (Update_Time, Update_Viewport'Access);
         raise;
   end On_Group_Delete;

   ----------------
   -- Set_Column --
   ----------------


   ------------
   -- Set_Page
   ------------
   procedure Set_Page (Group_Name : String) is
      Label       : Gtk_Label;
      Box         : Gtk.Box.Gtk_Vbox;
      Menu_Bar    : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu        : Gtk.Menu.Gtk_Menu;
      Delete_Item : Gtk_Image_Menu_Item;
      Image_Item  : Gtk_Image_Menu_Item;
      Menu_Item   : Gtk.Menu_Item.Gtk_Menu_Item;
      Window      : Gtk_Scrolled_Window;
      Model       : Gtk_Tree_Store;
      Tree        : Gtk_Tree_View;
      Column      : Gtk_Tree_View_Column;
      Text_Render : Gtk_Cell_Renderer_Text;
      Icon_Render : Gtk_Cell_Renderer_Pixbuf;
      Num         : Glib.Gint;

      procedure Set_Column
        (Title  : String; Column_Number : Glib.Gint; Xalign : Glib.Gfloat)
      is
      begin
         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (Title);
         Num := Tree.Append_Column (Column);
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
         Text_Render.Set_Alignment (Xalign, 0.5);
         Column.Pack_Start (Text_Render, True);
         Column.Add_Attribute (Text_Render, "text", Column_Number);
      end Set_Column;

   begin
      --
      -- create a new notebook page for every group
      --
      Gtk.Label.Gtk_New (Label);
      Label.Set_Text (Group_Name);
      Label.Show;
      --
      -- add a menu bar for operation on group
      --
      Gtk.Box.Gtk_New_Vbox (Box);
      Gtk.Menu_Bar.Gtk_New (Menu_Bar);
      Box.Pack_Start (Menu_Bar, False);
      Gtk.Menu_Item.Gtk_New (Menu_Item, "Group");
      Menu_Bar.Append (Menu_Item);
      Gtk.Menu.Gtk_New (Menu);
      Menu_Item.Set_Submenu (Menu);
      Gtk_New_From_Stock (Delete_Item, "gtk-delete", null);
      Menu.Append (Delete_Item);
      Menu_User_Cb.Connect
        (Delete_Item,
         "activate",
         Menu_User_Cb.To_Marshaller (On_Group_Delete'Access),
         Group_Name);
      Delete_Item.Set_Sensitive (False);
      Gtk_New_From_Stock (Image_Item, "gtk-edit", null);
      Menu.Append (Image_Item);
      Image_Item.Set_Sensitive (False);
      Gtk_New_From_Stock (Image_Item, "gtk-new", null);
      Image_Item.Set_Label ("New Connection");
      Menu.Append (Image_Item);
      Menu_User_Cb.Connect
        (Image_Item,
         "activate",
         Menu_User_Cb.To_Marshaller (New_Connection'Access),
         Group_Name);
      --
      -- create a scrolled window with the tree view
      --
      Gtk.Scrolled_Window.Gtk_New (Window);
      Window.Set_Policy (Policy_Automatic, Policy_Automatic);
      Box.Pack_Start (Window);

      Gtk.Tree_Store.Gtk_New
        (Model,
         (Name_Col          => GType_String,
          Icon_Col          => GType_String,
          Status_Col        => GType_String,
          ID_Col            => GType_String,
          CPU_Time_Col      => GType_String,
          CPU_Col           => GType_String,
          Disk_Read_Col     => GType_String,
          Disk_Write_Col    => GType_String,
          Net_Rx_Col        => GType_String,
          Net_Tx_Col        => GType_String,
          Group_Col         => GType_String,
          Hidden_Status_Col => GType_Boolean));

      Gtk.Tree_View.Gtk_New (Tree, Model);

      -- save the tree_store
      Stores.Insert (Group_Name, (Delete_Item, Model, Tree));

      -- Set Callbacks
      -- Double click on row
      Tree_Cb.Connect
        (Tree,
         "row-activated",
         Tree_Cb.To_Marshaller (On_Row_Activated'Access));

      -- right mouse button
      Tree_Return_Cb.Connect
        (Tree,
         "button-press-event",
         Tree_Return_Cb.To_Marshaller (Tree_View_Popup'Access));

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Host/VM");
      Num := Tree.Append_Column (Column);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Icon_Render);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Column.Pack_Start (Icon_Render, False);
      Column.Pack_Start (Text_Render, True);
      Column.Add_Attribute (Icon_Render, "icon-name", Icon_Col);
      Set_Property (Icon_Render, Stock_Size_Property, 3);
      Column.Add_Attribute (Text_Render, "text", Name_Col);

      Set_Column ("Status", Status_Col, 0.0);

      Set_Column ("ID", ID_Col, 1.0);

      Set_Column ("CPU Time", CPU_Time_Col, 1.0);

      Set_Column ("% CPU", CPU_Col, 1.0);

      Set_Column ("Disk Read", Disk_Read_Col, 1.0);

      Set_Column ("Disk Write", Disk_Write_Col, 1.0);

      Set_Column ("Net RX", Net_Rx_Col, 1.0);

      Set_Column ("Net TX", Net_Tx_Col, 1.0);

      -- Filler column
      Gtk.Tree_View_Column.Gtk_New (Column);
      Num := Tree.Append_Column (Column);

      Window.Add (Tree);

      Local_Viewport.Append_Page (Box, Label);
      Local_Viewport.Set_Tab_Reorderable (Box, True);
      Box.Show_All;
   end Set_Page;

   ----------------
   -- Create_Group
   ----------------
   procedure Create_Group (Group_Name : String) is
   begin
      -- stop the timed function
      Glib.Main.Remove (Timeout_Func);
      -- wait for complection
      delay 0.2;

      Monitors.Structures.Create_Group (Group_Name);
      Set_Page (Group_Name);

      -- re start the timed function
      Local_Viewport.Show_All;
      Timeout_Func := Glib.Main.Timeout_Add (Update_Time, Update_Viewport'Access);
   exception
      when others =>
         -- something went wrong creating the group
         -- restart the timed function and propagate the exception
         Timeout_Func := Glib.Main.Timeout_Add (Update_Time, Update_Viewport'Access);
         raise;
   end Create_Group;

   --------------
   -- Initialize
   --------------
   procedure Initialize (Main_Viewport : in out Gtk.Notebook.Gtk_Notebook) is

   begin
      Gtk.Notebook.Gtk_New (Main_Viewport);
      Main_Viewport.Popup_Enable;
      -- save viewport for future reference
      Local_Viewport := Main_Viewport;
      Groups_Iterate (Set_Page'Access);
      -- start the timed function that keep the store up-to-date
      Timeout_Func := Glib.Main.Timeout_Add (Update_Time, Update_Viewport'Access);
   end Initialize;

end Viewport;
