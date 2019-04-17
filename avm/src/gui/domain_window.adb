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
with Gtk.Window;               use Gtk.Window;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Grid;                 use Gtk.Grid;
with Gtkada.Handlers;          use Gtkada;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Stock;                use Gtk.Stock;

with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;

with Gtk.Handlers;

with Glib.Properties;          use Glib.Properties;
with Glib.Object;              use Glib.Object;
use Glib;

with Simple_Callbacks;         use Simple_Callbacks;
with Frame_Helpers;            use Frame_Helpers;
with Switch_To_Window;
with Main_Icon;

with Monitors.Structures;      use Monitors.Structures;
with Monitors.XMLTrees;        use Monitors.XMLTrees;
use Monitors.XMLTrees.XMLTree;
use Monitors;

package body Domain_Window is

   Name_Col   : constant := 0;
   Icon_Col   : constant := 1;
   Object_Col : constant := 2;

   package Selection_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Tree_Selection_Record,
      Gtk_Frame);

   ---------------------------
   -- On_Selection callback --
   ---------------------------
   procedure On_Selection
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Frame     : Gtk_Frame)
   is

      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      Selection.Get_Selected (Model, Iter);
      declare
         Box : Gtk_Box;
      begin
         Box := Gtk_Box (Get_Object (Model, Iter, Object_Col));
         if Frame.Get_Child /= null then
            Frame.Remove (Frame.Get_Child);
         end if;
         Frame.Add (Box);
         Frame.Show_All;
      end;
   end On_Selection;

   --------------------------
   -- Create_Domain_Window --
   --------------------------
   procedure Create_Domain_Window
     (Server_Name : String;
      Domain_Name : String)
   is

      Store       : Gtk_Tree_Store;
      Domain_Tree : XMLTree.Tree;

      ------------------------
      -- Store_Set (nested) --
      ------------------------
      procedure Store_Set
        (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Name   : UTF8_String;
         Icon   : UTF8_String     := "";
         Object : Gtk.Box.Gtk_Box := null)
      is
         Box : Gtk_Box := Gtk_Vbox_New;
      begin
         Store.Set (Iter, Name_Col, Name);
         if Icon /= "" then
            Store.Set (Iter, Icon_Col, Icon);
         end if;
         if Object = null then
            Store.Set (Iter, Object_Col, GObject (Box));
         else
            Store.Set (Iter, Object_Col, GObject (Object));
         end if;
      end Store_Set;

      ---------------------------
      -- Add_Disk_Row (nested) --
      ---------------------------
      type Disk_Type   is (File, Block, Dir, Network, Volume);
      type Disk_Device is (Floppy, Disk, Cdrom, Lun);
      type Target_Bus  is (Ide, Scsi, Virtio, Xen, Usb, Sata, Sd);

      procedure Add_Disk_Row
        (C      : XMLTree.Cursor;
         Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      is
         Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
         Child    : XMLTree.Cursor;
         D_Type   : Disk_Type;
         D_Device : Disk_Device;
         Bus      : Target_Bus;
         Box      : Gtk_Vbox;
         Frame    : Gtk_Frame;
         Grid     : Gtk_Grid;
      begin
         Store.Insert (Iter, Parent, -1);
         -- this should ever exist
         D_Type := Disk_Type'Value (Get_Attribute (C, "type"));
         -- device attr is not mandatory, if absent default to disk
         declare
            Device : String := Get_Attribute (C, "device");
         begin
            D_Device := (if Device = "" then Disk
                         else Disk_Device'Value (Device));
         end;
         Child := Get_By_Path (C, "target");
         declare
            Dev : String := Get_Attribute (Child, "dev");
            Dev_Start : String := Dev (Dev'First..Dev'First+1);
         begin
            Bus := Target_Bus'Value (Get_Attribute (Child, "bus"));
         exception
            when Constraint_Error =>
               -- bus is omitted. (I have never seen it, but the documentation
               -- says it can be) so we try to infer the bus from the dev name
               Bus := (if Dev_Start = "vd" then Virtio
                       elsif Dev_Start = "sd" then Scsi
                       else Ide);
         end;

         Gtk.Box.Gtk_New_Vbox (Box);

         case D_Device is
            when Floppy =>
               Store_Set (Iter, Target_Bus'Image (Bus) & " Floppy", Stock_Floppy);
               Frame := Create_Labeled_Frame (Target_Bus'Image (Bus) & " Floppy");
            when Cdrom =>
               Store_Set (Iter,  Target_Bus'Image (Bus) & " CDROM", Stock_Cdrom);
               Frame := Create_Labeled_Frame (Target_Bus'Image (Bus) & " CDROM");
           when Lun =>
               Store_Set (Iter, Target_Bus'Image (Bus) & " LUN", "network-server");
               Frame := Create_Labeled_Frame (Target_Bus'Image (Bus) & " LUN");
            when Disk =>
               Store_Set (Iter, Target_Bus'Image (Bus) & " Disk", Stock_Harddisk);
               Frame := Create_Labeled_Frame (Target_Bus'Image (Bus) & " Disk");
         end case;

         Grid  := Create_List_Grid;
         Frame.Add (Grid);
         Child := Get_By_Path (C, "source");
         declare
            Path : String :=
              (case D_Type is
                  when File    => Get_Attribute (Child, "file"),
                  when Block   => Get_Attribute (Child, "dev"),
                 -- never tested
                  when Dir     => Get_Attribute (Child, "dir"),
                  when Network => Get_Attribute (Child, "protocol"),
                  when Volume  => Get_Attribute (Child, "volume"));
         begin
            Add_Info_Row (Grid, "Path:", Path);
         end;

         Child := Get_By_Path (C, "readonly");
         Add_Info_Row (Grid, "Readonly:",
                       (if Child = No_Element
                        then "No" else "Yes"));

         Child := Get_By_Path (C, "shareable");
         Add_Info_Row (Grid, "Shareable:",
                       (if Child = No_Element
                        then "No" else "Yes"));

         Box.Pack_Start (Frame, False);
         Store.Set (Iter, Object_Col, GObject (Box));

      end Add_Disk_Row;

      ------------------------------
      -- Add_Generic_Row (nested) --
      ------------------------------
      procedure Add_Generic_Row
        (C      : XMLTree.Cursor;
         Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      is
         use Monitors.Strings_Maps;

         Box   : Gtk_Vbox;
         Frame : Gtk_Frame;
         Grid  : Gtk_Grid;
         Name  : String := Get_Name (C);
         AC    : Strings_Maps.Cursor;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Child : XMLTree.Cursor;
      begin
         Store.Insert (Iter, Parent, -1);
         if Name = "devices" then
            Store_Set (Iter, Name);
            -- search disks
            for I in Positive loop
               Child := Get_By_Path (C, "disk", I);
               exit when Child = XMLTree.No_Element;
               Add_Disk_Row (Child, Iter);
               Set_Seen (Domain_Tree, Child);
            end loop;
         elsif Name = "controller" then
            AC := Domain_Tree (C).Attributes.Find ("type");
            if AC /= Strings_Maps.No_Element then
               if Domain_Tree (C).Attributes (AC) = "ide" then
                  Store_Set (Iter, "Controller IDE", Stock_Harddisk);
               elsif Domain_Tree (C).Attributes (AC) = "fdc" then
                  Store_Set (Iter, "Controller Floppy Disk", Stock_Floppy);
               elsif Domain_Tree (C).Attributes (AC) = "scsi" then
                  Store_Set (Iter, "Controller SCSI", Stock_Harddisk);
               elsif Domain_Tree (C).Attributes (AC) = "sata" then
                  Store_Set (Iter, "Controller Sata", Stock_Harddisk);
               elsif Domain_Tree (C).Attributes (AC) = "usb" then
                  Store_Set (Iter, "Controller USB");
               elsif Domain_Tree (C).Attributes (AC) = "ccid" then
                  Store_Set (Iter, "Controller CCID", "media-flash");
               elsif Domain_Tree (C).Attributes (AC) = "virtio-serial" then
                  Store_Set (Iter, "Controller VirtIO Serial");
               elsif Domain_Tree (C).Attributes (AC) = "pci" then
                  Store_Set (Iter, "Controller PCI");
               else-- shouldn't happen
                  Store_Set (Iter, Name);
               end if;
            else
               -- shouldn't happen
               Store_Set (Iter, Name);
            end if;
         else
            Store_Set (Iter, Name);
         end if;

         Gtk.Box.Gtk_New_Vbox (Box);

         Frame :=
           Create_Labeled_Frame
             (Get_String (Gtk_Root_Tree_Model (Store), Iter, Name_Col));

         Grid := Create_List_Grid;

         if Get_Text (C) /= "" then
            Add_Info_Row (Grid, "Text:", Get_Text (C));
         end if;

         for Ac in Domain_Tree (C).Attributes.Iterate loop
            Add_Info_Row (Grid, Key (Ac) & " =", Element (Ac));
         end loop;

         Frame.Add (Grid);
         Box.Pack_Start (Frame, False);

         Store.Set (Iter, Object_Col, GObject (Box));

         for Child in Iterate_Children (Domain_Tree, C) loop
            if not Domain_Tree (Child).Seen then
               Add_Generic_Row (Child, Iter);
            end if;
         end loop;

      end Add_Generic_Row;

      -----------------------------
      -- Add_Memory_Row (nested) --
      -----------------------------
      procedure Add_Memory_Row
        (C      : XMLTree.Cursor;
         Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      is
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Child : XMLTree.Cursor;

         Box   : Gtk_Vbox;
         Frame : Gtk_Frame;
         Grid  : Gtk_Grid;

      begin
         Store.Insert (Iter, Parent, -1);

         Gtk.Box.Gtk_New_Vbox (Box);
         Frame := Create_Labeled_Frame ("Memory");
         Grid  := Create_List_Grid;

         Child := Get_By_Path (C, "memory");
         Add_Spinbutton_Row (Grid, "Memory:",
                             Long_Float (Long_Integer'Value (Get_Text (Child))));
         Set_Seen (Domain_Tree, Child);


         Child := Get_By_Path (C, "currentMemory");
         Add_Info_Row (Grid, "Current:", Get_Text (Child) & " " &
                         Get_Attribute (Child, "unit"));
         Set_Seen (Domain_Tree, Child);

         Frame.Add (Grid);
         Box.Pack_Start (Frame, False);
         Store_Set (Iter, "Memory", "", Box);
      end Add_Memory_Row;

      -----------------------------
      -- Add_Domain_Row (nested) --
      -----------------------------
      procedure Add_Domain_Row
        (C      : XMLTree.Cursor;
         Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      is

         Box   : Gtk_Vbox;
         Frame : Gtk_Frame;
         Grid  : Gtk_Grid;
         Name  : String := Get_Name (C);
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Child : XMLTree.Cursor;

      begin
         -- This shouldn't happen
         if Name /= "domain" then
            return;
         end if;

         Store.Insert (Iter, Parent, -1);

         Gtk.Box.Gtk_New_Vbox (Box);

         Frame := Create_Labeled_Frame ("Domain");
         Grid := Create_List_Grid;
         Frame.Add (Grid);
         Box.Pack_Start (Frame, False);
         --
         -- Set basic informations
         --
         Child := Get_By_Path (C, "name");
         Add_Entry_Row (Grid, "Name:", Get_Text (Child));
         Set_Seen (Domain_Tree, Child);

         Child := Get_By_Path (C, "uuid");
         Add_Info_Row (Grid, "UUID:", Get_Text (Child));
         Set_Seen (Domain_Tree, Child);

         Child := Get_By_Path (C, "title");
         Add_Entry_Row (Grid, "title:", Get_Text (Child));
         Set_Seen (Domain_Tree, Child);

         Child := Get_By_Path (C, "description");
         Add_Entry_Row (Grid, "description:", Get_Text (Child));
         Set_Seen (Domain_Tree, Child);
         --
         -- Set hypervisor informations
         --
         Frame := Create_Labeled_Frame ("Hypervisor");
         Grid := Create_List_Grid;
         Frame.Add (Grid);
         Box.Pack_Start (Frame, False);

         Child := Get_By_Path (C, "os/type");
         Add_Info_Row (Grid, "type:", Get_Attribute (C, "type"));
         Add_Info_Row (Grid, "OS:", Get_Text(Child));
         Add_Info_Row (Grid, "machine:", Get_Attribute (Child, "machine"));
         Add_Info_Row (Grid, "architecture:", Get_Attribute (Child, "arch"));
         Set_Seen (Domain_Tree, Child);
         Child := Get_By_Path (C, "devices/emulator");
         Add_Info_Row (Grid, "emulator:", Get_Text (Child));
         Set_Seen (Domain_Tree, Child);

         Add_Memory_Row (C, Iter);

         -- Ignore resource partitioning
         Child := Get_By_Path (C, "resource");
         if Child /= No_Element then
            Set_Seen (Domain_Tree, Child);
         end if;

         for Child in Iterate_Children (Domain_Tree, C) loop
            if not Domain_Tree (Child).Seen then
               Add_Generic_Row (Child, Iter);
               Domain_Tree (Child).Seen := True;
            end if;
         end loop;

         Store_Set (Iter, "Domain", "computer", Box);

      end Add_Domain_Row;

      Left_Frame  : Gtk_Frame;
      Right_Frame : Gtk_Frame;

      Window       : Gtk_Window;
      Paned        : Gtk_Paned;
      Scrolled_Win : Gtk_Scrolled_Window;

      Tree_View   : Gtk_Tree_View;
      Col         : Gtk_Tree_View_Column;
      Selection   : Gtk_Tree_Selection;
      Text_Render : Gtk_Cell_Renderer_Text;
      Icon_Render : Gtk_Cell_Renderer_Pixbuf;

      Domain      : VM_Type;
      Cursor      : XMLTree.Cursor;
      Unique_Name : String := "domain:" & Server_Name & "/" & Domain_Name;

   begin -- Create_Domain_Window
      -- check if a window with same name is open
      if Switch_To_Window (Unique_Name) then
         return;
      end if;
      -- retrieve the domain
      Domain := Get_Domain (Server_Name, Domain_Name);

      Gtk.Window.Gtk_New (Window);
      Window.Set_Name (Unique_Name);

      Window.Set_Title
        ("Domain: " & Domain_Name & " Hypervisor: " & Server_Name);

      Window.Set_Icon (Main_Icon.Icon);

      Handlers.Return_Callback.Connect (Window, "delete_event", Delete'Access);
      ---------------------------
      -- Add sliding horizontal paned
      ---------------------------
      Gtk.Paned.Gtk_New_Hpaned (Paned);
      Window.Add (Paned);

      Gtk.Frame.Gtk_New (Left_Frame);
      Left_Frame.Set_Shadow_Type (Shadow_In);

      Gtk.Frame.Gtk_New (Right_Frame);
      Right_Frame.Set_Shadow_Type (Shadow_In);

      Paned.Add1 (Left_Frame);
      Paned.Add2 (Right_Frame);
      Left_Frame.Set_Size_Request (200, 500);
      Right_Frame.Set_Size_Request (400, 500);
      Gtk.Scrolled_Window.Gtk_New (Scrolled_Win);
      Left_Frame.Add (Scrolled_Win);

      -----------------------------------
      -- Create the store and populate it
      -- with domain info
      ------------------------------------
      Gtk.Tree_Store.Gtk_New
        (Store,
         (Name_Col   => GType_String,
          Icon_Col   => GType_String,
          Object_Col => GType_Object));

      Domain_Tree := XML_To_Tree (Domain.Get_XML_Desc);
      Cursor      := XMLTree.First_Child (Domain_Tree.Root);
      -- populate the store
      Add_Domain_Row (Cursor, Gtk.Tree_Model.Null_Iter);

      Gtk.Tree_View.Gtk_New (Tree_View, Store);
      Gtk.Tree_View_Column.Gtk_New (Col);
      Col.Set_Title (Domain_Name);
      -- ignore the return value from append_column
      if Tree_View.Append_Column (Col) = 0 then
         null;
      end if;
      -- Set callback for row selected
      Selection := Gtk.Tree_View.Get_Selection (Tree_View);
      Selection_Cb.Connect
        (Selection,
         "changed",
         Selection_Cb.To_Marshaller (On_Selection'Access),
         Right_Frame);

      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Icon_Render);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Col.Pack_Start (Icon_Render, False);
      Col.Pack_Start (Text_Render, True);
      Col.Add_Attribute (Icon_Render, "icon-name", Icon_Col);
      Col.Add_Attribute (Text_Render, "text", Name_Col);
      Set_Property (Icon_Render, Stock_Size_Property, 3);

      Tree_View.Expand_To_Path (Store.Get_Path (Store.Get_Iter_First));
      Scrolled_Win.Add (Tree_View);
      Window.Show_All;

   exception
      when Domain_Not_Found =>
         null;
   end Create_Domain_Window;

end Domain_Window;
