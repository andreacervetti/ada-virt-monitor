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
with Gtk.Label;           use Gtk.Label;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Enums;           use Gtk.Enums;
with Glib; use Glib;

package body Frame_Helpers is

   function Create_Labeled_Frame (Title : String) return Gtk_Frame is
      Frame : Gtk_Frame;
      Label : Gtk_Label;
   begin
      Gtk.Frame.Gtk_New (Frame);
      Gtk.Label.Gtk_New (Label);

      Frame.Set_Shadow_Type (Shadow_None);
      Frame.Set_Label_Widget (Label);

      Label.Set_Markup ("<b>" & Title & "</b>");
      Label.Set_Halign (Align_Start);

      return Frame;
   end Create_Labeled_Frame;

   function Create_List_Grid return Gtk_Grid is
      Grid : Gtk_Grid;
   begin
      Gtk.Grid.Gtk_New (Grid);
      Grid.Set_Row_Spacing (2);
      Grid.Set_Column_Spacing (5);
      Grid.Set_Border_Width (10);
      Grid.Set_Margin_Start (10);
      return Grid;
   end Create_List_Grid;

   procedure Add_Entry_Row
     (Grid    : Gtk_Grid;
      Title    : String;
      Default : String := "";
      Info    : String := "";
      View    : Boolean := False)
   is

      Label        : Gtk_Label;
      Entry_Field  : Gtk_GEntry;
      Scrolled_Win : Gtk_Scrolled_Window;
      Text_View    : Gtk_Text_View;

   begin
      Gtk.Label.Gtk_New (Label);
      Label.Set_Halign (Align_Start);
      Label.Set_Label (Title);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);

      if View then
         Gtk.Scrolled_Window.Gtk_New (Scrolled_Win);
         Scrolled_Win.Set_Hexpand (True);
         Scrolled_Win.Set_Shadow_Type (Shadow_In);

         Gtk.Text_View.Gtk_New (Text_View);
         Text_View.Set_Hexpand (True);
         Scrolled_Win.Add (Text_View);
         Grid.Attach_Next_To (Scrolled_Win, Label, Pos_Right);
         if Info /= "" then
            Gtk.Label.Gtk_New (Label);
            Label.Set_Halign (Align_Start);
            Label.Set_Label (Info);
            Grid.Attach_Next_To (Label, Scrolled_Win, Pos_Right);
         end if;
      else
         Gtk.GEntry.Gtk_New (Entry_Field);
         Entry_Field.Set_Hexpand (True);
         if Default /= "" then
            Entry_Field.Set_Text (Default);
         end if;
         Grid.Attach_Next_To (Entry_Field, Label, Pos_Right);
         if Info /= "" then
            Gtk.Label.Gtk_New (Label);
            Label.Set_Halign (Align_Start);
            Label.Set_Label (Info);
            Grid.Attach_Next_To (Label, Entry_Field, Pos_Right);
         end if;
      end if;

   end Add_Entry_Row;


   procedure Add_Info_Row
     (Grid         : Gtk_Grid;
      Title        : String;
      Information  : String;
      Name         : String := "")
   is
      Label : Gtk_Label;
      Info  : Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label);
      Label.Set_Halign (Align_Start);
      Gtk.Label.Gtk_New (Info);
      Info.Set_Halign (Align_Start);
      Label.Set_Label (Title);
      Info.Set_Label (Information);
      if Name /= "" then
         Info.Set_Name (Name);
      end if;
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      Grid.Attach_Next_To (Info, Label, Pos_Right);
   end Add_Info_Row;

   procedure Add_Spinbutton_Row
     (Grid    : Gtk_Grid;
      Title    : String;
      Value   : Long_Float)
   is
      Label : Gtk_Label;
      Data  : Gtk_Spin_Button;
   begin
      Gtk.Label.Gtk_New (Label);
      Label.Set_Halign (Align_Start);
      Label.Set_Label (Title);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);

      Gtk.Spin_Button.Gtk_New (Data, null, 1024.0);
      Data.Set_Range (0.0, 32_000_000.0);
      Data.Set_Value (Gdouble(Value));
      Grid.Attach_Next_To (Data, Label, Pos_Right);
   end Add_Spinbutton_Row;

   procedure Add_Combo_Row
     (Grid     : Gtk_Grid;
      Title    : String;
      Combo    : access Gtk_Combo_Box_Record'Class;
      Name     : String := "")
   is
      Label : Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label);
      Label.Set_Halign (Align_Start);
      Label.Set_Label (Title);
      Grid.Attach_Next_To (Label, null, Pos_Bottom);
      if Name /= "" then
         Combo.Set_Name (Name);
      end if;
      Grid.Attach_Next_To (Combo, Label, Pos_Right);
   end Add_Combo_Row;


end Frame_Helpers;
