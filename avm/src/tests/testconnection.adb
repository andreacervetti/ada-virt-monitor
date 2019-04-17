with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Containers; use Ada.Containers;

with Virtada.Host; use Virtada.Host;
with Virtada.Host.Domain; use Virtada.Host.Domain;
use Virtada;

with Monitors.XMLTrees; use Monitors.XMLTrees;
use Monitors;

procedure Testconnection is

   Connection : Connect_Type;

   procedure Print_Tree (Tree : Xmltree.Tree) is
   begin
      for E in Tree.Iterate loop
         Put (Natural(XMLTree.Depth(E) - 2) * "   ");
         Put_Line (To_String(Tree(E).Element_Name));
         for C in Tree(E).Attributes.Iterate loop
            Put (Natural(XMLTree.Depth(E) - 2) * "   ");
            Put_Line(Strings_Maps.Key(C) & "=" & Strings_Maps.Element(C));
         end loop;
         if Tree(E).Text_Value /= Null_Unbounded_String then
            Put (Natural(XMLTree.Depth(E) - 2) * "   ");
            Put(" text: ");
            Put_Line(To_String(Tree(E).Text_Value));
         end if;
      end loop;
   end Print_Tree;

begin

   if Argument_Count /= 1 then
      Set_Exit_Status (Failure);
      Put_Line
        (Standard_Error,
         "usage: " & Simple_Name (Command_Name) & " host");
      return;
   end if;

   declare
      Uri : String := "qemu+ssh://root@" & Argument (1) & "/system";
   begin
      Connection.Connect (Uri);
      if not Is_Connected (Connection) then
         raise Connect_Failed;
      end if;

      Put_Line ("connection: " & Uri);
      Put_Line ("Host: " & Host_Name (Connection));
      Put_Line ("Max VCPU:" & Integer'Image(Get_Max_VCpus(Connection)));
--        Print_Tree (XML_To_Tree(Get_Capabilities (Connection)));
      -- Print_Tree (XML_To_Tree(Get_SysInfo(Connection)));

      declare
         Domains : Domain_Array := List_Domains (Connection);
         Dom     : Domain_Type;
      begin
         for I in Domains'Range loop
            Dom := Domains (I);
            Print_Tree (XML_To_Tree(Dom.Get_XML_Desc));
--              if I = Domains'First then
--                 New_Line;
--                 Put (" ID");
--                 Set_Col (14);
--                 Put ("STATUS");
--                 Set_Col (26);
--                 Put ("VM");
--                 New_Line;
--              end if;
--              Dom := Extend (Domains (I));
--              Put (ID (Dom));
--              Set_Col (14);
--              if Is_Active(Dom) then
--                 Put (State_Image(Dom));
--              else
--                 Put ("not running");
--              end if;
--              Set_Col (26);
--              Put (Get_Name (Dom));
--              New_Line;
            exit;
         end loop;
      end;
      -- Disconnect (Connection);

   exception
      when Connect_Failed =>
         Put_Line (Standard_Error, "Failed to open connection to " & Uri);
      when Close_Failed =>
         Put_Line (Standard_Error, "Failed closing connection to " & Uri);
   end;
end Testconnection;
