with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Direct_IO;

with Monitors.XMLTrees; use Monitors.XMLTrees;
use Monitors.XMLTrees.XMLTree;
with Monitors;
use Monitors;

procedure Find_Element is
   
   Tree : Xmltree.Tree;

begin
   if Argument_Count /= 2 then
      Set_Exit_Status (Failure);
      Put_Line
        (Standard_Error,
         "usage: " & Simple_Name (Command_Name) & " filename string");
      return;
   end if;
   
   declare
      File_Size : Natural :=
        Natural (Ada.Directories.Size (Argument (1)));
      
      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name       => Argument (1));
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);
      
      Tree := XML_To_Tree (Contents);
      
      if Get_By_Path (Tree, Argument (2)) = Xmltree.No_Element then
         Put_Line ("Not found.");
      else
         Put_Line ("Found!");
      end if;  
   end;
   
--     New_Line;
--     
--     Print_Tree (Tree, Tree.Root);
end Find_Element;
