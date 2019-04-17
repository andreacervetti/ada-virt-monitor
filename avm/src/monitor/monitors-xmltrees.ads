-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2018 Andrea Cervetti                      --
--               Copyright (C) 2018 Homebrew Internet s.r.l.               --
--                                                                         --
-- This library is free software: you can redistribute it and/or modify    --
-- it under the terms of the GNU General Public License as published by    --
-- the Free Software Foundation, either version 3 of the License, or       --
-- (at your option) any later version.                                     --
--                                                                         --
-- As a special exception, if other files instantiate generics from        --
-- this unit, or you link this unit with other files to produce an         --
-- executable, this unit does not by itself cause the resulting executable --
-- to be covered by the GNU General Public License. This exception does    --
-- not however invalidate any other reasons why the executable file might  --
-- be covered by the GNU Public License.                                   --
--                                                                         --
-- This library is distributed in the hope that it will be useful,         --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of          --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
-- GNU General Public License for more details.                            --
--                                                                         --
-- You should have received a copy of the GNU General Public License       --
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                         --
-----------------------------------------------------------------------------
--
-- Convert a XML String to a multiway tree
--
-- Although it seems silly convert from a tree structure to a tree structure,
-- there are a few reasons to do this.
--
--  - Manipulating a DOM tree require more memory than a simple ADA tree.
--    We build the tree from a DOM document then we can discard the document
--    to free the memory.
--  - Multiway trees are easier to walk through cursors than DOM Object.
--  - We don't need the full amount of informations stored in a DOM document.
--  - Containers are finalized. We do not have to deal with the complexity
--    of freeing the object created by the various xml DOM calls in different
--    parts of the program.
-----------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;

package Monitors.XMLTrees is

   subtype Count_Type is Ada.Containers.Count_Type;

   type Tree_Element is record
      Element_Name : Unbounded_String;
      Seen         : Boolean          := False;
      Text_Value   : Unbounded_String := Null_Unbounded_String;
      Attributes   : Strings_Maps.Map := Strings_Maps.Empty_Map;
   end record;

   package XMLTree is new Ada.Containers.Multiway_Trees (Tree_Element);

   function XML_To_Tree (XML : String; Empty_Text : Boolean := False)
                         return XMLTree.Tree;
   function Get_Text (C : XMLTree.Cursor) return String;
   function Get_Name (C : XMLTree.Cursor) return String;
   procedure Set_Seen (Tree : in out XMLTree.Tree; C : XMLTree.Cursor);
   function Get_Attribute (E : XMLTree.Cursor; Str : String)
                           return String;

   function Get_By_Path (Parent : XMLTree.Cursor;
                         Str    : String;
                         Num    : Positive := 1) return XMLTree.Cursor;

   function Get_By_Path (Tree   : XMLTree.Tree;
                         Str    : String;
                         Num    : Natural := 1) return XMLTree.Cursor;

end Monitors.XMLTrees;
