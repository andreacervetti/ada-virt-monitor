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
with Input_Sources.Strings; use Input_Sources.Strings;
with Unicode.CES.Utf8;      use Unicode.CES.Utf8;
with DOM.Core;              use DOM.Core;
with DOM.Core.Nodes;        use DOM.Core.Nodes;
with DOM.Core.Documents;    use DOM.Core.Documents;
with DOM.Readers;           use DOM.Readers;

package body Monitors.XMLTrees is
   package US renames Ada.Strings.Unbounded;

   use XMLTree;

   function Get_Text (C : XMLTree.Cursor) return String
   is
     (if C = No_Element then ""
      else To_String (XMLTree.Element (C).Text_Value));

   function Get_Name (C : XMLTree.Cursor) return String
   is
     (if C = No_Element then ""
      else To_String (XMLTree.Element (C).Element_Name));

   procedure Set_Seen (Tree : in out XMLTree.Tree; C : XMLTree.Cursor) is
   begin
      if C /= No_Element then
         Tree (C).Seen := True;
      end if;
   end Set_Seen;


   function Get_By_Path (Parent: XMLTree.Cursor;
                         Str   : String;
                         Num   : Positive := 1) return XMLTree.Cursor
   is
      S     : String := Str & '/';
      Count : Natural := 0;
      Start : Natural;
      Last  : Natural;
      C     : Xmltree.Cursor;
   begin
      if Parent = No_Element then
         raise Constraint_Error;
      end if;
      for I in  S'Range loop
         if S (I) = '/' then
            Start := I + 1;
            Last := I - 1;
            exit;
         end if;
      end loop;
      C := First_Child (Parent);
      while C /= No_Element loop
         if Get_Name (C) = S (S'First .. Last) then
            if S (Start..S'Last) = "" then
               Count := Count + 1;
               if Count = Num Then
                  return C;
               end if;
            else
               return Get_By_Path (C, Str (Start..Str'Last), Num);
            end if;
         end if;
         Next_Sibling (C);
      end loop;
      return XMLTree.No_Element;
   end Get_By_Path;


   function Get_Attribute (E : XMLTree.Cursor; Str : String)
                           return String is
   begin
      if E = No_Element then
         return "";
      end if;
      declare
         Attrs : Strings_Maps.Map := XMLTree.Element (E).Attributes;
      begin
         for Ac in Attrs.Iterate loop
            if Strings_Maps.Key (Ac) = Str then
               return Strings_Maps.Element (Ac);
            end if;
         end loop;
      end;
      return "";
   end Get_Attribute;


   function Get_By_Path (Tree   : XMLTree.Tree;
                         Str    : String;
                         Num    : Natural := 1) return XMLTree.Cursor
   is
   begin
      return Get_By_Path (Tree.Root, Str, Num);
   end Get_By_Path;

   function XML_To_Tree (XML : String; Empty_Text : Boolean := False)
                         return XMLTree.Tree
   is
      New_Tree : XMLTree.Tree;

      procedure Add_To_Tree (N : Node; C : Cursor) is
         New_Child : Cursor;
         Attrs     : Named_Node_Map;
         Childs    : Node_List;
         Str       : Unbounded_String;
      begin
         case Node_Type (N) is
            when Element_Node =>
               New_Tree.Insert_Child
                 (Parent   => C,
                  Before   => No_Element,
                  New_Item =>
                    (Element_Name => To_Unbounded_String (Node_Name (N)),
                     Seen         => <>,
                     Text_Value   => <>,
                     Attributes   => <>),
                  Position => New_Child);

               Attrs := Attributes (N);
               for I in 0 .. Length (Attrs) - 1 loop
                  Add_To_Tree (Item (Attrs, I), New_Child);
               end loop;

               Childs := Child_Nodes (N);
               for I in 0 .. Length (Childs) - 1 loop
                  Add_To_Tree (Item (Childs, I), New_Child);
               end loop;

            when Text_Node =>
               if not Empty_Text then
                  -- XMLada returns a text node for elements that do not have
                  -- an actual content, only new lines and spaces.
                  -- This is formally correct, but here it can be Cconfusing,
                  -- so we skip them
                  Str := To_Unbounded_String (Node_Value (N));
                  for I in 1 .. Length (Str) loop
                     if US.Element (Str, I) /= ' ' and
                       US.Element (Str, I) /= ASCII.LF
                     then
                        New_Tree (C).Text_Value := Str;
                        exit;
                     end if;
                  end loop;
               end if;

            when Attribute_Node =>
               New_Tree (C).Attributes.Insert (Node_Name (N), Node_Value (N));

            when others =>
               null;
         end case;
      end Add_To_Tree;

      Input  : String_Input;
      Doc    : Document;
      Root   : DOM.Core.Element;
      C      : XMLTree.Cursor;
      Reader : Tree_Reader;

   begin

      Open (XML, Utf8_Encoding, Input);
      Parse (Reader, Input);
      Doc  := Get_Tree (Reader);
      Root := Get_Element (Doc);

      C := New_Tree.Root;
      if Root /= null then
         Add_To_Tree (Root, C);
      end if;

      Free (Reader);
      -- Free(Doc);
      Close (Input);
      return New_Tree;
   end XML_To_Tree;

end Monitors.XMLTrees;
