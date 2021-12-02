--    Copyright 2016-2021 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Game; use Game;
with Items; use Items;

package body Help is

   procedure Load_Help(Reader: Tree_Reader) is
      use Short_String;
      use Tiny_String;

      Tmp_Help: Help_Data;
      Nodes_List: Node_List;
      Help_Data: Document;
      Action: Data_Action;
      Help_Index, Help_Title: Unbounded_String;
      Help_Node: Node;
   begin
      Help_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Help_Data, Tag_Name => "entry");
      Load_Help_Data_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Tmp_Help :=
           (Index => Null_Unbounded_String, Text => Null_Unbounded_String);
         Help_Node := Item(List => Nodes_List, Index => I);
         Action :=
           (if Get_Attribute(Elem => Help_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Help_Node, Name => "action"))
            else ADD);
         Help_Index :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Help_Node, Name => "index"));
         Help_Title :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Help_Node, Name => "title"));
         if Action in UPDATE | REMOVE then
            if not Help_Container.Contains
                (Container => Help_List, Key => Help_Title) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " help '" & To_String(Source => Help_Title) &
                 "', there no help with that title.";
            end if;
         elsif Help_Container.Contains
             (Container => Help_List, Key => Help_Title) then
            raise Data_Loading_Error
              with "Can't add help '" & To_String(Source => Help_Title) &
              "', there is one with that title.";
         end if;
         if Action /= REMOVE then
            Tmp_Help.Index := Help_Index;
            if Action = UPDATE then
               Tmp_Help := Help_List(Help_Title);
            end if;
            if Has_Child_Nodes(N => Help_Node) then
               Tmp_Help.Text :=
                 To_Unbounded_String
                   (Source => Node_Value(N => First_Child(N => Help_Node)));
            end if;
            if Action /= UPDATE then
               Help_Container.Include
                 (Container => Help_List, Key => Help_Title,
                  New_Item => Tmp_Help);
               Log_Message
                 (Message => "Help added: " & To_String(Source => Help_Title),
                  Message_Type => EVERYTHING);
            else
               Help_List(Help_Title) := Tmp_Help;
            end if;
         else
            Help_Container.Exclude(Container => Help_List, Key => Help_Title);
            Log_Message
              (Message => "Help removed: " & To_String(Source => Help_Title),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Help_Data_Loop;
      Tmp_Help.Index := To_Unbounded_String(Source => "stats");
      Help_Title :=
        To_Unbounded_String
          (Source =>
             Trim
               (Source => Positive'Image(Positive(Help_List.Length) + 1),
                Side => Left) &
             ". Attributes and skills");
      Tmp_Help.Text :=
        To_Unbounded_String
          (Source =>
             "Here you will find information about all available attributes and skills in the game" &
             LF & LF & "{u}Attributes{/u}" & LF);
      Load_Attributes_Loop :
      for I in 1 .. Attributes_Amount loop
         Load_Attributes_Block :
         declare
            Attribute: constant Attribute_Record :=
              AttributesData_Container.Element
                (Container => Attributes_List, Index => I);
         begin
            Append
              (Source => Tmp_Help.Text,
               New_Item =>
                 "{b}" & To_String(Source => Attribute.Name) & "{/b}" & LF &
                 "    " & To_String(Source => Attribute.Description) & LF &
                 LF);
         end Load_Attributes_Block;
      end loop Load_Attributes_Loop;
      Append(Source => Tmp_Help.Text, New_Item => LF & "{u}Skills{/u}" & LF);
      Load_Skills_Loop :
      for I in 1 .. Skills_Amount loop
         Load_Skills_Block :
         declare
            Skill: constant Skill_Record :=
              SkillsData_Container.Element
                (Container => Skills_List, Index => I);
         begin
            Append
              (Source => Tmp_Help.Text,
               New_Item =>
                 "{b}" & To_String(Source => Skill.Name) & "{/b}" & LF &
                 "    {i}Related attribute:{/i} " &
                 To_String
                   (Source =>
                      AttributesData_Container.Element
                        (Container => Attributes_List,
                         Index => Skill.Attribute)
                        .Name) &
                 LF);
            Load_Training_Tools_Loop :
            for Item of Items_List loop
               if Item.I_Type =
                 To_Unbounded_String
                   (Source => To_String(Source => Skill.Tool)) then
                  Append
                    (Source => Tmp_Help.Text,
                     New_Item =>
                       "    {i}Training tool:{/i} " &
                       (if Item.Show_Type = Null_Unbounded_String then
                          Item.I_Type
                        else Item.Show_Type) &
                       LF);
                  exit Load_Training_Tools_Loop;
               end if;
            end loop Load_Training_Tools_Loop;
            Append
              (Source => Tmp_Help.Text,
               New_Item =>
                 "    " & To_String(Source => Skill.Description) & LF & LF);
         end Load_Skills_Block;
      end loop Load_Skills_Loop;
      Help_List.Include(Key => Help_Title, New_Item => Tmp_Help);
      Log_Message
        (Message => "Help added: " & To_String(Source => Help_Title),
         Message_Type => EVERYTHING);
   end Load_Help;

end Help;
