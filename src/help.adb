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
               Log_Message("Help added: " & To_String(Help_Title), EVERYTHING);
            else
               Help_List(Help_Title) := Tmp_Help;
            end if;
         else
            Help_Container.Exclude(Help_List, Help_Title);
            Log_Message("Help removed: " & To_String(Help_Title), EVERYTHING);
         end if;
      end loop Load_Help_Data_Loop;
      Tmp_Help.Index := To_Unbounded_String("stats");
      Help_Title :=
        To_Unbounded_String
          (Trim(Positive'Image(Positive(Help_List.Length) + 1), Left) &
           ". Attributes and skills");
      Tmp_Help.Text :=
        To_Unbounded_String
          ("Here you will find information about all available attributes and skills in the game" &
           LF & LF & "{u}Attributes{/u}" & LF);
      for I in 1 .. Attributes_Amount loop
         declare
            Attribute: constant Attribute_Record :=
              AttributesData_Container.Element
                (Container => Attributes_List, Index => I);
         begin
            Append
              (Tmp_Help.Text,
               "{b}" & To_String(Attribute.Name) & "{/b}" & LF & "    " &
               To_String(Attribute.Description) & LF & LF);
         end;
      end loop;
      Append(Tmp_Help.Text, LF & "{u}Skills{/u}" & LF);
      for I in 1 .. Skills_Amount loop
         declare
            Skill: constant Skill_Record :=
              SkillsData_Container.Element(Skills_List, I);
         begin
            Append
              (Tmp_Help.Text,
               "{b}" & To_String(Skill.Name) & "{/b}" & LF &
               "    {i}Related attribute:{/i} " &
               To_String
                 (AttributesData_Container.Element
                    (Attributes_List, Skill.Attribute)
                    .Name) &
               LF);
            for Item of Items_List loop
               if Item.IType = To_Unbounded_String(To_String(Skill.Tool)) then
                  Append
                    (Tmp_Help.Text,
                     "    {i}Training tool:{/i} " &
                     (if Item.ShowType = Null_Unbounded_String then Item.IType
                      else Item.ShowType) &
                     LF);
                  exit;
               end if;
            end loop;
            Append
              (Tmp_Help.Text, "    " & To_String(Skill.Description) & LF & LF);
         end;
      end loop;
      Help_List.Include(Help_Title, Tmp_Help);
      Log_Message("Help added: " & To_String(Help_Title), EVERYTHING);
   end Load_Help;

end Help;
