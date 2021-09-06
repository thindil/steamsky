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

   procedure LoadHelp(Reader: Tree_Reader) is
      use Short_String;
      use Standard_String;
      use Tiny_String;

      TmpHelp: Help_Data;
      NodesList: Node_List;
      HelpData: Document;
      Action: Data_Action;
      HelpIndex, HelpTitle: Unbounded_String;
      HelpNode: Node;
   begin
      HelpData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(HelpData, "entry");
      Load_Help_Data :
      for I in 0 .. Length(NodesList) - 1 loop
         TmpHelp :=
           (Index => Null_Unbounded_String, Text => Null_Unbounded_String);
         HelpNode := Item(NodesList, I);
         Action :=
           (if Get_Attribute(HelpNode, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(HelpNode, "action"))
            else ADD);
         HelpIndex := To_Unbounded_String(Get_Attribute(HelpNode, "index"));
         HelpTitle := To_Unbounded_String(Get_Attribute(HelpNode, "title"));
         if Action in UPDATE | REMOVE then
            if not Help_Container.Contains(Help_List, HelpTitle) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " help '" & To_String(HelpTitle) &
                 "', there no help with that title.";
            end if;
         elsif Help_Container.Contains(Help_List, HelpTitle) then
            raise Data_Loading_Error
              with "Can't add help '" & To_String(HelpTitle) &
              "', there is one with that title.";
         end if;
         if Action /= REMOVE then
            TmpHelp.Index := HelpIndex;
            if Action = UPDATE then
               TmpHelp := Help_List(HelpTitle);
            end if;
            if Has_Child_Nodes(HelpNode) then
               TmpHelp.Text :=
                 To_Unbounded_String(Node_Value(First_Child(HelpNode)));
            end if;
            if Action /= UPDATE then
               Help_Container.Include(Help_List, HelpTitle, TmpHelp);
               Log_Message("Help added: " & To_String(HelpTitle), EVERYTHING);
            else
               Help_List(HelpTitle) := TmpHelp;
            end if;
         else
            Help_Container.Exclude(Help_List, HelpTitle);
            Log_Message("Help removed: " & To_String(HelpTitle), EVERYTHING);
         end if;
      end loop Load_Help_Data;
      TmpHelp.Index := To_Unbounded_String("stats");
      HelpTitle :=
        To_Unbounded_String
          (Trim(Positive'Image(Positive(Help_List.Length) + 1), Left) &
           ". Attributes and skills");
      TmpHelp.Text :=
        To_Unbounded_String
          ("Here you will find information about all available attributes and skills in the game" &
           LF & LF & "{u}Attributes{/u}" & LF);
      for I in
        AttributesData_Container.First_Index(Container => Attributes_List) ..
          AttributesData_Container.Last_Index
            (Container => Attributes_List) loop
         Append
           (TmpHelp.Text,
            "{b}" &
            To_String
              (AttributesData_Container.Element
                 (Container => Attributes_List, Index => I)
                 .Name) &
            "{/b}" & LF & "    " &
            To_String
              (AttributesData_Container.Element
                 (Container => Attributes_List, Index => I)
                 .Description) &
            LF & LF);
      end loop;
      Append(TmpHelp.Text, LF & "{u}Skills{/u}" & LF);
      for I in
        SkillsData_Container.First_Index(Skills_List) ..
          SkillsData_Container.Last_Index(Skills_List) loop
         Append
           (TmpHelp.Text,
            "{b}" &
            To_String(SkillsData_Container.Element(Skills_List, I).Name) &
            "{/b}" & LF & "    {i}Related attribute:{/i} " &
            To_String
              (AttributesData_Container.Element
                 (Attributes_List,
                  SkillsData_Container.Element(Skills_List, I).Attribute)
                 .Name) &
            LF);
         for Item of Items_List loop
            if Item.IType =
              To_Unbounded_String
                (To_String
                   (SkillsData_Container.Element(Skills_List, I).Tool)) then
               Append
                 (TmpHelp.Text,
                  "    {i}Training tool:{/i} " &
                  (if Item.ShowType = Null_Unbounded_String then Item.IType
                   else Item.ShowType) &
                  LF);
               exit;
            end if;
         end loop;
         Append
           (TmpHelp.Text,
            "    " &
            To_String
              (SkillsData_Container.Element(Skills_List, I).Description) &
            LF & LF);
      end loop;
      Help_List.Include(HelpTitle, TmpHelp);
      Log_Message("Help added: " & To_String(HelpTitle), EVERYTHING);
   end LoadHelp;

end Help;
