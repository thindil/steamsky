--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C.Strings;
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with BasesTypes;
with Careers;
with Factions;
with Game;
with Items;
with Log;

package body Help is

   procedure Load_Help(Reader: Tree_Reader; File_Name: String) is
      use Ada.Characters.Handling;
      use Ada.Characters.Latin_1;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use DOM.Core;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use BasesTypes;
      use Factions;
      use Game;
      use Log;
      use Short_String;
      use Tiny_String;

      Tmp_Help: Help_Data := Empty_Help;
      Nodes_List: Node_List;
      Help_Xml_Data: Document;
      Action: Data_Action := ADD;
      Help_Index, Help_Title: Unbounded_String := Null_Unbounded_String;
      Help_Node: Node;
      Result: chars_ptr;
      function Load_Ada_Help(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaHelp";
   begin
      Result := Load_Ada_Help(Name => New_String(Str => File_Name));
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Help_Xml_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Help_Xml_Data, Tag_Name => "entry");
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
         if Action = REMOVE then
            Help_Container.Exclude(Container => Help_List, Key => Help_Title);
            Log_Message
              (Message => "Help removed: " & To_String(Source => Help_Title),
               Message_Type => EVERYTHING);
         else
            Tmp_Help.Index := Help_Index;
            if Action = UPDATE then
               Tmp_Help := Help_List(Help_Title);
            end if;
            if Has_Child_Nodes(N => Help_Node) then
               Tmp_Help.Text :=
                 To_Unbounded_String
                   (Source => Node_Value(N => First_Child(N => Help_Node)));
            end if;
            if Action = UPDATE then
               Help_List(Help_Title) := Tmp_Help;
            else
               Help_Container.Include
                 (Container => Help_List, Key => Help_Title,
                  New_Item => Tmp_Help);
               Log_Message
                 (Message => "Help added: " & To_String(Source => Help_Title),
                  Message_Type => EVERYTHING);
            end if;
         end if;
      end loop Load_Help_Data_Loop;
      -- Add help page about available statistics and attributes
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
             LF & LF & "{u}Attributes{/u}" & LF & LF);
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
      Append
        (Source => Tmp_Help.Text, New_Item => LF & "{u}Skills{/u}" & LF & LF);
      Load_Skills_Loop :
      for I in 1 .. Skills_Amount loop
         Load_Skills_Block :
         declare
            use Items;

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
            for J in 1 .. Get_Proto_Amount loop
               if Get_Proto_Item(Index => J).I_Type = Skill.Tool then
                  Append
                    (Source => Tmp_Help.Text,
                     New_Item =>
                       "    {i}Training tool:{/i} " &
                       (if
                          Get_Proto_Item(Index => J).Show_Type =
                          Tiny_String.Null_Bounded_String
                        then
                          To_String
                            (Source => Get_Proto_Item(Index => J).I_Type)
                        else To_String
                            (Source => Get_Proto_Item(Index => J).Show_Type)) &
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
      -- Add help page about available careers and factions
      Tmp_Help.Index := To_Unbounded_String(Source => "factions");
      Help_Title :=
        To_Unbounded_String
          (Source =>
             Trim
               (Source => Positive'Image(Positive(Help_List.Length) + 1),
                Side => Left) &
             ". Factions and careers");
      Tmp_Help.Text :=
        To_Unbounded_String
          (Source =>
             "Here you will find information about all available factions and careers in the game" &
             LF & LF & "{u}Factions{/u}" & LF & LF);
      Load_Factions_Info_Loop :
      for I in 1 .. Get_Factions_Amount loop
         Load_Faction_Block :
         declare
            Faction: constant Faction_Record := Get_Faction(Number => I);
         begin
            if Faction.Careers.Length > 0 then
               Append
                 (Source => Tmp_Help.Text,
                  New_Item =>
                    "{b}" & To_String(Source => Faction.Name) & "{/b}" & LF &
                    "    " & To_String(Source => Faction.Description) & LF &
                    "    {i}Relations{/b}" & LF);
               Show_Relations_Loop :
               for J in Faction.Relations.Iterate loop
                  Append
                    (Source => Tmp_Help.Text,
                     New_Item =>
                       "        " &
                       To_String
                         (Source =>
                            Get_Faction
                              (Index => Relations_Container.Key(Position => J))
                              .Name) &
                       ": " &
                       (if Faction.Relations(J).Friendly then "Friendly"
                        else "Enemies") &
                       LF);
               end loop Show_Relations_Loop;
               Append(Source => Tmp_Help.Text, New_Item => LF);
            end if;
         end Load_Faction_Block;
      end loop Load_Factions_Info_Loop;
      Append
        (Source => Tmp_Help.Text, New_Item => LF & "{u}Careers{/u}" & LF & LF);
      Show_Careers_Block :
      declare
         use Careers;

         Faction: constant Faction_Record :=
           Get_Faction(Index => To_Bounded_String(Source => "POLEIS"));
      begin
         Load_Careers_Info_Loop :
         for I in Careers_List.Iterate loop
            Append
              (Source => Tmp_Help.Text,
               New_Item =>
                 "{b}" & To_String(Source => Careers_List(I).Name) & "{/b}" &
                 LF &
                 Faction.Careers(Careers.Careers_Container.Key(Position => I))
                   .Description &
                 LF);
            if Careers_List(I).Skills.Length > 0 then
               Append
                 (Source => Tmp_Help.Text,
                  New_Item => "    {i}Bonus to skills{/b}" & LF);
               Show_Skills_Loop :
               for Skill of Careers_List(I).Skills loop
                  Append
                    (Source => Tmp_Help.Text,
                     New_Item => "        " & To_String(Source => Skill) & LF);
               end loop Show_Skills_Loop;
            end if;
            Append(Source => Tmp_Help.Text, New_Item => LF);
         end loop Load_Careers_Info_Loop;
      end Show_Careers_Block;
      Help_List.Include(Key => Help_Title, New_Item => Tmp_Help);
      Log_Message
        (Message => "Help added: " & To_String(Source => Help_Title),
         Message_Type => EVERYTHING);
      -- Add help page about available careers and factions
      Tmp_Help.Index := To_Unbounded_String(Source => "basestypes");
      Help_Title :=
        To_Unbounded_String
          (Source =>
             Trim
               (Source => Positive'Image(Positive(Help_List.Length) + 1),
                Side => Left) &
             ". Bases Types");
      Tmp_Help.Text :=
        To_Unbounded_String
          (Source =>
             "Here you will find information about all available bases types in the game" &
             LF & LF);
      Load_Bases_Types_Info_Loop :
      for Index of Bases_Types loop
         exit Load_Bases_Types_Info_Loop when Get_Base_Type_Name
             (Base_Type => Index)'
             Length =
           0;
         Append
           (Source => Tmp_Help.Text,
            New_Item =>
              "{b}" & Get_Base_Type_Name(Base_Type => Index) & "{/b}" & LF &
              "    " & Get_Base_Type_Description(Base_Type => Index) & LF &
              LF);
      end loop Load_Bases_Types_Info_Loop;
      Help_List.Include(Key => Help_Title, New_Item => Tmp_Help);
      Log_Message
        (Message => "Help added: " & To_String(Source => Help_Title),
         Message_Type => EVERYTHING);
   end Load_Help;

end Help;
