--    Copyright 2018-2022 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces.C.Strings;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Utils; use Utils;
with Careers; use Careers;
with Items; use Items;
with BasesTypes; use BasesTypes;

package body Factions is

   procedure Load_Factions(Reader: Tree_Reader; File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Tiny_String;

      Temp_Record: Faction_Record;
      Nodes_List, Child_Nodes: Node_List;
      Factions_Data: Document;
      Tmp_Relations: Relations_Container.Map;
      Tmp_Relation: Relations_Record;
      Tmp_Food: TinyString_Container.Vector;
      Value, Career_Index: Unbounded_String;
      Relation_Index, Faction_Index: Tiny_String.Bounded_String;
      Skill_Index: SkillsData_Container.Extended_Index;
      Item_Index: Objects_Container.Extended_Index;
      Tmp_Careers: Factions.Careers_Container.Map;
      Tmp_Career: Factions.Career_Record;
      Faction_Node, Child_Node: Node;
      Delete_Index: Positive;
      Action, Sub_Action: Data_Action;
      Tmp_Base_Type_Chance: Positive;
      Tmp_Bases_Types: BaseType_Container.Map;
      Tmp_Flags: UnboundedString_Container.Vector;
      function Get_Action(Current_Node: Node) return Data_Action is
      begin
         return
           (if Get_Attribute(Elem => Current_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Current_Node, Name => "action"))
            else ADD);
      end Get_Action;
      procedure Add_Child_Node
        (Data: in out TinyString_Container.Vector; Name: String;
         Index: Natural; Check_Item_Type: Boolean := True) is
         Value: Bounded_String;
      begin
         Child_Nodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Elem => Item(List => Nodes_List, Index => Index), Name => Name);
         Load_Items_Loop :
         for J in 0 .. Length(List => Child_Nodes) - 1 loop
            Child_Node := Item(List => Child_Nodes, Index => J);
            Value :=
              To_Bounded_String
                (Source => Get_Attribute(Elem => Child_Node, Name => "name"));
            Sub_Action := Get_Action(Current_Node => Child_Node);
            if Check_Item_Type then
               Item_Index := Find_Proto_Item(Item_Type => Value);
               if Item_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no items with type '" & To_String(Source => Value) &
                    "'.";
               end if;
            end if;
            if Sub_Action /= REMOVE then
               Data.Append(New_Item => Value);
            else
               Delete_Index := Data.First_Index;
               Delete_Item_Loop :
               while Delete_Index <= Data.Last_Index loop
                  if Data(Delete_Index) = Value then
                     Data.Delete(Index => Delete_Index);
                     exit Delete_Item_Loop;
                  end if;
                  Delete_Index := Delete_Index + 1;
               end loop Delete_Item_Loop;
            end if;
         end loop Load_Items_Loop;
      end Add_Child_Node;
      procedure Add_Child_Node
        (Data: in out UnboundedString_Container.Vector; Name: String;
         Index: Natural; Check_Item_Type: Boolean := True) is
         Value: Unbounded_String;
      begin
         Child_Nodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Elem => Item(List => Nodes_List, Index => Index), Name => Name);
         Load_Items_Loop :
         for J in 0 .. Length(List => Child_Nodes) - 1 loop
            Child_Node := Item(List => Child_Nodes, Index => J);
            Value :=
              To_Unbounded_String
                (Source => Get_Attribute(Elem => Child_Node, Name => "name"));
            Sub_Action := Get_Action(Current_Node => Child_Node);
            if Check_Item_Type then
               Item_Index :=
                 Find_Proto_Item
                   (Item_Type =>
                      To_Bounded_String(Source => To_String(Source => Value)));
               if Item_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no items with type '" & To_String(Source => Value) &
                    "'.";
               end if;
            end if;
            if Sub_Action /= REMOVE then
               Data.Append(New_Item => Value);
            else
               Delete_Index := Data.First_Index;
               Delete_Item_Loop :
               while Delete_Index <= Data.Last_Index loop
                  if Data(Delete_Index) = Value then
                     Data.Delete(Index => Delete_Index);
                     exit Delete_Item_Loop;
                  end if;
                  Delete_Index := Delete_Index + 1;
               end loop Delete_Item_Loop;
            end if;
         end loop Load_Items_Loop;
      end Add_Child_Node;
      type Faction_Nim_Data is record
         Name: chars_ptr;
         Member_Name: chars_ptr;
         Plural_Member_Name: chars_ptr;
         Spawn_Chance: Natural := 0;
         Population: Attributes_Array;
         Names_Type: Integer;
         Description: chars_ptr;
         Healing_Tools: chars_ptr;
         Healing_Skill: SkillsData_Container.Extended_Index;
         Base_Icon: Integer;
         Weapon_Skill: SkillsData_Container.Extended_Index;
      end record;
      type Faction_Nim_Relation is array(0 .. 3) of Integer;
      type Career_Nim_Record is record
         Ship_Index: Positive;
         Player_Index: chars_ptr;
         Description: chars_ptr;
         Name: chars_ptr;
      end record;
      Temp_Nim_Record: Faction_Nim_Data;
      Index: Positive := 1;
      Index2: Natural := 0;
      Faction_Data: Unbounded_String;
      Faction_Relation: Faction_Nim_Relation;
      Faction_Career: Career_Nim_Record;
      procedure Load_Ada_Factions(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaFactions";
      function Get_Ada_Faction
        (Faction_Index: Integer; Ada_Faction: out Faction_Nim_Data)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFaction";
      function Get_Ada_Faction_Data
        (Faction_Index: chars_ptr; Item_Index: Integer; Data_Type: chars_ptr)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionData";
      function Get_Ada_Faction_Relation
        (Faction_Index: chars_ptr; Relation_Index: Integer;
         Relation: out Faction_Nim_Relation) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionRelation";
      function Get_Ada_Faction_Career
        (Faction_Index: chars_ptr; Career_Index: Integer;
         Career: out Career_Nim_Record) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionCareer";
   begin
      Load_Ada_Factions(Name => New_String(Str => File_Name));
      Load_Factions_Data_Loop :
      loop
         Faction_Index :=
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item =>
                     Get_Ada_Faction
                       (Faction_Index => Index,
                        Ada_Faction => Temp_Nim_Record)));
         exit Load_Factions_Data_Loop when Length(Source => Faction_Index) = 0;
         Temp_Record.Name :=
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value(Item => Temp_Nim_Record.Name));
         Temp_Record.Member_Name :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Member_Name));
         Temp_Record.Plural_Member_Name :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Plural_Member_Name));
         Temp_Record.Spawn_Chance := Temp_Nim_Record.Spawn_Chance;
         Temp_Record.Population := Temp_Nim_Record.Population;
         Temp_Record.Names_Type := Names_Types'Val(Temp_Nim_Record.Names_Type);
         Temp_Record.Description :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Description));
         Temp_Record.Healing_Tools :=
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Healing_Tools));
         Temp_Record.Healing_Skill := Temp_Nim_Record.Healing_Skill;
         Temp_Record.Base_Icon :=
           Wide_Character'Val(Temp_Nim_Record.Base_Icon);
         Temp_Record.Weapon_Skill := Temp_Nim_Record.Weapon_Skill;
         Index2 := 0;
         Load_Faction_Food_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Data
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "foodType")))));
            exit Load_Faction_Food_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Food_Types.Append
              (New_Item =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)));
            Index2 := Index2 + 1;
         end loop Load_Faction_Food_Loop;
         Index2 := 0;
         Load_Faction_Drinks_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Data
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "drinkType")))));
            exit Load_Faction_Drinks_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Drinks_Types.Append
              (New_Item =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)));
            Index2 := Index2 + 1;
         end loop Load_Faction_Drinks_Loop;
         Index2 := 0;
         Load_Faction_Flags_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Data
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "flag")))));
            exit Load_Faction_Flags_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Flags.Append(New_Item => Faction_Data);
            Index2 := Index2 + 1;
         end loop Load_Faction_Flags_Loop;
         Index2 := 1;
         Load_Faction_Relation_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Relation
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Relation_Index => Index2,
                            Relation => Faction_Relation))));
            exit Load_Faction_Relation_Loop when Length
                (Source => Faction_Data) =
              0;
            Temp_Record.Relations.Include
              (Key =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)),
               New_Item =>
                 (Reputation =>
                    (Min => Faction_Relation(0), Max => Faction_Relation(1)),
                  Friendly =>
                    (if Faction_Relation(2) = 0 then False else True)));
            Index2 := Index2 + 1;
         end loop Load_Faction_Relation_Loop;
         Index2 := 1;
         Load_Faction_Career_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Career
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Career_Index => Index2,
                            Career => Faction_Career))));
            exit Load_Faction_Career_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Careers.Include
              (Key => Faction_Data,
               New_Item =>
                 (Ship_Index => Faction_Career.Ship_Index,
                  Player_Index =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value
                           (Item => Faction_Career.Player_Index)),
                  Description =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value
                           (Item => Faction_Career.Description)),
                  Name =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value
                           (Item => Faction_Career.Name))));
            Index2 := Index2 + 1;
         end loop Load_Faction_Career_Loop;
         Factions_List.Include(Key => Faction_Index, New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Factions_Data_Loop;
      Factions_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Factions_Data, Tag_Name => "faction");
      Load_Factions_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Null_Bounded_String, Member_Name => Null_Unbounded_String,
            Plural_Member_Name => Null_Unbounded_String, Spawn_Chance => 0,
            Population => (1 => 0, 2 => 0), Names_Type => STANDARD,
            Relations => Tmp_Relations, Description => Null_Unbounded_String,
            Food_Types => Tmp_Food, Drinks_Types => Tmp_Food,
            Healing_Tools => Null_Bounded_String, Healing_Skill => 1,
            Flags => Tmp_Flags, Careers => Tmp_Careers,
            Base_Icon => Wide_Character'Val(16#f5d2#),
            Bases_Types => Tmp_Bases_Types, Weapon_Skill => 17);
         Faction_Node := Item(List => Nodes_List, Index => I);
         Faction_Index :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Faction_Node, Name => "index"));
         Action := Get_Action(Current_Node => Faction_Node);
         if Action in UPDATE | REMOVE then
            if not Factions_Container.Contains
                (Container => Factions_List, Key => Faction_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " faction '" & To_String(Source => Faction_Index) &
                 "', there no faction with that index.";
            end if;
         elsif Factions_Container.Contains
             (Container => Factions_List, Key => Faction_Index) then
            raise Data_Loading_Error
              with "Can't add faction '" & To_String(Source => Faction_Index) &
              "', there is already a faction with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Factions_List(Faction_Index);
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "name") /= "" then
               Temp_Record.Name :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Faction_Node, Name => "name"));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "membername") /=
              "" then
               Temp_Record.Member_Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Faction_Node, Name => "membername"));
            end if;
            if Get_Attribute
                (Elem => Faction_Node, Name => "pluralmembername") /=
              "" then
               Temp_Record.Plural_Member_Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Faction_Node, Name => "pluralmembername"));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "spawn") /= "" then
               Temp_Record.Spawn_Chance :=
                 Natural'Value
                   (Get_Attribute(Elem => Faction_Node, Name => "spawn"));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "population") /=
              "" then
               Temp_Record.Population(1) :=
                 Natural'Value
                   (Get_Attribute(Elem => Faction_Node, Name => "population"));
               Temp_Record.Population(2) := 0;
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "minpopulation") /=
              "" then
               Temp_Record.Population(1) :=
                 Natural'Value
                   (Get_Attribute
                      (Elem => Faction_Node, Name => "minpopulation"));
               Temp_Record.Population(2) :=
                 Natural'Value
                   (Get_Attribute
                      (Elem => Faction_Node, Name => "maxpopulation"));
               if Temp_Record.Population(2) < Temp_Record.Population(1) then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', invalid range for faction's population.";
               end if;
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "namestype") /=
              "" then
               Temp_Record.Names_Type :=
                 Names_Types'Value
                   (Get_Attribute(Elem => Faction_Node, Name => "namestype"));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "healingtools") /=
              "" then
               Value :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Faction_Node, Name => "healingtools"));
               Item_Index :=
                 Find_Proto_Item
                   (Item_Type =>
                      To_Bounded_String(Source => To_String(Source => Value)));
               if Item_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no items with type '" & To_String(Source => Value) &
                    "'.";
               end if;
               Temp_Record.Healing_Tools :=
                 To_Bounded_String(Source => To_String(Source => Value));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "healingskill") /=
              "" then
               Value :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Faction_Node, Name => "healingskill"));
               Skill_Index :=
                 Find_Skill_Index(Skill_Name => To_String(Source => Value));
               if Skill_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no skill named '" & To_String(Source => Value) & "'.";
               end if;
               Temp_Record.Healing_Skill := Skill_Index;
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "baseicon") /=
              "" then
               Temp_Record.Base_Icon :=
                 Wide_Character'Val
                   (Natural'Value
                      ("16#" &
                       Get_Attribute
                         (Elem => Faction_Node, Name => "baseicon") &
                       "#"));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "weaponskill") /=
              "" then
               Value :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Faction_Node, Name => "weaponskill"));
               Skill_Index :=
                 Find_Skill_Index(Skill_Name => To_String(Source => Value));
               if Skill_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no skill named '" & To_String(Source => Value) & "'.";
               end if;
               Temp_Record.Weapon_Skill := Skill_Index;
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Faction_Node, Name => "relation");
            Load_Relations_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Relation_Index :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "faction"));
               if Get_Attribute(Elem => Child_Node, Name => "reputation") /=
                 "" then
                  Tmp_Relation.Reputation :=
                    (Min =>
                       Integer'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "reputation")),
                     Max => 0);
               else
                  Tmp_Relation.Reputation :=
                    (Min =>
                       Integer'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "minreputation")),
                     Max =>
                       Integer'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "maxreputation")));
                  if Tmp_Relation.Reputation.Max <
                    Tmp_Relation.Reputation.Min then
                     raise Data_Loading_Error
                       with "Can't " &
                       To_Lower(Item => Data_Action'Image(Action)) &
                       " faction '" & To_String(Source => Faction_Index) &
                       "', invalid range for faction's reputation with '" &
                       To_String(Source => Relation_Index) & "'.";
                  end if;
               end if;
               if Get_Attribute(Elem => Child_Node, Name => "friendly") =
                 "Y" then
                  Tmp_Relation.Friendly := True;
               else
                  Tmp_Relation.Friendly := False;
               end if;
               if Action /= UPDATE then
                  Relations_Container.Include
                    (Container => Temp_Record.Relations, Key => Relation_Index,
                     New_Item => Tmp_Relation);
               else
                  Temp_Record.Relations(Relation_Index) := Tmp_Relation;
               end if;
            end loop Load_Relations_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Faction_Node, Name => "description");
            if Length(List => Child_Nodes) > 0 then
               Temp_Record.Description :=
                 To_Unbounded_String
                   (Source =>
                      Node_Value
                        (N =>
                           First_Child
                             (N => Item(List => Child_Nodes, Index => 0))));
            end if;
            Add_Child_Node
              (Data => Temp_Record.Food_Types, Name => "foodtype", Index => I);
            Add_Child_Node
              (Data => Temp_Record.Drinks_Types, Name => "drinktype",
               Index => I);
            Add_Child_Node
              (Data => Temp_Record.Flags, Name => "flag", Index => I,
               Check_Item_Type => False);
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Faction_Node, Name => "career");
            Load_Careers_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Career_Index :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "index"));
               Sub_Action := Get_Action(Current_Node => Child_Node);
               if Get_Attribute(Elem => Child_Node, Name => "shipindex") /=
                 "" then
                  Tmp_Career.Ship_Index :=
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "shipindex"));
               end if;
               if Get_Attribute(Elem => Child_Node, Name => "playerindex") /=
                 "" then
                  Tmp_Career.Player_Index :=
                    To_Unbounded_String
                      (Source =>
                         Get_Attribute
                           (Elem => Child_Node, Name => "playerindex"));
               end if;
               if Has_Child_Nodes(N => Child_Node) then
                  Tmp_Career.Description :=
                    To_Unbounded_String
                      (Source =>
                         Node_Value(N => First_Child(N => Child_Node)));
               end if;
               if Get_Attribute(Elem => Child_Node, Name => "name") /= "" then
                  Tmp_Career.Name :=
                    To_Unbounded_String
                      (Source =>
                         Get_Attribute(Elem => Child_Node, Name => "name"));
               else
                  Tmp_Career.Name := Careers_List(Career_Index).Name;
               end if;
               if Careers.Careers_Container.Contains
                   (Container => Careers_List, Key => Career_Index) then
                  case Sub_Action is
                     when REMOVE =>
                        Factions.Careers_Container.Exclude
                          (Container => Temp_Record.Careers,
                           Key => Career_Index);
                     when UPDATE =>
                        Temp_Record.Careers(Career_Index) := Tmp_Career;
                     when ADD =>
                        Factions.Careers_Container.Include
                          (Container => Temp_Record.Careers,
                           Key => Career_Index, New_Item => Tmp_Career);
                  end case;
               end if;
            end loop Load_Careers_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Faction_Node, Name => "basetype");
            Load_Bases_Types_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Career_Index :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "index"));
               Sub_Action := Get_Action(Current_Node => Child_Node);
               if Get_Attribute(Elem => Child_Node, Name => "chance") /=
                 "" then
                  Tmp_Base_Type_Chance :=
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "chance"));
               else
                  Tmp_Base_Type_Chance :=
                    Factions_List(Faction_Index).Bases_Types
                      (To_Bounded_String
                         (Source => To_String(Source => Career_Index)));
               end if;
               if BasesTypes_Container.Contains
                   (Container => Bases_Types_List,
                    Key =>
                      To_Bounded_String
                        (Source => To_String(Source => Career_Index))) then
                  case Sub_Action is
                     when REMOVE =>
                        Factions.BaseType_Container.Exclude
                          (Container => Temp_Record.Bases_Types,
                           Key =>
                             To_Bounded_String
                               (Source => To_String(Source => Career_Index)));
                     when UPDATE =>
                        Temp_Record.Bases_Types
                          (To_Bounded_String
                             (Source => To_String(Source => Career_Index))) :=
                          Tmp_Base_Type_Chance;
                     when ADD =>
                        Factions.BaseType_Container.Include
                          (Container => Temp_Record.Bases_Types,
                           Key =>
                             To_Bounded_String
                               (Source => To_String(Source => Career_Index)),
                           New_Item => Tmp_Base_Type_Chance);
                  end case;
               end if;
            end loop Load_Bases_Types_Loop;
            if Action /= UPDATE then
               if Temp_Record.Bases_Types.Length = 0 then
                  Add_Bases_Types_Loop :
                  for I in Bases_Types_List.Iterate loop
                     Factions.BaseType_Container.Include
                       (Container => Temp_Record.Bases_Types,
                        Key => BasesTypes_Container.Key(Position => I),
                        New_Item => 20);
                  end loop Add_Bases_Types_Loop;
               end if;
               Factions_Container.Include
                 (Container => Factions_List, Key => Faction_Index,
                  New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Faction added: " & To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            else
               Factions_List(Faction_Index) := Temp_Record;
               Log_Message
                 (Message =>
                    "Faction updated: " &
                    To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            end if;
         else
            Factions_Container.Exclude
              (Container => Factions_List, Key => Faction_Index);
            Log_Message
              (Message =>
                 "Faction removed: " & To_String(Source => Faction_Index),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Factions_Loop;
   end Load_Factions;

   function Get_Reputation
     (Source_Faction, Target_Faction: Tiny_String.Bounded_String)
      return Integer is
   begin
      return
        (if
           Factions_List(Source_Faction).Relations(Target_Faction).Reputation
             .Max =
           0
         then
           Factions_List(Source_Faction).Relations(Target_Faction).Reputation
             .Min
         else Get_Random
             (Min =>
                Factions_List(Source_Faction).Relations(Target_Faction)
                  .Reputation
                  .Min,
              Max =>
                Factions_List(Source_Faction).Relations(Target_Faction)
                  .Reputation
                  .Max));
   end Get_Reputation;

   function Is_Friendly
     (Source_Faction, Target_Faction: Tiny_String.Bounded_String)
      return Boolean is
   begin
      return Factions_List(Source_Faction).Relations(Target_Faction).Friendly;
   end Is_Friendly;

   function Get_Random_Faction return Tiny_String.Bounded_String is
      Faction_Index,
      Current_Index: Positive range 1 .. Positive(Factions_List.Length);
   begin
      Faction_Index :=
        Get_Random(Min => 1, Max => Positive(Factions_List.Length));
      Current_Index := 1;
      Get_Random_Faction_Loop :
      for J in Factions_List.Iterate loop
         if Current_Index = Faction_Index then
            return Factions_Container.Key(Position => J);
         end if;
         Current_Index := Current_Index + 1;
      end loop Get_Random_Faction_Loop;
      return Tiny_String.Null_Bounded_String;
   end Get_Random_Faction;

end Factions;
