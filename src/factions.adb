--    Copyright 2018-2021 Bartek thindil Jasicki
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

   procedure Load_Factions(Reader: Tree_Reader) is
      Temp_Record: Faction_Record;
      Nodes_List, Child_Nodes: Node_List;
      Factions_Data: Document;
      Tmp_Relations: Relations_Container.Map;
      Tmp_Relation: Relations_Record;
      Tmp_Food: UnboundedString_Container.Vector;
      Value, Career_Index, Relation_Index, Faction_Index,
      Item_Index: Unbounded_String;
      Skill_Index: Natural;
      Tmp_Careers: Factions.Careers_Container.Map;
      Tmp_Career: Factions.Career_Record;
      Faction_Node, Child_Node: Node;
      Delete_Index: Positive;
      Action, Sub_Action: Data_Action;
      Tmp_Base_Type_Chance: Positive;
      Tmp_Bases_Types: BaseType_Container.Map;
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
               Item_Index := FindProtoItem(ItemType => Value);
               if Item_Index = Null_Unbounded_String then
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
   begin
      Factions_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Factions_Data, Tag_Name => "faction");
      Load_Factions_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Null_Unbounded_String,
            Member_Name => Null_Unbounded_String,
            Plural_Member_Name => Null_Unbounded_String, Spawn_Chance => 0,
            Population => (1 => 0, 2 => 0), Names_Type => STANDARD,
            Relations => Tmp_Relations, Description => Null_Unbounded_String,
            Food_Types => Tmp_Food, Drinks_Types => Tmp_Food,
            Healing_Tools => Null_Unbounded_String, Healing_Skill => 1,
            Flags => Tmp_Food, Careers => Tmp_Careers,
            Base_Icon => Wide_Character'Val(16#f5d2#),
            Bases_Types => Tmp_Bases_Types, Weapon_Skill => 17);
         Faction_Node := Item(List => Nodes_List, Index => I);
         Faction_Index :=
           To_Unbounded_String
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
                 To_Unbounded_String
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
               Item_Index := FindProtoItem(ItemType => Value);
               if Item_Index = Null_Unbounded_String then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no items with type '" & To_String(Source => Value) & "'.";
               end if;
               Temp_Record.Healing_Tools := Value;
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "healingskill") /= "" then
               Value :=
                 To_Unbounded_String
                   (Source => Get_Attribute(Elem => Faction_Node, Name => "healingskill"));
               Skill_Index := Find_Skill_Index(Skill_Name => To_String(Source => Value));
               if Skill_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                    " faction '" & To_String(Source => Faction_Index) &
                    "', no skill named '" & To_String(Source => Value) & "'.";
               end if;
               Temp_Record.Healing_Skill := Skill_Index;
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "baseicon") /= "" then
               Temp_Record.Base_Icon :=
                 Wide_Character'Val
                   (Natural'Value
                      ("16#" & Get_Attribute(Elem => Faction_Node, Name => "baseicon") & "#"));
            end if;
            if Get_Attribute(Elem => Faction_Node, Name => "weaponskill") /= "" then
               Value :=
                 To_Unbounded_String
                   (Source => Get_Attribute(Elem => Faction_Node, Name => "weaponskill"));
               Skill_Index := Find_Skill_Index(To_String(Value));
               if Skill_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " faction '" & To_String(Faction_Index) &
                    "', no skill named '" & To_String(Value) & "'.";
               end if;
               Temp_Record.Weapon_Skill := Skill_Index;
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Faction_Node, "relation");
            Load_Relations_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Child_Node := Item(Child_Nodes, J);
               Relation_Index :=
                 To_Unbounded_String(Get_Attribute(Child_Node, "faction"));
               if Get_Attribute(Child_Node, "reputation") /= "" then
                  Tmp_Relation.Reputation :=
                    (Integer'Value(Get_Attribute(Child_Node, "reputation")),
                     0);
               else
                  Tmp_Relation.Reputation :=
                    (Integer'Value(Get_Attribute(Child_Node, "minreputation")),
                     Integer'Value
                       (Get_Attribute(Child_Node, "maxreputation")));
                  if Tmp_Relation.Reputation(2) <
                    Tmp_Relation.Reputation(1) then
                     raise Data_Loading_Error
                       with "Can't " & To_Lower(Data_Action'Image(Action)) &
                       " faction '" & To_String(Faction_Index) &
                       "', invalid range for faction's reputation with '" &
                       To_String(Relation_Index) & "'.";
                  end if;
               end if;
               if Get_Attribute(Child_Node, "friendly") = "Y" then
                  Tmp_Relation.Friendly := True;
               else
                  Tmp_Relation.Friendly := False;
               end if;
               if Action /= UPDATE then
                  Relations_Container.Include
                    (Temp_Record.Relations, Relation_Index, Tmp_Relation);
               else
                  Temp_Record.Relations(Relation_Index) := Tmp_Relation;
               end if;
            end loop Load_Relations_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Faction_Node, "description");
            if Length(Child_Nodes) > 0 then
               Temp_Record.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(Child_Nodes, 0))));
            end if;
            Add_Child_Node(Temp_Record.Food_Types, "foodtype", I);
            Add_Child_Node(Temp_Record.Drinks_Types, "drinktype", I);
            Add_Child_Node(Temp_Record.Flags, "flag", I, False);
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Faction_Node, "career");
            Load_Careers_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Child_Node := Item(Child_Nodes, J);
               Career_Index :=
                 To_Unbounded_String(Get_Attribute(Child_Node, "index"));
               Sub_Action := Get_Action(Child_Node);
               if Get_Attribute(Child_Node, "shipindex") /= "" then
                  Tmp_Career.Ship_Index :=
                    To_Unbounded_String
                      (Get_Attribute(Child_Node, "shipindex"));
               end if;
               if Get_Attribute(Child_Node, "playerindex") /= "" then
                  Tmp_Career.Player_Index :=
                    To_Unbounded_String
                      (Get_Attribute(Child_Node, "playerindex"));
               end if;
               if Has_Child_Nodes(Child_Node) then
                  Tmp_Career.Description :=
                    To_Unbounded_String(Node_Value(First_Child(Child_Node)));
               end if;
               if Get_Attribute(Child_Node, "name") /= "" then
                  Tmp_Career.Name :=
                    To_Unbounded_String(Get_Attribute(Child_Node, "name"));
               else
                  Tmp_Career.Name := Careers_List(Career_Index).Name;
               end if;
               if Careers.Careers_Container.Contains
                   (Careers_List, Career_Index) then
                  case Sub_Action is
                     when REMOVE =>
                        Factions.Careers_Container.Exclude
                          (Temp_Record.Careers, Career_Index);
                     when UPDATE =>
                        Temp_Record.Careers(Career_Index) := Tmp_Career;
                     when ADD =>
                        Factions.Careers_Container.Include
                          (Temp_Record.Careers, Career_Index, Tmp_Career);
                  end case;
               end if;
            end loop Load_Careers_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Faction_Node, "basetype");
            Load_Bases_Types_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Child_Node := Item(Child_Nodes, J);
               Career_Index :=
                 To_Unbounded_String(Get_Attribute(Child_Node, "index"));
               Sub_Action := Get_Action(Child_Node);
               if Get_Attribute(Child_Node, "chance") /= "" then
                  Tmp_Base_Type_Chance :=
                    Positive'Value(Get_Attribute(Child_Node, "chance"));
               else
                  Tmp_Base_Type_Chance :=
                    Factions_List(Faction_Index).Bases_Types(Career_Index);
               end if;
               if BasesTypes_Container.Contains
                   (Bases_Types_List, Career_Index) then
                  case Sub_Action is
                     when REMOVE =>
                        Factions.BaseType_Container.Exclude
                          (Temp_Record.Bases_Types, Career_Index);
                     when UPDATE =>
                        Temp_Record.Bases_Types(Career_Index) :=
                          Tmp_Base_Type_Chance;
                     when ADD =>
                        Factions.BaseType_Container.Include
                          (Temp_Record.Bases_Types, Career_Index,
                           Tmp_Base_Type_Chance);
                  end case;
               end if;
            end loop Load_Bases_Types_Loop;
            if Action /= UPDATE then
               if Temp_Record.Bases_Types.Length = 0 then
                  for I in Bases_Types_List.Iterate loop
                     Factions.BaseType_Container.Include
                       (Temp_Record.Bases_Types, BasesTypes_Container.Key(I),
                        20);
                  end loop;
               end if;
               Factions_Container.Include
                 (Factions_List, Faction_Index, Temp_Record);
               Log_Message
                 ("Faction added: " & To_String(Temp_Record.Name), EVERYTHING);
            else
               Factions_List(Faction_Index) := Temp_Record;
               Log_Message
                 ("Faction updated: " & To_String(Temp_Record.Name),
                  EVERYTHING);
            end if;
         else
            Factions_Container.Exclude(Factions_List, Faction_Index);
            Log_Message
              ("Faction removed: " & To_String(Faction_Index), EVERYTHING);
         end if;
      end loop Load_Factions_Loop;
   end Load_Factions;

   function Get_Reputation
     (Source_Faction, Target_Faction: Unbounded_String) return Integer is
   begin
      return
        (if
           Factions_List(Source_Faction).Relations(Target_Faction).Reputation
             (2) =
           0
         then
           Factions_List(Source_Faction).Relations(Target_Faction).Reputation
             (1)
         else Get_Random
             (Factions_List(Source_Faction).Relations(Target_Faction)
                .Reputation
                (1),
              Factions_List(Source_Faction).Relations(Target_Faction)
                .Reputation
                (2)));
   end Get_Reputation;

   function Is_Friendly
     (Source_Faction, Target_Faction: Unbounded_String) return Boolean is
   begin
      return Factions_List(Source_Faction).Relations(Target_Faction).Friendly;
   end Is_Friendly;

   function Get_Random_Faction return Unbounded_String is
      FactionIndex,
      CurrentIndex: Positive range 1 .. Positive(Factions_List.Length);
   begin
      FactionIndex := Get_Random(1, Positive(Factions_List.Length));
      CurrentIndex := 1;
      Get_Random_Faction_Loop :
      for J in Factions_List.Iterate loop
         if CurrentIndex = FactionIndex then
            return Factions_Container.Key(J);
         end if;
         CurrentIndex := CurrentIndex + 1;
      end loop Get_Random_Faction_Loop;
      return Null_Unbounded_String;
   end Get_Random_Faction;

end Factions;
