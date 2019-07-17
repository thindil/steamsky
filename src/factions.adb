--    Copyright 2018-2019 Bartek thindil Jasicki
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

package body Factions is

-- ****if* Factions/LoadFactions
-- SOURCE
   procedure LoadFactions(Reader: Tree_Reader) is
-- ****
      TempRecord: FactionRecord;
      NodesList, ChildNodes: Node_List;
      FactionsData: Document;
      TmpRelations: Relations_Container.Map;
      TmpRelation: RelationsRecord;
      TmpFood: UnboundedString_Container.Vector;
      Value, CareerIndex, RelationIndex, FactionIndex,
      ItemIndex: Unbounded_String;
      SkillIndex: Natural;
      TmpCareers: Factions.Careers_Container.Map;
      TmpCareer: Factions.CareerRecord;
      FactionNode, ChildNode: Node;
      DeleteIndex: Positive;
      Action, SubAction: DataAction;
      procedure AddChildNode
        (Data: in out UnboundedString_Container.Vector; Name: String;
         Index: Natural; CheckItemType: Boolean := True) is
         Value: Unbounded_String;
      begin
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, Index), Name);
         for J in 0 .. Length(ChildNodes) - 1 loop
            ChildNode := Item(ChildNodes, J);
            Value := To_Unbounded_String(Get_Attribute(ChildNode, "name"));
            if Get_Attribute(ChildNode, "action")'Length > 0 then
               SubAction :=
                 DataAction'Value(Get_Attribute(ChildNode, "action"));
            else
               SubAction := ADD;
            end if;
            if CheckItemType then
               ItemIndex := FindProtoItem(ItemType => Value);
               if ItemIndex = Null_Unbounded_String then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " faction '" & To_String(FactionIndex) &
                    "', no items with type '" & To_String(Value) & "'.";
               end if;
            end if;
            if SubAction /= REMOVE then
               Data.Append(New_Item => Value);
            else
               DeleteIndex := Data.First_Index;
               while DeleteIndex <= Data.Last_Index loop
                  if Data(DeleteIndex) = Value then
                     Data.Delete(Index => DeleteIndex);
                     exit;
                  end if;
                  DeleteIndex := DeleteIndex + 1;
               end loop;
            end if;
         end loop;
      end AddChildNode;
   begin
      TempRecord :=
        (Name => Null_Unbounded_String, MemberName => Null_Unbounded_String,
         PluralMemberName => Null_Unbounded_String, SpawnChance => 0,
         Population => (0, 0), NamesType => STANDARD,
         Relations => TmpRelations, Description => Null_Unbounded_String,
         FoodTypes => TmpFood, DrinksTypes => TmpFood,
         HealingTools => Null_Unbounded_String, HealingSkill => 1,
         Flags => TmpFood, Careers => TmpCareers,
         BaseIcon => Wide_Character'Val(16#f5d2#));
      FactionsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(FactionsData, "faction");
      for I in 0 .. Length(NodesList) - 1 loop
         FactionNode := Item(NodesList, I);
         FactionIndex :=
           To_Unbounded_String(Get_Attribute(FactionNode, "index"));
         if Get_Attribute(FactionNode, "action")'Length > 0 then
            Action := DataAction'Value(Get_Attribute(FactionNode, "action"));
         else
            Action := ADD;
         end if;
         if (Action = UPDATE or Action = REMOVE) then
            if not Factions_Container.Contains
                (Factions_List, FactionIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " faction '" & To_String(FactionIndex) &
                 "', there no faction with that index.";
            end if;
         elsif Factions_Container.Contains(Factions_List, FactionIndex) then
            raise Data_Loading_Error
              with "Can't add faction '" & To_String(FactionIndex) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Factions_List(FactionIndex);
            end if;
            if Get_Attribute(FactionNode, "name") /= "" then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(FactionNode, "name"));
            end if;
            if Get_Attribute(FactionNode, "membername") /= "" then
               TempRecord.MemberName :=
                 To_Unbounded_String(Get_Attribute(FactionNode, "membername"));
            end if;
            if Get_Attribute(FactionNode, "pluralmembername") /= "" then
               TempRecord.PluralMemberName :=
                 To_Unbounded_String
                   (Get_Attribute(FactionNode, "pluralmembername"));
            end if;
            if Get_Attribute(FactionNode, "spawn") /= "" then
               TempRecord.SpawnChance :=
                 Natural'Value(Get_Attribute(FactionNode, "spawn"));
            end if;
            if Get_Attribute(FactionNode, "population") /= "" then
               TempRecord.Population(1) :=
                 Natural'Value(Get_Attribute(FactionNode, "population"));
               TempRecord.Population(2) := 0;
            end if;
            if Get_Attribute(FactionNode, "minpopulation") /= "" then
               TempRecord.Population(1) :=
                 Natural'Value(Get_Attribute(FactionNode, "minpopulation"));
               TempRecord.Population(2) :=
                 Natural'Value(Get_Attribute(FactionNode, "maxpopulation"));
               if TempRecord.Population(2) < TempRecord.Population(1) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " faction '" & To_String(FactionIndex) &
                    "', invalid range for faction's population.";
               end if;
            end if;
            if Get_Attribute(FactionNode, "namestype") /= "" then
               TempRecord.NamesType :=
                 NamesTypes'Value(Get_Attribute(FactionNode, "namestype"));
            end if;
            if Get_Attribute(FactionNode, "healingtools") /= "" then
               Value :=
                 To_Unbounded_String
                   (Get_Attribute(FactionNode, "healingtools"));
               ItemIndex := FindProtoItem(ItemType => Value);
               if ItemIndex = Null_Unbounded_String then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " faction '" & To_String(FactionIndex) &
                    "', no items with type '" & To_String(Value) & "'.";
               end if;
               TempRecord.HealingTools := Value;
            end if;
            if Get_Attribute(FactionNode, "healingskill") /= "" then
               Value :=
                 To_Unbounded_String
                   (Get_Attribute(FactionNode, "healingskill"));
               SkillIndex := FindSkillIndex(Value);
               if SkillIndex = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " faction '" & To_String(FactionIndex) &
                    "', no skill named '" & To_String(Value) & "'.";
               end if;
               TempRecord.HealingSkill := SkillIndex;
            end if;
            if Get_Attribute(FactionNode, "baseicon") /= "" then
               TempRecord.BaseIcon :=
                 Wide_Character'Val
                   (Natural'Value
                      ("16#" & Get_Attribute(FactionNode, "baseicon") & "#"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (FactionNode, "relation");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               RelationIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "faction"));
               if Get_Attribute(ChildNode, "reputation") /= "" then
                  TmpRelation.Reputation :=
                    (Integer'Value(Get_Attribute(ChildNode, "reputation")), 0);
               else
                  TmpRelation.Reputation :=
                    (Integer'Value(Get_Attribute(ChildNode, "minreputation")),
                     Integer'Value(Get_Attribute(ChildNode, "maxreputation")));
                  if TmpRelation.Reputation(2) < TmpRelation.Reputation(1) then
                     raise Data_Loading_Error
                       with "Can't " & To_Lower(DataAction'Image(Action)) &
                       " faction '" & To_String(FactionIndex) &
                       "', invalid range for faction's reputation with '" &
                       To_String(RelationIndex) & "'.";
                  end if;
               end if;
               if Get_Attribute(ChildNode, "friendly") = "Y" then
                  TmpRelation.Friendly := True;
               else
                  TmpRelation.Friendly := False;
               end if;
               if Action /= UPDATE then
                  Relations_Container.Include
                    (TempRecord.Relations, RelationIndex, TmpRelation);
               else
                  TempRecord.Relations(RelationIndex) := TmpRelation;
               end if;
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (FactionNode, "description");
            if Length(ChildNodes) > 0 then
               TempRecord.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(ChildNodes, 0))));
            end if;
            AddChildNode(TempRecord.FoodTypes, "foodtype", I);
            AddChildNode(TempRecord.DrinksTypes, "drinktype", I);
            AddChildNode(TempRecord.Flags, "flag", I, False);
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (FactionNode, "career");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               CareerIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               if Get_Attribute(ChildNode, "action")'Length > 0 then
                  SubAction :=
                    DataAction'Value(Get_Attribute(ChildNode, "action"));
               else
                  SubAction := ADD;
               end if;
               if Get_Attribute(ChildNode, "shipindex") /= "" then
                  TmpCareer.ShipIndex :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "shipindex"));
               end if;
               if Get_Attribute(ChildNode, "playerindex") /= "" then
                  TmpCareer.PlayerIndex :=
                    To_Unbounded_String
                      (Get_Attribute(ChildNode, "playerindex"));
               end if;
               if Has_Child_Nodes(ChildNode) then
                  TmpCareer.Description :=
                    To_Unbounded_String(Node_Value(First_Child(ChildNode)));
               end if;
               if Get_Attribute(ChildNode, "name") /= "" then
                  TmpCareer.Name :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "name"));
               else
                  TmpCareer.Name := Careers_List(CareerIndex).Name;
               end if;
               if Careers.Careers_Container.Contains
                   (Careers_List, CareerIndex) then
                  case SubAction is
                     when REMOVE =>
                        Factions.Careers_Container.Exclude
                          (TempRecord.Careers, CareerIndex);
                     when UPDATE =>
                        TempRecord.Careers(CareerIndex) := TmpCareer;
                     when ADD =>
                        Factions.Careers_Container.Include
                          (TempRecord.Careers, CareerIndex, TmpCareer);
                  end case;
               end if;
            end loop;
            if Action /= UPDATE then
               Factions_Container.Include
                 (Factions_List, FactionIndex, TempRecord);
               LogMessage
                 ("Faction added: " & To_String(TempRecord.Name), Everything);
            else
               Factions_List(FactionIndex) := TempRecord;
               LogMessage
                 ("Faction updated: " & To_String(TempRecord.Name),
                  Everything);
            end if;
         else
            Factions_Container.Exclude(Factions_List, FactionIndex);
            LogMessage
              ("Faction removed: " & To_String(FactionIndex), Everything);
         end if;
         TempRecord :=
           (Name => Null_Unbounded_String, MemberName => Null_Unbounded_String,
            PluralMemberName => Null_Unbounded_String, SpawnChance => 0,
            Population => (0, 0), NamesType => STANDARD,
            Relations => TmpRelations, Description => Null_Unbounded_String,
            FoodTypes => TmpFood, DrinksTypes => TmpFood,
            HealingTools => Null_Unbounded_String, HealingSkill => 1,
            Flags => TmpFood, Careers => TmpCareers,
            BaseIcon => Wide_Character'Val(16#f5d2#));
      end loop;
   end LoadFactions;

-- ****if* Factions/GetReputation
-- SOURCE
   function GetReputation
     (SourceFaction, TargetFaction: Unbounded_String) return Integer is
-- ****
   begin
      if Factions_List(SourceFaction).Relations(TargetFaction).Reputation(2) =
        0 then
         return Factions_List(SourceFaction).Relations(TargetFaction)
             .Reputation
             (1);
      else
         return GetRandom
             (Factions_List(SourceFaction).Relations(TargetFaction).Reputation
                (1),
              Factions_List(SourceFaction).Relations(TargetFaction).Reputation
                (2));
      end if;
   end GetReputation;

-- ****if* Factions/IsFriendly
-- SOURCE
   function IsFriendly
     (SourceFaction, TargetFaction: Unbounded_String) return Boolean is
-- ****
   begin
      return Factions_List(SourceFaction).Relations(TargetFaction).Friendly;
   end IsFriendly;

-- ****if* Factions/GetRandomFaction
-- SOURCE
   function GetRandomFaction return Unbounded_String is
-- ****
      FactionIndex, CurrentIndex: Positive;
   begin
      FactionIndex := GetRandom(1, Positive(Factions_List.Length));
      CurrentIndex := 1;
      for J in Factions_List.Iterate loop
         if CurrentIndex = FactionIndex then
            return Factions_Container.Key(J);
         end if;
         CurrentIndex := CurrentIndex + 1;
      end loop;
      return Null_Unbounded_String;
   end GetRandomFaction;

end Factions;
