--    Copyright 2018 Bartek thindil Jasicki
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

   procedure LoadFactions(Reader: Tree_Reader) is
      TempRecord: FactionRecord;
      NodesList, ChildNodes: Node_List;
      FactionsData: Document;
      TmpRelations: Relations_Container.Vector;
      TmpRelation: RelationsRecord;
      TmpFood: UnboundedString_Container.Vector;
      Action, Value: Unbounded_String;
      DeleteIndex, ItemIndex, SkillIndex: Natural;
      TmpCareers: Careers_Container.Vector;
      TmpCareer: CareerRecord;
      CareerExists: Boolean;
      FactionNode, ChildNode: Node;
      procedure AddChildNode(Data: in out UnboundedString_Container.Vector;
         Name: String; Index: Natural; CheckItemType: Boolean := True) is
         Value: Unbounded_String;
      begin
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, Index), Name);
         for J in 0 .. Length(ChildNodes) - 1 loop
            Value :=
              To_Unbounded_String(Get_Attribute(Item(ChildNodes, J), "name"));
            if CheckItemType then
               ItemIndex := FindProtoItem(ItemType => Value);
               if ItemIndex = 0 then
                  raise Factions_Adding_Error
                    with "Can't add faction '" & To_String(TempRecord.Index) &
                    "', no items with type '" & To_String(Value) & "'.";
               end if;
            end if;
            Data.Append(New_Item => Value);
         end loop;
      end AddChildNode;
   begin
      TempRecord :=
        (Index => Null_Unbounded_String, Name => Null_Unbounded_String,
         MemberName => Null_Unbounded_String,
         PluralMemberName => Null_Unbounded_String, SpawnChance => 0,
         Population => (0, 0), NamesType => STANDARD,
         Relations => TmpRelations, Description => Null_Unbounded_String,
         FoodTypes => TmpFood, DrinksTypes => TmpFood,
         HealingTools => Null_Unbounded_String, HealingSkill => 1,
         Flags => TmpFood, Careers => TmpCareers);
      FactionsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(FactionsData, "faction");
      for I in 0 .. Length(NodesList) - 1 loop
         FactionNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(FactionNode, "index"));
         Action := To_Unbounded_String(Get_Attribute(FactionNode, "action"));
         if Action = Null_Unbounded_String or
           Action = To_Unbounded_String("add") then
            for Faction of Factions_List loop
               if Faction.Index = TempRecord.Index then
                  raise Factions_Adding_Error
                    with "Can't add faction '" & To_String(TempRecord.Index) &
                    "', there is one with that index.";
               end if;
            end loop;
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(FactionNode, "name"));
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
            end if;
            if Get_Attribute(FactionNode, "minpopulation") /= "" then
               TempRecord.Population(1) :=
                 Natural'Value(Get_Attribute(FactionNode, "minpopulation"));
               TempRecord.Population(2) :=
                 Natural'Value(Get_Attribute(FactionNode, "maxpopulation"));
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
               if ItemIndex = 0 then
                  raise Factions_Adding_Error
                    with "Can't add faction '" & To_String(TempRecord.Index) &
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
                  raise Factions_Adding_Error
                    with "Can't add faction '" & To_String(TempRecord.Index) &
                    "', no skill named '" & To_String(Value) & "'.";
               end if;
               TempRecord.HealingSkill := SkillIndex;
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (FactionNode, "relation");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               TmpRelation.TargetFaction :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "faction"));
               if Get_Attribute(ChildNode, "reputation") /= "" then
                  TmpRelation.Reputation :=
                    (Integer'Value(Get_Attribute(ChildNode, "reputation")), 0);
               else
                  TmpRelation.Reputation :=
                    (Integer'Value(Get_Attribute(ChildNode, "minreputation")),
                     Integer'Value(Get_Attribute(ChildNode, "maxreputation")));
               end if;
               if Get_Attribute(ChildNode, "friendly") = "Y" then
                  TmpRelation.Friendly := True;
               else
                  TmpRelation.Friendly := False;
               end if;
               TempRecord.Relations.Append(New_Item => TmpRelation);
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
               TmpCareer.Index :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               TmpCareer.ShipIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "shipindex"));
               TmpCareer.PlayerIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "playerindex"));
               TmpCareer.Description :=
                 To_Unbounded_String(Node_Value(First_Child(ChildNode)));
               if Get_Attribute(ChildNode, "name") /= "" then
                  TmpCareer.Name :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "name"));
               else
                  for Career of Careers_List loop
                     if Career.Index = TmpCareer.Index then
                        TmpCareer.Name := Career.Name;
                        exit;
                     end if;
                  end loop;
               end if;
               CareerExists := False;
               for Career of Careers_List loop
                  if Career.Index = TmpCareer.Index then
                     CareerExists := True;
                     exit;
                  end if;
               end loop;
               if CareerExists then
                  TempRecord.Careers.Append(New_Item => TmpCareer);
               end if;
            end loop;
            Factions_List.Append(New_Item => TempRecord);
            LogMessage
              ("Faction added: " & To_String(TempRecord.Name), Everything);
         else
            DeleteIndex := 0;
            for J in Factions_List.Iterate loop
               if Factions_List(J).Index = TempRecord.Index then
                  DeleteIndex := Factions_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            if DeleteIndex = 0 then
               raise Factions_Remove_Error
                 with "Can't delete faction '" & To_String(TempRecord.Index) &
                 "', no faction with that index.";
            end if;
            Factions_List.Delete(Index => DeleteIndex);
            LogMessage
              ("Faction removed: " & To_String(TempRecord.Index), Everything);
         end if;
         TempRecord :=
           (Index => Null_Unbounded_String, Name => Null_Unbounded_String,
            MemberName => Null_Unbounded_String,
            PluralMemberName => Null_Unbounded_String, SpawnChance => 0,
            Population => (0, 0), NamesType => Standard,
            Relations => TmpRelations, Description => Null_Unbounded_String,
            FoodTypes => TmpFood, DrinksTypes => TmpFood,
            HealingTools => Null_Unbounded_String, HealingSkill => 1,
            Flags => TmpFood, Careers => TmpCareers);
      end loop;
   end LoadFactions;

   function GetReputation
     (SourceFaction, TargetFaction: Unbounded_String) return Integer is
   begin
      for Source of Factions_List loop
         if To_Lower(To_String(Source.Index)) =
           To_Lower(To_String(SourceFaction)) then
            for Target of Source.Relations loop
               if To_Lower(To_String(Target.TargetFaction)) =
                 To_Lower(To_String(TargetFaction)) then
                  if Target.Reputation(2) = 0 then
                     return Target.Reputation(1);
                  else
                     return GetRandom
                         (Target.Reputation(1), Target.Reputation(2));
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      return 0;
   end GetReputation;

   function IsFriendly
     (SourceFaction, TargetFaction: Unbounded_String) return Boolean is
   begin
      for Source of Factions_List loop
         if To_Lower(To_String(Source.Index)) =
           To_Lower(To_String(SourceFaction)) then
            for Target of Source.Relations loop
               if To_Lower(To_String(Target.TargetFaction)) =
                 To_Lower(To_String(TargetFaction)) then
                  return Target.Friendly;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end IsFriendly;

end Factions;
