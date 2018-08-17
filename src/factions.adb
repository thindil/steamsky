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

with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Log; use Log;
with Utils; use Utils;

package body Factions is

   procedure LoadFactions is
      TempRecord: FactionRecord;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      FactionsFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes: Node_List;
      FactionsData: Document;
      TmpRelations: Relations_Container.Vector;
      TmpRelation: RelationsRecord;
      TmpFood: UnboundedString_Container.Vector;
   begin
      if Factions_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "factions" & Dir_Separator) then
         raise Factions_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "factions" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Factions_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Index => Null_Unbounded_String,
            Name => Null_Unbounded_String,
            MemberName => Null_Unbounded_String,
            PluralMemberName => Null_Unbounded_String,
            SpawnChance => (0, 0),
            Population => (0, 0),
            NamesType => To_Unbounded_String("standard"),
            Relations => TmpRelations,
            PlayerIndex => Null_Unbounded_String,
            PlayerShipIndex => Null_Unbounded_String,
            Description => Null_Unbounded_String,
            FoodTypes => TmpFood,
            DrinksTypes => TmpFood,
            HealingTools => Null_Unbounded_String,
            HealingSkill => 1);
         LogMessage
           ("Loading factions file: " & Full_Name(FoundFile),
            Everything);
         Open(Full_Name(FoundFile), FactionsFile);
         Parse(Reader, FactionsFile);
         Close(FactionsFile);
         FactionsData := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name
             (FactionsData,
              "faction");
         for I in 0 .. Length(NodesList) - 1 loop
            TempRecord.Index :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name"));
            if Get_Attribute(Item(NodesList, I), "membername") /= "" then
               TempRecord.MemberName :=
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "membername"));
            end if;
            if Get_Attribute(Item(NodesList, I), "pluralmembername") /= "" then
               TempRecord.PluralMemberName :=
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "pluralmembername"));
            end if;
            if Get_Attribute(Item(NodesList, I), "spawn") /= "" then
               TempRecord.SpawnChance(1) :=
                 Natural'Value(Get_Attribute(Item(NodesList, I), "spawn"));
            end if;
            if Get_Attribute(Item(NodesList, I), "minspawn") /= "" then
               TempRecord.SpawnChance(1) :=
                 Natural'Value(Get_Attribute(Item(NodesList, I), "minspawn"));
               TempRecord.SpawnChance(2) :=
                 Natural'Value(Get_Attribute(Item(NodesList, I), "maxspawn"));
            end if;
            if Get_Attribute(Item(NodesList, I), "population") /= "" then
               TempRecord.Population(1) :=
                 Natural'Value
                   (Get_Attribute(Item(NodesList, I), "population"));
            end if;
            if Get_Attribute(Item(NodesList, I), "minpopulation") /= "" then
               TempRecord.Population(1) :=
                 Natural'Value
                   (Get_Attribute(Item(NodesList, I), "minpopulation"));
               TempRecord.Population(2) :=
                 Natural'Value
                   (Get_Attribute(Item(NodesList, I), "maxpopulation"));
            end if;
            if Get_Attribute(Item(NodesList, I), "namestype") /= "" then
               TempRecord.NamesType :=
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "namestype"));
            end if;
            if Get_Attribute(Item(NodesList, I), "playerindex") /= "" then
               TempRecord.PlayerIndex :=
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "playerindex"));
            end if;
            if Get_Attribute(Item(NodesList, I), "playershipindex") /= "" then
               TempRecord.PlayerShipIndex :=
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "playershipindex"));
            end if;
            if Get_Attribute(Item(NodesList, I), "healingtools") /= "" then
               TempRecord.HealingTools :=
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "healingtools"));
            end if;
            if Get_Attribute(Item(NodesList, I), "healingskill") /= "" then
               TempRecord.HealingSkill :=
                 FindSkillIndex
                   (To_Unbounded_String
                      (Get_Attribute(Item(NodesList, I), "healingskill")));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "relation");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TmpRelation.TargetFaction :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "faction"));
               if Get_Attribute(Item(ChildNodes, J), "reputation") /= "" then
                  TmpRelation.Reputation :=
                    (Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "reputation")),
                     0);
               else
                  TmpRelation.Reputation :=
                    (Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "minreputation")),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "maxreputation")));
               end if;
               if Get_Attribute(Item(ChildNodes, J), "friendly") = "Y" then
                  TmpRelation.Friendly := True;
               else
                  TmpRelation.Friendly := False;
               end if;
               TempRecord.Relations.Append(New_Item => TmpRelation);
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "description");
            if Length(ChildNodes) > 0 then
               TempRecord.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(ChildNodes, 0))));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "foodtype");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.FoodTypes.Append
               (New_Item =>
                  To_Unbounded_String
                    (Get_Attribute(Item(ChildNodes, J), "name")));
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "drinktype");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.DrinksTypes.Append
               (New_Item =>
                  To_Unbounded_String
                    (Get_Attribute(Item(ChildNodes, J), "name")));
            end loop;
            Factions_List.Append(New_Item => TempRecord);
            LogMessage
              ("Faction added: " & To_String(TempRecord.Name),
               Everything);
            TempRecord :=
              (Index => Null_Unbounded_String,
               Name => Null_Unbounded_String,
               MemberName => Null_Unbounded_String,
               PluralMemberName => Null_Unbounded_String,
               SpawnChance => (0, 0),
               Population => (0, 0),
               NamesType => To_Unbounded_String("standard"),
               Relations => TmpRelations,
               PlayerIndex => Null_Unbounded_String,
               PlayerShipIndex => Null_Unbounded_String,
               Description => Null_Unbounded_String,
               FoodTypes => TmpFood,
               DrinksTypes => TmpFood,
               HealingTools => Null_Unbounded_String,
               HealingSkill => 1);
         end loop;
         Free(Reader);
      end loop;
      End_Search(Files);
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
                         (Target.Reputation(1),
                          Target.Reputation(2));
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
