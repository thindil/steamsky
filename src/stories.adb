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
with GNAT.String_Split; use GNAT.String_Split;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Factions; use Factions;
with Utils; use Utils;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Bases; use Bases;
with Events; use Events;
with Maps; use Maps;
with Crew; use Crew;
with Ships.Crew; use Ships.Crew;

package body Stories is

   procedure LoadStories(Reader: Tree_Reader) is
      TempRecord: Story_Data;
      NodesList, ChildNodes, StepDataNodes: Node_List;
      StoriesData: Document;
      TempValue: UnboundedString_Container.Vector;
      TempStep: Step_Data;
      TempSteps: Steps_Container.Vector;
      StartStep, FinalStep: Unbounded_String;
      TempTexts: StepTexts_Container.Vector;
      TempData: StepData_Container.Vector;
      Action: Unbounded_String;
      DeleteIndex: Positive;
      StoryNode, ChildNode, StepNode: Node;
   begin
      ClearCurrentStory;
      TempStep :=
        (Index => Null_Unbounded_String, FinishCondition => ASKINBASE,
         FinishData => TempData, FailText => Null_Unbounded_String,
         Texts => TempTexts);
      TempRecord :=
        (Index => Null_Unbounded_String, StartCondition => DROPITEM,
         StartData => TempValue, MinSteps => 1, MaxSteps => 2,
         StartingStep => TempStep, Steps => TempSteps, FinalStep => TempStep,
         EndText => Null_Unbounded_String, Name => Null_Unbounded_String,
         ForbiddenFactions => TempValue);
      StartStep := Null_Unbounded_String;
      StoriesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(StoriesData, "story");
      for I in 0 .. Length(NodesList) - 1 loop
         StoryNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(StoryNode, "index"));
         Action := To_Unbounded_String(Get_Attribute(StoryNode, "action"));
         if Action = Null_Unbounded_String or
           Action = To_Unbounded_String("add") then
            StartStep :=
              To_Unbounded_String(Get_Attribute(StoryNode, "startstep"));
            FinalStep :=
              To_Unbounded_String(Get_Attribute(StoryNode, "finalstep"));
            TempRecord.StartCondition :=
              StartConditionType'Value(Get_Attribute(StoryNode, "start"));
            TempRecord.MinSteps :=
              Positive'Value(Get_Attribute(StoryNode, "minsteps"));
            TempRecord.MaxSteps :=
              Positive'Value(Get_Attribute(StoryNode, "maxsteps"));
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(StoryNode, "name"));
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (StoryNode, "startdata");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.StartData.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Get_Attribute(Item(ChildNodes, J), "value")));
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (StoryNode, "forbiddenfaction");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.ForbiddenFactions.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Get_Attribute(Item(ChildNodes, J), "value")));
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(StoryNode, "step");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempStep :=
                 (Index => Null_Unbounded_String, FinishCondition => ASKINBASE,
                  FinishData => TempData, FailText => Null_Unbounded_String,
                  Texts => TempTexts);
               ChildNode := Item(ChildNodes, J);
               TempStep.Index :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               TempStep.FinishCondition :=
                 StepConditionType'Value(Get_Attribute(ChildNode, "finish"));
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (ChildNode, "finishdata");
               for K in 0 .. Length(StepDataNodes) - 1 loop
                  StepNode := Item(StepDataNodes, K);
                  TempStep.FinishData.Append
                    (New_Item =>
                       (Name =>
                          To_Unbounded_String(Get_Attribute(StepNode, "name")),
                        Value =>
                          To_Unbounded_String
                            (Get_Attribute(StepNode, "value"))));
               end loop;
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J), "text");
               for K in 0 .. Length(StepDataNodes) - 1 loop
                  StepNode := Item(StepDataNodes, K);
                  TempStep.Texts.Append
                    (New_Item =>
                       (Condition =>
                          StepConditionType'Value
                            (Get_Attribute(StepNode, "condition")),
                        Text =>
                          To_Unbounded_String
                            (Node_Value(First_Child(StepNode)))));
               end loop;
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J), "failtext");
               TempStep.FailText :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(StepDataNodes, 0))));
               if TempStep.Index = StartStep then
                  TempRecord.StartingStep := TempStep;
               elsif TempStep.Index = FinalStep then
                  TempRecord.FinalStep := TempStep;
               else
                  TempRecord.Steps.Append(New_Item => TempStep);
               end if;
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(StoryNode, "endtext");
            TempRecord.EndText :=
              To_Unbounded_String
                (Node_Value(First_Child(Item(ChildNodes, 0))));
            Stories_List.Append(New_Item => TempRecord);
            LogMessage
              ("Story added: " & To_String(TempRecord.Index), Everything);
         else
            for J in Stories_List.Iterate loop
               if Stories_List(J).Index = TempRecord.Index then
                  DeleteIndex := Stories_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            Stories_List.Delete(Index => DeleteIndex);
            LogMessage
              ("Story removed: " & To_String(TempRecord.Index), Everything);
         end if;
         TempRecord :=
           (Index => Null_Unbounded_String, StartCondition => DROPITEM,
            StartData => TempValue, MinSteps => 1, MaxSteps => 2,
            StartingStep => TempStep, Steps => TempSteps,
            FinalStep => TempStep, EndText => Null_Unbounded_String,
            Name => Null_Unbounded_String, ForbiddenFactions => TempValue);
      end loop;
   end LoadStories;

   function SelectBase(Value: String) return Unbounded_String is
      BaseIndex: Positive;
   begin
      if Value = "any" then
         return Null_Unbounded_String;
      end if;
      loop
         BaseIndex := GetRandom(SkyBases'First, SkyBases'Last);
         if SkyBases(BaseIndex).Known and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            PlayerShip.DestinationX := SkyBases(BaseIndex).SkyX;
            PlayerShip.DestinationY := SkyBases(BaseIndex).SkyY;
            return SkyBases(BaseIndex).Name;
         end if;
      end loop;
   end SelectBase;

   function SelectLocation
     (StepData: StepData_Container.Vector) return Unbounded_String is
      LocationData, Value: Unbounded_String := Null_Unbounded_String;
      LocationX, LocationY: Integer;
   begin
      Value := GetStepData(StepData, "x");
      if Value = To_Unbounded_String("random") then
         LocationX := GetRandom(SkyMap'First, SkyMap'Last);
         LocationData := To_Unbounded_String(Integer'Image(LocationX));
         Append(LocationData, ";");
      else
         LocationX := Integer'Value(To_String(Value));
         LocationData := Value;
         Append(LocationData, ";");
      end if;
      PlayerShip.DestinationX := LocationX;
      Value := GetStepData(StepData, "y");
      if Value = To_Unbounded_String("random") then
         loop
            LocationY := GetRandom(SkyMap'First, SkyMap'Last);
            exit when SkyMap(LocationX, LocationY).BaseIndex = 0 and
              LocationY /= PlayerShip.SkyY;
         end loop;
         Append(LocationData, Integer'Image(LocationY));
         Append(LocationData, ";");
      else
         LocationY := Integer'Value(To_String(Value));
         Append(LocationData, Value);
         Append(LocationData, ";");
      end if;
      PlayerShip.DestinationY := LocationY;
      return LocationData;
   end SelectLocation;

   function SelectEnemy
     (StepData: StepData_Container.Vector) return Unbounded_String is
      Enemies: Positive_Container.Vector;
      EnemyData, Value: Unbounded_String := Null_Unbounded_String;
   begin
      EnemyData := SelectLocation(StepData);
      Value := GetStepData(StepData, "ship");
      if Value /= To_Unbounded_String("random") then
         return EnemyData & Value;
      end if;
      Value := GetStepData(StepData, "faction");
      GenerateEnemies(Enemies, Value);
      return EnemyData &
        To_Unbounded_String
          (Integer'Image
             (Enemies(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
   end SelectEnemy;

   function SelectLoot
     (StepData: StepData_Container.Vector) return Unbounded_String is
      Enemies: Positive_Container.Vector;
      LootData, Value: Unbounded_String := Null_Unbounded_String;
   begin
      LootData := GetStepData(StepData, "item");
      Append(LootData, ";");
      Value := GetStepData(StepData, "ship");
      if Value /= To_Unbounded_String("random") then
         return LootData & Value;
      end if;
      Value := GetStepData(StepData, "faction");
      GenerateEnemies(Enemies, Value);
      return LootData &
        To_Unbounded_String
          (Integer'Image
             (Enemies(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
   end SelectLoot;

   procedure StartStory(FactionName: Unbounded_String;
      Condition: StartConditionType) is
      FactionIndex, StepData: Unbounded_String := Null_Unbounded_String;
      TempTexts: UnboundedString_Container.Vector;
      CanStart: Boolean;
   begin
      if CurrentStory.Index > 0 then
         return;
      end if;
      for Faction of Factions_List loop
         if Faction.Name = FactionName then
            FactionIndex := Faction.Index;
            exit;
         end if;
      end loop;
      if FactionIndex = Null_Unbounded_String then
         return;
      end if;
      for I in Stories_List.Iterate loop
         CanStart := True;
         for ForbiddenFaction of Stories_List(I).ForbiddenFactions loop
            if To_Lower(To_String(ForbiddenFaction)) =
              To_Lower
                (To_String
                   (Factions_List(PlayerShip.Crew(1).Faction).Index)) then
               CanStart := False;
               exit;
            end if;
         end loop;
         if CanStart then
            case Condition is
               when DROPITEM =>
                  if Stories_List(I).StartData(2) = FactionIndex
                    and then
                      GetRandom
                        (1,
                         Positive'Value
                           (To_String(Stories_List(I).StartData(3)))) =
                      1 then
                     case Stories_List(I).StartingStep.FinishCondition is
                        when ASKINBASE =>
                           StepData :=
                             SelectBase
                               (To_String
                                  (GetStepData
                                     (Stories_List(I).StartingStep.FinishData,
                                      "base")));
                        when DESTROYSHIP =>
                           StepData :=
                             SelectEnemy
                               (Stories_List(I).StartingStep.FinishData);
                        when EXPLORE =>
                           StepData :=
                             SelectLocation
                               (Stories_List(I).StartingStep.FinishData);
                        when LOOT =>
                           StepData :=
                             SelectLoot
                               (Stories_List(I).StartingStep.FinishData);
                        when ANY =>
                           null;
                     end case;
                     CurrentStory :=
                       (Index => Stories_Container.To_Index(I), Step => 1,
                        CurrentStep => 0,
                        MaxSteps =>
                          GetRandom
                            (Stories_List(I).MinSteps,
                             Stories_List(I).MaxSteps),
                        ShowText => True, Data => StepData,
                        FinishedStep => ANY);
                     UpdateCargo
                       (PlayerShip,
                        Positive'Value
                          (To_String(Stories_List(I).StartData(1))),
                        1);
                     FinishedStories.Append
                       (New_Item =>
                          (Index => CurrentStory.Index,
                           StepsAmount => CurrentStory.MaxSteps,
                           StepsTexts => TempTexts));
                     return;
                  end if;
            end case;
         end if;
      end loop;
   end StartStory;

   procedure ClearCurrentStory is
   begin
      CurrentStory :=
        (Index => 0, Step => 1, CurrentStep => -3, MaxSteps => 1,
         ShowText => False, Data => Null_Unbounded_String,
         FinishedStep => ANY);
   end ClearCurrentStory;

   function ProgressStory(NextStep: Boolean := False) return Boolean is
      Step: Step_Data;
      MaxRandom: Positive;
      FinishCondition: Unbounded_String;
      Chance: Natural;
   begin
      if CurrentStory.CurrentStep = 0 then
         Step := Stories_List(CurrentStory.Index).StartingStep;
      elsif CurrentStory.CurrentStep > 0 then
         Step :=
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep);
      else
         Step := Stories_List(CurrentStory.Index).FinalStep;
      end if;
      MaxRandom :=
        Positive'Value(To_String(GetStepData(Step.FinishData, "chance")));
      if Step.FinishCondition = DESTROYSHIP and NextStep then
         MaxRandom := 1;
      end if;
      FinishCondition := GetStepData(Step.FinishData, "condition");
      if FinishCondition = To_Unbounded_String("random")
        and then GetRandom(1, MaxRandom) > 1 then
         UpdateGame(10);
         return False;
      else
         Chance := 0;
         case Step.FinishCondition is
            when ASKINBASE =>
               declare
                  TraderIndex: constant Natural := FindMember(Talk);
               begin
                  if TraderIndex > 0 then
                     Chance :=
                       GetSkillLevel
                         (PlayerShip.Crew(TraderIndex),
                          FindSkillIndex(FinishCondition));
                  end if;
               end;
            when DESTROYSHIP | EXPLORE =>
               for Member of PlayerShip.Crew loop
                  if Member.Order = Pilot or Member.Order = Gunner then
                     Chance :=
                       Chance +
                       GetSkillLevel(Member, FindSkillIndex(FinishCondition));
                  end if;
               end loop;
            when LOOT =>
               for Member of PlayerShip.Crew loop
                  if Member.Order = Boarding then
                     Chance :=
                       Chance +
                       GetSkillLevel(Member, FindSkillIndex(FinishCondition));
                  end if;
               end loop;
            when ANY =>
               null;
         end case;
         Chance := Chance + GetRandom(1, 100);
         if Chance < MaxRandom then
            UpdateGame(10);
            return False;
         end if;
      end if;
      if Step.FinishCondition = DESTROYSHIP and not NextStep then
         return True;
      end if;
      if FinishCondition /= To_Unbounded_String("random") then
         case Step.FinishCondition is
            when ASKINBASE =>
               declare
                  TraderIndex: constant Natural := FindMember(Talk);
               begin
                  if TraderIndex > 0 then
                     GainExp(10, FindSkillIndex(FinishCondition), TraderIndex);
                  end if;
               end;
            when DESTROYSHIP | EXPLORE =>
               for I in PlayerShip.Crew.Iterate loop
                  if PlayerShip.Crew(I).Order = Pilot or
                    PlayerShip.Crew(I).Order = Gunner then
                     GainExp
                       (10, FindSkillIndex(FinishCondition),
                        Crew_Container.To_Index(I));
                  end if;
               end loop;
            when LOOT =>
               for I in PlayerShip.Crew.Iterate loop
                  if PlayerShip.Crew(I).Order = Boarding then
                     GainExp
                       (10, FindSkillIndex(FinishCondition),
                        Crew_Container.To_Index(I));
                  end if;
               end loop;
            when ANY =>
               null;
         end case;
      end if;
      UpdateGame(30);
      for FinishedStory of FinishedStories loop
         if FinishedStory.Index = CurrentStory.Index then
            FinishedStory.StepsTexts.Append(New_Item => GetCurrentStoryText);
            exit;
         end if;
      end loop;
      CurrentStory.Step := CurrentStory.Step + 1;
      CurrentStory.FinishedStep := Step.FinishCondition;
      CurrentStory.ShowText := True;
      if CurrentStory.Step < CurrentStory.MaxSteps then
         CurrentStory.CurrentStep :=
           GetRandom
             (Stories_List(CurrentStory.Index).Steps.First_Index,
              Stories_List(CurrentStory.Index).Steps.Last_Index);
         Step :=
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep);
      elsif CurrentStory.Step = CurrentStory.MaxSteps then
         CurrentStory.CurrentStep := -1;
         Step := Stories_List(CurrentStory.Index).FinalStep;
      else
         CurrentStory.CurrentStep := -2;
      end if;
      if CurrentStory.CurrentStep /= -2 then
         case Step.FinishCondition is
            when ASKINBASE =>
               CurrentStory.Data :=
                 SelectBase(To_String(GetStepData(Step.FinishData, "base")));
            when DESTROYSHIP =>
               CurrentStory.Data := SelectEnemy(Step.FinishData);
            when EXPLORE =>
               CurrentStory.Data := SelectLocation(Step.FinishData);
            when LOOT =>
               CurrentStory.Data := SelectLoot(Step.FinishData);
            when ANY =>
               null;
         end case;
      end if;
      return True;
   end ProgressStory;

   function GetCurrentStoryText return Unbounded_String is
      StepTexts: StepTexts_Container.Vector;
   begin
      if CurrentStory.CurrentStep = 0 then
         StepTexts := Stories_List(CurrentStory.Index).StartingStep.Texts;
      elsif CurrentStory.CurrentStep > 0 then
         StepTexts :=
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep)
             .Texts;
      else
         StepTexts := Stories_List(CurrentStory.Index).FinalStep.Texts;
      end if;
      for Text of StepTexts loop
         if Text.Condition = CurrentStory.FinishedStep then
            return Text.Text;
         end if;
      end loop;
      return Null_Unbounded_String;
   end GetCurrentStoryText;

   function GetStepData(FinishData: StepData_Container.Vector;
      Name: String) return Unbounded_String is
   begin
      for Data of FinishData loop
         if Data.Name = To_Unbounded_String(Name) then
            return Data.Value;
         end if;
      end loop;
      return Null_Unbounded_String;
   end GetStepData;

   procedure GetStoryLocation(StoryX, StoryY: in out Positive) is
      Tokens: Slice_Set;
   begin
      if CurrentStory.Data /= Null_Unbounded_String then
         Create(Tokens, To_String(CurrentStory.Data), ";");
         if Slice_Count(Tokens) < 3 then
            for I in SkyBases'Range loop
               if SkyBases(I).Name = CurrentStory.Data then
                  StoryX := SkyBases(I).SkyX;
                  StoryY := SkyBases(I).SkyY;
                  exit;
               end if;
            end loop;
         else
            StoryX := Integer'Value(Slice(Tokens, 1));
            StoryY := Integer'Value(Slice(Tokens, 2));
         end if;
      else
         StoryX := PlayerShip.SkyX;
         StoryY := PlayerShip.SkyY;
      end if;
   end GetStoryLocation;

end Stories;
