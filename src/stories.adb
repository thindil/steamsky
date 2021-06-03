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
      StartStep, FinalStep, Value, StoryIndex: Unbounded_String;
      TempTexts: StepTexts_Container.Vector;
      TempData: StepData_Container.Vector;
      Action, SubAction, SubSubAction: Data_Action;
      StoryNode, ChildNode, StepNode: Node;
      DeleteIndex, StepIndex: Positive;
   begin
      ClearCurrentStory;
      TempStep :=
        (Index => Null_Unbounded_String, FinishCondition => ASKINBASE,
         FinishData => TempData, FailText => Null_Unbounded_String,
         Texts => TempTexts);
      StartStep := Null_Unbounded_String;
      StoriesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(StoriesData, "story");
      Load_Stories_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (StartCondition => DROPITEM, StartData => TempValue, MinSteps => 1,
            MaxSteps => 2, StartingStep => TempStep, Steps => TempSteps,
            FinalStep => TempStep, EndText => Null_Unbounded_String,
            Name => Null_Unbounded_String, ForbiddenFactions => TempValue);
         StoryNode := Item(NodesList, I);
         StoryIndex := To_Unbounded_String(Get_Attribute(StoryNode, "index"));
         Action :=
           (if Get_Attribute(StoryNode, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(StoryNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Stories_Container.Contains(Stories_List, StoryIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " story '" & To_String(StoryIndex) &
                 "', there no story with that index.";
            end if;
         elsif Stories_Container.Contains(Stories_List, StoryIndex) then
            raise Data_Loading_Error
              with "Can't add story '" & To_String(StoryIndex) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Stories_List(StoryIndex);
               StartStep := Null_Unbounded_String;
               FinalStep := Null_Unbounded_String;
            end if;
            if Get_Attribute(StoryNode, "startstep")'Length > 0 then
               StartStep :=
                 To_Unbounded_String(Get_Attribute(StoryNode, "startstep"));
            end if;
            if Get_Attribute(StoryNode, "finalstep")'Length > 0 then
               FinalStep :=
                 To_Unbounded_String(Get_Attribute(StoryNode, "finalstep"));
            end if;
            if Get_Attribute(StoryNode, "start")'Length > 0 then
               TempRecord.StartCondition :=
                 StartConditionType'Value(Get_Attribute(StoryNode, "start"));
            end if;
            if Get_Attribute(StoryNode, "minsteps")'Length > 0 then
               TempRecord.MinSteps :=
                 Positive'Value(Get_Attribute(StoryNode, "minsteps"));
            end if;
            if Get_Attribute(StoryNode, "maxsteps")'Length > 0 then
               TempRecord.MaxSteps :=
                 Positive'Value(Get_Attribute(StoryNode, "maxsteps"));
            end if;
            if Get_Attribute(StoryNode, "name")'Length > 0 then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(StoryNode, "name"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (StoryNode, "startdata");
            Load_Start_Data_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               Value := To_Unbounded_String(Get_Attribute(ChildNode, "value"));
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               case SubAction is
                  when ADD =>
                     TempRecord.StartData.Append(New_Item => Value);
                  when REMOVE =>
                     Find_Delete_Start_Index_Loop :
                     for K in TempRecord.StartData.Iterate loop
                        if TempRecord.StartData(K) = Value then
                           DeleteIndex :=
                             UnboundedString_Container.To_Index(K);
                           exit Find_Delete_Start_Index_Loop;
                        end if;
                     end loop Find_Delete_Start_Index_Loop;
                     TempRecord.StartData.Delete(Index => DeleteIndex);
                  when UPDATE =>
                     null;
               end case;
            end loop Load_Start_Data_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (StoryNode, "forbiddenfaction");
            Load_Forbidden_Faction_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               Value := To_Unbounded_String(Get_Attribute(ChildNode, "value"));
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               case SubAction is
                  when ADD =>
                     TempRecord.ForbiddenFactions.Append(New_Item => Value);
                  when REMOVE =>
                     Find_Delete_Forbidden_Index_Loop :
                     for K in TempRecord.ForbiddenFactions.Iterate loop
                        if TempRecord.ForbiddenFactions(K) = Value then
                           DeleteIndex :=
                             UnboundedString_Container.To_Index(K);
                           exit Find_Delete_Forbidden_Index_Loop;
                        end if;
                     end loop Find_Delete_Forbidden_Index_Loop;
                     TempRecord.ForbiddenFactions.Delete(Index => DeleteIndex);
                  when UPDATE =>
                     null;
               end case;
            end loop Load_Forbidden_Faction_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(StoryNode, "step");
            Load_Steps_Data_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempStep :=
                 (Index => Null_Unbounded_String, FinishCondition => ASKINBASE,
                  FinishData => TempData, FailText => Null_Unbounded_String,
                  Texts => TempTexts);
               ChildNode := Item(ChildNodes, J);
               TempStep.Index :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               Find_Step_Index_Loop :
               for K in TempRecord.Steps.Iterate loop
                  if TempRecord.Steps(K).Index = TempStep.Index then
                     StepIndex := Steps_Container.To_Index(K);
                     exit Find_Step_Index_Loop;
                  end if;
               end loop Find_Step_Index_Loop;
               if SubAction /= REMOVE then
                  if SubAction = UPDATE then
                     TempStep := TempRecord.Steps(StepIndex);
                  end if;
                  if Get_Attribute(ChildNode, "finish")'Length > 0 then
                     TempStep.FinishCondition :=
                       StepConditionType'Value
                         (Get_Attribute(ChildNode, "finish"));
                  end if;
                  StepDataNodes :=
                    DOM.Core.Elements.Get_Elements_By_Tag_Name
                      (ChildNode, "finishdata");
                  Load_Finish_Data_Loop :
                  for K in 0 .. Length(StepDataNodes) - 1 loop
                     StepNode := Item(StepDataNodes, K);
                     SubSubAction :=
                       (if Get_Attribute(StepNode, "action")'Length > 0 then
                          Data_Action'Value(Get_Attribute(StepNode, "action"))
                        else ADD);
                     Value :=
                       To_Unbounded_String(Get_Attribute(StepNode, "name"));
                     case SubSubAction is
                        when ADD =>
                           TempStep.FinishData.Append
                             (New_Item =>
                                (Name => Value,
                                 Value =>
                                   To_Unbounded_String
                                     (Get_Attribute(StepNode, "value"))));
                        when UPDATE =>
                           for Data of TempStep.FinishData loop
                              if Data.Name = Value then
                                 Data.Value :=
                                   To_Unbounded_String
                                     (Get_Attribute(StepNode, "value"));
                                 exit;
                              end if;
                           end loop;
                        when REMOVE =>
                           Find_Delete_Finish_Index_Loop :
                           for L in TempStep.FinishData.Iterate loop
                              if TempStep.FinishData(L).Name = Value then
                                 DeleteIndex := StepData_Container.To_Index(L);
                                 exit Find_Delete_Finish_Index_Loop;
                              end if;
                           end loop Find_Delete_Finish_Index_Loop;
                           TempStep.FinishData.Delete(Index => DeleteIndex);
                     end case;
                  end loop Load_Finish_Data_Loop;
                  StepDataNodes :=
                    DOM.Core.Elements.Get_Elements_By_Tag_Name
                      (Item(ChildNodes, J), "text");
                  Load_Step_Text_Loop :
                  for K in 0 .. Length(StepDataNodes) - 1 loop
                     StepNode := Item(StepDataNodes, K);
                     SubSubAction :=
                       (if Get_Attribute(StepNode, "action")'Length > 0 then
                          Data_Action'Value(Get_Attribute(StepNode, "action"))
                        else ADD);
                     Value :=
                       To_Unbounded_String
                         (Get_Attribute(StepNode, "condition"));
                     case SubSubAction is
                        when ADD =>
                           TempStep.Texts.Append
                             (New_Item =>
                                (Condition =>
                                   StepConditionType'Value(To_String(Value)),
                                 Text =>
                                   To_Unbounded_String
                                     (Node_Value(First_Child(StepNode)))));
                        when UPDATE =>
                           Load_Update_Text_Loop :
                           for Text of TempStep.Texts loop
                              if Text.Condition =
                                StepConditionType'Value(To_String(Value)) then
                                 Text.Text :=
                                   To_Unbounded_String
                                     (Node_Value(First_Child(StepNode)));
                                 exit Load_Update_Text_Loop;
                              end if;
                           end loop Load_Update_Text_Loop;
                        when REMOVE =>
                           Find_Delete_Text_Index_Loop :
                           for L in TempStep.Texts.Iterate loop
                              if TempStep.Texts(L).Condition =
                                StepConditionType'Value(To_String(Value)) then
                                 DeleteIndex :=
                                   StepTexts_Container.To_Index(L);
                                 exit Find_Delete_Text_Index_Loop;
                              end if;
                           end loop Find_Delete_Text_Index_Loop;
                           TempStep.Texts.Delete(Index => DeleteIndex);
                     end case;
                  end loop Load_Step_Text_Loop;
                  StepDataNodes :=
                    DOM.Core.Elements.Get_Elements_By_Tag_Name
                      (Item(ChildNodes, J), "failtext");
                  if Length(StepDataNodes) > 0 then
                     TempStep.FailText :=
                       To_Unbounded_String
                         (Node_Value(First_Child(Item(StepDataNodes, 0))));
                  end if;
                  if TempStep.Index = StartStep then
                     TempRecord.StartingStep := TempStep;
                  elsif TempStep.Index = FinalStep then
                     TempRecord.FinalStep := TempStep;
                  else
                     if SubAction = ADD then
                        TempRecord.Steps.Append(New_Item => TempStep);
                     else
                        TempRecord.Steps(StepIndex) := TempStep;
                     end if;
                  end if;
               else
                  TempRecord.Steps.Delete(Index => StepIndex);
               end if;
            end loop Load_Steps_Data_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(StoryNode, "endtext");
            if Length(ChildNodes) > 0 then
               TempRecord.EndText :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(ChildNodes, 0))));
            end if;
            if Action /= UPDATE then
               Stories_Container.Include(Stories_List, StoryIndex, TempRecord);
               Log_Message
                 ("Story added: " & To_String(StoryIndex), EVERYTHING);
            else
               Stories_List(StoryIndex) := TempRecord;
               Log_Message
                 ("Story updated: " & To_String(StoryIndex), EVERYTHING);
            end if;
         else
            Stories_Container.Exclude(Stories_List, StoryIndex);
            Log_Message("Story removed: " & To_String(StoryIndex), EVERYTHING);
         end if;
      end loop Load_Stories_Loop;
   end LoadStories;

   -- ****if* Stories/Stories.SelectBase
   -- Select name of the base for story
   -- PARAMETERS
   -- Value - Only value "any" matters
   -- RESULT
   -- Empty string if Value is "any", otherwise random base name
   -- SOURCE
   function SelectBase(Value: String) return Unbounded_String is
      -- ****
      BaseIndex: Bases_Range;
   begin
      if Value = "any" then
         return Null_Unbounded_String;
      end if;
      Select_Base_Loop :
      loop
         BaseIndex := GetRandom(SkyBases'First, SkyBases'Last);
         if SkyBases(BaseIndex).Known and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            Player_Ship.Destination_X := SkyBases(BaseIndex).SkyX;
            Player_Ship.Destination_Y := SkyBases(BaseIndex).SkyY;
            return SkyBases(BaseIndex).Name;
         end if;
      end loop Select_Base_Loop;
   end SelectBase;

   -- ****if* Stories/Stories.SelectLocation
   -- FUNCTION
   -- Get the map location for story step
   -- PARAMETERS
   -- StepData - Data for selected step
   -- RESULT
   -- String with X and Y coordinates for selected step location.
   -- SOURCE
   function SelectLocation
     (StepData: StepData_Container.Vector) return Unbounded_String is
      -- ****
      LocationData, Value: Unbounded_String := Null_Unbounded_String;
      LocationX, LocationY: Positive;
   begin
      Value := GetStepData(StepData, "x");
      if Value = To_Unbounded_String("random") then
         LocationX := GetRandom(SkyMap'First(1), SkyMap'Last(1));
         LocationData := To_Unbounded_String(Integer'Image(LocationX));
         Append(LocationData, ";");
      else
         LocationX := Integer'Value(To_String(Value));
         LocationData := Value;
         Append(LocationData, ";");
      end if;
      Player_Ship.Destination_X := LocationX;
      Value := GetStepData(StepData, "y");
      if Value = To_Unbounded_String("random") then
         Random_Location_Loop :
         loop
            LocationY := GetRandom(SkyMap'First(2), SkyMap'Last(2));
            exit Random_Location_Loop when SkyMap(LocationX, LocationY)
                .BaseIndex =
              0 and
              LocationY /= Player_Ship.Sky_Y;
         end loop Random_Location_Loop;
         Append(LocationData, Integer'Image(LocationY));
         Append(LocationData, ";");
      else
         LocationY := Integer'Value(To_String(Value));
         Append(LocationData, Value);
         Append(LocationData, ";");
      end if;
      Player_Ship.Destination_Y := LocationY;
      return LocationData;
   end SelectLocation;

   -- ****if* Stories/Stories.SelectEnemy
   -- FUNCTION
   -- Get enemy ship for selected story step
   -- PARAMETERS
   -- StepData - Data for selected step
   -- RESULT
   -- String with location and name for enemy ship for selected story step
   -- SOURCE
   function SelectEnemy
     (StepData: StepData_Container.Vector) return Unbounded_String is
      -- ****
      Enemies: UnboundedString_Container.Vector;
      EnemyData, Value: Unbounded_String := Null_Unbounded_String;
   begin
      EnemyData := SelectLocation(StepData);
      Value := GetStepData(StepData, "ship");
      if Value /= To_Unbounded_String("random") then
         return EnemyData & Value;
      end if;
      Value := GetStepData(StepData, "faction");
      GenerateEnemies(Enemies, Value);
      return
        EnemyData &
        Enemies(GetRandom(Enemies.First_Index, Enemies.Last_Index));
   end SelectEnemy;

   -- ****if* Stories/Stories.SelectLoot
   -- FUNCTION
   -- Get what item should be looted for this step
   -- PARAMETERS
   -- StepData - Data for selected step
   -- RESULT
   -- String with Item type and enemy prototype ship index to loot
   -- SOURCE
   function SelectLoot
     (StepData: StepData_Container.Vector) return Unbounded_String is
      -- ****
      Enemies: UnboundedString_Container.Vector;
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
      return
        LootData & Enemies(GetRandom(Enemies.First_Index, Enemies.Last_Index));
   end SelectLoot;

   procedure StartStory
     (FactionName: Unbounded_String; Condition: StartConditionType) is
      FactionIndex, StepData: Unbounded_String := Null_Unbounded_String;
      TempTexts: UnboundedString_Container.Vector;
      CanStart: Boolean;
   begin
      if CurrentStory.Index /= Null_Unbounded_String then
         return;
      end if;
      Find_Faction_Index_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = FactionName then
            FactionIndex := Factions_Container.Key(I);
            exit Find_Faction_Index_Loop;
         end if;
      end loop Find_Faction_Index_Loop;
      if FactionIndex = Null_Unbounded_String then
         return;
      end if;
      Check_Stories_Loop :
      for I in Stories_List.Iterate loop
         CanStart := True;
         Check_Faction_Loop :
         for ForbiddenFaction of Stories_List(I).ForbiddenFactions loop
            if To_Lower(To_String(ForbiddenFaction)) =
              To_Lower(To_String(Player_Ship.Crew(1).Faction)) then
               CanStart := False;
               exit Check_Faction_Loop;
            end if;
         end loop Check_Faction_Loop;
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
                       (Index => Stories_Container.Key(I), Step => 1,
                        CurrentStep => 0,
                        MaxSteps =>
                          GetRandom
                            (Stories_List(I).MinSteps,
                             Stories_List(I).MaxSteps),
                        ShowText => True, Data => StepData,
                        FinishedStep => ANY);
                     UpdateCargo(Player_Ship, Stories_List(I).StartData(1), 1);
                     FinishedStories.Append
                       (New_Item =>
                          (Index => CurrentStory.Index,
                           StepsAmount => CurrentStory.MaxSteps,
                           StepsTexts => TempTexts));
                     return;
                  end if;
            end case;
         end if;
      end loop Check_Stories_Loop;
   end StartStory;

   procedure ClearCurrentStory is
   begin
      CurrentStory :=
        (Index => Null_Unbounded_String, Step => 1, CurrentStep => -3,
         MaxSteps => 1, ShowText => False, Data => Null_Unbounded_String,
         FinishedStep => ANY);
   end ClearCurrentStory;

   function ProgressStory(NextStep: Boolean := False) return Boolean is
      Step: Step_Data :=
        (if CurrentStory.CurrentStep = 0 then
           Stories_List(CurrentStory.Index).StartingStep
         elsif CurrentStory.CurrentStep > 0 then
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep)
         else Stories_List(CurrentStory.Index).FinalStep);
      MaxRandom: constant Positive :=
        (if Step.FinishCondition = DESTROYSHIP and NextStep then 1
         else Positive'Value
             (To_String(GetStepData(Step.FinishData, "chance"))));
      FinishCondition: Unbounded_String;
      Chance: Natural;
   begin
      FinishCondition := GetStepData(Step.FinishData, "condition");
      if FinishCondition = To_Unbounded_String("random")
        and then GetRandom(1, MaxRandom) > 1 then
         Update_Game(10);
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
                         (Player_Ship.Crew(TraderIndex),
                          Find_Skill_Index(FinishCondition));
                  end if;
               end;
            when DESTROYSHIP | EXPLORE =>
               Count_Explore_Chance_Loop :
               for Member of Player_Ship.Crew loop
                  if Member.Order = Pilot or Member.Order = Gunner then
                     Chance :=
                       Chance +
                       GetSkillLevel
                         (Member, Find_Skill_Index(FinishCondition));
                  end if;
               end loop Count_Explore_Chance_Loop;
            when LOOT =>
               Count_Loot_Chance_Loop :
               for Member of Player_Ship.Crew loop
                  if Member.Order = Boarding then
                     Chance :=
                       Chance +
                       GetSkillLevel
                         (Member, Find_Skill_Index(FinishCondition));
                  end if;
               end loop Count_Loot_Chance_Loop;
            when ANY =>
               null;
         end case;
         Chance := Chance + GetRandom(1, 100);
         if Chance < MaxRandom then
            Update_Game(10);
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
                     GainExp
                       (10, Find_Skill_Index(FinishCondition), TraderIndex);
                  end if;
               end;
            when DESTROYSHIP | EXPLORE =>
               Count_Explore_Experience_Loop :
               for I in Player_Ship.Crew.Iterate loop
                  if Player_Ship.Crew(I).Order = Pilot or
                    Player_Ship.Crew(I).Order = Gunner then
                     GainExp
                       (10, Find_Skill_Index(FinishCondition),
                        Crew_Container.To_Index(I));
                  end if;
               end loop Count_Explore_Experience_Loop;
            when LOOT =>
               Count_Loot_Experience_Loop :
               for I in Player_Ship.Crew.Iterate loop
                  if Player_Ship.Crew(I).Order = Boarding then
                     GainExp
                       (10, Find_Skill_Index(FinishCondition),
                        Crew_Container.To_Index(I));
                  end if;
               end loop Count_Loot_Experience_Loop;
            when ANY =>
               null;
         end case;
      end if;
      Update_Game(30);
      Update_Finished_Stories_Loop :
      for FinishedStory of FinishedStories loop
         if FinishedStory.Index = CurrentStory.Index then
            FinishedStory.StepsTexts.Append(New_Item => GetCurrentStoryText);
            exit Update_Finished_Stories_Loop;
         end if;
      end loop Update_Finished_Stories_Loop;
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
      StepTexts: constant StepTexts_Container.Vector :=
        (if CurrentStory.CurrentStep = 0 then
           Stories_List(CurrentStory.Index).StartingStep.Texts
         elsif CurrentStory.CurrentStep > 0 then
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep)
             .Texts
         else Stories_List(CurrentStory.Index).FinalStep.Texts);
   begin
      Current_Story_Text_Loop :
      for Text of StepTexts loop
         if Text.Condition = CurrentStory.FinishedStep then
            return Text.Text;
         end if;
      end loop Current_Story_Text_Loop;
      return Null_Unbounded_String;
   end GetCurrentStoryText;

   function GetStepData
     (FinishData: StepData_Container.Vector; Name: String)
      return Unbounded_String is
   begin
      Get_Step_Data_Loop :
      for Data of FinishData loop
         if Data.Name = To_Unbounded_String(Name) then
            return Data.Value;
         end if;
      end loop Get_Step_Data_Loop;
      return Null_Unbounded_String;
   end GetStepData;

   procedure GetStoryLocation
     (StoryX: out Map_X_Range; StoryY: out Map_Y_Range) is
      Tokens: Slice_Set;
   begin
      if CurrentStory.Data /= Null_Unbounded_String then
         Create(Tokens, To_String(CurrentStory.Data), ";");
         if Slice_Count(Tokens) < 3 then
            Get_Story_Location_Loop :
            for I in SkyBases'Range loop
               if SkyBases(I).Name = CurrentStory.Data then
                  StoryX := SkyBases(I).SkyX;
                  StoryY := SkyBases(I).SkyY;
                  exit Get_Story_Location_Loop;
               end if;
            end loop Get_Story_Location_Loop;
         else
            StoryX := Integer'Value(Slice(Tokens, 1));
            StoryY := Integer'Value(Slice(Tokens, 2));
         end if;
      else
         StoryX := Player_Ship.Sky_X;
         StoryY := Player_Ship.Sky_Y;
      end if;
   end GetStoryLocation;

end Stories;
