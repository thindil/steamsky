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

   procedure Load_Stories(Reader: Tree_Reader) is
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
      Clear_Current_Story;
      TempStep :=
        (Index => Null_Unbounded_String, Finish_Condition => ASKINBASE,
         Finish_Data => TempData, Fail_Text => Null_Unbounded_String,
         Texts => TempTexts);
      StartStep := Null_Unbounded_String;
      StoriesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(StoriesData, "story");
      Load_Stories_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Start_Condition => DROPITEM, Start_Data => TempValue, Min_Steps => 1,
            Max_Steps => 2, Starting_Step => TempStep, Steps => TempSteps,
            Final_Step => TempStep, End_Text => Null_Unbounded_String,
            Name => Null_Unbounded_String, Forbidden_Factions => TempValue);
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
               TempRecord.Start_Condition :=
                 Start_Condition_Type'Value(Get_Attribute(StoryNode, "start"));
            end if;
            if Get_Attribute(StoryNode, "minsteps")'Length > 0 then
               TempRecord.Min_Steps :=
                 Positive'Value(Get_Attribute(StoryNode, "minsteps"));
            end if;
            if Get_Attribute(StoryNode, "maxsteps")'Length > 0 then
               TempRecord.Max_Steps :=
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
                     TempRecord.Start_Data.Append(New_Item => Value);
                  when REMOVE =>
                     Find_Delete_Start_Index_Loop :
                     for K in TempRecord.Start_Data.Iterate loop
                        if TempRecord.Start_Data(K) = Value then
                           DeleteIndex :=
                             UnboundedString_Container.To_Index(K);
                           exit Find_Delete_Start_Index_Loop;
                        end if;
                     end loop Find_Delete_Start_Index_Loop;
                     TempRecord.Start_Data.Delete(Index => DeleteIndex);
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
                     TempRecord.Forbidden_Factions.Append(New_Item => Value);
                  when REMOVE =>
                     Find_Delete_Forbidden_Index_Loop :
                     for K in TempRecord.Forbidden_Factions.Iterate loop
                        if TempRecord.Forbidden_Factions(K) = Value then
                           DeleteIndex :=
                             UnboundedString_Container.To_Index(K);
                           exit Find_Delete_Forbidden_Index_Loop;
                        end if;
                     end loop Find_Delete_Forbidden_Index_Loop;
                     TempRecord.Forbidden_Factions.Delete(Index => DeleteIndex);
                  when UPDATE =>
                     null;
               end case;
            end loop Load_Forbidden_Faction_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(StoryNode, "step");
            Load_Steps_Data_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempStep :=
                 (Index => Null_Unbounded_String, Finish_Condition => ASKINBASE,
                  Finish_Data => TempData, Fail_Text => Null_Unbounded_String,
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
                     TempStep.Finish_Condition :=
                       Step_Condition_Type'Value
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
                           TempStep.Finish_Data.Append
                             (New_Item =>
                                (Name => Value,
                                 Value =>
                                   To_Unbounded_String
                                     (Get_Attribute(StepNode, "value"))));
                        when UPDATE =>
                           for Data of TempStep.Finish_Data loop
                              if Data.Name = Value then
                                 Data.Value :=
                                   To_Unbounded_String
                                     (Get_Attribute(StepNode, "value"));
                                 exit;
                              end if;
                           end loop;
                        when REMOVE =>
                           Find_Delete_Finish_Index_Loop :
                           for L in TempStep.Finish_Data.Iterate loop
                              if TempStep.Finish_Data(L).Name = Value then
                                 DeleteIndex := StepData_Container.To_Index(L);
                                 exit Find_Delete_Finish_Index_Loop;
                              end if;
                           end loop Find_Delete_Finish_Index_Loop;
                           TempStep.Finish_Data.Delete(Index => DeleteIndex);
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
                                   Step_Condition_Type'Value(To_String(Value)),
                                 Text =>
                                   To_Unbounded_String
                                     (Node_Value(First_Child(StepNode)))));
                        when UPDATE =>
                           Load_Update_Text_Loop :
                           for Text of TempStep.Texts loop
                              if Text.Condition =
                                Step_Condition_Type'Value(To_String(Value)) then
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
                                Step_Condition_Type'Value(To_String(Value)) then
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
                     TempStep.Fail_Text :=
                       To_Unbounded_String
                         (Node_Value(First_Child(Item(StepDataNodes, 0))));
                  end if;
                  if TempStep.Index = StartStep then
                     TempRecord.Starting_Step := TempStep;
                  elsif TempStep.Index = FinalStep then
                     TempRecord.Final_Step := TempStep;
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
               TempRecord.End_Text :=
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
   end Load_Stories;

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
         BaseIndex := Get_Random(Sky_Bases'First, Sky_Bases'Last);
         if Sky_Bases(BaseIndex).Known and
           Sky_Bases(BaseIndex).Reputation(1) > -25 then
            Player_Ship.Destination_X := Sky_Bases(BaseIndex).Sky_X;
            Player_Ship.Destination_Y := Sky_Bases(BaseIndex).Sky_Y;
            return Sky_Bases(BaseIndex).Name;
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
      Value := Get_Step_Data(StepData, "x");
      if Value = To_Unbounded_String("random") then
         LocationX := Get_Random(Sky_Map'First(1), Sky_Map'Last(1));
         LocationData := To_Unbounded_String(Integer'Image(LocationX));
         Append(LocationData, ";");
      else
         LocationX := Integer'Value(To_String(Value));
         LocationData := Value;
         Append(LocationData, ";");
      end if;
      Player_Ship.Destination_X := LocationX;
      Value := Get_Step_Data(StepData, "y");
      if Value = To_Unbounded_String("random") then
         Random_Location_Loop :
         loop
            LocationY := Get_Random(Sky_Map'First(2), Sky_Map'Last(2));
            exit Random_Location_Loop when Sky_Map(LocationX, LocationY)
                .Base_Index =
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
      Value := Get_Step_Data(StepData, "ship");
      if Value /= To_Unbounded_String("random") then
         return EnemyData & Value;
      end if;
      Value := Get_Step_Data(StepData, "faction");
      Generate_Enemies(Enemies, Value);
      return
        EnemyData &
        Enemies(Get_Random(Enemies.First_Index, Enemies.Last_Index));
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
      LootData := Get_Step_Data(StepData, "item");
      Append(LootData, ";");
      Value := Get_Step_Data(StepData, "ship");
      if Value /= To_Unbounded_String("random") then
         return LootData & Value;
      end if;
      Value := Get_Step_Data(StepData, "faction");
      Generate_Enemies(Enemies, Value);
      return
        LootData &
        Enemies(Get_Random(Enemies.First_Index, Enemies.Last_Index));
   end SelectLoot;

   procedure Start_Story
     (Faction_Name: Unbounded_String; Condition: Start_Condition_Type) is
      FactionIndex, StepData: Unbounded_String := Null_Unbounded_String;
      TempTexts: UnboundedString_Container.Vector;
   begin
      if Current_Story.Index /= Null_Unbounded_String then
         return;
      end if;
      Find_Faction_Index_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = Faction_Name then
            FactionIndex := Factions_Container.Key(I);
            exit Find_Faction_Index_Loop;
         end if;
      end loop Find_Faction_Index_Loop;
      if FactionIndex = Null_Unbounded_String then
         return;
      end if;
      Check_Stories_Loop :
      for I in Stories_List.Iterate loop
         Check_Faction_Loop :
         for ForbiddenFaction of Stories_List(I).Forbidden_Factions loop
            if To_Lower(To_String(ForbiddenFaction)) =
              To_Lower(To_String(Player_Ship.Crew(1).Faction)) then
               goto End_Of_Check_Stories_Loop;
            end if;
         end loop Check_Faction_Loop;
         case Condition is
            when DROPITEM =>
               if Stories_List(I).Start_Data(2) = FactionIndex
                 and then
                   Get_Random
                     (1,
                      Positive'Value
                        (To_String(Stories_List(I).Start_Data(3)))) =
                   1 then
                  case Stories_List(I).Starting_Step.Finish_Condition is
                     when ASKINBASE =>
                        StepData :=
                          SelectBase
                            (To_String
                               (Get_Step_Data
                                  (Stories_List(I).Starting_Step.Finish_Data,
                                   "base")));
                     when DESTROYSHIP =>
                        StepData :=
                          SelectEnemy(Stories_List(I).Starting_Step.Finish_Data);
                     when EXPLORE =>
                        StepData :=
                          SelectLocation
                            (Stories_List(I).Starting_Step.Finish_Data);
                     when LOOT =>
                        StepData :=
                          SelectLoot(Stories_List(I).Starting_Step.Finish_Data);
                     when ANY =>
                        null;
                  end case;
                  Current_Story :=
                    (Index => Stories_Container.Key(I), Step => 1,
                     Current_Step => 0,
                     Max_Steps =>
                       Get_Random
                         (Stories_List(I).Min_Steps, Stories_List(I).Max_Steps),
                     Show_Text => True, Data => StepData, Finished_Step => ANY);
                  UpdateCargo
                    (Player_Ship,
                     Tiny_String.To_Bounded_String
                       (Source =>
                          To_String(Source => Stories_List(I).Start_Data(1))),
                     1);
                  Finished_Stories.Append
                    (New_Item =>
                       (Index => Current_Story.Index,
                        Steps_Amount => Current_Story.Max_Steps,
                        Steps_Texts => TempTexts));
                  return;
               end if;
         end case;
         <<End_Of_Check_Stories_Loop>>
      end loop Check_Stories_Loop;
   end Start_Story;

   procedure Clear_Current_Story is
   begin
      Current_Story :=
        (Index => Null_Unbounded_String, Step => 1, Current_Step => -3,
         Max_Steps => 1, Show_Text => False, Data => Null_Unbounded_String,
         Finished_Step => ANY);
   end Clear_Current_Story;

   function Progress_Story(Next_Step: Boolean := False) return Boolean is
      Step: Step_Data :=
        (if Current_Story.Current_Step = 0 then
           Stories_List(Current_Story.Index).Starting_Step
         elsif Current_Story.Current_Step > 0 then
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step)
         else Stories_List(Current_Story.Index).Final_Step);
      MaxRandom: constant Positive :=
        (if Step.Finish_Condition = DESTROYSHIP and Next_Step then 1
         else Positive'Value
             (To_String(Get_Step_Data(Step.Finish_Data, "chance"))));
      FinishCondition: Unbounded_String;
      Chance: Natural;
   begin
      FinishCondition := Get_Step_Data(Step.Finish_Data, "condition");
      if FinishCondition = To_Unbounded_String("random")
        and then Get_Random(1, MaxRandom) > 1 then
         Update_Game(10);
         return False;
      else
         Chance := 0;
         case Step.Finish_Condition is
            when ASKINBASE =>
               declare
                  TraderIndex: constant Natural := Find_Member(TALK);
               begin
                  if TraderIndex > 0 then
                     Chance :=
                       Get_Skill_Level
                         (Player_Ship.Crew(TraderIndex),
                          Find_Skill_Index(To_String(FinishCondition)));
                  end if;
               end;
            when DESTROYSHIP | EXPLORE =>
               Count_Explore_Chance_Loop :
               for Member of Player_Ship.Crew loop
                  if Member.Order in PILOT | GUNNER then
                     Chance :=
                       Chance +
                       Get_Skill_Level
                         (Member,
                          Find_Skill_Index(To_String(FinishCondition)));
                  end if;
               end loop Count_Explore_Chance_Loop;
            when LOOT =>
               Count_Loot_Chance_Loop :
               for Member of Player_Ship.Crew loop
                  if Member.Order = BOARDING then
                     Chance :=
                       Chance +
                       Get_Skill_Level
                         (Member,
                          Find_Skill_Index(To_String(FinishCondition)));
                  end if;
               end loop Count_Loot_Chance_Loop;
            when ANY =>
               null;
         end case;
         Chance := Chance + Get_Random(1, 100);
         if Chance < MaxRandom then
            Update_Game(10);
            return False;
         end if;
      end if;
      if Step.Finish_Condition = DESTROYSHIP and not Next_Step then
         return True;
      end if;
      if FinishCondition /= To_Unbounded_String("random") then
         case Step.Finish_Condition is
            when ASKINBASE =>
               declare
                  TraderIndex: constant Natural := Find_Member(TALK);
               begin
                  if TraderIndex > 0 then
                     Gain_Exp
                       (10, Find_Skill_Index(To_String(FinishCondition)),
                        TraderIndex);
                  end if;
               end;
            when DESTROYSHIP | EXPLORE =>
               Count_Explore_Experience_Loop :
               for I in Player_Ship.Crew.Iterate loop
                  if Player_Ship.Crew(I).Order = PILOT or
                    Player_Ship.Crew(I).Order = GUNNER then
                     Gain_Exp
                       (10, Find_Skill_Index(To_String(FinishCondition)),
                        Crew_Container.To_Index(I));
                  end if;
               end loop Count_Explore_Experience_Loop;
            when LOOT =>
               Count_Loot_Experience_Loop :
               for I in Player_Ship.Crew.Iterate loop
                  if Player_Ship.Crew(I).Order = BOARDING then
                     Gain_Exp
                       (10, Find_Skill_Index(To_String(FinishCondition)),
                        Crew_Container.To_Index(I));
                  end if;
               end loop Count_Loot_Experience_Loop;
            when ANY =>
               null;
         end case;
      end if;
      Update_Game(30);
      Update_Finished_Stories_Loop :
      for FinishedStory of Finished_Stories loop
         if FinishedStory.Index = Current_Story.Index then
            FinishedStory.Steps_Texts.Append(New_Item => Get_Current_Story_Text);
            exit Update_Finished_Stories_Loop;
         end if;
      end loop Update_Finished_Stories_Loop;
      Current_Story.Step := Current_Story.Step + 1;
      Current_Story.Finished_Step := Step.Finish_Condition;
      Current_Story.Show_Text := True;
      if Current_Story.Step < Current_Story.Max_Steps then
         Current_Story.Current_Step :=
           Get_Random
             (Stories_List(Current_Story.Index).Steps.First_Index,
              Stories_List(Current_Story.Index).Steps.Last_Index);
         Step :=
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step);
      elsif Current_Story.Step = Current_Story.Max_Steps then
         Current_Story.Current_Step := -1;
         Step := Stories_List(Current_Story.Index).Final_Step;
      else
         Current_Story.Current_Step := -2;
      end if;
      if Current_Story.Current_Step /= -2 then
         case Step.Finish_Condition is
            when ASKINBASE =>
               Current_Story.Data :=
                 SelectBase(To_String(Get_Step_Data(Step.Finish_Data, "base")));
            when DESTROYSHIP =>
               Current_Story.Data := SelectEnemy(Step.Finish_Data);
            when EXPLORE =>
               Current_Story.Data := SelectLocation(Step.Finish_Data);
            when LOOT =>
               Current_Story.Data := SelectLoot(Step.Finish_Data);
            when ANY =>
               null;
         end case;
      end if;
      return True;
   end Progress_Story;

   function Get_Current_Story_Text return Unbounded_String is
      StepTexts: constant StepTexts_Container.Vector :=
        (if Current_Story.Current_Step = 0 then
           Stories_List(Current_Story.Index).Starting_Step.Texts
         elsif Current_Story.Current_Step > 0 then
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step)
             .Texts
         else Stories_List(Current_Story.Index).Final_Step.Texts);
   begin
      Current_Story_Text_Loop :
      for Text of StepTexts loop
         if Text.Condition = Current_Story.Finished_Step then
            return Text.Text;
         end if;
      end loop Current_Story_Text_Loop;
      return Null_Unbounded_String;
   end Get_Current_Story_Text;

   function Get_Step_Data
     (Finish_Data: StepData_Container.Vector; Name: String)
      return Unbounded_String is
   begin
      Get_Step_Data_Loop :
      for Data of Finish_Data loop
         if Data.Name = To_Unbounded_String(Name) then
            return Data.Value;
         end if;
      end loop Get_Step_Data_Loop;
      return Null_Unbounded_String;
   end Get_Step_Data;

   procedure Get_Story_Location
     (Story_X: out Map_X_Range; Story_Y: out Map_Y_Range) is
      Tokens: Slice_Set;
   begin
      if Current_Story.Data /= Null_Unbounded_String then
         Create(Tokens, To_String(Current_Story.Data), ";");
         if Slice_Count(Tokens) < 3 then
            Get_Story_Location_Loop :
            for I in Sky_Bases'Range loop
               if Sky_Bases(I).Name = Current_Story.Data then
                  Story_X := Sky_Bases(I).Sky_X;
                  Story_Y := Sky_Bases(I).Sky_Y;
                  exit Get_Story_Location_Loop;
               end if;
            end loop Get_Story_Location_Loop;
         else
            Story_X := Integer'Value(Slice(Tokens, 1));
            Story_Y := Integer'Value(Slice(Tokens, 2));
         end if;
      else
         Story_X := Player_Ship.Sky_X;
         Story_Y := Player_Ship.Sky_Y;
      end if;
   end Get_Story_Location;

end Stories;
