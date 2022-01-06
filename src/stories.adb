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
      Temp_Record: Story_Data;
      Nodes_List, Child_Nodes, Step_Data_Nodes: Node_List;
      Stories_Data: Document;
      Temp_Value: UnboundedString_Container.Vector;
      Temp_Step: Step_Data;
      Temp_Steps: Steps_Container.Vector;
      Start_Step, Final_Step, Value, Story_Index: Unbounded_String;
      Temp_Texts: StepTexts_Container.Vector;
      Temp_Data: StepData_Container.Vector;
      Action, Sub_Action, Sub_Sub_Action: Data_Action;
      Story_Node, Child_Node, Step_Node: Node;
      Delete_Index, Step_Index: Positive;
   begin
      Clear_Current_Story;
      Temp_Step :=
        (Index => Null_Unbounded_String, Finish_Condition => ASKINBASE,
         Finish_Data => Temp_Data, Fail_Text => Null_Unbounded_String,
         Texts => Temp_Texts);
      Start_Step := Null_Unbounded_String;
      Stories_Data := Get_Tree(Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Stories_Data, "story");
      Load_Stories_Loop :
      for I in 0 .. Length(Nodes_List) - 1 loop
         Temp_Record :=
           (Start_Condition => DROPITEM, Start_Data => Temp_Value,
            Min_Steps => 1, Max_Steps => 2, Starting_Step => Temp_Step,
            Steps => Temp_Steps, Final_Step => Temp_Step,
            End_Text => Null_Unbounded_String, Name => Null_Unbounded_String,
            Forbidden_Factions => Temp_Value);
         Story_Node := Item(Nodes_List, I);
         Story_Index := To_Unbounded_String(Get_Attribute(Story_Node, "index"));
         Action :=
           (if Get_Attribute(Story_Node, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(Story_Node, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Stories_Container.Contains(Stories_List, Story_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " story '" & To_String(Story_Index) &
                 "', there no story with that index.";
            end if;
         elsif Stories_Container.Contains(Stories_List, Story_Index) then
            raise Data_Loading_Error
              with "Can't add story '" & To_String(Story_Index) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Stories_List(Story_Index);
               Start_Step := Null_Unbounded_String;
               Final_Step := Null_Unbounded_String;
            end if;
            if Get_Attribute(Story_Node, "startstep")'Length > 0 then
               Start_Step :=
                 To_Unbounded_String(Get_Attribute(Story_Node, "startstep"));
            end if;
            if Get_Attribute(Story_Node, "finalstep")'Length > 0 then
               Final_Step :=
                 To_Unbounded_String(Get_Attribute(Story_Node, "finalstep"));
            end if;
            if Get_Attribute(Story_Node, "start")'Length > 0 then
               Temp_Record.Start_Condition :=
                 Start_Condition_Type'Value(Get_Attribute(Story_Node, "start"));
            end if;
            if Get_Attribute(Story_Node, "minsteps")'Length > 0 then
               Temp_Record.Min_Steps :=
                 Positive'Value(Get_Attribute(Story_Node, "minsteps"));
            end if;
            if Get_Attribute(Story_Node, "maxsteps")'Length > 0 then
               Temp_Record.Max_Steps :=
                 Positive'Value(Get_Attribute(Story_Node, "maxsteps"));
            end if;
            if Get_Attribute(Story_Node, "name")'Length > 0 then
               Temp_Record.Name :=
                 To_Unbounded_String(Get_Attribute(Story_Node, "name"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Story_Node, "startdata");
            Load_Start_Data_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Child_Node := Item(Child_Nodes, J);
               Value := To_Unbounded_String(Get_Attribute(Child_Node, "value"));
               Sub_Action :=
                 (if Get_Attribute(Child_Node, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(Child_Node, "action"))
                  else ADD);
               case Sub_Action is
                  when ADD =>
                     Temp_Record.Start_Data.Append(New_Item => Value);
                  when REMOVE =>
                     Find_Delete_Start_Index_Loop :
                     for K in Temp_Record.Start_Data.Iterate loop
                        if Temp_Record.Start_Data(K) = Value then
                           Delete_Index :=
                             UnboundedString_Container.To_Index(K);
                           exit Find_Delete_Start_Index_Loop;
                        end if;
                     end loop Find_Delete_Start_Index_Loop;
                     Temp_Record.Start_Data.Delete(Index => Delete_Index);
                  when UPDATE =>
                     null;
               end case;
            end loop Load_Start_Data_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Story_Node, "forbiddenfaction");
            Load_Forbidden_Faction_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Child_Node := Item(Child_Nodes, J);
               Value := To_Unbounded_String(Get_Attribute(Child_Node, "value"));
               Sub_Action :=
                 (if Get_Attribute(Child_Node, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(Child_Node, "action"))
                  else ADD);
               case Sub_Action is
                  when ADD =>
                     Temp_Record.Forbidden_Factions.Append(New_Item => Value);
                  when REMOVE =>
                     Find_Delete_Forbidden_Index_Loop :
                     for K in Temp_Record.Forbidden_Factions.Iterate loop
                        if Temp_Record.Forbidden_Factions(K) = Value then
                           Delete_Index :=
                             UnboundedString_Container.To_Index(K);
                           exit Find_Delete_Forbidden_Index_Loop;
                        end if;
                     end loop Find_Delete_Forbidden_Index_Loop;
                     Temp_Record.Forbidden_Factions.Delete
                       (Index => Delete_Index);
                  when UPDATE =>
                     null;
               end case;
            end loop Load_Forbidden_Faction_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(Story_Node, "step");
            Load_Steps_Data_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Temp_Step :=
                 (Index => Null_Unbounded_String,
                  Finish_Condition => ASKINBASE, Finish_Data => Temp_Data,
                  Fail_Text => Null_Unbounded_String, Texts => Temp_Texts);
               Child_Node := Item(Child_Nodes, J);
               Temp_Step.Index :=
                 To_Unbounded_String(Get_Attribute(Child_Node, "index"));
               Sub_Action :=
                 (if Get_Attribute(Child_Node, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(Child_Node, "action"))
                  else ADD);
               Find_Step_Index_Loop :
               for K in Temp_Record.Steps.Iterate loop
                  if Temp_Record.Steps(K).Index = Temp_Step.Index then
                     Step_Index := Steps_Container.To_Index(K);
                     exit Find_Step_Index_Loop;
                  end if;
               end loop Find_Step_Index_Loop;
               if Sub_Action /= REMOVE then
                  if Sub_Action = UPDATE then
                     Temp_Step := Temp_Record.Steps(Step_Index);
                  end if;
                  if Get_Attribute(Child_Node, "finish")'Length > 0 then
                     Temp_Step.Finish_Condition :=
                       Step_Condition_Type'Value
                         (Get_Attribute(Child_Node, "finish"));
                  end if;
                  Step_Data_Nodes :=
                    DOM.Core.Elements.Get_Elements_By_Tag_Name
                      (Child_Node, "finishdata");
                  Load_Finish_Data_Loop :
                  for K in 0 .. Length(Step_Data_Nodes) - 1 loop
                     Step_Node := Item(Step_Data_Nodes, K);
                     Sub_Sub_Action :=
                       (if Get_Attribute(Step_Node, "action")'Length > 0 then
                          Data_Action'Value(Get_Attribute(Step_Node, "action"))
                        else ADD);
                     Value :=
                       To_Unbounded_String(Get_Attribute(Step_Node, "name"));
                     case Sub_Sub_Action is
                        when ADD =>
                           Temp_Step.Finish_Data.Append
                             (New_Item =>
                                (Name => Value,
                                 Value =>
                                   To_Unbounded_String
                                     (Get_Attribute(Step_Node, "value"))));
                        when UPDATE =>
                           for Data of Temp_Step.Finish_Data loop
                              if Data.Name = Value then
                                 Data.Value :=
                                   To_Unbounded_String
                                     (Get_Attribute(Step_Node, "value"));
                                 exit;
                              end if;
                           end loop;
                        when REMOVE =>
                           Find_Delete_Finish_Index_Loop :
                           for L in Temp_Step.Finish_Data.Iterate loop
                              if Temp_Step.Finish_Data(L).Name = Value then
                                 Delete_Index := StepData_Container.To_Index(L);
                                 exit Find_Delete_Finish_Index_Loop;
                              end if;
                           end loop Find_Delete_Finish_Index_Loop;
                           Temp_Step.Finish_Data.Delete(Index => Delete_Index);
                     end case;
                  end loop Load_Finish_Data_Loop;
                  Step_Data_Nodes :=
                    DOM.Core.Elements.Get_Elements_By_Tag_Name
                      (Item(Child_Nodes, J), "text");
                  Load_Step_Text_Loop :
                  for K in 0 .. Length(Step_Data_Nodes) - 1 loop
                     Step_Node := Item(Step_Data_Nodes, K);
                     Sub_Sub_Action :=
                       (if Get_Attribute(Step_Node, "action")'Length > 0 then
                          Data_Action'Value(Get_Attribute(Step_Node, "action"))
                        else ADD);
                     Value :=
                       To_Unbounded_String
                         (Get_Attribute(Step_Node, "condition"));
                     case Sub_Sub_Action is
                        when ADD =>
                           Temp_Step.Texts.Append
                             (New_Item =>
                                (Condition =>
                                   Step_Condition_Type'Value(To_String(Value)),
                                 Text =>
                                   To_Unbounded_String
                                     (Node_Value(First_Child(Step_Node)))));
                        when UPDATE =>
                           Load_Update_Text_Loop :
                           for Text of Temp_Step.Texts loop
                              if Text.Condition =
                                Step_Condition_Type'Value
                                  (To_String(Value)) then
                                 Text.Text :=
                                   To_Unbounded_String
                                     (Node_Value(First_Child(Step_Node)));
                                 exit Load_Update_Text_Loop;
                              end if;
                           end loop Load_Update_Text_Loop;
                        when REMOVE =>
                           Find_Delete_Text_Index_Loop :
                           for L in Temp_Step.Texts.Iterate loop
                              if Temp_Step.Texts(L).Condition =
                                Step_Condition_Type'Value
                                  (To_String(Value)) then
                                 Delete_Index :=
                                   StepTexts_Container.To_Index(L);
                                 exit Find_Delete_Text_Index_Loop;
                              end if;
                           end loop Find_Delete_Text_Index_Loop;
                           Temp_Step.Texts.Delete(Index => Delete_Index);
                     end case;
                  end loop Load_Step_Text_Loop;
                  Step_Data_Nodes :=
                    DOM.Core.Elements.Get_Elements_By_Tag_Name
                      (Item(Child_Nodes, J), "failtext");
                  if Length(Step_Data_Nodes) > 0 then
                     Temp_Step.Fail_Text :=
                       To_Unbounded_String
                         (Node_Value(First_Child(Item(Step_Data_Nodes, 0))));
                  end if;
                  if Temp_Step.Index = Start_Step then
                     Temp_Record.Starting_Step := Temp_Step;
                  elsif Temp_Step.Index = Final_Step then
                     Temp_Record.Final_Step := Temp_Step;
                  else
                     if Sub_Action = ADD then
                        Temp_Record.Steps.Append(New_Item => Temp_Step);
                     else
                        Temp_Record.Steps(Step_Index) := Temp_Step;
                     end if;
                  end if;
               else
                  Temp_Record.Steps.Delete(Index => Step_Index);
               end if;
            end loop Load_Steps_Data_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(Story_Node, "endtext");
            if Length(Child_Nodes) > 0 then
               Temp_Record.End_Text :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(Child_Nodes, 0))));
            end if;
            if Action /= UPDATE then
               Stories_Container.Include(Stories_List, Story_Index, Temp_Record);
               Log_Message
                 ("Story added: " & To_String(Story_Index), EVERYTHING);
            else
               Stories_List(Story_Index) := Temp_Record;
               Log_Message
                 ("Story updated: " & To_String(Story_Index), EVERYTHING);
            end if;
         else
            Stories_Container.Exclude(Stories_List, Story_Index);
            Log_Message("Story removed: " & To_String(Story_Index), EVERYTHING);
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
                          SelectEnemy
                            (Stories_List(I).Starting_Step.Finish_Data);
                     when EXPLORE =>
                        StepData :=
                          SelectLocation
                            (Stories_List(I).Starting_Step.Finish_Data);
                     when LOOT =>
                        StepData :=
                          SelectLoot
                            (Stories_List(I).Starting_Step.Finish_Data);
                     when ANY =>
                        null;
                  end case;
                  Current_Story :=
                    (Index => Stories_Container.Key(I), Step => 1,
                     Current_Step => 0,
                     Max_Steps =>
                       Get_Random
                         (Stories_List(I).Min_Steps,
                          Stories_List(I).Max_Steps),
                     Show_Text => True, Data => StepData,
                     Finished_Step => ANY);
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
            FinishedStory.Steps_Texts.Append
              (New_Item => Get_Current_Story_Text);
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
                 SelectBase
                   (To_String(Get_Step_Data(Step.Finish_Data, "base")));
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
