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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Log; use Log;
with Factions; use Factions;
with Utils; use Utils;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Bases; use Bases;
with Events; use Events;
with Maps; use Maps;

package body Stories is

   procedure LoadStories is
      TempRecord: Story_Data;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      StoriesFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes, StepDataNodes: Node_List;
      StoriesData: Document;
      TempValue: UnboundedString_Container.Vector;
      TempStep: Step_Data;
      TempSteps: Steps_Container.Vector;
      StartStep, FinalStep: Unbounded_String;
      TempTexts: StepTexts_Container.Vector;
   begin
      ClearCurrentStory;
      if Stories_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "stories" & Dir_Separator) then
         raise Stories_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "stories" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Stories_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempStep :=
           (Index => Null_Unbounded_String,
            FinishCondition => ASKINBASE,
            FinishData => TempValue,
            FailText => Null_Unbounded_String,
            Texts => TempTexts);
         TempRecord :=
           (Index => Null_Unbounded_String,
            StartCondition => DROPITEM,
            StartData => TempValue,
            MinSteps => 1,
            MaxSteps => 2,
            StartingStep => TempStep,
            Steps => TempSteps,
            FinalStep => TempStep,
            EndText => Null_Unbounded_String,
            Name => Null_Unbounded_String);
         StartStep := Null_Unbounded_String;
         LogMessage
           ("Loading stories file: " & Full_Name(FoundFile),
            Everything);
         Open(Full_Name(FoundFile), StoriesFile);
         Parse(Reader, StoriesFile);
         Close(StoriesFile);
         StoriesData := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(StoriesData, "story");
         for I in 0 .. Length(NodesList) - 1 loop
            StartStep :=
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "startstep"));
            FinalStep :=
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "finalstep"));
            TempRecord.Index :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
            TempRecord.StartCondition :=
              StartConditionType'Value
                (Get_Attribute(Item(NodesList, I), "start"));
            TempRecord.MinSteps :=
              Positive'Value(Get_Attribute(Item(NodesList, I), "minsteps"));
            TempRecord.MaxSteps :=
              Positive'Value(Get_Attribute(Item(NodesList, I), "maxsteps"));
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name"));
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "startdata");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.StartData.Append
               (New_Item =>
                  To_Unbounded_String
                    (Get_Attribute(Item(ChildNodes, J), "value")));
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "step");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempStep :=
                 (Index => Null_Unbounded_String,
                  FinishCondition => ASKINBASE,
                  FinishData => TempValue,
                  FailText => Null_Unbounded_String,
                  Texts => TempTexts);
               TempStep.Index :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index"));
               TempStep.FinishCondition :=
                 StepConditionType'Value
                   (Get_Attribute(Item(ChildNodes, J), "finish"));
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J),
                    "finishdata");
               for K in 0 .. Length(StepDataNodes) - 1 loop
                  TempStep.FinishData.Append
                  (New_Item =>
                     To_Unbounded_String
                       (Get_Attribute(Item(StepDataNodes, K), "value")));
               end loop;
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J),
                    "text");
               for K in 0 .. Length(StepDataNodes) - 1 loop
                  TempStep.Texts.Append
                  (New_Item =>
                     (Condition =>
                        StepConditionType'Value
                          (Get_Attribute(Item(StepDataNodes, K), "condition")),
                      Text =>
                        To_Unbounded_String
                          (Node_Value(First_Child(Item(StepDataNodes, K))))));
               end loop;
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J),
                    "failtext");
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
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "endtext");
            TempRecord.EndText :=
              To_Unbounded_String
                (Node_Value(First_Child(Item(ChildNodes, 0))));
            Stories_List.Append(New_Item => TempRecord);
            LogMessage
              ("Story added: " & To_String(TempRecord.Index),
               Everything);
            TempRecord :=
              (Index => Null_Unbounded_String,
               StartCondition => DROPITEM,
               StartData => TempValue,
               MinSteps => 1,
               MaxSteps => 2,
               StartingStep => TempStep,
               Steps => TempSteps,
               FinalStep => TempStep,
               EndText => Null_Unbounded_String,
               Name => Null_Unbounded_String);
         end loop;
         Free(Reader);
      end loop;
      End_Search(Files);
   end LoadStories;

   function SelectBase(Value: String) return Unbounded_String is
      BaseIndex: Positive;
   begin
      if Value = "any" then
         return Null_Unbounded_String;
      end if;
      loop
         BaseIndex := GetRandom(SkyBases'First, SkyBases'Last);
         if SkyBases(BaseIndex).Known then
            PlayerShip.DestinationX := SkyBases(BaseIndex).SkyX;
            PlayerShip.DestinationY := SkyBases(BaseIndex).SkyY;
            return SkyBases(BaseIndex).Name;
         end if;
      end loop;
   end SelectBase;

   function SelectEnemy
     (StepData: UnboundedString_Container.Vector) return Unbounded_String is
      Enemies: Positive_Container.Vector;
      EnemyData: Unbounded_String := Null_Unbounded_String;
      Coord: Integer;
   begin
      if StepData(3) = To_Unbounded_String("random") then
         Coord := GetRandom(SkyMap'First, SkyMap'Last);
         EnemyData := To_Unbounded_String(Integer'Image(Coord));
         Append(EnemyData, ";");
      else
         Coord := Integer'Value(To_String(StepData(3)));
         EnemyData := StepData(3);
         Append(EnemyData, ";");
      end if;
      PlayerShip.DestinationX := Coord;
      if StepData(4) = To_Unbounded_String("random") then
         Coord := GetRandom(SkyMap'First, SkyMap'Last);
         Append(EnemyData, Integer'Image(Coord));
         Append(EnemyData, ";");
      else
         Coord := Integer'Value(To_String(StepData(4)));
         Append(EnemyData, StepData(4));
         Append(EnemyData, ";");
      end if;
      PlayerShip.DestinationY := Coord;
      if StepData(2) /= To_Unbounded_String("random") then
         return EnemyData & StepData(2);
      end if;
      GenerateEnemies(Enemies, StepData(1));
      return EnemyData &
        To_Unbounded_String
          (Integer'Image
             (Enemies(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
   end SelectEnemy;

   procedure StartStory
     (FactionName: Unbounded_String;
      Condition: StartConditionType) is
      FactionIndex, StepData: Unbounded_String := Null_Unbounded_String;
   begin
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
         case Condition is
            when DROPITEM =>
               if Stories_List(I).StartData(2) = FactionIndex then
                  if GetRandom
                      (1,
                       Positive'Value
                         (To_String(Stories_List(I).StartData(3)))) =
                    1 then
                     case Stories_List(I).StartingStep.FinishCondition is
                        when ASKINBASE =>
                           StepData :=
                             SelectBase
                               (To_String
                                  (Stories_List(I).StartingStep.FinishData
                                     (3)));
                        when DESTROYSHIP =>
                           StepData :=
                             SelectEnemy
                               (Stories_List(I).StartingStep.FinishData);
                        when ANY =>
                           null;
                     end case;
                     CurrentStory :=
                       (Index => Stories_Container.To_Index(I),
                        Step => 1,
                        CurrentStep => 0,
                        MaxSteps =>
                          GetRandom
                            (Stories_List(I).MinSteps,
                             Stories_List(I).MaxSteps),
                        ShowText => True,
                        Data => StepData,
                        FinishedStep => ANY);
                     UpdateCargo
                       (PlayerShip,
                        Positive'Value
                          (To_String(Stories_List(I).StartData(1))),
                        1);
                     return;
                  end if;
               end if;
         end case;
      end loop;
   end StartStory;

   procedure ClearCurrentStory is
   begin
      CurrentStory :=
        (Index => 0,
         Step => 1,
         CurrentStep => -3,
         MaxSteps => 1,
         ShowText => False,
         Data => Null_Unbounded_String,
         FinishedStep => ANY);
   end ClearCurrentStory;

   function ProgressStory(NextStep: Boolean := False) return Boolean is
      Step: Step_Data;
      MaxRandom: Positive;
   begin
      if CurrentStory.CurrentStep = 0 then
         Step := Stories_List(CurrentStory.Index).StartingStep;
      elsif CurrentStory.CurrentStep > 0 then
         Step :=
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep);
      else
         Step := Stories_List(CurrentStory.Index).FinalStep;
      end if;
      case Step.FinishCondition is
         when ASKINBASE =>
            MaxRandom := Positive'Value(To_String(Step.FinishData(2)));
         when DESTROYSHIP =>
            MaxRandom := Positive'Value(To_String(Step.FinishData(5)));
            if NextStep then
               MaxRandom := 1;
            end if;
         when others =>
            null;
      end case;
      if GetRandom(1, MaxRandom) > 1 then
         UpdateGame(10);
         return False;
      end if;
      if Step.FinishCondition = DESTROYSHIP and not NextStep then
         return True;
      end if;
      UpdateGame(30);
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
               CurrentStory.Data := SelectBase(To_String(Step.FinishData(3)));
            when DESTROYSHIP =>
               CurrentStory.Data := SelectEnemy(Step.FinishData);
            when ANY =>
               null;
         end case;
      end if;
      return True;
   end ProgressStory;

end Stories;
