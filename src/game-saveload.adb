--    Copyright 2017-2019 Bartek thindil Jasicki
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Bases; use Bases;
with Bases.SaveLoad; use Bases.SaveLoad;
with Maps; use Maps;
with Ships; use Ships;
with Ships.SaveLoad; use Ships.SaveLoad;
with Messages; use Messages;
with Crafts; use Crafts;
with Events; use Events;
with Statistics; use Statistics;
with Goals; use Goals;
with Config; use Config;
with Stories; use Stories;
with Log; use Log;
with Missions; use Missions;
with Utils; use Utils;
with Careers; use Careers;

package body Game.SaveLoad is

   SaveData: Document;

   procedure SaveGame(PrettyPrint: Boolean := False) is
      Save: DOM_Implementation;
      CategoryNode, MainNode: DOM.Core.Element;
      RawValue: Unbounded_String;
      SaveFile: File_Type;
      procedure SaveStatistics
        (StatisticsVector: in out Statistics_Container.Vector;
         StatName: String) is
         StatNode: DOM.Core.Element;
      begin
         for Statistic of StatisticsVector loop
            StatNode := Create_Element(SaveData, StatName);
            StatNode := Append_Child(CategoryNode, StatNode);
            Set_Attribute(StatNode, "index", To_String(Statistic.Index));
            RawValue := To_Unbounded_String(Integer'Image(Statistic.Amount));
            Set_Attribute
              (StatNode, "amount",
               To_String(Trim(RawValue, Ada.Strings.Left)));
         end loop;
      end SaveStatistics;
      type Difficulty_Data is record
         Name: Unbounded_String;
         Value: Float;
      end record;
      Difficulties: constant array(Positive range <>) of Difficulty_Data :=
        ((To_Unbounded_String("enemydamagebonus"),
          NewGameSettings.EnemyDamageBonus),
         (To_Unbounded_String("playerdamagebonus"),
          NewGameSettings.PlayerDamageBonus),
         (To_Unbounded_String("enemymeleedamagebonus"),
          NewGameSettings.EnemyMeleeDamageBonus),
         (To_Unbounded_String("playermeleedamagebonus"),
          NewGameSettings.PlayerMeleeDamageBonus),
         (To_Unbounded_String("experiencebonus"),
          NewGameSettings.ExperienceBonus),
         (To_Unbounded_String("reputationbonus"),
          NewGameSettings.ReputationBonus),
         (To_Unbounded_String("upgradecostbonus"),
          NewGameSettings.UpgradeCostBonus));
   begin
      LogMessage
        ("Start saving game in file " & To_String(SaveName) & ".", Everything);
      SaveData := Create_Document(Save);
      MainNode := Create_Element(SaveData, "save");
      MainNode := Append_Child(SaveData, MainNode);
      -- Save game difficulty settings
      LogMessage("Saving game difficulty settings...", Everything, False);
      CategoryNode := Create_Element(SaveData, "difficulty");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      for Difficulty of Difficulties loop
         RawValue := To_Unbounded_String(Float'Image(Difficulty.Value));
         Set_Attribute
           (CategoryNode, To_String(Difficulty.Name),
            To_String(Trim(RawValue, Ada.Strings.Left)));
      end loop;
      LogMessage("done.", Everything, True, False);
      -- Save game date
      LogMessage("Saving game time...", Everything, False);
      CategoryNode := Create_Element(SaveData, "gamedate");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Year));
      Set_Attribute
        (CategoryNode, "year", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Month));
      Set_Attribute
        (CategoryNode, "month", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Day));
      Set_Attribute
        (CategoryNode, "day", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Hour));
      Set_Attribute
        (CategoryNode, "hour", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Minutes));
      Set_Attribute
        (CategoryNode, "minutes", To_String(Trim(RawValue, Ada.Strings.Left)));
      LogMessage("done.", Everything, True, False);
      -- Save map
      LogMessage("Saving map...", Everything, False);
      declare
         FieldNode: DOM.Core.Element;
      begin
         for X in SkyMap'Range loop
            for Y in 1 .. 1024 loop
               if SkyMap(X, Y).Visited then
                  FieldNode := Create_Element(SaveData, "field");
                  FieldNode := Append_Child(MainNode, FieldNode);
                  RawValue := To_Unbounded_String(Integer'Image(X));
                  Set_Attribute
                    (FieldNode, "x",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  RawValue := To_Unbounded_String(Integer'Image(Y));
                  Set_Attribute
                    (FieldNode, "y",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               end if;
            end loop;
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Save bases
      LogMessage("Saving bases...", Everything, False);
      SaveBases(SaveData, MainNode);
      LogMessage("done.", Everything, True, False);
      -- Save player ship
      LogMessage("Saving player ship...", Everything, False);
      SavePlayerShip(SaveData, MainNode);
      LogMessage("done.", Everything, True, False);
      -- Save known recipes
      LogMessage("Saving known recipes...", Everything, False);
      declare
         RecipeNode: DOM.Core.Element;
      begin
         for Recipe of Known_Recipes loop
            RecipeNode := Create_Element(SaveData, "recipe");
            RecipeNode := Append_Child(MainNode, RecipeNode);
            Set_Attribute(RecipeNode, "index", To_String(Recipe));
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Save messages
      LogMessage("Saving messages...", Everything, False);
      declare
         Messages: Natural := GameSettings.SavedMessages;
         StartLoop: Positive;
         MessageNode: DOM.Core.Element;
         Message: Message_Data;
         MessageText: Text;
      begin
         if Messages > MessagesAmount then
            Messages := MessagesAmount;
         end if;
         if Messages > 0 then
            StartLoop := MessagesAmount - Messages + 1;
            for I in StartLoop .. MessagesAmount loop
               Message := GetMessage(I);
               MessageNode := Create_Element(SaveData, "message");
               MessageNode := Append_Child(MainNode, MessageNode);
               RawValue :=
                 To_Unbounded_String
                   (Integer'Image(Message_Type'Pos(Message.MType)));
               Set_Attribute
                 (MessageNode, "type",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               RawValue :=
                 To_Unbounded_String
                   (Integer'Image(Message_Color'Pos(Message.Color)));
               Set_Attribute
                 (MessageNode, "color",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               MessageText :=
                 Create_Text_Node(SaveData, To_String(Message.Message));
               MessageText := Append_Child(MessageNode, MessageText);
            end loop;
         end if;
      end;
      LogMessage("done.", Everything, True, False);
      -- Save events
      LogMessage("Saving events...", Everything, False);
      declare
         EventNode: DOM.Core.Element;
      begin
         for Event of Events_List loop
            EventNode := Create_Element(SaveData, "event");
            EventNode := Append_Child(MainNode, EventNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Events_Types'Pos(Event.EType)));
            Set_Attribute
              (EventNode, "type", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.SkyX));
            Set_Attribute
              (EventNode, "x", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.SkyY));
            Set_Attribute
              (EventNode, "y", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.Time));
            Set_Attribute
              (EventNode, "time", To_String(Trim(RawValue, Ada.Strings.Left)));
            case Event.EType is
               when DoublePrice =>
                  RawValue := Event.ItemIndex;
               when AttackOnBase | EnemyShip | EnemyPatrol | Trader |
                 FriendlyShip =>
                  RawValue := Event.ShipIndex;
               when others =>
                  RawValue := To_Unbounded_String(Integer'Image(Event.Data));
            end case;
            Set_Attribute
              (EventNode, "data", To_String(Trim(RawValue, Ada.Strings.Left)));
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Save game statistics
      LogMessage("Saving game statistics...", Everything, False);
      CategoryNode := Create_Element(SaveData, "statistics");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      SaveStatistics(GameStats.DestroyedShips, "destroyedships");
      RawValue := To_Unbounded_String(Positive'Image(GameStats.BasesVisited));
      Set_Attribute
        (CategoryNode, "visitedbases",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Positive'Image(GameStats.MapVisited));
      Set_Attribute
        (CategoryNode, "mapdiscovered",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.DistanceTraveled));
      Set_Attribute
        (CategoryNode, "distancetraveled",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      SaveStatistics(GameStats.CraftingOrders, "finishedcrafts");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.AcceptedMissions));
      Set_Attribute
        (CategoryNode, "acceptedmissions",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      SaveStatistics(GameStats.FinishedMissions, "finishedmissions");
      SaveStatistics(GameStats.FinishedGoals, "finishedgoals");
      SaveStatistics(GameStats.KilledMobs, "killedmobs");
      RawValue := To_Unbounded_String(Natural'Image(GameStats.Points));
      Set_Attribute
        (CategoryNode, "points", To_String(Trim(RawValue, Ada.Strings.Left)));
      LogMessage("done.", Everything, True, False);
      -- Save current goal
      LogMessage("Saving current goal...", Everything, False);
      CategoryNode := Create_Element(SaveData, "currentgoal");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "index", To_String(CurrentGoal.Index));
      RawValue :=
        To_Unbounded_String(Integer'Image(GoalTypes'Pos(CurrentGoal.GType)));
      Set_Attribute
        (CategoryNode, "type", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Natural'Image(CurrentGoal.Amount));
      Set_Attribute
        (CategoryNode, "amount", To_String(Trim(RawValue, Ada.Strings.Left)));
      Set_Attribute
        (CategoryNode, "target", To_String(CurrentGoal.TargetIndex));
      LogMessage("done.", Everything, True, False);
      -- Save current story
      if CurrentStory.Index /= Null_Unbounded_String then
         LogMessage("Saving current story...", Everything, False);
         CategoryNode := Create_Element(SaveData, "currentstory");
         CategoryNode := Append_Child(MainNode, CategoryNode);
         Set_Attribute(CategoryNode, "index", To_String(CurrentStory.Index));
         RawValue := To_Unbounded_String(Positive'Image(CurrentStory.Step));
         Set_Attribute
           (CategoryNode, "step", To_String(Trim(RawValue, Ada.Strings.Left)));
         if CurrentStory.CurrentStep = 0 then
            Set_Attribute(CategoryNode, "currentstep", "start");
         elsif CurrentStory.CurrentStep = -1 then
            Set_Attribute(CategoryNode, "currentstep", "finish");
         else
            Set_Attribute
              (CategoryNode, "currentstep",
               To_String
                 (Stories_List(CurrentStory.Index).Steps
                    (CurrentStory.CurrentStep)
                    .Index));
         end if;
         RawValue :=
           To_Unbounded_String(Positive'Image(CurrentStory.MaxSteps));
         Set_Attribute
           (CategoryNode, "maxsteps",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         if CurrentStory.ShowText then
            Set_Attribute(CategoryNode, "showtext", "Y");
         else
            Set_Attribute(CategoryNode, "showtext", "N");
         end if;
         if CurrentStory.Data /= Null_Unbounded_String then
            Set_Attribute(CategoryNode, "data", To_String(CurrentStory.Data));
         end if;
         RawValue :=
           To_Unbounded_String
             (Integer'Image(StepConditionType'Pos(CurrentStory.FinishedStep)));
         Set_Attribute
           (CategoryNode, "finishedstep",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         LogMessage("done.", Everything, True, False);
      end if;
      -- Save finished stories data
      declare
         StepNode: DOM.Core.Element;
         StepText: Text;
      begin
         LogMessage("Saving finished stories...", Everything, False);
         for FinishedStory of FinishedStories loop
            CategoryNode := Create_Element(SaveData, "finishedstory");
            CategoryNode := Append_Child(MainNode, CategoryNode);
            Set_Attribute
              (CategoryNode, "index", To_String(FinishedStory.Index));
            RawValue :=
              To_Unbounded_String(Positive'Image(FinishedStory.StepsAmount));
            Set_Attribute
              (CategoryNode, "stepsamount",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            for Step of FinishedStory.StepsTexts loop
               StepNode := Create_Element(SaveData, "steptext");
               StepNode := Append_Child(CategoryNode, StepNode);
               StepText := Create_Text_Node(SaveData, To_String(Step));
               StepText := Append_Child(StepNode, StepText);
            end loop;
         end loop;
         LogMessage("done.", Everything, True, False);
      end;
      -- Save missions accepted by player
      for Mission of AcceptedMissions loop
         CategoryNode := Create_Element(SaveData, "acceptedmission");
         CategoryNode := Append_Child(MainNode, CategoryNode);
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Missions_Types'Pos(Mission.MType)));
         Set_Attribute
           (CategoryNode, "type", To_String(Trim(RawValue, Ada.Strings.Left)));
         if Mission.MType = Deliver then
            RawValue := Mission.ItemIndex;
         elsif Mission.MType = Passenger then
            RawValue := To_Unbounded_String(Integer'Image(Mission.Data));
         elsif Mission.MType = Destroy then
            RawValue := Mission.ShipIndex;
         else
            RawValue := To_Unbounded_String(Integer'Image(Mission.Target));
         end if;
         Set_Attribute
           (CategoryNode, "target",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue := To_Unbounded_String(Integer'Image(Mission.Time));
         Set_Attribute
           (CategoryNode, "time", To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue := To_Unbounded_String(Integer'Image(Mission.TargetX));
         Set_Attribute
           (CategoryNode, "targetx",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue := To_Unbounded_String(Integer'Image(Mission.TargetY));
         Set_Attribute
           (CategoryNode, "targety",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue := To_Unbounded_String(Integer'Image(Mission.Reward));
         Set_Attribute
           (CategoryNode, "reward",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue := To_Unbounded_String(Integer'Image(Mission.StartBase));
         Set_Attribute
           (CategoryNode, "startbase",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         if Mission.Finished then
            Set_Attribute(CategoryNode, "finished", "Y");
         else
            Set_Attribute(CategoryNode, "finished", "N");
         end if;
         if Mission.Multiplier /= 1.0 then
            RawValue :=
              To_Unbounded_String(RewardMultiplier'Image(Mission.Multiplier));
            Set_Attribute
              (CategoryNode, "multiplier",
               To_String(Trim(RawValue, Ada.Strings.Left)));
         end if;
      end loop;
      -- Save player career
      LogMessage("Saving player career...", Everything, False);
      CategoryNode := Create_Element(SaveData, "playercareer");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "index", To_String(PlayerCareer));
      LogMessage("done.", Everything, True, False);
      Create(SaveFile, Out_File, To_String(SaveName));
      Write
        (Stream => Stream(SaveFile), N => SaveData,
         Pretty_Print => PrettyPrint);
      Close(SaveFile);
      LogMessage("Finished saving game.", Everything);
   end SaveGame;

   procedure LoadGame is
      SaveFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes: Node_List;
      SavedNode: Node;
   begin
      LogMessage
        ("Start loading game from file " & To_String(SaveName) & ".",
         Everything);
      Open(To_String(SaveName), SaveFile);
      Parse(Reader, SaveFile);
      Close(SaveFile);
      SaveData := Get_Tree(Reader);
      -- Load game difficulty settings
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "difficulty");
      if Length(NodesList) > 0 then
         LogMessage("Loading game difficulty settings...", Everything, False);
         SavedNode := Item(NodesList, 0);
         NewGameSettings.EnemyDamageBonus :=
           Float'Value(Get_Attribute(SavedNode, "enemydamagebonus"));
         NewGameSettings.PlayerDamageBonus :=
           Float'Value(Get_Attribute(SavedNode, "playerdamagebonus"));
         NewGameSettings.EnemyMeleeDamageBonus :=
           Float'Value(Get_Attribute(SavedNode, "enemymeleedamagebonus"));
         NewGameSettings.PlayerMeleeDamageBonus :=
           Float'Value(Get_Attribute(SavedNode, "playermeleedamagebonus"));
         NewGameSettings.ExperienceBonus :=
           Float'Value(Get_Attribute(SavedNode, "experiencebonus"));
         NewGameSettings.ReputationBonus :=
           Float'Value(Get_Attribute(SavedNode, "reputationbonus"));
         NewGameSettings.UpgradeCostBonus :=
           Float'Value(Get_Attribute(SavedNode, "upgradecostbonus"));
         LogMessage("done.", Everything, True, False);
      end if;
      -- Load game date
      LogMessage("Loading game time...", Everything, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "gamedate");
      SavedNode := Item(NodesList, 0);
      GameDate.Year := Natural'Value(Get_Attribute(SavedNode, "year"));
      GameDate.Month := Natural'Value(Get_Attribute(SavedNode, "month"));
      GameDate.Day := Natural'Value(Get_Attribute(SavedNode, "day"));
      GameDate.Hour := Natural'Value(Get_Attribute(SavedNode, "hour"));
      GameDate.Minutes := Natural'Value(Get_Attribute(SavedNode, "minutes"));
      LogMessage("done.", Everything, True, False);
      -- Load sky map
      LogMessage("Loading map...", Everything, False);
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0, Visited => False, EventIndex => 0,
               MissionIndex => 0)));
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "field");
      declare
         X, Y: Positive;
      begin
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            X := Natural'Value(Get_Attribute(SavedNode, "x"));
            Y := Natural'Value(Get_Attribute(SavedNode, "y"));
            SkyMap(X, Y).Visited := True;
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Load sky bases
      LogMessage("Loading bases...", Everything, False);
      LoadBases(SaveData);
      LogMessage("done.", Everything, True, False);
      -- Load player ship
      LogMessage("Loading player ship...", Everything, False);
      LoadPlayerShip(SaveData);
      LogMessage("done.", Everything, True, False);
      -- Load known recipes
      LogMessage("Loading known recipes...", Everything, False);
      Known_Recipes.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "recipe");
      for I in 0 .. Length(NodesList) - 1 loop
         Known_Recipes.Append
           (New_Item =>
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index")));
      end loop;
      LogMessage("done.", Everything, True, False);
      -- Load messages
      LogMessage("Loading messages...", Everything, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "message");
      ClearMessages;
      declare
         Text: Unbounded_String;
         MType: Message_Type;
         Color: Message_Color;
      begin
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            Text := To_Unbounded_String(Node_Value(First_Child(SavedNode)));
            MType :=
              Message_Type'Val
                (Integer'Value(Get_Attribute(SavedNode, "type")));
            Color :=
              Message_Color'Val
                (Integer'Value(Get_Attribute(SavedNode, "color")));
            RestoreMessage(Text, MType, Color);
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Load events
      LogMessage("Loading events...", Everything, False);
      Events_List.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "event");
      declare
         EType: Events_Types;
         X, Y, Time: Integer;
         Data: Unbounded_String;
      begin
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            EType :=
              Events_Types'Val
                (Integer'Value(Get_Attribute(SavedNode, "type")));
            X := Integer'Value(Get_Attribute(SavedNode, "x"));
            Y := Integer'Value(Get_Attribute(SavedNode, "y"));
            Time := Integer'Value(Get_Attribute(SavedNode, "time"));
            Data := To_Unbounded_String(Get_Attribute(SavedNode, "data"));
            case EType is
               when EnemyShip =>
                  Events_List.Append
                    (New_Item =>
                       (EType => EnemyShip, SkyX => X, SkyY => Y, Time => Time,
                        ShipIndex => Data));
               when AttackOnBase =>
                  Events_List.Append
                    (New_Item =>
                       (EType => AttackOnBase, SkyX => X, SkyY => Y,
                        Time => Time, ShipIndex => Data));
               when Disease =>
                  Events_List.Append
                    (New_Item =>
                       (EType => Disease, SkyX => X, SkyY => Y, Time => Time,
                        Data => Integer'Value(To_String(Data))));
               when DoublePrice =>
                  Events_List.Append
                    (New_Item =>
                       (EType => DoublePrice, SkyX => X, SkyY => Y,
                        Time => Time, ItemIndex => Data));
               when FullDocks =>
                  Events_List.Append
                    (New_Item =>
                       (EType => FullDocks, SkyX => X, SkyY => Y, Time => Time,
                        Data => Integer'Value(To_String(Data))));
               when EnemyPatrol =>
                  Events_List.Append
                    (New_Item =>
                       (EType => EnemyPatrol, SkyX => X, SkyY => Y,
                        Time => Time, ShipIndex => Data));
               when Trader =>
                  Events_List.Append
                    (New_Item =>
                       (EType => Trader, SkyX => X, SkyY => Y, Time => Time,
                        ShipIndex => Data));
               when FriendlyShip =>
                  Events_List.Append
                    (New_Item =>
                       (EType => FriendlyShip, SkyX => X, SkyY => Y,
                        Time => Time, ShipIndex => Data));
               when None | BaseRecovery =>
                  null;
            end case;
            SkyMap(Events_List(I + 1).SkyX, Events_List(I + 1).SkyY)
              .EventIndex :=
              I + 1;
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Load game statistics
      LogMessage("Loading game statistics...", Everything, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "statistics");
      declare
         StatIndex, NodeName: Unbounded_String;
         StatAmount: Positive;
      begin
         SavedNode := Item(NodesList, 0);
         GameStats.BasesVisited :=
           Positive'Value(Get_Attribute(SavedNode, "visitedbases"));
         GameStats.MapVisited :=
           Positive'Value(Get_Attribute(SavedNode, "mapdiscovered"));
         GameStats.DistanceTraveled :=
           Positive'Value(Get_Attribute(SavedNode, "distancetraveled"));
         GameStats.AcceptedMissions :=
           Natural'Value(Get_Attribute(SavedNode, "acceptedmissions"));
         GameStats.Points :=
           Positive'Value(Get_Attribute(SavedNode, "points"));
         ChildNodes := Child_Nodes(SavedNode);
         for I in 0 .. Length(ChildNodes) - 1 loop
            NodeName := To_Unbounded_String(Node_Name(Item(ChildNodes, I)));
            if To_String(NodeName) /= "#text" then
               StatIndex :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, I), "index"));
               StatAmount :=
                 Positive'Value(Get_Attribute(Item(ChildNodes, I), "amount"));
            end if;
            if To_String(NodeName) = "destroyedships" then
               GameStats.DestroyedShips.Append
                 (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif To_String(NodeName) = "finishedcrafts" then
               GameStats.CraftingOrders.Append
                 (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif To_String(NodeName) = "finishedmissions" then
               GameStats.FinishedMissions.Append
                 (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif To_String(NodeName) = "finishedgoals" then
               GameStats.FinishedGoals.Append
                 (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif To_String(NodeName) = "killedmobs" then
               GameStats.KilledMobs.Append
                 (New_Item => (Index => StatIndex, Amount => StatAmount));
            end if;
         end loop;
      end;
      LogMessage("done.", Everything, True, False);
      -- Load current goal
      LogMessage("Loading current goal...", Everything, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "currentgoal");
      CurrentGoal.Index :=
        To_Unbounded_String(Get_Attribute(Item(NodesList, 0), "index"));
      CurrentGoal.GType :=
        GoalTypes'Val
          (Integer'Value(Get_Attribute(Item(NodesList, 0), "type")));
      CurrentGoal.Amount :=
        Integer'Value(Get_Attribute(Item(NodesList, 0), "amount"));
      CurrentGoal.TargetIndex :=
        To_Unbounded_String(Get_Attribute(Item(NodesList, 0), "target"));
      LogMessage("done.", Everything, True, False);
      -- Load current story
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "currentstory");
      if Length(NodesList) > 0 then
         LogMessage("Loading current story...", Everything, False);
         SavedNode := Item(NodesList, 0);
         CurrentStory.Index :=
           To_Unbounded_String(Get_Attribute(SavedNode, "index"));
         CurrentStory.Step := Positive'Value(Get_Attribute(SavedNode, "step"));
         if Get_Attribute(SavedNode, "currentstep") = "start" then
            CurrentStory.CurrentStep := 0;
         elsif Get_Attribute(SavedNode, "currentstep") = "finish" then
            CurrentStory.CurrentStep := -1;
         else
            for I in Stories_List(CurrentStory.Index).Steps.Iterate loop
               if Stories_List(CurrentStory.Index).Steps(I).Index =
                 To_Unbounded_String
                   (Get_Attribute(SavedNode, "currentstep")) then
                  CurrentStory.CurrentStep := Steps_Container.To_Index(I);
                  exit;
               end if;
            end loop;
         end if;
         CurrentStory.MaxSteps :=
           Positive'Value(Get_Attribute(SavedNode, "maxsteps"));
         if Get_Attribute(SavedNode, "showtext") = "Y" then
            CurrentStory.ShowText := True;
         else
            CurrentStory.ShowText := False;
         end if;
         if Get_Attribute(SavedNode, "data") /= "" then
            CurrentStory.Data :=
              To_Unbounded_String(Get_Attribute(SavedNode, "data"));
         end if;
         CurrentStory.FinishedStep :=
           StepConditionType'Val
             (Integer'Value(Get_Attribute(SavedNode, "finishedstep")));
         LogMessage("done.", Everything, True, False);
      end if;
      -- Load finished stories data
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "finishedstory");
      declare
         StepsAmount: Positive;
         TempTexts: UnboundedString_Container.Vector;
         StoryIndex: Unbounded_String;
      begin
         LogMessage("Loading finished stories...", Everything, False);
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            StoryIndex :=
              To_Unbounded_String(Get_Attribute(SavedNode, "index"));
            StepsAmount :=
              Positive'Value(Get_Attribute(SavedNode, "stepsamount"));
            TempTexts.Clear;
            ChildNodes := Child_Nodes(SavedNode);
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempTexts.Append
                 (New_Item =>
                    (To_Unbounded_String
                       (Node_Value(First_Child(Item(ChildNodes, J))))));
            end loop;
            FinishedStories.Append
              (New_Item =>
                 (Index => StoryIndex, StepsAmount => StepsAmount,
                  StepsTexts => TempTexts));
         end loop;
         LogMessage("done.", Everything, True, False);
      end;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (SaveData, "acceptedmission");
      declare
         MType: Missions_Types;
         TargetX, TargetY, StartBase: Natural;
         Time, Reward, MIndex: Positive;
         Finished: Boolean;
         Target: Natural;
         Index: Unbounded_String;
         Multiplier: RewardMultiplier;
      begin
         LogMessage("Loading accepted missions...", Everything, False);
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            MType :=
              Missions_Types'Val
                (Integer'Value(Get_Attribute(SavedNode, "type")));
            if MType = Deliver or MType = Destroy then
               Index :=
                 To_Unbounded_String(Get_Attribute(SavedNode, "target"));
            else
               Target := Integer'Value(Get_Attribute(SavedNode, "target"));
            end if;
            Time := Positive'Value(Get_Attribute(SavedNode, "time"));
            TargetX := Natural'Value(Get_Attribute(SavedNode, "targetx"));
            TargetY := Natural'Value(Get_Attribute(SavedNode, "targety"));
            Reward := Positive'Value(Get_Attribute(SavedNode, "reward"));
            StartBase := Natural'Value(Get_Attribute(SavedNode, "startbase"));
            if Get_Attribute(SavedNode, "multiplier") /= "" then
               Multiplier :=
                 RewardMultiplier'Value
                   (Get_Attribute(SavedNode, "multiplier"));
            else
               Multiplier := 1.0;
            end if;
            if Get_Attribute(Item(NodesList, I), "finished") = "Y" then
               Finished := True;
            else
               Finished := False;
            end if;
            case MType is
               when Deliver =>
                  AcceptedMissions.Append
                    (New_Item =>
                       (MType => Deliver, ItemIndex => Index, Time => Time,
                        TargetX => TargetX, TargetY => TargetY,
                        Reward => Reward, StartBase => StartBase,
                        Finished => Finished, Multiplier => Multiplier));
               when Destroy =>
                  AcceptedMissions.Append
                    (New_Item =>
                       (MType => Destroy, ShipIndex => Index, Time => Time,
                        TargetX => TargetX, TargetY => TargetY,
                        Reward => Reward, StartBase => StartBase,
                        Finished => Finished, Multiplier => Multiplier));
               when Patrol =>
                  AcceptedMissions.Append
                    (New_Item =>
                       (MType => Patrol, Target => Target, Time => Time,
                        TargetX => TargetX, TargetY => TargetY,
                        Reward => Reward, StartBase => StartBase,
                        Finished => Finished, Multiplier => Multiplier));
               when Explore =>
                  AcceptedMissions.Append
                    (New_Item =>
                       (MType => Explore, Target => Target, Time => Time,
                        TargetX => TargetX, TargetY => TargetY,
                        Reward => Reward, StartBase => StartBase,
                        Finished => Finished, Multiplier => Multiplier));
               when Passenger =>
                  if Target > 91 then
                     Target := 91;
                  end if;
                  AcceptedMissions.Append
                    (New_Item =>
                       (MType => Passenger, Data => Target, Time => Time,
                        TargetX => TargetX, TargetY => TargetY,
                        Reward => Reward, StartBase => StartBase,
                        Finished => Finished, Multiplier => Multiplier));
            end case;
            MIndex := AcceptedMissions.Last_Index;
            if not Finished then
               SkyMap
                 (AcceptedMissions(MIndex).TargetX,
                  AcceptedMissions(MIndex).TargetY)
                 .MissionIndex :=
                 MIndex;
            else
               SkyMap
                 (SkyBases(AcceptedMissions(MIndex).StartBase).SkyX,
                  SkyBases(AcceptedMissions(MIndex).StartBase).SkyY)
                 .MissionIndex :=
                 MIndex;
            end if;
         end loop;
      end;
      -- Load player career
      LogMessage("Loading player career...", Everything, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "playercareer");
      if Length(NodesList) > 0 then
         SavedNode := Item(NodesList, 0);
         PlayerCareer :=
           To_Unbounded_String(Get_Attribute(SavedNode, "index"));
      else
         PlayerCareer := Careers_Container.Key(Careers_List.First);
      end if;
      LogMessage("done.", Everything, True, False);
      Free(Reader);
      LogMessage("Finished loading game.", Everything);
   exception
      when An_Exception : others =>
         Free(Reader);
         PlayerShip.Crew.Clear;
         raise SaveGame_Invalid_Data with Exception_Message(An_Exception);
   end LoadGame;

   procedure GenerateSaveName(RenameSave: Boolean := False) is
      OldSaveName: constant String := To_String(SaveName);
   begin
      loop
         SaveName :=
           SaveDirectory & PlayerShip.Crew(1).Name & To_Unbounded_String("_") &
           PlayerShip.Name &
           To_Unbounded_String
             ("_" & Positive'Image(GetRandom(100, 999))(2 .. 4) & ".sav");
         exit when not Exists(To_String(SaveName));
      end loop;
      if RenameSave then
         if Exists(OldSaveName) then
            Rename(OldSaveName, To_String(SaveName));
         end if;
      end if;
   end GenerateSaveName;

end Game.SaveLoad;
