--    Copyright 2017-2021 Bartek thindil Jasicki
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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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

   -- ****iv* GSaveLoad/GSaveLoad.SaveData
   -- FUNCTION
   -- XML Structure for save/load the game data from file
   -- SOURCE
   SaveData: Document;
   -- ****

   -- ****iv* GSaveLoad/GSaveLoad.SaveVersion
   -- FUNCTION
   -- Current version of the save game
   -- SOURCE
   SaveVersion: constant Positive := 5;
   -- ****

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
         Save_Statistics_Loop :
         for Statistic of StatisticsVector loop
            StatNode := Create_Element(SaveData, StatName);
            StatNode := Append_Child(CategoryNode, StatNode);
            Set_Attribute(StatNode, "index", To_String(Statistic.Index));
            RawValue := To_Unbounded_String(Integer'Image(Statistic.Amount));
            Set_Attribute
              (StatNode, "amount",
               To_String(Trim(RawValue, Ada.Strings.Left)));
         end loop Save_Statistics_Loop;
      end SaveStatistics;
      procedure SaveNumber
        (Value: Integer; Name: String;
         Node: DOM.Core.Element := CategoryNode) is
         RawValue: constant String :=
           Trim(Integer'Image(Value), Ada.Strings.Left);
      begin
         Set_Attribute(Node, Name, RawValue);
      end SaveNumber;
      type Difficulty_Data is record
         Name: Unbounded_String;
         Value: Bonus_Type;
      end record;
      Difficulties: constant array(1 .. 8) of Difficulty_Data :=
        ((To_Unbounded_String("enemydamagebonus"),
          New_Game_Settings.Enemy_Damage_Bonus),
         (To_Unbounded_String("playerdamagebonus"),
          New_Game_Settings.Player_Damage_Bonus),
         (To_Unbounded_String("enemymeleedamagebonus"),
          New_Game_Settings.Enemy_Melee_Damage_Bonus),
         (To_Unbounded_String("playermeleedamagebonus"),
          New_Game_Settings.Player_Melee_Damage_Bonus),
         (To_Unbounded_String("experiencebonus"),
          New_Game_Settings.Experience_Bonus),
         (To_Unbounded_String("reputationbonus"),
          New_Game_Settings.Reputation_Bonus),
         (To_Unbounded_String("upgradecostbonus"),
          New_Game_Settings.Upgrade_Cost_Bonus),
         (To_Unbounded_String("pricesbonus"), New_Game_Settings.Prices_Bonus));
   begin
      Log_Message
        ("Start saving game in file " & To_String(SaveName) & ".", EVERYTHING);
      SaveData := Create_Document(Save);
      MainNode := Create_Element(SaveData, "save");
      MainNode := Append_Child(SaveData, MainNode);
      -- Write save game version
      Set_Attribute
        (MainNode, "version",
         Trim(Positive'Image(SaveVersion), Ada.Strings.Left));
      -- Save game difficulty settings
      Log_Message("Saving game difficulty settings...", EVERYTHING, False);
      CategoryNode := Create_Element(SaveData, "difficulty");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Save_Difficulty_Loop :
      for Difficulty of Difficulties loop
         RawValue := To_Unbounded_String(Bonus_Type'Image(Difficulty.Value));
         Set_Attribute
           (CategoryNode, To_String(Difficulty.Name),
            To_String(Trim(RawValue, Ada.Strings.Left)));
      end loop Save_Difficulty_Loop;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save game date
      Log_Message("Saving game time...", EVERYTHING, False);
      CategoryNode := Create_Element(SaveData, "gamedate");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      SaveNumber(Game_Date.Year, "year");
      SaveNumber(Game_Date.Month, "month");
      SaveNumber(Game_Date.Day, "day");
      SaveNumber(Game_Date.Hour, "hour");
      SaveNumber(Game_Date.Minutes, "minutes");
      Log_Message("done.", EVERYTHING, True, False);
      -- Save map
      Log_Message("Saving map...", EVERYTHING, False);
      declare
         FieldNode: DOM.Core.Element;
      begin
         Save_Map_X_Loop :
         for X in SkyMap'Range(1) loop
            Save_Map_Y_Loop :
            for Y in SkyMap'Range(2) loop
               if SkyMap(X, Y).Visited then
                  FieldNode := Create_Element(SaveData, "field");
                  FieldNode := Append_Child(MainNode, FieldNode);
                  SaveNumber(X, "x", FieldNode);
                  SaveNumber(Y, "y", FieldNode);
               end if;
            end loop Save_Map_Y_Loop;
         end loop Save_Map_X_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save bases
      Log_Message("Saving bases...", EVERYTHING, False);
      SaveBases(SaveData, MainNode);
      Log_Message("done.", EVERYTHING, True, False);
      -- Save player ship
      Log_Message("Saving player ship...", EVERYTHING, False);
      SavePlayerShip(SaveData, MainNode);
      Log_Message("done.", EVERYTHING, True, False);
      -- Save known recipes
      Log_Message("Saving known recipes...", EVERYTHING, False);
      declare
         RecipeNode: DOM.Core.Element;
      begin
         Save_Known_Recipes_Loop :
         for Recipe of Known_Recipes loop
            RecipeNode := Create_Element(SaveData, "recipe");
            RecipeNode := Append_Child(MainNode, RecipeNode);
            Set_Attribute(RecipeNode, "index", To_String(Recipe));
         end loop Save_Known_Recipes_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save messages
      Log_Message("Saving messages...", EVERYTHING, False);
      declare
         Messages: Natural := Game_Settings.Saved_Messages;
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
            Save_Messages_Loop :
            for I in StartLoop .. MessagesAmount loop
               Message := GetMessage(I);
               MessageNode := Create_Element(SaveData, "message");
               MessageNode := Append_Child(MainNode, MessageNode);
               SaveNumber
                 (Message_Type'Pos(Message.MType), "type", MessageNode);
               SaveNumber
                 (Message_Color'Pos(Message.Color), "color", MessageNode);
               MessageText :=
                 Create_Text_Node(SaveData, To_String(Message.Message));
               MessageText := Append_Child(MessageNode, MessageText);
            end loop Save_Messages_Loop;
         end if;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save events
      Log_Message("Saving events...", EVERYTHING, False);
      declare
         EventNode: DOM.Core.Element;
      begin
         Save_Events_Loop :
         for Event of Events_List loop
            EventNode := Create_Element(SaveData, "event");
            EventNode := Append_Child(MainNode, EventNode);
            SaveNumber(Events_Types'Pos(Event.EType), "type", EventNode);
            SaveNumber(Event.SkyX, "x", EventNode);
            SaveNumber(Event.SkyY, "y", EventNode);
            SaveNumber(Event.Time, "time", EventNode);
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
         end loop Save_Events_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save game statistics
      Log_Message("Saving game statistics...", EVERYTHING, False);
      CategoryNode := Create_Element(SaveData, "statistics");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      SaveStatistics(GameStats.DestroyedShips, "destroyedships");
      SaveNumber(GameStats.BasesVisited, "visitedbases");
      SaveNumber(GameStats.MapVisited, "mapdiscovered");
      SaveNumber(GameStats.DistanceTraveled, "distancetraveled");
      SaveStatistics(GameStats.CraftingOrders, "finishedcrafts");
      SaveNumber(GameStats.AcceptedMissions, "acceptedmissions");
      SaveStatistics(GameStats.FinishedMissions, "finishedmissions");
      SaveStatistics(GameStats.FinishedGoals, "finishedgoals");
      SaveStatistics(GameStats.KilledMobs, "killedmobs");
      SaveNumber(GameStats.Points, "points");
      Log_Message("done.", EVERYTHING, True, False);
      -- Save current goal
      Log_Message("Saving current goal...", EVERYTHING, False);
      CategoryNode := Create_Element(SaveData, "currentgoal");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "index", To_String(CurrentGoal.Index));
      SaveNumber(GoalTypes'Pos(CurrentGoal.GType), "type");
      SaveNumber(CurrentGoal.Amount, "amount");
      Set_Attribute
        (CategoryNode, "target", To_String(CurrentGoal.TargetIndex));
      Log_Message("done.", EVERYTHING, True, False);
      -- Save current story
      if CurrentStory.Index /= Null_Unbounded_String then
         Log_Message("Saving current story...", EVERYTHING, False);
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
         SaveNumber(CurrentStory.MaxSteps, "maxsteps");
         if CurrentStory.ShowText then
            Set_Attribute(CategoryNode, "showtext", "Y");
         else
            Set_Attribute(CategoryNode, "showtext", "N");
         end if;
         if CurrentStory.Data /= Null_Unbounded_String then
            Set_Attribute(CategoryNode, "data", To_String(CurrentStory.Data));
         end if;
         SaveNumber
           (StepConditionType'Pos(CurrentStory.FinishedStep), "finishedstep");
         Log_Message("done.", EVERYTHING, True, False);
      end if;
      -- Save finished stories data
      declare
         StepNode: DOM.Core.Element;
         StepText: Text;
      begin
         Log_Message("Saving finished stories...", EVERYTHING, False);
         Save_Finished_Stories_Loop :
         for FinishedStory of FinishedStories loop
            CategoryNode := Create_Element(SaveData, "finishedstory");
            CategoryNode := Append_Child(MainNode, CategoryNode);
            Set_Attribute
              (CategoryNode, "index", To_String(FinishedStory.Index));
            SaveNumber(FinishedStory.StepsAmount, "stepsamount");
            Save_Story_Steps_Loop :
            for Step of FinishedStory.StepsTexts loop
               StepNode := Create_Element(SaveData, "steptext");
               StepNode := Append_Child(CategoryNode, StepNode);
               StepText := Create_Text_Node(SaveData, To_String(Step));
               StepText := Append_Child(StepNode, StepText);
            end loop Save_Story_Steps_Loop;
         end loop Save_Finished_Stories_Loop;
         Log_Message("done.", EVERYTHING, True, False);
      end;
      -- Save missions accepted by player
      Save_Missions_Loop :
      for Mission of AcceptedMissions loop
         CategoryNode := Create_Element(SaveData, "acceptedmission");
         CategoryNode := Append_Child(MainNode, CategoryNode);
         SaveNumber(Missions_Types'Pos(Mission.MType), "type");
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
         SaveNumber(Mission.Time, "time");
         SaveNumber(Mission.TargetX, "targetx");
         SaveNumber(Mission.TargetY, "targety");
         SaveNumber(Mission.Reward, "reward");
         SaveNumber(Mission.StartBase, "startbase");
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
      end loop Save_Missions_Loop;
      -- Save player career
      Log_Message("Saving player career...", EVERYTHING, False);
      CategoryNode := Create_Element(SaveData, "playercareer");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "index", To_String(Player_Career));
      Log_Message("done.", EVERYTHING, True, False);
      Create(SaveFile, Out_File, To_String(SaveName));
      Write
        (Stream => Stream(SaveFile), N => SaveData,
         Pretty_Print => PrettyPrint);
      Close(SaveFile);
      Log_Message("Finished saving game.", EVERYTHING);
   end SaveGame;

   procedure LoadGame is
      SaveFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes: Node_List;
      SavedNode: Node;
   begin
      Log_Message
        ("Start loading game from file " & To_String(SaveName) & ".",
         EVERYTHING);
      Open(To_String(SaveName), SaveFile);
      Parse(Reader, SaveFile);
      Close(SaveFile);
      SaveData := Get_Tree(Reader);
      -- Check save game compatybility
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "save");
      SavedNode := Item(NodesList, 0);
      if Get_Attribute(SavedNode, "version") /= "" then
         if Positive'Value(Get_Attribute(SavedNode, "version")) >
           SaveVersion then
            raise SaveGame_Invalid_Data
              with "This save is incompatible with this version of the game";
         end if;
      end if;
      -- Load game difficulty settings
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "difficulty");
      if Length(NodesList) > 0 then
         Log_Message("Loading game difficulty settings...", EVERYTHING, False);
         SavedNode := Item(NodesList, 0);
         New_Game_Settings.Enemy_Damage_Bonus :=
           Bonus_Type'Value(Get_Attribute(SavedNode, "enemydamagebonus"));
         New_Game_Settings.Player_Damage_Bonus :=
           Bonus_Type'Value(Get_Attribute(SavedNode, "playerdamagebonus"));
         New_Game_Settings.Enemy_Melee_Damage_Bonus :=
           Bonus_Type'Value(Get_Attribute(SavedNode, "enemymeleedamagebonus"));
         New_Game_Settings.Player_Melee_Damage_Bonus :=
           Bonus_Type'Value
             (Get_Attribute(SavedNode, "playermeleedamagebonus"));
         New_Game_Settings.Experience_Bonus :=
           Bonus_Type'Value(Get_Attribute(SavedNode, "experiencebonus"));
         New_Game_Settings.Reputation_Bonus :=
           Bonus_Type'Value(Get_Attribute(SavedNode, "reputationbonus"));
         New_Game_Settings.Upgrade_Cost_Bonus :=
           Bonus_Type'Value(Get_Attribute(SavedNode, "upgradecostbonus"));
         if Get_Attribute(SavedNode, "pricesbonus") /= "" then
            New_Game_Settings.Prices_Bonus :=
              Bonus_Type'Value(Get_Attribute(SavedNode, "pricesbonus"));
         end if;
         Log_Message("done.", EVERYTHING, True, False);
      end if;
      -- Load game date
      Log_Message("Loading game time...", EVERYTHING, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "gamedate");
      SavedNode := Item(NodesList, 0);
      Game_Date.Year := Natural'Value(Get_Attribute(SavedNode, "year"));
      Game_Date.Month := Natural'Value(Get_Attribute(SavedNode, "month"));
      Game_Date.Day := Natural'Value(Get_Attribute(SavedNode, "day"));
      Game_Date.Hour := Natural'Value(Get_Attribute(SavedNode, "hour"));
      Game_Date.Minutes := Natural'Value(Get_Attribute(SavedNode, "minutes"));
      Log_Message("done.", EVERYTHING, True, False);
      -- Load sky map
      Log_Message("Loading map...", EVERYTHING, False);
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
         Load_Map_Loop :
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            X := Natural'Value(Get_Attribute(SavedNode, "x"));
            Y := Natural'Value(Get_Attribute(SavedNode, "y"));
            SkyMap(X, Y).Visited := True;
         end loop Load_Map_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Load sky bases
      Log_Message("Loading bases...", EVERYTHING, False);
      LoadBases(SaveData);
      Log_Message("done.", EVERYTHING, True, False);
      -- Load player ship
      Log_Message("Loading player ship...", EVERYTHING, False);
      LoadPlayerShip(SaveData);
      Log_Message("done.", EVERYTHING, True, False);
      -- Load known recipes
      Log_Message("Loading known recipes...", EVERYTHING, False);
      Known_Recipes.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "recipe");
      Load_Known_Recipes_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         Known_Recipes.Append
           (New_Item =>
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index")));
      end loop Load_Known_Recipes_Loop;
      Log_Message("done.", EVERYTHING, True, False);
      -- Load messages
      Log_Message("Loading messages...", EVERYTHING, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "message");
      ClearMessages;
      declare
         Text: Unbounded_String;
         MType: Message_Type;
         Color: Message_Color;
      begin
         Load_Messages_Loop :
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
         end loop Load_Messages_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Load events
      Log_Message("Loading events...", EVERYTHING, False);
      Events_List.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "event");
      declare
         EType: Events_Types;
         X, Y, Time: Integer;
         Data: Unbounded_String;
      begin
         Load_Events_Loop :
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
         end loop Load_Events_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Load game statistics
      Log_Message("Loading game statistics...", EVERYTHING, False);
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
         Load_Statistics_Loop :
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
         end loop Load_Statistics_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Load current goal
      Log_Message("Loading current goal...", EVERYTHING, False);
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
      Log_Message("done.", EVERYTHING, True, False);
      -- Load current story
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "currentstory");
      if Length(NodesList) > 0 then
         Log_Message("Loading current story...", EVERYTHING, False);
         SavedNode := Item(NodesList, 0);
         CurrentStory.Index :=
           To_Unbounded_String(Get_Attribute(SavedNode, "index"));
         CurrentStory.Step := Positive'Value(Get_Attribute(SavedNode, "step"));
         if Get_Attribute(SavedNode, "currentstep") = "start" then
            CurrentStory.CurrentStep := 0;
         elsif Get_Attribute(SavedNode, "currentstep") = "finish" then
            CurrentStory.CurrentStep := -1;
         else
            Load_Story_Steps_Loop :
            for I in Stories_List(CurrentStory.Index).Steps.Iterate loop
               if Stories_List(CurrentStory.Index).Steps(I).Index =
                 To_Unbounded_String
                   (Get_Attribute(SavedNode, "currentstep")) then
                  CurrentStory.CurrentStep := Steps_Container.To_Index(I);
                  exit Load_Story_Steps_Loop;
               end if;
            end loop Load_Story_Steps_Loop;
         end if;
         CurrentStory.MaxSteps :=
           Positive'Value(Get_Attribute(SavedNode, "maxsteps"));
         CurrentStory.ShowText :=
           (if Get_Attribute(SavedNode, "showtext") = "Y" then True
            else False);
         if Get_Attribute(SavedNode, "data") /= "" then
            CurrentStory.Data :=
              To_Unbounded_String(Get_Attribute(SavedNode, "data"));
         end if;
         CurrentStory.FinishedStep :=
           StepConditionType'Val
             (Integer'Value(Get_Attribute(SavedNode, "finishedstep")));
         Log_Message("done.", EVERYTHING, True, False);
      end if;
      -- Load finished stories data
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "finishedstory");
      declare
         StepsAmount: Positive;
         TempTexts: UnboundedString_Container.Vector;
         StoryIndex: Unbounded_String;
      begin
         Log_Message("Loading finished stories...", EVERYTHING, False);
         Load_Finished_Stories_Loop :
         for I in 0 .. Length(NodesList) - 1 loop
            SavedNode := Item(NodesList, I);
            StoryIndex :=
              To_Unbounded_String(Get_Attribute(SavedNode, "index"));
            StepsAmount :=
              Positive'Value(Get_Attribute(SavedNode, "stepsamount"));
            TempTexts.Clear;
            ChildNodes := Child_Nodes(SavedNode);
            Load_Stories_Text_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempTexts.Append
                 (New_Item =>
                    (To_Unbounded_String
                       (Node_Value(First_Child(Item(ChildNodes, J))))));
            end loop Load_Stories_Text_Loop;
            FinishedStories.Append
              (New_Item =>
                 (Index => StoryIndex, StepsAmount => StepsAmount,
                  StepsTexts => TempTexts));
         end loop Load_Finished_Stories_Loop;
         Log_Message("done.", EVERYTHING, True, False);
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
         Log_Message("Loading accepted missions...", EVERYTHING, False);
         Load_Missions_Loop :
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
            Multiplier :=
              (if Get_Attribute(SavedNode, "multiplier") /= "" then
                 RewardMultiplier'Value(Get_Attribute(SavedNode, "multiplier"))
               else 1.0);
            Finished :=
              (if Get_Attribute(Item(NodesList, I), "finished") = "Y" then True
               else False);
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
         end loop Load_Missions_Loop;
      end;
      -- Load player career
      Log_Message("Loading player career...", EVERYTHING, False);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "playercareer");
      if Length(NodesList) > 0 then
         SavedNode := Item(NodesList, 0);
         Player_Career :=
           To_Unbounded_String(Get_Attribute(SavedNode, "index"));
      else
         Player_Career := Careers_Container.Key(Careers_List.First);
      end if;
      Log_Message("done.", EVERYTHING, True, False);
      Free(Reader);
      Log_Message("Finished loading game.", EVERYTHING);
   exception
      when An_Exception : others =>
         Free(Reader);
         Player_Ship.Crew.Clear;
         raise SaveGame_Invalid_Data with Exception_Message(An_Exception);
   end LoadGame;

   procedure GenerateSaveName(RenameSave: Boolean := False) is
      OldSaveName: constant String := To_String(SaveName);
   begin
      Generate_Save_Name_Loop :
      loop
         SaveName :=
           Save_Directory & Player_Ship.Crew(1).Name &
           To_Unbounded_String("_") & Player_Ship.Name &
           To_Unbounded_String
             ("_" & Positive'Image(GetRandom(100, 999))(2 .. 4) & ".sav");
         exit Generate_Save_Name_Loop when not Exists(To_String(SaveName)) and
           SaveName /= OldSaveName;
      end loop Generate_Save_Name_Loop;
      if RenameSave then
         if Exists(OldSaveName) then
            Rename(OldSaveName, To_String(SaveName));
         end if;
      end if;
   end GenerateSaveName;

end Game.SaveLoad;
