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

   -- ****iv* GSaveLoad/GSaveLoad.Save_Version
   -- FUNCTION
   -- Current version of the save game
   -- SOURCE
   Save_Version: constant Positive := 5;
   -- ****

   procedure Save_Game(Pretty_Print: Boolean := False) is
      --## rule off IMPROPER_INITIALIZATION
      Save: DOM_Implementation;
      --## rule on IMPROPER_INITIALIZATION
      Category_Node, Main_Node: DOM.Core.Element;
      Raw_Value: Unbounded_String := Null_Unbounded_String;
      Save_File: File_Type;
      Save_Data: Document;
      procedure Save_Statistics
        (Statistics_Vector: in out Statistics_Container.Vector;
         Stat_Name: String) is
         Stat_Node: DOM.Core.Element;
      begin
         Save_Statistics_Loop :
         for Statistic of Statistics_Vector loop
            Stat_Node :=
              Append_Child
                (N => Category_Node,
                 New_Child =>
                   Create_Element(Doc => Save_Data, Tag_Name => Stat_Name));
            Set_Attribute
              (Elem => Stat_Node, Name => "index",
               Value => To_String(Source => Statistic.Index));
            Raw_Value :=
              To_Unbounded_String(Source => Integer'Image(Statistic.Amount));
            Set_Attribute
              (Elem => Stat_Node, Name => "amount",
               Value =>
                 To_String
                   (Source =>
                      Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
         end loop Save_Statistics_Loop;
      end Save_Statistics;
      procedure Save_Number
        (Value: Integer; Name: String;
         Node: DOM.Core.Element := Category_Node) is
         Number_String: constant String :=
           Trim(Source => Integer'Image(Value), Side => Ada.Strings.Left);
      begin
         Set_Attribute(Elem => Node, Name => Name, Value => Number_String);
      end Save_Number;
      type Difficulty_Data is record
         Name: Unbounded_String;
         Value: Bonus_Type;
      end record;
      Difficulties: constant array(1 .. 8) of Difficulty_Data :=
        (1 =>
           (Name => To_Unbounded_String(Source => "enemydamagebonus"),
            Value => New_Game_Settings.Enemy_Damage_Bonus),
         2 =>
           (Name => To_Unbounded_String(Source => "playerdamagebonus"),
            Value => New_Game_Settings.Player_Damage_Bonus),
         3 =>
           (Name => To_Unbounded_String(Source => "enemymeleedamagebonus"),
            Value => New_Game_Settings.Enemy_Melee_Damage_Bonus),
         4 =>
           (Name => To_Unbounded_String(Source => "playermeleedamagebonus"),
            Value => New_Game_Settings.Player_Melee_Damage_Bonus),
         5 =>
           (Name => To_Unbounded_String(Source => "experiencebonus"),
            Value => New_Game_Settings.Experience_Bonus),
         6 =>
           (Name => To_Unbounded_String(Source => "reputationbonus"),
            Value => New_Game_Settings.Reputation_Bonus),
         7 =>
           (Name => To_Unbounded_String(Source => "upgradecostbonus"),
            Value => New_Game_Settings.Upgrade_Cost_Bonus),
         8 =>
           (Name => To_Unbounded_String(Source => "pricesbonus"),
            Value => New_Game_Settings.Prices_Bonus));
   begin
      Log_Message
        ("Start saving game in file " & To_String(Save_Name) & ".",
         EVERYTHING);
      --## rule off IMPROPER_INITIALIZATION
      Save_Data := Create_Document(Save);
      --## rule on IMPROPER_INITIALIZATION
      Main_Node := Append_Child(Save_Data, Create_Element(Save_Data, "save"));
      -- Write save game version
      Set_Attribute
        (Main_Node, "version",
         Trim(Positive'Image(Save_Version), Ada.Strings.Left));
      -- Save game difficulty settings
      Log_Message("Saving game difficulty settings...", EVERYTHING, False);
      Category_Node :=
        Append_Child(Main_Node, Create_Element(Save_Data, "difficulty"));
      Save_Difficulty_Loop :
      for Difficulty of Difficulties loop
         Raw_Value := To_Unbounded_String(Bonus_Type'Image(Difficulty.Value));
         Set_Attribute
           (Category_Node, To_String(Difficulty.Name),
            To_String(Trim(Raw_Value, Ada.Strings.Left)));
      end loop Save_Difficulty_Loop;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save game date
      Log_Message("Saving game time...", EVERYTHING, False);
      Category_Node :=
        Append_Child(Main_Node, Create_Element(Save_Data, "gamedate"));
      Save_Number(Game_Date.Year, "year");
      Save_Number(Game_Date.Month, "month");
      Save_Number(Game_Date.Day, "day");
      Save_Number(Game_Date.Hour, "hour");
      Save_Number(Game_Date.Minutes, "minutes");
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
                  FieldNode := Create_Element(Save_Data, "field");
                  FieldNode := Append_Child(Main_Node, FieldNode);
                  Save_Number(X, "x", FieldNode);
                  Save_Number(Y, "y", FieldNode);
               end if;
            end loop Save_Map_Y_Loop;
         end loop Save_Map_X_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save bases
      Log_Message("Saving bases...", EVERYTHING, False);
      SaveBases(Save_Data, Main_Node);
      Log_Message("done.", EVERYTHING, True, False);
      -- Save player ship
      Log_Message("Saving player ship...", EVERYTHING, False);
      SavePlayerShip(Save_Data, Main_Node);
      Log_Message("done.", EVERYTHING, True, False);
      -- Save known recipes
      Log_Message("Saving known recipes...", EVERYTHING, False);
      declare
         RecipeNode: DOM.Core.Element;
      begin
         Save_Known_Recipes_Loop :
         for Recipe of Known_Recipes loop
            RecipeNode := Create_Element(Save_Data, "recipe");
            RecipeNode := Append_Child(Main_Node, RecipeNode);
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
               MessageNode := Create_Element(Save_Data, "message");
               MessageNode := Append_Child(Main_Node, MessageNode);
               Save_Number
                 (Message_Type'Pos(Message.MType), "type", MessageNode);
               Save_Number
                 (Message_Color'Pos(Message.Color), "color", MessageNode);
               MessageText :=
                 Create_Text_Node(Save_Data, To_String(Message.Message));
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
            EventNode := Create_Element(Save_Data, "event");
            EventNode := Append_Child(Main_Node, EventNode);
            Save_Number(Events_Types'Pos(Event.EType), "type", EventNode);
            Save_Number(Event.SkyX, "x", EventNode);
            Save_Number(Event.SkyY, "y", EventNode);
            Save_Number(Event.Time, "time", EventNode);
            case Event.EType is
               when DoublePrice =>
                  Raw_Value := Event.ItemIndex;
               when AttackOnBase | EnemyShip | EnemyPatrol | Trader |
                 FriendlyShip =>
                  Raw_Value := Event.ShipIndex;
               when others =>
                  Raw_Value := To_Unbounded_String(Integer'Image(Event.Data));
            end case;
            Set_Attribute
              (EventNode, "data",
               To_String(Trim(Raw_Value, Ada.Strings.Left)));
         end loop Save_Events_Loop;
      end;
      Log_Message("done.", EVERYTHING, True, False);
      -- Save game statistics
      Log_Message("Saving game statistics...", EVERYTHING, False);
      Category_Node :=
        Append_Child(Main_Node, Create_Element(Save_Data, "statistics"));
      Save_Statistics(GameStats.DestroyedShips, "destroyedships");
      Save_Number(GameStats.BasesVisited, "visitedbases");
      Save_Number(GameStats.MapVisited, "mapdiscovered");
      Save_Number(GameStats.DistanceTraveled, "distancetraveled");
      Save_Statistics(GameStats.CraftingOrders, "finishedcrafts");
      Save_Number(GameStats.AcceptedMissions, "acceptedmissions");
      Save_Statistics(GameStats.FinishedMissions, "finishedmissions");
      Save_Statistics(GameStats.FinishedGoals, "finishedgoals");
      Save_Statistics(GameStats.KilledMobs, "killedmobs");
      Save_Number(GameStats.Points, "points");
      Log_Message("done.", EVERYTHING, True, False);
      -- Save current goal
      Log_Message("Saving current goal...", EVERYTHING, False);
      Category_Node :=
        Append_Child(Main_Node, Create_Element(Save_Data, "currentgoal"));
      Set_Attribute(Category_Node, "index", To_String(CurrentGoal.Index));
      Save_Number(GoalTypes'Pos(CurrentGoal.GType), "type");
      Save_Number(CurrentGoal.Amount, "amount");
      Set_Attribute
        (Category_Node, "target", To_String(CurrentGoal.TargetIndex));
      Log_Message("done.", EVERYTHING, True, False);
      -- Save current story
      if CurrentStory.Index /= Null_Unbounded_String then
         Log_Message("Saving current story...", EVERYTHING, False);
         Category_Node := Create_Element(Save_Data, "currentstory");
         Category_Node := Append_Child(Main_Node, Category_Node);
         Set_Attribute(Category_Node, "index", To_String(CurrentStory.Index));
         Raw_Value := To_Unbounded_String(Positive'Image(CurrentStory.Step));
         Set_Attribute
           (Category_Node, "step",
            To_String(Trim(Raw_Value, Ada.Strings.Left)));
         if CurrentStory.CurrentStep = 0 then
            Set_Attribute(Category_Node, "currentstep", "start");
         elsif CurrentStory.CurrentStep = -1 then
            Set_Attribute(Category_Node, "currentstep", "finish");
         else
            Set_Attribute
              (Category_Node, "currentstep",
               To_String
                 (Stories_List(CurrentStory.Index).Steps
                    (CurrentStory.CurrentStep)
                    .Index));
         end if;
         Save_Number(CurrentStory.MaxSteps, "maxsteps");
         if CurrentStory.ShowText then
            Set_Attribute(Category_Node, "showtext", "Y");
         else
            Set_Attribute(Category_Node, "showtext", "N");
         end if;
         if CurrentStory.Data /= Null_Unbounded_String then
            Set_Attribute(Category_Node, "data", To_String(CurrentStory.Data));
         end if;
         Save_Number
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
            Category_Node := Create_Element(Save_Data, "finishedstory");
            Category_Node := Append_Child(Main_Node, Category_Node);
            Set_Attribute
              (Category_Node, "index", To_String(FinishedStory.Index));
            Save_Number(FinishedStory.StepsAmount, "stepsamount");
            Save_Story_Steps_Loop :
            for Step of FinishedStory.StepsTexts loop
               StepNode := Create_Element(Save_Data, "steptext");
               StepNode := Append_Child(Category_Node, StepNode);
               StepText := Create_Text_Node(Save_Data, To_String(Step));
               StepText := Append_Child(StepNode, StepText);
            end loop Save_Story_Steps_Loop;
         end loop Save_Finished_Stories_Loop;
         Log_Message("done.", EVERYTHING, True, False);
      end;
      -- Save missions accepted by player
      Save_Missions_Loop :
      for Mission of AcceptedMissions loop
         Category_Node := Create_Element(Save_Data, "acceptedmission");
         Category_Node := Append_Child(Main_Node, Category_Node);
         Save_Number(Missions_Types'Pos(Mission.MType), "type");
         if Mission.MType = Deliver then
            Raw_Value := Mission.ItemIndex;
         elsif Mission.MType = Passenger then
            Raw_Value := To_Unbounded_String(Integer'Image(Mission.Data));
         elsif Mission.MType = Destroy then
            Raw_Value := Mission.ShipIndex;
         else
            Raw_Value := To_Unbounded_String(Integer'Image(Mission.Target));
         end if;
         Set_Attribute
           (Category_Node, "target",
            To_String(Trim(Raw_Value, Ada.Strings.Left)));
         Save_Number(Mission.Time, "time");
         Save_Number(Mission.TargetX, "targetx");
         Save_Number(Mission.TargetY, "targety");
         Save_Number(Mission.Reward, "reward");
         Save_Number(Mission.StartBase, "startbase");
         if Mission.Finished then
            Set_Attribute(Category_Node, "finished", "Y");
         else
            Set_Attribute(Category_Node, "finished", "N");
         end if;
         if Mission.Multiplier /= 1.0 then
            Raw_Value :=
              To_Unbounded_String(RewardMultiplier'Image(Mission.Multiplier));
            Set_Attribute
              (Category_Node, "multiplier",
               To_String(Trim(Raw_Value, Ada.Strings.Left)));
         end if;
      end loop Save_Missions_Loop;
      -- Save player career
      Log_Message("Saving player career...", EVERYTHING, False);
      Category_Node :=
        Append_Child(Main_Node, Create_Element(Save_Data, "playercareer"));
      Set_Attribute(Category_Node, "index", To_String(Player_Career));
      Log_Message("done.", EVERYTHING, True, False);
      Create(Save_File, Out_File, To_String(Save_Name));
      Write
        (Stream => Stream(Save_File), N => Save_Data,
         Pretty_Print => Pretty_Print);
      Close(Save_File);
      Log_Message("Finished saving game.", EVERYTHING);
   end Save_Game;

   procedure Load_Game is
      Save_File: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes: Node_List;
      SavedNode: Node;
      Save_Data: Document;
   begin
      Log_Message
        ("Start loading game from file " & To_String(Save_Name) & ".",
         EVERYTHING);
      Open(To_String(Save_Name), Save_File);
      Parse(Reader, Save_File);
      Close(Save_File);
      Save_Data := Get_Tree(Reader);
      -- Check save game compatybility
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "save");
      SavedNode := Item(NodesList, 0);
      if Get_Attribute(SavedNode, "version") /= "" then
         if Positive'Value(Get_Attribute(SavedNode, "version")) >
           Save_Version then
            raise Save_Game_Invalid_Data
              with "This save is incompatible with this version of the game";
         end if;
      end if;
      -- Load game difficulty settings
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "difficulty");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "gamedate");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "field");
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
      LoadBases(Save_Data);
      Log_Message("done.", EVERYTHING, True, False);
      -- Load player ship
      Log_Message("Loading player ship...", EVERYTHING, False);
      LoadPlayerShip(Save_Data);
      Log_Message("done.", EVERYTHING, True, False);
      -- Load known recipes
      Log_Message("Loading known recipes...", EVERYTHING, False);
      Known_Recipes.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "recipe");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "message");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "event");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "statistics");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "currentgoal");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "currentstory");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Save_Data, "finishedstory");
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
          (Save_Data, "acceptedmission");
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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "playercareer");
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
         raise Save_Game_Invalid_Data with Exception_Message(An_Exception);
   end Load_Game;

   procedure Generate_Save_Name(Rename_Save: Boolean := False) is
      OldSave_Name: constant String := To_String(Save_Name);
   begin
      Generate_Save_Name_Loop :
      loop
         Save_Name :=
           Save_Directory & Player_Ship.Crew(1).Name &
           To_Unbounded_String("_") & Player_Ship.Name &
           To_Unbounded_String
             ("_" & Positive'Image(GetRandom(100, 999))(2 .. 4) & ".sav");
         exit Generate_Save_Name_Loop when not Exists(To_String(Save_Name)) and
           Save_Name /= OldSave_Name;
      end loop Generate_Save_Name_Loop;
      if Rename_Save then
         if Exists(OldSave_Name) then
            Rename(OldSave_Name, To_String(Save_Name));
         end if;
      end if;
   end Generate_Save_Name;

end Game.SaveLoad;
