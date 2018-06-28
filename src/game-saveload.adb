--    Copyright 2017-2018 Bartek thindil Jasicki
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

package body Game.SaveLoad is

   SaveData: Document;

   procedure SaveGame is
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
              (StatNode,
               "amount",
               To_String(Trim(RawValue, Ada.Strings.Left)));
         end loop;
      end SaveStatistics;
   begin
      SaveData := Create_Document(Save);
      MainNode := Create_Element(SaveData, "save");
      MainNode := Append_Child(SaveData, MainNode);
      -- Save game date
      CategoryNode := Create_Element(SaveData, "gamedate");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Year));
      Set_Attribute
        (CategoryNode,
         "year",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Month));
      Set_Attribute
        (CategoryNode,
         "month",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Day));
      Set_Attribute
        (CategoryNode,
         "day",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Hour));
      Set_Attribute
        (CategoryNode,
         "hour",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Minutes));
      Set_Attribute
        (CategoryNode,
         "minutes",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      -- Save map
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
                    (FieldNode,
                     "x",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  RawValue := To_Unbounded_String(Integer'Image(Y));
                  Set_Attribute
                    (FieldNode,
                     "y",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               end if;
            end loop;
         end loop;
      end;
      -- Save bases
      SaveBases(SaveData, MainNode);
      -- Save player ship
      SavePlayerShip(SaveData, MainNode);
      -- Save known recipes
      declare
         RecipeNode: DOM.Core.Element;
      begin
         for Recipe of Known_Recipes loop
            RecipeNode := Create_Element(SaveData, "recipe");
            RecipeNode := Append_Child(MainNode, RecipeNode);
            Set_Attribute
              (RecipeNode,
               "index",
               To_String(Recipes_List(Recipe).Index));
         end loop;
      end;
      -- Save messages
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
                 (MessageNode,
                  "type",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               RawValue := To_Unbounded_String(Integer'Image(Message.Color));
               Set_Attribute
                 (MessageNode,
                  "color",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               MessageText :=
                 Create_Text_Node(SaveData, To_String(Message.Message));
               MessageText := Append_Child(MessageNode, MessageText);
            end loop;
         end if;
      end;
      -- Save events
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
              (EventNode,
               "type",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.SkyX));
            Set_Attribute
              (EventNode,
               "x",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.SkyY));
            Set_Attribute
              (EventNode,
               "y",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.Time));
            Set_Attribute
              (EventNode,
               "time",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Event.Data));
            Set_Attribute
              (EventNode,
               "data",
               To_String(Trim(RawValue, Ada.Strings.Left)));
         end loop;
      end;
      -- Save game statistics
      CategoryNode := Create_Element(SaveData, "statistics");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      SaveStatistics(GameStats.DestroyedShips, "destroyedships");
      RawValue := To_Unbounded_String(Positive'Image(GameStats.BasesVisited));
      Set_Attribute
        (CategoryNode,
         "visitedbases",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Positive'Image(GameStats.MapVisited));
      Set_Attribute
        (CategoryNode,
         "mapdiscovered",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.DistanceTraveled));
      Set_Attribute
        (CategoryNode,
         "distancetraveled",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      SaveStatistics(GameStats.CraftingOrders, "finishedcrafts");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.AcceptedMissions));
      Set_Attribute
        (CategoryNode,
         "acceptedmissions",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      SaveStatistics(GameStats.FinishedMissions, "finishedmissions");
      SaveStatistics(GameStats.FinishedGoals, "finishedgoals");
      SaveStatistics(GameStats.KilledMobs, "killedmobs");
      RawValue := To_Unbounded_String(Natural'Image(GameStats.Points));
      Set_Attribute
        (CategoryNode,
         "points",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      -- Save current goal
      CategoryNode := Create_Element(SaveData, "currentgoal");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "index", To_String(CurrentGoal.Index));
      RawValue :=
        To_Unbounded_String(Integer'Image(GoalTypes'Pos(CurrentGoal.GType)));
      Set_Attribute
        (CategoryNode,
         "type",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Natural'Image(CurrentGoal.Amount));
      Set_Attribute
        (CategoryNode,
         "amount",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      Set_Attribute
        (CategoryNode,
         "target",
         To_String(CurrentGoal.TargetIndex));
      -- Save current story
      if CurrentStory.Index /= Null_Unbounded_String then
         CategoryNode := Create_Element(SaveData, "currentstory");
         CategoryNode := Append_Child(MainNode, CategoryNode);
         Set_Attribute(CategoryNode, "index", To_String(CurrentStory.Index));
         RawValue := To_Unbounded_String(Positive'Image(CurrentStory.Step));
         Set_Attribute
           (CategoryNode,
            "step",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         Set_Attribute
           (CategoryNode,
            "currentstep",
            To_String(CurrentStory.CurrentStep));
         RawValue :=
           To_Unbounded_String(Positive'Image(CurrentStory.MaxSteps));
         Set_Attribute
           (CategoryNode,
            "maxsteps",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         if CurrentStory.ShowText then
            Set_Attribute(CategoryNode, "showtext", "Y");
         else
            Set_Attribute(CategoryNode, "showtext", "N");
         end if;
         if CurrentStory.Data /= Null_Unbounded_String then
            Set_Attribute(CategoryNode, "data", To_String(CurrentStory.Data));
         end if;
      end if;
      Create(SaveFile, Out_File, To_String(SaveDirectory) & "savegame.dat");
      Write(Stream => Stream(SaveFile), N => SaveData, Pretty_Print => False);
      Close(SaveFile);
   end SaveGame;

   procedure LoadGame is
      SaveFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes: Node_List;
   begin
      Open(To_String(SaveDirectory) & "savegame.dat", SaveFile);
      Parse(Reader, SaveFile);
      Close(SaveFile);
      SaveData := Get_Tree(Reader);
      -- Load game date
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "gamedate");
      GameDate.Year :=
        Natural'Value(Get_Attribute(Item(NodesList, 0), "year"));
      GameDate.Month :=
        Natural'Value(Get_Attribute(Item(NodesList, 0), "month"));
      GameDate.Day := Natural'Value(Get_Attribute(Item(NodesList, 0), "day"));
      GameDate.Hour :=
        Natural'Value(Get_Attribute(Item(NodesList, 0), "hour"));
      GameDate.Minutes :=
        Natural'Value(Get_Attribute(Item(NodesList, 0), "minutes"));
      -- Load sky map
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0,
               Visited => False,
               EventIndex => 0,
               MissionIndex => 0)));
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "field");
      declare
         X, Y: Positive;
      begin
         for I in 0 .. Length(NodesList) - 1 loop
            X := Natural'Value(Get_Attribute(Item(NodesList, I), "x"));
            Y := Natural'Value(Get_Attribute(Item(NodesList, I), "y"));
            SkyMap(X, Y).Visited := True;
         end loop;
      end;
      -- Load sky bases
      LoadBases(SaveData);
      -- Load player ship
      LoadPlayerShip(SaveData);
      -- Load known recipes
      Known_Recipes.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "recipe");
      for I in 0 .. Length(NodesList) - 1 loop
         Known_Recipes.Append
         (New_Item =>
            FindRecipe
              (To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "index"))));
      end loop;
      -- Load messages
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "message");
      ClearMessages;
      declare
         Text: Unbounded_String;
         MType: Message_Type;
         Color: Natural;
      begin
         for I in 0 .. Length(NodesList) - 1 loop
            Text :=
              To_Unbounded_String(Node_Value(First_Child(Item(NodesList, I))));
            MType :=
              Message_Type'Val
                (Integer'Value(Get_Attribute(Item(NodesList, I), "type")));
            Color := Natural'Value(Get_Attribute(Item(NodesList, I), "color"));
            RestoreMessage(Text, MType, Color);
         end loop;
      end;
      -- Load events
      Events_List.Clear;
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "event");
      declare
         EType: Events_Types;
         X, Y, Time: Integer;
         Data: Positive;
      begin
         for I in 0 .. Length(NodesList) - 1 loop
            EType :=
              Events_Types'Val
                (Integer'Value(Get_Attribute(Item(NodesList, I), "type")));
            X := Integer'Value(Get_Attribute(Item(NodesList, I), "x"));
            Y := Integer'Value(Get_Attribute(Item(NodesList, I), "y"));
            Time := Integer'Value(Get_Attribute(Item(NodesList, I), "time"));
            Data := Positive'Value(Get_Attribute(Item(NodesList, I), "data"));
            Events_List.Append
            (New_Item =>
               (EType => EType,
                SkyX => X,
                SkyY => Y,
                Time => Time,
                Data => Data));
            SkyMap(Events_List(I + 1).SkyX, Events_List(I + 1).SkyY)
              .EventIndex :=
              I + 1;
         end loop;
      end;
      -- Load game statistics
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "statistics");
      declare
         StatIndex: Unbounded_String;
         StatAmount: Positive;
      begin
         GameStats.BasesVisited :=
           Positive'Value(Get_Attribute(Item(NodesList, 0), "visitedbases"));
         GameStats.MapVisited :=
           Positive'Value(Get_Attribute(Item(NodesList, 0), "mapdiscovered"));
         GameStats.DistanceTraveled :=
           Positive'Value
             (Get_Attribute(Item(NodesList, 0), "distancetraveled"));
         GameStats.AcceptedMissions :=
           Natural'Value
             (Get_Attribute(Item(NodesList, 0), "acceptedmissions"));
         GameStats.Points :=
           Positive'Value(Get_Attribute(Item(NodesList, 0), "points"));
         ChildNodes := Child_Nodes(Item(NodesList, 0));
         for I in 0 .. Length(ChildNodes) - 1 loop
            if Node_Name(Item(ChildNodes, I)) /= "#text" then
               StatIndex :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, I), "index"));
               StatAmount :=
                 Positive'Value(Get_Attribute(Item(ChildNodes, I), "amount"));
            end if;
            if Node_Name(Item(ChildNodes, I)) = "destroyedships" then
               GameStats.DestroyedShips.Append
               (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif Node_Name(Item(ChildNodes, I)) = "finishedcrafts" then
               GameStats.CraftingOrders.Append
               (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif Node_Name(Item(ChildNodes, I)) = "finishedmissions" then
               GameStats.FinishedMissions.Append
               (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif Node_Name(Item(ChildNodes, I)) = "finishedgoals" then
               GameStats.FinishedGoals.Append
               (New_Item => (Index => StatIndex, Amount => StatAmount));
            elsif Node_Name(Item(ChildNodes, I)) = "killedmobs" then
               GameStats.KilledMobs.Append
               (New_Item => (Index => StatIndex, Amount => StatAmount));
            end if;
         end loop;
      end;
      -- Load current goal
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
      -- Load current story
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "currentstory");
      if Length(NodesList) > 0 then
         CurrentStory.Index :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, 0), "index"));
         CurrentStory.Step :=
           Positive'Value(Get_Attribute(Item(NodesList, 0), "step"));
         CurrentStory.CurrentStep :=
           To_Unbounded_String
             (Get_Attribute(Item(NodesList, 0), "currentstep"));
         CurrentStory.MaxSteps :=
           Positive'Value(Get_Attribute(Item(NodesList, 0), "maxsteps"));
         if Get_Attribute(Item(NodesList, 0), "maxsteps") = "Y" then
            CurrentStory.ShowText := True;
         else
            CurrentStory.ShowText := False;
         end if;
         if Get_Attribute(Item(NodesList, 0), "data") /= "" then
            CurrentStory.Index :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, 0), "data"));
         end if;
      end if;
      Free(Reader);
   exception
      when An_Exception : others =>
         Free(Reader);
         PlayerShip.Crew.Clear;
         raise SaveGame_Invalid_Data with Exception_Message(An_Exception);
   end LoadGame;

end Game.SaveLoad;
