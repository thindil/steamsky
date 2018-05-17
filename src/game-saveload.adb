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
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
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

package body Game.SaveLoad is

   SaveData: Document;

   procedure SaveGame is
      Save: DOM_Implementation;
      CategoryNode, MainNode: DOM.Core.Element;
      RawValue: Unbounded_String;
      SaveFile: File_Type;
      procedure SaveStatistics
        (StatisticsVector: in out Statistics_Container.Vector;
         NodeName, StatName: String) is
         StatGroupNode, StatNode: DOM.Core.Element;
      begin
         StatGroupNode := Create_Element(SaveData, NodeName);
         StatGroupNode := Append_Child(CategoryNode, StatGroupNode);
         for Statistic of StatisticsVector loop
            StatNode := Create_Element(SaveData, StatName);
            StatNode := Append_Child(StatGroupNode, StatNode);
            AddData("index", To_String(Statistic.Index), StatNode);
            RawValue := To_Unbounded_String(Integer'Image(Statistic.Amount));
            AddData
              ("amount",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               StatNode);
         end loop;
      end SaveStatistics;
   begin
      SaveData := Create_Document(Save);
      MainNode := Create_Element(SaveData, "save");
      MainNode := Append_Child(SaveData, MainNode);
      -- Save game date
      CategoryNode := Create_Element(SaveData, "date");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Year));
      AddData
        ("year",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Month));
      AddData
        ("month",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Day));
      AddData
        ("day",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Hour));
      AddData
        ("hour",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Minutes));
      AddData
        ("minutes",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      -- Save map
      CategoryNode := Create_Element(SaveData, "map");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      declare
         FieldNode: DOM.Core.Element;
      begin
         for X in SkyMap'Range loop
            for Y in 1 .. 1024 loop
               if SkyMap(X, Y).Visited then
                  FieldNode := Create_Element(SaveData, "field");
                  FieldNode := Append_Child(CategoryNode, FieldNode);
                  RawValue := To_Unbounded_String(Integer'Image(X));
                  AddData
                    ("x",
                     To_String(Trim(RawValue, Ada.Strings.Left)),
                     FieldNode);
                  RawValue := To_Unbounded_String(Integer'Image(Y));
                  AddData
                    ("y",
                     To_String(Trim(RawValue, Ada.Strings.Left)),
                     FieldNode);
               end if;
            end loop;
         end loop;
      end;
      -- Save bases
      SaveBases(SaveData, MainNode);
      -- Save player ship
      SavePlayerShip(SaveData, MainNode);
      -- Save known recipes
      CategoryNode := Create_Element(SaveData, "knownrecipes");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      for Recipe of Known_Recipes loop
         AddData("index", To_String(Recipes_List(Recipe).Index), CategoryNode);
      end loop;
      -- Save messages
      declare
         Messages: Natural := GameSettings.SavedMessages;
         StartLoop: Positive;
         MessageNode: DOM.Core.Element;
         Message: Message_Data;
      begin
         if Messages > MessagesAmount then
            Messages := MessagesAmount;
         end if;
         CategoryNode := Create_Element(SaveData, "messages");
         CategoryNode := Append_Child(MainNode, CategoryNode);
         if Messages > 0 then
            StartLoop := MessagesAmount - Messages + 1;
            for I in StartLoop .. MessagesAmount loop
               Message := GetMessage(I);
               MessageNode := Create_Element(SaveData, "message");
               MessageNode := Append_Child(CategoryNode, MessageNode);
               AddData("text", To_String(Message.Message), MessageNode);
               RawValue :=
                 To_Unbounded_String
                   (Integer'Image(Message_Type'Pos(Message.MType)));
               AddData
                 ("type",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  MessageNode);
               RawValue := To_Unbounded_String(Integer'Image(Message.Color));
               AddData
                 ("color",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  MessageNode);
            end loop;
         end if;
      end;
      -- Save events
      CategoryNode := Create_Element(SaveData, "events");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      declare
         EventNode: DOM.Core.Element;
      begin
         for Event of Events_List loop
            EventNode := Create_Element(SaveData, "event");
            EventNode := Append_Child(CategoryNode, EventNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Events_Types'Pos(Event.EType)));
            AddData
              ("type",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               EventNode);
            RawValue := To_Unbounded_String(Integer'Image(Event.SkyX));
            AddData
              ("x",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               EventNode);
            RawValue := To_Unbounded_String(Integer'Image(Event.SkyY));
            AddData
              ("y",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               EventNode);
            RawValue := To_Unbounded_String(Integer'Image(Event.Time));
            AddData
              ("time",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               EventNode);
            RawValue := To_Unbounded_String(Integer'Image(Event.Data));
            AddData
              ("data",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               EventNode);
         end loop;
      end;
      -- Save game statistics
      CategoryNode := Create_Element(SaveData, "statistics");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      SaveStatistics(GameStats.DestroyedShips, "destroyedships", "ship");
      RawValue := To_Unbounded_String(Positive'Image(GameStats.BasesVisited));
      AddData
        ("visitedbases",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Positive'Image(GameStats.MapVisited));
      AddData
        ("mapdiscovered",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.DistanceTraveled));
      AddData
        ("distancetraveled",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      SaveStatistics(GameStats.CraftingOrders, "finishedcrafts", "order");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.AcceptedMissions));
      AddData
        ("acceptedmissions",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      SaveStatistics
        (GameStats.FinishedMissions,
         "finishedmissions",
         "mission");
      SaveStatistics(GameStats.FinishedGoals, "finishedgoals", "goal");
      SaveStatistics(GameStats.KilledMobs, "killedmobs", "mob");
      RawValue := To_Unbounded_String(Natural'Image(GameStats.Points));
      AddData
        ("points",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      -- Save current goal
      CategoryNode := Create_Element(SaveData, "currentgoal");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      AddData("index", To_String(CurrentGoal.Index), CategoryNode);
      RawValue :=
        To_Unbounded_String(Integer'Image(GoalTypes'Pos(CurrentGoal.GType)));
      AddData
        ("type",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Natural'Image(CurrentGoal.Amount));
      AddData
        ("amount",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      AddData("target", To_String(CurrentGoal.TargetIndex), CategoryNode);
      Create(SaveFile, Out_File, To_String(SaveDirectory) & "savegame.dat");
      Write(Stream => Stream(SaveFile), N => SaveData, Pretty_Print => False);
      Close(SaveFile);
   end SaveGame;

   procedure LoadGame is
      SaveFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes: Node_List;
      procedure LoadStatistics
        (StatisticsVector: in out Statistics_Container.Vector;
         StatName: String;
         ParentNode: Node) is
         StatList, StatData: Node_List;
         Index: Unbounded_String;
         Amount: Positive;
      begin
         StatList := Child_Nodes(ParentNode);
         for I in 0 .. Length(StatList) - 1 loop
            if Node_Name(Item(StatList, I)) = StatName then
               StatData := Child_Nodes(ParentNode);
               Index := Null_Unbounded_String;
               Amount := 1;
               for J in 0 .. Length(StatData) - 1 loop
                  if Node_Name(Item(StatData, J)) = "index" then
                     Index :=
                       To_Unbounded_String
                         (Node_Value(First_Child(Item(StatData, J))));
                  elsif Node_Name(Item(StatData, J)) = "amount" then
                     Amount :=
                       Positive'Value
                         (Node_Value(First_Child(Item(StatData, J))));
                  end if;
               end loop;
               StatisticsVector.Append
               (New_Item => (Index => Index, Amount => Amount));
            end if;
         end loop;
      end LoadStatistics;
   begin
      Open(To_String(SaveDirectory) & "savegame.dat", SaveFile);
      Parse(Reader, SaveFile);
      Close(SaveFile);
      SaveData := Get_Tree(Reader);
      -- Load game date
      NodesList := Get_Elements_By_Tag_Name(SaveData, "date");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      for I in 0 .. Length(ChildNodes) - 1 loop
         if Node_Name(Item(ChildNodes, I)) = "year" then
            GameDate.Year :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "month" then
            GameDate.Month :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "day" then
            GameDate.Day :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "hour" then
            GameDate.Hour :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "minutes" then
            GameDate.Minutes :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         end if;
      end loop;
      -- Load sky map
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0,
               Visited => False,
               EventIndex => 0,
               MissionIndex => 0)));
      NodesList := Get_Elements_By_Tag_Name(SaveData, "map");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      declare
         FieldData: Node_List;
         X, Y: Positive;
      begin
         for I in 0 .. Length(ChildNodes) - 1 loop
            if Node_Name(Item(ChildNodes, I)) = "field" then
               FieldData := Child_Nodes(Item(ChildNodes, I));
               X := 1;
               Y := 1;
               for J in 0 .. Length(FieldData) - 1 loop
                  if Node_Name(Item(FieldData, J)) = "x" then
                     X :=
                       Positive'Value
                         (Node_Value(First_Child(Item(FieldData, J))));
                  elsif Node_Name(Item(FieldData, J)) = "y" then
                     Y :=
                       Positive'Value
                         (Node_Value(First_Child(Item(FieldData, J))));
                  end if;
               end loop;
               SkyMap(X, Y).Visited := True;
            end if;
         end loop;
      end;
      -- Load sky bases
--      LoadBases(SaveGame);
      -- Load player ship
--      LoadPlayerShip(SaveGame);
      -- Load known recipes
      NodesList := Get_Elements_By_Tag_Name(SaveData, "knownrecipes");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      for I in 0 .. Length(ChildNodes) - 1 loop
         if Node_Name(Item(ChildNodes, I)) = "index" then
            Known_Recipes.Append
            (New_Item =>
               FindRecipe
                 (To_Unbounded_String
                    (Node_Value(First_Child(Item(ChildNodes, I))))));
         end if;
      end loop;
      -- Load messages
      NodesList := Get_Elements_By_Tag_Name(SaveData, "messages");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      declare
         MessageData: Node_List;
         Text: Unbounded_String;
         MType: Message_Type;
         Color: Natural;
      begin
         for I in 0 .. Length(ChildNodes) - 1 loop
            if Node_Name(Item(ChildNodes, I)) = "message" then
               MessageData := Child_Nodes(Item(ChildNodes, I));
               Text := Null_Unbounded_String;
               MType := OtherMessage;
               Color := 0;
               for J in 0 .. Length(MessageData) - 1 loop
                  if Node_Name(Item(MessageData, J)) = "text" then
                     Text :=
                       To_Unbounded_String
                         (Node_Value(First_Child(Item(MessageData, J))));
                  elsif Node_Name(Item(MessageData, J)) = "type" then
                     MType :=
                       Message_Type'Val
                         (Integer'Value
                            (Node_Value(First_Child(Item(MessageData, J)))));
                  elsif Node_Name(Item(MessageData, J)) = "color" then
                     Color :=
                       Natural'Value
                         (Node_Value(First_Child(Item(MessageData, 5))));
                  end if;
               end loop;
               RestoreMessage(Text, MType, Color);
            end if;
         end loop;
      end;
      -- Load events
      NodesList := Get_Elements_By_Tag_Name(SaveData, "events");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      declare
         EventData: Node_List;
         EventIndex: Positive := 1;
         EType: Events_Types;
         X, Y, Time: Integer;
         Data: Positive;
      begin
         for I in 0 .. Length(ChildNodes) - 1 loop
            if Node_Name(Item(ChildNodes, I)) = "event" then
               EventData := Child_Nodes(Item(ChildNodes, I));
               EType := None;
               X := 0;
               Y := 0;
               Time := 0;
               Data := 1;
               for J in 0 .. Length(EventData) - 1 loop
                  if Node_Name(Item(EventData, J)) = "type" then
                     EType :=
                       Events_Types'Val
                         (Integer'Value
                            (Node_Value(First_Child(Item(EventData, J)))));
                  elsif Node_Name(Item(EventData, J)) = "x" then
                     X :=
                       Integer'Value
                         (Node_Value(First_Child(Item(EventData, J))));
                  elsif Node_Name(Item(EventData, J)) = "y" then
                     Y :=
                       Integer'Value
                         (Node_Value(First_Child(Item(EventData, J))));
                  elsif Node_Name(Item(EventData, J)) = "time" then
                     Time :=
                       Integer'Value
                         (Node_Value(First_Child(Item(EventData, J))));
                  elsif Node_Name(Item(EventData, J)) = "data" then
                     Data :=
                       Positive'Value
                         (Node_Value(First_Child(Item(EventData, J))));
                  end if;
               end loop;
               Events_List.Append
               (New_Item =>
                  (EType => EType,
                   SkyX => X,
                   SkyY => Y,
                   Time => Time,
                   Data => Data));
               SkyMap
                 (Events_List(EventIndex).SkyX,
                  Events_List(EventIndex).SkyY)
                 .EventIndex :=
                 EventIndex;
               EventIndex := EventIndex + 1;
            end if;
         end loop;
      end;
      -- Load game statistics
      NodesList := Get_Elements_By_Tag_Name(SaveData, "statistics");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      for I in 0 .. Length(ChildNodes) - 1 loop
         if Node_Name(Item(ChildNodes, I)) = "destroyedships" then
            LoadStatistics
              (GameStats.DestroyedShips,
               "ship",
               Item(ChildNodes, I));
         elsif Node_Name(Item(ChildNodes, I)) = "visitedbases" then
            GameStats.BasesVisited :=
              Positive'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "mapdiscovered" then
            GameStats.MapVisited :=
              Positive'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "distancetraveled" then
            GameStats.DistanceTraveled :=
              Positive'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "finishedcrafts" then
            LoadStatistics
              (GameStats.CraftingOrders,
               "order",
               Item(ChildNodes, I));
         elsif Node_Name(Item(ChildNodes, I)) = "acceptedmissions" then
            GameStats.AcceptedMissions :=
              Positive'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "finishedmissions" then
            LoadStatistics
              (GameStats.FinishedMissions,
               "mission",
               Item(ChildNodes, I));
         elsif Node_Name(Item(ChildNodes, I)) = "finishedgoals" then
            LoadStatistics
              (GameStats.FinishedGoals,
               "goal",
               Item(ChildNodes, I));
         elsif Node_Name(Item(ChildNodes, I)) = "killedmobs" then
            LoadStatistics(GameStats.KilledMobs, "mob", Item(ChildNodes, I));
         elsif Node_Name(Item(ChildNodes, I)) = "points" then
            GameStats.Points :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         end if;
      end loop;
      -- Load current goal
      NodesList := Get_Elements_By_Tag_Name(SaveData, "currentgoal");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      for I in 0 .. Length(NodesList) loop
         if Node_Name(Item(ChildNodes, I)) = "index" then
            CurrentGoal.Index :=
              To_Unbounded_String
                (Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "type" then
            CurrentGoal.GType :=
              GoalTypes'Val
                (Integer'Value(Node_Value(First_Child(Item(ChildNodes, I)))));
         elsif Node_Name(Item(ChildNodes, I)) = "amount" then
            CurrentGoal.Amount :=
              Natural'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "target" then
            CurrentGoal.TargetIndex :=
              To_Unbounded_String
                (Node_Value(First_Child(Item(ChildNodes, I))));
         end if;
      end loop;
      Free(Reader);
   exception
      when An_Exception : others =>
         Free(Reader);
         raise SaveGame_Invalid_Data with Exception_Message(An_Exception);
   end LoadGame;

   function ReadData(SaveGame: File_Type) return Unbounded_String is
      RawData: Unbounded_String := To_Unbounded_String("");
      Char: Character;
   begin
      Get(SaveGame, Char);
      while Char not in ';' loop
         Append(RawData, Char);
         Get(SaveGame, Char);
      end loop;
      return RawData;
   end ReadData;

   procedure AddData(NodeName, Value: String; ParentNode: DOM.Core.Element) is
      DataNode: DOM.Core.Element;
      Data: Text;
   begin
      DataNode := Create_Element(SaveData, NodeName);
      DataNode := Append_Child(ParentNode, DataNode);
      Data := Create_Text_Node(SaveData, Value);
      Data := Append_Child(DataNode, Data);
   end AddData;

end Game.SaveLoad;
