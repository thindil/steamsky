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
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
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

   SaveVersion: constant String := "2.4";

   procedure SaveGame is
      Save: DOM_Implementation;
      SaveData: Document;
      CategoryNode: DOM.Core.Element;
      RawValue: Unbounded_String;
      SaveFile: File_Type;
      procedure AddData
        (NodeName, Value: String;
         ParentNode: DOM.Core.Element := CategoryNode) is
         DataNode: DOM.Core.Element;
         Data: Text;
      begin
         DataNode := Create_Element(SaveData, NodeName);
         DataNode := Append_Child(ParentNode, DataNode);
         Data := Create_Text_Node(SaveData, Value);
         Data := Append_Child(DataNode, Data);
      end AddData;
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
      -- Save game date
      CategoryNode := Create_Element(SaveData, "date");
      CategoryNode := Append_Child(SaveData, CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Year));
      AddData("year", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Month));
      AddData("month", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Day));
      AddData("day", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Hour));
      AddData("hour", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Minutes));
      AddData("minutes", To_String(Trim(RawValue, Ada.Strings.Left)));
      -- Save map
      CategoryNode := Create_Element(SaveData, "map");
      CategoryNode := Append_Child(SaveData, CategoryNode);
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
      --SaveBases(SaveGame);
      -- Save player ship
      --SavePlayerShip(SaveGame);
      -- Save known recipes
      CategoryNode := Create_Element(SaveData, "known recipes");
      CategoryNode := Append_Child(SaveData, CategoryNode);
      for Recipe of Known_Recipes loop
         AddData("index", To_String(Recipes_List(Recipe).Index));
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
         CategoryNode := Append_Child(SaveData, CategoryNode);
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
      CategoryNode := Append_Child(SaveData, CategoryNode);
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
      CategoryNode := Append_Child(SaveData, CategoryNode);
      SaveStatistics(GameStats.DestroyedShips, "destroyed ships", "ship");
      RawValue := To_Unbounded_String(Positive'Image(GameStats.BasesVisited));
      AddData("visited bases", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Positive'Image(GameStats.MapVisited));
      AddData("map discovered", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.DistanceTraveled));
      AddData
        ("distance traveled",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      SaveStatistics(GameStats.CraftingOrders, "finished crafts", "order");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.AcceptedMissions));
      AddData
        ("accepted missions",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      SaveStatistics
        (GameStats.FinishedMissions,
         "finished missions",
         "mission");
      SaveStatistics(GameStats.FinishedGoals, "finished goals", "goal");
      SaveStatistics(GameStats.KilledMobs, "killed mobs", "mob");
      RawValue := To_Unbounded_String(Natural'Image(GameStats.Points));
      AddData("points", To_String(Trim(RawValue, Ada.Strings.Left)));
      -- Save current goal
      CategoryNode := Create_Element(SaveData, "current goal");
      CategoryNode := Append_Child(SaveData, CategoryNode);
      AddData("index", To_String(CurrentGoal.Index));
      RawValue :=
        To_Unbounded_String(Integer'Image(GoalTypes'Pos(CurrentGoal.GType)));
      AddData("type", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Natural'Image(CurrentGoal.Amount));
      AddData("amount", To_String(Trim(RawValue, Ada.Strings.Left)));
      AddData("target", To_String(CurrentGoal.TargetIndex));
      Create(SaveFile, Out_File, To_String(SaveDirectory) & "savegame.dat");
      Write(Stream => Stream(SaveFile), N => SaveData, Pretty_Print => True);
      Close(SaveFile);
   end SaveGame;

   procedure LoadGame is
      SaveGame: File_Type;
      VectorLength: Natural;
      Message: Unbounded_String;
      MType: Message_Type;
      VisitedFields: Positive;
      procedure LoadStatistics
        (StatisticsVector: in out Statistics_Container.Vector) is
      begin
         VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
         for I in 1 .. VectorLength loop
            StatisticsVector.Append
            (New_Item =>
               (Index => ReadData(SaveGame),
                Amount => Positive'Value(To_String(ReadData(SaveGame)))));
         end loop;
      end LoadStatistics;
   begin
      Open(SaveGame, In_File, To_String(SaveDirectory) & "savegame.dat");
      -- Check save version
      if ReadData(SaveGame) /= SaveVersion then
         Close(SaveGame);
         raise SaveGame_Invalid_Version;
      end if;
      -- Load game date
      GameDate.Year := Natural'Value(To_String(ReadData(SaveGame)));
      GameDate.Month := Natural'Value(To_String(ReadData(SaveGame)));
      GameDate.Day := Natural'Value(To_String(ReadData(SaveGame)));
      GameDate.Hour := Natural'Value(To_String(ReadData(SaveGame)));
      GameDate.Minutes := Natural'Value(To_String(ReadData(SaveGame)));
      -- Load sky map
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0,
               Visited => False,
               EventIndex => 0,
               MissionIndex => 0)));
      VisitedFields := Positive'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VisitedFields loop
         SkyMap
           (Positive'Value(To_String(ReadData(SaveGame))),
            Positive'Value(To_String(ReadData(SaveGame))))
           .Visited :=
           True;
      end loop;
      -- Load sky bases
      LoadBases(SaveGame);
      -- Load player ship
      LoadPlayerShip(SaveGame);
      -- Load known recipes
      VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VectorLength loop
         Known_Recipes.Append(New_Item => FindRecipe(ReadData(SaveGame)));
      end loop;
      -- Load messages
      VectorLength := Integer'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VectorLength loop
         Message := ReadData(SaveGame);
         MType :=
           Message_Type'Val(Integer'Value(To_String(ReadData(SaveGame))));
         RestoreMessage
           (Message,
            MType,
            Natural'Value(To_String(ReadData(SaveGame))));
      end loop;
      -- Load events
      VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VectorLength loop
         Events_List.Append
         (New_Item =>
            (EType =>
               Events_Types'Val(Integer'Value(To_String(ReadData(SaveGame)))),
             SkyX => Integer'Value(To_String(ReadData(SaveGame))),
             SkyY => Integer'Value(To_String(ReadData(SaveGame))),
             Time => Integer'Value(To_String(ReadData(SaveGame))),
             Data => Integer'Value(To_String(ReadData(SaveGame)))));
         SkyMap(Events_List(I).SkyX, Events_List(I).SkyY).EventIndex := I;
      end loop;
      -- Load game statistics
      LoadStatistics(GameStats.DestroyedShips);
      GameStats.BasesVisited := Positive'Value(To_String(ReadData(SaveGame)));
      GameStats.MapVisited := Positive'Value(To_String(ReadData(SaveGame)));
      GameStats.DistanceTraveled :=
        Positive'Value(To_String(ReadData(SaveGame)));
      LoadStatistics(GameStats.CraftingOrders);
      GameStats.AcceptedMissions :=
        Positive'Value(To_String(ReadData(SaveGame)));
      LoadStatistics(GameStats.FinishedMissions);
      LoadStatistics(GameStats.FinishedGoals);
      LoadStatistics(GameStats.KilledMobs);
      GameStats.Points := Natural'Value(To_String(ReadData(SaveGame)));
      -- Load current goal
      CurrentGoal.Index := ReadData(SaveGame);
      CurrentGoal.GType :=
        GoalTypes'Val(Integer'Value(To_String(ReadData(SaveGame))));
      CurrentGoal.Amount := Natural'Value(To_String(ReadData(SaveGame)));
      CurrentGoal.TargetIndex := ReadData(SaveGame);
      Close(SaveGame);
   exception
      when An_Exception : Constraint_Error | End_Error =>
         Close(SaveGame);
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

end Game.SaveLoad;
