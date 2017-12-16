--    Copyright 2017 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Messages; use Messages;
with Crafts; use Crafts;
with Events; use Events;
with Statistics; use Statistics;
with Goals; use Goals;

package body Game.SaveLoad is

   SaveVersion: constant String := "1.8";

   procedure SaveGame is
      SaveGame: File_Type;
      RawValue: Unbounded_String;
      Messages: Natural := 10;
      StartLoop: Positive;
      Message: Message_Data;
      VisitedFields: Natural := 0;
   begin
      Create(SaveGame, Out_File, To_String(SaveDirectory) & "savegame.dat");
      -- Save version
      Put(SaveGame, SaveVersion & ";");
      -- Save game date
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Year));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Month));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Day));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Hour));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(GameDate.Minutes));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      -- Save map
      for X in 1 .. 1024 loop
         for Y in 1 .. 1024 loop
            if SkyMap(X, Y).Visited then
               VisitedFields := VisitedFields + 1;
            end if;
         end loop;
      end loop;
      RawValue := To_Unbounded_String(Integer'Image(VisitedFields));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for X in 1 .. 1024 loop
         for Y in 1 .. 1024 loop
            if SkyMap(X, Y).Visited then
               RawValue := To_Unbounded_String(Integer'Image(X));
               Put
                 (SaveGame,
                  To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
               RawValue := To_Unbounded_String(Integer'Image(Y));
               Put
                 (SaveGame,
                  To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end if;
         end loop;
      end loop;
      -- Save bases
      SaveBases(SaveGame);
      -- Save player ship
      SavePlayerShip(SaveGame);
      -- Save known recipes
      RawValue := To_Unbounded_String(Known_Recipes.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Recipe of Known_Recipes loop
         Put(SaveGame, To_String(Recipes_List(Recipe).Index) & ";");
      end loop;
      -- Save messages
      if Messages > MessagesAmount then
         Messages := MessagesAmount;
      end if;
      RawValue := To_Unbounded_String(Messages'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      if Messages > 0 then
         StartLoop := MessagesAmount - Messages + 1;
         for I in StartLoop .. MessagesAmount loop
            Message := GetMessage(I);
            Put(SaveGame, To_String(Message.Message) & ";");
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Message_Type'Pos(Message.MType)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Message.Color));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
      end if;
      -- Save events
      RawValue := To_Unbounded_String(Events_List.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Event of Events_List loop
         RawValue :=
           To_Unbounded_String(Integer'Image(Events_Types'Pos(Event.EType)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Event.SkyX));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Event.SkyY));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Event.Time));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Event.Data));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      -- Save game statistics
      RawValue := To_Unbounded_String(GameStats.DestroyedShips.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for DestroyedShip of GameStats.DestroyedShips loop
         Put(SaveGame, To_String(DestroyedShip.Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(DestroyedShip.Amount));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue := To_Unbounded_String(Positive'Image(GameStats.BasesVisited));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Positive'Image(GameStats.MapVisited));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.DistanceTraveled));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(GameStats.CraftingOrders.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for CraftingOrder of GameStats.CraftingOrders loop
         Put(SaveGame, To_String(CraftingOrder.Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(CraftingOrder.Amount));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.AcceptedMissions));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(GameStats.FinishedMissions.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for FinishedMission of GameStats.FinishedMissions loop
         Put(SaveGame, To_String(FinishedMission.Index) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(FinishedMission.Amount));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue := To_Unbounded_String(GameStats.FinishedGoals.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for FinishedGoal of GameStats.FinishedGoals loop
         Put(SaveGame, To_String(FinishedGoal.Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(FinishedGoal.Amount));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue := To_Unbounded_String(Natural'Image(GameStats.Points));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      -- Save current goal
      Put(SaveGame, To_String(CurrentGoal.Index) & ";");
      RawValue :=
        To_Unbounded_String(Integer'Image(GoalTypes'Pos(CurrentGoal.GType)));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Natural'Image(CurrentGoal.Amount));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      Put(SaveGame, To_String(CurrentGoal.TargetIndex) & ";");
      Close(SaveGame);
   end SaveGame;

   procedure LoadGame is
      SaveGame: File_Type;
      VectorLength: Natural;
      Message: Unbounded_String;
      MType: Message_Type;
      VisitedFields: Positive;
      function ReadData return Unbounded_String is
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
   begin
      Open(SaveGame, In_File, To_String(SaveDirectory) & "savegame.dat");
      -- Check save version
      if ReadData /= SaveVersion then
         Close(SaveGame);
         raise SaveGame_Invalid_Version;
      end if;
      -- Load game date
      GameDate.Year := Natural'Value(To_String(ReadData));
      GameDate.Month := Natural'Value(To_String(ReadData));
      GameDate.Day := Natural'Value(To_String(ReadData));
      GameDate.Hour := Natural'Value(To_String(ReadData));
      GameDate.Minutes := Natural'Value(To_String(ReadData));
      -- Load sky map
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0,
               Visited => False,
               EventIndex => 0,
               MissionIndex => 0)));
      VisitedFields := Positive'Value(To_String(ReadData));
      for I in 1 .. VisitedFields loop
         SkyMap
           (Positive'Value(To_String(ReadData)),
            Positive'Value(To_String(ReadData)))
           .Visited :=
           True;
      end loop;
      -- Load sky bases
      LoadBases(SaveGame);
      -- Load player ship
      LoadPlayerShip(SaveGame);
      -- Load known recipes
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         Known_Recipes.Append(New_Item => FindRecipe(ReadData));
      end loop;
      -- Load messages
      VectorLength := Integer'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         Message := ReadData;
         MType := Message_Type'Val(Integer'Value(To_String(ReadData)));
         RestoreMessage(Message, MType, Natural'Value(To_String(ReadData)));
      end loop;
      -- Load events
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         Events_List.Append
         (New_Item =>
            (EType => Events_Types'Val(Integer'Value(To_String(ReadData))),
             SkyX => Integer'Value(To_String(ReadData)),
             SkyY => Integer'Value(To_String(ReadData)),
             Time => Integer'Value(To_String(ReadData)),
             Data => Integer'Value(To_String(ReadData))));
         SkyMap(Events_List(I).SkyX, Events_List(I).SkyY).EventIndex := I;
      end loop;
      -- Load game statistics
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         GameStats.DestroyedShips.Append
         (New_Item =>
            (Index => ReadData,
             Amount => Positive'Value(To_String(ReadData))));
      end loop;
      GameStats.BasesVisited := Positive'Value(To_String(ReadData));
      GameStats.MapVisited := Positive'Value(To_String(ReadData));
      GameStats.DistanceTraveled := Positive'Value(To_String(ReadData));
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         GameStats.CraftingOrders.Append
         (New_Item =>
            (Index => ReadData,
             Amount => Positive'Value(To_String(ReadData))));
      end loop;
      GameStats.AcceptedMissions := Positive'Value(To_String(ReadData));
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         GameStats.FinishedMissions.Append
         (New_Item =>
            (Index => ReadData,
             Amount => Positive'Value(To_String(ReadData))));
      end loop;
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         GameStats.FinishedGoals.Append
         (New_Item =>
            (Index => ReadData,
             Amount => Positive'Value(To_String(ReadData))));
      end loop;
      GameStats.Points := Natural'Value(To_String(ReadData));
      -- Load current goal
      CurrentGoal.Index := ReadData;
      CurrentGoal.GType := GoalTypes'Val(Integer'Value(To_String(ReadData)));
      CurrentGoal.Amount := Natural'Value(To_String(ReadData));
      CurrentGoal.TargetIndex := ReadData;
      Close(SaveGame);
   exception
      when An_Exception : Constraint_Error | End_Error =>
         Close(SaveGame);
         raise SaveGame_Invalid_Data with Exception_Message(An_Exception);
   end LoadGame;

end Game.SaveLoad;
