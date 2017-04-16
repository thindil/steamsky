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
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Messages; use Messages;
with Crafts; use Crafts;
with Events; use Events;
with Statistics; use Statistics;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Items; use Items;

package body Game.SaveLoad is

   SaveVersion: constant String := "1.1";

   procedure SaveGame is
      SaveGame: File_Type;
      RawValue: Unbounded_String;
      Messages: Natural := 10;
      StartLoop: Positive;
      Message: Message_Data;
      VisitedFields: Natural := 0;
   begin
      Create(SaveGame, Out_File, "data/savegame.dat");
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
      for I in SkyBases'Range loop
         Put(SaveGame, To_String(SkyBases(I).Name) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Year));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         if SkyBases(I).Visited.Year > 0 then
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Month));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Day));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Hour));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Minutes));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end if;
         RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyX));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyY));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Bases_Types'Pos(SkyBases(I).BaseType)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Population));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         if SkyBases(I).Visited.Year > 0 then
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Year));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).RecruitDate.Month));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Day));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(SkyBases(I).Recruits.Length'Img);
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            if SkyBases(I).Recruits.Length > 0 then
               for Recruit of SkyBases(I).Recruits loop
                  Put(SaveGame, To_String(Recruit.Name) & ";");
                  Put(SaveGame, Recruit.Gender & ";");
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Recruit.Price));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  RawValue := To_Unbounded_String(Recruit.Skills.Length'Img);
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  for Skill of Recruit.Skills loop
                     RawValue := To_Unbounded_String(Integer'Image(Skill(1)));
                     Put
                       (SaveGame,
                        To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                     RawValue := To_Unbounded_String(Integer'Image(Skill(2)));
                     Put
                       (SaveGame,
                        To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                     RawValue := To_Unbounded_String(Integer'Image(Skill(3)));
                     Put
                       (SaveGame,
                        To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  end loop;
               end loop;
            end if;
            if SkyBases(I).AskedForBases then
               Put(SaveGame, "Y;");
            else
               Put(SaveGame, "N;");
            end if;
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Year));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Month));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Day));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end if;
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(1)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(2)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         if SkyBases(I).Visited.Year > 0 then
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).MissionsDate.Year));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).MissionsDate.Month));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).MissionsDate.Day));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(SkyBases(I).Missions.Length'Img);
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            if SkyBases(I).Missions.Length > 0 then
               for Mission of SkyBases(I).Missions loop
                  RawValue :=
                    To_Unbounded_String
                      (Integer'Image(Missions_Types'Pos(Mission.MType)));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Mission.Target));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  RawValue := To_Unbounded_String(Integer'Image(Mission.Time));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Mission.TargetX));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Mission.TargetY));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Mission.Reward));
                  Put
                    (SaveGame,
                     To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
               end loop;
            end if;
         end if;
         if SkyBases(I).Known then
            Put(SaveGame, "Y;");
         else
            Put(SaveGame, "N;");
         end if;
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Bases_Owners'Pos(SkyBases(I).Owner)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      -- Save player ship
      Put(SaveGame, To_String(PlayerShip.Name) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue :=
        To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.UpgradeModule));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationX));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationY));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.RepairModule));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(PlayerShip.Modules.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Module of PlayerShip.Modules loop
         Put(SaveGame, To_String(Module.Name) & ";");
         Put(SaveGame, To_String(Modules_List(Module.ProtoIndex).Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Weight));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Current_Value));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Max_Value));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Durability));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.MaxDurability));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Owner));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(Module.UpgradeProgress));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String
             (Integer'Image(ShipUpgrade'Pos(Module.UpgradeAction)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue := To_Unbounded_String(PlayerShip.Cargo.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Item of PlayerShip.Cargo loop
         Put(SaveGame, To_String(Items_List(Item.ProtoIndex).Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         Put(SaveGame, To_String(Item.Name) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue := To_Unbounded_String(PlayerShip.Crew.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Member of PlayerShip.Crew loop
         Put(SaveGame, To_String(Member.Name) & ";");
         Put(SaveGame, Member.Gender & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Health));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Tired));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Hunger));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Thirst));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(Crew_Orders'Pos(Member.Order)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Crew_Orders'Pos(Member.PreviousOrder)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.OrderTime));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Member.Skills.Length'Img);
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         for Skill of Member.Skills loop
            RawValue := To_Unbounded_String(Integer'Image(Skill(1)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Skill(2)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Skill(3)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
         for J in Member.Orders'Range loop
            RawValue := To_Unbounded_String(Integer'Image(Member.Orders(J)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
      end loop;
      RawValue := To_Unbounded_String(PlayerShip.Missions.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      if PlayerShip.Missions.Length > 0 then
         for Mission of PlayerShip.Missions loop
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Missions_Types'Pos(Mission.MType)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.Target));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.Time));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetX));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetY));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.Reward));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.StartBase));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            if Mission.Finished then
               Put(SaveGame, "Y;");
            else
               Put(SaveGame, "N;");
            end if;
         end loop;
      end if;
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
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Message_Type'Pos(Message.MType)));
            Put
              (SaveGame,
               To_String(Message.Message) &
               ";" &
               To_String(Trim(RawValue, Ada.Strings.Left)) &
               ";");
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
         RawValue :=
           To_Unbounded_String(Integer'Image(DestroyedShip.ProtoIndex));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
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
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.CraftingOrders));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.AcceptedMissions));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue :=
        To_Unbounded_String(Positive'Image(GameStats.FinishedMissions));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      Close(SaveGame);
   end SaveGame;

   function LoadGame return Unbounded_String is
      SaveGame: File_Type;
      VectorLength, SkillsLength: Natural;
      Skills: Skills_Container.Vector;
      ShipModules: Modules_Container.Vector;
      ShipCargo: Cargo_Container.Vector;
      ShipCrew: Crew_Container.Vector;
      Message: Unbounded_String;
      MType: Message_Type;
      BaseRecruits: Recruit_Container.Vector;
      VisitedFields: Positive;
      BaseMissions: Mission_Container.Vector;
      Finished: Boolean;
      TmpOrders: Orders_Array;
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
      procedure UpdateMember(Member: in out Member_Data) is
      begin
         Member.Skills := Skills;
         Member.Orders := TmpOrders;
      end UpdateMember;
      procedure UpdateRecruit(Recruit: in out Recruit_Data) is
      begin
         Recruit.Skills := Skills;
      end UpdateRecruit;
      procedure UpdateMission(Mission: in out Mission_Data) is
      begin
         Mission.Finished := Finished;
      end UpdateMission;
   begin
      Open(SaveGame, In_File, "data/savegame.dat");
      -- Check save version
      if ReadData /= SaveVersion then
         Close(SaveGame);
         return To_Unbounded_String
             ("This saved game is incompatible with this version of game and can't be loaded.");
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
      for I in SkyBases'Range loop
         SkyBases(I) :=
           (Name => ReadData,
            Visited => (0, 0, 0, 0, 0),
            SkyX => 0,
            SkyY => 0,
            BaseType => Industrial,
            Population => 0,
            RecruitDate => (0, 0, 0, 0, 0),
            Recruits => BaseRecruits,
            Known => False,
            AskedForBases => False,
            AskedForEvents => (0, 0, 0, 0, 0),
            Reputation => (0, 0),
            MissionsDate => (0, 0, 0, 0, 0),
            Missions => BaseMissions,
            Owner => Poleis);
         SkyBases(I).Visited.Year := Natural'Value(To_String(ReadData));
         if SkyBases(I).Visited.Year > 0 then
            SkyBases(I).Visited.Month := Natural'Value(To_String(ReadData));
            SkyBases(I).Visited.Day := Natural'Value(To_String(ReadData));
            SkyBases(I).Visited.Hour := Natural'Value(To_String(ReadData));
            SkyBases(I).Visited.Minutes := Natural'Value(To_String(ReadData));
         end if;
         SkyBases(I).SkyX := Integer'Value(To_String(ReadData));
         SkyBases(I).SkyY := Integer'Value(To_String(ReadData));
         SkyBases(I).BaseType :=
           Bases_Types'Val(Integer'Value(To_String(ReadData)));
         SkyBases(I).Population := Natural'Value(To_String(ReadData));
         if SkyBases(I).Visited.Year > 0 then
            SkyBases(I).RecruitDate.Year := Natural'Value(To_String(ReadData));
            SkyBases(I).RecruitDate.Month :=
              Natural'Value(To_String(ReadData));
            SkyBases(I).RecruitDate.Day := Natural'Value(To_String(ReadData));
            VectorLength := Natural'Value(To_String(ReadData));
            if VectorLength > 0 then
               for J in 1 .. VectorLength loop
                  Skills.Clear;
                  BaseRecruits.Append
                  (New_Item =>
                     (Name => ReadData,
                      Gender => Element(ReadData, 1),
                      Price => Positive'Value(To_String(ReadData)),
                      Skills => Skills));
                  SkillsLength := Positive'Value(To_String(ReadData));
                  for K in 1 .. SkillsLength loop
                     Skills.Append
                     (New_Item =>
                        (Natural'Value(To_String(ReadData)),
                         Natural'Value(To_String(ReadData)),
                         Natural'Value(To_String(ReadData))));
                  end loop;
                  BaseRecruits.Update_Element
                  (Index =>
                     BaseRecruits.Last_Index, Process =>
                     UpdateRecruit'Access);
               end loop;
               SkyBases(I).Recruits := BaseRecruits;
               BaseRecruits.Clear;
            end if;
            if ReadData = To_Unbounded_String("Y") then
               SkyBases(I).AskedForBases := True;
            end if;
            SkyBases(I).AskedForEvents.Year :=
              Natural'Value(To_String(ReadData));
            SkyBases(I).AskedForEvents.Month :=
              Natural'Value(To_String(ReadData));
            SkyBases(I).AskedForEvents.Day :=
              Natural'Value(To_String(ReadData));
         end if;
         SkyBases(I).Reputation(1) := Integer'Value(To_String(ReadData));
         SkyBases(I).Reputation(2) := Integer'Value(To_String(ReadData));
         if SkyBases(I).Visited.Year > 0 then
            SkyBases(I).MissionsDate.Year :=
              Natural'Value(To_String(ReadData));
            SkyBases(I).MissionsDate.Month :=
              Natural'Value(To_String(ReadData));
            SkyBases(I).MissionsDate.Day := Natural'Value(To_String(ReadData));
            VectorLength := Natural'Value(To_String(ReadData));
            if VectorLength > 0 then
               for J in 1 .. VectorLength loop
                  BaseMissions.Append
                  (New_Item =>
                     (MType =>
                        Missions_Types'Val(Integer'Value(To_String(ReadData))),
                      Target => Natural'Value(To_String(ReadData)),
                      Time => Integer'Value(To_String(ReadData)),
                      TargetX => Integer'Value(To_String(ReadData)),
                      TargetY => Integer'Value(To_String(ReadData)),
                      Reward => Integer'Value(To_String(ReadData)),
                      StartBase => I,
                      Finished => False));
               end loop;
               SkyBases(I).Missions := BaseMissions;
               BaseMissions.Clear;
            end if;
         end if;
         if ReadData = To_Unbounded_String("Y") then
            SkyBases(I).Known := True;
         end if;
         SkyBases(I).Owner :=
           Bases_Owners'Val(Integer'Value(To_String(ReadData)));
         SkyMap(SkyBases(I).SkyX, SkyBases(I).SkyY).BaseIndex := I;
      end loop;
      -- Load player ship
      PlayerShip.Name := ReadData;
      PlayerShip.SkyX := Integer'Value(To_String(ReadData));
      PlayerShip.SkyY := Integer'Value(To_String(ReadData));
      PlayerShip.Speed := ShipSpeed'Val(Integer'Value(To_String(ReadData)));
      PlayerShip.UpgradeModule := Integer'Value(To_String(ReadData));
      PlayerShip.DestinationX := Integer'Value(To_String(ReadData));
      PlayerShip.DestinationY := Integer'Value(To_String(ReadData));
      PlayerShip.RepairModule := Integer'Value(To_String(ReadData));
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         ShipModules.Append
         (New_Item =>
            (Name => ReadData,
             ProtoIndex => FindProtoModule(ReadData),
             Weight => Natural'Value(To_String(ReadData)),
             Current_Value => Integer'Value(To_String(ReadData)),
             Max_Value => Integer'Value(To_String(ReadData)),
             Durability => Integer'Value(To_String(ReadData)),
             MaxDurability => Integer'Value(To_String(ReadData)),
             Owner => Integer'Value(To_String(ReadData)),
             UpgradeProgress => Integer'Value(To_String(ReadData)),
             UpgradeAction =>
               ShipUpgrade'Val(Integer'Value(To_String(ReadData)))));
      end loop;
      PlayerShip.Modules := ShipModules;
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         ShipCargo.Append
         (New_Item =>
            (ProtoIndex => FindProtoItem(ReadData),
             Amount => Positive'Value(To_String(ReadData)),
             Name => ReadData,
             Durability => Positive'Value(To_String(ReadData))));
      end loop;
      PlayerShip.Cargo := ShipCargo;
      VectorLength := Positive'Value(To_String(ReadData));
      for I in 1 .. VectorLength loop
         Skills.Clear;
         ShipCrew.Append
         (New_Item =>
            (Name => ReadData,
             Gender => Element(ReadData, 1),
             Health => Natural'Value(To_String(ReadData)),
             Tired => Natural'Value(To_String(ReadData)),
             Skills => Skills,
             Hunger => Natural'Value(To_String(ReadData)),
             Thirst => Natural'Value(To_String(ReadData)),
             Order => Crew_Orders'Val(Integer'Value(To_String(ReadData))),
             PreviousOrder =>
               Crew_Orders'Val(Integer'Value(To_String(ReadData))),
             OrderTime => Integer'Value(To_String(ReadData)),
             Orders => (others => 0)));
         SkillsLength := Positive'Value(To_String(ReadData));
         for J in 1 .. SkillsLength loop
            Skills.Append
            (New_Item =>
               (Natural'Value(To_String(ReadData)),
                Natural'Value(To_String(ReadData)),
                Natural'Value(To_String(ReadData))));
         end loop;
         for J in TmpOrders'Range loop
            TmpOrders(J) := Natural'Value(To_String(ReadData));
         end loop;
         ShipCrew.Update_Element
         (Index => ShipCrew.Last_Index, Process => UpdateMember'Access);
      end loop;
      PlayerShip.Crew := ShipCrew;
      VectorLength := Natural'Value(To_String(ReadData));
      if VectorLength > 0 then
         for I in 1 .. VectorLength loop
            BaseMissions.Append
            (New_Item =>
               (MType =>
                  Missions_Types'Val(Integer'Value(To_String(ReadData))),
                Target => Natural'Value(To_String(ReadData)),
                Time => Integer'Value(To_String(ReadData)),
                TargetX => Integer'Value(To_String(ReadData)),
                TargetY => Integer'Value(To_String(ReadData)),
                Reward => Integer'Value(To_String(ReadData)),
                StartBase => Integer'Value(To_String(ReadData)),
                Finished => False));
            if To_String(ReadData) = "Y" then
               Finished := True;
            else
               Finished := False;
            end if;
            BaseMissions.Update_Element
            (Index =>
               BaseMissions.Last_Index, Process =>
               UpdateMission'Access);
            if not BaseMissions(I).Finished then
               SkyMap(BaseMissions(I).TargetX, BaseMissions(I).TargetY)
                 .MissionIndex :=
                 I;
            else
               SkyMap
                 (SkyBases(BaseMissions(I).StartBase).SkyX,
                  SkyBases(BaseMissions(I).StartBase).SkyY)
                 .MissionIndex :=
                 I;
            end if;
         end loop;
         PlayerShip.Missions := BaseMissions;
      end if;
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
         RestoreMessage(Message, MType);
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
            (ProtoIndex => Positive'Value(To_String(ReadData)),
             Amount => Positive'Value(To_String(ReadData))));
      end loop;
      GameStats.BasesVisited := Positive'Value(To_String(ReadData));
      GameStats.MapVisited := Positive'Value(To_String(ReadData));
      GameStats.DistanceTraveled := Positive'Value(To_String(ReadData));
      GameStats.CraftingOrders := Positive'Value(To_String(ReadData));
      GameStats.AcceptedMissions := Positive'Value(To_String(ReadData));
      GameStats.FinishedMissions := Positive'Value(To_String(ReadData));
      Close(SaveGame);
      return Null_Unbounded_String;
   exception
      when Constraint_Error | End_Error =>
         Close(SaveGame);
         return To_Unbounded_String("Can't load savegame file. Invalid data.");
   end LoadGame;

end Game.SaveLoad;
