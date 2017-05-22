--    Copyright 2016-2017 Bartek thindil Jasicki
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
with Ada.Directories; use Ada.Directories;
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Ships.Upgrade; use Ships.Upgrade;
with Crew; use Crew;
with Messages; use Messages;
with Crafts; use Crafts;
with Items; use Items;
with Events; use Events;
with ShipModules; use ShipModules;
with Config; use Config;
with Statistics; use Statistics;
with Missions; use Missions;
with Utils; use Utils;

package body Game is

   procedure NewGame
     (CharName, ShipName: Unbounded_String;
      Gender: Character) is
      PosX, PosY, RandomBase, ShipIndex: Positive;
      ValidLocation: Boolean;
      TempX, TempY, BaseReputation: Integer;
      TmpSkills: Skills_Container.Vector;
      TmpRecruits: Recruit_Container.Vector;
      TmpMissions: Mission_Container.Vector;
      CabinAssigned: Boolean := False;
      BaseOwner: Bases_Owners;
      BasePopulation: Natural;
   begin
      -- Save new game configuration
      NewGameSettings :=
        (PlayerName => CharName, PlayerGender => Gender, ShipName => ShipName);
      SaveConfig;
      -- Set game statistics
      ClearGameStats;
      -- Set Game time
      GameDate :=
        (Year => 1600, Month => 3, Day => 1, Hour => 8, Minutes => 0);
      -- Generate world
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0,
               Visited => False,
               EventIndex => 0,
               MissionIndex => 0)));
      for I in SkyBases'Range loop
         loop
            ValidLocation := True;
            PosX := GetRandom(1, 1024);
            PosY := GetRandom(1, 1024);
            for J in -5 .. 5 loop
               TempX := Integer(PosX) + J;
               if TempX < 1 then
                  TempX := 1;
               end if;
               if TempX > 1024 then
                  TempX := 1024;
               end if;
               for K in -5 .. 5 loop
                  TempY := Integer(PosY) + K;
                  if TempY < 1 then
                     TempY := 1;
                  end if;
                  if TempY > 1024 then
                     TempY := 1024;
                  end if;
                  if SkyMap(TempX, TempY).BaseIndex > 0 then
                     ValidLocation := False;
                     exit;
                  end if;
               end loop;
               if not ValidLocation then
                  exit;
               end if;
            end loop;
            if SkyMap(Integer(PosX), Integer(PosY)).BaseIndex > 0 then
               ValidLocation := False;
            end if;
            exit when ValidLocation;
         end loop;
         SkyMap(Integer(PosX), Integer(PosY)) :=
           (BaseIndex => I,
            Visited => False,
            EventIndex => 0,
            MissionIndex => 0);
         BasePopulation := GetRandom(10, 500);
         case GetRandom(1, 100) is
            when 1 .. 94 =>
               BaseOwner := Poleis;
               BaseReputation := 0;
            when 95 =>
               BaseOwner := Independent;
               case GetRandom(1, 100) is
                  when 1 .. 95 =>
                     BaseReputation := 0;
                  when 96 =>
                     BaseReputation := -1;
                  when 97 =>
                     BaseReputation := -2;
                  when 98 =>
                     BaseReputation := -3;
                  when 99 =>
                     BaseReputation := 1;
                  when 100 =>
                     BaseReputation := 2;
                  when others =>
                     null;
               end case;
            when 96 =>
               BaseOwner := Abandoned;
               BaseReputation := 0;
               BasePopulation := 0;
            when 97 =>
               BaseOwner := Pirates;
               BaseReputation := -10;
            when 98 =>
               BaseOwner := Undead;
               BaseReputation := -100;
            when 99 =>
               BaseOwner := Drones;
               BaseReputation := -100;
            when 100 =>
               BaseOwner := Inquisition;
               BaseReputation := -50;
            when others =>
               null;
         end case;
         SkyBases(I) :=
           (Name => GenerateBaseName,
            Visited => (0, 0, 0, 0, 0),
            SkyX => Integer(PosX),
            SkyY => Integer(PosY),
            BaseType => Bases_Types'Val(GetRandom(0, 3)),
            Population => BasePopulation,
            RecruitDate => (0, 0, 0, 0, 0),
            Recruits => TmpRecruits,
            Known => False,
            AskedForBases => False,
            AskedForEvents => (0, 0, 0, 0, 0),
            Reputation => (BaseReputation, 0),
            MissionsDate => (0, 0, 0, 0, 0),
            Missions => TmpMissions,
            Owner => BaseOwner);
      end loop;
      -- Place player ship in random large base
      loop
         RandomBase := GetRandom(1, 1024);
         exit when SkyBases(RandomBase).Population > 299 and
           SkyBases(RandomBase).Owner = Poleis;
      end loop;
      -- Create player ship
      for I in ProtoShips_List.Iterate loop
         if ProtoShips_List(I).Index = PlayerShipIndex then
            ShipIndex := ProtoShips_Container.To_Index(I);
            exit;
         end if;
      end loop;
      PlayerShip :=
        CreateShip
          (ShipIndex,
           ShipName,
           SkyBases(Integer(RandomBase)).SkyX,
           SkyBases(Integer(RandomBase)).SkyY,
           DOCKED);
      -- Add player to ship
      TmpSkills.Append(New_Item => (4, 5, 0));
      PlayerShip.Crew.Prepend
      (New_Item =>
         (Name => CharName,
          Gender => Gender,
          Health => 100,
          Tired => 0,
          Skills => TmpSkills,
          Hunger => 0,
          Thirst => 0,
          Order => Talk,
          PreviousOrder => Rest,
          OrderTime => 15,
          Orders => (0, 0, 0, 1, 1, 1, 2, 1, 1)));
      for Module of PlayerShip.Modules loop
         if Module.Owner > 0 then
            Module.Owner := Module.Owner + 1;
         end if;
         if Modules_List(Module.ProtoIndex).MType = CABIN and
           Module.Owner = 0 and
           not CabinAssigned then
            Module.Name := CharName & To_Unbounded_String("'s Cabin");
            Module.Owner := 1;
            CabinAssigned := True;
         end if;
      end loop;
      -- Set current map field/sky base info
      SkyBases(Integer(RandomBase)).Visited := GameDate;
      SkyBases(Integer(RandomBase)).Known := True;
      SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited := True;
      GenerateRecruits(Integer(RandomBase));
      GenerateMissions(Integer(RandomBase));
   end NewGame;

   procedure UpdateGame(Minutes: Positive) is
      AddedHours, AddedMinutes: Natural;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      TiredPoints: Natural := 0;
      NeedCleaning: Boolean := False;
   begin
      for I in 1 .. Minutes loop
         if ((GameDate.Minutes + I) rem 15) = 0 then
            TiredPoints := TiredPoints + 1;
         end if;
      end loop;
      -- Update game time
      AddedMinutes := Minutes rem 60;
      AddedHours := Minutes / 60;
      GameDate.Minutes := GameDate.Minutes + AddedMinutes;
      if GameDate.Minutes > 59 then
         GameDate.Minutes := GameDate.Minutes - 60;
         GameDate.Hour := GameDate.Hour + 1;
      end if;
      GameDate.Hour := GameDate.Hour + AddedHours;
      if GameDate.Hour > 23 then
         GameDate.Hour := GameDate.Hour - 24;
         GameDate.Day := GameDate.Day + 1;
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = CABIN and
              Module.Current_Value > 0 then
               Module.Current_Value := Module.Current_Value - 1;
               NeedCleaning := True;
            end if;
         end loop;
         if NeedCleaning then
            UpdateOrders;
         end if;
      end if;
      if GameDate.Day > 30 then
         GameDate.Day := 1;
         GameDate.Month := GameDate.Month + 1;
      end if;
      if GameDate.Month > 12 then
         GameDate.Month := 1;
         GameDate.Year := GameDate.Year + 1;
      end if;
      -- Update crew
      UpdateCrew(Minutes, TiredPoints);
      -- Repair ship (if needed)
      RepairShip(Minutes);
      -- Craft items
      Manufacturing(Minutes);
      -- Upgrade ship module
      UpgradeShip(Minutes);
      -- Update base
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Visited.Year = 0 then
            GameStats.BasesVisited := GameStats.BasesVisited + 1;
         end if;
         SkyBases(BaseIndex).Visited := GameDate;
         if not SkyBases(BaseIndex).Known then
            SkyBases(BaseIndex).Known := True;
            AddMessage
              ("You discovered base " &
               To_String(SkyBases(BaseIndex).Name) &
               ".",
               OtherMessage);
         end if;
         UpdatePopulation(BaseIndex);
         GenerateRecruits(BaseIndex);
         GenerateMissions(BaseIndex);
         UpdateOrders;
      end if;
      -- Update map cell
      if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited = False then
         GameStats.MapVisited := GameStats.MapVisited + 1;
         SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited := True;
      end if;
      -- Update events
      UpdateEvents(Minutes);
      -- Update accepted missions
      UpdateMissions(Minutes);
   end UpdateGame;

   function LoadData return Boolean is
      DataFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex, StartIndex, EndIndex, Amount: Natural;
      FieldsNames: constant array(1 .. 29) of Unbounded_String :=
        (To_Unbounded_String("BasesSyllablesPre"),
         To_Unbounded_String("BasesSyllablesStart"),
         To_Unbounded_String("BasesSyllablesEnd"),
         To_Unbounded_String("BasesSyllablesPost"),
         To_Unbounded_String("MaleSyllablesStart"),
         To_Unbounded_String("MaleSyllablesMiddle"),
         To_Unbounded_String("MaleSyllablesEnd"),
         To_Unbounded_String("FemaleSyllablesEnd"),
         To_Unbounded_String("SkillsNames"),
         To_Unbounded_String("ItemsTypes"),
         To_Unbounded_String("MaleVocals"),
         To_Unbounded_String("MaleConsonants"),
         To_Unbounded_String("FemaleSyllablesStart"),
         To_Unbounded_String("FemaleSyllablesMiddle"),
         To_Unbounded_String("FemaleVocals"),
         To_Unbounded_String("ShipSyllablesStart"),
         To_Unbounded_String("ShipSyllablesMiddle"),
         To_Unbounded_String("ShipSyllablesEnd"),
         To_Unbounded_String("RepairTools"),
         To_Unbounded_String("CleaningTools"),
         To_Unbounded_String("HealingTools"),
         To_Unbounded_String("PlayerShipIndex"),
         To_Unbounded_String("AlchemyTools"),
         To_Unbounded_String("DrinksType"),
         To_Unbounded_String("CorpseIndex"),
         To_Unbounded_String("MissionItemsType"),
         To_Unbounded_String("FoodTypes"),
         To_Unbounded_String("FuelType"),
         To_Unbounded_String("MoneyIndex"));
   begin
      if BaseSyllablesStart.Length > 0 then
         return True;
      end if;
      if not Exists("data/game.dat") then
         return False;
      end if;
      Open(DataFile, In_File, "data/game.dat");
      while not End_Of_File(DataFile) loop
         RawData := To_Unbounded_String(Get_Line(DataFile));
         EqualIndex := Index(RawData, "=");
         FieldName := Head(RawData, EqualIndex - 2);
         Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
         for I in FieldsNames'Range loop
            if FieldName = FieldsNames(I) then
               StartIndex := 1;
               Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
               for J in 1 .. Amount loop
                  EndIndex := Index(Value, ", ", StartIndex);
                  if EndIndex = 0 then
                     EndIndex := Length(Value) + 1;
                  end if;
                  case I is
                     when 1 =>
                        BaseSyllablesPre.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 2 =>
                        BaseSyllablesStart.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 3 =>
                        BaseSyllablesEnd.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 4 =>
                        BaseSyllablesPost.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 5 =>
                        MaleSyllablesStart.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 6 =>
                        MaleSyllablesMiddle.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 7 =>
                        MaleSyllablesEnd.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 8 =>
                        FemaleSyllablesEnd.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 9 =>
                        Skills_Names.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 10 =>
                        Items_Types.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 11 =>
                        MaleVocals.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 12 =>
                        MaleConsonants.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 13 =>
                        FemaleSyllablesStart.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 14 =>
                        FemaleSyllablesMiddle.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 15 =>
                        FemaleVocals.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 16 =>
                        ShipSyllablesStart.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 17 =>
                        ShipSyllablesMiddle.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 18 =>
                        ShipSyllablesEnd.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 19 =>
                        RepairTools := Value;
                     when 20 =>
                        CleaningTools := Value;
                     when 21 =>
                        HealingTools := Value;
                     when 22 =>
                        PlayerShipIndex := Value;
                     when 23 =>
                        AlchemyTools := Value;
                     when 24 =>
                        DrinksType := Value;
                     when 25 =>
                        CorpseIndex := Value;
                     when 26 =>
                        MissionItemsType := Value;
                     when 27 =>
                        FoodTypes(J) :=
                          Unbounded_Slice(Value, StartIndex, EndIndex - 1);
                     when 28 =>
                        FuelType := Value;
                     when 29 =>
                        MoneyIndex := Value;
                  end case;
                  StartIndex := EndIndex + 2;
               end loop;
               exit;
            end if;
         end loop;
      end loop;
      Close(DataFile);
      return True;
   end LoadData;

end Game;
