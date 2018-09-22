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
with Bases.Ship; use Bases.Ship;
with Bases.Cargo; use Bases.Cargo;
with Maps; use Maps;
with Ships; use Ships;
with Ships.Upgrade; use Ships.Upgrade;
with Ships.Repairs;
with Ships.Crew; use Ships.Crew;
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
with Goals; use Goals;
with Game.SaveLoad; use Game.SaveLoad;
with Mobs; use Mobs;

package body Game is

   PlayerIndex: Unbounded_String; -- Index of mob used for player starting data

   procedure NewGame
     (CharName, ShipName: Unbounded_String;
      Gender: Character) is
      PosX, PosY, RandomBase, ShipIndex, Amount: Positive;
      ValidLocation: Boolean;
      TempX, TempY, BaseReputation: Integer;
      TmpRecruits: Recruit_Container.Vector;
      TmpMissions: Mission_Container.Vector;
      CabinAssigned: Boolean := False;
      BaseOwner: Bases_Owners;
      BasePopulation: Natural;
      TmpCargo: BaseCargo_Container.Vector;
      TmpInventory: Inventory_Container.Vector;
      PlayerIndex2: constant Positive := FindProtoMob(PlayerIndex);
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
            Owner => BaseOwner,
            Cargo => TmpCargo);
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
           DOCKED,
           False);
      -- Add player to ship
      for Item of ProtoMobs_List(PlayerIndex2).Inventory loop
         if Item(3) > 0 then
            Amount := GetRandom(Item(2), Item(3));
         else
            Amount := Item(2);
         end if;
         TmpInventory.Append
         (New_Item =>
            (ProtoIndex => Item(1),
             Amount => Amount,
             Name => Null_Unbounded_String,
             Durability => 100));
      end loop;
      PlayerShip.Crew.Prepend
      (New_Item =>
         (Name => CharName,
          Gender => Gender,
          Health => 100,
          Tired => 0,
          Skills => ProtoMobs_List(PlayerIndex2).Skills,
          Hunger => 0,
          Thirst => 0,
          Order => ProtoMobs_List(PlayerIndex2).Order,
          PreviousOrder => Rest,
          OrderTime => 15,
          Orders => ProtoMobs_List(PlayerIndex2).Priorities,
          Attributes => ProtoMobs_List(PlayerIndex2).Attributes,
          Inventory => TmpInventory,
          Equipment => ProtoMobs_List(PlayerIndex2).Equipment));
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
      GenerateRecruits;
      GenerateMissions;
      GenerateCargo;
      -- Set player goal if not set yet
      if CurrentGoal.GType = RANDOM then
         CurrentGoal :=
           Goals_List
             (GetRandom(Goals_List.First_Index, Goals_List.Last_Index));
      end if;
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
              Module.Data(1) > 0 then
               Module.Data(1) := Module.Data(1) - 1;
               NeedCleaning := True;
            end if;
         end loop;
         if NeedCleaning then
            UpdateOrders(PlayerShip);
         end if;
         if PlayerShip.Speed = DOCKED then
            PayForDock;
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
      Ships.Repairs.RepairShip(Minutes);
      -- Craft items
      Manufacturing(Minutes);
      -- Upgrade ship module
      UpgradeShip(Minutes);
      -- Update base
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Visited.Year = 0 then
            GameStats.BasesVisited := GameStats.BasesVisited + 1;
            GameStats.Points := GameStats.Points + 1;
            UpdateGoal
              (VISIT,
               To_Unbounded_String
                 (Bases_Owners'Image(SkyBases(BaseIndex).Owner)));
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
         UpdatePopulation;
         GenerateRecruits;
         GenerateMissions;
         GenerateCargo;
         UpdatePrices;
         UpdateOrders(PlayerShip);
      end if;
      -- Update map cell
      if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited = False then
         GameStats.MapVisited := GameStats.MapVisited + 1;
         GameStats.Points := GameStats.Points + 1;
         UpdateGoal(DISCOVER, Null_Unbounded_String);
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
      FieldsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("BasesSyllablesPre"),
         To_Unbounded_String("BasesSyllablesStart"),
         To_Unbounded_String("BasesSyllablesEnd"),
         To_Unbounded_String("BasesSyllablesPost"),
         To_Unbounded_String("MaleSyllablesStart"),
         To_Unbounded_String("MaleSyllablesMiddle"),
         To_Unbounded_String("MaleSyllablesEnd"),
         To_Unbounded_String("FemaleSyllablesEnd"),
         To_Unbounded_String("Skills"),
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
         To_Unbounded_String("MoneyIndex"),
         To_Unbounded_String("TradersName"),
         To_Unbounded_String("AttributesNames"),
         To_Unbounded_String("ConditionName"),
         To_Unbounded_String("StrengthName"),
         To_Unbounded_String("HealingSkill"),
         To_Unbounded_String("PilotingSkill"),
         To_Unbounded_String("EngineeringSkill"),
         To_Unbounded_String("GunnerySkill"),
         To_Unbounded_String("TalkingSkill"),
         To_Unbounded_String("PerceptionSkill"),
         To_Unbounded_String("PlayerIndex"),
         To_Unbounded_String("HeadArmor"),
         To_Unbounded_String("ChestArmor"),
         To_Unbounded_String("ArmsArmor"),
         To_Unbounded_String("LegsArmor"),
         To_Unbounded_String("ShieldType"),
         To_Unbounded_String("WeaponType"),
         To_Unbounded_String("DodgeSkill"),
         To_Unbounded_String("UnarmedSkill"));
   begin
      if BaseSyllablesStart.Length > 0 then
         return True;
      end if;
      if not Exists(To_String(DataDirectory) & "game.dat") then
         return False;
      end if;
      Open(DataFile, In_File, To_String(DataDirectory) & "game.dat");
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
                        declare
                           ColonIndex, AttributeIndex: Positive;
                           AttributeName: Unbounded_String;
                        begin
                           ColonIndex := Index(Value, ":", StartIndex);
                           AttributeName :=
                             Unbounded_Slice
                               (Value,
                                ColonIndex + 1,
                                EndIndex - 1);
                           for I in Attributes_Names.Iterate loop
                              if Attributes_Names(I) = AttributeName then
                                 AttributeIndex :=
                                   UnboundedString_Container.To_Index(I);
                                 exit;
                              end if;
                           end loop;
                           Skills_List.Append
                           (New_Item =>
                              (Name =>
                                 Unbounded_Slice
                                   (Value,
                                    StartIndex,
                                    ColonIndex - 1),
                               Attribute => AttributeIndex));
                        end;
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
                        FoodTypes.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 28 =>
                        FuelType := Value;
                     when 29 =>
                        MoneyIndex := Value;
                     when 30 =>
                        TradersName := Value;
                     when 31 =>
                        Attributes_Names.Append
                        (New_Item =>
                           Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     when 32 =>
                        ConditionIndex :=
                          Attributes_Names.Find_Index(Item => Value);
                     when 33 =>
                        StrengthIndex :=
                          Attributes_Names.Find_Index(Item => Value);
                     when 34 =>
                        HealingSkill := FindSkillIndex(Value);
                     when 35 =>
                        PilotingSkill := FindSkillIndex(Value);
                     when 36 =>
                        EngineeringSkill := FindSkillIndex(Value);
                     when 37 =>
                        GunnerySkill := FindSkillIndex(Value);
                     when 38 =>
                        TalkingSkill := FindSkillIndex(Value);
                     when 39 =>
                        PerceptionSkill := FindSkillIndex(Value);
                     when 40 =>
                        PlayerIndex := Value;
                     when 41 =>
                        HeadArmor := Value;
                     when 42 =>
                        ChestArmor := Value;
                     when 43 =>
                        ArmsArmor := Value;
                     when 44 =>
                        LegsArmor := Value;
                     when 45 =>
                        ShieldType := Value;
                     when 46 =>
                        WeaponType := Value;
                     when 47 =>
                        DodgeSkill := FindSkillIndex(Value);
                     when 48 =>
                        UnarmedSkill := FindSkillIndex(Value);
                     when others =>
                        null;
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

   function DaysDifference(DateToCompare: Date_Record) return Natural is
   begin
      return (GameDate.Day + (30 * GameDate.Month) + (GameDate.Year * 360)) -
        (DateToCompare.Day +
         (30 * DateToCompare.Month) +
         (DateToCompare.Year * 360));
   end DaysDifference;

   procedure EndGame(Save: Boolean) is
   begin
      if Save then
         SaveGame;
      else
         if Exists(To_String(SaveDirectory) & "savegame.dat") then
            Delete_File(To_String(SaveDirectory) & "savegame.dat");
         end if;
      end if;
      ClearMessages;
      Events_List.Clear;
      ClearGameStats;
      Known_Recipes.Clear;
      ClearCurrentGoal;
   end EndGame;

   function FindSkillIndex(SkillName: Unbounded_String) return Positive is
   begin
      for I in Skills_List.Iterate loop
         if Skills_List(I).Name = SkillName then
            return SkillsData_Container.To_Index(I);
         end if;
      end loop;
      return 1;
   end FindSkillIndex;

end Game;
