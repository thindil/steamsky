--    Copyright 2016-2018 Bartek thindil Jasicki
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

with Ada.Directories; use Ada.Directories;
with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Bases; use Bases;
with Bases.Ship; use Bases.Ship;
with Bases.Cargo; use Bases.Cargo;
with Maps; use Maps;
with Ships; use Ships;
with Ships.Upgrade; use Ships.Upgrade;
with Ships.Repairs; use Ships.Repairs;
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
with Factions; use Factions;

package body Game is

   procedure NewGame
     (CharName, ShipName: Unbounded_String;
      Gender: Character;
      FactionIndex: Positive) is
      PosX, PosY, RandomBase, ShipIndex, Amount, FactionRoll: Positive;
      ValidLocation: Boolean;
      TempX, TempY, BaseReputation: Integer;
      TmpRecruits: Recruit_Container.Vector;
      TmpMissions: Mission_Container.Vector;
      CabinAssigned: Boolean := False;
      BaseOwner: Positive;
      BasePopulation: Natural;
      TmpCargo: BaseCargo_Container.Vector;
      TmpInventory: Inventory_Container.Vector;
      PlayerIndex2: Positive;
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
      -- Get player faction index
      PlayerFaction := Factions_List(FactionIndex).Index;
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
         FactionRoll := GetRandom(1, 100);
         for J in Factions_List.Iterate loop
            if (FactionRoll = Factions_List(J).SpawnChance(1)) or
              (FactionRoll > Factions_List(J).SpawnChance(1) and
               FactionRoll <= Factions_List(J).SpawnChance(2)) then
               BaseOwner := Factions_Container.To_Index(J);
               if Factions_List(J).Population(2) = 0 then
                  BasePopulation := Factions_List(J).Population(1);
               else
                  BasePopulation :=
                    GetRandom
                      (Factions_List(J).Population(1),
                       Factions_List(J).Population(2));
               end if;
               BaseReputation :=
                 GetReputation(PlayerFaction, Factions_List(J).Index);
               exit;
            end if;
         end loop;
         SkyBases(I) :=
           (Name => GenerateBaseName,
            Visited => (0, 0, 0, 0, 0),
            SkyX => Integer(PosX),
            SkyY => Integer(PosY),
            BaseType => Bases_Types'Val(GetRandom(0, 4)),
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
           SkyBases(RandomBase).Owner = FactionIndex;
      end loop;
      -- Create player ship
      for I in ProtoShips_List.Iterate loop
         if ProtoShips_List(I).Index =
           Factions_List(FactionIndex).PlayerShipIndex then
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
      PlayerIndex2 := FindProtoMob(Factions_List(FactionIndex).PlayerIndex);
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
          Equipment => ProtoMobs_List(PlayerIndex2).Equipment,
          Payment => (others => 0),
          ContractLength => -1,
          Morale => 100,
          Loyalty => 100));
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
      -- Set name of savegame
      GenerateSaveName;
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
         DailyPayment;
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
            UpdateGoal(VISIT, Factions_List(SkyBases(BaseIndex).Owner).Index);
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
      DataFile: File_Input;
      Reader: Tree_Reader;
      GameData: Document;
      NodesList: Node_List;
   begin
      if BaseSyllablesStart.Length > 0 then
         return True;
      end if;
      if not Exists(To_String(DataDirectory) & "game.dat") then
         return False;
      end if;
      Open(To_String(DataDirectory) & "game.dat", DataFile);
      Parse(Reader, DataFile);
      Close(DataFile);
      GameData := Get_Tree(Reader);
      NodesList := Child_Nodes(First_Child(GameData));
      for I in 0 .. Length(NodesList) - 1 loop
         if Node_Name(Item(NodesList, I)) = "basessyllablepre" then
            BaseSyllablesPre.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "basessyllablestart" then
            BaseSyllablesStart.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "basessyllableend" then
            BaseSyllablesEnd.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "basessyllablepost" then
            BaseSyllablesPost.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "malessyllablestart" then
            MaleSyllablesStart.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "malessyllablemiddle" then
            MaleSyllablesMiddle.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "malessyllableend" then
            MaleSyllablesEnd.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "malesvocal" then
            MaleVocals.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "malesconsonant" then
            MaleConsonants.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "femalessyllablestart" then
            FemaleSyllablesStart.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "femalessyllablemiddle" then
            FemaleSyllablesMiddle.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "femalessyllableend" then
            FemaleSyllablesEnd.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "femalesvocal" then
            FemaleVocals.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "shipssyllablestart" then
            ShipSyllablesStart.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "shipssyllablemiddle" then
            ShipSyllablesMiddle.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "shipssyllableend" then
            ShipSyllablesEnd.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "itemtype" then
            Items_Types.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "repairtools" then
            RepairTools :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "healingtools" then
            HealingTools :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "cleaningtools" then
            CleaningTools :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "alchemytools" then
            AlchemyTools :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "drinkstype" then
            DrinksType :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "corpseindex" then
            CorpseIndex :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "missionitemstype" then
            MissionItemsType :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "foodtype" then
            FoodTypes.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "fueltype" then
            FuelType :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "moneyindex" then
            MoneyIndex :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "tradersname" then
            TradersName :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "attribute" then
            Attributes_List.Append
            (New_Item =>
               (Name =>
                  To_Unbounded_String
                    (Get_Attribute(Item(NodesList, I), "name")),
                Description =>
                  To_Unbounded_String
                    (Node_Value(First_Child(Item(NodesList, I))))));
         elsif Node_Name(Item(NodesList, I)) = "skill" then
            for J in
              Attributes_List.First_Index .. Attributes_List.Last_Index loop
               if Attributes_List(J).Name =
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "attribute")) then
                  Skills_List.Append
                  (New_Item =>
                     (Name =>
                        To_Unbounded_String
                          (Get_Attribute(Item(NodesList, I), "name")),
                      Attribute => J,
                      Description =>
                        To_Unbounded_String
                          (Node_Value(First_Child(Item(NodesList, I))))));
                  exit;
               end if;
            end loop;
         elsif Node_Name(Item(NodesList, I)) = "conditionname" then
            for J in
              Attributes_List.First_Index .. Attributes_List.Last_Index loop
               if Attributes_List(J).Name =
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")) then
                  ConditionIndex := J;
                  exit;
               end if;
            end loop;
         elsif Node_Name(Item(NodesList, I)) = "strengthname" then
            for J in
              Attributes_List.First_Index .. Attributes_List.Last_Index loop
               if Attributes_List(J).Name =
                 To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")) then
                  StrengthIndex := J;
                  exit;
               end if;
            end loop;
         elsif Node_Name(Item(NodesList, I)) = "healingskill" then
            HealingSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "pilotingskill" then
            PilotingSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "engineeringskill" then
            EngineeringSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "gunneryskill" then
            GunnerySkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "talkingskill" then
            TalkingSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "perceptionskill" then
            PerceptionSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "headarmor" then
            HeadArmor :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "chestarmor" then
            ChestArmor :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "armsarmor" then
            ArmsArmor :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "legsarmor" then
            LegsArmor :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "shieldtype" then
            ShieldType :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "weapontype" then
            WeaponType :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "value"));
         elsif Node_Name(Item(NodesList, I)) = "dodgeskill" then
            DodgeSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         elsif Node_Name(Item(NodesList, I)) = "unarmedskill" then
            UnarmedSkill :=
              FindSkillIndex
                (To_Unbounded_String
                   (Get_Attribute(Item(NodesList, I), "value")));
         end if;
      end loop;
      Free(Reader);
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
         if Exists(To_String(SaveName)) then
            Delete_File(To_String(SaveName));
         end if;
      end if;
      ClearMessages;
      Events_List.Clear;
      ClearGameStats;
      Known_Recipes.Clear;
      ClearCurrentGoal;
      AcceptedMissions.Clear;
      SaveConfig;
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
