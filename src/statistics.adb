--    Copyright 2016-2019 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Goals; use Goals;
with Ships; use Ships;
with Config; use Config;

package body Statistics is

-- ****if* Statistics/UpdateDestroyedShips
-- SOURCE
   procedure UpdateDestroyedShips(ShipName: Unbounded_String) is
-- ****
      Updated: Boolean := False;
      ShipIndex: Unbounded_String;
   begin
      for I in ProtoShips_List.Iterate loop
         if ProtoShips_List(I).Name = ShipName then
            ShipIndex := ProtoShips_Container.Key(I);
            GameStats.Points :=
              GameStats.Points + (ProtoShips_List(I).CombatValue / 10);
            exit;
         end if;
      end loop;
      for DestroyedShip of GameStats.DestroyedShips loop
         if DestroyedShip.Index = ShipIndex then
            DestroyedShip.Amount := DestroyedShip.Amount + 1;
            Updated := True;
            exit;
         end if;
      end loop;
      if not Updated then
         GameStats.DestroyedShips.Append
           (New_Item => (Index => ShipIndex, Amount => 1));
      end if;
   end UpdateDestroyedShips;

-- ****if* Statistics/ClearGameStats
-- SOURCE
   procedure ClearGameStats is
-- ****
   begin
      GameStats.DestroyedShips.Clear;
      GameStats.BasesVisited := 1;
      GameStats.MapVisited := 1;
      GameStats.DistanceTraveled := 0;
      GameStats.CraftingOrders.Clear;
      GameStats.AcceptedMissions := 0;
      GameStats.FinishedMissions.Clear;
      GameStats.FinishedGoals.Clear;
      GameStats.KilledMobs.Clear;
      GameStats.Points := 0;
   end ClearGameStats;

-- ****if* Statistics/UpdateFinishedGoals
-- SOURCE
   procedure UpdateFinishedGoals(Index: Unbounded_String) is
-- ****
      Updated: Boolean := False;
   begin
      for Goal of Goals_List loop
         if Goal.Index = Index then
            GameStats.Points :=
              GameStats.Points + (Goal.Amount * Goal.Multiplier);
            exit;
         end if;
      end loop;
      for FinishedGoal of GameStats.FinishedGoals loop
         if FinishedGoal.Index = Index then
            FinishedGoal.Amount := FinishedGoal.Amount + 1;
            Updated := True;
            exit;
         end if;
      end loop;
      if not Updated then
         for Goal of Goals_List loop
            if Goal.Index = Index then
               GameStats.FinishedGoals.Append
                 (New_Item => (Index => Goal.Index, Amount => 1));
               exit;
            end if;
         end loop;
      end if;
   end UpdateFinishedGoals;

-- ****if* Statistics/UpdateFinishedMissions
-- SOURCE
   procedure UpdateFinishedMissions(MType: Unbounded_String) is
-- ****
      Updated: Boolean := False;
   begin
      for FinishedMission of GameStats.FinishedMissions loop
         if FinishedMission.Index = MType then
            FinishedMission.Amount := FinishedMission.Amount + 1;
            Updated := True;
            exit;
         end if;
      end loop;
      if not Updated then
         GameStats.FinishedMissions.Append
           (New_Item => (Index => MType, Amount => 1));
      end if;
      GameStats.Points := GameStats.Points + 50;
   end UpdateFinishedMissions;

-- ****if* Statistics/UpdateCraftingOrders
-- SOURCE
   procedure UpdateCraftingOrders(Index: Unbounded_String) is
-- ****
      Updated: Boolean := False;
   begin
      for CraftingOrder of GameStats.CraftingOrders loop
         if CraftingOrder.Index = Index then
            CraftingOrder.Amount := CraftingOrder.Amount + 1;
            Updated := True;
            exit;
         end if;
      end loop;
      if not Updated then
         GameStats.CraftingOrders.Append
           (New_Item => (Index => Index, Amount => 1));
      end if;
      GameStats.Points := GameStats.Points + 5;
   end UpdateCraftingOrders;

-- ****if* Statistics/UpdateKilledMobs
-- SOURCE
   procedure UpdateKilledMobs
     (Mob: Member_Data; FractionName: Unbounded_String) is
-- ****
      Updated: Boolean := False;
   begin
      for Attribute of Mob.Attributes loop
         GameStats.Points := GameStats.Points + Attribute(1);
      end loop;
      for Skill of Mob.Skills loop
         GameStats.Points := GameStats.Points + Skill(2);
      end loop;
      for KilledMob of GameStats.KilledMobs loop
         if To_Lower(To_String(KilledMob.Index)) = To_String(FractionName) then
            KilledMob.Amount := KilledMob.Amount + 1;
            Updated := True;
            exit;
         end if;
      end loop;
      if not Updated then
         GameStats.KilledMobs.Append
           (New_Item =>
              (Index =>
                 To_Unbounded_String
                   (To_Upper(Slice(FractionName, 1, 1)) &
                    Slice(FractionName, 2, Length(FractionName))),
               Amount => 1));
      end if;
   end UpdateKilledMobs;

-- ****if* Statistics/GetGamePoints
-- SOURCE
   function GetGamePoints return Natural is
-- ****
      MalusIndexes: constant array(Positive range <>) of Positive :=
        (2, 4, 5, 6);
      DifficultyValues: constant array(Positive range <>) of Float :=
        (NewGameSettings.EnemyDamageBonus, NewGameSettings.PlayerDamageBonus,
         NewGameSettings.EnemyMeleeDamageBonus,
         NewGameSettings.PlayerMeleeDamageBonus,
         NewGameSettings.ExperienceBonus, NewGameSettings.ReputationBonus,
         NewGameSettings.UpgradeCostBonus);
      PointsBonus, Value: Float := 0.0;
   begin
      for I in DifficultyValues'Range loop
         Value := DifficultyValues(I);
         for J in MalusIndexes'Range loop
            if I = MalusIndexes(J) then
               if Value < 1.0 then
                  Value := 1.0 + ((1.0 - Value) * 4.0);
               elsif Value > 1.0 then
                  Value := 1.0 - Value;
               end if;
               exit;
            end if;
         end loop;
         PointsBonus := PointsBonus + Value;
      end loop;
      PointsBonus := PointsBonus / Float(DifficultyValues'Length);
      if PointsBonus < 0.01 then
         PointsBonus := 0.01;
      end if;
      return Natural(Float(GameStats.Points) * PointsBonus);
   end GetGamePoints;

end Statistics;
