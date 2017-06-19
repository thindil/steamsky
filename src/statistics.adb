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

with Goals; use Goals;

package body Statistics is

   procedure UpdateDestroyedShips(ShipName: Unbounded_String) is
      Updated: Boolean := False;
   begin
      for DestroyedShip of GameStats.DestroyedShips loop
         if DestroyedShip.Index = ShipName then
            DestroyedShip.Amount := DestroyedShip.Amount + 1;
            Updated := True;
            exit;
         end if;
      end loop;
      if not Updated then
         GameStats.DestroyedShips.Append
         (New_Item => (Index => ShipName, Amount => 1));
      end if;
   end UpdateDestroyedShips;

   procedure ClearGameStats is
   begin
      GameStats.DestroyedShips.Clear;
      GameStats.BasesVisited := 1;
      GameStats.MapVisited := 1;
      GameStats.DistanceTraveled := 0;
      GameStats.CraftingOrders := 0;
      GameStats.AcceptedMissions := 0;
      GameStats.FinishedMissions := 0;
      GameStats.FinishedGoals.Clear;
   end ClearGameStats;

   procedure UpdateFinishedGoals(Index: Unbounded_String) is
      Updated: Boolean := False;
   begin
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

end Statistics;
