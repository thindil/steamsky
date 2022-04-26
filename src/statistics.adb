--    Copyright 2016-2022 Bartek thindil Jasicki
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Goals; use Goals;
with Ships; use Ships;
with Config; use Config;

package body Statistics is

   procedure Update_Destroyed_Ships(Ship_Name: Tiny_String.Bounded_String) is
      Updated: Boolean := False;
      Ship_Index: Proto_Ships_Container.Extended_Index := 0;
   begin
      Proto_Ships_Loop :
      for I in Proto_Ships_List.Iterate loop
         if Proto_Ships_List(I).Name = Ship_Name then
            Ship_Index := Proto_Ships_Container.To_Index(Position => I);
            Game_Stats.Points :=
              Game_Stats.Points + (Proto_Ships_List(I).Combat_Value / 10);
            exit Proto_Ships_Loop;
         end if;
      end loop Proto_Ships_Loop;
      if Ship_Index = 0 then
         return;
      end if;
      Destroyed_Ships_Loop :
      for DestroyedShip of Game_Stats.Destroyed_Ships loop
         if DestroyedShip.Index =
           To_Unbounded_String
             (Source => Trim(Source => Ship_Index'Img, Side => Left)) then
            DestroyedShip.Amount := DestroyedShip.Amount + 1;
            Updated := True;
            exit Destroyed_Ships_Loop;
         end if;
      end loop Destroyed_Ships_Loop;
      if not Updated then
         Game_Stats.Destroyed_Ships.Append
           (New_Item =>
              (Index =>
                 To_Unbounded_String
                   (Source => Trim(Source => Ship_Index'Img, Side => Left)),
               Amount => 1));
      end if;
   end Update_Destroyed_Ships;

   procedure Clear_Game_Stats is
   begin
      Game_Stats.Destroyed_Ships.Clear;
      Game_Stats.Bases_Visited := 1;
      Game_Stats.Map_Visited := 1;
      Game_Stats.Distance_Traveled := 0;
      Game_Stats.Crafting_Orders.Clear;
      Game_Stats.Accepted_Missions := 0;
      Game_Stats.Finished_Missions.Clear;
      Game_Stats.Finished_Goals.Clear;
      Game_Stats.Killed_Mobs.Clear;
      Game_Stats.Points := 0;
   end Clear_Game_Stats;

   procedure Update_Finished_Goals(Index: Unbounded_String) is
      Updated: Boolean := False;
   begin
      Find_Goal_Index_Loop :
      for Goal of Goals_List loop
         if Goal.Index = Index then
            Game_Stats.Points :=
              Game_Stats.Points + (Goal.Amount * Goal.Multiplier);
            exit Find_Goal_Index_Loop;
         end if;
      end loop Find_Goal_Index_Loop;
      Update_Finished_Goals_Loop :
      for FinishedGoal of Game_Stats.Finished_Goals loop
         if FinishedGoal.Index = Index then
            FinishedGoal.Amount := FinishedGoal.Amount + 1;
            Updated := True;
            exit Update_Finished_Goals_Loop;
         end if;
      end loop Update_Finished_Goals_Loop;
      if not Updated then
         Add_Finished_Goal_Loop :
         for Goal of Goals_List loop
            if Goal.Index = Index then
               Game_Stats.Finished_Goals.Append
                 (New_Item => (Index => Goal.Index, Amount => 1));
               exit Add_Finished_Goal_Loop;
            end if;
         end loop Add_Finished_Goal_Loop;
      end if;
   end Update_Finished_Goals;

   procedure Update_Finished_Missions(M_Type: Unbounded_String) is
      Updated: Boolean := False;
   begin
      Update_Finished_Missions_Loop :
      for FinishedMission of Game_Stats.Finished_Missions loop
         if FinishedMission.Index = M_Type then
            FinishedMission.Amount := FinishedMission.Amount + 1;
            Updated := True;
            exit Update_Finished_Missions_Loop;
         end if;
      end loop Update_Finished_Missions_Loop;
      if not Updated then
         Game_Stats.Finished_Missions.Append
           (New_Item => (Index => M_Type, Amount => 1));
      end if;
      Game_Stats.Points := Game_Stats.Points + 50;
   end Update_Finished_Missions;

   procedure Update_Crafting_Orders(Index: Tiny_String.Bounded_String) is
      Updated: Boolean := False;
   begin
      Update_Crafting_Loop :
      for CraftingOrder of Game_Stats.Crafting_Orders loop
         if To_String(Source => CraftingOrder.Index) =
           To_String(Source => Index) then
            CraftingOrder.Amount := CraftingOrder.Amount + 1;
            Updated := True;
            exit Update_Crafting_Loop;
         end if;
      end loop Update_Crafting_Loop;
      if not Updated then
         Game_Stats.Crafting_Orders.Append
           (New_Item =>
              (Index =>
                 To_Unbounded_String(Source => To_String(Source => Index)),
               Amount => 1));
      end if;
      Game_Stats.Points := Game_Stats.Points + 5;
   end Update_Crafting_Orders;

   procedure Update_Killed_Mobs
     (Mob: Member_Data; Fraction_Name: Unbounded_String) is
      Updated: Boolean := False;
   begin
      Get_Attribute_Points_Loop :
      for Attribute of Mob.Attributes loop
         Game_Stats.Points := Game_Stats.Points + Attribute.Level;
      end loop Get_Attribute_Points_Loop;
      Get_Skill_Points_Loop :
      for Skill of Mob.Skills loop
         Game_Stats.Points := Game_Stats.Points + Skill.Level;
      end loop Get_Skill_Points_Loop;
      Update_Killed_Mobs_Loop :
      for KilledMob of Game_Stats.Killed_Mobs loop
         if To_Lower(Item => To_String(Source => KilledMob.Index)) =
           To_String(Source => Fraction_Name) then
            KilledMob.Amount := KilledMob.Amount + 1;
            Updated := True;
            exit Update_Killed_Mobs_Loop;
         end if;
      end loop Update_Killed_Mobs_Loop;
      if not Updated then
         Game_Stats.Killed_Mobs.Append
           (New_Item =>
              (Index =>
                 To_Unbounded_String
                   (Source =>
                      To_Upper
                        (Item =>
                           Slice
                             (Source => Fraction_Name, Low => 1, High => 1)) &
                      Slice
                        (Source => Fraction_Name, Low => 2,
                         High => Length(Source => Fraction_Name))),
               Amount => 1));
      end if;
   end Update_Killed_Mobs;

   function Get_Game_Points return Natural is
      Malus_Indexes: constant array(1 .. 4) of Positive :=
        (1 => 2, 2 => 4, 3 => 5, 4 => 6);
      Difficulty_Values: constant array(1 .. 7) of Bonus_Type :=
        (1 => New_Game_Settings.Enemy_Damage_Bonus,
         2 => New_Game_Settings.Player_Damage_Bonus,
         3 => New_Game_Settings.Enemy_Melee_Damage_Bonus,
         4 => New_Game_Settings.Player_Melee_Damage_Bonus,
         5 => New_Game_Settings.Experience_Bonus,
         6 => New_Game_Settings.Reputation_Bonus,
         7 => New_Game_Settings.Upgrade_Cost_Bonus);
      Points_Bonus, Value: Float := 0.0;
   begin
      Get_Game_Points_Loop :
      for I in Difficulty_Values'Range loop
         Value := Float(Difficulty_Values(I));
         Update_Game_Points_Loop :
         for J in Malus_Indexes'Range loop
            if I = Malus_Indexes(J) then
               if Value < 1.0 then
                  Value := 1.0 + ((1.0 - Value) * 4.0);
               elsif Value > 1.0 then
                  Value := 1.0 - Value;
               end if;
               exit Update_Game_Points_Loop;
            end if;
         end loop Update_Game_Points_Loop;
         Points_Bonus := Points_Bonus + Value;
      end loop Get_Game_Points_Loop;
      Points_Bonus := Points_Bonus / Float(Difficulty_Values'Length);
      if Points_Bonus < 0.01 then
         Points_Bonus := 0.01;
      end if;
      return Natural(Float(Game_Stats.Points) * Points_Bonus);
   end Get_Game_Points;

end Statistics;
