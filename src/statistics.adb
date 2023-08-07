--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Statistics is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Game_Stats is record
      Bases_Visited: Integer;
      Map_Visited: Integer;
      Distance_Traveled: Integer;
      Accepted_Missions: Integer;
      Points: Integer;
   end record;
   type Nim_Statistics_Data is record
      Index: chars_ptr;
      Amount: Integer;
   end record;
   type Nim_Stats_List is array(0 .. 511) of Nim_Statistics_Data;
   --## rule on TYPE_INITIAL_VALUES

   procedure Get_Game_Stats is
      Nim_Stats: constant Nim_Game_Stats :=
        (Bases_Visited => Game_Stats.Bases_Visited,
         Map_Visited => Game_Stats.Map_Visited,
         Distance_Traveled => Game_Stats.Distance_Traveled,
         Accepted_Missions => Game_Stats.Accepted_Missions,
         Points => Game_Stats.Points);
      procedure Get_Ada_Game_Stats(Stats: Nim_Game_Stats) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGameStats";
   begin
      Get_Ada_Game_Stats(Stats => Nim_Stats);
   end Get_Game_Stats;

   procedure Set_Game_Stats is
      --## rule off IMPROPER_INITIALIZATION
      Temp_Stats: Nim_Game_Stats :=
        (Bases_Visited => 0, Map_Visited => 0, Distance_Traveled => 0,
         Accepted_Missions => 0, Points => 0);
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Game_Stats(Stats: out Nim_Game_Stats) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameStats";
   begin
      Set_Ada_Game_Stats(Stats => Temp_Stats);
      Game_Stats.Bases_Visited := Temp_Stats.Bases_Visited;
      Game_Stats.Map_Visited := Temp_Stats.Map_Visited;
      Game_Stats.Distance_Traveled := Temp_Stats.Distance_Traveled;
      Game_Stats.Accepted_Missions := Temp_Stats.Accepted_Missions;
      Game_Stats.Points := Temp_Stats.Points;
   end Set_Game_Stats;

   procedure Get_Game_Stats_List(Name: String) is
      Nim_List: Nim_Stats_List := (others => <>);
      procedure Get_Ada_Game_Stats_List
        (N: chars_ptr; Stats_List: Nim_Stats_List) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGameStatsList";
   begin
      if Name = "craftingOrders" then
         Get_Crafting_Orders_Loop :
         for I in
           Game_Stats.Crafting_Orders.First_Index ..
             Game_Stats.Crafting_Orders.Last_Index loop
            Nim_List(I - 1) :=
              (Index =>
                 New_String
                   (Str =>
                      To_String
                        (Source => Game_Stats.Crafting_Orders(I).Index)),
               Amount => Game_Stats.Crafting_Orders(I).Amount);
         end loop Get_Crafting_Orders_Loop;
      elsif Name = "finishedGoals" then
         Get_Finished_Goals_Loop :
         for I in
           Game_Stats.Finished_Goals.First_Index ..
             Game_Stats.Finished_Goals.Last_Index loop
            Nim_List(I - 1) :=
              (Index =>
                 New_String
                   (Str =>
                      To_String(Source => Game_Stats.Finished_Goals(I).Index)),
               Amount => Game_Stats.Finished_Goals(I).Amount);
         end loop Get_Finished_Goals_Loop;
      elsif Name = "finishedMissions" then
         Get_Finished_Missions_Loop :
         for I in
           Game_Stats.Finished_Missions.First_Index ..
             Game_Stats.Finished_Missions.Last_Index loop
            Nim_List(I - 1) :=
              (Index =>
                 New_String
                   (Str =>
                      To_String
                        (Source => Game_Stats.Finished_Missions(I).Index)),
               Amount => Game_Stats.Finished_Missions(I).Amount);
         end loop Get_Finished_Missions_Loop;
      elsif Name = "killedMobs" then
         Get_Killed_Mobs_Loop :
         for I in
           Game_Stats.Killed_Mobs.First_Index ..
             Game_Stats.Killed_Mobs.Last_Index loop
            Nim_List(I - 1) :=
              (Index =>
                 New_String
                   (Str =>
                      To_String(Source => Game_Stats.Killed_Mobs(I).Index)),
               Amount => Game_Stats.Killed_Mobs(I).Amount);
         end loop Get_Killed_Mobs_Loop;
      end if;
      Get_Ada_Game_Stats_List
        (N => New_String(Str => Name), Stats_List => Nim_List);
   end Get_Game_Stats_List;

   procedure Set_Game_Stats_List(Name: String) is
      use Interfaces.C;

      --## rule off IMPROPER_INITIALIZATION
      Nim_List: Nim_Stats_List := (others => <>);
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Game_Stats_List
        (N: chars_ptr; Stats_List: out Nim_Stats_List) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameStatsList";
   begin
      Set_Ada_Game_Stats_List
        (N => New_String(Str => Name), Stats_List => Nim_List);
      if Name = "craftingOrders" then
         Game_Stats.Crafting_Orders.Clear;
         Set_Crafting_Orders_Loop :
         for Order of Nim_List loop
            exit Set_Crafting_Orders_Loop when Strlen(Item => Order.Index) = 0;
            Game_Stats.Crafting_Orders.Append
              (New_Item =>
                 (Index =>
                    To_Unbounded_String(Source => Value(Item => Order.Index)),
                  Amount => Order.Amount));
         end loop Set_Crafting_Orders_Loop;
      elsif Name = "finishedGoals" then
         Game_Stats.Finished_Goals.Clear;
         Set_Finished_Goals_Loop :
         for Goal of Nim_List loop
            exit Set_Finished_Goals_Loop when Strlen(Item => Goal.Index) = 0;
            Game_Stats.Finished_Goals.Append
              (New_Item =>
                 (Index =>
                    To_Unbounded_String(Source => Value(Item => Goal.Index)),
                  Amount => Goal.Amount));
         end loop Set_Finished_Goals_Loop;
      elsif Name = "finishedMissions" then
         Game_Stats.Finished_Missions.Clear;
         Set_Finished_Missions_Loop :
         for Mission of Nim_List loop
            exit Set_Finished_Missions_Loop when Strlen
                (Item => Mission.Index) =
              0;
            Game_Stats.Finished_Missions.Append
              (New_Item =>
                 (Index =>
                    To_Unbounded_String
                      (Source => Value(Item => Mission.Index)),
                  Amount => Mission.Amount));
         end loop Set_Finished_Missions_Loop;
      elsif Name = "killedMobs" then
         Game_Stats.Killed_Mobs.Clear;
         Set_Killed_Mobs_Loop :
         for Mob of Nim_List loop
            exit Set_Killed_Mobs_Loop when Strlen(Item => Mob.Index) = 0;
            Game_Stats.Killed_Mobs.Append
              (New_Item =>
                 (Index =>
                    To_Unbounded_String(Source => Value(Item => Mob.Index)),
                  Amount => Mob.Amount));
         end loop Set_Killed_Mobs_Loop;
      end if;
   end Set_Game_Stats_List;

   procedure Update_Destroyed_Ships(Ship_Name: Tiny_String.Bounded_String) is
      procedure Update_Ada_Destroyed_Ships(S_Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaDestroyedShips";
   begin
      Get_Game_Stats;
      Get_Game_Stats_List(Name => "destroyedShips");
      Update_Ada_Destroyed_Ships
        (S_Name => New_String(Str => To_String(Source => Ship_Name)));
      Set_Game_Stats_List(Name => "destroyedShips");
      Set_Game_Stats;
   end Update_Destroyed_Ships;

   procedure Clear_Game_Stats is
      procedure Clear_Ada_Game_Stats with
         Import => True,
         Convention => C,
         External_Name => "clearAdaGameStats";
   begin
      Clear_Ada_Game_Stats;
      Set_Game_Stats;
      Game_Stats.Destroyed_Ships.Clear;
      Game_Stats.Crafting_Orders.Clear;
      Game_Stats.Finished_Missions.Clear;
      Game_Stats.Finished_Goals.Clear;
      Game_Stats.Killed_Mobs.Clear;
   end Clear_Game_Stats;

   procedure Update_Finished_Goals(Index: Unbounded_String) is
      procedure Update_Ada_Finished_Goals(I: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaFinishedGoals";
   begin
      Get_Game_Stats;
      Get_Game_Stats_List(Name => "finishedGoals");
      Update_Ada_Finished_Goals
        (I => New_String(Str => To_String(Source => Index)));
      Set_Game_Stats_List(Name => "finishedGoals");
      Set_Game_Stats;
   end Update_Finished_Goals;

   procedure Update_Finished_Missions(M_Type: Unbounded_String) is
      procedure Update_Ada_Finished_Missions(M_T: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaFinishedMissions";
   begin
      Get_Game_Stats;
      Get_Game_Stats_List(Name => "finishedMissions");
      Update_Ada_Finished_Missions
        (M_T => New_String(Str => To_String(Source => M_Type)));
      Set_Game_Stats_List(Name => "finishedMissions");
      Set_Game_Stats;
   end Update_Finished_Missions;

   procedure Update_Crafting_Orders(Index: Tiny_String.Bounded_String) is
      procedure Update_Ada_Crafting_Orders(I: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaCraftingOrders";
   begin
      Get_Game_Stats;
      Get_Game_Stats_List(Name => "craftingOrders");
      Update_Ada_Crafting_Orders
        (I => New_String(Str => Tiny_String.To_String(Source => Index)));
      Set_Game_Stats_List(Name => "craftingOrders");
      Set_Game_Stats;
   end Update_Crafting_Orders;

   procedure Update_Killed_Mobs
     (Mob: Member_Data; Fraction_Name: Unbounded_String) is
      procedure Update_Ada_Killed_Mobs
        (M: Nim_Member_Data; F_Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaKilledMobs";
   begin
      Get_Game_Stats;
      Get_Game_Stats_List(Name => "killedMobs");
      Update_Ada_Killed_Mobs
        (M => Member_To_Nim(Member => Mob),
         F_Name => New_String(Str => To_String(Source => Fraction_Name)));
      Set_Game_Stats_List(Name => "killedMobs");
      Set_Game_Stats;
   end Update_Killed_Mobs;

   function Get_Game_Points return Natural is
      function Get_Ada_Game_Points return Natural with
         Import => True,
         Convention => C,
         External_Name => "getAdaGamePoints";
   begin
      Get_Game_Stats;
      return Get_Ada_Game_Points;
   end Get_Game_Points;

end Statistics;
