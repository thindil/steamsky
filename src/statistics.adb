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
   type Nim_Statistics_Data is record
      Index: chars_ptr;
      Amount: Integer;
   end record;
   type Nim_Stats_List is array(0 .. 511) of Nim_Statistics_Data;
   --## rule on TYPE_INITIAL_VALUES

   procedure Set_Game_Stats is
   begin
      null;
   end Set_Game_Stats;

   procedure Get_Game_Stats_List(Name: String) is
   begin
      null;
   end Get_Game_Stats_List;

   procedure Set_Game_Stats_List(Name: String) is
   begin
      null;
   end Set_Game_Stats_List;

   procedure Update_Destroyed_Ships(Ship_Name: Tiny_String.Bounded_String) is
      procedure Update_Ada_Destroyed_Ships(S_Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaDestroyedShips";
   begin
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
   end Clear_Game_Stats;

   procedure Update_Finished_Goals(Index: Unbounded_String) is
      procedure Update_Ada_Finished_Goals(I: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaFinishedGoals";
   begin
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
      return Get_Ada_Game_Points;
   end Get_Game_Points;

   function Get_Game_Stats_Number(Name: String) return Natural is
      Value: Natural;
      procedure Set_Ada_Game_Stats_Number
        (N: chars_ptr; Stats_Value: out Natural) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameStatsNumber";
   begin
      Set_Ada_Game_Stats_Number
        (N => New_String(Str => Name), Stats_Value => Value);
      return Value;
   end Get_Game_Stats_Number;

   function Get_Game_Stats_List
     (Name: String) return Statistics_Container.Vector is
      use Interfaces.C;

      --## rule off IMPROPER_INITIALIZATION
      Nim_List: Nim_Stats_List := (others => <>);
      Ada_List: Statistics_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Game_Stats_List
        (N: chars_ptr; Stats_List: out Nim_Stats_List) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameStatsList";
   begin
      Set_Ada_Game_Stats_List
        (N => New_String(Str => Name), Stats_List => Nim_List);
      Set_List_Loop :
      for Item of Nim_List loop
         exit Set_List_Loop when Strlen(Item => Item.Index) = 0;
         Ada_List.Append
           (New_Item =>
              (Index =>
                 To_Unbounded_String(Source => Value(Item => Item.Index)),
               Amount => Item.Amount));
      end loop Set_List_Loop;
      return Ada_List;
   end Get_Game_Stats_List;

end Statistics;
