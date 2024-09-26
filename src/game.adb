--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Bases;
with Events;
with Goals;
with Maps;
with Ships; use Ships;
with Statistics;

package body Game is

   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False) is
      use Bases;
      use Events;
      use Maps;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Update_Ada_Game(M, In_C: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaGame";
   begin
      Get_Game_Date;
      Set_Ship_In_Nim;
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Update_Ada_Game(M => Minutes, In_C => (if In_Combat then 1 else 0));
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
   end Update_Game;

   procedure End_Game(Save: Boolean) is
      use Goals;
      use Statistics;

      procedure End_Ada_Game(S: Integer) with
         Import => True,
         Convention => C,
         External_Name => "endAdaGame";
   begin
      End_Ada_Game(S => (if Save then 1 else 0));
      Clear_Game_Stats;
      Clear_Current_Goal;
   end End_Game;

   function Find_Skill_Index
     (Skill_Name: String) return SkillsData_Container.Extended_Index is
      function Find_Ada_Skill_Index
        (S_Name: chars_ptr) return SkillsData_Container.Extended_Index with
         Import => True,
         Convention => C,
         External_Name => "findAdaSkillIndex";
   begin
      return Find_Ada_Skill_Index(S_Name => New_String(Str => Skill_Name));
   end Find_Skill_Index;

   procedure Get_Game_Date is
      procedure Get_Ada_Game_Date
        (Year, Month, Day, Hour, Minutes: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGameDate";
   begin
      Get_Ada_Game_Date
        (Year => Game_Date.Year, Month => Game_Date.Month,
         Day => Game_Date.Day, Hour => Game_Date.Hour,
         Minutes => Game_Date.Minutes);
   end Get_Game_Date;

   procedure Set_Game_Date is
      procedure Set_Ada_Game_Date(Year, Month, Day, Hour, M: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameDate";
   begin
      Set_Ada_Game_Date
        (Year => Game_Date.Year, Month => Game_Date.Month,
         Day => Game_Date.Day, Hour => Game_Date.Hour, M => Game_Date.Minutes);
   end Set_Game_Date;

end Game;
