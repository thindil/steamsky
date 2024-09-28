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

with Interfaces.C.Strings;

package body Game is

   function Find_Skill_Index
     (Skill_Name: String) return SkillsData_Container.Extended_Index is
      use Interfaces.C.Strings;

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
