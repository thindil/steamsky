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

with Ada.Strings;

package body Statistics is

   procedure Clear_Game_Stats is
      procedure Clear_Ada_Game_Stats with
         Import => True,
         Convention => C,
         External_Name => "clearAdaGameStats";
   begin
      Clear_Ada_Game_Stats;
   end Clear_Game_Stats;

   function Get_Game_Points return Natural is
      function Get_Ada_Game_Points return Natural with
         Import => True,
         Convention => C,
         External_Name => "getAdaGamePoints";
   begin
      return Get_Ada_Game_Points;
   end Get_Game_Points;

end Statistics;
