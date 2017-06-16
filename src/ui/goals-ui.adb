--    Copyright 2017 Bartek thindil Jasicki
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

with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;

package body Goals.UI is

   GoalsMenu: Menu;
   MenuWindow: Window;

   procedure ShowGoalsList(GType: GoalTypes) is
   begin
      null;
   end ShowGoalsList;

   function GoalsMenuKeys(Key: Key_Code) return GameStates is
   begin
      return Goals_Menu;
   end GoalsMenuKeys;
end Goals.UI;
