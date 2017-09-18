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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;

package Crew.UI is

   procedure ShowCrewInfo; -- Show crew info
   procedure ShowOrdersMenu; -- Show menu with orders for crew
   procedure DismissMember; -- Dismiss selected crew member
   procedure ShowInventory; -- Show inventory of selected crew member

private
   CrewMenu, OrdersMenu, PrioritiesMenu: Menu;
   MenuWindow, MenuWindow2, SkillsPad: Window;
   MemberIndex, PriorityIndex: Positive := 1;
   NeedClean, NeedRepairs: Boolean := False;
   StartIndex, EndIndex: Integer := 0;

   procedure ShowMemberInfo; -- Show informations about selected crew member
   procedure ShowOrdersForAll; -- Show menu with orders for whole crew
   procedure ShowPrioritiesMenu; -- Show piorities settings for selected crew member
   procedure ShowItemInfo; -- Show informations about selected item in crew member inventory

end Crew.UI;
