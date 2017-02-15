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
with Game; use Game;

package Crew.UI is

    procedure ShowCrewInfo; -- Show crew info
    procedure ShowOrdersMenu; -- Show menu with orders for crew
    procedure DismissMember; -- Dismiss selected crew member
    function CrewInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates; -- Handle keys in crew info menu
    function CrewOrdersKeys(Key : Key_Code) return GameStates; -- Handle keys in crew orders menu
    function CrewOrdersAllKeys(Key : Key_Code) return GameStates; -- Handle keys in orders for all crew menu
    function OrdersPrioritiesKeys(Key : Key_Code) return GameStates; -- Handle keys in crew orders priorities menu

end Crew.UI;
