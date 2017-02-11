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

package Combat.UI is
    
    procedure ShowCombat; -- Show combat screen
    function CombatKeys(Key : Key_Code) return GameStates; -- Handle keys on combat screen
    function CombatOrdersKeys(Key : Key_Code) return GameStates; -- Handle keys in combat orders menu
    function EnemyInfoKeys(Key : Key_Code) return GameStates; -- Handle keys in detailed enemy info

end Combat.UI;
