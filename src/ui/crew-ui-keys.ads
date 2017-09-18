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

with Game; use Game;

package Crew.UI.Keys is

   function CrewInfoKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates; -- Handle keys in crew info menu
   function CrewOrdersKeys
     (Key: Key_Code) return GameStates; -- Handle keys in crew orders menu
   function CrewOrdersAllKeys
     (Key: Key_Code)
     return GameStates; -- Handle keys in orders for all crew menu
   function OrdersPrioritiesKeys
     (Key: Key_Code)
     return GameStates; -- Handle keys in crew orders priorities menu
   function InventoryKeys
     (Key: Key_Code)
     return GameStates; -- Handle keys in crew member inventory view

end Crew.UI.Keys;
