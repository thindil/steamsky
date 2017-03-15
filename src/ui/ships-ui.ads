--    Copyright 2016 Bartek thindil Jasicki
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

package Ships.UI is

   ShipsMenu: Menu; -- Menu for ship modules/cargo

   procedure ShowShipForm
     (OptionText: String;
      MaxRange: Natural := 0); -- Show form to rename module/drop cargo
   function ShipFormKeys
     (Key: Key_Code;
      CurrentState: GameStates)
     return GameStates; -- Handle keys in rename/drop cargo form

end Ships.UI;
