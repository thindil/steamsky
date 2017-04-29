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

package UserInterface.Keys is

   function GameMenuKeys
     (CurrentState: GameStates;
      Key: Key_Code) return GameStates; -- Handle keys in game menu
   function OrdersMenuKeys
     (OldState: GameStates;
      Key: Key_Code) return GameStates; -- Handle keys in orders menu
   function ConfirmKeys
     (OldState: GameStates;
      Key: Key_Code) return GameStates; -- Handle keys in confirm window
   function WaitMenuKeys
     (OldState: GameStates;
      Key: Key_Code) return GameStates; -- Handle keys in wait orders menu
   function WaitFormKeys
     (Key: Key_Code) return GameStates; -- Handle keys in wait x minutes form

end UserInterface.Keys;
