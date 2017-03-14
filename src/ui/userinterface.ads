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
with Game; use Game;

package UserInterface is

    OrdersMenu : Menu; -- Menu for orders/game menu
    MenuWindow : Window; -- Window for menu
    
    procedure ShowGameHeader(CurrentState : GameStates); -- Show in-game header
    procedure ShowDialog(Message : String); -- Show dialog window with message
    function HideDialog return Boolean; -- Hide dialog if visible, return True if dialog was hidden
    procedure DrawGame(CurrentState : GameStates); -- Draw game screen
    procedure ShowGameMenu; -- Show in game menu
    
end UserInterface;
