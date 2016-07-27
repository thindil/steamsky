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
with Game; use Game;

package UserInterface is
    
    procedure ShowMainMenu; -- Show main game menu
    procedure ShowGameMenu(CurrentState : GameStates); -- Show in-game menu
    procedure ShowDialog(Message : String); -- Show dialog window with message
    function HideDialog return Boolean; -- Hide dialog if visible, return True if dialog was hidden
    procedure DrawGame(CurrentState : GameStates); -- Draw game screen
    function MainMenuKeys(Key : Key_Code) return GameStates; -- Handle keys on main menu
    function GameMenuKeys(CurrentState : GameStates; Key : Key_Code) return GameStates; -- Handle keys in game menu
    function SpeedMenuKeys(OldState : GameStates; Key : Key_Code) return GameStates; -- Handle keys in speed control menu
    function HelpKeys(Key : Key_Code) return GameStates; -- Handle keys in help window
    
end UserInterface;
