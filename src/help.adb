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

with UserInterface; use UserInterface;

package body Help is

    procedure ShowHelp is
        Line : Line_Position;
        Column : Column_Position;
    begin
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "At this moment, help is under heavy developement (as whole game). Below you can find few useful tips.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* Your ship starts docked to base. To move it, you must first undock from base. Hit 'o' key for open orders menu.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* To move your ship, you need to set it speed, have fuel (charcollum, which works as moneys too) and pilot and engineer on duty.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* To buy/sell items from bases you must first dock to base. All bases buy all items, but which items are sold, depends on base type.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* As you dock to stations, you discover they types and then they will be colored on sky map (eg. Agricultural bases are green). Unvisited bases are white. ");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* You can wait a moment without doing anything, by hit key 5 on keypad.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* You and your crew must eat and drink. If you dont have food or drink onboard, your crew will slowly die from dehydration or starvation.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* Travel time depends on skills of Pilot and Engineer (current members on this positions). Prices in bases depends on player skill Bartering - with higher skill you can sell items for more and buy for less.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* To escape from fight, you must set Pilot order on Escape and Engineer order on Full Speed, after few turns (depends how far enemy was) you should escape.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* Slow speed in combat helps your gunner hits enemies but also helps enemies hits you. If you want to have more chances to evade enemy attack, set speed to full speed. That same is with pilot orders - evade or escape order helps avoids attacks but also reduce your chance to hit enemies.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* Watch out on damage level for your ship hull and engine. If one of them will be destroyed, whole ship will be destroyed in explosion too and you die.");
        Get_Cursor_Position(Line => Line, Column => Column);
        Move_Cursor(Line => (Line + 1), Column => 2);
        Add(Str => "* To start manufacturing items, first you must have enough material and proper module installed on ship. Then you must set proper crafting order in Craft menu and give crew member order to start crafting.");
    end ShowHelp;

    function HelpKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                ShowHelp;
                return Help_View;
        end case;
    end HelpKeys;

end Help;
