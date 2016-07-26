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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Bases; use Bases;
with Messages; use Messages;

package body UserInterface is

    procedure ShowMainMenu is
        Visibility : Cursor_Visibility := Invisible;
    begin
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);

        -- Game logo
        Move_Cursor(Line => Lines / 5, Column => (Columns - 15) / 2);
        Add(Str => "STEAM SKY");
        Move_Cursor(Line => (Lines / 5) + 1, Column => (Columns - 12) / 2);
        -- Game version
        Add(Str => "ver 0.2");
        Move_Cursor(Line => (Lines / 3) + 1, Column => (Columns - 12) / 2);
        -- Game menu
        Add(Str => "New game");
        Change_Attributes(Line => (Lines / 3) + 1, Column => (Columns - 12) / 2,
            Count => 1, Color => 1);
        if Exists("data/savegame.dat") then
            Move_Cursor(Line => (Lines / 3) + 2, Column => (Columns - 12) / 2);
            Add(Str => "Load game");
            Change_Attributes(Line => (Lines / 3) + 2, Column => (Columns - 12) / 2,
                Count => 1, Color => 1);
            Move_Cursor(Line => (Lines / 3) + 3, Column => (Columns - 12) / 2);
            Add(Str => "Quit game");
            Change_Attributes(Line => (Lines / 3) + 3, Column => (Columns - 12) / 2,
                Count => 1, Color => 1);
        else
            Move_Cursor(Line => (Lines / 3) + 2, Column => (Columns - 12) / 2);
            Add(Str => "Quit game");
            Change_Attributes(Line => (Lines / 3) + 2, Column => (Columns - 12) / 2,
                Count => 1, Color => 1);
        end if;
        -- Copyright
        Move_Cursor(Line => Lines - 1, Column => (Columns - 20) / 2);
        Add(Str => "2016 Bartek thindil Jasicki");
    end ShowMainMenu;

    procedure ShowGameMenu(CurrentState : GameStates) is
        Speed : Unbounded_String;
    begin
        case CurrentState is
            when Sky_Map_View =>
                Add(Str => "[Ship] [Crew] [Orders] [Messages] [Help] [Quit]");
                Change_Attributes(Line => 0, Column => 1, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 8, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 24, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 35, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 42, Count => 1, Color => 1);
            when Ship_Info =>
                Add(Str => "Ship Informations [Quit]");
                Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            when Crew_Info =>
                Add(Str => "Crew Informations [Quit]");
                Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            when Messages_View =>
                Add(Str => "Last Messages [Quit]");
                Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
            when Trade_View =>
                Add(Str => "Trade with base [Quit]");
                Change_Attributes(Line => 0, Column => 17, Count => 1, Color => 1);
            when Help_View =>
                Add(Str => "Help [Quit]");
                Change_Attributes(Line => 0, Column => 6, Count => 1, Color => 1);
            when others =>
                null;
        end case;
        if CurrentState /= Help_View then
            case PlayerShip.Speed is
                when DOCKED =>
                    Speed := To_Unbounded_String("Docked");
                when FULL_STOP =>
                    Speed := To_Unbounded_String("Stopped");
                when QUARTER_SPEED =>
                    Speed := To_Unbounded_String("Quarter Speed");
                when HALF_SPEED =>
                    Speed := To_Unbounded_String("Half Speed");
                when FULL_SPEED =>
                    Speed := To_Unbounded_String("Full Speed");
            end case;
            Move_Cursor(Line => 0, Column => (Columns / 2));
            Add(Str => FormatedTime & "     Speed: " & To_String(Speed));
        end if;
    end ShowGameMenu;

    procedure ShowSpeedControl is
        SpeedWindow : Window;
    begin
        SpeedWindow := Create(10, 20, (Lines / 3), (Columns / 3));
        Box(SpeedWindow);
        if PlayerShip.Speed = DOCKED then
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 5);
            Add(Win => SpeedWindow, Str => "Undock");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 4, Column => 5);
            Add(Win => SpeedWindow, Str => "Trade");
            Change_Attributes(Win => SpeedWindow, Line => 4, Column => 5, 
                Count => 1, Color => 1);
        else
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
                Move_Cursor(Win => SpeedWindow, Line => 2, Column => 5);
                Add(Win => SpeedWindow, Str => "Dock");
                Change_Attributes(Win => SpeedWindow, Line => 2, Column => 5, 
                    Count => 1, Color => 1);
            end if;
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 5);
            Add(Win => SpeedWindow, Str => "Full stop");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 4, Column => 5);
            Add(Win => SpeedWindow, Str => "Quarter speed");
            Change_Attributes(Win => SpeedWindow, Line => 4, Column => 7, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 5, Column => 5);
            Add(Win => SpeedWindow, Str => "Half speed");
            Change_Attributes(Win => SpeedWindow, Line => 5, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 6, Column => 5);
            Add(Win => SpeedWindow, Str => "Full speed");
            Change_Attributes(Win => SpeedWindow, Line => 6, Column => 7, 
                Count => 1, Color => 1);
        end if;
        Move_Cursor(Win => SpeedWindow, Line => 8, Column => 5);
        Add(Win => SpeedWindow, Str => "Quit");
        Change_Attributes(Win => SpeedWindow, Line => 8, Column => 5, Count => 1,
            Color => 1);
        Refresh(SpeedWindow);
    end ShowSpeedControl;

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
    end ShowHelp;

    procedure DrawGame(CurrentState : GameStates) is
    begin
        Erase;
        Refresh;
        ShowGameMenu(CurrentState);
        case CurrentState is
            when Sky_Map_View =>
                ShowSkyMap;
            when Control_Speed =>
                ShowSpeedControl;
            when Ship_Info =>
                ShowShipInfo;
            when Crew_Info =>
                ShowCrewInfo(KEY_NONE);
            when Giving_Orders =>
                ShowOrdersMenu;
            when Messages_View =>
                ShowMessages;
            when Trade_View =>
                ShowTrade(KEY_NONE);
            when Help_View =>
                ShowHelp;
            when others =>
                null;
        end case;
    end DrawGame;

    function MainMenuKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Quit game
                return Quit;
            when Character'Pos('n') | Character'Pos('N') => -- New game
                -- Start new game
                NewGame;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('l') | Character'Pos('L') => -- Load game
                if Exists("data/savegame.dat") then
                    LoadGame;
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                else
                    return Main_Menu;
                end if;
            when others => 
                return Main_Menu;
        end case;
    end MainMenuKeys;
    
    function GameMenuKeys(CurrentState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu
                SaveGame;
                ClearMessages;
                Erase;
                Refresh;
                ShowMainMenu;
                return Main_Menu;
            when Character'Pos('s') | Character'Pos('S') => -- Ship info screen
                DrawGame(Ship_Info);
                return Ship_Info;
            when Character'Pos('c') | Character'Pos('C') => -- Crew info screen
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('m') | Character'Pos('M') => -- Messages list screen
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('h') | Character'Pos('H') => -- Help screen
                DrawGame(Help_View);
                return Help_View;
            when others =>
                return CurrentState;
        end case;
    end GameMenuKeys;

    function SpeedMenuKeys(OldState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('t') | Character'Pos('T') => -- Trade with base
                if PlayerShip.Speed = DOCKED then
                    DrawGame(Trade_View);
                    return Trade_View;
                else
                    return Control_Speed;
                end if;
            when Character'Pos('u') | Character'Pos('U') => -- Undock ship from base
                DockShip(False);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('d') | Character'Pos('D') => -- Dock ship to base
                DockShip(True);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('f') | Character'Pos('F') => -- Full stop
                ChangeShipSpeed(FULL_STOP);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('a') | Character'Pos('A') => -- Quarter speed
                ChangeShipSpeed(QUARTER_SPEED);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('h') | Character'Pos('H') => -- Half speed
                ChangeShipSpeed(HALF_SPEED);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('l') | Character'Pos('L') => -- Full speed
                ChangeShipSpeed(FULL_SPEED);
                DrawGame(Sky_Map_View);
                return OldState;
            when others =>
                return Control_Speed;
        end case;
    end SpeedMenuKeys;

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

end UserInterface;
