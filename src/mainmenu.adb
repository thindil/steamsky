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
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Help; use Help;
with Items; use Items;
with UserInterface; use UserInterface;

package body MainMenu is

    CharName : String(1..12);
    ShipName : String(1..12);

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

    procedure ShowNewGameMenu(Key : Key_Code) is
        NewGameWindow : Window;
        Visibility : Cursor_Visibility := Normal;
    begin
        if Key = Character'Pos('c') or Key = Character'Pos('C') then
            CharName := "            ";
        elsif Key = Character'Pos('h') or Key = Character'Pos('H') then
            ShipName := "            ";
        end if;
        NewGameWindow := Create(6, 31, (Lines / 3), (Columns / 3));
        Box(NewGameWindow);
        Move_Cursor(Win => NewGameWindow, Line => 1, Column => 1);
        Add(Win => NewGameWindow, Str => "Character Name: " & CharName);
        Change_Attributes(Win => NewGameWindow, Line => 1, Column => 1,
            Count => 1, Color => 1);
        Move_Cursor(Win => NewGameWindow, Line => 2, Column => 1);
        Add(Win => NewGameWindow, Str => "Ship Name: " & ShipName);
        Change_Attributes(Win => NewGameWindow, Line => 2, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Win => NewGameWindow, Line => 4, Column => 5);
        Add(Win => NewGameWindow, Str => "[Quit] [Start]");
        Change_Attributes(Win => NewGameWindow, Line => 4, Column => 6,
            Count => 1, Color => 1);
        Change_Attributes(Win => NewGameWindow, Line => 4, Column => 13,
            Count => 1, Color => 1);
        if Key /= KEY_NONE then
            if Key = Character'Pos('c') or Key = Character'Pos('C') then
                Set_Echo_Mode(True);
                Set_Cursor_Visibility(Visibility);
                Move_Cursor(Win => NewGameWindow, Line => 1, Column => 17);
                Get(Win => NewGameWindow, Str => CharName, Len => 12);
                if CharName = "            " then
                    CharName := "Laeran      ";
                end if;
            elsif Key = Character'Pos('h') or Key = Character'Pos('H') then
                Set_Echo_Mode(True);
                Set_Cursor_Visibility(Visibility);
                Move_Cursor(Win => NewGameWindow, Line => 2, Column => 12);
                Get(Win => NewGameWindow, Str => ShipName, Len => 12);
                if ShipName = "            " then
                    ShipName := "Hawk        ";
                end if;
            end if;
            if Key = Character'Pos('c') or Key = Character'Pos('C') or Key = Character'Pos('h') or Key = Character'Pos('H') then
                Erase;
                ShowMainMenu;
                Refresh_Without_Update;
                ShowNewGameMenu(KEY_NONE);
                Update_Screen;
                return;
            end if;
        end if;
        Refresh(NewGameWindow);
    end ShowNewGameMenu;

    function MainMenuKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Quit game
                return Quit;
            when Character'Pos('n') | Character'Pos('N') => -- New game
                CharName := "Laeran      ";
                ShipName := "Hawk        ";
                ShowNewGameMenu(KEY_NONE);
                Update_Screen;
                return New_Game;
            when Character'Pos('l') | Character'Pos('L') => -- Load game
                if Exists("data/savegame.dat") then
                    LoadHelp;
                    LoadItems;
                    if LoadGame then
                        DrawGame(Sky_Map_View);
                        return Sky_Map_View;
                    else
                        ShowDialog("This saved game is incompatible with this version of game and can't be loaded.");
                        Update_Panels;
                        Update_Screen;
                        return Main_Menu;
                    end if;
                else
                    return Main_Menu;
                end if;
            when others => 
                return Main_Menu;
        end case;
    end MainMenuKeys;
    
    function NewGameKeys(Key : Key_Code) return GameStates is
        NewCharName, NewShipName : Unbounded_String;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu
                Erase;
                Refresh;
                ShowMainMenu;
                return Main_Menu;
            when Character'Pos('c') | Character'Pos('C') | Character'Pos('h') | Character'Pos('H') => -- Change character/ship name
                Erase;
                ShowMainMenu;
                Refresh_Without_Update;
                ShowNewGameMenu(Key);
                Update_Screen;
                return New_Game;
            when Character'Pos('s') | Character'Pos('S') => -- Start new game;
                LoadHelp;
                LoadItems;
                NewCharName := Trim(To_Unbounded_String(CharName), Ada.Strings.Both);
                NewShipName := Trim(To_Unbounded_String(ShipName), Ada.Strings.Both);
                NewGame(NewCharName, NewShipName);
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                return New_Game;
        end case;
    end NewGameKeys;

end MainMenu;
