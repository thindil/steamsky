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
with Ada.Text_IO; use Ada.Text_IO;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Help; use Help;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Ships; use Ships;

package body MainMenu is

    CharName : String(1..12);
    ShipName : String(1..12);
    CharGender : Character;
    StartIndex : Integer := 0;
    EndIndex : Integer;
    LicensePad : Window := Null_Window;

    procedure ShowMainMenu is
        Visibility : Cursor_Visibility := Invisible;
        CurrentLine : Line_Position := 1;
    begin
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);

        -- Game logo
        Move_Cursor(Line => Lines / 5, Column => (Columns - 15) / 2);
        Add(Str => "STEAM SKY");
        -- Game version
        Move_Cursor(Line => (Lines / 5) + 2, Column => (Columns - 17) / 2);
        Add(Str => GameVersion);
        -- Game menu
        Move_Cursor(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2);
        Add(Str => "New game");
        Change_Attributes(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2,
            Count => 1, Color => 1);
        CurrentLine := CurrentLine + 1;
        if Exists("data/savegame.dat") then
            Move_Cursor(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2);
            Add(Str => "Load game");
            Change_Attributes(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2,
                Count => 1, Color => 1);
            CurrentLine := CurrentLine + 1;
        end if;
        Move_Cursor(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2);
        Add(Str => "News");
        Change_Attributes(Line => (Lines / 3) + CurrentLine, Column => ((Columns - 12) / 2) + 1,
            Count => 1, Color => 1);
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2);
        Add(Str => "License");
        Change_Attributes(Line => (Lines / 3) + CurrentLine, Column => ((Columns - 12) / 2) + 1,
            Count => 1, Color => 1);
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2);
        Add(Str => "Quit game");
        Change_Attributes(Line => (Lines / 3) + CurrentLine, Column => (Columns - 12) / 2,
            Count => 1, Color => 1);
        -- Copyright
        Move_Cursor(Line => Lines - 1, Column => (Columns - 20) / 2);
        Add(Str => "2016 Bartek thindil Jasicki");
    end ShowMainMenu;

    procedure ShowNewGameMenu(Key : Key_Code) is
        NewGameWindow : Window;
        Visibility : Cursor_Visibility := Normal;
        GenderKey : Key_Code;
        procedure RemoveSemicolons(Name : in out String) is
            NewName : Unbounded_String;
            SemicolonIndex : Natural;
        begin
            NewName := To_Unbounded_String(Name);
            SemicolonIndex := Index(NewName, ";");
            while SemicolonIndex > 0 loop
                Delete(NewName, SemicolonIndex, SemicolonIndex);
                NewName := NewName & " ";
                SemicolonIndex := Index(NewName, ";");
            end loop;
            Name := To_String(NewName);
        end RemoveSemicolons;
    begin
        if Key = Character'Pos('c') or Key = Character'Pos('C') then
            CharName := "            ";
        elsif Key = Character'Pos('h') or Key = Character'Pos('H') then
            ShipName := "            ";
        end if;
        NewGameWindow := Create(7, 31, (Lines / 3), (Columns / 3));
        Box(NewGameWindow);
        Move_Cursor(Win => NewGameWindow, Line => 1, Column => 1);
        Add(Win => NewGameWindow, Str => "Character Name: " & CharName);
        Change_Attributes(Win => NewGameWindow, Line => 1, Column => 1,
            Count => 1, Color => 1);
        Move_Cursor(Win => NewGameWindow, Line => 2, Column => 1);
        Add(Win => NewGameWindow, Str => "Character Gender: ");
        if Key /= Character'Pos('g') and Key /= Character'Pos('G') then
            if CharGender = 'M' then
                Add(Win => NewGameWindow, Str => "Male");
            else
                Add(Win => NewGameWindow, Str => "Female");
            end if;
        else
            Add(Win => NewGameWindow, Str => "Male/Female");
            Change_Attributes(Win => NewGameWindow, Line => 2, Column => 19,
                Count => 1, Color => 1);
            Change_Attributes(Win => NewGameWindow, Line => 2, Column => 24,
                Count => 1, Color => 1);
        end if;
        Change_Attributes(Win => NewGameWindow, Line => 2, Column => 11,
            Count => 1, Color => 1);
        Move_Cursor(Win => NewGameWindow, Line => 3, Column => 1);
        Add(Win => NewGameWindow, Str => "Ship Name: " & ShipName);
        Change_Attributes(Win => NewGameWindow, Line => 3, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Win => NewGameWindow, Line => 5, Column => 5);
        Add(Win => NewGameWindow, Str => "[Quit] [Start]");
        Change_Attributes(Win => NewGameWindow, Line => 5, Column => 6,
            Count => 1, Color => 1);
        Change_Attributes(Win => NewGameWindow, Line => 5, Column => 13,
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
                RemoveSemicolons(CharName);
            elsif Key = Character'Pos('h') or Key = Character'Pos('H') then
                Set_Echo_Mode(True);
                Set_Cursor_Visibility(Visibility);
                Move_Cursor(Win => NewGameWindow, Line => 3, Column => 12);
                Get(Win => NewGameWindow, Str => ShipName, Len => 12);
                if ShipName = "            " then
                    ShipName := "Hawk        ";
                end if;
                RemoveSemicolons(ShipName);
            elsif Key = Character'Pos('g') or Key = Character'Pos('G') then
                Refresh(NewGameWindow);
                GenderKey := Get_KeyStroke;
                if GenderKey = Character'Pos('m') or GenderKey = Character'Pos('M') then
                    CharGender := 'M';
                elsif GenderKey = Character'Pos('f') or GenderKey = Character'Pos('F') then
                    CharGender := 'F';
                end if;
            end if;
            if Key = Character'Pos('c') or Key = Character'Pos('C') or Key = Character'Pos('h') or Key = Character'Pos('H') 
                or Key = Character'Pos('g') or Key = Character'Pos('G') then
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

    procedure ShowLicenseInfo is
        InfoWindow : Window;
        CurrentLine : Line_Position;
        CurrentColumn : Column_Position;
    begin
        InfoWindow := Create(15, (Columns / 2), (Lines / 3), (Columns / 4));
        Add(Win => InfoWindow, Str => "Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.");
        Get_Cursor_Position(InfoWindow, CurrentLine, CurrentColumn);
        Move_Cursor(Win => InfoWindow, Line => CurrentLine + 2, Column => 0);
        Add(Win => InfoWindow, Str => "Steam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.");
        Refresh(InfoWindow);
        Move_Cursor(Line => (Lines - 2), Column => (Columns / 4));
        Add(Str => "F for full license text, other keys back to main menu.");
        Change_Attributes(Line => (Lines - 2), Column => (Columns / 4), Count => 1, Color => 1);
    end ShowLicenseInfo;

    procedure ShowFullLicense is
        LicenseFile : File_Type;
        LinesAmount, TmpLinesAmount : Line_Position;
        LicenseText : Unbounded_String := Null_Unbounded_String;
    begin
        if LicensePad = Null_Window then
            LinesAmount := 0;
            if not Exists("COPYING") then
                LicenseText := To_Unbounded_String("Can't find license file. Did COPYING file is in this same directory where executable is?");
            else
                Open(LicenseFile, In_File, "COPYING");
                while not End_Of_File(LicenseFile) loop
                    Append(LicenseText, Get_Line(LicenseFile));
                    Append(LicenseText, ASCII.LF);
                    LinesAmount := LinesAmount + 1;
                end loop;
                Close(LicenseFile);
            end if;
            TmpLinesAmount := Line_Position(Length(LicenseText)) / Line_Position(Columns - 2);
            if TmpLinesAmount < 1 then
                TmpLinesAmount := 1;
            end if;
            if TmpLinesAmount > LinesAmount then
                LinesAmount := TmpLinesAmount;
            end if;
            LicensePad := New_Pad(LinesAmount + 1, Columns);
            Add(Win => LicensePad, Str => To_String(LicenseText));
            EndIndex := Integer(LinesAmount - (Lines - 3));
            if EndIndex < 0 then
                EndIndex := 0;
            end if;
        end if;
        Refresh(LicensePad, Line_Position(StartIndex), 0, 3, 0, (Lines - 1), Columns);
    end ShowFullLicense;

    procedure ShowNews is
        CurrentLine : Line_Position := (Lines / 5) + 2;
        CurrentColumn : Column_Position;
        News : constant array (Positive range <>) of Unbounded_String :=
            (To_Unbounded_String("* Savegames from previous version are not compatible with current!"), 
                To_Unbounded_String("* Added new enemy ship"),
                To_Unbounded_String("* Added few new items to buy/sell in bases"),
                To_Unbounded_String("* Added few new ship modules"),
                To_Unbounded_String("* Updated combat between ships"),
                To_Unbounded_String("* Added few new manufacturing recipes"),
                To_Unbounded_String("* Updated interface"),
                To_Unbounded_String("* Updated help"),
                To_Unbounded_String("* Added hiring/dismiss crew members"),
                To_Unbounded_String("* Added ability to upgrade ship modules"),
                To_Unbounded_String("* Added ability to install/remove ship modules"),
                To_Unbounded_String("* Changed names of some ship modules"),
                To_Unbounded_String("* Fixed few bugs"));
    begin
        Move_Cursor(Line => (Lines / 5), Column => 10);
        Add(Str => "Main changes since last release (0.3):");
        for I in News'Range loop
            Move_Cursor(Line => CurrentLine, Column => 10);
            Add(Str => To_String(News(I)));
            Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
            CurrentLine := CurrentLine + 1;
        end loop;
        Move_Cursor(Line => (CurrentLine + 1), Column => 10);
        Add(Str => "For more informations about changes, see CHANGELOG.md");
        Move_Cursor(Line => (Lines - 2), Column => (Columns / 3));
        Add(Str => "Press any key to back to main menu");
    end ShowNews;

    procedure LoadGameError(Message : String) is
    begin
        ShowDialog(Message);
        Update_Panels;
        Update_Screen;
    end LoadGameError;

    function MainMenuKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Quit game
                return Quit;
            when Character'Pos('n') | Character'Pos('N') => -- New game
                CharName := "Laeran      ";
                ShipName := "Hawk        ";
                CharGender := 'M';
                ShowNewGameMenu(KEY_NONE);
                Update_Screen;
                return New_Game;
            when Character'Pos('l') | Character'Pos('L') => -- Load game
                if Exists("data/savegame.dat") then
                    if not LoadHelp then
                        LoadGameError("Can't load help system. Probably missing file data/help.dat");
                        return Main_Menu;
                    end if;
                    if not LoadData then
                        LoadGameError("Can't load game data. Probably missing file data/game.dat");
                        return Main_Menu;
                    end if;
                    if not LoadItems then
                        LoadGameError("Can't load items. Probably missing file data/items.dat");
                        return Main_Menu;
                    end if;
                    if not LoadShipModules then
                        LoadGameError("Can't load ship modules. Probably missing file data/shipmodules.dat");
                        return Main_Menu;
                    end if;
                    if not LoadRecipes then
                        LoadGameError("Can't load crafting recipes. Probably missing file data/recipes.dat");
                        return Main_Menu;
                    end if;
                    if not LoadShips then
                        LoadGameError("Can't load ship. Probably missing file data/ships.dat");
                        return Main_Menu;
                    end if;
                    if LoadGame then
                        DrawGame(Sky_Map_View);
                        return Sky_Map_View;
                    else
                        return Main_Menu;
                    end if;
                else
                    return Main_Menu;
                end if;
            when Character'Pos('i') | Character'Pos('I') => -- Show license info
                Erase;
                Refresh_Without_Update;
                ShowLicenseInfo;
                Update_Screen;
                return License_Info;
            when Character'Pos('e') | Character'Pos('E') => -- Show news screen
                Erase;
                Refresh_Without_Update;
                ShowNews;
                Update_Screen;
                return News_View;
            when others => 
                return Main_Menu;
        end case;
    end MainMenuKeys;

    procedure NewGameError(Message : String) is
    begin
        Erase;
        ShowMainMenu;
        Refresh_Without_Update;
        ShowDialog(Message);
        Update_Panels;
        Update_Screen;
    end NewGameError;
    
    function NewGameKeys(Key : Key_Code) return GameStates is
        NewCharName, NewShipName : Unbounded_String;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu
                Erase;
                Refresh;
                ShowMainMenu;
                return Main_Menu;
            when Character'Pos('c') | Character'Pos('C') | Character'Pos('h') | Character'Pos('H') | 
            Character'Pos('g') | Character'Pos('G') => -- Change character/ship name
                Erase;
                ShowMainMenu;
                Refresh_Without_Update;
                ShowNewGameMenu(Key);
                Update_Screen;
                return New_Game;
            when Character'Pos('s') | Character'Pos('S') => -- Start new game;
                if not LoadHelp then
                    NewGameError("Can't load help system. Probably missing file data/help.dat");
                    return Main_Menu;
                end if;
                if not LoadData then
                    NewGameError("Can't load game data. Probably missing file data/game.dat");
                    return Main_Menu;
                end if;
                if not LoadItems then
                    NewGameError("Can't load items. Probably missing file data/items.dat");
                    return Main_Menu;
                end if;
                if not LoadShipModules then
                    NewGameError("Can't load ship modules. Probably missing file data/shipmodules.dat");
                    return Main_Menu;
                end if;
                if not LoadRecipes then
                    NewGameError("Can't load crafting recipes. Probably missing file data/recipes.dat");
                    return Main_Menu;
                end if;
                if not LoadShips then
                    NewGameError("Can't load ship. Probably missing file data/ships.dat");
                    return Main_Menu;
                end if;
                NewCharName := Trim(To_Unbounded_String(CharName), Ada.Strings.Both);
                NewShipName := Trim(To_Unbounded_String(ShipName), Ada.Strings.Both);
                NewGame(NewCharName, NewShipName, CharGender);
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                return New_Game;
        end case;
    end NewGameKeys;

    function LicenseKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('f') | Character'Pos('F') => -- Show full license
                StartIndex := 0;
                Erase;
                Refresh;
                Add(Str => "Up/down arrows to scroll on line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, any other key - back to main menu.");
                ShowFullLicense;
                return License_Full;
            when others => -- Back to main menu
                Erase;
                Refresh;
                ShowMainMenu;
                return Main_Menu;
        end case;
    end LicenseKeys;

    function FullLicenseKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when 56 | KEY_UP => -- Scroll license up one line
                StartIndex := StartIndex - 1;
            when 50 | KEY_DOWN => -- Scroll license down one line
                StartIndex := StartIndex + 1;
            when 51 | KEY_NPAGE => -- Scroll license down one page
                StartIndex := StartIndex + Integer(Lines - 3);
            when 57 | KEY_PPAGE => -- Scroll license up one page
                StartIndex := StartIndex - Integer(Lines - 3);
            when 55 | KEY_HOME => -- Scroll license to start
                StartIndex := 0;
            when 49 | KEY_END => -- Scroll license to end
                StartIndex := EndIndex;
            when others => -- Back to main menu
                Erase;
                Refresh;
                ShowMainMenu;
                return Main_Menu;
        end case;
        if StartIndex < 0 then
            StartIndex := 0;
        end if;
        if StartIndex > EndIndex then
            StartIndex := EndIndex;
        end if;
        ShowFullLicense;
        return License_Full;
    end FullLicenseKeys;

end MainMenu;
