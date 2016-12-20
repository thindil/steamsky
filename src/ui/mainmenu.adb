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
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration; use Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Help; use Help;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Ships; use Ships;
with Config; use Config;
with Crew; use Crew;

package body MainMenu is

    StartIndex : Integer := 0;
    EndIndex : Integer;
    LicensePad, NewsPad : Window := Null_Window;
    NewGameForm : Forms.Form;
    FormWindow : Window;
    GameMenu : Menu;
    MenuWindow : Window;

    procedure ShowMainMenu is
        Visibility : Cursor_Visibility := Invisible;
        Menu_Items : constant Item_Array_Access := new Item_Array(1..6);
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuIndex : Positive := 2;
    begin
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);
        -- Game logo
        Move_Cursor(Line => 3, Column => (Columns / 2) - 25);
        Add(Str => "  ______                            ______ _");
        Move_Cursor(Line => 4, Column => (Columns / 2) - 25);
        Add(Str => " / _____) _                        / _____) |");
        Move_Cursor(Line => 5, Column => (Columns / 2) - 25);
        Add(Str => "( (____ _| |_ _____ _____ ____    ( (____ | |  _ _   _");
        Move_Cursor(Line => 6, Column => (Columns / 2) - 25);
        Add(Str => " \____ (_   _) ___ (____ |    \    \____ \| |_/ ) | | |");
        Move_Cursor(Line => 7, Column => (Columns / 2) - 25);
        Add(Str => " _____) )| |_| ____/ ___ | | | |   _____) )  _ (| |_| |");
        Move_Cursor(Line => 8, Column => (Columns / 2) - 25);
        Add(Str => "(______/  \__)_____)_____|_|_|_|  (______/|_| \_)\__  |");
        Move_Cursor(Line => 9, Column => (Columns / 2) - 25);
        Add(Str => "                                                (____/ ");
        -- Game version
        Move_Cursor(Line => 10, Column => (Columns - 17) / 2);
        Add(Str => GameVersion);
        -- Game menu
        Menu_Items.all(1) := New_Item("New game");
        if Exists("data/savegame.dat") then
            Menu_Items.all(2) := New_Item("Load game");
            MenuIndex := 3;
        end if;
        Menu_Items.all(MenuIndex) := New_Item("News");
        MenuIndex := MenuIndex + 1;
        Menu_Items.all(MenuIndex) := New_Item("License");
        MenuIndex := MenuIndex + 1;
        Menu_Items.all(MenuIndex) := New_Item("Quit game");
        MenuIndex := MenuIndex + 1;
        for I in MenuIndex..Menu_Items'Last loop
            Menu_Items.all(I) := Null_Item;
        end loop;
        GameMenu := New_Menu(Menu_Items);
        Set_Format(GameMenu, Lines - 5, 1);
        Set_Mark(GameMenu, "");
        Scale(GameMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 14, (Columns - 17) / 2);
        Set_Window(GameMenu, MenuWindow);
        Set_Sub_Window(GameMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(GameMenu);
        -- Copyright
        Move_Cursor(Line => Lines - 1, Column => (Columns / 2) - 14);
        Add(Str => "2016 Bartek thindil Jasicki");
        Refresh;
        Refresh(MenuWindow);
    end ShowMainMenu;

    procedure ShowNewGameForm is
        NewGame_Fields : constant Field_Array_Access := new Field_Array(1..10);
        FormHeight : Line_Position;
        FormLength : Column_Position;
        Visibility : Cursor_Visibility := Normal;
        FieldOptions : Field_Option_Set;
        Genders : constant Enumeration_Info := (C => 2, Names => (new String'("Male ->"), new String'("Female ->")),
            Case_Sensitive => False, Match_Must_Be_Unique => False);
    begin
        if NewGameForm = Null_Form then
            Set_Cursor_Visibility(Visibility);
            NewGame_Fields.all(1) := New_Field(1, 18, 0, 0, 0, 0);
            Set_Buffer(NewGame_Fields.all(1), 0, "Character Name: ");
            FieldOptions := Get_Options(NewGame_Fields.all(1));
            FieldOptions.Active := False;
            Set_Options(NewGame_Fields.all(1),  FieldOptions);
            NewGame_Fields.all(2) := New_Field(1, 12, 0, 18, 0, 0);
            Set_Buffer(NewGame_Fields.all(2), 0, To_String(NewGameSettings.PlayerName));
            FieldOptions := Get_Options(NewGame_Fields.all(2));
            FieldOptions.Auto_Skip := False;
            Set_Options(NewGame_Fields.all(2),  FieldOptions);
            Set_Background(NewGame_Fields.all(2), (Reverse_Video => True, others => False));
            NewGame_Fields.all(3) := New_Field(1, 18, 1, 0, 0, 0);
            Set_Buffer(NewGame_Fields.all(3), 0, "Character Gender: ");
            FieldOptions := Get_Options(NewGame_Fields.all(3));
            FieldOptions.Active := False;
            Set_Options(NewGame_Fields.all(3),  FieldOptions);
            NewGame_Fields.all(4) := New_Field(1, 12, 1, 18, 0, 0);
            Set_Field_Type(NewGame_Fields.all(4), Create(Genders, True));
            if NewGameSettings.PlayerGender = 'M' then
                Set_Buffer(NewGame_Fields.all(4), 0, "Male ->");
            else
                Set_Buffer(NewGame_Fields.all(4), 0, "Female ->");
            end if;
            FieldOptions := Get_Options(NewGame_Fields.all(4));
            FieldOptions.Edit := False;
            Set_Options(NewGame_Fields.all(4), FieldOptions);
            NewGame_Fields.all(5) := New_Field(1, 18, 2, 0, 0, 0);
            Set_Buffer(NewGame_Fields.all(5), 0, "Ship Name: ");
            FieldOptions := Get_Options(NewGame_Fields.all(5));
            FieldOptions.Active := False;
            Set_Options(NewGame_Fields.all(5),  FieldOptions);
            NewGame_Fields.all(6) := New_Field(1, 12, 2, 18, 0, 0);
            Set_Buffer(NewGame_Fields.all(6), 0, To_String(NewGameSettings.ShipName));
            FieldOptions := Get_Options(NewGame_Fields.all(6));
            FieldOptions.Auto_Skip := False;
            Set_Options(NewGame_Fields.all(6),  FieldOptions);
            NewGame_Fields.all(7) := New_Field(2, 30, 4, 0, 0, 0);
            Set_Buffer(NewGame_Fields.all(7), 0, "Press Enter for random name.");
            FieldOptions := Get_Options(NewGame_Fields.all(7));
            FieldOptions.Active := False;
            Set_Options(NewGame_Fields.all(7),  FieldOptions);
            NewGame_Fields.all(8) := New_Field(1, 6, 7, 5, 0, 0);
            Set_Buffer(NewGame_Fields.all(8), 0, "[Quit]");
            FieldOptions := Get_Options(NewGame_Fields.all(8));
            FieldOptions.Edit := False;
            Set_Options(NewGame_Fields.all(8), FieldOptions);
            NewGame_Fields.all(9) := New_Field(1, 7, 7, 13, 0, 0);
            FieldOptions := Get_Options(NewGame_Fields.all(9));
            FieldOptions.Edit := False;
            Set_Options(NewGame_Fields.all(9), FieldOptions);
            Set_Buffer(NewGame_Fields.all(9), 0, "[Start]");
            NewGame_Fields.all(10) := Null_Field;
            NewGameForm := New_Form(NewGame_Fields);
            Set_Options(NewGameForm, (others => False));
            Scale(NewGameForm, FormHeight, FormLength);
            FormWindow := Create(FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)), ((Columns / 2) - (FormLength / 2)));
            Box(FormWindow);
            Set_Window(NewGameForm, FormWindow);
            Set_Sub_Window(NewGameForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
            Post(NewGameForm);
        end if;
        Refresh;
        Refresh(FormWindow);
    end ShowNewGameForm;

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
        ChangesFile : File_Type;
        LinesAmount, TmpLinesAmount : Line_Position;
        NewsText : Unbounded_String := Null_Unbounded_String;
        FileText : Unbounded_String;
    begin
        if NewsPad = Null_Window then
            LinesAmount := 0;
            if not Exists("CHANGELOG.md") then
                NewsText := To_Unbounded_String("Can't find changelog file. Did CHANGELOG.md file is in this same directory where executable is?");
            else
                Open(ChangesFile, In_File, "CHANGELOG.md");
                Set_Line(ChangesFile, 6);
                loop
                    FileText := To_Unbounded_String(Get_Line(ChangesFile));
                    if Length(FileText) > 1 then
                        exit when Slice(FileText, 1, 3) = "## ";
                    end if;
                    Append(NewsText, FileText);
                    Append(NewsText, ASCII.LF);
                    LinesAmount := LinesAmount + 1;
                end loop;
                Close(ChangesFile);
                Append(NewsText, "For more informations about changes, see CHANGELOG.md");
            end if;
            TmpLinesAmount := Line_Position(Length(NewsText)) / Line_Position(Columns - 2);
            if TmpLinesAmount < 1 then
                TmpLinesAmount := 1;
            end if;
            if TmpLinesAmount > LinesAmount then
                LinesAmount := TmpLinesAmount;
            end if;
            NewsPad := New_Pad(LinesAmount + 1, Columns);
            Add(Win => NewsPad, Str => To_String(NewsText));
            EndIndex := Integer(LinesAmount - (Lines - 3));
            if EndIndex < 0 then
                EndIndex := 0;
            end if;
        end if;
        Refresh(NewsPad, Line_Position(StartIndex), 0, 3, 0, (Lines - 1), Columns);
    end ShowNews;

    procedure LoadGameError(Message : String) is
    begin
        ShowDialog(Message);
        Update_Panels;
        Update_Screen;
    end LoadGameError;

    function MainMenuKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
        Option : constant String := Name(Current(GameMenu));
        LoadError : Unbounded_String;
    begin
        case Key is
            when 56 | KEY_UP => -- Select previous option
                Result := Driver(GameMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(GameMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next option
                Result := Driver(GameMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(GameMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow);
                end if;
            when 10 => -- Select option
                if Option = "New game" then
                    ShowNewGameForm;
                    return New_Game;
                elsif Option = "Load game" then
                    if not LoadHelp then
                        LoadGameError("Can't load help system. Probably missing file data/help.dat");
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
                    LoadError := LoadGame;
                    if LoadError = Null_Unbounded_String then
                        DrawGame(Sky_Map_View);
                        return Sky_Map_View;
                    else
                        LoadGameError(To_String(LoadError));
                        return Main_Menu;
                    end if;
                elsif Option = "News" then
                    StartIndex := 0;
                    Erase;
                    Add(Str => "Up/down arrows to scroll on line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, any other key - back to main menu.");
                    Refresh_Without_Update;
                    ShowNews;
                    Update_Screen;
                    return News_View;
                elsif Option = "License" then
                    Erase;
                    Refresh_Without_Update;
                    ShowLicenseInfo;
                    Update_Screen;
                    return License_Info;
                else
                    return Quit;
                end if;
            when others =>
                Result := Driver(GameMenu, Key);
                if Result = Menu_Ok then
                    Refresh(MenuWindow);
                else
                    Result := Driver(GameMenu, M_CLEAR_PATTERN);
                    Result := Driver(GameMenu, Key);
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    end if;
                end if;
        end case;
        return Main_Menu;
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
        Result : Forms.Driver_Result;
        FieldIndex : Positive := Get_Index(Current(NewGameForm));
        Visibility : Cursor_Visibility := Invisible;
        CharGender : Character;
        procedure RemoveSemicolons(Name : in out Unbounded_String) is
            SemicolonIndex : Natural;
        begin
            SemicolonIndex := Index(Name, ";");
            while SemicolonIndex > 0 loop
                Delete(Name, SemicolonIndex, SemicolonIndex);
                Name := Name & " ";
                SemicolonIndex := Index(Name, ";");
            end loop;
        end RemoveSemicolons;
    begin
        case Key is
            when KEY_UP => -- Select previous field
                Result := Driver(NewGameForm, F_Previous_Field);
                FieldIndex := Get_Index(Current(NewGameForm));
                if FieldIndex = 2 or FieldIndex = 6 then
                    Result := Driver(NewGameForm, F_End_Line);
                end if;
            when KEY_DOWN => -- Select next field
                Result := Driver(NewGameForm, F_Next_Field);
                FieldIndex := Get_Index(Current(NewGameForm));
                if FieldIndex = 2 or FieldIndex = 6 then
                    Result := Driver(NewGameForm, F_End_Line);
                end if;
            when 10 => -- quit/start game or change character gender, depends on form field
                CharGender := Get_Buffer(Fields(NewGameForm, 4))(1);
                if FieldIndex = 2 then
                    NewCharName := GenerateMemberName(CharGender);
                    Set_Buffer(Fields(NewGameForm, 2), 0, To_String(NewCharName));
                    Result := Driver(NewGameForm, F_End_Line);
                    Refresh(FormWindow);
                end if;
                if FieldIndex = 4 then
                    Result := Driver(NewGameForm, F_Next_Choice);
                    Refresh(FormWindow);
                end if;
                if FieldIndex = 6 then
                    NewShipName := GenerateShipName;
                    Set_Buffer(Fields(NewGameForm, 6), 0, To_String(NewShipName));
                    Result := Driver(NewGameForm, F_End_Line);
                    Refresh(FormWindow);
                end if;
                if FieldIndex < 8 then
                    return New_Game;
                end if;
                if FieldIndex = 8 then
                    Set_Cursor_Visibility(Visibility);
                    Post(NewGameForm, False);
                    Delete(NewGameForm);
                    Erase;
                    Refresh;
                    ShowMainMenu;
                    return Main_Menu;
                end if;
                if not LoadHelp then
                    NewGameError("Can't load help system. Probably missing file data/help.dat");
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
                NewCharName := To_Unbounded_String(Get_Buffer(Fields(NewGameForm, 2)));
                NewShipName := To_Unbounded_String(Get_Buffer(Fields(NewGameForm, 6)));
                RemoveSemicolons(NewCharName);
                RemoveSemicolons(NewShipName);
                Trim(NewCharName, Ada.Strings.Both);
                Trim(NewShipName, Ada.Strings.Both);
                if Length(NewCharName) = 0 then
                    NewCharName := To_Unbounded_String("Laeran");
                end if;
                if Length(NewShipName) = 0 then
                    NewShipName := To_Unbounded_String("Anaria");
                end if;
                Set_Cursor_Visibility(Visibility);
                Post(NewGameForm, False);
                Delete(NewGameForm);
                NewGame(NewCharName, NewShipName, CharGender);
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when KEY_BACKSPACE => -- delete last character
                if FieldIndex = 2 or FieldIndex = 6 then
                    Result := Driver(NewGameForm, F_Delete_Previous);
                end if;
            when KEY_DC => -- delete character at cursor
                if FieldIndex = 2 or FieldIndex = 6 then
                    Result := Driver(NewGameForm, F_Delete_Char);
                end if;
            when KEY_RIGHT => -- Move cursor right
                if FieldIndex = 2 or FieldIndex = 6 then
                    Result := Driver(NewGameForm, F_Right_Char);
                elsif FieldIndex = 4 then
                    Result := Driver(NewGameForm, F_Next_Choice);
                end if;
            when Character'Pos('m') | Character'Pos('M') => -- Select male gender
                if FieldIndex = 4 then
                    Set_Buffer(Current(NewGameForm), 0, "Male ->");
                else
                    Result := Driver(NewGameForm, Key);
                end if;
            when Character'Pos('f') | Character'Pos('F') => -- Select female gender
                if FieldIndex = 4 then
                    Set_Buffer(Current(NewGameForm), 0, "Female ->");
                else
                    Result := Driver(NewGameForm, Key);
                end if;
            when KEY_LEFT => -- Move cursor left
                if FieldIndex = 2 or FieldIndex = 6 then
                    Result := Driver(NewGameForm, F_Left_Char);
                elsif FieldIndex = 4 then
                    Result := Driver(NewGameForm, F_Previous_Choice);
                end if;
            when others =>
                Result := Driver(NewGameForm, Key);
        end case;
        if Result = Form_Ok then
            if FieldIndex = 2 or FieldIndex = 6 then
                Set_Buffer(Fields(NewGameForm, 7), 0, "Press Enter for random name.");
                Set_Background(Current(NewGameForm), (Reverse_Video => True, others => False));
            else
                if FieldIndex = 4 then
                    Set_Buffer(Fields(NewGameForm, 7), 0, "Left or Right arrow to change gender.");
                else
                    Set_Buffer(Fields(NewGameForm, 7), 0, "");
                end if;
                Set_Background(Fields(NewGameForm, 2), (others => False));
                Set_Background(Fields(NewGameForm, 6), (others => False));
            end if;
            Refresh(FormWindow);
        end if;
        return New_Game;
    end NewGameKeys;

    function LicenseKeys(Key : Key_Code) return GameStates is
    begin
        Erase;
        Refresh;
        case Key is
            when Character'Pos('f') | Character'Pos('F') => -- Show full license
                StartIndex := 0;
                Add(Str => "Up/down arrows to scroll on line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, any other key - back to main menu.");
                ShowFullLicense;
                return License_Full;
            when others => -- Back to main menu
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

    function NewsKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when 56 | KEY_UP => -- Scroll news up one line
                StartIndex := StartIndex - 1;
            when 50 | KEY_DOWN => -- Scroll news down one line
                StartIndex := StartIndex + 1;
            when 51 | KEY_NPAGE => -- Scroll news down one page
                StartIndex := StartIndex + Integer(Lines - 3);
            when 57 | KEY_PPAGE => -- Scroll news up one page
                StartIndex := StartIndex - Integer(Lines - 3);
            when 55 | KEY_HOME => -- Scroll news to start
                StartIndex := 0;
            when 49 | KEY_END => -- Scroll news to end
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
        ShowNews;
        return News_View;
    end NewsKeys;

end MainMenu;
