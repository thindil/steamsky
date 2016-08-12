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
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Bases; use Bases;
with Messages; use Messages;
with Combat; use Combat;
with Crafts; use Crafts;
with Help; use Help;

package body UserInterface is

    DialogPanel : Panel := Null_Panel;
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

    procedure ShowGameMenu(CurrentState : GameStates) is
        Speed : Unbounded_String;
    begin
        case CurrentState is
            when Sky_Map_View | Control_Speed =>
                Add(Str => "[Ship] [Crew] [Orders] [Craft] [Messages] [Help] [Quit]");
                Change_Attributes(Line => 0, Column => 1, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 8, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 25, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 32, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 43, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 50, Count => 1, Color => 1);
            when Ship_Info =>
                Add(Str => "Ship Informations [Quit]");
                Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            when Crew_Info | Giving_Orders =>
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
            when Craft_View =>
                Add(Str => "Manufacturing [Quit]");
                Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
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
        if PlayerShip.Speed = DOCKED then
            SpeedWindow := Create(7, 16, (Lines / 2) - 5, (Columns / 2) - 8);
            Box(SpeedWindow);
            Move_Cursor(Win => SpeedWindow, Line => 2, Column => 5);
            Add(Win => SpeedWindow, Str => "Undock");
            Change_Attributes(Win => SpeedWindow, Line => 2, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 5);
            Add(Win => SpeedWindow, Str => "Trade");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 5, Column => 5);
            Add(Win => SpeedWindow, Str => "Quit");
            Change_Attributes(Win => SpeedWindow, Line => 5, Column => 5, Count => 1,
                Color => 1);
        else
            SpeedWindow := Create(10, 20, (Lines / 2) - 5, (Columns / 2) - 10);
            Box(SpeedWindow);
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
            Move_Cursor(Win => SpeedWindow, Line => 8, Column => 5);
            Add(Win => SpeedWindow, Str => "Quit");
            Change_Attributes(Win => SpeedWindow, Line => 8, Column => 5, Count => 1,
                Color => 1);
        end if;
        Refresh(SpeedWindow);
    end ShowSpeedControl;

    procedure ShowConfirm(Message : String) is
        ConfirmWindow : Window;
        Width : Positive;
        Height : Positive := 1;
    begin
        Width := Message'Length + 8;
        if Width >= Positive(Columns - 4) then
            Height := (Width / Positive(Columns - 4) + 2);
            Width := (Width / Height) + 2;
        end if;
        Height := Height + 2;
        ConfirmWindow := New_Window(Line_Position(Height),
            Column_Position(Width), ((Lines / 2) - Line_Position(Height / 2)),
            ((Columns / 2) - Column_Position(Width / 2)));
        Box(ConfirmWindow);
        Add(Win => ConfirmWindow, Str => Message & " (Y/N)", Line => 1, Column => 1);
        Refresh(ConfirmWindow);
    end ShowConfirm;

    procedure ShowDialog(Message : String) is
        DialogWindow : Window;
        Width : Positive;
        Height : Positive := 1;
    begin
        Width := Message'Length + 2;
        if Width >= Positive(Columns - 4) then
            Height := (Width / Positive(Columns - 4) + 2);
            Width := (Width / Height) + 2;
        end if;
        Height := Height + 2;
        DialogWindow := New_Window(Line_Position(Height),
            Column_Position(Width), ((Lines / 2) - Line_Position(Height / 2)),
            ((Columns / 2) - Column_Position(Width / 2)));
        Box(DialogWindow);
        Add(Win => DialogWindow, Str => Message, Line => 1, Column => 1);
        if DialogPanel = Null_Panel then
            DialogPanel := New_Panel(DialogWindow);
        else
            Replace(DialogPanel, DialogWindow);
        end if;
        if Is_Hidden(DialogPanel) then
            Show(DialogPanel);
        end if;
    end ShowDialog;

    function HideDialog return Boolean is
    begin
        if not Is_Hidden(DialogPanel) then
            Hide(DialogPanel);
            return True;
        end if;
        return False;
    end HideDialog;

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

    procedure DrawGame(CurrentState : GameStates) is
    begin
        Erase;
        ShowGameMenu(CurrentState);
        case CurrentState is
            when Sky_Map_View =>
                ShowSkyMap;
            when Control_Speed =>
                ShowSkyMap;
                ShowSpeedControl;
            when Ship_Info =>
                ShowShipInfo;
            when Crew_Info =>
                ShowCrewInfo(KEY_NONE);
            when Messages_View =>
                ShowMessages;
            when Trade_View =>
                ShowTrade(KEY_NONE);
            when Help_View =>
                ShowHelp;
            when Quit_Confirm =>
                Refresh_Without_Update;
                ShowConfirm("Are you sure want to quit game?");
            when Combat_Confirm =>
                Refresh_Without_Update;
                ShowConfirm("We are attacked, engage?");
            when Combat_State =>
                ShowCombat;
            when Craft_View =>
                ShowCraft(KEY_NONE);
            when others =>
                null;
        end case;
        if LastMessage /= To_Unbounded_String("") and CurrentState /= Combat_State then
            Move_Cursor(Line => 1, Column => 2);
            Add(Str => To_String(LastMessage));
            LastMessage := To_Unbounded_String("");
        end if;
        Update_Panels;
        Update_Screen;
    end DrawGame;

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
    
    function GameMenuKeys(CurrentState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu
                DrawGame(Quit_Confirm);
                return Quit_Confirm;
            when Character'Pos('s') | Character'Pos('S') => -- Ship info screen
                DrawGame(Ship_Info);
                return Ship_Info;
            when Character'Pos('c') | Character'Pos('C') => -- Crew info screen
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('r') | Character'Pos('R') => -- Crafting screen
                DrawGame(Craft_View);
                return Craft_View;
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

    function ConfirmKeys(OldState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('n') | Character'Pos('N') => -- Back to old screen
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('y') | Character'Pos('Y') => -- Confirm action
                if OldState = Quit_Confirm then
                    SaveGame;
                    ClearMessages;
                    Erase;
                    Refresh;
                    ShowMainMenu;
                    return Main_Menu;
                else
                    DrawGame(Combat_State);
                    return Combat_State;
                end if;
            when others =>
                DrawGame(OldState);
                return OldState;
        end case;
    end ConfirmKeys;

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
                NewCharName := Trim(To_Unbounded_String(CharName), Ada.Strings.Both);
                NewShipName := Trim(To_Unbounded_String(ShipName), Ada.Strings.Both);
                NewGame(NewCharName, NewShipName);
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                return New_Game;
        end case;
    end NewGameKeys;

end UserInterface;
