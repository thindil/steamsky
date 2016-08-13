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
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Bases; use Bases;
with Messages; use Messages;
with Combat; use Combat;
with Crafts; use Crafts;
with Help; use Help;
with MainMenu; use MainMenu;

package body UserInterface is

    DialogPanel : Panel := Null_Panel;

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

end UserInterface;
