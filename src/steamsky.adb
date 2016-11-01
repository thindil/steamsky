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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses_Constants; use Terminal_Interface.Curses_Constants;
with UserInterface; use UserInterface;
with Maps; use Maps;
with Game; use Game;
with Messages; use Messages;
with Crew.UI; use Crew.UI;
with Ships; use Ships;
with Ships.UI; use Ships.UI;
with Bases.UI; use Bases.UI;
with Events; use Events;
with Combat.UI; use Combat.UI;
with Crafts.UI; use Crafts.UI;
with Help; use Help;
with MainMenu; use MainMenu;

procedure SteamSky is
    GameState : GameStates := Main_Menu;
    OldState : GameStates;
    Key : Key_Code;
    Result : Integer;
    ErrorFile : File_Type;
begin
    Set_Directory(Command_Name(Command_Name'First..Command_Name'Last - 9));
    Init_Screen;
    Start_Color;
    Set_Timeout_Mode(Standard_Window, Blocking, 0);
    Init_Color(8, 80, 80, 80);
    Init_Pair(1, Color_Yellow, Color_Black);
    Init_Pair(2, Color_Green, Color_Black);
    Init_Pair(3, Color_Red, Color_Black);
    Init_Pair(4, Color_Blue, Color_Black);
    Init_Pair(5, Color_Cyan, Color_Black);
    Init_Pair(6, 8, 8);
    Init_Pair(7, Color_White, 8);
    Set_KeyPad_Mode(SwitchOn => True);

    ShowMainMenu;

    while GameState /= Quit loop
        Key := Get_Keystroke;
        if Key = Terminal_Interface.Curses.KEY_RESIZE then
            Erase;
            case GameState is
                when Main_Menu =>
                    ShowMainMenu;
                when New_Game =>
                    ShowMainMenu;
                    Refresh;
                    GameState := Main_Menu;
                    Key := Character'Pos('n');
                when License_Info =>
                    GameState := Main_Menu;
                    Key := Character'Pos('i');
                when License_Full =>
                    GameState := License_Info;
                    Key := Character'Pos('f');
                when News_View =>
                    GameState := Main_Menu;
                    Key := Character'Pos('e');
                when others =>
                    DrawGame(GameState);
            end case;
        end if;
        if GameState /= Main_Menu and GameState /= New_Game and GameState /= License_Info 
            and GameState /= License_Full and GameState /= News_View then
            if PlayerShip.Crew.Element(1).Health = 0 then -- Player is dead
                ShowDialog("You are dead.");
                Update_Panels;
                Update_Screen;
                Key := Get_Keystroke;
                if Exists("data/savegame.dat") then
                    Delete_File("data/savegame.dat");
                end if;
                ClearMessages;
                Erase;
                Refresh;
                ShowMainMenu;
                GameState := Main_Menu;
            end if;
        end if;
        if HideDialog then
            Key := Get_Keystroke;
        end if;
        case GameState is
            when Main_Menu =>
                GameState := MainMenuKeys(Key);
            when Sky_Map_View =>
                Result := SkyMapKeys(Key);
                OldState := GameState;
                case Result is
                    when 0 =>
                        GameState := GameMenuKeys(GameState, Key);
                    when 1 =>
                        GameState := CheckForEvent(GameState);
                        DrawGame(GameState);
                    when 2 =>
                        GameState := Control_Speed;
                        DrawGame(GameState);
                    when 3 =>
                        GameState := Wait_Order;
                        DrawGame(GameState);
                    when others =>
                        DrawGame(GameState);
                end case;
            when Control_Speed =>
                GameState := SpeedMenuKeys(OldState, Key);
            when Ship_Info =>
                GameState := ShipInfoKeys(Key, OldState);
            when Crew_Info =>
                GameState := CrewInfoKeys(Key, OldState);
            when Giving_Orders =>
                GameState := CrewOrdersKeys(Key);
            when Messages_View =>
                GameState := MessagesKeys(Key, OldState);
            when Trade_View =>
                GameState := TradeKeys(Key);
            when Help_View =>
                GameState := HelpMenuKeys(Key);
            when Quit_Confirm | Combat_Confirm | Clear_Confirm | Dismiss_Confirm =>
                GameState := ConfirmKeys(GameState, Key);
            when New_Game =>
                GameState := NewGameKeys(Key);
            when Combat_State =>
                OldState := GameState;
                GameState := CombatKeys(Key);
            when Combat_Orders =>
                GameState := CombatOrdersKeys(Key);
            when Craft_View =>
                GameState := CraftKeys(Key);
            when License_Info =>
                GameState := LicenseKeys(Key);
            when License_Full =>
                GameState := FullLicenseKeys(Key);
            when Wait_Order =>
                GameState := WaitMenuKeys(OldState, Key);
            when News_View =>
                Erase;
                Refresh;
                ShowMainMenu;
                GameState := Main_Menu;
            when Cargo_Info =>
                GameState := CargoInfoKeys(Key, OldState);
            when Help_Topic =>
                GameState := HelpKeys(Key);
            when Repairs_View =>
                GameState := RepairKeys(Key);
            when Module_Options =>
                GameState := ModuleOptionsKeys(Key);
            when Shipyard_View =>
                GameState := ShipyardKeys(Key);
            when Recruits_View =>
                GameState := RecruitKeys(Key);
            when Rename_Module | Drop_Cargo | Rename_Ship =>
                GameState := ShipFormKeys(Key, GameState);
            when Trade_Form =>
                GameState := TradeFormKeys(Key);
            when Assign_Owner =>
                GameState := AssignOwnerKeys(Key);
            when Recipe_Setting =>
                GameState := RecipeSettingKeys(Key);
            when others =>
                GameState := GameMenuKeys(GameState, Key);
        end case;
    end loop;

    End_Windows;
exception
    when An_Exception : others =>
        if GameState /= Main_Menu and GameState /= New_Game then
            SaveGame;
        end if;
        if Exists("data/error.log") then
            Open(ErrorFile, Append_File, "data/error.log");
        else
            Create(ErrorFile, Append_File, "data/error.log");
        end if;
        Put_Line(ErrorFile, Ada.Calendar.Formatting.Image(Clock));
        Put_Line(ErrorFile, GameVersion); 
        Put_Line(ErrorFile, Exception_Information(An_Exception));
        Close(ErrorFile);
        Erase;
        Refresh;
        Move_Cursor(Line => (Lines / 2), Column => 2);
        Add(Str => "Oops, something bad happens and game crashed. Game should save your progress, but better check it. Also, please, remember what you done before crash and report this problem at https://github.com/thindil/steamsky/issues (or if you prefer, on mail thindil@laeran.pl) and attach (if possible) file error.log from data directory. Hit any key to quit game.");
        Key := Get_Keystroke;
        End_Windows;
end SteamSky;
