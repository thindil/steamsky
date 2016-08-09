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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses_Constants; use Terminal_Interface.Curses_Constants;
with UserInterface; use UserInterface;
with Maps; use Maps;
with Game; use Game;
with Messages; use Messages;
with Crew; use Crew;
with Ships; use Ships;
with Bases; use Bases;
with Events; use Events;
with Combat; use Combat;
with Crafts; use Crafts;
with Help; use Help;

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
    Init_Pair(1, Color_Yellow, Color_Black);
    Init_Pair(2, Color_Green, Color_Black);
    Init_Pair(3, Color_Red, Color_Black);
    Init_Pair(4, Color_Blue, Color_Black);

    ShowMainMenu;

    while GameState /= Quit loop
        Key := Get_Keystroke;
        if GameState /= Main_Menu and GameState /= New_Game then
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
                    when others =>
                        DrawGame(GameState);
                end case;
            when Control_Speed =>
                GameState := SpeedMenuKeys(OldState, Key);
            when Ship_Info =>
                GameState := ShipInfoKeys(Key);
            when Crew_Info =>
                GameState := CrewInfoKeys(Key);
            when Giving_Orders =>
                GameState := CrewOrdersKeys(Key);
            when Messages_View =>
                GameState := MessagesKeys(Key);
            when Trade_View =>
                GameState := TradeKeys(Key);
            when Help_View =>
                GameState := HelpKeys(Key);
            when Quit_Confirm | Combat_Confirm =>
                GameState := ConfirmKeys(GameState, Key);
            when New_Game =>
                GameState := NewGameKeys(Key);
            when Combat_State =>
                GameState := CombatKeys(Key);
            when Combat_Orders =>
                GameState := CombatOrdersKeys(Key);
            when Craft_View =>
                GameState := CraftKeys(Key);
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
        Put_Line(ErrorFile, "Version: 0.2"); 
        Put_Line(ErrorFile, Exception_Information(An_Exception));
        Close(ErrorFile);
        Erase;
        Refresh;
        Move_Cursor(Line => (Lines / 2), Column => 2);
        Add(Str => "Oops, something bad happens and game crashed. Game should save your progress, but better check it. Also, please, remember what you done before crash, report bug at https://github.com/thindil/steamsky/issues and attach (if possible) file error.log from data directory. Hit any key to quit game.");
        Key := Get_Keystroke;
        End_Windows;
end SteamSky;
