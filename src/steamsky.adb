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
with UserInterface; use UserInterface;
with Maps; use Maps;
with Game; use Game;

procedure SteamSky is
    GameState : GameStates := Main_Menu;
    OldState : GameStates;
    Key : Key_Code;
    Result : Integer;
begin
    Init_Screen;
    Start_Color;
    Set_Timeout_Mode(Standard_Window, Blocking, 0);

    ShowMainMenu;

    while GameState /= Quit loop
        Key := Get_Keystroke;
        case GameState is
            when Main_Menu =>
                GameState := MainMenuKeys(Key);
            when Sky_Map_View =>
                Result := SkyMapKeys(Key);
                case Result is
                    when 0 =>
                        GameState := GameMenuKeys(GameState, Key);
                    when 2 =>
                        OldState := GameState;
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
            when others =>
                GameState := GameMenuKeys(GameState, Key);
        end case;
    end loop;

    End_Windows;
end SteamSky;
