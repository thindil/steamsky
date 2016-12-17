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

with Ada.Directories; use Ada.Directories;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Events; use Events;
with Messages; use Messages;
with MainMenu; use MainMenu;

package body Statistics is

    DestroyedShipsPad : Window;
    StartIndex, EndIndex : Integer := 0;

    procedure UpdateDestroyedShips(ShipName : Unbounded_String) is
        ProtoIndex : Natural := 0;
        procedure UpdateData(Ship : in out DestroyedShips_Data) is
        begin
            Ship.Amount := Ship.Amount + 1;
        end UpdateData;
    begin
        for I in GameStats.DestroyedShips.First_Index..GameStats.DestroyedShips.Last_Index loop
            if Enemies_List.Element(I).Name = ShipName then
                ProtoIndex := I;
                exit;
            end if;
        end loop;
        if ProtoIndex = 0 then
            for I in Enemies_List.First_Index..Enemies_List.Last_Index loop
                if Enemies_List.Element(I).Name = ShipName then
                    GameStats.DestroyedShips.Append(New_Item => (ProtoIndex => I, Amount => 1));
                    exit;
                end if;
            end loop;
        else
            GameStats.DestroyedShips.Update_Element(Index => ProtoIndex, Process => UpdateData'Access);
        end if;
    end UpdateDestroyedShips;

    procedure ClearGameStats is
    begin
        GameStats.DestroyedShips.Clear;
        GameStats.BasesVisited := 1;
        GameStats.MapVisited := 1;
    end ClearGameStats;

    procedure ShowGameStats(RefreshOnly : Boolean := False) is
        MinutesDiff : Natural;
        TimePassed : Date_Record := (Year => 0, Month => 0, Day => 0, Hour => 0, Minutes => 0);
        VisitedPercent : Natural;
    begin
        if not RefreshOnly then
            MinutesDiff := (GameDate.Minutes + (GameDate.Hour * 60) + (GameDate.Day * 1440) + (GameDate.Month * 43200) + 
            (GameDate.Year * 518400)) - 829571520;
            while MinutesDiff > 0 loop
                if MinutesDiff >= 518400 then
                    TimePassed.Year := TimePassed.Year + 1;
                    MinutesDiff := MinutesDiff - 518400;
                elsif MinutesDiff >= 43200 then
                    TimePassed.Month := TimePassed.Month + 1;
                    MinutesDiff := MinutesDiff - 43200;
                elsif MinutesDiff >= 1440 then
                    TimePassed.Day := TimePassed.Day + 1;
                    MinutesDiff := MinutesDiff - 1440;
                elsif MinutesDiff >= 60 then
                    TimePassed.Hour := TimePassed.Hour + 1;
                    MinutesDiff := MinutesDiff - 60;
                else
                    TimePassed.Minutes := MinutesDiff;
                    MinutesDiff := 0;
                end if;
            end loop;
            Move_Cursor(Line => 2, Column => 2);
            Add(Str => "Time passed:" & Natural'Image(TimePassed.Year) & "y," & Natural'Image(TimePassed.Month) & "m," 
            & Natural'Image(TimePassed.Day) & "d," & Natural'Image(TimePassed.Hour) & "h," & Natural'Image(TimePassed.Minutes) & "mins");
            if GameStats.DestroyedShips.Length > 0 then
                DestroyedShipsPad := New_Pad(Line_Position(GameStats.DestroyedShips.Length + 2), (Columns / 2));
                Add(Win => DestroyedShipsPad, Str => "Destroyed ships:");
                for I in GameStats.DestroyedShips.First_Index..GameStats.DestroyedShips.Last_Index loop
                    Move_Cursor(Win => DestroyedShipsPad, Line => Line_Position(I), Column => 0);
                    Add(Win => DestroyedShipsPad, Str => 
                        To_String(Enemies_List.Element(GameStats.DestroyedShips.Element(I).ProtoIndex).Name) & ":" & 
                        Positive'Image(GameStats.DestroyedShips.Element(I).Amount));
                end loop;
                EndIndex := Integer(GameStats.DestroyedShips.Length) - Integer(Lines - 2);
            else
                DestroyedShipsPad := New_Pad(2, (Columns / 2));
                Add(Win => DestroyedShipsPad, Str => "Destroyed ships:");
                Move_Cursor(Win => DestroyedShipsPad, Line => 1, Column => 0);
                Add(Win => DestroyedShipsPad, Str => "none");
            end if;
            Move_Cursor(Line => 3, Column => 2);
            VisitedPercent := Natural((Float(GameStats.BasesVisited) / 1024.0) * 100.0);
            Add(Str => "Bases visited:" & Positive'Image(GameStats.BasesVisited) & " (" & Natural'Image(VisitedPercent) & "% )");
            Move_Cursor(Line => 4, Column => 2);
            VisitedPercent := Natural((Float(GameStats.MapVisited) / (1024.0 * 1024.0)) * 100.0);
            Add(Str => "Map discovered:" & Positive'Image(VisitedPercent) & "%");
            Refresh;
        end if;
        Refresh(DestroyedShipsPad, Line_Position(StartIndex), 0, 2, (Columns / 2), (Lines - 1), Columns);
    end ShowGameStats;

    function ShowGameStatsKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                if PlayerShip.Crew.Element(1).Health = 0 then -- Player is dead
                    if Exists("data/savegame.dat") then
                        Delete_File("data/savegame.dat");
                    end if;
                    ClearMessages;
                    Events_List.Clear;
                    ClearGameStats;
                    Erase;
                    Refresh;
                    ShowMainMenu;
                    return Main_Menu;
                end if;
                StartIndex := 0;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | KEY_UP => -- Scroll destroyed ships list one line up
                StartIndex := StartIndex - 1;
            when 50 | KEY_DOWN => -- Scroll destroyed ships list one line down
                StartIndex := StartIndex + 1;
            when 51 | KEY_NPAGE => -- Scroll destroyed ship list one screen down
                StartIndex := StartIndex + Integer(Lines - 2);
            when 57 | KEY_PPAGE => -- Scroll destroyed ship list one screen up
                StartIndex := StartIndex - Integer(Lines - 2);
            when 55 | KEY_HOME => -- Scroll destroyed ship list to start
                StartIndex := 0;
            when 49 | KEY_END => -- Scroll destroyed ship list to end
                StartIndex := EndIndex;
            when others =>
                null;
        end case;
        if StartIndex < 0 then
            StartIndex := 0;
        end if;
        if StartIndex > EndIndex then
            StartIndex := EndIndex;
        end if;
        ShowGameStats(True);
        return GameStats_View;
    end ShowGameStatsKeys;

end Statistics;
