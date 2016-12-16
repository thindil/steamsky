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

with UserInterface; use UserInterface;
with Ships; use Ships;

package body Statistics is

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

    procedure ShowGameStats is
        MinutesDiff : Natural;
        TimePassed : Date_Record := (Year => 0, Month => 0, Day => 0, Hour => 0, Minutes => 0);
    begin
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
            Move_Cursor(Line => 2, Column => (Columns / 2));
            Add(Str => "Destroyed ships:");
            for I in GameStats.DestroyedShips.First_Index..GameStats.DestroyedShips.Last_Index loop
                Move_Cursor(Line => Line_Position(2 + I), Column => ((Columns / 2) + 1));
                Add(Str => To_String(Enemies_List.Element(GameStats.DestroyedShips.Element(I).ProtoIndex).Name) & ":" & 
                    Positive'Image(GameStats.DestroyedShips.Element(I).Amount));
            end loop;
        end if;
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Bases visited:" & Positive'Image(GameStats.BasesVisited));
        Move_Cursor(Line => 4, Column => 2);
        Add(Str => "Map discovered:" & Positive'Image(GameStats.MapVisited));
    end ShowGameStats;

    function ShowGameStatsKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                return GameStats_View;
        end case;
    end ShowGameStatsKeys;
end Statistics;
