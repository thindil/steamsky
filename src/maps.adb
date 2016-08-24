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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ships; use Ships;
with Bases; use Bases;
with Game; use Game;

package body Maps is

    MoveX, MoveY : Integer := 0;

    procedure ShowSkyMap is
        StartX : Integer;
        StartY : Integer;
        BaseIndex : Natural;
        InfoWindow : Window;
        WindowHeight : Line_Position := 3;
    begin
        StartX := PlayerShip.SkyX - Integer(Columns / 2);
        StartX := StartX + MoveX;
        if StartX < 0 then
            StartX := 0;
            MoveX := MoveX + 1;
        elsif  (StartX + Integer(Columns)) > 1025 then
            StartX := 1025 - Integer(Columns);
            MoveX := MoveX - 1;
        end if;
        StartY := PlayerShip.SkyY - Integer(Lines / 2);
        StartY := StartY + MoveY;
        if StartY < 0 then
            StartY := 0;
            MoveY := MoveY + 1;
        elsif (StartY + Integer(Lines)) > 1025 then
            StartY := 1025 - Integer(Lines);
            MoveY := MoveY - 1;
        end if;
        for X in 1..Integer(Columns) - 1 loop
            for Y in 1..Integer(Lines) - 1 loop
                BaseIndex := SkyMap(StartX + X, StartY + Y).BaseIndex;
                if BaseIndex > 0 then
                    Move_Cursor(Line => Line_Position(Y), Column =>
                        Column_Position(X - 1));
                    Add(Ch => 'o');
                    if SkyBases(BaseIndex).Visited then
                        case SkyBases(BaseIndex).BaseType is
                            when Industrial =>
                                Change_Attributes(Line => Line_Position(Y), Column =>
                                Column_Position(X - 1), Count => 1, Color => 3);
                            when Agricultural =>
                                Change_Attributes(Line => Line_Position(Y), Column =>
                                Column_Position(X - 1), Count => 1, Color => 2);
                            when Refinery =>
                                Change_Attributes(Line => Line_Position(Y), Column =>
                                Column_Position(X - 1), Count => 1, Color => 4);
                        end case;
                    end if;
                end if;
                if StartX + X = PlayerShip.SkyX and StartY + Y = PlayerShip.SkyY then
                    Move_Cursor(Line => Line_Position(Y), Column =>
                        Column_Position(X - 1));
                    Add(Ch => '+');
                end if;
            end loop;
        end loop;
        Refresh_Without_Update;
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
            WindowHeight := WindowHeight + 3;
        end if;
        InfoWindow := Create(WindowHeight, 20, 1, (Columns - 21));
        Box(InfoWindow);
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 3);
        Add(Win => InfoWindow, Str => "X:" & Positive'Image(PlayerShip.SkyX) & 
            " Y:" & Positive'Image(PlayerShip.SkyY));
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
            BaseIndex := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
            Move_Cursor(Win => InfoWindow, Line => 3, Column => 2);
            Add(Win => InfoWindow, Str => To_String(SkyBases(BaseIndex).Name));
            if SkyBases(BaseIndex).Visited then
                Move_Cursor(Win => InfoWindow, Line => 4, Column => 2);
                Add(Win => InfoWindow, Str => To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
            end if;
        end if;
        Refresh(InfoWindow);
    end ShowSkyMap;

    function SkyMapKeys(Key : Key_Code) return Integer is
        Result : Integer := 1;
        NewKey : Key_Code;
    begin
        case Key is
            when 56 => -- Move up
                MoveShip(0, 0, -1);
            when 50 => -- Move down
                MoveShip(0, 0, 1); 
            when 54 => -- Move right
                MoveShip(0, 1, 0);
            when 52 => -- Move left
                MoveShip(0, -1, 0);
            when 49 => -- Move down/left
                MoveShip(0, -1, 1);
            when 51 => -- Move down/right
                MoveShip(0, 1, 1);
            when 55 => -- Move up/left
                MoveShip(0, -1, -1);
            when 57 => -- Move up/right
                MoveShip(0, 1, -1);
            when Character'Pos('o') | Character'Pos('O') => -- Ship orders menu
                Result := 2;
            when Character'Pos('w') | Character'Pos('W') => -- Wait order menu
                Result := 3;
            when 53 => -- Wait 1 minute
                UpdateGame(1);
            when 27 => -- Map moving and ship moving with arrows keys
                NewKey := Get_KeyStroke;
                if NewKey = 91 then
                    NewKey := Get_KeyStroke;
                    case NewKey is
                        when 65 => -- Move up
                            MoveShip(0, 0, -1);
                        when 66 => -- Move down
                            MoveShip(0, 0, 1); 
                        when 67 => -- Move right
                            MoveShip(0, 1, 0);
                        when 68 => -- Move left
                            MoveShip(0, -1, 0);
                        when 97 => -- Move map up
                            MoveY := MoveY - 1;
                        when 98 => -- Move map down
                            MoveY := MoveY + 1;
                        when 99 => -- Move map right
                            MoveX := MoveX + 1;
                        when 100 => -- Move map left
                            MoveX := MoveX - 1;
                        when 55 => -- Move map up/left
                            MoveX := MoveX - 1;
                            MoveY := MoveY - 1;
                        when 53 => -- Move map up/right
                            MoveX := MoveX + 1;
                            MoveY := MoveY - 1;
                        when 56 => -- Move map down/left
                            MoveX := MoveX - 1;
                            MoveY := MoveY + 1;
                        when 54 => -- Move map down/right
                            MoveX := MoveX + 1;
                            MoveY := MoveY + 1;
                        when others =>
                            Result := 0;
                    end case;
                end if;
            when Character'Pos(' ') => -- Center on ship
                MoveX := 0;
                MoveY := 0;
            when others =>
                Result := 0;
        end case;
        return Result;
    end SkyMapKeys;
end Maps;
