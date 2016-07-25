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

with Ships; use Ships;
with Bases; use Bases;

package body Maps is

    procedure ShowSkyMap is
        StartX : Integer;
        StartY : Integer;
        BaseIndex : Natural;
    begin
        StartX := PlayerShip.SkyX - Integer(Columns / 2);
        if StartX < 0 then
            StartX := 0;
        elsif  (StartX + Integer(Columns)) > 1025 then
            StartX := 1025 - Integer(Columns);
        end if;
        StartY := PlayerShip.SkyY - Integer(Lines / 2);
        if StartY < 0 then
            StartY := 0;
        elsif (StartY + Integer(Lines)) > 1025 then
            StartY := 1025 - Integer(Lines);
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
    end ShowSkyMap;

    function SkyMapKeys(Key : Key_Code) return Integer is
        Result : Integer := 0;
    begin
        case Key is
            when 56 | 65 => -- Move up
                if MoveShip(0, 0, -1) then
                    Result := 1;
                end if;
            when 50 | 66 => -- Move down
                if MoveShip(0, 0, 1) then
                    Result := 1;
                end if;
            when 54 | 67 => -- Move right
                if MoveShip(0, 1, 0) then
                    Result := 1;
                end if;
            when 52 | 68 => -- Move left
                if MoveShip(0, -1, 0) then
                    Result := 1;
                end if;
            when 49 => -- Move down/left
                if MoveShip(0, -1, 1) then
                    Result := 1;
                end if;
            when 51 => -- Move down/right
                if MoveShip(0, 1, 1) then
                    Result := 1;
                end if;
            when 55 => -- Move up/left
                if MoveShip(0, -1, -1) then
                    Result := 1;
                end if;
            when 57 => -- Move up/right
                if MoveShip(0, 1, -1) then
                    Result := 1;
                end if;
            when Character'Pos('o') | Character'Pos('O') => -- Ship orders menu
                Result := 2;
            when others =>
                null;
        end case;
        return Result;
    end SkyMapKeys;
end Maps;
