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
with Ada.Containers.Vectors; use Ada.Containers;
with UserInterface; use UserInterface;

package body Messages is

    package Messages_Container is new Vectors(Positive, Unbounded_String);
    Messages_List : Messages_Container.Vector;

    function FormatedTime return String is
        Result : Unbounded_String := To_Unbounded_String("");
        RawImage : Unbounded_String;
        TimeArray : constant array(1..5) of Natural := (GameDate.Year,
            GameDate.Month, GameDate.Day, GameDate.Hour, GameDate.Minutes);
    begin
        for I in TimeArray'Range loop
            RawImage := To_Unbounded_String(Natural'Image(TimeArray(I)));
            case I is
                when 1 =>
                    Result := Result & Trim(RawImage, Ada.Strings.Left);
                when 2 | 3 =>
                    Result := Result & To_Unbounded_String("-") & Trim(RawImage, Ada.Strings.Left);
                when 4 =>
                    Result := Result & RawImage;
                when 5 =>
                    if TimeArray(5) < 10 then
                        Result := Result & ":0" & Trim(RawImage, Ada.Strings.Left);
                    else
                        Result := Result & ":" & Trim(RawImage, Ada.Strings.Left);
                    end if;
            end case;
        end loop;
        return To_String(Result);
    end FormatedTime;

    procedure AddMessage(Message : String) is
    begin
        Messages_List.Append(New_Item => To_Unbounded_String(FormatedTime) & ": " & To_Unbounded_String(Message));
    end AddMessage;

    procedure ClearMessages is
    begin
        Messages_List.Clear;
    end ClearMessages;

    procedure ShowMessages is
        LoopStart : Positive;
        LinePos : Line_Position := 2;
    begin
        if Messages_List.Length = 0 then
            Move_Cursor(Line => (Lines / 2), Column => (Columns / 2));
            Add(Str => "No messages yet.");
            return;
        end if;
        if Messages_List.Last_Index > Positive(Lines - 2) then
            LoopStart := Messages_List.Last_Index - Positive(Lines - 2);
        else
            LoopStart := Messages_List.First_Index;
        end if;
        for I in LoopStart..Messages_List.Last_Index loop
            Move_Cursor(Line => LinePos, Column => 2);
            Add(Str => To_String(Messages_List.Element(I)));
            LinePos := LinePos + 1;
        end loop;
    end ShowMessages;

    function MessagesKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                DrawGame(Messages_View);
                return Messages_View;
        end case;
    end MessagesKeys;

end Messages;
