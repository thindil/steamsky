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
with Ada.Text_IO; use Ada.Text_IO;
with UserInterface; use UserInterface;

package body Help is

    HelpText : Unbounded_String := Null_Unbounded_String;
    StartIndex : Integer := 1;

    procedure LoadHelp is
        HelpFile : File_Type;
    begin
        if HelpText /= Null_Unbounded_String then
            return;
        end if;
        Open(HelpFile, In_File, "data/help.dat");
        while not End_Of_File(HelpFile) loop
            Append(HelpText, Get_Line(HelpFile));
        end loop;
        Close(HelpFile);
    end LoadHelp;

    procedure ShowHelp is
        CurrentLine : Line_Position := 2;
        CurrentColumn : Column_Position;
        Index : Positive;
    begin
        Move_Cursor(Line => 2, Column => 0);
        Index := StartIndex;
        while CurrentLine < (Lines - 1) and Index <= Length(HelpText) loop
            Add(Ch => Element(HelpText, Index));
            Index := Index + 1;
            Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
            if Index <= Length(HelpText) then
                if Element(HelpText, Index) = '*' then
                    CurrentLine := CurrentLine + 1;
                    Move_Cursor(Line => CurrentLine, Column => 0);
                end if;
            end if;
        end loop;
    end ShowHelp;

    function HelpKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | 65 => -- Move help up
                StartIndex := StartIndex - Positive(Columns);
                if StartIndex < 1 then
                    StartIndex := 1;
                    DrawGame(Help_View);
                    return Help_View;
                end if;
                DrawGame(Help_View);
                return Help_View;
            when 50 | 66 => -- Move help down
                StartIndex := StartIndex + Positive(Columns);
                if (Length(HelpText) - StartIndex) < (Positive(Lines - 7) * Positive(Columns)) then
                    StartIndex := StartIndex - Positive(Columns);
                end if;
                DrawGame(Help_View);
                return Help_View;
            when others =>
                return Help_View;
        end case;
    end HelpKeys;

end Help;
