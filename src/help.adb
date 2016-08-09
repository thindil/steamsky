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

    procedure LoadHelp is
        HelpFile : File_Type;
    begin
        if HelpText /= Null_Unbounded_String then
            return;
        end if;
        Open(HelpFile, In_File, "data/help.dat");
        while not End_Of_File(HelpFile) loop
            Append(HelpText, Get_Line(HelpFile));
            Append(HelpText, ASCII.LF);
        end loop;
        Close(HelpFile);
    end LoadHelp;

    procedure ShowHelp is
    begin
        Move_Cursor(Line => 2, Column => 0);
        Add(Str => To_String(HelpText));
    end ShowHelp;

    function HelpKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                ShowHelp;
                return Help_View;
        end case;
    end HelpKeys;

end Help;
