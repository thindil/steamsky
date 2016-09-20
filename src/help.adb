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
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with UserInterface; use UserInterface;

package body Help is

    type Help_Data is
        record
            Title : Unbounded_String;
            Text : Unbounded_String;
        end record;
    package Help_Container is new Vectors(Positive, Help_Data);
    Help_List : Help_Container.Vector;
    StartIndex, EndIndex : Integer := 1;
    TopicIndex : Integer := 1;
    HelpPad : Window;

    function LoadHelp return Boolean is
        HelpFile : File_Type;
        RawData : Unbounded_String;
        TmpHelp : Help_Data;
    begin
        if Help_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/help.dat") then
            return False;
        end if;
        TmpHelp := (Title => Null_Unbounded_String, Text =>
            Null_Unbounded_String);
        Open(HelpFile, In_File, "data/help.dat");
        while not End_Of_File(HelpFile) loop
            RawData := To_Unbounded_String(Get_Line(HelpFile));
            if Element(RawData, 1) /= '[' then
                Append(TmpHelp.Text, RawData);
                Append(TmpHelp.Text, ASCII.LF);                
            else
                if TmpHelp.Text /= Null_Unbounded_String then
                    Help_List.Append(New_Item => TmpHelp);
                    TmpHelp := (Title => Null_Unbounded_String, Text =>
                        Null_Unbounded_String);
                end if;
                if Length(RawData) > 2 then 
                    TmpHelp.Title := To_Unbounded_String(Slice(RawData, 2, Length(RawData) - 1));
                end if;
            end if;
        end loop;
        Close(HelpFile);
        return True;
    end LoadHelp;

    procedure ShowHelpMenu is
    begin
        for I in Help_List.First_Index..Help_List.Last_Index loop
            Move_Cursor(Line => Line_Position(I + 2), Column => (Columns / 3));
            Add(Str => Character'Val(96 + I) & " " & To_String(Help_List.Element(I).Title));
            Change_Attributes(Line => Line_Position(2 + I), Column => (Columns / 3), Count => 1, Color => 1);
        end loop;
    end ShowHelpMenu;

    procedure ShowHelp(NewHelp : Boolean := False) is
        LinesAmount : Line_Position;
        TextPosition, OldTextPosition : Natural := 1;
    begin
        if NewHelp then
            LinesAmount := Line_Position(Ada.Strings.Unbounded.Count(Help_List.Element(TopicIndex).Text, To_Set(ASCII.LF)));
            while TextPosition > 0 loop
                TextPosition := Index(Help_List.Element(TopicIndex).Text, To_Set(ASCII.LF), OldTextPosition);
                if TextPosition > 0 and Column_Position(TextPosition - OldTextPosition) > Columns then
                    LinesAmount := LinesAmount + (Line_Position((TextPosition - OldTextPosition)) / Line_Position(Columns));
                end if;
                OldTextPosition := TextPosition + 1;
            end loop;
            if LinesAmount < 1 then
                LinesAmount := 1;
            end if;
            HelpPad := New_Pad(LinesAmount + 1, Columns);
            Add(Win => HelpPad, Str => To_String(Help_List.Element(TopicIndex).Text));
            EndIndex := Integer(LinesAmount - (Lines - 2));
            if EndIndex < 0 then
                EndIndex := 0;
            end if;
            Refresh;
        end if;
        Refresh(HelpPad, Line_Position(StartIndex), 0, 2, 0, (Lines - 1), Columns);
    end ShowHelp;

    function HelpMenuKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | KEY_UP => -- Move help up
                StartIndex := StartIndex - 1;
                if StartIndex < 1 then
                    StartIndex := 1;
                end if;
                DrawGame(Help_View);
            when 50 | KEY_DOWN => -- Move help down
                StartIndex := StartIndex + 1;
                if Integer(Help_List.Length) < Integer(Lines - 5) then
                    StartIndex := 1;
                end if;
                DrawGame(Help_View);
            when others =>
                if (Key > 96 and Key <= Key_Code(96 + Help_List.Length)) or
                    (Key > 65 and Key <= Key_Code(65 + Help_List.Length)) then
                    if Key > 96 then
                        TopicIndex := Positive(Key - 96);
                    else
                        TopicIndex := Positive(Key - 65);
                    end if;
                    StartIndex := 0;
                    DrawGame(Help_Topic);
                    return Help_Topic;
                end if;
        end case;
        return Help_View;
    end HelpMenuKeys;

    function HelpKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('m') | Character'Pos('M') => -- Back to help menu
                StartIndex := 1;
                DrawGame(Help_View);
                return Help_View;
            when 56 | KEY_UP => -- Scroll help one line up
                StartIndex := StartIndex - 1;
            when 50 | KEY_DOWN => -- Scroll help one line down
                StartIndex := StartIndex + 1;
            when 51 | KEY_NPAGE => -- Scroll help one screen down
                StartIndex := StartIndex + Integer(Lines - 2);
            when 57 | KEY_PPAGE => -- Scroll help one screen up
                StartIndex := StartIndex - Integer(Lines - 2);
            when 55 | KEY_HOME => -- Scroll help to start
                StartIndex := 0;
            when 49 | KEY_END => -- Scroll help to end
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
        ShowHelp;
        return Help_Topic;
    end HelpKeys;
end Help;
