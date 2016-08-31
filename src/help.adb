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
with UserInterface; use UserInterface;

package body Help is

    type Help_Data is
        record
            Title : Unbounded_String;
            Text : Unbounded_String;
        end record;
    package Help_Container is new Vectors(Positive, Help_Data);
    Help_List : Help_Container.Vector;
    StartIndex, TopicIndex : Integer := 1;

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
                if Length(RawData) > 3 then 
                    TmpHelp.Title := To_Unbounded_String(Slice(RawData, 2, Length(RawData) - 1));
                end if;
                if TmpHelp.Text /= Null_Unbounded_String then
                    Help_List.Append(New_Item => TmpHelp);
                    TmpHelp := (Title => Null_Unbounded_String, Text =>
                        Null_Unbounded_String);
                end if;
            end if;
        end loop;
        Close(HelpFile);
        return True;
    end LoadHelp;

    procedure ShowHelpMenu is
    begin
        for I in StartIndex..Help_List.Last_Index loop
            Move_Cursor(Line => Line_Position(I + 2), Column => (Columns / 3));
            Add(Str => Character'Val(96 + I) & " " & To_String(Help_List.Element(I).Title));
            Change_Attributes(Line => Line_Position(2 + I), Column => (Columns / 3), Count => 1, Color => 1);
        end loop;
    end ShowHelpMenu;

    procedure ShowHelp is
        CurrentLine : Line_Position := 2;
        CurrentColumn : Column_Position;
        Index : Positive;
    begin
        Move_Cursor(Line => 2, Column => 0);
        Index := StartIndex;
        while CurrentLine < (Lines - 1) and Index <= Length(Help_List.Element(TopicIndex).Text) loop
            Add(Ch => Element(Help_List.Element(TopicIndex).Text, Index));
            Index := Index + 1;
            Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
        end loop;
    end ShowHelp;

    function HelpMenuKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | 65 => -- Move help up
                StartIndex := StartIndex - 1;
                if StartIndex < 1 then
                    StartIndex := 1;
                end if;
                DrawGame(Help_View);
            when 50 | 66 => -- Move help down
                StartIndex := StartIndex + 1;
                if Integer(Help_List.Length) < Integer(Lines - 5) then
                    StartIndex := 1;
                end if;
                DrawGame(Help_View);
            when others =>
                if (Key > 96 and Key <= Key_Code(96 + Help_List.Length)) or
                    (Key > 65 and Key <= Key_Code(65 + Help_List.Length)) then
                    StartIndex := 1;
                    if Key > 96 then
                        TopicIndex := Positive(Key - 96);
                    else
                        TopicIndex := Positive(Key - 65);
                    end if;
                    DrawGame(Help_Topic);
                    return Help_Topic;
                end if;
        end case;
        return Help_View;
    end HelpMenuKeys;

    function HelpKeys(Key : Key_Code) return GameStates is
        TextLength : Positive := 80;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('m') | Character'Pos('M') => -- Back to help menu
                StartIndex := 1;
                DrawGame(Help_View);
                return Help_View;
            when 56 | 65 => -- Move help up
                StartIndex := StartIndex - Positive(Columns);
                if StartIndex < 1 then
                    StartIndex := 1;
                end if;
                DrawGame(Help_Topic);
            when 50 | 66 => -- Move help down
                StartIndex := StartIndex + Positive(Columns);
                if TextLength > Positive(Columns - 1) then
                    TextLength := Positive(Columns - 1);
                end if;
                if (Length(Help_List.Element(TopicIndex).Text) - StartIndex) < (Positive(Lines - 7) * TextLength) then
                    StartIndex := StartIndex - Positive(Columns);
                end if;
                DrawGame(Help_Topic);
            when others =>
                null;
        end case;
        return Help_Topic;
    end HelpKeys;
end Help;
