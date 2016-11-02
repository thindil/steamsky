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
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.IntField; use Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Ships; use Ships;
with Bases; use Bases;
with UserInterface; use UserInterface;

package body Maps is

    MoveX, MoveY : Integer := 0;
    MoveForm : Form;
    FormWindow : Window;

    procedure ShowSkyMap is
        StartX : Integer;
        StartY : Integer;
        BaseIndex : Natural;
        InfoWindow : Window;
        WindowHeight : Line_Position := 3;
        WindowWidth : Column_Position := 20;
        CurrentCell : Attributed_Character;
    begin
        CurrentCell := ACS_Map(ACS_Solid_Block);
        CurrentCell.Color := 0;
        StartX := PlayerShip.SkyX - Integer(Columns / 2);
        StartX := StartX + MoveX;
        if StartX < 0 then
            StartX := 0;
        elsif  (StartX + Integer(Columns)) > 1025 then
            StartX := 1025 - Integer(Columns);
        end if;
        if PlayerShip.SkyX + MoveX <= 1 then
            MoveX := 1 - PlayerShip.SkyX;
        elsif PlayerShip.SkyX + MoveX > 1024 then
            MoveX := 1024 - PlayerShip.SkyX;
        end if;
        StartY := PlayerShip.SkyY - Integer(Lines / 2);
        StartY := StartY + MoveY;
        if StartY < 0 then
            StartY := 0;
        elsif (StartY + Integer(Lines)) > 1025 then
            StartY := 1025 - Integer(Lines);
        end if;
        if PlayerShip.SkyY + MoveY <= 1 then
            MoveY := 1 - PlayerShip.SkyY;
        elsif PlayerShip.SkyY + MoveY > 1024 then
            MoveY := 1024 - PlayerShip.SkyY;
        end if;
        for X in 1..Integer(Columns) - 1 loop
            for Y in 1..Integer(Lines) - 1 loop
                BaseIndex := SkyMap(StartX + X, StartY + Y).BaseIndex;
                if BaseIndex > 0 then
                    if SkyBases(BaseIndex).Known then
                        Move_Cursor(Line => Line_Position(Y), Column =>
                            Column_Position(X - 1));
                        Add(Ch => 'o');
                        if SkyBases(BaseIndex).Visited.Year > 0 then
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
                                when Shipyard =>
                                    Change_Attributes(Line => Line_Position(Y), Column =>
                                    Column_Position(X - 1), Count => 1, Color => 5);
                            end case;
                        else
                            Change_Attributes(Line => Line_Position(Y), Column =>
                                Column_Position(X - 1), Count => 1, Color => 7);
                        end if;
                    else 
                        Move_Cursor(Line => Line_Position(Y), Column =>
                            Column_Position(X - 1));
                        Change_Attributes(Line => Line_Position(Y), Column =>
                            Column_Position(X - 1), Count => 1, Color => 6);
                    end if;
                elsif not SkyMap(StartX + X, StartY + Y).Visited then
                    Change_Attributes(Line => Line_Position(Y), Column =>
                        Column_Position(X - 1), Count => 1, Color => 6);
                end if;
                if StartX + X = PlayerShip.SkyX and StartY + Y = PlayerShip.SkyY then
                    Move_Cursor(Line => Line_Position(Y), Column =>
                        Column_Position(X - 1));
                    Add(Ch => '+');
                end if;
                if (MoveX /= 0 or MoveY /= 0) and (StartX + X = PlayerShip.SkyX + MoveX and StartY + Y = PlayerShip.SkyY + MoveY) then
                    if Peek(Line => Line_Position(Y), Column => Column_Position(X - 1)).Ch = ' ' then
                        Move_Cursor(Line => Line_Position(Y), Column =>
                            Column_Position(X - 1));
                        Add(Ch => CurrentCell);
                    end if;
                end if;
            end loop;
        end loop;
        Refresh_Without_Update;
        BaseIndex := SkyMap(PlayerShip.SkyX + MoveX, PlayerShip.SkyY + MoveY).BaseIndex;
        if BaseIndex > 0 then
            if SkyBases(BaseIndex).Visited.Year > 0 then
                WindowHeight := WindowHeight + 4;
            elsif SkyBases(BaseIndex).Known then
                WindowHeight := WindowHeight + 2;
            end if;
            WindowWidth := 4 + Column_Position(Length(SkyBases(BaseIndex).Name));
            if WindowWidth < 20 then
                WindowWidth := 20;
            end if;
        end if;
        InfoWindow := Create(WindowHeight, WindowWidth, 1, (Columns - WindowWidth - 1));
        Box(InfoWindow);
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 3);
        Add(Win => InfoWindow, Str => "X:" & Positive'Image(PlayerShip.SkyX + MoveX) & 
            " Y:" & Positive'Image(PlayerShip.SkyY + MoveY));
        if BaseIndex > 0 then
            if SkyBases(BaseIndex).Known then
                Move_Cursor(Win => InfoWindow, Line => 3, Column => 2);
                Add(Win => InfoWindow, Str => To_String(SkyBases(BaseIndex).Name));
            end if;
            if SkyBases(BaseIndex).Visited.Year > 0 then
                Move_Cursor(Win => InfoWindow, Line => 4, Column => 2);
                Add(Win => InfoWindow, Str => To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
                Move_Cursor(Win => InfoWindow, Line => 5, Column => 2);
                if SkyBases(BaseIndex).Population < 150 then
                    Add(Win => InfoWindow, Str => "small");
                elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
                    Add(Win => InfoWindow, Str => "medium");
                else
                    Add(Win => InfoWindow, Str => "large");
                end if;
            end if;
        end if;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowSkyMap;

    procedure ShowMoveMapForm is
        Move_Fields : constant Field_Array_Access := new Field_Array(1..7);
        FieldOptions : Field_Option_Set;
        FormHeight : Line_Position;
        FormLength : Column_Position;
        Visibility : Cursor_Visibility := Normal;
    begin
        if MoveForm = Null_Form then
            Set_Cursor_Visibility(Visibility);
            Move_Fields.all(1) := New_Field(1, 2, 0, 0, 0, 0);
            FieldOptions := Get_Options(Move_Fields.all(1));
            Set_Buffer(Move_Fields.all(1), 0, "X:");
            FieldOptions.Active := False;
            Set_Options(Move_Fields.all(1), FieldOptions);
            Move_Fields.all(2) := New_Field(1, 5, 0, 2, 0, 0);
            FieldOptions := Get_Options(Move_Fields.all(2));
            Set_Buffer(Move_Fields.all(2), 0, Natural'Image(PlayerShip.SkyX + MoveX));
            FieldOptions.Auto_Skip := False;
            FieldOptions.Null_Ok := False;
            Set_Options(Move_Fields.all(2), FieldOptions);
            Set_Background(Move_Fields.all(2), (Reverse_Video => True, others => False));
            Set_Field_Type(Move_Fields.all(2), (0, 1, 1024));
            Move_Fields.all(3) := New_Field(1, 2, 1, 0, 0, 0);
            FieldOptions := Get_Options(Move_Fields.all(3));
            Set_Buffer(Move_Fields.all(3), 0, "Y:");
            FieldOptions.Active := False;
            Set_Options(Move_Fields.all(3), FieldOptions);
            Move_Fields.all(4) := New_Field(1, 5, 1, 2, 0, 0);
            FieldOptions := Get_Options(Move_Fields.all(4));
            Set_Buffer(Move_Fields.all(4), 0, Natural'Image(PlayerShip.SkyY + MoveY));
            FieldOptions.Auto_Skip := False;
            FieldOptions.Null_Ok := False;
            Set_Options(Move_Fields.all(4), FieldOptions);
            Set_Field_Type(Move_Fields.all(4), (0, 1, 1024));
            Move_Fields.all(5) := New_Field(1, 8, 3, 1, 0, 0);
            Set_Buffer(Move_Fields.all(5), 0, "[Cancel]");
            FieldOptions := Get_Options(Move_Fields.all(5));
            FieldOptions.Edit := False;
            Set_Options(Move_Fields.all(5), FieldOptions);
            Move_Fields.all(6) := New_Field(1, 4, 3, 11, 0, 0);
            FieldOptions := Get_Options(Move_Fields.all(6));
            FieldOptions.Edit := False;
            Set_Options(Move_Fields.all(6), FieldOptions);
            Set_Buffer(Move_Fields.all(6), 0, "[Ok]");
            Move_Fields.all(7) := Null_Field;
            MoveForm := New_Form(Move_Fields);
            Set_Options(MoveForm, (others => False));
            Scale(MoveForm, FormHeight, FormLength);
            FormWindow := Create(FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)), ((Columns / 2) - (FormLength / 2)));
            Box(FormWindow);
            Set_Window(MoveForm, FormWindow);
            Set_Sub_Window(MoveForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
            Post(MoveForm);
        end if;
        Refresh;
        Refresh(FormWindow);
    end ShowMoveMapForm;

    function SkyMapKeys(Key : Key_Code) return Integer is
        Result : Integer := 1;
        NewKey : Key_Code;
    begin
        case Key is
            when 56 | KEY_UP => -- Move up
                Result := MoveShip(0, 0, -1);
            when 50 | KEY_DOWN => -- Move down
                Result := MoveShip(0, 0, 1); 
            when 54 | KEY_RIGHT => -- Move right
                Result := MoveShip(0, 1, 0);
            when 52 | KEY_LEFT => -- Move left
                Result := MoveShip(0, -1, 0);
            when 49 => -- Move down/left
                Result := MoveShip(0, -1, 1);
            when 51 => -- Move down/right
                Result := MoveShip(0, 1, 1);
            when 55 => -- Move up/left
                Result := MoveShip(0, -1, -1);
            when 57 => -- Move up/right
                Result := MoveShip(0, 1, -1);
            when Character'Pos('o') | Character'Pos('O') => -- Ship orders menu
                Result := 2;
            when Character'Pos('w') | Character'Pos('W') => -- Wait order menu
                Result := 3;
            when 53 => -- Wait 1 minute
                UpdateGame(1);
            when KEY_SRIGHT =>
                MoveX := MoveX + 1;
                Result := 4;
            when KEY_SLEFT =>
                MoveX := MoveX - 1;
                Result := 4;
            when KEY_SHOME => -- Move map up/left
                MoveX := MoveX - 1;
                MoveY := MoveY - 1;
                Result := 4;
            when KEY_SPREVIOUS => -- Move map up/right
                MoveX := MoveX + 1;
                MoveY := MoveY - 1;
                Result := 4;
            when KEY_SEND => -- Move map down/left
                MoveX := MoveX - 1;
                MoveY := MoveY + 1;
                Result := 4;
            when KEY_SNEXT => -- Move map down/right
                MoveX := MoveX + 1;
                MoveY := MoveY + 1;
                Result := 4;
            when 27 => -- Map moving with arrows keys
                NewKey := Get_KeyStroke;
                if NewKey = 91 then
                    NewKey := Get_KeyStroke;
                    case NewKey is
                        when 97 => -- Move map up
                            MoveY := MoveY - 1;
                            Result := 4;
                        when 98 => -- Move map down
                            MoveY := MoveY + 1;
                            Result := 4;
                        when others =>
                            Result := 0;
                    end case;
                end if;
            when Character'Pos(' ') => -- Center on ship
                MoveX := 0;
                MoveY := 0;
                Result := 4;
            when others =>
                Result := 0;
        end case;
        return Result;
    end SkyMapKeys;

    function MoveFormKeys(Key : Key_Code) return GameStates is
        Result : Forms.Driver_Result;
        FieldIndex : Positive := Get_Index(Current(MoveForm));
        Visibility : Cursor_Visibility := Invisible;
    begin
        case Key is
            when KEY_UP => -- Select previous field
                Result := Driver(MoveForm, F_Previous_Field);
                FieldIndex := Get_Index(Current(MoveForm));
                if FieldIndex = 2 or FieldIndex = 4 then
                    Result := Driver(MoveForm, F_End_Line);
                end if;
            when KEY_DOWN => -- Select next field
                Result := Driver(MoveForm, F_Next_Field);
                FieldIndex := Get_Index(Current(MoveForm));
                if FieldIndex = 2  or FieldIndex = 4 then
                    Result := Driver(MoveForm, F_End_Line);
                end if;
            when 10 => -- quit/move map
                if FieldIndex = 6 then
                    MoveX := Integer'Value(Get_Buffer(Fields(MoveForm, 2))) - PlayerShip.SkyX;
                    MoveY := Integer'Value(Get_Buffer(Fields(MoveForm, 4))) - PlayerShip.SkyY;
                end if;
                Set_Cursor_Visibility(Visibility);
                Post(MoveForm, False);
                Delete(MoveForm);
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when KEY_BACKSPACE => -- delete last character
                if FieldIndex = 2 or FieldIndex = 4 then
                    Result := Driver(MoveForm, F_Delete_Previous);
                end if;
            when KEY_DC => -- delete character at cursor
                if FieldIndex = 2 or FieldIndex = 4 then
                    Result := Driver(MoveForm, F_Delete_Char);
                end if;
            when KEY_RIGHT => -- Move cursor right
                if FieldIndex = 2 or FieldIndex = 4 then
                    Result := Driver(MoveForm, F_Right_Char);
                end if;
            when KEY_LEFT => -- Move cursor left
                if FieldIndex = 2 or FieldIndex = 4 then
                    Result := Driver(MoveForm, F_Left_Char);
                end if;
            when others =>
                Result := Driver(MoveForm, Key);
        end case;
        if Result = Form_Ok then
            Set_Background(Fields(MoveForm, 2), (others => False));
            Set_Background(Fields(MoveForm, 4), (others => False));
            if FieldIndex = 2 or FieldIndex = 4 then
                Set_Background(Current(MoveForm), (Reverse_Video => True, others => False));
            end if;
            Refresh(FormWindow);
        end if;
        return Move_Map;
    end MoveFormKeys;

end Maps;
