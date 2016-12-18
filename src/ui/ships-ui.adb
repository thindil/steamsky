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

with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Messages; use Messages;
with Ships; use Ships;
with Items; use Items;
with UserInterface; use UserInterface;

package body Ships.UI is

    RenameForm : Form;
    FormWindow : Window;

    procedure ShowShipForm(OptionText : String; MaxRange : Natural := 0) is
        Rename_Fields : constant Field_Array_Access := new Field_Array(1..5);
        FieldOptions : Field_Option_Set;
        FormHeight : Line_Position;
        FormLength : Column_Position;
        Visibility : Cursor_Visibility := Normal;
    begin
        if RenameForm = Null_Form then
            Set_Cursor_Visibility(Visibility);
            Rename_Fields.all(1) := New_Field(1, OptionText'Length, 0, 0, 0, 0);
            FieldOptions := Get_Options(Rename_Fields.all(1));
            Set_Buffer(Rename_Fields.all(1), 0, OptionText);
            FieldOptions.Active := False;
            Set_Options(Rename_Fields.all(1), FieldOptions);
            Rename_Fields.all(2) := New_Field(1, 20, 0, OptionText'Length, 0, 0);
            FieldOptions := Get_Options(Rename_Fields.all(2));
            FieldOptions.Auto_Skip := False;
            Set_Options(Rename_Fields.all(2), FieldOptions);
            Set_Background(Rename_Fields.all(2), (Reverse_Video => True, others => False));
            if MaxRange > 0 then
                Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type(Rename_Fields.all(2), (0, 0, MaxRange));
            end if;
            Rename_Fields.all(3) := New_Field(1, 8, 2, (OptionText'Length / 2), 0, 0);
            Set_Buffer(Rename_Fields.all(3), 0, "[Cancel]");
            FieldOptions := Get_Options(Rename_Fields.all(3));
            FieldOptions.Edit := False;
            Set_Options(Rename_Fields.all(3), FieldOptions);
            Rename_Fields.all(4) := New_Field(1, 4, 2, (OptionText'Length / 2) + 10, 0, 0);
            FieldOptions := Get_Options(Rename_Fields.all(4));
            FieldOptions.Edit := False;
            Set_Options(Rename_Fields.all(4), FieldOptions);
            Set_Buffer(Rename_Fields.all(4), 0, "[Ok]");
            Rename_Fields.all(5) := Null_Field;
            RenameForm := New_Form(Rename_Fields);
            Scale(RenameForm, FormHeight, FormLength);
            FormWindow := Create(FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)), ((Columns / 2) - (FormLength / 2)));
            Box(FormWindow);
            Set_Window(RenameForm, FormWindow);
            Set_Sub_Window(RenameForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
            Post(RenameForm);
        end if;
        Refresh;
        Refresh(FormWindow);
    end ShowShipForm;

    function RenameResult(CurrentState : GameStates) return GameStates is
        Visibility : Cursor_Visibility := Invisible;
        ModuleIndex : constant Positive := Get_Index(Current(ShipsMenu));
        FieldIndex : constant Positive := Get_Index(Current(RenameForm));
        NewName : Unbounded_String;
        SemicolonIndex : Natural;
    begin
        if FieldIndex < 3 then
            return CurrentState;
        elsif FieldIndex = 4 then
            NewName := Trim(To_Unbounded_String(Get_Buffer(Fields(RenameForm, 2))), Ada.Strings.Both);
            if Length(NewName) > 0 then
                SemicolonIndex := Index(NewName, ";");
                while SemicolonIndex > 0 loop
                    Delete(NewName, SemicolonIndex, SemicolonIndex);
                    SemicolonIndex := Index(NewName, ";");
                end loop;
                if CurrentState = Rename_Module then
                    UpdateModule(PlayerShip, ModuleIndex, "Name", To_String(NewName));
                else
                    PlayerShip.Name := NewName;
                end if;
            end if;
        end if;
        Set_Cursor_Visibility(Visibility);
        Post(RenameForm, False);
        Delete(RenameForm);
        DrawGame(Ship_Info);
        return Ship_Info;
    end RenameResult;

    function DropCargoResult return GameStates is
        ItemIndex : constant Positive := Get_Index(Current(ShipsMenu));
        ItemName : constant String := To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name);
        DropAmount : Natural;
        Visibility : Cursor_Visibility := Invisible;
        FieldIndex : constant Positive := Get_Index(Current(RenameForm));
    begin
        if FieldIndex < 3 then
            return Drop_Cargo;
        elsif FieldIndex = 4 then
            DropAmount := Natural'Value(Get_Buffer(Fields(RenameForm, 2)));
            UpdateCargo(PlayerShip, PlayerShip.Cargo.Element(ItemIndex).ProtoIndex, (0 - DropAmount));
            AddMessage("You dropped" & Positive'Image(DropAmount) & " " & ItemName, OtherMessage);
        end if;
        Set_Cursor_Visibility(Visibility);
        Post(RenameForm, False);
        Delete(RenameForm);
        DrawGame(Cargo_Info);
        return Cargo_Info;
    exception
        when CONSTRAINT_ERROR =>
            Set_Cursor_Visibility(Visibility);
            Post(RenameForm, False);
            Delete(RenameForm);
            DrawGame(Cargo_Info);
            return Cargo_Info;
    end DropCargoResult;

    function ShipFormKeys(Key : Key_Code; CurrentState : GameStates) return GameStates is
        Result : Forms.Driver_Result;
        FieldIndex : Positive := Get_Index(Current(RenameForm));
    begin
        case Key is
            when KEY_UP => -- Select previous field
                Result := Driver(RenameForm, F_Previous_Field);
                FieldIndex := Get_Index(Current(RenameForm));
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_End_Line);
                end if;
            when KEY_DOWN => -- Select next field
                Result := Driver(RenameForm, F_Next_Field);
                FieldIndex := Get_Index(Current(RenameForm));
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_End_Line);
                end if;
            when 10 => -- quit/rename module/drop cargo
                if CurrentState = Drop_Cargo then
                    return DropCargoResult;
                else
                    return RenameResult(CurrentState);
                end if;
            when KEY_BACKSPACE => -- delete last character
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Delete_Previous);
                    if Result = Form_Ok then
                        FieldIndex := Get_Index(Current(RenameForm));
                        if FieldIndex /= 2 then
                            Set_Current(RenameForm, Fields(RenameForm, 2));
                        end if;
                    end if;
                end if;
            when KEY_DC => -- delete character at cursor
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Delete_Char);
                end if;
            when KEY_RIGHT => -- Move cursor right
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Right_Char);
                end if;
            when KEY_LEFT => -- Move cursor left
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Left_Char);
                end if;
            when others =>
                Result := Driver(RenameForm, Key);
        end case;
        if Result = Form_Ok then
            if FieldIndex = 2 then
                Set_Background(Current(RenameForm), (Reverse_Video => True, others => False));
            else
                Set_Background(Fields(RenameForm, 2), (others => False));
            end if;
            Refresh(FormWindow);
        end if;
        return CurrentState;
    end ShipFormKeys;

end Ships.UI;
