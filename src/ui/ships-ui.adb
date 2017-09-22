--    Copyright 2016-2017 Bartek thindil Jasicki
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

with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Messages; use Messages;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with UserInterface; use UserInterface;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   FormWindow: Window;

   procedure ShowShipForm(OptionText: String; MaxRange: Natural := 0) is
      Rename_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FieldOptions: Field_Option_Set;
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
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
         Set_Foreground(Rename_Fields.all(2), BoldCharacters, 11);
         Set_Background(Rename_Fields.all(2), BoldCharacters, 11);
         if MaxRange > 0 then
            Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
              (Rename_Fields.all(2),
               (0, 0, MaxRange));
         end if;
         Rename_Fields.all(3) :=
           New_Field(1, 8, 2, (OptionText'Length / 2), 0, 0);
         Set_Buffer(Rename_Fields.all(3), 0, "[Cancel]");
         FieldOptions := Get_Options(Rename_Fields.all(3));
         FieldOptions.Edit := False;
         Set_Options(Rename_Fields.all(3), FieldOptions);
         Rename_Fields.all(4) :=
           New_Field(1, 4, 2, (OptionText'Length / 2) + 10, 0, 0);
         FieldOptions := Get_Options(Rename_Fields.all(4));
         FieldOptions.Edit := False;
         Set_Options(Rename_Fields.all(4), FieldOptions);
         Set_Buffer(Rename_Fields.all(4), 0, "[Ok]");
         Rename_Fields.all(5) := Null_Field;
         RenameForm := New_Form(Rename_Fields);
         Scale(RenameForm, FormHeight, FormLength);
         FormWindow :=
           Create
             (FormHeight + 2,
              FormLength + 2,
              ((Lines / 3) - (FormHeight / 2)),
              ((Columns / 2) - (FormLength / 2)));
         WindowFrame(FormWindow, 5, "");
         Set_Window(RenameForm, FormWindow);
         Set_Sub_Window
           (RenameForm,
            Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
         Post(RenameForm);
      end if;
      Refresh;
      Refresh(FormWindow);
   end ShowShipForm;

   procedure SetCurrentField(FieldNumber: Positive) is
      Visibility: Cursor_Visibility := Invisible;
   begin
      Set_Foreground(Fields(RenameForm, 2));
      Set_Background(Fields(RenameForm, 2));
      Set_Current(RenameForm, Fields(RenameForm, FieldNumber));
      Set_Foreground(Current(RenameForm), BoldCharacters, 11);
      Set_Background(Current(RenameForm), BoldCharacters, 11);
      Set_Cursor_Visibility(Visibility);
      Refresh(FormWindow);
   end SetCurrentField;

   function RenameResult(CurrentState: GameStates) return GameStates is
      ModuleIndex: constant Positive := Get_Index(Current(ShipsMenu));
      FieldIndex: constant Positive := Get_Index(Current(RenameForm));
      NewName: Unbounded_String;
      SemicolonIndex: Natural;
      procedure UpdateName(Module: in out ModuleData) is
      begin
         Module.Name := NewName;
      end UpdateName;
   begin
      NewName :=
        Trim
          (To_Unbounded_String(Get_Buffer(Fields(RenameForm, 2))),
           Ada.Strings.Both);
      if FieldIndex = 2 then
         if Length(NewName) > 0 then
            SetCurrentField(4);
         else
            SetCurrentField(3);
         end if;
         return CurrentState;
      elsif FieldIndex = 4 then
         if Length(NewName) > 0 then
            SemicolonIndex := Index(NewName, ";");
            while SemicolonIndex > 0 loop
               Delete(NewName, SemicolonIndex, SemicolonIndex);
               SemicolonIndex := Index(NewName, ";");
            end loop;
            if CurrentState = Rename_Module then
               PlayerShip.Modules.Update_Element
               (Index => ModuleIndex, Process => UpdateName'Access);
            else
               PlayerShip.Name := NewName;
            end if;
         end if;
      end if;
      Post(RenameForm, False);
      Delete(RenameForm);
      DrawGame(Ship_Info);
      return Ship_Info;
   end RenameResult;

   function DropCargoResult return GameStates is
      ItemIndex: constant Positive := Get_Index(Current(ShipsMenu));
      DropAmount, DropAmount2: Natural;
      FieldIndex: constant Positive := Get_Index(Current(RenameForm));
   begin
      DropAmount := Natural'Value(Get_Buffer(Fields(RenameForm, 2)));
      if FieldIndex = 2 then
         SetCurrentField(4);
         return Drop_Cargo;
      elsif FieldIndex = 4 then
         if DropAmount > 0 and
           Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
             MissionItemsType then
            DropAmount2 := DropAmount;
            for J in 1 .. DropAmount2 loop
               for I in
                 PlayerShip.Missions.First_Index ..
                     PlayerShip.Missions.Last_Index loop
                  if PlayerShip.Missions(I).MType = Deliver and
                    PlayerShip.Missions(I).Target =
                      PlayerShip.Cargo(ItemIndex).ProtoIndex then
                     DeleteMission(I);
                     DropAmount := DropAmount - 1;
                     exit;
                  end if;
               end loop;
            end loop;
         end if;
         if DropAmount > 0 then
            AddMessage
              ("You dropped" &
               Positive'Image(DropAmount) &
               " " &
               GetItemName(PlayerShip.Cargo(ItemIndex)) &
               ".",
               OtherMessage);
            UpdateCargo
              (PlayerShip,
               PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
               (0 - DropAmount),
               PlayerShip.Cargo.Element(ItemIndex).Durability);
         end if;
      end if;
      Post(RenameForm, False);
      Delete(RenameForm);
      DrawGame(Cargo_Info);
      return Cargo_Info;
   exception
      when Constraint_Error =>
         if FieldIndex > 2 then
            Post(RenameForm, False);
            Delete(RenameForm);
            DrawGame(Cargo_Info);
            return Cargo_Info;
         end if;
         SetCurrentField(3);
         return Drop_Cargo;
   end DropCargoResult;

   function ShipFormKeys
     (Key: Key_Code;
      CurrentState: GameStates) return GameStates is
      Result: Forms.Driver_Result;
      FieldIndex: Positive := Get_Index(Current(RenameForm));
      Visibility: Cursor_Visibility := Invisible;
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
         when Key_Backspace => -- delete last character
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
         when 27 => -- Escape select cancel button, second time closes form
            if FieldIndex /= 3 then
               FieldIndex := 3;
               Set_Current(RenameForm, Fields(RenameForm, 3));
               Result := Form_Ok;
            else
               if CurrentState = Drop_Cargo then
                  return DropCargoResult;
               else
                  return RenameResult(CurrentState);
               end if;
            end if;
         when others =>
            Result := Driver(RenameForm, Key);
      end case;
      if Result = Form_Ok then
         for I in 2 .. 4 loop
            Set_Foreground(Fields(RenameForm, I));
            Set_Background(Fields(RenameForm, I));
         end loop;
         Set_Foreground(Current(RenameForm), BoldCharacters, 11);
         Set_Background(Current(RenameForm), BoldCharacters, 11);
         if FieldIndex = 2 then
            Visibility := Normal;
         end if;
         Set_Cursor_Visibility(Visibility);
         Refresh(FormWindow);
      end if;
      return CurrentState;
   end ShipFormKeys;

end Ships.UI;
