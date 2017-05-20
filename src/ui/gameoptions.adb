--    Copyright 2017 Bartek thindil Jasicki
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
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
use Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
with Config; use Config;
with UserInterface; use UserInterface;

package body GameOptions is

   OptionsForm: Forms.Form;
   FormWindow: Window;

   procedure ShowOptions is
      Options_Fields: constant Field_Array_Access := new Field_Array(1 .. 4);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      FieldOptions: Field_Option_Set;
      YesNo: constant Enumeration_Info :=
        (C => 2,
         Names => (new String'("Yes ->"), new String'("No ->")),
         Case_Sensitive => False,
         Match_Must_Be_Unique => False);
   begin
      Options_Fields.all(1) := New_Field(1, 10, 0, 0, 0, 0);
      Set_Buffer(Options_Fields.all(1), 0, "Auto rest: ");
      FieldOptions := Get_Options(Options_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(1), FieldOptions);
      Options_Fields.all(2) := New_Field(1, 6, 0, 11, 0, 0);
      Set_Field_Type(Options_Fields.all(2), Create(YesNo, True));
      if GameSettings.AutoRest then
         Set_Buffer(Options_Fields.all(2), 0, "Yes ->");
      else
         Set_Buffer(Options_Fields.all(2), 0, "No ->");
      end if;
      FieldOptions := Get_Options(Options_Fields.all(2));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(2), FieldOptions);
      Set_Background
        (Options_Fields.all(2),
         (Reverse_Video => True, others => False));
      Options_Fields.all(3) := New_Field(5, Columns, 3, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(3),
         0,
         "Wait for crew is rested when pilot or engineer are too tired to work.");
      FieldOptions := Get_Options(Options_Fields.all(3));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(3), FieldOptions);
      Options_Fields.all(4) := Null_Field;
      OptionsForm := New_Form(Options_Fields);
      Set_Options(OptionsForm, (others => False));
      Scale(OptionsForm, FormHeight, FormLength);
      FormWindow := Create(FormHeight + 2, FormLength + 2, 1, 0);
      Set_Window(OptionsForm, FormWindow);
      Set_Sub_Window
        (OptionsForm,
         Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(OptionsForm);
      Refresh;
      Refresh(FormWindow);
   end ShowOptions;

   function GameOptionsKeys(Key: Key_Code) return GameStates is
      Result: Forms.Driver_Result;
      FieldIndex: constant Positive := Get_Index(Current(OptionsForm));
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(OptionsForm, F_Previous_Field);
         when KEY_DOWN => -- Select next field
            Result := Driver(OptionsForm, F_Next_Field);
         when 10 => -- change option value
            if FieldIndex = 2 then
               Result := Driver(OptionsForm, F_Next_Choice);
            end if;
         when Character'Pos('q') |
           Character'Pos('Q') => -- Save options and quit to map
            if Get_Buffer(Fields(OptionsForm, 2)) = "Yes ->" then
               GameSettings.AutoRest := True;
            else
               GameSettings.AutoRest := False;
            end if;
            SaveConfig;
            Post(OptionsForm, False);
            Delete(OptionsForm);
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when Character'Pos('y') | Character'Pos('Y') => -- Select yes option
            if FieldIndex = 2 then
               Set_Buffer(Current(OptionsForm), 0, "Yes ->");
            else
               Result := Driver(OptionsForm, Key);
            end if;
         when Character'Pos('n') | Character'Pos('N') => -- Select no option
            if FieldIndex = 2 then
               Set_Buffer(Current(OptionsForm), 0, "Yes ->");
            else
               Result := Driver(OptionsForm, Key);
            end if;
         when KEY_RIGHT => -- Select next value
            if FieldIndex = 2 then
               Result := Driver(OptionsForm, F_Next_Choice);
            end if;
         when KEY_LEFT => -- Select previous value
            if FieldIndex = 2 then
               Result := Driver(OptionsForm, F_Previous_Choice);
            end if;
         when others =>
            Result := Driver(OptionsForm, Key);
      end case;
      if Result = Form_Ok then
         if FieldIndex /= 2 then
            Set_Background(Fields(OptionsForm, 2), (others => False));
         else
            Set_Buffer
              (Fields(OptionsForm, 3),
               0,
               "Wait for crew is rested when pilot or engineer are too tired to work.");
            Set_Background
              (Current(OptionsForm),
               (Reverse_Video => True, others => False));
         end if;
         Refresh(FormWindow);
      end if;
      return GameOptions_View;
   end GameOptionsKeys;

end GameOptions;
