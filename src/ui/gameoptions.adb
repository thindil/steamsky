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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
use Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
with Config; use Config;
with UserInterface; use UserInterface;
with Ships; use Ships;

package body GameOptions is

   OptionsForm: Forms.Form;
   FormWindow: Window;

   procedure ShowOptions is
      Options_Fields: constant Field_Array_Access := new Field_Array(1 .. 12);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      FieldOptions: Field_Option_Set;
      YesNo,
      YesNo2,
      YesNo3,
      YesNo4: constant Enumeration_Info :=
        (C => 2,
         Names => (new String'("Yes ->"), new String'("No ->")),
         Case_Sensitive => False,
         Match_Must_Be_Unique => False);
      SpeedEnum: constant Enumeration_Info :=
        (C => 4,
         Names =>
           (new String'("Full Stop ->"),
            new String'("Quarter Speed ->"),
            new String'("Half Speed ->"),
            new String'("Full Speed ->")),
         Case_Sensitive => False,
         Match_Must_Be_Unique => False);
      procedure CreateLabel(Line: Line_Position; Text: String) is
         FormIndex: constant Positive := Natural(Line * 2) + 1;
      begin
         Options_Fields.all(FormIndex) := New_Field(1, 40, Line, 0, 0, 0);
         Set_Buffer(Options_Fields.all(FormIndex), 0, Text);
         FieldOptions := Get_Options(Options_Fields.all(FormIndex));
         FieldOptions.Active := False;
         Set_Options(Options_Fields.all(FormIndex), FieldOptions);
      end CreateLabel;
   begin
      CreateLabel(0, "Auto rest when crew is tired: ");
      Options_Fields.all(2) := New_Field(1, 6, 0, 41, 0, 0);
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
      CreateLabel(1, "Default speed after undocking: ");
      Options_Fields.all(4) := New_Field(1, 16, 1, 41, 0, 0);
      Set_Field_Type(Options_Fields.all(4), Create(SpeedEnum, True));
      case GameSettings.UndockSpeed is
         when FULL_STOP =>
            Set_Buffer(Options_Fields.all(4), 0, "Full Stop ->");
         when QUARTER_SPEED =>
            Set_Buffer(Options_Fields.all(4), 0, "Quarter Speed ->");
         when HALF_SPEED =>
            Set_Buffer(Options_Fields.all(4), 0, "Half Speed ->");
         when FULL_SPEED =>
            Set_Buffer(Options_Fields.all(4), 0, "Full Speed ->");
         when others =>
            null;
      end case;
      FieldOptions := Get_Options(Options_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(4), FieldOptions);
      CreateLabel(2, "Auto center map after set destination: ");
      Options_Fields.all(6) := New_Field(1, 6, 2, 41, 0, 0);
      Set_Field_Type(Options_Fields.all(6), Create(YesNo2, True));
      if GameSettings.AutoCenter then
         Set_Buffer(Options_Fields.all(6), 0, "Yes ->");
      else
         Set_Buffer(Options_Fields.all(6), 0, "No ->");
      end if;
      FieldOptions := Get_Options(Options_Fields.all(6));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(6), FieldOptions);
      CreateLabel(3, "Auto set base after finished mission: ");
      Options_Fields.all(8) := New_Field(1, 6, 3, 41, 0, 0);
      Set_Field_Type(Options_Fields.all(8), Create(YesNo3, True));
      if GameSettings.AutoReturn then
         Set_Buffer(Options_Fields.all(8), 0, "Yes ->");
      else
         Set_Buffer(Options_Fields.all(8), 0, "No ->");
      end if;
      FieldOptions := Get_Options(Options_Fields.all(8));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(8), FieldOptions);
      CreateLabel(4, "Auto finish missions: ");
      Options_Fields.all(10) := New_Field(1, 6, 4, 41, 0, 0);
      Set_Field_Type(Options_Fields.all(10), Create(YesNo4, True));
      if GameSettings.AutoFinish then
         Set_Buffer(Options_Fields.all(10), 0, "Yes ->");
      else
         Set_Buffer(Options_Fields.all(10), 0, "No ->");
      end if;
      FieldOptions := Get_Options(Options_Fields.all(10));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(10), FieldOptions);
      Options_Fields.all(11) := New_Field(6, Columns, 7, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(11),
         0,
         "Wait for crew is rested when pilot or engineer are too tired to work.");
      FieldOptions := Get_Options(Options_Fields.all(11));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(11), FieldOptions);
      Options_Fields.all(12) := Null_Field;
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
      FieldIndex: Positive := Get_Index(Current(OptionsForm));
      FieldValue: Unbounded_String;
      procedure SetDescription(Description: String; Field: Positive) is
         FieldsNumbers: constant array(Positive range <>) of Positive :=
           (2, 4, 6, 8, 10);
      begin
         Set_Buffer(Fields(OptionsForm, 11), 0, Description);
         Set_Background
           (Current(OptionsForm),
            (Reverse_Video => True, others => False));
         for FieldNumber of FieldsNumbers loop
            if FieldNumber /= Field then
               Set_Background
                 (Fields(OptionsForm, FieldNumber),
                  (others => False));
            end if;
         end loop;
      end SetDescription;
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(OptionsForm, F_Previous_Field);
            FieldIndex := Get_Index(Current(OptionsForm));
         when KEY_DOWN => -- Select next field
            Result := Driver(OptionsForm, F_Next_Field);
            FieldIndex := Get_Index(Current(OptionsForm));
         when 10 => -- change option value
            Result := Driver(OptionsForm, F_Next_Choice);
         when Character'Pos('q') |
           Character'Pos('Q') => -- Save options and quit to map
            FieldValue :=
              To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 2)));
            if FieldValue = To_Unbounded_String("Yes ->") then
               GameSettings.AutoRest := True;
            else
               GameSettings.AutoRest := False;
            end if;
            FieldValue :=
              Trim
                (To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 4))),
                 Side => Both);
            if FieldValue = To_Unbounded_String("Full Stop ->") then
               GameSettings.UndockSpeed := FULL_STOP;
            elsif FieldValue = To_Unbounded_String("Half Speed ->") then
               GameSettings.UndockSpeed := HALF_SPEED;
            elsif FieldValue = To_Unbounded_String("Quarter Speed ->") then
               GameSettings.UndockSpeed := QUARTER_SPEED;
            elsif FieldValue = To_Unbounded_String("Full Speed ->") then
               GameSettings.UndockSpeed := FULL_SPEED;
            end if;
            FieldValue :=
              To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 6)));
            if FieldValue = To_Unbounded_String("Yes ->") then
               GameSettings.AutoCenter := True;
            else
               GameSettings.AutoCenter := False;
            end if;
            FieldValue :=
              To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 8)));
            if FieldValue = To_Unbounded_String("Yes ->") then
               GameSettings.AutoReturn := True;
            else
               GameSettings.AutoReturn := False;
            end if;
            FieldValue :=
              To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 10)));
            if FieldValue = To_Unbounded_String("Yes ->") then
               GameSettings.AutoFinish := True;
            else
               GameSettings.AutoFinish := False;
            end if;
            SaveConfig;
            Post(OptionsForm, False);
            Delete(OptionsForm);
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when KEY_RIGHT => -- Select next value
            Result := Driver(OptionsForm, F_Next_Choice);
         when KEY_LEFT => -- Select previous value
            Result := Driver(OptionsForm, F_Previous_Choice);
         when others =>
            Result := Driver(OptionsForm, Key);
      end case;
      if Result = Form_Ok then
         case FieldIndex is
            when 2 =>
               SetDescription
                 ("Wait for crew is rested when pilot or engineer are too tired to work.",
                  2);
            when 4 =>
               SetDescription
                 ("Default speed of ship after undock from base.",
                  4);
            when 6 =>
               SetDescription
                 ("After set destination for ship, center map on ship.",
                  6);
            when 8 =>
               SetDescription
                 ("After finished mission, set skybase from which mission was taken as a destination for ship.",
                  8);
            when 10 =>
               SetDescription
                 ("Auto finish missions when ship is near corresponding skybase.",
                  10);
            when others =>
               null;
         end case;
         Refresh(FormWindow);
      end if;
      return GameOptions_View;
   end GameOptionsKeys;

end GameOptions;
