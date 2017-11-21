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
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Config; use Config;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body GameOptions is

   OptionsForm: Forms.Form;
   FormWindow: Window;
   KeySetting: Boolean := False;

   procedure ShowOptions is
      Options_Fields: constant Field_Array_Access := new Field_Array(1 .. 103);
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
      AutoStopEnum: constant Enumeration_Info :=
        (C => 4,
         Names =>
           (new String'("Never ->"),
            new String'("Any ship ->"),
            new String'("Friendly ship ->"),
            new String'("Enemy ship ->")),
         Case_Sensitive => False,
         Match_Must_Be_Unique => False);
      procedure CreateLabel
        (Line: Line_Position;
         Text: String;
         Page: Positive := 1) is
         FormIndex: Positive := Natural(Line * 2) + 1;
      begin
         if Page = 2 then
            FormIndex := FormIndex + 22;
         elsif Page = 3 then
            FormIndex := FormIndex + 44;
         elsif Page = 4 then
            FormIndex := FormIndex + 66;
         end if;
         Options_Fields.all(FormIndex) := New_Field(1, 40, Line, 0, 0, 0);
         Set_Buffer(Options_Fields.all(FormIndex), 0, Text);
         FieldOptions := Get_Options(Options_Fields.all(FormIndex));
         FieldOptions.Active := False;
         Set_Options(Options_Fields.all(FormIndex), FieldOptions);
      end CreateLabel;
      procedure CreateKeyField(Line: Line_Position; Page: Positive := 2) is
         FormIndex: Positive := Natural(Line * 2) + 1;
         Key: Key_Code;
      begin
         if Page = 2 then
            FormIndex := FormIndex + 23;
            Key := Key_Code(GameSettings.Keys(Positive(Line + 1)));
         elsif Page = 3 then
            FormIndex := FormIndex + 45;
            Key := Key_Code(GameSettings.Keys(Positive(Line + 11)));
         elsif Page = 4 then
            FormIndex := FormIndex + 67;
            Key := Key_Code(GameSettings.Keys(Positive(Line + 21)));
         end if;
         Options_Fields.all(FormIndex) := New_Field(1, 20, Line, 41, 0, 0);
         FieldOptions := Get_Options(Options_Fields.all(FormIndex));
         FieldOptions.Edit := False;
         Set_Options(Options_Fields.all(FormIndex), FieldOptions);
         Set_Buffer(Options_Fields.all(FormIndex), 0, GetKeyName(Key));
      end CreateKeyField;
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
      Set_Foreground(Options_Fields.all(2), BoldCharacters, 11);
      Set_Background(Options_Fields.all(2), BoldCharacters, 11);
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
      CreateLabel(5, "Low level of fuel: ");
      Options_Fields.all(12) := New_Field(1, 8, 5, 41, 0, 0);
      Set_Buffer
        (Options_Fields.all(12),
         0,
         Positive'Image(GameSettings.LowFuel));
      Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
        (Options_Fields.all(12),
         (0, 1, 10000));
      CreateLabel(6, "Low level of drinks: ");
      Options_Fields.all(14) := New_Field(1, 8, 6, 41, 0, 0);
      Set_Buffer
        (Options_Fields.all(14),
         0,
         Positive'Image(GameSettings.LowDrinks));
      Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
        (Options_Fields.all(14),
         (0, 1, 10000));
      CreateLabel(7, "Low level of food: ");
      Options_Fields.all(16) := New_Field(1, 8, 7, 41, 0, 0);
      Set_Buffer
        (Options_Fields.all(16),
         0,
         Positive'Image(GameSettings.LowFood));
      Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
        (Options_Fields.all(16),
         (0, 1, 10000));
      CreateLabel(8, "Stop auto movement: ");
      Options_Fields.all(18) := New_Field(1, 16, 8, 41, 0, 0);
      Set_Field_Type(Options_Fields.all(18), Create(AutoStopEnum, True));
      case GameSettings.AutoMoveStop is
         when NEVER =>
            Set_Buffer(Options_Fields.all(18), 0, "Never ->");
         when ANY =>
            Set_Buffer(Options_Fields.all(18), 0, "Any ship ->");
         when FRIENDLY =>
            Set_Buffer(Options_Fields.all(18), 0, "Friendly ship ->");
         when ENEMY =>
            Set_Buffer(Options_Fields.all(18), 0, "Enemy ship ->");
      end case;
      FieldOptions := Get_Options(Options_Fields.all(18));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(18), FieldOptions);
      Options_Fields.all(19) := New_Field(1, 30, 10, 0, 0, 0);
      Set_Buffer(Options_Fields.all(19), 0, "Ship movement keys settings ->");
      FieldOptions := Get_Options(Options_Fields.all(19));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(19), FieldOptions);
      Options_Fields.all(20) := New_Field(1, 33, 11, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(20),
         0,
         "Map manipulation keys settings ->");
      FieldOptions := Get_Options(Options_Fields.all(20));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(20), FieldOptions);
      Options_Fields.all(21) := New_Field(1, 26, 12, 0, 0, 0);
      Set_Buffer(Options_Fields.all(21), 0, "Menu shortcuts settings ->");
      FieldOptions := Get_Options(Options_Fields.all(21));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(21), FieldOptions);
      Options_Fields.all(22) := New_Field(6, Columns, 14, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(22),
         0,
         "Wait for crew is rested when pilot or engineer are too tired to work.");
      FieldOptions := Get_Options(Options_Fields.all(22));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(22), FieldOptions);
      CreateLabel(0, "Move ship up: ", 2);
      Set_New_Page(Options_Fields.all(23));
      CreateKeyField(0);
      CreateLabel(1, "Move ship down: ", 2);
      CreateKeyField(1);
      CreateLabel(2, "Move ship right: ", 2);
      CreateKeyField(2);
      CreateLabel(3, "Move ship left: ", 2);
      CreateKeyField(3);
      CreateLabel(4, "Move ship down/left: ", 2);
      CreateKeyField(4);
      CreateLabel(5, "Move ship down/right: ", 2);
      CreateKeyField(5);
      CreateLabel(6, "Move ship up/left: ", 2);
      CreateKeyField(6);
      CreateLabel(7, "Move ship up/right: ", 2);
      CreateKeyField(7);
      CreateLabel(8, "Wait 1 min or move 1 field: ", 2);
      CreateKeyField(8);
      CreateLabel(9, "Auto move ship: ", 2);
      CreateKeyField(9);
      Options_Fields.all(43) := New_Field(1, 7, 11, 0, 0, 0);
      Set_Buffer(Options_Fields.all(43), 0, "<- Back");
      FieldOptions := Get_Options(Options_Fields.all(43));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(43), FieldOptions);
      Options_Fields.all(44) := New_Field(6, Columns, 13, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(44),
         0,
         "Press Enter to start setting new key.");
      FieldOptions := Get_Options(Options_Fields.all(44));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(44), FieldOptions);
      CreateLabel(0, "Move map up: ", 3);
      Set_New_Page(Options_Fields.all(45));
      CreateKeyField(0, 3);
      CreateLabel(1, "Move map down: ", 3);
      CreateKeyField(1, 3);
      CreateLabel(2, "Move map right: ", 3);
      CreateKeyField(2, 3);
      CreateLabel(3, "Move map left: ", 3);
      CreateKeyField(3, 3);
      CreateLabel(4, "Move map up/left: ", 3);
      CreateKeyField(4, 3);
      CreateLabel(5, "Move map up/right: ", 3);
      CreateKeyField(5, 3);
      CreateLabel(6, "Move map down/left: ", 3);
      CreateKeyField(6, 3);
      CreateLabel(7, "Move map down/right: ", 3);
      CreateKeyField(7, 3);
      CreateLabel(8, "Center map on ship: ", 3);
      CreateKeyField(8, 3);
      CreateLabel(9, "Set destination for ship: ", 3);
      CreateKeyField(9, 3);
      Options_Fields.all(65) := New_Field(1, 7, 11, 0, 0, 0);
      Set_Buffer(Options_Fields.all(65), 0, "<- Back");
      FieldOptions := Get_Options(Options_Fields.all(65));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(65), FieldOptions);
      Options_Fields.all(66) := New_Field(6, Columns, 13, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(66),
         0,
         "Press Enter to start setting new key.");
      FieldOptions := Get_Options(Options_Fields.all(66));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(66), FieldOptions);
      CreateLabel(0, "Ship informations: ", 4);
      Set_New_Page(Options_Fields.all(67));
      CreateKeyField(0, 4);
      CreateLabel(1, "Ship cargo: ", 4);
      CreateKeyField(1, 4);
      CreateLabel(2, "Crew informations: ", 4);
      CreateKeyField(2, 4);
      CreateLabel(3, "Ship orders: ", 4);
      CreateKeyField(3, 4);
      CreateLabel(4, "Crafting: ", 4);
      CreateKeyField(4, 4);
      CreateLabel(5, "Last messages: ", 4);
      CreateKeyField(5, 4);
      CreateLabel(6, "List of known bases: ", 4);
      CreateKeyField(6, 4);
      CreateLabel(7, "List of known events: ", 4);
      CreateKeyField(7, 4);
      CreateLabel(8, "Accepted missions: ", 4);
      CreateKeyField(8, 4);
      CreateLabel(9, "Wait orders: ", 4);
      CreateKeyField(9, 4);
      CreateLabel(10, "Move map position: ", 4);
      CreateKeyField(10, 4);
      CreateLabel(11, "Game statistics: ", 4);
      CreateKeyField(11, 4);
      CreateLabel(12, "Help: ", 4);
      CreateKeyField(12, 4);
      CreateLabel(13, "Game options: ", 4);
      CreateKeyField(13, 4);
      CreateLabel(14, "Quit from game: ", 4);
      CreateKeyField(14, 4);
      CreateLabel(15, "Resign from game: ", 4);
      CreateKeyField(15, 4);
      CreateLabel(16, "Close menu: ", 4);
      CreateKeyField(16, 4);
      Options_Fields.all(101) := New_Field(1, 7, 18, 0, 0, 0);
      Set_Buffer(Options_Fields.all(101), 0, "<- Back");
      FieldOptions := Get_Options(Options_Fields.all(101));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(101), FieldOptions);
      Options_Fields.all(102) := New_Field(6, Columns, 20, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(102),
         0,
         "Press Enter to start setting new key.");
      FieldOptions := Get_Options(Options_Fields.all(102));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(102), FieldOptions);
      Options_Fields.all(103) := Null_Field;
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
      Visibility: Cursor_Visibility := Invisible;
      procedure SetDescription(Description: String; Field: Positive) is
         FieldsNumbers: constant array(Positive range <>) of Positive :=
           (2,
            4,
            6,
            8,
            10,
            12,
            14,
            16,
            18,
            19,
            20,
            21,
            24,
            26,
            28,
            30,
            32,
            34,
            36,
            38,
            40,
            42,
            43,
            46,
            48,
            50,
            52,
            54,
            56,
            58,
            60,
            62,
            64,
            65,
            68,
            70,
            72,
            74,
            76,
            78,
            80,
            82,
            84,
            86,
            88,
            90,
            92,
            94,
            96,
            98,
            100,
            101);
         CursorFieldsNumbers: constant array(1 .. 3) of Positive :=
           (12, 14, 16);
      begin
         if Field < 22 then
            Set_Buffer(Fields(OptionsForm, 22), 0, Description);
         elsif Field < 44 then
            Set_Buffer(Fields(OptionsForm, 44), 0, Description);
         elsif Field < 66 then
            Set_Buffer(Fields(OptionsForm, 66), 0, Description);
         else
            Set_Buffer(Fields(OptionsForm, 102), 0, Description);
         end if;
         Set_Foreground(Current(OptionsForm), BoldCharacters, 11);
         for CursorField of CursorFieldsNumbers loop
            if CursorField = Field then
               Visibility := Normal;
               Set_Background(Current(OptionsForm), BoldCharacters, 11);
               Result := Driver(OptionsForm, F_End_Line);
               exit;
            end if;
         end loop;
         Set_Cursor_Visibility(Visibility);
         for FieldNumber of FieldsNumbers loop
            if FieldNumber /= Field then
               Set_Foreground(Fields(OptionsForm, FieldNumber));
               Set_Background(Fields(OptionsForm, FieldNumber));
            end if;
         end loop;
      end SetDescription;
      function SetKey(KeyName: String) return Integer is
      begin
         if KeyName = "Keypad Up" then
            return 56;
         elsif KeyName = "Keypad Down" then
            return 50;
         elsif KeyName = "Keypad Right" then
            return 54;
         elsif KeyName = "Keypad Left" then
            return 52;
         elsif KeyName = "Keypad End" then
            return 49;
         elsif KeyName = "Keypad PageDown" then
            return 51;
         elsif KeyName = "Keypad Home" then
            return 55;
         elsif KeyName = "Keypad PageUp" then
            return 57;
         elsif KeyName = "Arrow Up" then
            return Integer(KEY_UP);
         elsif KeyName = "Arrow Down" then
            return Integer(KEY_DOWN);
         elsif KeyName = "Arrow Right" then
            return Integer(KEY_RIGHT);
         elsif KeyName = "Arrow Left" then
            return Integer(KEY_LEFT);
         elsif KeyName = "Space" then
            return 32;
         elsif KeyName = "Page Up" then
            return Integer(KEY_PPAGE);
         elsif KeyName = "Page Down" then
            return Integer(KEY_NPAGE);
         elsif KeyName = "Home" then
            return Integer(Key_Home);
         elsif KeyName = "End" then
            return Integer(Key_End);
         elsif KeyName = "Delete" then
            return Integer(KEY_DC);
         elsif KeyName = "Insert" then
            return Integer(KEY_IC);
         elsif KeyName = "Shift + Arrow Up" then
            return 337;
         elsif KeyName = "Shift + Arrow Down" then
            return 336;
         elsif KeyName = "Shift + Arrow Right" then
            return Integer(KEY_SRIGHT);
         elsif KeyName = "Shift + Arrow Left" then
            return Integer(KEY_SLEFT);
         elsif KeyName = "Shift + Home" then
            return Integer(KEY_SHOME);
         elsif KeyName = "Shift + Page Up" then
            return Integer(KEY_SPREVIOUS);
         elsif KeyName = "Shift + End" then
            return Integer(KEY_SEND);
         elsif KeyName = "Shift + Page Down" then
            return Integer(KEY_SNEXT);
         elsif KeyName = "Enter" then
            return 10;
         elsif KeyName = "Escape" then
            return 27;
         elsif KeyName = "Shift + Insert" then
            return Integer(KEY_SIC);
         elsif KeyName = "Shift + Delete" then
            return Integer(KEY_SDC);
         elsif KeyName = "Backspace" then
            return Integer(Key_Backspace);
         elsif KeyName = "Backspace2" then
            return 127;
         elsif KeyName = "Tab" then
            return 9;
         elsif KeyName = "Shift + Tab" then
            return 353;
         elsif KeyName = "F1" then
            return Integer(Key_F1);
         elsif KeyName = "F2" then
            return Integer(Key_F2);
         elsif KeyName = "F3" then
            return Integer(Key_F3);
         elsif KeyName = "F4" then
            return Integer(Key_F4);
         elsif KeyName = "F5" then
            return Integer(Key_F5);
         elsif KeyName = "F6" then
            return Integer(Key_F6);
         elsif KeyName = "F7" then
            return Integer(Key_F7);
         elsif KeyName = "F8" then
            return Integer(Key_F8);
         elsif KeyName = "F9" then
            return Integer(Key_F9);
         elsif KeyName = "F10" then
            return Integer(Key_F10);
         elsif KeyName = "F11" then
            return Integer(Key_F11);
         elsif KeyName = "F12" then
            return Integer(Key_F12);
         elsif KeyName = "Shift + F1" then
            return 277;
         elsif KeyName = "Shift + F2" then
            return 278;
         elsif KeyName = "Shift + F3" then
            return 279;
         elsif KeyName = "Shift + F4" then
            return 280;
         elsif KeyName = "Shift + F5" then
            return 281;
         elsif KeyName = "Shift + F6" then
            return 282;
         elsif KeyName = "Shift + F7" then
            return 283;
         elsif KeyName = "Shift + F8" then
            return 284;
         elsif KeyName = "Shift + F9" then
            return 285;
         elsif KeyName = "Shift + F10" then
            return 286;
         elsif KeyName = "Shift + F11" then
            return 287;
         elsif KeyName = "Shift + F12" then
            return 288;
         elsif KeyName'Length = 1 then
            return Character'Pos(KeyName(KeyName'First));
         end if;
         return Integer'Value(KeyName);
      end SetKey;
      procedure SetNewKey is
         NewFieldIndex: Positive;
      begin
         Result := Unknown_Request;
         for I in 1 .. 37 loop
            if I < 11 then
               NewFieldIndex := 22 + (I * 2);
            elsif I < 21 then
               NewFieldIndex := 24 + (I * 2);
            else
               NewFieldIndex := 26 + (I * 2);
            end if;
            FieldValue :=
              Trim
                (To_Unbounded_String
                   (Get_Buffer(Fields(OptionsForm, NewFieldIndex))),
                 Side => Both);
            if SetKey(To_String(FieldValue)) = Integer(Key) and
              NewFieldIndex /= FieldIndex then
               SetDescription
                 ("You can't use '" &
                  To_String(FieldValue) &
                  "' because it is set for other action. Please choice another key.",
                  FieldIndex);
               Refresh(FormWindow);
               return;
            end if;
         end loop;
         Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
         SetDescription
           ("New key was set. Press Enter again to start setting key.",
            FieldIndex);
         Refresh(FormWindow);
         KeySetting := False;
      end SetNewKey;
   begin
      case Key is
         when KEY_UP => -- Select previous field or set up arrow for key shortcut
            if not KeySetting then
               Result := Driver(OptionsForm, F_Previous_Field);
               FieldIndex := Get_Index(Current(OptionsForm));
            else
               SetNewKey;
            end if;
         when KEY_DOWN => -- Select next field or set down arrow for key shortcut
            if not KeySetting then
               Result := Driver(OptionsForm, F_Next_Field);
               FieldIndex := Get_Index(Current(OptionsForm));
            else
               SetNewKey;
            end if;
         when 10 => -- change option value or start setting key
            if not KeySetting then
               if FieldIndex < 11 or FieldIndex = 18 then
                  Result := Driver(OptionsForm, F_Next_Choice);
               elsif FieldIndex = 19 then
                  Set_Page(OptionsForm, 1);
                  Result := Form_Ok;
                  FieldIndex := 24;
               elsif FieldIndex = 20 then
                  Set_Page(OptionsForm, 2);
                  Result := Form_Ok;
                  FieldIndex := 46;
               elsif FieldIndex = 21 then
                  Set_Page(OptionsForm, 3);
                  Result := Form_Ok;
                  FieldIndex := 68;
               elsif (FieldIndex > 23 and FieldIndex < 43) or
                 (FieldIndex > 45 and FieldIndex < 65) or
                 (FieldIndex > 67 and FieldIndex < 101) then
                  SetDescription
                    ("Press new key for set this shortcut.",
                     FieldIndex);
                  KeySetting := True;
                  Refresh(FormWindow);
                  return GameOptions_View;
               elsif FieldIndex = 43 or
                 FieldIndex = 65 or
                 FieldIndex = 101 then
                  Result := Driver(OptionsForm, F_First_Page);
                  FieldIndex := 2;
               end if;
            else
               SetNewKey;
            end if;
         when 27 => -- Save options and quit to map or set Esc as key shortcut
            if not KeySetting then
               Visibility := Invisible;
               Set_Cursor_Visibility(Visibility);
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
               for I in 1 .. 37 loop
                  if I < 11 then
                     FieldIndex := 22 + (I * 2);
                  elsif I < 21 then
                     FieldIndex := 24 + (I * 2);
                  else
                     FieldIndex := 26 + (I * 2);
                  end if;
                  FieldValue :=
                    Trim
                      (To_Unbounded_String
                         (Get_Buffer(Fields(OptionsForm, FieldIndex))),
                       Side => Both);
                  GameSettings.Keys(I) := SetKey(To_String(FieldValue));
               end loop;
               FieldValue :=
                 To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 12)));
               GameSettings.LowFuel := Positive'Value(To_String(FieldValue));
               FieldValue :=
                 To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 14)));
               GameSettings.LowDrinks := Positive'Value(To_String(FieldValue));
               FieldValue :=
                 To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 16)));
               GameSettings.LowFood := Positive'Value(To_String(FieldValue));
               FieldValue :=
                 Trim
                   (To_Unbounded_String(Get_Buffer(Fields(OptionsForm, 18))),
                    Side => Both);
               if FieldValue = To_Unbounded_String("Never ->") then
                  GameSettings.AutoMoveStop := NEVER;
               elsif FieldValue = To_Unbounded_String("Any ship ->") then
                  GameSettings.AutoMoveStop := ANY;
               elsif FieldValue = To_Unbounded_String("Friendly ship ->") then
                  GameSettings.AutoMoveStop := FRIENDLY;
               elsif FieldValue = To_Unbounded_String("Enemy ship ->") then
                  GameSettings.AutoMoveStop := ENEMY;
               end if;
               SaveConfig;
               Post(OptionsForm, False);
               Delete(OptionsForm);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            else
               SetNewKey;
            end if;
         when KEY_RIGHT => -- Select next value
            if not KeySetting then
               if FieldIndex < 12 or FieldIndex = 18 then
                  Result := Driver(OptionsForm, F_Next_Choice);
               elsif FieldIndex < 17 then
                  Result := Driver(OptionsForm, F_Right_Char);
               end if;
            else
               SetNewKey;
            end if;
         when KEY_LEFT => -- Select previous value
            if not KeySetting then
               if FieldIndex < 12 or FieldIndex = 18 then
                  Result := Driver(OptionsForm, F_Previous_Choice);
               elsif FieldIndex < 17 then
                  Result := Driver(OptionsForm, F_Left_Char);
               end if;
            else
               SetNewKey;
            end if;
         when Key_Backspace | 127 => -- delete last character
            if not KeySetting then
               if FieldIndex > 11 and FieldIndex < 17 then
                  Result := Driver(OptionsForm, F_Delete_Previous);
                  if Result = Form_Ok then
                     FieldIndex := Get_Index(Current(OptionsForm));
                     if FieldIndex < 12 then
                        Set_Current(OptionsForm, Fields(OptionsForm, 12));
                        FieldIndex := 12;
                     end if;
                  end if;
               end if;
            else
               SetNewKey;
            end if;
         when KEY_DC => -- delete character at cursor
            if not KeySetting then
               if FieldIndex > 11 and FieldIndex < 17 then
                  Result := Driver(OptionsForm, F_Delete_Char);
               end if;
            else
               SetNewKey;
            end if;
         when others =>
            if KeySetting then
               SetNewKey;
            else
               Result := Driver(OptionsForm, Key);
            end if;
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
            when 12 =>
               SetDescription
                 ("Amount of fuel below which you will see warning about low level of. Between 1 and 10000.",
                  12);
            when 14 =>
               SetDescription
                 ("Amount of drinks below which you will see warning about low level of. Between 1 and 10000.",
                  14);
            when 16 =>
               SetDescription
                 ("Amount of food below which you will see warning about low level of. Between 1 and 10000.",
                  16);
            when 18 =>
               SetDescription
                 ("Set when auto move ship should stop: never, on meet any ship, on meet friendly ship or on meet enemy ship.",
                  18);
            when 19 =>
               SetDescription
                 ("Set ship movement keys (directions, auto-move key, etc).",
                  FieldIndex);
            when 20 =>
               SetDescription
                 ("Set map manipulation keys (move map, center on ship, set destination, etc).",
                  FieldIndex);
            when 21 =>
               SetDescription
                 ("Set menu shortcuts keys (ship/crew/cargo info, crafting screen, etc).",
                  FieldIndex);
            when 24 .. 42 =>
               SetDescription("Press Enter to start setting key.", FieldIndex);
            when 43 | 65 | 101 =>
               SetDescription("Back to general game settings.", FieldIndex);
            when 45 .. 64 =>
               SetDescription("Press Enter to start setting key.", FieldIndex);
            when 68 .. 100 =>
               SetDescription("Press Enter to start setting key.", FieldIndex);
            when others =>
               null;
         end case;
         Refresh(FormWindow);
      end if;
      return GameOptions_View;
   end GameOptionsKeys;

end GameOptions;
