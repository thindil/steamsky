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
   KeySetting: Boolean := False;

   function GetKeyName(Key: Key_Code) return String is
   begin
      case Key is
         when 56 =>
            return "Keypad Up";
         when 50 =>
            return "Keypad Down";
         when 54 =>
            return "Keypad Right";
         when 52 =>
            return "Keypad Left";
         when 49 =>
            return "Keypad End";
         when 51 =>
            return "Keypad PageDown";
         when 55 =>
            return "Keypad Home";
         when 57 =>
            return "Keypad PageUp";
         when KEY_UP =>
            return "Arrow Up";
         when KEY_DOWN =>
            return "Arrow Down";
         when KEY_RIGHT =>
            return "Arrow Right";
         when KEY_LEFT =>
            return "Arrow Left";
         when 32 =>
            return "Space";
         when KEY_PPAGE =>
            return "Page Up";
         when KEY_NPAGE =>
            return "Page Down";
         when Key_Home =>
            return "Home";
         when Key_End =>
            return "End";
         when KEY_DC =>
            return "Delete";
         when KEY_IC =>
            return "Insert";
         when 337 =>
            return "Shift + Arrow Up";
         when 336 =>
            return "Shift + Arrow Down";
         when KEY_SRIGHT =>
            return "Shift + Arrow Right";
         when KEY_SLEFT =>
            return "Shift + Arrow Left";
         when KEY_SHOME =>
            return "Shift + Home";
         when KEY_SPREVIOUS =>
            return "Shift + Page Up";
         when KEY_SEND =>
            return "Shift + End";
         when KEY_SNEXT =>
            return "Shift + Page Down";
         when 10 =>
            return "Enter";
         when 27 =>
            return "Escape";
         when KEY_SIC =>
            return "Shift + Insert";
         when KEY_SDC =>
            return "Shift + Delete";
         when Key_Backspace =>
            return "Backspace";
         when 9 =>
            return "Tab";
         when 353 =>
            return "Shift + Tab";
         when Key_F1 =>
            return "F1";
         when Key_F2 =>
            return "F2";
         when Key_F3 =>
            return "F3";
         when Key_F4 =>
            return "F4";
         when Key_F5 =>
            return "F5";
         when Key_F6 =>
            return "F6";
         when Key_F7 =>
            return "F7";
         when Key_F8 =>
            return "F8";
         when Key_F9 =>
            return "F9";
         when Key_F10 =>
            return "F10";
         when Key_F11 =>
            return "F11";
         when Key_F12 =>
            return "F12";
         when 277 =>
            return "Shift + F1";
         when 278 =>
            return "Shift + F2";
         when 279 =>
            return "Shift + F3";
         when 280 =>
            return "Shift + F4";
         when 281 =>
            return "Shift + F5";
         when 282 =>
            return "Shift + F6";
         when 283 =>
            return "Shift + F7";
         when 284 =>
            return "Shift + F8";
         when 285 =>
            return "Shift + F9";
         when 286 =>
            return "Shift + F10";
         when 287 =>
            return "Shift + F11";
         when 288 =>
            return "Shift + F12";
         when others =>
            if Key in Normal_Key_Code then
               return "" & Character'Val(Key);
            else
               return To_String
                   (Trim
                      (To_Unbounded_String(Key_Code'Image(Key)),
                       Side => Both));
            end if;
      end case;
   end GetKeyName;

   procedure ShowOptions is
      Options_Fields: constant Field_Array_Access := new Field_Array(1 .. 58);
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
      procedure CreateLabel
        (Line: Line_Position;
         Text: String;
         Page: Positive := 1) is
         FormIndex: Positive := Natural(Line * 2) + 1;
      begin
         if Page = 2 then
            FormIndex := FormIndex + 13;
         elsif Page = 3 then
            FormIndex := FormIndex + 35;
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
            FormIndex := FormIndex + 14;
            Key := Key_Code(GameSettings.Keys(Positive(Line + 1)));
         elsif Page = 3 then
            FormIndex := FormIndex + 36;
            Key := Key_Code(GameSettings.Keys(Positive(Line + 11)));
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
      Options_Fields.all(11) := New_Field(1, 30, 6, 0, 0, 0);
      Set_Buffer(Options_Fields.all(11), 0, "Ship movement keys settings ->");
      FieldOptions := Get_Options(Options_Fields.all(11));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(11), FieldOptions);
      Options_Fields.all(12) := New_Field(1, 33, 7, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(12),
         0,
         "Map manipulation keys settings ->");
      FieldOptions := Get_Options(Options_Fields.all(12));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(12), FieldOptions);
      Options_Fields.all(13) := New_Field(6, Columns, 9, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(13),
         0,
         "Wait for crew is rested when pilot or engineer are too tired to work.");
      FieldOptions := Get_Options(Options_Fields.all(13));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(13), FieldOptions);
      CreateLabel(0, "Move ship up: ", 2);
      Set_New_Page(Options_Fields.all(14));
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
      Options_Fields.all(34) := New_Field(1, 7, 11, 0, 0, 0);
      Set_Buffer(Options_Fields.all(34), 0, "<- Back");
      FieldOptions := Get_Options(Options_Fields.all(34));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(34), FieldOptions);
      Options_Fields.all(35) := New_Field(6, Columns, 13, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(35),
         0,
         "Press Enter to start setting new key.");
      FieldOptions := Get_Options(Options_Fields.all(35));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(35), FieldOptions);
      CreateLabel(0, "Move map up: ", 3);
      Set_New_Page(Options_Fields.all(36));
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
      Options_Fields.all(56) := New_Field(1, 7, 11, 0, 0, 0);
      Set_Buffer(Options_Fields.all(56), 0, "<- Back");
      FieldOptions := Get_Options(Options_Fields.all(56));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(56), FieldOptions);
      Options_Fields.all(57) := New_Field(6, Columns, 13, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(57),
         0,
         "Press Enter to start setting new key.");
      FieldOptions := Get_Options(Options_Fields.all(57));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(57), FieldOptions);
      Options_Fields.all(58) := Null_Field;
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
           (2,
            4,
            6,
            8,
            10,
            11,
            12,
            15,
            17,
            19,
            21,
            23,
            25,
            27,
            29,
            31,
            33,
            34,
            37,
            39,
            41,
            43,
            45,
            47,
            49,
            51,
            53,
            55,
            56);
      begin
         if Field < 14 then
            Set_Buffer(Fields(OptionsForm, 13), 0, Description);
         elsif Field < 36 then
            Set_Buffer(Fields(OptionsForm, 35), 0, Description);
         else
            Set_Buffer(Fields(OptionsForm, 57), 0, Description);
         end if;
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
         for I in 1 .. 20 loop
            if I < 11 then
               NewFieldIndex := 13 + (I * 2);
            else
               NewFieldIndex := 15 + (I * 2);
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
               if FieldIndex < 11 then
                  Result := Driver(OptionsForm, F_Next_Choice);
               elsif FieldIndex = 11 then
                  Set_Page(OptionsForm, 1);
                  Result := Form_Ok;
                  FieldIndex := 14;
                  Set_Background
                    (Current(OptionsForm),
                     (Reverse_Video => True, others => False));
               elsif FieldIndex = 12 then
                  Set_Page(OptionsForm, 2);
                  Result := Form_Ok;
                  FieldIndex := 37;
                  Set_Background
                    (Current(OptionsForm),
                     (Reverse_Video => True, others => False));
               elsif (FieldIndex > 14 and FieldIndex < 34) or
                 (FieldIndex > 36 and FieldIndex < 56) then
                  SetDescription
                    ("Press new key for set this shortcut.",
                     FieldIndex);
                  KeySetting := True;
                  Refresh(FormWindow);
                  return GameOptions_View;
               elsif FieldIndex = 34 or FieldIndex = 56 then
                  Result := Driver(OptionsForm, F_First_Page);
                  FieldIndex := 2;
                  Set_Background
                    (Current(OptionsForm),
                     (Reverse_Video => True, others => False));
               end if;
            else
               SetNewKey;
            end if;
         when Character'Pos('q') |
           Character'Pos
             ('Q') => -- Save options and quit to map or set Q as key shortcut
            if not KeySetting then
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
               for I in 1 .. 20 loop
                  if I < 11 then
                     FieldIndex := 13 + (I * 2);
                  else
                     FieldIndex := 15 + (I * 2);
                  end if;
                  FieldValue :=
                    Trim
                      (To_Unbounded_String
                         (Get_Buffer(Fields(OptionsForm, FieldIndex))),
                       Side => Both);
                  GameSettings.Keys(I) := SetKey(To_String(FieldValue));
               end loop;
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
               Result := Driver(OptionsForm, F_Next_Choice);
            else
               SetNewKey;
            end if;
         when KEY_LEFT => -- Select previous value
            if not KeySetting then
               Result := Driver(OptionsForm, F_Previous_Choice);
            else
               SetNewKey;
            end if;
         when others =>
            if KeySetting then
               SetNewKey;
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
            when 11 =>
               SetDescription
                 ("Set ship movement keys (directions, auto-move key, etc).",
                  11);
            when 12 =>
               SetDescription
                 ("Set map manipulation keys (move map, center on ship, set destination, etc).",
                  12);
            when 15 .. 33 =>
               SetDescription("Press Enter to start setting key.", FieldIndex);
            when 34 | 56 =>
               SetDescription("Back to general game settings.", FieldIndex);
            when 36 .. 55 =>
               SetDescription("Press Enter to start setting key.", FieldIndex);
            when others =>
               null;
         end case;
         Refresh(FormWindow);
      end if;
      return GameOptions_View;
   end GameOptionsKeys;

end GameOptions;
