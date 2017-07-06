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
      Options_Fields: constant Field_Array_Access := new Field_Array(1 .. 35);
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
            FormIndex := FormIndex + 12;
         end if;
         Options_Fields.all(FormIndex) := New_Field(1, 40, Line, 0, 0, 0);
         Set_Buffer(Options_Fields.all(FormIndex), 0, Text);
         FieldOptions := Get_Options(Options_Fields.all(FormIndex));
         FieldOptions.Active := False;
         Set_Options(Options_Fields.all(FormIndex), FieldOptions);
      end CreateLabel;
      procedure CreateKeyField(Line: Line_Position) is
         FormIndex: constant Positive := Natural(Line * 2) + 14;
         Key: constant Key_Code :=
           Key_Code(GameSettings.Keys(Positive(Line + 1)));
      begin
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
      Options_Fields.all(12) := New_Field(6, Columns, 8, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(12),
         0,
         "Wait for crew is rested when pilot or engineer are too tired to work.");
      FieldOptions := Get_Options(Options_Fields.all(12));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(12), FieldOptions);
      CreateLabel(0, "Move ship up: ", 2);
      Set_New_Page(Options_Fields.all(13));
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
      Options_Fields.all(33) := New_Field(1, 7, 11, 0, 0, 0);
      Set_Buffer(Options_Fields.all(33), 0, "<- Back");
      FieldOptions := Get_Options(Options_Fields.all(33));
      FieldOptions.Edit := False;
      Set_Options(Options_Fields.all(33), FieldOptions);
      Options_Fields.all(34) := New_Field(6, Columns, 13, 0, 0, 0);
      Set_Buffer
        (Options_Fields.all(34),
         0,
         "Press Enter to start setting new key.");
      FieldOptions := Get_Options(Options_Fields.all(34));
      FieldOptions.Active := False;
      Set_Options(Options_Fields.all(34), FieldOptions);
      Options_Fields.all(35) := Null_Field;
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
           (2, 4, 6, 8, 10, 11, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 33);
      begin
         if Field < 14 then
            Set_Buffer(Fields(OptionsForm, 12), 0, Description);
         else
            Set_Buffer(Fields(OptionsForm, 34), 0, Description);
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
         elsif KeyName'Length = 1 then
            return Character'Pos(KeyName(KeyName'First));
         end if;
         return Integer'Value(KeyName);
      end SetKey;
   begin
      case Key is
         when KEY_UP => -- Select previous field or set up arrow for key shortcut
            if not KeySetting then
               Result := Driver(OptionsForm, F_Previous_Field);
               FieldIndex := Get_Index(Current(OptionsForm));
            else
               Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
               Result := Form_Ok;
               KeySetting := False;
            end if;
         when KEY_DOWN => -- Select next field or set down arrow for key shortcut
            if not KeySetting then
               Result := Driver(OptionsForm, F_Next_Field);
               FieldIndex := Get_Index(Current(OptionsForm));
            else
               Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
               Result := Form_Ok;
               KeySetting := False;
            end if;
         when 10 => -- change option value
            if FieldIndex < 11 then
               Result := Driver(OptionsForm, F_Next_Choice);
            elsif FieldIndex = 11 then
               Result := Driver(OptionsForm, F_Next_Page);
               FieldIndex := 14;
               Set_Background
                 (Current(OptionsForm),
                  (Reverse_Video => True, others => False));
            elsif FieldIndex > 13 and FieldIndex < 33 then
               SetDescription
                 ("Press new key for set this shortcut.",
                  FieldIndex);
               KeySetting := True;
               Refresh(FormWindow);
               return GameOptions_View;
            elsif FieldIndex = 33 then
               Result := Driver(OptionsForm, F_Previous_Page);
               FieldIndex := 2;
               Set_Background
                 (Current(OptionsForm),
                  (Reverse_Video => True, others => False));
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
               for I in GameSettings.Keys'Range loop
                  FieldIndex := 12 + (I * 2);
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
               Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
               Result := Form_Ok;
               KeySetting := False;
            end if;
         when KEY_RIGHT => -- Select next value
            if not KeySetting then
               Result := Driver(OptionsForm, F_Next_Choice);
            else
               Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
               Result := Form_Ok;
               KeySetting := False;
            end if;
         when KEY_LEFT => -- Select previous value
            if not KeySetting then
               Result := Driver(OptionsForm, F_Previous_Choice);
            else
               Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
               Result := Form_Ok;
               KeySetting := False;
            end if;
         when others =>
            if KeySetting then
               Set_Buffer(Fields(OptionsForm, FieldIndex), 0, GetKeyName(Key));
               Result := Form_Ok;
               KeySetting := False;
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
            when 14 .. 32 =>
               SetDescription("Press Enter to start setting key.", FieldIndex);
            when 33 =>
               SetDescription("Back to general game settings.", 33);
            when others =>
               null;
         end case;
         Refresh(FormWindow);
      end if;
      return GameOptions_View;
   end GameOptionsKeys;

end GameOptions;
