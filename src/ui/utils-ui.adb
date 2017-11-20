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

package body Utils.UI is

   procedure WindowFrame(Win: Window; Color: Color_Pair; Caption: String) is
   begin
      Set_Color(Win, Color);
      Box(Win);
      Move_Cursor(Win => Win, Line => 0, Column => 2);
      if Caption'Length > 0 then
         Add(Win => Win, Str => "[");
         Set_Color(Win, Color_Pair'First);
         Add(Win => Win, Str => Caption);
         Set_Color(Win, Color);
         Add(Win => Win, Str => "]");
      end if;
      Set_Color(Win, Color_Pair'First);
   end WindowFrame;

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

end Utils.UI;
