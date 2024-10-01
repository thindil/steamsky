-- Copyright (c) 2021-2024 Bartek thindil Jasicki
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces.C.Strings;
with Interfaces.C;

package body Dialogs is

   procedure Show_Message
     (Text: String; Parent_Frame: String := ".gameframe"; Title: String) is
      use Interfaces.C.Strings;
      use Interfaces.C;

      function Show_Ada_Message
        (Te, P_Frame, Ti: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "showAdaMessage";
   begin
      if Strlen
          (Item =>
             Show_Ada_Message
               (Te => New_String(Str => Text),
                P_Frame => New_String(Str => Parent_Frame),
                Ti => New_String(Str => Title))) =
        0 then
         return;
      end if;
   end Show_Message;

end Dialogs;
