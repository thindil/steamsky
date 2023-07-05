--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Help is

   --## rule off IMPROPER_INITIALIZATION
   function Get_Help
     (Title: out Unbounded_String; Help_Index: Integer := -1)
      return Help_Data is
      Text, Index, Help_Title: chars_ptr;
      Help_Entry: Help_Data;
      procedure Get_Ada_Help
        (I: Natural; H_Index, H_Title, H_Text: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHelp";
      procedure Get_Ada_Help_2(T: chars_ptr; I, Te: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHelp2";
   begin
      if Help_Index = -1 then
         Get_Ada_Help_2
           (T => New_String(Str => To_String(Source => Title)), I => Index,
            Te => Text);
      else
         Get_Ada_Help
           (I => Help_Index, H_Index => Index, H_Title => Help_Title,
            H_Text => Text);
         Title := To_Unbounded_String(Source => Value(Item => Help_Title));
      end if;
      Help_Entry :=
        (Text => To_Unbounded_String(Source => Value(Item => Text)),
         Index => To_Unbounded_String(Source => Value(Item => Index)));
      return Help_Entry;
   end Get_Help;
   --## rule on IMPROPER_INITIALIZATION

end Help;
