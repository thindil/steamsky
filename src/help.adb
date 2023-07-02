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
with Game;

package body Help is

   procedure Load_Help(File_Name: String) is
      use Interfaces.C;
      use Game;

      Tmp_Help: Help_Data := Empty_Help;
      Result, Help_Index, Help_Title, Help_Text: chars_ptr;
      Index: Natural := 0;
      function Load_Ada_Help(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaHelp";
      procedure Get_Ada_Help
        (I: Natural; H_Index, H_Title, H_Text: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHelp";
   begin
      Result := Load_Ada_Help(Name => New_String(Str => File_Name));
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Load_Help_Loop :
      loop
         Get_Ada_Help
           (I => Index, H_Index => Help_Index, H_Title => Help_Title,
            H_Text => Help_Text);
         exit Load_Help_Loop when Strlen(Item => Help_Index) = 0;
         Tmp_Help :=
           (Index => To_Unbounded_String(Source => Value(Item => Help_Index)),
            Text => To_Unbounded_String(Source => Value(Item => Help_Text)));
         Help_Container.Include
           (Container => Help_List,
            Key => To_Unbounded_String(Source => Value(Item => Help_Title)),
            New_Item => Tmp_Help);
         Index := Index + 1;
      end loop Load_Help_Loop;
   end Load_Help;

   function Get_Help(Title: Unbounded_String) return Help_Data is
      Text, Index: chars_ptr;
      procedure Get_Ada_Help(T: chars_ptr; I, Te: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHelp2";
      Help_Entry: Help_Data;
   begin
      Get_Ada_Help
        (T => New_String(Str => To_String(Source => Title)), I => Index,
         Te => Text);
      Help_Entry :=
        (Text => To_Unbounded_String(Source => Value(Item => Text)),
         Index => To_Unbounded_String(Source => Value(Item => Index)));
      return Help_Entry;
   end Get_Help;

end Help;
