--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Config is

   procedure Load_Config is
      procedure Load_Ada_Config with
         Import => True,
         Convention => C,
         External_Name => "loadAdaConfig";
   begin
      Load_Ada_Config;
   end Load_Config;

   function Get_Boolean_Setting(Name: String) return Boolean is
      function Get_Ada_Boolean_Setting(N: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaBooleanSetting";
   begin
      if Get_Ada_Boolean_Setting(N => New_String(Str => Name)) = 1 then
         return True;
      end if;
      return False;
   end Get_Boolean_Setting;

   function Get_Integer_Setting(Name: String) return Integer is
      function Get_Ada_Integer_Setting(N: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaIntegerSetting";
   begin
      return Get_Ada_Integer_Setting(N => New_String(Str => Name));
   end Get_Integer_Setting;

   procedure Set_Integer_Setting(Name: String; Value: Integer) is
      procedure Set_Ada_Integer_Setting(N: chars_ptr; V: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaIntegerSetting";
   begin
      Set_Ada_Integer_Setting(N => New_String(Str => Name), V => Value);
   end Set_Integer_Setting;

end Config;
