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

   procedure Save_Config is
      procedure Save_Ada_Config with
         Import => True,
         Convention => C,
         External_Name => "saveAdaConfig";
   begin
      Save_Ada_Config;
   end Save_Config;

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

   procedure Set_Boolean_Setting(Name: String; Value: Boolean) is
      procedure Set_Ada_Boolean_Setting(N: chars_ptr; V: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBooleanSetting";
   begin
      Set_Ada_Boolean_Setting
        (N => New_String(Str => Name), V => (if Value then 1 else 0));
   end Set_Boolean_Setting;

   function Get_Integer_Setting(Name: String) return Integer is
      function Get_Ada_Integer_Setting(N: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaIntegerSetting";
   begin
      return Get_Ada_Integer_Setting(N => New_String(Str => Name));
   end Get_Integer_Setting;

   procedure Set_Ada_Integer_Setting(N: chars_ptr; V: Integer) with
      Import => True,
      Convention => C,
      External_Name => "setAdaIntegerSetting";

   procedure Set_Integer_Setting(Name: String; Value: Integer) is
   begin
      Set_Ada_Integer_Setting(N => New_String(Str => Name), V => Value);
   end Set_Integer_Setting;

   procedure Set_Undock_Speed(Value: Ship_Speed) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "undockSpeed"), V => Ship_Speed'Pos(Value));
   end Set_Undock_Speed;

   procedure Set_Auto_Move_Stop(Value: Auto_Move_Break) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "autoMoveStop"),
         V => Auto_Move_Break'Pos(Value));
   end Set_Auto_Move_Stop;

   procedure Set_Auto_Save(Value: Auto_Save_Type) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "autoSave"), V => Auto_Save_Type'Pos(Value));
   end Set_Auto_Save;

   procedure Set_Messages_Order(Value: Messages_Order_Type) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "messagesOrder"),
         V => Messages_Order_Type'Pos(Value));
   end Set_Messages_Order;

   function Get_Interface_Theme return Unbounded_String is
      function Get_Ada_Interface_Theme return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaInterfaceTheme";
   begin
      return
        To_Unbounded_String(Source => Value(Item => Get_Ada_Interface_Theme));
   end Get_Interface_Theme;

   function Get_String_Setting(Name: String) return String is
      function Get_Ada_String_Setting(N: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaStringSetting";
   begin
      return
        Value(Item => Get_Ada_String_Setting(N => New_String(Str => Name)));
   end Get_String_Setting;

   procedure Set_String_Setting(Name, Value: String) is
      procedure Set_Ada_String_Setting(N, V: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaStringSetting";
   begin
      Set_Ada_String_Setting
        (N => New_String(Str => Name), V => New_String(Str => Value));
   end Set_String_Setting;

   function Get_Float_Setting(Name: String) return Bonus_Type is
      function Get_Ada_Float_Setting(N: chars_ptr) return Bonus_Type with
         Import => True,
         Convention => C,
         External_Name => "getAdaFloatSetting";
   begin
      return Get_Ada_Float_Setting(N => New_String(Str => Name));
   end Get_Float_Setting;

   procedure Set_Float_Setting(Name: String; Value: Bonus_Type) is
      procedure Set_Ada_Float_Setting(N: chars_ptr; V: Bonus_Type) with
         Import => True,
         Convention => C,
         External_Name => "setAdaFloatSetting";
   begin
      Set_Ada_Float_Setting(N => New_String(Str => Name), V => Value);
   end Set_Float_Setting;

   function Get_Difficulty return Difficulty_Type is
   begin
      return Difficulty_Type'Val(Get_Integer_Setting(Name => "difficulty"));
   end Get_Difficulty;

   procedure Set_Difficulty(Value: Difficulty_Type) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "difficulty"),
         V => Difficulty_Type'Pos(Value));
   end Set_Difficulty;

end Config;
