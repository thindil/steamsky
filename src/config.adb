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

with Ada.Characters.Handling;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Config is

   --## rule off TYPE_INITIAL_VALUES
   type New_Nim_Game_Record is record
      Player_Name: chars_ptr;
      Player_Gender: Character;
      Ship_Name: chars_ptr;
      Player_Faction: chars_ptr;
      Player_Career: chars_ptr;
      Starting_Base: chars_ptr;
      Enemy_Damage_Bonus: Float;
      Player_Damage_Bonus: Float;
      Enemy_Melee_Damage_Bonus: Float;
      Player_Melee_Damage_Bonus: Float;
      Experience_Bonus: Float;
      Reputation_Bonus: Float;
      Upgrade_Cost_Bonus: Float;
      Prices_Bonus: Float;
      Difficulty_Level: chars_ptr;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   procedure Load_Config is
      use Tiny_String;

      Temp_New_Game: New_Nim_Game_Record;
      procedure Load_Ada_Config
        (Ada_New_Game_Settings: out New_Nim_Game_Record) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaConfig";
   begin
      Load_Ada_Config(Ada_New_Game_Settings => Temp_New_Game);
      New_Game_Settings :=
        (Player_Name =>
           To_Unbounded_String
             (Source => Value(Item => Temp_New_Game.Player_Name)),
         Player_Gender => Temp_New_Game.Player_Gender,
         Ship_Name =>
           To_Unbounded_String
             (Source => Value(Item => Temp_New_Game.Ship_Name)),
         Player_Faction =>
           To_Bounded_String
             (Source => Value(Item => Temp_New_Game.Player_Faction)),
         Player_Career =>
           To_Unbounded_String
             (Source => Value(Item => Temp_New_Game.Player_Career)),
         Starting_Base =>
           To_Bounded_String
             (Source => Value(Item => Temp_New_Game.Starting_Base)),
         Enemy_Damage_Bonus => Temp_New_Game.Enemy_Damage_Bonus,
         Player_Damage_Bonus => Temp_New_Game.Player_Damage_Bonus,
         Enemy_Melee_Damage_Bonus => Temp_New_Game.Enemy_Melee_Damage_Bonus,
         Player_Melee_Damage_Bonus => Temp_New_Game.Player_Melee_Damage_Bonus,
         Experience_Bonus => Temp_New_Game.Experience_Bonus,
         Reputation_Bonus => Temp_New_Game.Reputation_Bonus,
         Upgrade_Cost_Bonus => Temp_New_Game.Upgrade_Cost_Bonus,
         Prices_Bonus => Temp_New_Game.Prices_Bonus,
         Difficulty_Level =>
           Difficulty_Type'Value
             (Value(Item => Temp_New_Game.Difficulty_Level)));
   end Load_Config;

   procedure Save_Config is
      use Tiny_String;

      Temp_New_Game: New_Nim_Game_Record;
      procedure Save_Ada_Config
        (Ada_New_Game_Settings: New_Nim_Game_Record) with
         Import => True,
         Convention => C,
         External_Name => "saveAdaConfig";
   begin
      Temp_New_Game :=
        (Player_Name =>
           New_String
             (Str => To_String(Source => New_Game_Settings.Player_Name)),
         Player_Gender => New_Game_Settings.Player_Gender,
         Ship_Name =>
           New_String(Str => To_String(Source => New_Game_Settings.Ship_Name)),
         Player_Faction =>
           New_String
             (Str => To_String(Source => New_Game_Settings.Player_Faction)),
         Player_Career =>
           New_String
             (Str => To_String(Source => New_Game_Settings.Player_Career)),
         Starting_Base =>
           New_String
             (Str => To_String(Source => New_Game_Settings.Starting_Base)),
         Enemy_Damage_Bonus => New_Game_Settings.Enemy_Damage_Bonus,
         Player_Damage_Bonus => New_Game_Settings.Player_Damage_Bonus,
         Enemy_Melee_Damage_Bonus =>
           New_Game_Settings.Enemy_Melee_Damage_Bonus,
         Player_Melee_Damage_Bonus =>
           New_Game_Settings.Player_Melee_Damage_Bonus,
         Experience_Bonus => New_Game_Settings.Experience_Bonus,
         Reputation_Bonus => New_Game_Settings.Reputation_Bonus,
         Upgrade_Cost_Bonus => New_Game_Settings.Upgrade_Cost_Bonus,
         Prices_Bonus => New_Game_Settings.Prices_Bonus,
         Difficulty_Level =>
           New_String(Str => New_Game_Settings.Difficulty_Level'Image));
      Save_Ada_Config(Ada_New_Game_Settings => Temp_New_Game);
   end Save_Config;

   procedure Get_New_Game_Settings is
      use Ada.Characters.Handling;
      use Tiny_String;

      procedure Get_Ada_New_Game_Settings
        (Ada_New_Game_Settings: New_Nim_Game_Record) with
         Import => True,
         Convention => C,
         External_Name => "getAdaNewGameSettings";
   begin
      Get_Ada_New_Game_Settings
        (Ada_New_Game_Settings =>
           (Player_Name =>
              New_String
                (Str => To_String(Source => New_Game_Settings.Player_Name)),
            Player_Gender => New_Game_Settings.Player_Gender,
            Ship_Name =>
              New_String
                (Str => To_String(Source => New_Game_Settings.Ship_Name)),
            Player_Faction =>
              New_String
                (Str => To_String(Source => New_Game_Settings.Player_Faction)),
            Player_Career =>
              New_String
                (Str => To_String(Source => New_Game_Settings.Player_Career)),
            Starting_Base =>
              New_String
                (Str => To_String(Source => New_Game_Settings.Starting_Base)),
            Enemy_Damage_Bonus => New_Game_Settings.Enemy_Damage_Bonus,
            Player_Damage_Bonus => New_Game_Settings.Player_Damage_Bonus,
            Enemy_Melee_Damage_Bonus =>
              New_Game_Settings.Enemy_Melee_Damage_Bonus,
            Player_Melee_Damage_Bonus =>
              New_Game_Settings.Player_Melee_Damage_Bonus,
            Experience_Bonus => New_Game_Settings.Experience_Bonus,
            Reputation_Bonus => New_Game_Settings.Reputation_Bonus,
            Upgrade_Cost_Bonus => New_Game_Settings.Upgrade_Cost_Bonus,
            Prices_Bonus => New_Game_Settings.Prices_Bonus,
            Difficulty_Level =>
              New_String
                (Str =>
                   To_Lower
                     (Item =>
                        Difficulty_Type'Image
                          (New_Game_Settings.Difficulty_Level)))));
   end Get_New_Game_Settings;

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

   function Get_Undock_Speed return Ship_Speed is
   begin
      return Ship_Speed'Val(Get_Integer_Setting(Name => "undockSpeed"));
   end Get_Undock_Speed;

   procedure Set_Undock_Speed(Value: Ship_Speed) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "undockSpeed"), V => Ship_Speed'Pos(Value));
   end Set_Undock_Speed;

   function Get_Auto_Move_Stop return Auto_Move_Break is
   begin
      return Auto_Move_Break'Val(Get_Integer_Setting(Name => "autoMoveStop"));
   end Get_Auto_Move_Stop;

   procedure Set_Auto_Move_Stop(Value: Auto_Move_Break) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "autoMoveStop"),
         V => Auto_Move_Break'Pos(Value));
   end Set_Auto_Move_Stop;

   function Get_Auto_Save return Auto_Save_Type is
   begin
      return Auto_Save_Type'Val(Get_Integer_Setting(Name => "autoSave"));
   end Get_Auto_Save;

   procedure Set_Auto_Save(Value: Auto_Save_Type) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "autoSave"), V => Auto_Save_Type'Pos(Value));
   end Set_Auto_Save;

   function Get_Messages_Order return Messages_Order_Type is
   begin
      return
        Messages_Order_Type'Val(Get_Integer_Setting(Name => "messagesOrder"));
   end Get_Messages_Order;

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

   procedure Set_Interface_Theme(Value: Unbounded_String) is
      procedure Set_Ada_Interface_Theme(V: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaInterfaceTheme";
   begin
      Set_Ada_Interface_Theme
        (V => New_String(Str => To_String(Source => Value)));
   end Set_Interface_Theme;

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
      return
        Difficulty_Type'Val(Get_Integer_Setting(Name => "difficulty"));
   end Get_Difficulty;

   procedure Set_Difficulty(Value: Difficulty_Type) is
   begin
      Set_Ada_Integer_Setting
        (N => New_String(Str => "difficulty"),
         V => Difficulty_Type'Pos(Value));
   end Set_Difficulty;

end Config;
