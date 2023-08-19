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
   type Game_Nim_Settings_Record is record
      Auto_Rest: Integer;
      Undock_Speed: chars_ptr;
      Auto_Center: Integer;
      Auto_Return: Integer;
      Auto_Finish: Integer;
      Low_Fuel: Integer;
      Low_Drinks: Integer;
      Low_Food: Integer;
      Auto_Move_Stop: chars_ptr;
      Window_Width: Integer;
      Window_Height: Integer;
      Messages_Limit: Integer;
      Saved_Messages: Integer;
      Help_Font_Size: Integer;
      Map_Font_Size: Integer;
      Interface_Font_Size: Integer;
      Interface_Theme: chars_ptr;
      Messages_Order: chars_ptr;
      Auto_Ask_For_Bases: Integer;
      Auto_Ask_For_Events: Integer;
      Show_Tooltips: Integer;
      Show_Last_Messages: Integer;
      Messages_Position: Integer;
      Full_Screen: Integer;
      Auto_Close_Messages_Time: Integer;
      Auto_Save: chars_ptr;
      Topics_Position: Integer;
      Show_Numbers: Integer;
      Right_Button: Integer;
      Lists_Limit: Integer;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   procedure Load_Config is
      use Tiny_String;

      Temp_New_Game: New_Nim_Game_Record;
      Temp_Settings: Game_Nim_Settings_Record;
      procedure Load_Ada_Config
        (Ada_New_Game_Settings: out New_Nim_Game_Record;
         Ada_Game_Settings: out Game_Nim_Settings_Record) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaConfig";
      function Get_Bool(Value: Integer) return Boolean is
      begin
         if Value = 1 then
            return True;
         end if;
         return False;
      end Get_Bool;
   begin
      Load_Ada_Config
        (Ada_New_Game_Settings => Temp_New_Game,
         Ada_Game_Settings => Temp_Settings);
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
      Game_Settings :=
        (Auto_Rest => Get_Bool(Value => Temp_Settings.Auto_Rest),
         Undock_Speed =>
           Ship_Speed'Value(Value(Item => Temp_Settings.Undock_Speed)),
         Auto_Center => Get_Bool(Value => Temp_Settings.Auto_Center),
         Auto_Return => Get_Bool(Value => Temp_Settings.Auto_Return),
         Auto_Finish => Get_Bool(Value => Temp_Settings.Auto_Finish),
         Low_Fuel => Temp_Settings.Low_Fuel,
         Low_Drinks => Temp_Settings.Low_Drinks,
         Low_Food => Temp_Settings.Low_Food,
         Auto_Move_Stop =>
           Auto_Move_Break'Value(Value(Item => Temp_Settings.Auto_Move_Stop)),
         Window_Width => Temp_Settings.Window_Width,
         Window_Height => Temp_Settings.Window_Height,
         Messages_Limit => Temp_Settings.Messages_Limit,
         Saved_Messages => Temp_Settings.Saved_Messages,
         Help_Font_Size => Temp_Settings.Help_Font_Size,
         Map_Font_Size => Temp_Settings.Map_Font_Size,
         Interface_Font_Size => Temp_Settings.Interface_Font_Size,
         Interface_Theme =>
           To_Unbounded_String
             (Source => Value(Item => Temp_Settings.Interface_Theme)),
         Messages_Order =>
           Messages_Order_Type'Value
             (Value(Item => Temp_Settings.Messages_Order)),
         Auto_Ask_For_Bases =>
           Get_Bool(Value => Temp_Settings.Auto_Ask_For_Bases),
         Auto_Ask_For_Events =>
           Get_Bool(Value => Temp_Settings.Auto_Ask_For_Events),
         Show_Tooltips => Get_Bool(Value => Temp_Settings.Show_Tooltips),
         Show_Last_Messages =>
           Get_Bool(Value => Temp_Settings.Show_Last_Messages),
         Messages_Position => Temp_Settings.Messages_Position,
         Full_Screen => Get_Bool(Value => Temp_Settings.Full_Screen),
         Auto_Close_Messages_Time => Temp_Settings.Auto_Close_Messages_Time,
         Auto_Save =>
           Auto_Save_Type'Value(Value(Item => Temp_Settings.Auto_Save)),
         Topics_Position => Temp_Settings.Topics_Position,
         Show_Numbers => Get_Bool(Value => Temp_Settings.Show_Numbers),
         Right_Button => Get_Bool(Value => Temp_Settings.Right_Button),
         Lists_Limit => Temp_Settings.Lists_Limit);
   end Load_Config;

   procedure Save_Config is
      use Tiny_String;

      Temp_New_Game: New_Nim_Game_Record;
      Temp_Settings: Game_Nim_Settings_Record;
      procedure Save_Ada_Config
        (Ada_New_Game_Settings: New_Nim_Game_Record;
         Ada_Game_Settings: Game_Nim_Settings_Record) with
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
      Temp_Settings :=
        (Auto_Rest => (if Game_Settings.Auto_Rest then 1 else 0),
         Undock_Speed =>
           New_String(Str => Ship_Speed'Image(Game_Settings.Undock_Speed)),
         Auto_Center => (if Game_Settings.Auto_Center then 1 else 0),
         Auto_Return => (if Game_Settings.Auto_Return then 1 else 0),
         Auto_Finish => (if Game_Settings.Auto_Finish then 1 else 0),
         Low_Fuel => Game_Settings.Low_Fuel,
         Low_Drinks => Game_Settings.Low_Drinks,
         Low_Food => Game_Settings.Low_Food,
         Auto_Move_Stop =>
           New_String
             (Str => Auto_Move_Break'Image(Game_Settings.Auto_Move_Stop)),
         Window_Width => Game_Settings.Window_Width,
         Window_Height => Game_Settings.Window_Height,
         Messages_Limit => Game_Settings.Messages_Limit,
         Saved_Messages => Game_Settings.Saved_Messages,
         Help_Font_Size => Game_Settings.Help_Font_Size,
         Map_Font_Size => Game_Settings.Map_Font_Size,
         Interface_Font_Size => Game_Settings.Interface_Font_Size,
         Interface_Theme =>
           New_String
             (Str => To_String(Source => Game_Settings.Interface_Theme)),
         Messages_Order =>
           New_String
             (Str => Messages_Order_Type'Image(Game_Settings.Messages_Order)),
         Auto_Ask_For_Bases =>
           (if Game_Settings.Auto_Ask_For_Bases then 1 else 0),
         Auto_Ask_For_Events =>
           (if Game_Settings.Auto_Ask_For_Events then 1 else 0),
         Show_Tooltips => (if Game_Settings.Show_Tooltips then 1 else 0),
         Show_Last_Messages =>
           (if Game_Settings.Show_Last_Messages then 1 else 0),
         Full_Screen => (if Game_Settings.Full_Screen then 1 else 0),
         Auto_Close_Messages_Time => Game_Settings.Auto_Close_Messages_Time,
         Auto_Save =>
           New_String(Str => Auto_Save_Type'Image(Game_Settings.Auto_Save)),
         Topics_Position => Game_Settings.Topics_Position,
         Show_Numbers => (if Game_Settings.Show_Numbers then 1 else 0),
         Right_Button => (if Game_Settings.Right_Button then 1 else 0),
         Lists_Limit => Game_Settings.Lists_Limit,
         Messages_Position => Game_Settings.Messages_Position);
      Save_Ada_Config
        (Ada_New_Game_Settings => Temp_New_Game,
         Ada_Game_Settings => Temp_Settings);
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

   function Get_Boolean_Setting
     (Name: String; From_Game_Setting: Boolean := True) return Boolean is
      function Get_Ada_Boolean_Setting
        (N: chars_ptr; From_Game: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaBooleanSetting";
   begin
      if Get_Ada_Boolean_Setting
          (N => New_String(Str => Name),
           From_Game => (if From_Game_Setting then 1 else 0)) =
        1 then
         return True;
      end if;
      return False;
   end Get_Boolean_Setting;

   procedure Set_Boolean_Setting
     (Name: String; Value: Boolean; In_Game_Setting: Boolean := True) is
      procedure Set_Ada_Boolean_Setting(N: chars_ptr; V, In_Game: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBooleanSetting";
   begin
      Set_Ada_Boolean_Setting
        (N => New_String(Str => Name), V => (if Value then 1 else 0),
         In_Game => (if In_Game_Setting then 1 else 0));
   end Set_Boolean_Setting;

end Config;
