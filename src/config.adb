--    Copyright 2016-2022 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

package body Config is

   procedure Load_Config is
      use Interfaces.C.Strings;
      use Tiny_String;

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
      Temp_New_Game: New_Nim_Game_Record;
      procedure Load_Ada_Config
        (Ada_New_Game_Settings: out New_Nim_Game_Record) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaConfig";
   begin
      Game_Settings := Default_Game_Settings;
      Load_Ada_Config(Ada_New_Game_Settings => Temp_New_Game);
      New_Game_Settings :=
        (Player_Name =>
           To_Unbounded_String(Value(Item => Temp_New_Game.Player_Name)),
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

      Config_File: File_Type;
      procedure Save_Boolean(Value: Boolean; Name: String) is
      begin
         if Value then
            Put_Line(File => Config_File, Item => Name & " = Yes");
         else
            Put_Line(File => Config_File, Item => Name & " = No");
         end if;
      end Save_Boolean;
   begin
      Create
        (File => Config_File, Mode => Append_File,
         Name => To_String(Source => Save_Directory) & "game.cfg");
      Put_Line
        (File => Config_File,
         Item =>
           "PlayerName = " &
           To_String(Source => New_Game_Settings.Player_Name));
      Put_Line
        (File => Config_File,
         Item => "PlayerGender = " & New_Game_Settings.Player_Gender);
      Put_Line
        (File => Config_File,
         Item =>
           "ShipName = " & To_String(Source => New_Game_Settings.Ship_Name));
      Put_Line
        (File => Config_File,
         Item =>
           "PlayerFaction = " &
           To_String(Source => New_Game_Settings.Player_Faction));
      Put_Line
        (File => Config_File,
         Item =>
           "PlayerCareer = " &
           To_String(Source => New_Game_Settings.Player_Career));
      Put_Line
        (File => Config_File,
         Item =>
           "StartingBase = " &
           To_String(Source => New_Game_Settings.Starting_Base));
      Put_Line
        (File => Config_File,
         Item =>
           "EnemyDamageBonus =" &
           Bonus_Type'Image(New_Game_Settings.Enemy_Damage_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "PlayerDamageBonus =" &
           Bonus_Type'Image(New_Game_Settings.Player_Damage_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "EnemyMeleeDamageBonus =" &
           Bonus_Type'Image(New_Game_Settings.Enemy_Melee_Damage_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "PlayerMeleeDamageBonus =" &
           Bonus_Type'Image(New_Game_Settings.Player_Melee_Damage_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "ExperienceBonus =" &
           Bonus_Type'Image(New_Game_Settings.Experience_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "ReputationBonus =" &
           Bonus_Type'Image(New_Game_Settings.Reputation_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "UpgradeCostBonus =" &
           Bonus_Type'Image(New_Game_Settings.Upgrade_Cost_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "PricesBonus =" & Bonus_Type'Image(New_Game_Settings.Prices_Bonus));
      Put_Line
        (File => Config_File,
         Item =>
           "DifficultyLevel = " &
           Difficulty_Type'Image(New_Game_Settings.Difficulty_Level));
      Save_Boolean(Value => Game_Settings.Auto_Rest, Name => "AutoRest");
      Put_Line
        (File => Config_File,
         Item =>
           "UndockSpeed = " & Ship_Speed'Image(Game_Settings.Undock_Speed));
      Save_Boolean(Value => Game_Settings.Auto_Center, Name => "AutoCenter");
      Save_Boolean(Value => Game_Settings.Auto_Return, Name => "AutoReturn");
      Save_Boolean(Value => Game_Settings.Auto_Finish, Name => "AutoFinish");
      Put_Line
        (File => Config_File,
         Item => "LowFuel =" & Positive'Image(Game_Settings.Low_Fuel));
      Put_Line
        (File => Config_File,
         Item => "LowDrinks =" & Positive'Image(Game_Settings.Low_Drinks));
      Put_Line
        (File => Config_File,
         Item => "LowFood =" & Positive'Image(Game_Settings.Low_Food));
      Put_Line
        (File => Config_File,
         Item =>
           "AutoMoveStop = " &
           Auto_Move_Break'Image(Game_Settings.Auto_Move_Stop));
      Put_Line
        (File => Config_File,
         Item => "WindowWidth =" & Positive'Image(Game_Settings.Window_Width));
      Put_Line
        (File => Config_File,
         Item =>
           "WindowHeight =" & Positive'Image(Game_Settings.Window_Height));
      Put_Line
        (File => Config_File,
         Item =>
           "MessagesLimit =" & Positive'Image(Game_Settings.Messages_Limit));
      Put_Line
        (File => Config_File,
         Item =>
           "SavedMessages =" & Positive'Image(Game_Settings.Saved_Messages));
      Put_Line
        (File => Config_File,
         Item =>
           "HelpFontSize =" & Positive'Image(Game_Settings.Help_Font_Size));
      Put_Line
        (File => Config_File,
         Item =>
           "MapFontSize =" & Positive'Image(Game_Settings.Map_Font_Size));
      Put_Line
        (File => Config_File,
         Item =>
           "InterfaceFontSize =" &
           Positive'Image(Game_Settings.Interface_Font_Size));
      Put_Line
        (File => Config_File,
         Item =>
           "InterfaceTheme = " &
           To_String(Source => Game_Settings.Interface_Theme));
      Put_Line
        (File => Config_File,
         Item =>
           "MessagesOrder = " &
           Messages_Order_Type'Image(Game_Settings.Messages_Order));
      Save_Boolean
        (Value => Game_Settings.Auto_Ask_For_Bases, Name => "AutoAskForBases");
      Save_Boolean
        (Value => Game_Settings.Auto_Ask_For_Events,
         Name => "AutoAskForEvents");
      Save_Boolean
        (Value => Game_Settings.Show_Tooltips, Name => "ShowTooltips");
      Save_Boolean
        (Value => Game_Settings.Show_Last_Messages,
         Name => "ShowLastMessages");
      Put_Line
        (File => Config_File,
         Item =>
           "MessagesPosition =" &
           Natural'Image(Game_Settings.Messages_Position));
      Save_Boolean(Value => Game_Settings.Full_Screen, Name => "FullScreen");
      Put_Line
        (File => Config_File,
         Item =>
           "AutoCloseMessagesTime =" &
           Positive'Image(Game_Settings.Auto_Close_Messages_Time));
      Put_Line
        (File => Config_File,
         Item =>
           "AutoSave = " & Auto_Save_Type'Image(Game_Settings.Auto_Save));
      Put_Line
        (File => Config_File,
         Item =>
           "TopicsPosition =" & Natural'Image(Game_Settings.Topics_Position));
      Save_Boolean(Value => Game_Settings.Show_Numbers, Name => "ShowNumbers");
      Save_Boolean(Value => Game_Settings.Right_Button, Name => "RightButton");
      Put_Line
        (File => Config_File,
         Item => "ListsLimit =" & Positive'Image(Game_Settings.Lists_Limit));
      Close(File => Config_File);
   end Save_Config;

end Config;
