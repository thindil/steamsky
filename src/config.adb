--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Game; use Game;

package body Config is

   procedure Load_Config is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      function LoadBoolean return Boolean is
      begin
         if Value = To_Unbounded_String("Yes") then
            return True;
         end if;
         return False;
      end LoadBoolean;
   begin
      New_Game_Settings :=
        (Player_Name => To_Unbounded_String("Laeran"), Player_Gender => 'M',
         Ship_Name => To_Unbounded_String("Anaria"),
         Player_Faction => To_Unbounded_String("POLEIS"),
         Player_Career => To_Unbounded_String("general"),
         Starting_Base => To_Unbounded_String("Any"),
         Enemy_Damage_Bonus => 1.0, Player_Damage_Bonus => 1.0,
         Enemy_Melee_Damage_Bonus => 1.0, Player_Melee_Damage_Bonus => 1.0,
         Experience_Bonus => 1.0, Reputation_Bonus => 1.0,
         Upgrade_Cost_Bonus => 1.0, Prices_Bonus => 1.0,
         Difficulty_Level => NORMAL);
      Game_Settings :=
        (Auto_Rest => True, Undock_Speed => FULL_SPEED, Auto_Center => True,
         Auto_Return => True, Auto_Finish => True, Low_Fuel => 100,
         Low_Drinks => 50, Low_Food => 25, Auto_Move_Stop => NEVER,
         Window_Width => 800, Window_Height => 600, Messages_Limit => 500,
         Saved_Messages => 10, Help_Font_Size => 14, Map_Font_Size => 16,
         Interface_Font_Size => 14,
         Interface_Theme => To_Unbounded_String("steamsky"),
         Messages_Order => OLDER_FIRST, Auto_Ask_For_Bases => False,
         Auto_Ask_For_Events => False, Show_Tooltips => True,
         Show_Last_Messages => True, Messages_Position => 213,
         Full_Screen => False, Auto_Close_Messages_Time => 6,
         Auto_Save => NONE, Topics_Position => 200, Show_Numbers => False);
      Open(ConfigFile, In_File, To_String(Save_Directory) & "game.cfg");
      Read_Config_File_Loop :
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Get_Line(ConfigFile));
         if Length(RawData) > 0 then
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            if FieldName = To_Unbounded_String("PlayerName") then
               New_Game_Settings.Player_Name := Value;
            elsif FieldName = To_Unbounded_String("PlayerGender") then
               New_Game_Settings.Player_Gender := Element(Value, 1);
            elsif FieldName = To_Unbounded_String("ShipName") then
               New_Game_Settings.Ship_Name := Value;
            elsif FieldName = To_Unbounded_String("PlayerFaction") then
               New_Game_Settings.Player_Faction := Value;
            elsif FieldName = To_Unbounded_String("PlayerCareer") then
               New_Game_Settings.Player_Career := Value;
            elsif FieldName = To_Unbounded_String("StartingBase") then
               New_Game_Settings.Starting_Base := Value;
            elsif FieldName = To_Unbounded_String("EnemyDamageBonus") then
               New_Game_Settings.Enemy_Damage_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("PlayerDamageBonus") then
               New_Game_Settings.Player_Damage_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("EnemyMeleeDamageBonus") then
               New_Game_Settings.Enemy_Melee_Damage_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName =
              To_Unbounded_String("PlayerMeleeDamageBonus") then
               New_Game_Settings.Player_Melee_Damage_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ExperienceBonus") then
               New_Game_Settings.Experience_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ReputationBonus") then
               New_Game_Settings.Reputation_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("UpgradeCostBonus") then
               New_Game_Settings.Upgrade_Cost_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("PricesBonus") then
               New_Game_Settings.Prices_Bonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("DifficultyLevel") then
               if To_String(Value) in "VERY_EASY" | "EASY" | "NORMAL" |
                     "HARD" | "VERY_HARD" | "CUSTOM" then
                  New_Game_Settings.Difficulty_Level :=
                    Difficulty_Type'Value(To_String(Value));
               else
                  New_Game_Settings.Difficulty_Level := NORMAL;
               end if;
            elsif FieldName = To_Unbounded_String("AutoRest") then
               Game_Settings.Auto_Rest := LoadBoolean;
            elsif FieldName = To_Unbounded_String("UndockSpeed") then
               Game_Settings.Undock_Speed := ShipSpeed'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoCenter") then
               Game_Settings.Auto_Center := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoReturn") then
               Game_Settings.Auto_Return := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoFinish") then
               Game_Settings.Auto_Finish := LoadBoolean;
            elsif FieldName = To_Unbounded_String("LowFuel") then
               Game_Settings.Low_Fuel := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("LowDrinks") then
               Game_Settings.Low_Drinks := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("LowFood") then
               Game_Settings.Low_Food := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoMoveStop") then
               Game_Settings.Auto_Move_Stop :=
                 Auto_Move_Break'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowWidth") then
               Game_Settings.Window_Width := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowHeight") then
               Game_Settings.Window_Height := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("MessagesLimit") then
               Game_Settings.Messages_Limit :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("SavedMessages") then
               Game_Settings.Saved_Messages :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("HelpFontSize") then
               Game_Settings.Help_Font_Size :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("MapFontSize") then
               Game_Settings.Map_Font_Size := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("InterfaceFontSize") then
               Game_Settings.Interface_Font_Size :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("InterfaceTheme") then
               Game_Settings.Interface_Theme := Value;
            elsif FieldName = To_Unbounded_String("MessagesOrder") then
               Game_Settings.Messages_Order :=
                 Messages_Order_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoAskForBases") then
               Game_Settings.Auto_Ask_For_Bases := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoAskForEvents") then
               Game_Settings.Auto_Ask_For_Events := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowTooltips") then
               Game_Settings.Show_Tooltips := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowLastMessages") then
               Game_Settings.Show_Last_Messages := LoadBoolean;
            elsif FieldName = To_Unbounded_String("MessagesPosition") then
               Game_Settings.Messages_Position :=
                 Natural'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("FullScreen") then
               Game_Settings.Full_Screen := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoCloseMessagesTime") then
               Game_Settings.Auto_Close_Messages_Time :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoSave") then
               Game_Settings.Auto_Save :=
                 Auto_Save_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("TopicsPosition") then
               Game_Settings.Topics_Position :=
                 Natural'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ShowNumbers") then
               Game_Settings.Show_Numbers := LoadBoolean;
            end if;
         end if;
      end loop Read_Config_File_Loop;
      Close(ConfigFile);
   exception
      when Ada.Directories.Name_Error =>
         null;
   end Load_Config;

   procedure Save_Config is
      ConfigFile: File_Type;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         if Value then
            Put_Line(ConfigFile, Name & " = Yes");
         else
            Put_Line(ConfigFile, Name & " = No");
         end if;
      end SaveBoolean;
   begin
      Create(ConfigFile, Append_File, To_String(Save_Directory) & "game.cfg");
      Put_Line
        (ConfigFile,
         "PlayerName = " & To_String(New_Game_Settings.Player_Name));
      Put_Line
        (ConfigFile, "PlayerGender = " & New_Game_Settings.Player_Gender);
      Put_Line
        (ConfigFile, "ShipName = " & To_String(New_Game_Settings.Ship_Name));
      Put_Line
        (ConfigFile,
         "PlayerFaction = " & To_String(New_Game_Settings.Player_Faction));
      Put_Line
        (ConfigFile,
         "PlayerCareer = " & To_String(New_Game_Settings.Player_Career));
      Put_Line
        (ConfigFile,
         "StartingBase = " & To_String(New_Game_Settings.Starting_Base));
      Put_Line
        (ConfigFile,
         "EnemyDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Enemy_Damage_Bonus));
      Put_Line
        (ConfigFile,
         "PlayerDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Player_Damage_Bonus));
      Put_Line
        (ConfigFile,
         "EnemyMeleeDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Enemy_Melee_Damage_Bonus));
      Put_Line
        (ConfigFile,
         "PlayerMeleeDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Player_Melee_Damage_Bonus));
      Put_Line
        (ConfigFile,
         "ExperienceBonus =" &
         Bonus_Type'Image(New_Game_Settings.Experience_Bonus));
      Put_Line
        (ConfigFile,
         "ReputationBonus =" &
         Bonus_Type'Image(New_Game_Settings.Reputation_Bonus));
      Put_Line
        (ConfigFile,
         "UpgradeCostBonus =" &
         Bonus_Type'Image(New_Game_Settings.Upgrade_Cost_Bonus));
      Put_Line
        (ConfigFile,
         "PricesBonus =" & Bonus_Type'Image(New_Game_Settings.Prices_Bonus));
      Put_Line
        (ConfigFile,
         "DifficultyLevel = " &
         Difficulty_Type'Image(New_Game_Settings.Difficulty_Level));
      SaveBoolean(Game_Settings.Auto_Rest, "AutoRest");
      Put_Line
        (ConfigFile,
         "UndockSpeed = " & ShipSpeed'Image(Game_Settings.Undock_Speed));
      SaveBoolean(Game_Settings.Auto_Center, "AutoCenter");
      SaveBoolean(Game_Settings.Auto_Return, "AutoReturn");
      SaveBoolean(Game_Settings.Auto_Finish, "AutoFinish");
      Put_Line
        (ConfigFile, "LowFuel =" & Positive'Image(Game_Settings.Low_Fuel));
      Put_Line
        (ConfigFile, "LowDrinks =" & Positive'Image(Game_Settings.Low_Drinks));
      Put_Line
        (ConfigFile, "LowFood =" & Positive'Image(Game_Settings.Low_Food));
      Put_Line
        (ConfigFile,
         "AutoMoveStop = " &
         Auto_Move_Break'Image(Game_Settings.Auto_Move_Stop));
      Put_Line
        (ConfigFile,
         "WindowWidth =" & Positive'Image(Game_Settings.Window_Width));
      Put_Line
        (ConfigFile,
         "WindowHeight =" & Positive'Image(Game_Settings.Window_Height));
      Put_Line
        (ConfigFile,
         "MessagesLimit =" & Positive'Image(Game_Settings.Messages_Limit));
      Put_Line
        (ConfigFile,
         "SavedMessages =" & Positive'Image(Game_Settings.Saved_Messages));
      Put_Line
        (ConfigFile,
         "HelpFontSize =" & Positive'Image(Game_Settings.Help_Font_Size));
      Put_Line
        (ConfigFile,
         "MapFontSize =" & Positive'Image(Game_Settings.Map_Font_Size));
      Put_Line
        (ConfigFile,
         "InterfaceFontSize =" &
         Positive'Image(Game_Settings.Interface_Font_Size));
      Put_Line
        (ConfigFile,
         "InterfaceTheme = " & To_String(Game_Settings.Interface_Theme));
      Put_Line
        (ConfigFile,
         "MessagesOrder = " &
         Messages_Order_Type'Image(Game_Settings.Messages_Order));
      SaveBoolean(Game_Settings.Auto_Ask_For_Bases, "AutoAskForBases");
      SaveBoolean(Game_Settings.Auto_Ask_For_Events, "AutoAskForEvents");
      SaveBoolean(Game_Settings.Show_Tooltips, "ShowTooltips");
      SaveBoolean(Game_Settings.Show_Last_Messages, "ShowLastMessages");
      Put_Line
        (ConfigFile,
         "MessagesPosition =" &
         Natural'Image(Game_Settings.Messages_Position));
      SaveBoolean(Game_Settings.Full_Screen, "FullScreen");
      Put_Line
        (ConfigFile,
         "AutoCloseMessagesTime =" &
         Positive'Image(Game_Settings.Auto_Close_Messages_Time));
      Put_Line
        (ConfigFile,
         "AutoSave = " & Auto_Save_Type'Image(Game_Settings.Auto_Save));
      Put_Line
        (ConfigFile,
         "TopicsPosition =" & Natural'Image(Game_Settings.Topics_Position));
      SaveBoolean(Game_Settings.Show_Numbers, "ShowNumbers");
      Close(ConfigFile);
   end Save_Config;

end Config;
