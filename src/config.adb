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
      Config_File: File_Type;
      Raw_Data, Field_Name, Value: Unbounded_String := Null_Unbounded_String;
      Equal_Index: Natural := 0;
      function Load_Boolean return Boolean is
      begin
         if Value = To_Unbounded_String(Source => "Yes") then
            return True;
         end if;
         return False;
      end Load_Boolean;
   begin
      New_Game_Settings := Default_New_Game_Settings;
      Game_Settings := Default_Game_Settings;
      Open
        (File => Config_File, Mode => In_File,
         Name => To_String(Source => Save_Directory) & "game.cfg");
      Read_Config_File_Loop :
      while not End_Of_File(File => Config_File) loop
         Raw_Data :=
           To_Unbounded_String(Source => Get_Line(File => Config_File));
         if Length(Source => Raw_Data) = 0 then
            goto End_Of_Loop;
         end if;
         Equal_Index := Index(Raw_Data, "=");
         Field_Name := Head(Raw_Data, Equal_Index - 2);
         Value := Tail(Raw_Data, (Length(Raw_Data) - Equal_Index - 1));
         if Field_Name = To_Unbounded_String(Source => "PlayerName") then
            New_Game_Settings.Player_Name := Value;
         elsif Field_Name = To_Unbounded_String(Source => "PlayerGender") then
            New_Game_Settings.Player_Gender := Element(Value, 1);
         elsif Field_Name = To_Unbounded_String(Source => "ShipName") then
            New_Game_Settings.Ship_Name := Value;
         elsif Field_Name = To_Unbounded_String(Source => "PlayerFaction") then
            New_Game_Settings.Player_Faction := Value;
         elsif Field_Name = To_Unbounded_String(Source => "PlayerCareer") then
            New_Game_Settings.Player_Career := Value;
         elsif Field_Name = To_Unbounded_String(Source => "StartingBase") then
            New_Game_Settings.Starting_Base := Value;
         elsif Field_Name =
           To_Unbounded_String(Source => "EnemyDamageBonus") then
            New_Game_Settings.Enemy_Damage_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "PlayerDamageBonus") then
            New_Game_Settings.Player_Damage_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "EnemyMeleeDamageBonus") then
            New_Game_Settings.Enemy_Melee_Damage_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "PlayerMeleeDamageBonus") then
            New_Game_Settings.Player_Melee_Damage_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "ExperienceBonus") then
            New_Game_Settings.Experience_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "ReputationBonus") then
            New_Game_Settings.Reputation_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "UpgradeCostBonus") then
            New_Game_Settings.Upgrade_Cost_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "PricesBonus") then
            New_Game_Settings.Prices_Bonus :=
              Bonus_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "DifficultyLevel") then
            if To_String(Source => Value) in "VERY_EASY" | "EASY" | "NORMAL" |
                  "HARD" | "VERY_HARD" | "CUSTOM" then
               New_Game_Settings.Difficulty_Level :=
                 Difficulty_Type'Value(To_String(Source => Value));
            else
               New_Game_Settings.Difficulty_Level := Default_Difficulty_Type;
            end if;
         elsif Field_Name = To_Unbounded_String(Source => "AutoRest") then
            Game_Settings.Auto_Rest := Load_Boolean;
         elsif Field_Name = To_Unbounded_String(Source => "UndockSpeed") then
            Game_Settings.Undock_Speed :=
              ShipSpeed'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "AutoCenter") then
            Game_Settings.Auto_Center := Load_Boolean;
         elsif Field_Name = To_Unbounded_String(Source => "AutoReturn") then
            Game_Settings.Auto_Return := Load_Boolean;
         elsif Field_Name = To_Unbounded_String(Source => "AutoFinish") then
            Game_Settings.Auto_Finish := Load_Boolean;
         elsif Field_Name = To_Unbounded_String(Source => "LowFuel") then
            Game_Settings.Low_Fuel :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "LowDrinks") then
            Game_Settings.Low_Drinks :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "LowFood") then
            Game_Settings.Low_Food :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "AutoMoveStop") then
            Game_Settings.Auto_Move_Stop :=
              Auto_Move_Break'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "WindowWidth") then
            Game_Settings.Window_Width :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "WindowHeight") then
            Game_Settings.Window_Height :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "MessagesLimit") then
            Game_Settings.Messages_Limit :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "SavedMessages") then
            Game_Settings.Saved_Messages :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "HelpFontSize") then
            Game_Settings.Help_Font_Size :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "MapFontSize") then
            Game_Settings.Map_Font_Size :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "InterfaceFontSize") then
            Game_Settings.Interface_Font_Size :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "InterfaceTheme") then
            Game_Settings.Interface_Theme := Value;
         elsif Field_Name = To_Unbounded_String(Source => "MessagesOrder") then
            Game_Settings.Messages_Order :=
              Messages_Order_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "AutoAskForBases") then
            Game_Settings.Auto_Ask_For_Bases := Load_Boolean;
         elsif Field_Name =
           To_Unbounded_String(Source => "AutoAskForEvents") then
            Game_Settings.Auto_Ask_For_Events := Load_Boolean;
         elsif Field_Name = To_Unbounded_String(Source => "ShowTooltips") then
            Game_Settings.Show_Tooltips := Load_Boolean;
         elsif Field_Name =
           To_Unbounded_String(Source => "ShowLastMessages") then
            Game_Settings.Show_Last_Messages := Load_Boolean;
         elsif Field_Name =
           To_Unbounded_String(Source => "MessagesPosition") then
            Game_Settings.Messages_Position :=
              Natural'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "FullScreen") then
            Game_Settings.Full_Screen := Load_Boolean;
         elsif Field_Name =
           To_Unbounded_String(Source => "AutoCloseMessagesTime") then
            Game_Settings.Auto_Close_Messages_Time :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "AutoSave") then
            Game_Settings.Auto_Save :=
              Auto_Save_Type'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "TopicsPosition") then
            Game_Settings.Topics_Position :=
              Natural'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "ShowNumbers") then
            Game_Settings.Show_Numbers := Load_Boolean;
         end if;
         <<End_Of_Loop>>
      end loop Read_Config_File_Loop;
      Close(Config_File);
   exception
      when Ada.Directories.Name_Error =>
         null;
   end Load_Config;

   procedure Save_Config is
      Config_File: File_Type;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         if Value then
            Put_Line(Config_File, Name & " = Yes");
         else
            Put_Line(Config_File, Name & " = No");
         end if;
      end SaveBoolean;
   begin
      Create
        (Config_File, Append_File,
         To_String(Source => Save_Directory) & "game.cfg");
      Put_Line
        (Config_File,
         "PlayerName = " & To_String(Source => New_Game_Settings.Player_Name));
      Put_Line
        (Config_File, "PlayerGender = " & New_Game_Settings.Player_Gender);
      Put_Line
        (Config_File,
         "ShipName = " & To_String(Source => New_Game_Settings.Ship_Name));
      Put_Line
        (Config_File,
         "PlayerFaction = " &
         To_String(Source => New_Game_Settings.Player_Faction));
      Put_Line
        (Config_File,
         "PlayerCareer = " &
         To_String(Source => New_Game_Settings.Player_Career));
      Put_Line
        (Config_File,
         "StartingBase = " &
         To_String(Source => New_Game_Settings.Starting_Base));
      Put_Line
        (Config_File,
         "EnemyDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Enemy_Damage_Bonus));
      Put_Line
        (Config_File,
         "PlayerDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Player_Damage_Bonus));
      Put_Line
        (Config_File,
         "EnemyMeleeDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Enemy_Melee_Damage_Bonus));
      Put_Line
        (Config_File,
         "PlayerMeleeDamageBonus =" &
         Bonus_Type'Image(New_Game_Settings.Player_Melee_Damage_Bonus));
      Put_Line
        (Config_File,
         "ExperienceBonus =" &
         Bonus_Type'Image(New_Game_Settings.Experience_Bonus));
      Put_Line
        (Config_File,
         "ReputationBonus =" &
         Bonus_Type'Image(New_Game_Settings.Reputation_Bonus));
      Put_Line
        (Config_File,
         "UpgradeCostBonus =" &
         Bonus_Type'Image(New_Game_Settings.Upgrade_Cost_Bonus));
      Put_Line
        (Config_File,
         "PricesBonus =" & Bonus_Type'Image(New_Game_Settings.Prices_Bonus));
      Put_Line
        (Config_File,
         "DifficultyLevel = " &
         Difficulty_Type'Image(New_Game_Settings.Difficulty_Level));
      SaveBoolean(Game_Settings.Auto_Rest, "AutoRest");
      Put_Line
        (Config_File,
         "UndockSpeed = " & ShipSpeed'Image(Game_Settings.Undock_Speed));
      SaveBoolean(Game_Settings.Auto_Center, "AutoCenter");
      SaveBoolean(Game_Settings.Auto_Return, "AutoReturn");
      SaveBoolean(Game_Settings.Auto_Finish, "AutoFinish");
      Put_Line
        (Config_File, "LowFuel =" & Positive'Image(Game_Settings.Low_Fuel));
      Put_Line
        (Config_File,
         "LowDrinks =" & Positive'Image(Game_Settings.Low_Drinks));
      Put_Line
        (Config_File, "LowFood =" & Positive'Image(Game_Settings.Low_Food));
      Put_Line
        (Config_File,
         "AutoMoveStop = " &
         Auto_Move_Break'Image(Game_Settings.Auto_Move_Stop));
      Put_Line
        (Config_File,
         "WindowWidth =" & Positive'Image(Game_Settings.Window_Width));
      Put_Line
        (Config_File,
         "WindowHeight =" & Positive'Image(Game_Settings.Window_Height));
      Put_Line
        (Config_File,
         "MessagesLimit =" & Positive'Image(Game_Settings.Messages_Limit));
      Put_Line
        (Config_File,
         "SavedMessages =" & Positive'Image(Game_Settings.Saved_Messages));
      Put_Line
        (Config_File,
         "HelpFontSize =" & Positive'Image(Game_Settings.Help_Font_Size));
      Put_Line
        (Config_File,
         "MapFontSize =" & Positive'Image(Game_Settings.Map_Font_Size));
      Put_Line
        (Config_File,
         "InterfaceFontSize =" &
         Positive'Image(Game_Settings.Interface_Font_Size));
      Put_Line
        (Config_File,
         "InterfaceTheme = " &
         To_String(Source => Game_Settings.Interface_Theme));
      Put_Line
        (Config_File,
         "MessagesOrder = " &
         Messages_Order_Type'Image(Game_Settings.Messages_Order));
      SaveBoolean(Game_Settings.Auto_Ask_For_Bases, "AutoAskForBases");
      SaveBoolean(Game_Settings.Auto_Ask_For_Events, "AutoAskForEvents");
      SaveBoolean(Game_Settings.Show_Tooltips, "ShowTooltips");
      SaveBoolean(Game_Settings.Show_Last_Messages, "ShowLastMessages");
      Put_Line
        (Config_File,
         "MessagesPosition =" &
         Natural'Image(Game_Settings.Messages_Position));
      SaveBoolean(Game_Settings.Full_Screen, "FullScreen");
      Put_Line
        (Config_File,
         "AutoCloseMessagesTime =" &
         Positive'Image(Game_Settings.Auto_Close_Messages_Time));
      Put_Line
        (Config_File,
         "AutoSave = " & Auto_Save_Type'Image(Game_Settings.Auto_Save));
      Put_Line
        (Config_File,
         "TopicsPosition =" & Natural'Image(Game_Settings.Topics_Position));
      SaveBoolean(Game_Settings.Show_Numbers, "ShowNumbers");
      Close(Config_File);
   end Save_Config;

end Config;
