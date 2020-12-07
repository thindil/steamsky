--    Copyright 2016-2020 Bartek thindil Jasicki
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

   procedure LoadConfig is
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
      NewGameSettings :=
        (PlayerName => To_Unbounded_String("Laeran"), PlayerGender => 'M',
         ShipName => To_Unbounded_String("Anaria"),
         PlayerFaction => To_Unbounded_String("POLEIS"),
         PlayerCareer => To_Unbounded_String("general"),
         StartingBase => To_Unbounded_String("Any"), EnemyDamageBonus => 1.0,
         PlayerDamageBonus => 1.0, EnemyMeleeDamageBonus => 1.0,
         PlayerMeleeDamageBonus => 1.0, ExperienceBonus => 1.0,
         ReputationBonus => 1.0, UpgradeCostBonus => 1.0, PricesBonus => 1.0,
         DifficultyLevel => NORMAL);
      GameSettings :=
        (AutoRest => True, UndockSpeed => FULL_SPEED, AutoCenter => True,
         AutoReturn => True, AutoFinish => True, LowFuel => 100,
         LowDrinks => 50, LowFood => 25, AutoMoveStop => NEVER,
         WindowWidth => 800, WindowHeight => 600, MessagesLimit => 500, SavedMessages => 10,
         HelpFontSize => 14, MapFontSize => 16, InterfaceFontSize => 14,
         InterfaceTheme => To_Unbounded_String("steamsky"),
         MessagesOrder => OLDER_FIRST, AutoAskForBases => False,
         AutoAskForEvents => False, ShowTooltips => True,
         ShowLastMessages => True, MessagesPosition => 213,
         FullScreen => False, AutoCloseMessagesTime => 6, AutoSave => NONE,
         TopicsPosition => 200, ShowNumbers => False);
      Open(ConfigFile, In_File, To_String(SaveDirectory) & "game.cfg");
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Get_Line(ConfigFile));
         if Length(RawData) > 0 then
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            if FieldName = To_Unbounded_String("PlayerName") then
               NewGameSettings.PlayerName := Value;
            elsif FieldName = To_Unbounded_String("PlayerGender") then
               NewGameSettings.PlayerGender := Element(Value, 1);
            elsif FieldName = To_Unbounded_String("ShipName") then
               NewGameSettings.ShipName := Value;
            elsif FieldName = To_Unbounded_String("PlayerFaction") then
               NewGameSettings.PlayerFaction := Value;
            elsif FieldName = To_Unbounded_String("PlayerCareer") then
               NewGameSettings.PlayerCareer := Value;
            elsif FieldName = To_Unbounded_String("StartingBase") then
               NewGameSettings.StartingBase := Value;
            elsif FieldName = To_Unbounded_String("EnemyDamageBonus") then
               NewGameSettings.EnemyDamageBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("PlayerDamageBonus") then
               NewGameSettings.PlayerDamageBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("EnemyMeleeDamageBonus") then
               NewGameSettings.EnemyMeleeDamageBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName =
              To_Unbounded_String("PlayerMeleeDamageBonus") then
               NewGameSettings.PlayerMeleeDamageBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ExperienceBonus") then
               NewGameSettings.ExperienceBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ReputationBonus") then
               NewGameSettings.ReputationBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("UpgradeCostBonus") then
               NewGameSettings.UpgradeCostBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("PricesBonus") then
               NewGameSettings.PricesBonus :=
                 Bonus_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("DifficultyLevel") then
               NewGameSettings.DifficultyLevel :=
                 Difficulty_Type'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoRest") then
               GameSettings.AutoRest := LoadBoolean;
            elsif FieldName = To_Unbounded_String("UndockSpeed") then
               GameSettings.UndockSpeed := ShipSpeed'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoCenter") then
               GameSettings.AutoCenter := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoReturn") then
               GameSettings.AutoReturn := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoFinish") then
               GameSettings.AutoFinish := LoadBoolean;
            elsif FieldName = To_Unbounded_String("LowFuel") then
               GameSettings.LowFuel := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("LowDrinks") then
               GameSettings.LowDrinks := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("LowFood") then
               GameSettings.LowFood := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoMoveStop") then
               GameSettings.AutoMoveStop :=
                 AutoMoveBreak'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowWidth") then
               GameSettings.WindowWidth := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowHeight") then
               GameSettings.WindowHeight := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("MessagesLimit") then
               GameSettings.MessagesLimit := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("SavedMessages") then
               GameSettings.SavedMessages := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("HelpFontSize") then
               GameSettings.HelpFontSize := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("MapFontSize") then
               GameSettings.MapFontSize := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("InterfaceFontSize") then
               GameSettings.InterfaceFontSize :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("InterfaceTheme") then
               GameSettings.InterfaceTheme := Value;
            elsif FieldName = To_Unbounded_String("MessagesOrder") then
               GameSettings.MessagesOrder :=
                 MessagesOrderType'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoAskForBases") then
               GameSettings.AutoAskForBases := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoAskForEvents") then
               GameSettings.AutoAskForEvents := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowTooltips") then
               GameSettings.ShowTooltips := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowLastMessages") then
               GameSettings.ShowLastMessages := LoadBoolean;
            elsif FieldName = To_Unbounded_String("MessagesPosition") then
               GameSettings.MessagesPosition :=
                 Natural'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("FullScreen") then
               GameSettings.FullScreen := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoCloseMessagesTime") then
               GameSettings.AutoCloseMessagesTime :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoSave") then
               GameSettings.AutoSave := AutoSaveType'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("TopicsPosition") then
               GameSettings.TopicsPosition := Natural'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ShowNumbers") then
               GameSettings.ShowNumbers := LoadBoolean;
            end if;
         end if;
      end loop;
      Close(ConfigFile);
   exception
      when Ada.Directories.Name_Error =>
         null;
   end LoadConfig;

   procedure SaveConfig is
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
      Create(ConfigFile, Append_File, To_String(SaveDirectory) & "game.cfg");
      Put_Line
        (ConfigFile, "PlayerName = " & To_String(NewGameSettings.PlayerName));
      Put_Line(ConfigFile, "PlayerGender = " & NewGameSettings.PlayerGender);
      Put_Line
        (ConfigFile, "ShipName = " & To_String(NewGameSettings.ShipName));
      Put_Line
        (ConfigFile,
         "PlayerFaction = " & To_String(NewGameSettings.PlayerFaction));
      Put_Line
        (ConfigFile,
         "PlayerCareer = " & To_String(NewGameSettings.PlayerCareer));
      Put_Line
        (ConfigFile,
         "StartingBase = " & To_String(NewGameSettings.StartingBase));
      Put_Line
        (ConfigFile,
         "EnemyDamageBonus =" &
         Bonus_Type'Image(NewGameSettings.EnemyDamageBonus));
      Put_Line
        (ConfigFile,
         "PlayerDamageBonus =" &
         Bonus_Type'Image(NewGameSettings.PlayerDamageBonus));
      Put_Line
        (ConfigFile,
         "EnemyMeleeDamageBonus =" &
         Bonus_Type'Image(NewGameSettings.EnemyMeleeDamageBonus));
      Put_Line
        (ConfigFile,
         "PlayerMeleeDamageBonus =" &
         Bonus_Type'Image(NewGameSettings.PlayerMeleeDamageBonus));
      Put_Line
        (ConfigFile,
         "ExperienceBonus =" &
         Bonus_Type'Image(NewGameSettings.ExperienceBonus));
      Put_Line
        (ConfigFile,
         "ReputationBonus =" &
         Bonus_Type'Image(NewGameSettings.ReputationBonus));
      Put_Line
        (ConfigFile,
         "UpgradeCostBonus =" &
         Bonus_Type'Image(NewGameSettings.UpgradeCostBonus));
      Put_Line
        (ConfigFile,
         "PricesBonus =" & Bonus_Type'Image(NewGameSettings.PricesBonus));
      Put_Line
        (ConfigFile,
         "DifficultyLevel = " &
         Difficulty_Type'Image(NewGameSettings.DifficultyLevel));
      SaveBoolean(GameSettings.AutoRest, "AutoRest");
      Put_Line
        (ConfigFile,
         "UndockSpeed = " & ShipSpeed'Image(GameSettings.UndockSpeed));
      SaveBoolean(GameSettings.AutoCenter, "AutoCenter");
      SaveBoolean(GameSettings.AutoReturn, "AutoReturn");
      SaveBoolean(GameSettings.AutoFinish, "AutoFinish");
      Put_Line(ConfigFile, "LowFuel =" & Positive'Image(GameSettings.LowFuel));
      Put_Line
        (ConfigFile, "LowDrinks =" & Positive'Image(GameSettings.LowDrinks));
      Put_Line(ConfigFile, "LowFood =" & Positive'Image(GameSettings.LowFood));
      Put_Line
        (ConfigFile,
         "AutoMoveStop = " & AutoMoveBreak'Image(GameSettings.AutoMoveStop));
      Put_Line
        (ConfigFile,
         "WindowWidth =" & Positive'Image(GameSettings.WindowWidth));
      Put_Line
        (ConfigFile,
         "WindowHeight =" & Positive'Image(GameSettings.WindowHeight));
      Put_Line
        (ConfigFile,
         "MessagesLimit =" & Positive'Image(GameSettings.MessagesLimit));
      Put_Line
        (ConfigFile,
         "SavedMessages =" & Positive'Image(GameSettings.SavedMessages));
      Put_Line
        (ConfigFile,
         "HelpFontSize =" & Positive'Image(GameSettings.HelpFontSize));
      Put_Line
        (ConfigFile,
         "MapFontSize =" & Positive'Image(GameSettings.MapFontSize));
      Put_Line
        (ConfigFile,
         "InterfaceFontSize =" &
         Positive'Image(GameSettings.InterfaceFontSize));
      Put_Line
        (ConfigFile,
         "InterfaceTheme = " & To_String(GameSettings.InterfaceTheme));
      Put_Line
        (ConfigFile,
         "MessagesOrder = " &
         MessagesOrderType'Image(GameSettings.MessagesOrder));
      SaveBoolean(GameSettings.AutoAskForBases, "AutoAskForBases");
      SaveBoolean(GameSettings.AutoAskForEvents, "AutoAskForEvents");
      SaveBoolean(GameSettings.ShowTooltips, "ShowTooltips");
      SaveBoolean(GameSettings.ShowLastMessages, "ShowLastMessages");
      Put_Line
        (ConfigFile,
         "MessagesPosition =" & Natural'Image(GameSettings.MessagesPosition));
      SaveBoolean(GameSettings.FullScreen, "FullScreen");
      Put_Line
        (ConfigFile,
         "AutoCloseMessagesTime =" &
         Positive'Image(GameSettings.AutoCloseMessagesTime));
      Put_Line
        (ConfigFile,
         "AutoSave = " & AutoSaveType'Image(GameSettings.AutoSave));
      Put_Line
        (ConfigFile,
         "TopicsPosition =" & Natural'Image(GameSettings.TopicsPosition));
      SaveBoolean(GameSettings.ShowNumbers, "ShowNumbers");
      Close(ConfigFile);
   end SaveConfig;

end Config;
