--    Copyright 2016-2017 Bartek thindil Jasicki
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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Game; use Game;

package body Config is

   procedure LoadConfig is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex, StartIndex, EndIndex, KeysAmount: Natural;
   begin
      NewGameSettings :=
        (PlayerName => To_Unbounded_String("Laeran"),
         PlayerGender => 'M',
         ShipName => To_Unbounded_String("Anaria"));
      GameSettings :=
        (AutoRest => True,
         UndockSpeed => QUARTER_SPEED,
         AutoCenter => True,
         AutoReturn => True,
         AutoFinish => True,
         Keys =>
           (56,
            50,
            54,
            52,
            49,
            51,
            55,
            57,
            53,
            37,
            337,
            336,
            402,
            393,
            391,
            398,
            386,
            396,
            32,
            10));
      if not Exists(To_String(SaveDirectory) & "game.cfg") then
         return;
      end if;
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
            elsif FieldName = To_Unbounded_String("AutoRest") then
               if Value = To_Unbounded_String("Yes") then
                  GameSettings.AutoRest := True;
               else
                  GameSettings.AutoRest := False;
               end if;
            elsif FieldName = To_Unbounded_String("UndockSpeed") then
               GameSettings.UndockSpeed := ShipSpeed'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("AutoCenter") then
               if Value = To_Unbounded_String("Yes") then
                  GameSettings.AutoCenter := True;
               else
                  GameSettings.AutoCenter := False;
               end if;
            elsif FieldName = To_Unbounded_String("AutoReturn") then
               if Value = To_Unbounded_String("Yes") then
                  GameSettings.AutoReturn := True;
               else
                  GameSettings.AutoReturn := False;
               end if;
            elsif FieldName = To_Unbounded_String("AutoFinish") then
               if Value = To_Unbounded_String("Yes") then
                  GameSettings.AutoFinish := True;
               else
                  GameSettings.AutoFinish := False;
               end if;
            elsif FieldName = To_Unbounded_String("Keys") then
               StartIndex := 1;
               KeysAmount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
               for I in 1 .. KeysAmount loop
                  EndIndex := Index(Value, ", ", StartIndex);
                  if EndIndex = 0 then
                     EndIndex := Length(Value) + 1;
                  end if;
                  GameSettings.Keys(I) :=
                    Integer'Value(Slice(Value, StartIndex, EndIndex - 1));
                  StartIndex := EndIndex + 2;
               end loop;
            end if;
         end if;
      end loop;
      Close(ConfigFile);
   end LoadConfig;

   procedure SaveConfig is
      ConfigFile: File_Type;
   begin
      Create(ConfigFile, Append_File, To_String(SaveDirectory) & "game.cfg");
      Put_Line
        (ConfigFile,
         "PlayerName = " & To_String(NewGameSettings.PlayerName));
      Put_Line(ConfigFile, "PlayerGender = " & NewGameSettings.PlayerGender);
      Put_Line
        (ConfigFile,
         "ShipName = " & To_String(NewGameSettings.ShipName));
      if GameSettings.AutoRest then
         Put_Line(ConfigFile, "AutoRest = Yes");
      else
         Put_Line(ConfigFile, "AutoRest = No");
      end if;
      Put_Line
        (ConfigFile,
         "UndockSpeed = " & ShipSpeed'Image(GameSettings.UndockSpeed));
      if GameSettings.AutoCenter then
         Put_Line(ConfigFile, "AutoCenter = Yes");
      else
         Put_Line(ConfigFile, "AutoCenter = No");
      end if;
      if GameSettings.AutoReturn then
         Put_Line(ConfigFile, "AutoReturn = Yes");
      else
         Put_Line(ConfigFile, "AutoReturn = No");
      end if;
      if GameSettings.AutoFinish then
         Put_Line(ConfigFile, "AutoFinish = Yes");
      else
         Put_Line(ConfigFile, "AutoFinish = No");
      end if;
      Put(ConfigFile, "Keys = ");
      for I in GameSettings.Keys'Range loop
         if I < GameSettings.Keys'Last then
            Put(ConfigFile, Integer'Image(GameSettings.Keys(I)) & ", ");
         else
            Put_Line(ConfigFile, Integer'Image(GameSettings.Keys(I)));
         end if;
      end loop;
      Close(ConfigFile);
   end SaveConfig;

end Config;
