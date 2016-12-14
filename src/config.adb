--    Copyright 2016 Bartek thindil Jasicki
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

package body Config is

    procedure LoadConfig is
        ConfigFile : File_Type;
    begin
        if not Exists("data/game.cfg") then
            NewGameSettings := (PlayerName => To_Unbounded_String("Laeran"),
                PlayerGender => 'M', ShipName => To_Unbounded_String("Hawk"));
            return;
        end if;
        Open(ConfigFile, In_File, "data/game.cfg");
        NewGameSettings.PlayerName := To_Unbounded_String(Get_Line(ConfigFile));
        NewGameSettings.PlayerGender := Get_Line(ConfigFile)(1);
        NewGameSettings.ShipName := To_Unbounded_String(Get_Line(ConfigFile));
        Close(ConfigFile);
    end LoadConfig;

    procedure SaveConfig is
        ConfigFile : File_Type;
    begin
        Create(ConfigFile, Append_File, "data/game.cfg");
        Put_Line(ConfigFile, To_String(NewGameSettings.PlayerName));
        Put(ConfigFile, NewGameSettings.PlayerGender);
        Put(ConfigFile, ASCII.LF);
        Put_Line(ConfigFile, To_String(NewGameSettings.ShipName));
        Close(ConfigFile);
    end SaveConfig;

end Config;
