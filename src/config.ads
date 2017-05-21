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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ships; use Ships;

package Config is

   type NewGameRecord is -- Data for new game settings
   record
      PlayerName: Unbounded_String; -- Default player name
      PlayerGender: Character; -- Default player gender
      ShipName: Unbounded_String; -- Default ship name
   end record;
   type GameSettingsRecord is -- Data for game settings
   record
      AutoRest: Boolean; -- If true, rest when pilot/engineer need rest
      UndockSpeed: ShipSpeed; -- Default player ship speed after undock
   end record;
   NewGameSettings: NewGameRecord;
   GameSettings: GameSettingsRecord;

   procedure LoadConfig; -- Load game configuration from file
   procedure SaveConfig; -- Save game configuration to file

end Config;
