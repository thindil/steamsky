--    Copyright 2017-2019 Bartek thindil Jasicki
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

package Game.SaveLoad is

-- ****v* Game.SaveLoad/SaveName
-- FUNCTION
-- Full path with file name for current savegame
-- SOURCE
   SaveName: Unbounded_String;
-- ****
-- ****v* Game.SaveLoad/SaveGame_Invalid_Data
-- FUNCTION
-- Raised when invalid data found in savegame
-- SOURCE
   SaveGame_Invalid_Data: exception;
-- ****

-- ****f* Game.SaveLoad/SaveGame
-- FUNCTION
-- Save game to file
-- SOURCE
   procedure SaveGame(PrettyPrint: Boolean := False);
-- ****
-- ****f* Game.SaveLoad/LoadGame;
-- FUNCTION
-- Load game from file
-- SOURCE
   procedure LoadGame;
-- ****
-- ****f* Game.SaveLoad/GenerateSaveName
-- FUNCTION
-- Generate unique name for save game file
-- SOURCE
   procedure GenerateSaveName(RenameSave: Boolean := False);
-- ****

end Game.SaveLoad;
