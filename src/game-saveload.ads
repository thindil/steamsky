--    Copyright 2017-2021 Bartek thindil Jasicki
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

-- ****h* Game/GSaveLoad
-- FUNCTION
-- Provide code to save and load the game data from file
-- SOURCE
package Game.SaveLoad is
-- ****

   -- ****v* GSaveLoad/GSaveLoad.Save_Name
   -- FUNCTION
   -- Full path with file name for current savegame
   -- SOURCE
   Save_Name: Unbounded_String;
   -- ****

   -- ****e* GSaveLoad/GSaveLoad.Save_Game_Invalid_Data
   -- FUNCTION
   -- Raised when invalid data found in savegame
   -- SOURCE
   Save_Game_Invalid_Data: exception;
   -- ****

   -- ****f* GSaveLoad/GSaveLoad.Save_Game
   -- FUNCTION
   -- Save game to file
   -- PARAMETERS
   -- Pretty_Print - Did data stored in file should be pretty printed. Default
   --               false
   -- SOURCE
   procedure Save_Game(Pretty_Print: Boolean := False);
   -- ****

   -- ****f* GSaveLoad/GSaveLoad.Load_Game
   -- FUNCTION
   -- Load game from file
   -- SOURCE
   procedure Load_Game;
   -- ****

   -- ****f* GSaveLoad/GSaveLoad.Generate_Save_Name
   -- FUNCTION
   -- Generate unique name for save game file
   -- PARAMETERS
   -- Rename_Save - If true, rename existing save game file. Default false.
   -- SOURCE
   procedure Generate_Save_Name(Rename_Save: Boolean := False) with
      Post => Save_Name /= Save_Name'Old,
      Test_Case => (Name => "Test_GenerateSave_Name", Mode => Nominal);
      -- ****

end Game.SaveLoad;
