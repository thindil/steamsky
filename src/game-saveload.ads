--    Copyright 2017-2024 Bartek thindil Jasicki
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

   -- ****f* GSaveLoad/GSaveLoad.Save_Game
   -- FUNCTION
   -- Save game to file
   -- PARAMETERS
   -- Pretty_Print - Did data stored in file should be pretty printed. Default
   --               false
   -- SOURCE
   procedure Save_Game(Pretty_Print: Boolean := False);
   -- ****

end Game.SaveLoad;
