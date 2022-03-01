--    Copyright 2017-2022 Bartek thindil Jasicki
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

with DOM.Core; use DOM.Core;

-- ****h* Ships/SSaveLoad
-- FUNCTION
-- Provides code to save and load player ship data to file
-- SOURCE
package Ships.SaveLoad is
-- ****

   -- ****f* SSaveLoad/SSaveLoad.Save_Player_Ship
   -- FUNCTION
   -- Save player ship to file
   -- PAEAMETERS
   -- Save_Data - XML structure to which player ship data will be saved
   -- Main_Node - XML main node to which player ship data will be saved
   -- SOURCE
   procedure Save_Player_Ship
     (Save_Data: Document; Main_Node: DOM.Core.Element);
   -- ****

   -- ****f* SSaveLoad/SSaveLoad.Load_Player_Ship
   -- FUNCTION
   -- Load saved player ship from file
   -- PARAMETERS
   -- Save_Data - XML structure from which player ship data will be loaded
   -- SOURCE
   procedure Load_Player_Ship(Save_Data: Document);
   -- ****

end Ships.SaveLoad;
