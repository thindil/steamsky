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

with DOM.Core; use DOM.Core;

-- ****h* Bases/BSaveLoad
-- FUNCTION
-- Provide code to save and load sky bases data from file
-- SOURCE
package Bases.SaveLoad is
-- ****

   -- ****f* BSaveLoad/BSaveLoad.SaveBases
   -- FUNCTION
   -- Save bases from current game in file
   -- PARAMETERS
   -- SaveData - XML structure to which sky bases data will be saved
   -- MainNode - XML main node to which sky bases data will be saved
   -- SOURCE
   procedure SaveBases
     (SaveData: not null Document; MainNode: not null DOM.Core.Element);
   -- ****

   -- ****f* BSaveLoad/BSaveLoad.LoadBases
   -- FUNCTION
   -- Load bases from file
   -- PARAMETERS
   -- SaveData - XML structure from which sky bases data will be loaded
   -- SOURCE
   procedure LoadBases(SaveData: not null Document) with
      Post =>
      (for all I in SkyBases'Range =>
         SkyBases(I).Name /= Null_Unbounded_String);
   -- ****

end Bases.SaveLoad;
