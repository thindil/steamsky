--    Copyright 2017-2023 Bartek thindil Jasicki
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

   -- ****f* BSaveLoad/BSaveLoad.Load_Bases
   -- FUNCTION
   -- Load bases from file
   -- PARAMETERS
   -- Save_Data - XML structure from which sky bases data will be loaded
   -- SOURCE
   procedure Load_Bases(Save_Data: not null Document) with
      Post =>
      (for all Index in Sky_Bases'Range =>
         Tiny_String.Length(Source => Sky_Bases(Index).Name) > 0);
   -- ****

end Bases.SaveLoad;
