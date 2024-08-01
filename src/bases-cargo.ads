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

-- ****h* Bases/BCargo
-- FUNCTION
-- Provide code for manipulate cargo of sky bases
-- SOURCE
package Bases.Cargo is
-- ****

   -- ****f* BCargo/BCargo.Find_Base_Cargo
   -- FUNCTION
   -- Find index of item in base cargo
   -- PARAMETERS
   -- Proto_Index - Index of prototype of item to search
   -- Durability  - Durability of item to search. Can be empty
   -- RESULT
   -- Index of item in sky base cargo or 0 if item not found
   -- SOURCE
   function Find_Base_Cargo
     (Proto_Index: Natural;
      Durability: Items_Durability := Items_Durability'Last)
      return Natural with
      Pre => Proto_Index > 0;
      -- ****

end Bases.Cargo;
