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

-- ****h* Ships/SMovement
-- FUNCTION
-- Provides code related to ships movement
-- SOURCE
package Ships.Movement is
-- ****

   -- ****f* SMovement/SMovement.Wait_In_Place
   -- FUNCTION
   -- Use fuel when ship wait in place
   -- PARAMETERS
   -- Minutes - Amount of passed in-game minutes
   -- SOURCE
   procedure Wait_In_Place(Minutes: Positive);
   -- ****

end Ships.Movement;
