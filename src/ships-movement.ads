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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Ships/SMovement
-- FUNCTION
-- Provides code related to ships movement
-- SOURCE
package Ships.Movement is
-- ****

     -- ****f* SMovement/SMovement.Dock_Ship
     -- FUNCTION
     -- Dock/Undock ship at base
     -- PARAMETERS
     -- Docking - If true, ship docks to the base, otherwise false
     -- Escape  - If true, the player is trying to escape from the base
     --           without paying. Default value is False
     -- RESULT
     -- Empty string if operation was succesfull, otherwise message what goes
     -- wrong
     -- SOURCE
   function Dock_Ship
     (Docking: Boolean; Escape: Boolean := False) return String;
      -- ****

      -- ****f* SMovement/SMovement.Change_Ship_Speed
      -- FUNCTION
      -- Change speed of ship
      -- PARAMETERS
      -- Speed_Value - New speed for the ship
      -- RESULT
      -- Empty string if speed was changed, otherwise message what goes wrong
      -- SOURCE
   function Change_Ship_Speed(Speed_Value: Ship_Speed) return String;
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
