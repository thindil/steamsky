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

-- ****h* Steamsky/Ships.Movement
-- FUNCTION
-- Provides code related to ships movement
-- SOURCE
package Ships.Movement is
-- ****

   -- ****f* Ships.Movement/MoveShip
   -- FUNCTION
   -- Move player ship
   -- PARAMETERS
   -- X       - Amount of X coordinate fields to move player ship
   -- Y       - Amount of Y coordinate fields to move player ship
   -- Message - If ship cannot be moved, here will be reason of why
   -- RESULT
   -- State after move (or not, then return 0) player ship and parameter
   -- Message
   -- SOURCE
   function MoveShip
     (X, Y: Integer; Message: in out Unbounded_String) return Natural with
      Test_Case => ("Test_MoveShip", Robustness);
      -- ****

      -- ****f* Ships.Movement/DockShip
      -- FUNCTION
      -- Dock/Undock ship at base
      -- PARAMETERS
      -- Docking - If true, ship docks to the base, otherwise false
      -- RESULT
      -- Empty string if operation was succesfull, otherwise message what goes
      -- wrong
      -- SOURCE
   function DockShip(Docking: Boolean) return String with
      Test_Case => ("Test_DockShip", Robustness);
      -- ****

      -- ****f* Ships.Movement/ChangeShipSpeed
      -- FUNCTION
      -- Change speed of ship
      -- PARAMETERS
      -- SpeedValue - New speed for the ship
      -- RESULT
      -- Empty string if speed was changed, otherwise message what goes wrong
      -- SOURCE
   function ChangeShipSpeed(SpeedValue: ShipSpeed) return String with
      Test_Case => ("Test_ChangeShipSpeed", Robustness);
      -- ****

      -- ****f* Ships.Movement/RealSpeed
      -- FUNCTION
      -- Count real ship speed in meters per minute
      -- PARAMETERS
      -- Ship     - Ship which real speed will be counted
      -- InfoOnly - If true and ship is docked to the base, count max speed
      --            of the ship. Default is false
      -- RESULT
      -- Empty string if ship can move, otherwise message why it can't
      -- SOURCE
   function RealSpeed
     (Ship: ShipRecord; InfoOnly: Boolean := False) return Natural;
   -- ****

   -- ****f* Ships.Movement/CountFuelNeeded
   -- FUNCTION
   -- Count amount of fuel needed by player ship to travel
   -- RESULT
   -- Amount of fuel needed by player ship to travel
   -- SOURCE
   function CountFuelNeeded return Integer;
   -- ****

   -- ****f* Ships.Movement/WaitInPlace
   -- FUNCTION
   -- Use fuel when ship wait in place
   -- PARAMETERS
   -- Minutes - Amount of passed in-game minutes
   -- SOURCE
   procedure WaitInPlace(Minutes: Positive);
   -- ****

end Ships.Movement;
