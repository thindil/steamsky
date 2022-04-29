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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Ships/SMovement
-- FUNCTION
-- Provides code related to ships movement
-- SOURCE
package Ships.Movement is
-- ****

   -- ****f* SMovement/SMovement.MoveShip
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
   function Move_Ship
     (X, Y: Integer; Message: in out Unbounded_String) return Natural with
      Test_Case => (Name => "Test_MoveShip", Mode => Robustness);
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
     (Docking: Boolean; Escape: Boolean := False) return String with
      Test_Case => (Name => "Test_DockShip", Mode => Robustness);
      -- ****

      -- ****f* SMovement/SMovement.Change_Ship_Speed
      -- FUNCTION
      -- Change speed of ship
      -- PARAMETERS
      -- Speed_Value - New speed for the ship
      -- RESULT
      -- Empty string if speed was changed, otherwise message what goes wrong
      -- SOURCE
   function Change_Ship_Speed(Speed_Value: Ship_Speed) return String with
      Test_Case => (Name => "Test_ChangeShipSpeed", Mode => Robustness);
      -- ****

      -- ****f* SMovement/SMovement.Real_Speed
      -- FUNCTION
      -- Count real ship speed in meters per minute
      -- PARAMETERS
      -- Ship      - Ship which real speed will be counted
      -- Info_Only - If true and ship is docked to the base, count max speed
      --             of the ship. Default is false
      -- RESULT
      -- The real speed of the selected ship or 0 if the ship can't move
      -- SOURCE
   function Real_Speed
     (Ship: Ship_Record; Info_Only: Boolean := False) return Natural with
      Test_Case => (Name => "Test_RealSpeed", Mode => Robustness);
      -- ****

      -- ****f* SMovement/SMovement.Count_Fuel_Needed
      -- FUNCTION
      -- Count amount of fuel needed by player ship to travel
      -- RESULT
      -- Amount of fuel needed by player ship to travel
      -- SOURCE
   function Count_Fuel_Needed return Integer with
      Test_Case => (Name => "Test_CountFuelNeeded", Mode => Robustness);
      -- ****

      -- ****f* SMovement/SMovement.Wait_In_Place
      -- FUNCTION
      -- Use fuel when ship wait in place
      -- PARAMETERS
      -- Minutes - Amount of passed in-game minutes
      -- SOURCE
   procedure Wait_In_Place(Minutes: Positive) with
      Test_Case => (Name => "Test_WaitInPlace", Mode => Robustness);
      -- ****

end Ships.Movement;
