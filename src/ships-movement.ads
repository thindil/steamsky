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

package Ships.Movement is

-- ****f* Ships.Movement/MoveShip
-- FUNCTION
-- Move player ship
-- SOURCE
   function MoveShip
     (X, Y: Integer; Message: in out Unbounded_String) return Natural;
-- ****
-- ****f* Ships.Movement/DockShip
-- FUNCTION
-- Dock/Undock ship at base, returns empty string if all ok otherwise error message
-- SOURCE
   function DockShip(Docking: Boolean) return String;
-- ****
-- ****f* Ships.Movement/ChangeShipSpeed
-- FUNCTION
-- Change speed of ship, returns empty string if all ok otherwise error message
-- SOURCE
   function ChangeShipSpeed(SpeedValue: ShipSpeed) return String;
-- ****
-- ****f* Ships.Movement/RealSpeed
-- FUNCTION
-- Return real ship speed in meters per minute
-- SOURCE
   function RealSpeed
     (Ship: ShipRecord; InfoOnly: Boolean := False) return Natural;
-- ****
-- ****f* Ships.Movement/CountFuelNeeded
-- FUNCTION
-- Return fuel needed by player ship to travel
-- SOURCE
   function CountFuelNeeded return Integer;
-- ****
-- ****f* Ships.Movement/WaitInPlace
-- FUNCTION
-- Use fuel when ship wait in place
-- SOURCE
   procedure WaitInPlace(Minutes: Positive);
-- ****

end Ships.Movement;
