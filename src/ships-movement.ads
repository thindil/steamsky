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

   -- Move player ship
   function MoveShip
     (X, Y: Integer; Message: in out Unbounded_String) return Natural;
   -- Dock/Undock ship at base, returns empty string if all ok otherwise error message
   function DockShip(Docking: Boolean) return String;
-- Change speed of ship, returns empty string if all ok otherwise error message
   function ChangeShipSpeed(SpeedValue: ShipSpeed) return String;
   -- Return real ship speed in meters per minute
   function RealSpeed
     (Ship: ShipRecord; InfoOnly: Boolean := False) return Natural;
   -- Return fuel needed by player ship to travel
   function CountFuelNeeded return Integer;
   -- Use fuel when ship wait in place
   procedure WaitInPlace(Minutes: Positive);

end Ships.Movement;
