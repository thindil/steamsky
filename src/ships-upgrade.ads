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

package Ships.Upgrade is

-- ****v* Ships.Upgrade/Ship_Upgrade_Error
-- FUNCTION
-- Raised when player can't start upgrading module
-- SOURCE
   Ship_Upgrade_Error: exception;
-- ****

-- ****f* Ships.Upgrade/StartUpgrading
-- FUNCTION
-- Set upgrading order
-- SOURCE
   procedure StartUpgrading(ModuleIndex, UpgradeType: Positive) with
      Pre =>
      (ModuleIndex <= PlayerShip.Modules.Last_Index and UpgradeType < 5);
-- ****
-- ****f* Ships.Upgrade/UpgradeShip
-- FUNCTION
-- Upgrade selected module on ship
-- SOURCE
   procedure UpgradeShip(Minutes: Positive);
-- ****

end Ships.Upgrade;
