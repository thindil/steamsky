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

-- ****h* Steamsky/Ships.Upgrade
-- FUNCTION
-- Provided code for upgrade player ship modules
-- SOURCE
package Ships.Upgrade is
-- ****

   -- ****e* Ships.Upgrade/Ship_Upgrade_Error
   -- FUNCTION
   -- Raised when player can't start upgrading module
   -- SOURCE
   Ship_Upgrade_Error: exception;
   -- ****

   -- ****f* Ships.Upgrade/StartUpgrading
   -- FUNCTION
   -- Set upgrading order
   -- PARAMETERS
   -- ModuleIndex - Player ship index of module to upgrade
   -- UpgradeType - Type of upgrade to start
   -- SOURCE
   procedure StartUpgrading(ModuleIndex, UpgradeType: Positive) with
      Pre =>
      (ModuleIndex <= PlayerShip.Modules.Last_Index and UpgradeType < 5);
      -- ****
      -- ****f* Ships.Upgrade/UpgradeShip
      -- FUNCTION
      -- Upgrade selected module on ship
      -- PARAMETERS
      -- Minutes - Amount of passed in-game minutes
      -- SOURCE
   procedure UpgradeShip(Minutes: Positive);
   -- ****

end Ships.Upgrade;
