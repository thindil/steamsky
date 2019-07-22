--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Steamsky/Bases.UI
-- FUNCTION
-- Provides code for buying recipes, healing and repair ship in bases
-- SOURCE
package Bases.UI is
-- ****

   -- ****f* Bases.UI/CreateBasesUI
   -- FUNCTION
   -- Create infterace for various bases options
   -- PARAMETERS
   -- NewBuilder - Gtkada_Builder used to create UI
   -- SOURCE
   procedure CreateBasesUI(NewBuilder: Gtkada_Builder);
   -- ****
   -- ****f* Bases.UI/ShowBuyRecipesUI;
   -- FUNCTION
   -- Show interface for buying crafting recipes
   -- SOURCE
   procedure ShowBuyRecipesUI;
   -- ****
   -- ****f* Bases.UI/ShowRepairUI;
   -- FUNCTION
   -- Show interface for buying ship repairs in bases
   -- SOURCE
   procedure ShowRepairUI;
   -- ****
   -- ****f* Bases.UI/ShowHealUI;
   -- FUNCTION
   -- Show interface for buying crew members healing in bases
   -- SOURCE
   procedure ShowHealUI;
   -- ****

end Bases.UI;
