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

-- ****h* Ships/SCargo
-- FUNCTION
-- Provides code to manipulate ships cargos
-- SOURCE
package Ships.Cargo is
-- ****

   --## rule off LOCAL_HIDING
   -- ****f* SCargo/SCargo.Update_Cargo
   -- FUNCTION
   -- Update selected item in ship cargo
   -- PARAMETERS
   -- Ship        - Ship which cargo will be updated
   -- Proto_Index - Prototype index of the item which will be modified. Can be
   --               empty if CargoIndex is set
   -- Amount      - Amount of item to add or delete from cargo
   -- Durability  - Durability of item to modify. Can be empty
   -- Cargo_Index - Ship cargo index of the item which will be modified. Can be
   --               empty if ProtoIndex is set
   -- Price       - Price of the item which will be modified
   -- RESULT
   -- Parameter Ship
   -- SOURCE
   procedure Update_Cargo
     (Ship: in out Ship_Record; Proto_Index: Natural := 0; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index, Price: Natural := 0) with
      Pre => Cargo_Index <=
      Inventory_Container.Last_Index(Container => Ship.Cargo);
      -- ****
      --## rule on LOCAL_HIDING

end Ships.Cargo;
