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

-- ****h* Steamsky/Ships.Cargo
-- FUNCTION
-- Provides code to manipulate ships cargos
-- SOURCE
package Ships.Cargo is
-- ****

   -- ****f* Ships.Cargo/UpdateCargo
   -- FUNCTION
   -- Update selected item in ship cargo
   -- PARAMETERS
   -- Ship       - Ship which cargo will be updated
   -- ProtoIndex - Prototype index of the item which will be modified. Can be
   --              empty if CargoIndex is set
   -- Amount     - Amount of item to add or delete from cargo
   -- Durability - Durability of item to modify. Can be empty
   -- CargoIndex - Ship cargo index of the item which will be modified. Can be
   --              empty if ProtoIndex is set
   -- Price      - Price of the item which will be modified
   -- RESULT
   -- Parameter Ship
   -- SOURCE
   procedure UpdateCargo
     (Ship: in out ShipRecord;
      ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Natural := 100; CargoIndex, Price: Natural := 0) with
      Pre => CargoIndex <= Ship.Cargo.Last_Index,
      Test_Case => ("Test_UpdateCargo", Nominal);
      -- ****

      -- ****f* Ships.Cargo/FreeCargo
      -- FUNCTION
      -- Check how much is free space in cargo of selected ship
      -- PARAMETERS
      -- Amount - Amount of kilograms to add or delete before count free
      --          cargo space
      -- Ship   - Ship in which cargo will be check for free space. Default
      --          is player ship
      -- RESULT
      -- Amount of free cargo space in kilograms after add or remove Amount
      -- of kilograms
      -- SOURCE
   function FreeCargo
     (Amount: Integer; Ship: ShipRecord := PlayerShip) return Integer;
   -- ****

   -- ****f* Ships.Cargo/GetItemAmount
   -- FUNCTION
   -- Check how much selected items is in player ship cargo
   -- PARAMETERS
   -- ItemType - Type of items which will be looking for
   -- RESULT
   -- Amount of items of selected type on player ship
   -- SOURCE
   function GetItemAmount(ItemType: Unbounded_String) return Natural with
      Pre => ItemType /= Null_Unbounded_String;
      -- ****

      -- ****f* Ships.Cargo/GetItemsAmount
      -- FUNCTION
      -- Check amount of selected consumables on player ship
      -- PARAMETERS
      -- IType - "Drinks" or "Food". Type of items which will be looking for
      -- RESULT
      -- Amount of drinks or food, depends on IType on the player ship
      -- SOURCE
   function GetItemsAmount(IType: String) return Natural with
      Pre => IType = "Drinks" or IType = "Food";
      -- ****

end Ships.Cargo;
