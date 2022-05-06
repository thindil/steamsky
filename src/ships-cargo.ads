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

-- ****h* Ships/SCargo
-- FUNCTION
-- Provides code to manipulate ships cargos
-- SOURCE
package Ships.Cargo is
-- ****

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
     (Ship: in out Ship_Record;
      Proto_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Amount: Integer; Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index, Price: Natural := 0) with
      Pre => Cargo_Index <=
      Inventory_Container.Last_Index(Container => Ship.Cargo),
      Test_Case => (Name => "Test_UpdateCargo", Mode => Nominal);
      -- ****

      -- ****f* SCargo/SCargo.Free_Cargo
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
   function Free_Cargo
     (Amount: Integer; Ship: Ship_Record := Player_Ship) return Integer with
      Test_Case => (Name => "Test_FreeCargo", Mode => Robustness);
      -- ****

      -- ****f* SCargo/SCargo.Get_Item_Amount
      -- FUNCTION
      -- Check how much selected items is in player ship cargo
      -- PARAMETERS
      -- Item_Type - Type of items which will be looking for
      -- RESULT
      -- Amount of items of selected type on player ship
      -- SOURCE
   function Get_Item_Amount
     (Item_Type: Tiny_String.Bounded_String) return Natural with
      Pre => Tiny_String.Length(Source => Item_Type) > 0,
      Test_Case => (Name => "Test_GetItemAmount", Mode => Nominal);
      -- ****

      -- ****f* SCargo/SCargo.Get_Items_Amount
      -- FUNCTION
      -- Check amount of selected consumables on player ship
      -- PARAMETERS
      -- I_Type - "Drinks" or "Food". Type of items which will be looking for
      -- RESULT
      -- Amount of drinks or food, depends on IType on the player ship
      -- SOURCE
   function Get_Items_Amount(I_Type: String) return Natural with
      Pre => I_Type in "Drinks" | "Food",
      Test_Case => (Name => "Test_GetItemsAmount", Mode => Nominal);
      -- ****

end Ships.Cargo;
