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

package Ships.Cargo is

   procedure UpdateCargo(Ship: in out ShipRecord;
      ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Natural := 100; CargoIndex, Price: Natural := 0) with
      Pre => CargoIndex <=
      Ship.Cargo.Last_Index; -- Update selected item in ship cargo
   function FreeCargo(Amount: Integer;
      Ship: ShipRecord := PlayerShip)
     return Integer; -- Return available space in cargo after adding/extracting Amount
   -- Return amount of items of selected type on player ship
   function GetItemAmount(ItemType: Unbounded_String) return Natural;
   -- Return amount of drinks or food (depends on IType) on player ship
   function GetItemsAmount(IType: String) return Natural;

end Ships.Cargo;
