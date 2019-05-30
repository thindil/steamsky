--    Copyright 2017 Bartek thindil Jasicki
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

with ShipModules; use ShipModules;

package body Ships.Cargo is

   procedure UpdateCargo
     (Ship: in out ShipRecord; ProtoIndex: Natural := 0; Amount: Integer;
      Durability: Natural := 100; CargoIndex: Natural := 0) is
      ItemIndex: Natural := 0;
      NewAmount: Integer;
   begin
      if ProtoIndex > 0 and CargoIndex = 0 then
         for I in Ship.Cargo.Iterate loop
            if Ship.Cargo(I).ProtoIndex = ProtoIndex and
              Ship.Cargo(I).Durability = Durability then
               ItemIndex := Inventory_Container.To_Index(I);
               exit;
            end if;
         end loop;
      else
         ItemIndex := CargoIndex;
      end if;
      if ItemIndex = 0 then
         Ship.Cargo.Append
           (New_Item =>
              (ProtoIndex => ProtoIndex, Amount => Amount,
               Name => Null_Unbounded_String, Durability => Durability));
      else
         NewAmount := Ship.Cargo(ItemIndex).Amount + Amount;
         if NewAmount < 1 then
            Ship.Cargo.Delete(Index => ItemIndex);
            for Module of Ship.Modules loop
               if Modules_List(Module.ProtoIndex).MType = GUN then
                  if Module.Data(1) > ItemIndex then
                     Module.Data(1) := Module.Data(1) - 1;
                  elsif Module.Data(1) = ItemIndex then
                     Module.Data(1) := 0;
                  end if;
               end if;
            end loop;
         else
            Ship.Cargo(ItemIndex).Amount := NewAmount;
         end if;
      end if;
   end UpdateCargo;

   function FreeCargo
     (Amount: Integer; Ship: ShipRecord := PlayerShip) return Integer is
      FreeCargo: Integer := 0;
   begin
      for Module of Ship.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ShipModules.CARGO and
           Module.Durability > 0 then
            FreeCargo := FreeCargo + Module.Data(2);
         end if;
      end loop;
      for Item of Ship.Cargo loop
         FreeCargo :=
           FreeCargo - (Items_List(Item.ProtoIndex).Weight * Item.Amount);
      end loop;
      FreeCargo := FreeCargo + Amount;
      return FreeCargo;
   end FreeCargo;

end Ships.Cargo;
