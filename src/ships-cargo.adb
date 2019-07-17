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

with ShipModules; use ShipModules;
with Factions; use Factions;
with Config; use Config;

package body Ships.Cargo is

-- ****if* Ships.Cargo/UpdateCargo
-- SOURCE
   procedure UpdateCargo
     (Ship: in out ShipRecord;
      ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Natural := 100; CargoIndex, Price: Natural := 0) is
-- ****
      ItemIndex: Natural := 0;
   begin
      if ProtoIndex /= Null_Unbounded_String and CargoIndex = 0 then
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
               Name => Null_Unbounded_String, Durability => Durability,
               Price => Price));
      else
         declare
            NewAmount: constant Integer :=
              Ship.Cargo(ItemIndex).Amount + Amount;
         begin
            if NewAmount < 1 then
               Ship.Cargo.Delete(Index => ItemIndex);
               for Module of Ship.Modules loop
                  if Module.MType = GUN then
                     if Module.AmmoIndex > ItemIndex then
                        Module.AmmoIndex := Module.AmmoIndex - 1;
                     elsif Module.AmmoIndex = ItemIndex then
                        Module.AmmoIndex := 0;
                     end if;
                  end if;
               end loop;
            else
               Ship.Cargo(ItemIndex).Amount := NewAmount;
               Ship.Cargo(ItemIndex).Price := Price;
            end if;
         end;
      end if;
   end UpdateCargo;

-- ****if* Ships.Cargo/FreeCargo
-- SOURCE
   function FreeCargo
     (Amount: Integer; Ship: ShipRecord := PlayerShip) return Integer is
-- ****
      FreeCargo: Integer := 0;
   begin
      for Module of Ship.Modules loop
         if Module.MType = CARGO_ROOM and Module.Durability > 0 then
            FreeCargo := FreeCargo + Modules_List(Module.ProtoIndex).MaxValue;
         end if;
      end loop;
      for Item of Ship.Cargo loop
         FreeCargo :=
           FreeCargo - (Items_List(Item.ProtoIndex).Weight * Item.Amount);
      end loop;
      FreeCargo := FreeCargo + Amount;
      return FreeCargo;
   end FreeCargo;

-- ****if* Ships.Cargo/GetItemAmount
-- SOURCE
   function GetItemAmount(ItemType: Unbounded_String) return Natural is
-- ****
      Amount: Natural := 0;
   begin
      for Item of PlayerShip.Cargo loop
         if Items_List(Item.ProtoIndex).IType = ItemType then
            Amount := Amount + Item.Amount;
         end if;
      end loop;
      return Amount;
   end GetItemAmount;

-- ****if* Ships.Cargo/GetItemsAmount
-- SOURCE
   function GetItemsAmount(IType: String) return Natural is
-- ****
      ItemsAmount: Natural;
   begin
      if IType = "Drinks" then
         for Member of PlayerShip.Crew loop
            if Factions_List(Member.Faction).DrinksTypes.Length = 0 then
               ItemsAmount := GameSettings.LowDrinks + 1;
            else
               ItemsAmount := 0;
               for DrinkType of Factions_List(Member.Faction).DrinksTypes loop
                  ItemsAmount := ItemsAmount + GetItemAmount(DrinkType);
               end loop;
               exit when ItemsAmount < GameSettings.LowDrinks;
            end if;
         end loop;
      else
         for Member of PlayerShip.Crew loop
            if Factions_List(Member.Faction).FoodTypes.Length = 0 then
               ItemsAmount := GameSettings.LowFood + 1;
            else
               ItemsAmount := 0;
               for FoodType of Factions_List(Member.Faction).FoodTypes loop
                  ItemsAmount := ItemsAmount + GetItemAmount(FoodType);
               end loop;
               exit when ItemsAmount < GameSettings.LowFood;
            end if;
         end loop;
      end if;
      return ItemsAmount;
   end GetItemsAmount;

end Ships.Cargo;
