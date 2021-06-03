--    Copyright 2017-2021 Bartek thindil Jasicki
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

   procedure UpdateCargo
     (Ship: in out Ship_Record;
      ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      CargoIndex, Price: Natural := 0) is
      ItemIndex: Inventory_Container.Extended_Index := 0;
   begin
      if ProtoIndex /= Null_Unbounded_String and CargoIndex = 0 then
         Find_Item_Index_Loop :
         for I in Ship.Cargo.Iterate loop
            if Ship.Cargo(I).ProtoIndex = ProtoIndex and
              Ship.Cargo(I).Durability = Durability then
               ItemIndex := Inventory_Container.To_Index(I);
               exit Find_Item_Index_Loop;
            end if;
         end loop Find_Item_Index_Loop;
      else
         ItemIndex := CargoIndex;
      end if;
      if ItemIndex = 0 and
        (ProtoIndex = Null_Unbounded_String or Amount < 0) then
         return;
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
               Update_Ammo_Index_Loop :
               for Module of Ship.Modules loop
                  if Module.M_Type = GUN then
                     if Module.Ammo_Index > ItemIndex then
                        Module.Ammo_Index := Module.Ammo_Index - 1;
                     elsif Module.Ammo_Index = ItemIndex then
                        Module.Ammo_Index := 0;
                     end if;
                  end if;
               end loop Update_Ammo_Index_Loop;
            else
               Ship.Cargo(ItemIndex).Amount := NewAmount;
               Ship.Cargo(ItemIndex).Price := Price;
            end if;
         end;
      end if;
   end UpdateCargo;

   function FreeCargo
     (Amount: Integer; Ship: Ship_Record := Player_Ship) return Integer is
      FreeCargo: Integer := 0;
   begin
      Count_Cargo_Size_Loop :
      for Module of Ship.Modules loop
         if Module.M_Type = CARGO_ROOM and Module.Durability > 0 then
            FreeCargo := FreeCargo + Modules_List(Module.Proto_Index).MaxValue;
         end if;
      end loop Count_Cargo_Size_Loop;
      Count_Cargo_Weight_Loop :
      for Item of Ship.Cargo loop
         FreeCargo :=
           FreeCargo - (Items_List(Item.ProtoIndex).Weight * Item.Amount);
      end loop Count_Cargo_Weight_Loop;
      FreeCargo := FreeCargo + Amount;
      return FreeCargo;
   end FreeCargo;

   function GetItemAmount(ItemType: Unbounded_String) return Natural is
      Amount: Natural := 0;
   begin
      Get_Item_Amount_Loop :
      for Item of Player_Ship.Cargo loop
         if Items_List(Item.ProtoIndex).IType = ItemType then
            Amount := Amount + Item.Amount;
         end if;
      end loop Get_Item_Amount_Loop;
      return Amount;
   end GetItemAmount;

   function GetItemsAmount(IType: String) return Natural is
      ItemsAmount: Natural;
   begin
      if IType = "Drinks" then
         Get_Drinks_Amount_Loop :
         for Member of Player_Ship.Crew loop
            if Factions_List(Member.Faction).DrinksTypes.Length = 0 then
               ItemsAmount := Game_Settings.Low_Drinks + 1;
            else
               ItemsAmount := 0;
               Get_Selected_Drinks_Amount_Loop :
               for DrinkType of Factions_List(Member.Faction).DrinksTypes loop
                  ItemsAmount := ItemsAmount + GetItemAmount(DrinkType);
               end loop Get_Selected_Drinks_Amount_Loop;
               exit Get_Drinks_Amount_Loop when ItemsAmount <
                 Game_Settings.Low_Drinks;
            end if;
         end loop Get_Drinks_Amount_Loop;
      else
         Get_Items_Amount_Loop :
         for Member of Player_Ship.Crew loop
            if Factions_List(Member.Faction).FoodTypes.Length = 0 then
               ItemsAmount := Game_Settings.Low_Food + 1;
            else
               ItemsAmount := 0;
               Get_Food_Amount_Loop :
               for FoodType of Factions_List(Member.Faction).FoodTypes loop
                  ItemsAmount := ItemsAmount + GetItemAmount(FoodType);
               end loop Get_Food_Amount_Loop;
               exit Get_Items_Amount_Loop when ItemsAmount <
                 Game_Settings.Low_Food;
            end if;
         end loop Get_Items_Amount_Loop;
      end if;
      return ItemsAmount;
   end GetItemsAmount;

end Ships.Cargo;
