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

with ShipModules; use ShipModules;
with Factions; use Factions;
with Config; use Config;

package body Ships.Cargo is

   procedure Update_Cargo
     (Ship: in out Ship_Record;
      Proto_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Amount: Integer; Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index, Price: Natural := 0) is
      use Tiny_String;

      ItemIndex: Inventory_Container.Extended_Index := 0;
   begin
      if Proto_Index /= Null_Bounded_String and Cargo_Index = 0 then
         Find_Item_Index_Loop :
         for I in
           Inventory_Container.First_Index(Container => Ship.Cargo) ..
             Inventory_Container.Last_Index(Container => Ship.Cargo) loop
            if Inventory_Container.Element(Container => Ship.Cargo, Index => I)
                .Proto_Index =
              Proto_Index and
              Inventory_Container.Element(Container => Ship.Cargo, Index => I)
                  .Durability =
                Durability then
               ItemIndex := I;
               exit Find_Item_Index_Loop;
            end if;
         end loop Find_Item_Index_Loop;
      else
         ItemIndex := Cargo_Index;
      end if;
      if ItemIndex = 0 and
        (Proto_Index = Null_Bounded_String or Amount < 0) then
         return;
      end if;
      if ItemIndex = 0 then
         Inventory_Container.Append
           (Container => Ship.Cargo,
            New_Item =>
              (Proto_Index => Proto_Index, Amount => Amount,
               Name => Null_Bounded_String, Durability => Durability,
               Price => Price));
      else
         declare
            Item: Inventory_Data :=
              Inventory_Container.Element
                (Container => Ship.Cargo, Index => ItemIndex);
            NewAmount: constant Integer := Item.Amount + Amount;
         begin
            if NewAmount < 1 then
               Inventory_Container.Delete
                 (Container => Ship.Cargo, Index => ItemIndex);
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
               Item.Amount := NewAmount;
               Item.Price := Price;
               Inventory_Container.Replace_Element
                 (Container => Ship.Cargo, Index => ItemIndex,
                  New_Item => Item);
            end if;
         end;
      end if;
   end Update_Cargo;

   function Free_Cargo
     (Amount: Integer; Ship: Ship_Record := Player_Ship) return Integer is
      FreeCargo: Integer := 0;
   begin
      Count_Cargo_Size_Loop :
      for Module of Ship.Modules loop
         if Module.M_Type = CARGO_ROOM and Module.Durability > 0 then
            FreeCargo :=
              FreeCargo + Modules_List(Module.Proto_Index).Max_Value;
         end if;
      end loop Count_Cargo_Size_Loop;
      Count_Cargo_Weight_Loop :
      for Item of Ship.Cargo loop
         FreeCargo :=
           FreeCargo - (Items_List(Item.Proto_Index).Weight * Item.Amount);
      end loop Count_Cargo_Weight_Loop;
      FreeCargo := FreeCargo + Amount;
      return FreeCargo;
   end Free_Cargo;

   function Get_Item_Amount(Item_Type: Unbounded_String) return Natural is
      Amount: Natural := 0;
   begin
      Get_Item_Amount_Loop :
      for Item of Player_Ship.Cargo loop
         if Items_List(Item.Proto_Index).I_Type = Item_Type then
            Amount := Amount + Item.Amount;
         end if;
      end loop Get_Item_Amount_Loop;
      return Amount;
   end Get_Item_Amount;

   function Get_Items_Amount(I_Type: String) return Natural is
      ItemsAmount: Natural;
   begin
      if I_Type = "Drinks" then
         Get_Drinks_Amount_Loop :
         for Member of Player_Ship.Crew loop
            if Factions_List(Member.Faction).Drinks_Types.Length = 0 then
               ItemsAmount := Game_Settings.Low_Drinks + 1;
            else
               ItemsAmount := 0;
               Get_Selected_Drinks_Amount_Loop :
               for DrinkType of Factions_List(Member.Faction).Drinks_Types loop
                  ItemsAmount := ItemsAmount + Get_Item_Amount(DrinkType);
               end loop Get_Selected_Drinks_Amount_Loop;
               exit Get_Drinks_Amount_Loop when ItemsAmount <
                 Game_Settings.Low_Drinks;
            end if;
         end loop Get_Drinks_Amount_Loop;
      else
         Get_Items_Amount_Loop :
         for Member of Player_Ship.Crew loop
            if Factions_List(Member.Faction).Food_Types.Length = 0 then
               ItemsAmount := Game_Settings.Low_Food + 1;
            else
               ItemsAmount := 0;
               Get_Food_Amount_Loop :
               for FoodType of Factions_List(Member.Faction).Food_Types loop
                  ItemsAmount := ItemsAmount + Get_Item_Amount(FoodType);
               end loop Get_Food_Amount_Loop;
               exit Get_Items_Amount_Loop when ItemsAmount <
                 Game_Settings.Low_Food;
            end if;
         end loop Get_Items_Amount_Loop;
      end if;
      return ItemsAmount;
   end Get_Items_Amount;

end Ships.Cargo;
