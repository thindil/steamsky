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

with Utils; use Utils;
with Trades; use Trades;
with BasesTypes; use BasesTypes;
with Maps; use Maps;

package body Bases.Cargo is

   procedure Generate_Cargo is
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Population: constant Positive :=
        (if Sky_Bases(Base_Index).Population > 0 then
           Sky_Bases(Base_Index).Population
         else 1);
      Chance: Positive :=
        (if Population < 150 then 5 elsif Population < 300 then 10 else 15);
   begin
      Chance := Chance + Days_Difference(Sky_Bases(Base_Index).Visited);
      if Sky_Bases(Base_Index).Cargo.Length = 0 then
         Chance := 101;
      end if;
      if Get_Random(1, 100) > Chance then
         return;
      end if;
      if Sky_Bases(Base_Index).Cargo.Length = 0 then
         Sky_Bases(Base_Index).Cargo.Append
           (New_Item =>
              (Proto_Index => Money_Index,
               Amount => (Get_Random(50, 200) * Population),
               Durability => Default_Item_Durability, Price => 0));
         Add_Base_Cargo_Loop :
         for I in Items_List.Iterate loop
            if Is_Buyable
                (Sky_Bases(Base_Index).Base_Type, Objects_Container.Key(I),
                 False) then
               Sky_Bases(Base_Index).Cargo.Append
                 (New_Item =>
                    (Proto_Index => Objects_Container.Key(I),
                     Amount => (Get_Random(0, 100) * Population),
                     Durability => Default_Item_Durability,
                     Price =>
                       Get_Price
                         (Sky_Bases(Base_Index).Base_Type,
                          Objects_Container.Key(I))));
            end if;
         end loop Add_Base_Cargo_Loop;
         if BasesTypes_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
             (To_Unbounded_String("blackmarket")) then
            declare
               Amount: constant Positive range 1 .. 30 :=
                 (if Population < 150 then Get_Random(1, 10)
                  elsif Population < 300 then Get_Random(1, 20)
                  else Get_Random(1, 30));
               ItemIndex: Natural range 0 .. Positive(Items_List.Length);
            begin
               Add_BlackMarket_Cargo_Loop :
               for I in 1 .. Amount loop
                  ItemIndex := Get_Random(1, Positive(Items_List.Length));
                  Update_Item_Amount_Loop :
                  for J in Items_List.Iterate loop
                     ItemIndex := ItemIndex - 1;
                     if ItemIndex = 0 then
                        if Get_Price
                            (Sky_Bases(Base_Index).Base_Type,
                             Objects_Container.Key(J)) =
                          0 then
                           ItemIndex := ItemIndex + 1;
                        else
                           Sky_Bases(Base_Index).Cargo.Append
                             (New_Item =>
                                (Proto_Index => Objects_Container.Key(J),
                                 Amount => (Get_Random(0, 100) * Population),
                                 Durability => Default_Item_Durability,
                                 Price =>
                                   Get_Price
                                     (Sky_Bases(Base_Index).Base_Type,
                                      Objects_Container.Key(J))));
                           exit Update_Item_Amount_Loop;
                        end if;
                     end if;
                  end loop Update_Item_Amount_Loop;
               end loop Add_BlackMarket_Cargo_Loop;
            end;
         end if;
      else
         declare
            Roll: Positive range 1 .. 100;
            function GetMaxAmount(Amount: Positive) return Positive is
               MaxAmount: Natural;
            begin
               MaxAmount := Amount / 2;
               if MaxAmount < 1 then
                  MaxAmount := 1;
               end if;
               return MaxAmount;
            end GetMaxAmount;
         begin
            Update_Cargo_Loop :
            for Item of Sky_Bases(Base_Index).Cargo loop
               Roll := Get_Random(1, 100);
               if Roll < 30 and Item.Amount > 0 then
                  Item.Amount :=
                    Item.Amount - Get_Random(1, GetMaxAmount(Item.Amount));
               elsif Roll < 60 and Sky_Bases(Base_Index).Population > 0 then
                  Item.Amount :=
                    (if Item.Amount = 0 then Get_Random(1, 10) * Population
                     else Item.Amount +
                       Get_Random(1, GetMaxAmount(Item.Amount)));
               end if;
            end loop Update_Cargo_Loop;
         end;
      end if;
   end Generate_Cargo;

   procedure Update_Base_Cargo
     (Proto_Index: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index: Inventory_Container.Extended_Index := 0) is
      BaseIndex: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      ItemIndex: constant Natural range 0 ..
          Natural(Sky_Bases(BaseIndex).Cargo.Length) :=
        (if Proto_Index /= Null_Unbounded_String then
           Find_Base_Cargo(Proto_Index, Durability)
         else Cargo_Index);
   begin
      if Amount > 0 then
         if ItemIndex = 0 then
            Sky_Bases(BaseIndex).Cargo.Append
              (New_Item =>
                 (Proto_Index => Proto_Index, Amount => Amount,
                  Durability => Durability,
                  Price =>
                    Get_Price(Sky_Bases(BaseIndex).Base_Type, Proto_Index)));
         else
            Sky_Bases(BaseIndex).Cargo(ItemIndex).Amount :=
              Sky_Bases(BaseIndex).Cargo(ItemIndex).Amount + Amount;
         end if;
      else
         Sky_Bases(BaseIndex).Cargo(ItemIndex).Amount :=
           Sky_Bases(BaseIndex).Cargo(ItemIndex).Amount + Amount;
         if Sky_Bases(BaseIndex).Cargo(ItemIndex).Amount = 0 and
           not Is_Buyable
             (Sky_Bases(BaseIndex).Base_Type,
              Sky_Bases(BaseIndex).Cargo(ItemIndex).Proto_Index) and
           ItemIndex > 1 then
            Sky_Bases(BaseIndex).Cargo.Delete(Index => ItemIndex);
         end if;
      end if;
   end Update_Base_Cargo;

   function Find_Base_Cargo
     (Proto_Index: Unbounded_String;
      Durability: Items_Durability := Items_Durability'Last) return Natural is
      BaseIndex: constant Extended_Base_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      function FindCargo(Cargo: BaseCargo_Container.Vector) return Natural is
      begin
         Find_Cargo_Loop :
         for I in Cargo.Iterate loop
            if Durability < Items_Durability'Last then
               if Cargo(I).Proto_Index = Proto_Index and
                 Cargo(I).Durability = Durability then
                  return BaseCargo_Container.To_Index(I);
               end if;
            else
               if Cargo(I).Proto_Index = Proto_Index then
                  return BaseCargo_Container.To_Index(I);
               end if;
            end if;
         end loop Find_Cargo_Loop;
         return 0;
      end FindCargo;
   begin
      if BaseIndex > 0 then
         return FindCargo(Sky_Bases(BaseIndex).Cargo);
      end if;
      return FindCargo(TraderCargo);
   end Find_Base_Cargo;

end Bases.Cargo;
