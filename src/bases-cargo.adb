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
      Chance :=
        Chance +
        Days_Difference(Date_To_Compare => Sky_Bases(Base_Index).Visited);
      if Sky_Bases(Base_Index).Cargo.Length = 0 then
         Chance := 101;
      end if;
      if Get_Random(Min => 1, Max => 100) > Chance then
         return;
      end if;
      if Sky_Bases(Base_Index).Cargo.Length = 0 then
         Sky_Bases(Base_Index).Cargo.Append
           (New_Item =>
              (Proto_Index => Money_Index,
               Amount => (Get_Random(Min => 50, Max => 200) * Population),
               Durability => Default_Item_Durability, Price => 0));
         Add_Base_Cargo_Loop :
         for I in Items_List.Iterate loop
            if Is_Buyable
                (BaseType => Sky_Bases(Base_Index).Base_Type,
                 ItemIndex => Objects_Container.Key(Position => I),
                 CheckFlag => False) then
               Sky_Bases(Base_Index).Cargo.Append
                 (New_Item =>
                    (Proto_Index => Objects_Container.Key(Position => I),
                     Amount => (Get_Random(Min => 0, Max => 100) * Population),
                     Durability => Default_Item_Durability,
                     Price =>
                       Get_Price
                         (BaseType => Sky_Bases(Base_Index).Base_Type,
                          ItemIndex => Objects_Container.Key(Position => I))));
            end if;
         end loop Add_Base_Cargo_Loop;
         if BasesTypes_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
             (Item => To_Unbounded_String(Source => "blackmarket")) then
            Add_Black_Market_Cargo_Block :
            declare
               Amount: constant Positive range 1 .. 30 :=
                 (if Population < 150 then Get_Random(Min => 1, Max => 10)
                  elsif Population < 300 then Get_Random(Min => 1, Max => 20)
                  else Get_Random(Min => 1, Max => 30));
               Item_Index: Natural range 0 .. Positive(Items_List.Length);
            begin
               Add_Black_Market_Cargo_Loop :
               for I in 1 .. Amount loop
                  Item_Index :=
                    Get_Random(Min => 1, Max => Positive(Items_List.Length));
                  Update_Item_Amount_Loop :
                  for J in Items_List.Iterate loop
                     Item_Index := Item_Index - 1;
                     if Item_Index = 0 then
                        if Get_Price
                            (BaseType => Sky_Bases(Base_Index).Base_Type,
                             ItemIndex =>
                               Objects_Container.Key(Position => J)) =
                          0 then
                           Item_Index := Item_Index + 1;
                        else
                           Sky_Bases(Base_Index).Cargo.Append
                             (New_Item =>
                                (Proto_Index => Objects_Container.Key(Position => J),
                                 Amount => (Get_Random(Min => 0, Max => 100) * Population),
                                 Durability => Default_Item_Durability,
                                 Price =>
                                   Get_Price
                                     (BaseType =>
                                        Sky_Bases(Base_Index).Base_Type,
                                      ItemIndex =>
                                        Objects_Container.Key
                                          (Position => J))));
                           exit Update_Item_Amount_Loop;
                        end if;
                     end if;
                  end loop Update_Item_Amount_Loop;
               end loop Add_Black_Market_Cargo_Loop;
            end Add_Black_Market_Cargo_Block;
         end if;
      else
         Update_Cargo_Block :
         declare
            Roll: Positive range 1 .. 100;
            function Get_Max_Amount(Amount: Positive) return Positive is
               Max_Amount: Natural;
            begin
               Max_Amount := Amount / 2;
               if Max_Amount < 1 then
                  Max_Amount := 1;
               end if;
               return Max_Amount;
            end Get_Max_Amount;
         begin
            Update_Cargo_Loop :
            for Item of Sky_Bases(Base_Index).Cargo loop
               Roll := Get_Random(Min => 1, Max => 100);
               if Roll < 30 and Item.Amount > 0 then
                  Item.Amount :=
                    Item.Amount - Get_Random(Min => 1, Max => Get_Max_Amount(Amount => Item.Amount));
               elsif Roll < 60 and Sky_Bases(Base_Index).Population > 0 then
                  Item.Amount :=
                    (if Item.Amount = 0 then Get_Random(Min => 1, Max => 10) * Population
                     else Item.Amount +
                       Get_Random(Min => 1, Max => Get_Max_Amount(Amount => Item.Amount)));
               end if;
            end loop Update_Cargo_Loop;
         end Update_Cargo_Block;
      end if;
   end Generate_Cargo;

   procedure Update_Base_Cargo
     (Proto_Index: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index: Inventory_Container.Extended_Index := 0) is
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Item_Index: constant Natural range 0 ..
          Natural(Sky_Bases(Base_Index).Cargo.Length) :=
        (if Proto_Index /= Null_Unbounded_String then
           Find_Base_Cargo(Proto_Index => Proto_Index, Durability => Durability)
         else Cargo_Index);
   begin
      if Amount > 0 then
         if Item_Index = 0 then
            Sky_Bases(Base_Index).Cargo.Append
              (New_Item =>
                 (Proto_Index => Proto_Index, Amount => Amount,
                  Durability => Durability,
                  Price =>
                    Get_Price(BaseType => Sky_Bases(Base_Index).Base_Type, ItemIndex => Proto_Index)));
         else
            Sky_Bases(Base_Index).Cargo(Item_Index).Amount :=
              Sky_Bases(Base_Index).Cargo(Item_Index).Amount + Amount;
         end if;
      else
         Sky_Bases(Base_Index).Cargo(Item_Index).Amount :=
           Sky_Bases(Base_Index).Cargo(Item_Index).Amount + Amount;
         if Sky_Bases(Base_Index).Cargo(Item_Index).Amount = 0 and
           not Is_Buyable
             (BaseType => Sky_Bases(Base_Index).Base_Type,
              ItemIndex => Sky_Bases(Base_Index).Cargo(Item_Index).Proto_Index) and
           Item_Index > 1 then
            Sky_Bases(Base_Index).Cargo.Delete(Index => Item_Index);
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
