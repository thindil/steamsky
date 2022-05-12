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

with Utils; use Utils;
with Trades; use Trades;
with BasesTypes; use BasesTypes;
with Maps; use Maps;

package body Bases.Cargo is

   procedure Generate_Cargo is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
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
      if BaseCargo_Container.Length(Container => Sky_Bases(Base_Index).Cargo) =
        0 then
         Chance := 101;
      end if;
      if Get_Random(Min => 1, Max => 100) > Chance then
         return;
      end if;
      if BaseCargo_Container.Length(Container => Sky_Bases(Base_Index).Cargo) =
        0 then
         BaseCargo_Container.Append
           (Container => Sky_Bases(Base_Index).Cargo,
            New_Item =>
              (Proto_Index => Money_Index,
               Amount => (Get_Random(Min => 50, Max => 200) * Population),
               Durability => Default_Item_Durability, Price => 0));
         Add_Base_Cargo_Loop :
         for I in Items_List.Iterate loop
            if Is_Buyable
                (Base_Type => Sky_Bases(Base_Index).Base_Type,
                 Item_Index => Objects_Container.To_Index(Position => I),
                 Check_Flag => False) then
               BaseCargo_Container.Append
                 (Container => Sky_Bases(Base_Index).Cargo,
                  New_Item =>
                    (Proto_Index => Objects_Container.Key(Position => I),
                     Amount => (Get_Random(Min => 0, Max => 100) * Population),
                     Durability => Default_Item_Durability,
                     Price =>
                       Get_Price
                         (Base_Type => Sky_Bases(Base_Index).Base_Type,
                          Item_Index =>
                            Objects_Container.Key(Position => I))));
            end if;
         end loop Add_Base_Cargo_Loop;
         if Bases_Types_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
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
                            (Base_Type => Sky_Bases(Base_Index).Base_Type,
                             Item_Index =>
                               Objects_Container.Key(Position => J)) =
                          0 then
                           Item_Index := Item_Index + 1;
                        else
                           BaseCargo_Container.Append
                             (Container => Sky_Bases(Base_Index).Cargo,
                              New_Item =>
                                (Proto_Index =>
                                   Objects_Container.Key(Position => J),
                                 Amount =>
                                   (Get_Random(Min => 0, Max => 100) *
                                    Population),
                                 Durability => Default_Item_Durability,
                                 Price =>
                                   Get_Price
                                     (Base_Type =>
                                        Sky_Bases(Base_Index).Base_Type,
                                      Item_Index =>
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
            Item: Base_Cargo;
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
            for I in
              BaseCargo_Container.First_Index
                (Container => Sky_Bases(Base_Index).Cargo) ..
                BaseCargo_Container.Last_Index
                  (Container => Sky_Bases(Base_Index).Cargo) loop
               Roll := Get_Random(Min => 1, Max => 100);
               Item :=
                 BaseCargo_Container.Element
                   (Container => Sky_Bases(Base_Index).Cargo, Index => I);
               if Roll < 30 and Item.Amount > 0 then
                  Item.Amount :=
                    Item.Amount -
                    Get_Random
                      (Min => 1, Max => Get_Max_Amount(Amount => Item.Amount));
               elsif Roll < 60 and Sky_Bases(Base_Index).Population > 0 then
                  Item.Amount :=
                    (if Item.Amount = 0 then
                       Get_Random(Min => 1, Max => 10) * Population
                     else Item.Amount +
                       Get_Random
                         (Min => 1,
                          Max => Get_Max_Amount(Amount => Item.Amount)));
               end if;
               BaseCargo_Container.Replace_Element
                 (Container => Sky_Bases(Base_Index).Cargo, Index => I,
                  New_Item => Item);
            end loop Update_Cargo_Loop;
         end Update_Cargo_Block;
      end if;
   end Generate_Cargo;

   procedure Update_Base_Cargo
     (Proto_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Amount: Integer; Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index: Inventory_Container.Extended_Index := 0) is
      use Tiny_String;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Item_Index: constant Natural range 0 ..
          Natural
            (BaseCargo_Container.Length
               (Container => Sky_Bases(Base_Index).Cargo)) :=
        (if Proto_Index /= Null_Bounded_String then
           Find_Base_Cargo
             (Proto_Index => Proto_Index, Durability => Durability)
         else Cargo_Index);
      Item: Base_Cargo;
   begin
      if Amount > 0 then
         if Item_Index = 0 then
            BaseCargo_Container.Append
              (Container => Sky_Bases(Base_Index).Cargo,
               New_Item =>
                 (Proto_Index => Proto_Index, Amount => Amount,
                  Durability => Durability,
                  Price =>
                    Get_Price
                      (Base_Type => Sky_Bases(Base_Index).Base_Type,
                       Item_Index => Proto_Index)));
         else
            Item :=
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Item_Index);
            Item.Amount := Item.Amount + Amount;
            BaseCargo_Container.Replace_Element
              (Container => Sky_Bases(Base_Index).Cargo, Index => Item_Index,
               New_Item => Item);
         end if;
      else
         Item :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo, Index => Item_Index);
         Item.Amount := Item.Amount + Amount;
         if Item.Amount = 0 and
           not Is_Buyable
             (Base_Type => Sky_Bases(Base_Index).Base_Type,
              Item_Index => Item.Proto_Index) and
           Item_Index > 1 then
            BaseCargo_Container.Delete
              (Container => Sky_Bases(Base_Index).Cargo, Index => Item_Index);
         else
            BaseCargo_Container.Replace_Element
              (Container => Sky_Bases(Base_Index).Cargo, Index => Item_Index,
               New_Item => Item);
         end if;
      end if;
   end Update_Base_Cargo;

   function Find_Base_Cargo
     (Proto_Index: Tiny_String.Bounded_String;
      Durability: Items_Durability := Items_Durability'Last) return Natural is
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      function Find_Cargo(Cargo: BaseCargo_Container.Vector) return Natural is
         use Tiny_String;
      begin
         Find_Cargo_Loop :
         for I in
           BaseCargo_Container.First_Index(Container => Cargo) ..
             BaseCargo_Container.Last_Index(Container => Cargo) loop
            if Durability < Items_Durability'Last then
               if BaseCargo_Container.Element(Container => Cargo, Index => I)
                   .Proto_Index =
                 Proto_Index and
                 BaseCargo_Container.Element(Container => Cargo, Index => I)
                     .Durability =
                   Durability then
                  return I;
               end if;
            else
               if BaseCargo_Container.Element(Container => Cargo, Index => I)
                   .Proto_Index =
                 Proto_Index then
                  return I;
               end if;
            end if;
         end loop Find_Cargo_Loop;
         return 0;
      end Find_Cargo;
   begin
      if Base_Index > 0 then
         return Find_Cargo(Cargo => Sky_Bases(Base_Index).Cargo);
      end if;
      return Find_Cargo(Cargo => TraderCargo);
   end Find_Base_Cargo;

end Bases.Cargo;
