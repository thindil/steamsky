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

with BasesTypes;
with Maps; use Maps;
with Trades;

package body Bases.Cargo is

   procedure Generate_Cargo is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Generate_Ada_Cargo with
         Import => True,
         Convention => C,
         External_Name => "generateAdaCargo";
   begin
      Get_Ada_Base_Visited_Date
        (Base_Index => Base_Index, Year => Sky_Bases(Base_Index).Visited.Year,
         Month => Sky_Bases(Base_Index).Visited.Month,
         Day => Sky_Bases(Base_Index).Visited.Day,
         Hour => Sky_Bases(Base_Index).Visited.Hour,
         Minutes => Sky_Bases(Base_Index).Visited.Minutes);
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Ada_Base_Population
        (Base_Index => Base_Index,
         Population => Sky_Bases(Base_Index).Population);
      Get_Base_Reputation(Base_Index => Base_Index);
      Get_Base_Type(Base_Index => Base_Index, Base_Type => Sky_Bases(Base_Index).Base_Type);
      Generate_Ada_Cargo;
      Set_Base_Cargo(Base_Index => Base_Index);
   end Generate_Cargo;

   procedure Update_Base_Cargo
     (Proto_Index: Natural := 0; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index: Inventory_Container.Extended_Index := 0) is
      use BasesTypes;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Item_Index: constant Natural range 0 ..
          Natural
            (BaseCargo_Container.Length
               (Container => Sky_Bases(Base_Index).Cargo)) :=
        (if Proto_Index > 0 then
           Find_Base_Cargo
             (Proto_Index => Proto_Index, Durability => Durability)
         else Cargo_Index);
      Item: Base_Cargo := Empty_Base_Cargo;
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
            --## rule off ASSIGNMENTS
            Item :=
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Item_Index);
            Item.Amount := Item.Amount + Amount;
            --## rule on ASSIGNMENTS
            BaseCargo_Container.Replace_Element
              (Container => Sky_Bases(Base_Index).Cargo, Index => Item_Index,
               New_Item => Item);
         end if;
      else
         --## rule off ASSIGNMENTS
         Item :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo, Index => Item_Index);
         Item.Amount := Item.Amount + Amount;
         --## rule on ASSIGNMENTS
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
     (Proto_Index: Natural;
      Durability: Items_Durability := Items_Durability'Last) return Natural is
      use Trades;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      function Find_Cargo
        (Local_Base_Cargo: BaseCargo_Container.Vector) return Natural is
      begin
         Find_Cargo_Loop :
         for I in
           BaseCargo_Container.First_Index(Container => Local_Base_Cargo) ..
             BaseCargo_Container.Last_Index(Container => Local_Base_Cargo) loop
            if Durability < Items_Durability'Last then
               if BaseCargo_Container.Element
                   (Container => Local_Base_Cargo, Index => I)
                   .Proto_Index =
                 Proto_Index and
                 BaseCargo_Container.Element
                     (Container => Local_Base_Cargo, Index => I)
                     .Durability =
                   Durability then
                  return I;
               end if;
            else
               if BaseCargo_Container.Element
                   (Container => Local_Base_Cargo, Index => I)
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
         return Find_Cargo(Local_Base_Cargo => Sky_Bases(Base_Index).Cargo);
      end if;
      return Find_Cargo(Local_Base_Cargo => Trader_Cargo);
   end Find_Base_Cargo;

end Bases.Cargo;
