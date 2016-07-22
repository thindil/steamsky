--    Copyright 2016 Bartek thindil Jasicki
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

with Ships; use Ships;
with Maps; use Maps;
with UserInterface; use UserInterface;

package body Bases is

    procedure BuyItems(ItemIndex : Positive; Amount : String) is
        BuyAmount : Positive;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Cost : Positive;
        MoneyIndex, CargoItemIndex : Natural := 0;
        NewAmount, NewWeight : Natural;
        procedure UpdateCargo(Item : in out CargoData) is
        begin
            Item.Amount := NewAmount;
            Item.Weight := NewWeight;
        end UpdateCargo;
    begin
        BuyAmount := Positive'Value(Amount);
        if not SkyBases(BaseIndex).Goods(ItemIndex).Buyable then
            return;
        end if;
        Cost := BuyAmount * SkyBases(BaseIndex).Goods(ItemIndex).Price;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).Name = "Charcollum" then
                MoneyIndex := I;
                exit;
            end if;
        end loop;
        if MoneyIndex = 0 then
            return;
        end if;
        if Cost > PlayerShip.Cargo.Element(MoneyIndex).Amount then
            return;
        end if;
        -- Update amount of "moneys" (charcollum)
        NewAmount := PlayerShip.Cargo.Element(MoneyIndex).Amount - Cost;
        NewWeight := PlayerShip.Cargo.Element(MoneyIndex).Weight - Cost;
        if NewAmount = 0 then
            PlayerShip.Cargo.Delete(Index => MoneyIndex, Count => 1);
        else
            PlayerShip.Cargo.Update_Element(Index => MoneyIndex, Process => UpdateCargo'Access);
        end if;
        -- Add/update cargo
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).Name = SkyBases(BaseIndex).Goods(ItemIndex).Name then
                CargoItemIndex := I;
                exit;
            end if;
        end loop;
        NewWeight := PlayerShip.Cargo.Element(CargoItemIndex).Weight + (BuyAmount * SkyBases(BaseIndex).Goods(ItemIndex).Weight);
        if CargoItemIndex > 0 then
            NewAmount := PlayerShip.Cargo.Element(CargoItemIndex).Amount + BuyAmount;
            PlayerShip.Cargo.Update_Element(Index => CargoItemIndex, Process => UpdateCargo'Access);
        else
            PlayerShip.Cargo.Append(New_Item => (Name => SkyBases(BaseIndex).Goods(ItemIndex).Name, 
                Weight => NewWeight, Amount => BuyAmount));
        end if;
        AddMessage("You bought" & Positive'Image(BuyAmount) & " " & To_String(SkyBases(BaseIndex).Goods(ItemIndex).Name) &
            " for" & Positive'Image(Cost) & " Charcollum.");
    exception
        when others =>
            return;
    end BuyItems;

    procedure SellItems(ItemIndex : Positive; Amount : String) is
        SellAmount : Positive;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Profit : Positive;
        MoneyIndex, BaseItemIndex : Natural := 0;
        NewAmount, NewWeight : Natural;
        procedure UpdateCargo(Item : in out CargoData) is
        begin
            Item.Amount := NewAmount;
            Item.Weight := NewWeight;
        end UpdateCargo;
    begin
        SellAmount := Positive'Value(Amount);
        if PlayerShip.Cargo.Element(ItemIndex).Amount < SellAmount then
            return;
        end if;
        for I in SkyBases(BaseIndex).Goods'Range loop
            if SkyBases(BaseIndex).Goods(I).Name = PlayerShip.Cargo.Element(ItemIndex).Name then
                BaseItemIndex := I;
                exit;
            end if;
        end loop;
        -- Update amount of items
        NewAmount := PlayerShip.Cargo.Element(ItemIndex).Amount - SellAmount;
        if NewAmount = 0 then
            PlayerShip.Cargo.Delete(Index => ItemIndex, Count => 1);
        else
            NewWeight := PlayerShip.Cargo.Element(ItemIndex).Amount * SkyBases(BaseIndex).Goods(BaseItemIndex).Weight;
            PlayerShip.Cargo.Update_Element(Index => ItemIndex, Process => UpdateCargo'Access);
        end if;
        -- Update amount of "moneys" charcollum
        Profit := SkyBases(BaseIndex).Goods(BaseItemIndex).Price * SellAmount;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).Name = "Charcollum" then
                MoneyIndex := I;
                exit;
            end if;
        end loop;
        if MoneyIndex = 0 then
            PlayerShip.Cargo.Append(New_Item => (Name => To_Unbounded_String("Charcollum"),
                Weight => Profit, Amount => Profit));
        else
            NewAmount := PlayerShip.Cargo.Element(MoneyIndex).Amount + Profit;
            NewWeight := PlayerShip.Cargo.Element(MoneyIndex).Weight + Profit;
            PlayerShip.Cargo.Update_Element(Index => MoneyIndex, Process => UpdateCargo'Access);
        end if;
        AddMessage("You sold" & Positive'Image(SellAmount) & " " & To_String(SkyBases(BaseIndex).Goods(BaseItemIndex).Name) &
            " for" & Positive'Image(Profit) & " Charcollum.");
    exception
        when others =>
            return;
    end SellItems;

end Bases;
