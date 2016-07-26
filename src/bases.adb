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
with Messages; use Messages;
with Prototypes; use Prototypes;
with UserInterface; use UserInterface;

package body Bases is

    procedure BuyItems(ItemIndex : Positive; Amount : String) is
        BuyAmount : Positive;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Cost : Positive;
        MoneyIndex : Natural := 0;
    begin
        BuyAmount := Positive'Value(Amount);
        if not SkyBases(BaseIndex).Goods(ItemIndex).Buyable then
            return;
        end if;
        Cost := BuyAmount * SkyBases(BaseIndex).Goods(ItemIndex).Price;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
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
        UpdateCargo(1, (0 - Cost));
        UpdateCargo(SkyBases(BaseIndex).Goods(ItemIndex).ProtoIndex, BuyAmount);
        AddMessage("You bought" & Positive'Image(BuyAmount) & " " &
            To_String(Objects_Prototypes(SkyBases(BaseIndex).Goods(ItemIndex).ProtoIndex).Name) &
            " for" & Positive'Image(Cost) & " Charcollum.");
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            return;
    end BuyItems;

    procedure SellItems(ItemIndex : Positive; Amount : String) is
        SellAmount : Positive;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Profit : Positive;
        BaseItemIndex : Natural := 0;
    begin
        SellAmount := Positive'Value(Amount);
        if PlayerShip.Cargo.Element(ItemIndex).Amount < SellAmount then
            return;
        end if;
        for I in SkyBases(BaseIndex).Goods'Range loop
            if SkyBases(BaseIndex).Goods(I).ProtoIndex = PlayerShip.Cargo.Element(ItemIndex).ProtoIndex then
                BaseItemIndex := I;
                exit;
            end if;
        end loop;
        UpdateCargo(SkyBases(BaseIndex).Goods(BaseItemIndex).ProtoIndex, (0 -
            SellAmount));
        Profit := SkyBases(BaseIndex).Goods(BaseItemIndex).Price * SellAmount;
        UpdateCargo(1, Profit);
        AddMessage("You sold" & Positive'Image(SellAmount) & " " &
            To_String(Objects_Prototypes(SkyBases(BaseIndex).Goods(BaseItemIndex).ProtoIndex).Name) &
            " for" & Positive'Image(Profit) & " Charcollum.");
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            return;
    end SellItems;

    procedure ShowTrade(Key : Key_Code) is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        BuyLetter, SellLetter : Character;
        BuyLetters : array (SkyBases(BaseIndex).Goods'Range) of Character;
        SellLetters : array (1..PlayerShip.Cargo.Last_Index) of Character := (others => ' ');
        Visibility : Cursor_Visibility := Normal;
        Amount : String(1..6);
        ItemIndex : Natural := 0;
    begin
        if Key /= KEY_NONE then
            Erase;
            Refresh;
            ShowGameMenu(Trade_View);
        end if;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "BUY SELL");
        for I in SkyBases(BaseIndex).Goods'Range loop
            if SkyBases(BaseIndex).Goods(I).Buyable then
                BuyLetter := Character'Val(96 + I);
            else
                BuyLetter := ' ';
            end if;
            BuyLetters(I) := BuyLetter;
            SellLetter := ' ';
            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(J).ProtoIndex = SkyBases(BaseIndex).Goods(I).ProtoIndex then
                    SellLetter := Character'Val(64 + I);
                    SellLetters(J) := SellLetter;
                    exit;
                end if;
            end loop;
            Move_Cursor(Line => Line_Position(2 + I), Column => 3);
            Add(Str => BuyLetter & "   " & SellLetter & "   " &
                To_String(Objects_Prototypes(SkyBases(BaseIndex).Goods(I).ProtoIndex).Name) & " Price:" &
                Positive'Image(SkyBases(BaseIndex).Goods(I).Price) & 
                " charcollum");
        end loop;
        if Key /= KEY_NONE then -- start buying/selling items from/to base
            for I in BuyLetters'Range loop
                if Key = Character'Pos(BuyLetters(I)) and BuyLetters(I) /= ' ' then
                    ItemIndex := I;
                    exit;
                end if;
            end loop;
            if ItemIndex > 0 then -- Buy item from base
                Set_Echo_Mode(True);
                Set_Cursor_Visibility(Visibility);
                Move_Cursor(Line => (Lines / 2), Column => 2);
                Add(Str => "Enter amount to buy: ");
                Get(Str => Amount, Len => 6);
                BuyItems(ItemIndex, Amount);
                ItemIndex := 0;
            else
                for I in SellLetters'Range loop
                    if Key = Character'Pos(SellLetters(I)) and SellLetters(I) /= ' ' then
                        ItemIndex := I;
                        exit;
                    end if;
                end loop;
                if ItemIndex > 0 then -- Sell item to base
                    Set_Echo_Mode(True);
                    Set_Cursor_Visibility(Visibility);
                    Move_Cursor(Line => (Lines / 2), Column => 2);
                    Add(Str => "Enter amount to sell: ");
                    Get(Str => Amount, Len => 6);
                    SellItems(ItemIndex, Amount);
                    ItemIndex := 0;
                end if;
            end if;
            if ItemIndex = 0 then
                Visibility := Invisible;
                Set_Echo_Mode(False);
                Set_Cursor_Visibility(Visibility);
                DrawGame(Trade_View);
            end if;
        end if;
    end ShowTrade;

    function TradeKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                ShowTrade(Key);
                return Trade_View;
        end case;
    end TradeKeys;

end Bases;
