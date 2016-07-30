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
with Crew; use Crew;

package body Bases is

    procedure BuyItems(ItemIndex : Positive; Amount : String) is
        BuyAmount : Positive;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        ItemName : constant String := To_String(Objects_Prototypes(SkyBases(BaseIndex).Goods(ItemIndex).ProtoIndex).Name);
        Cost : Positive;
        MoneyIndex : Natural := 0;
        FreeCargo : Integer := 0;
    begin
        BuyAmount := Positive'Value(Amount);
        if not SkyBases(BaseIndex).Goods(ItemIndex).Buyable then
            ShowDialog("You can't buy " & ItemName & " in this base.");
            return;
        end if;
        Cost := BuyAmount * SkyBases(BaseIndex).Goods(ItemIndex).Price;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Mtype = CARGO then
                FreeCargo := FreeCargo + PlayerShip.Modules.Element(I).Max_Value;
            end if;
        end loop;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            FreeCargo := FreeCargo - (Objects_Prototypes(PlayerShip.Cargo.Element(I).ProtoIndex).Weight * 
                PlayerShip.Cargo.Element(I).Amount);
            if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
                MoneyIndex := I;
            end if;
        end loop;
        FreeCargo := FreeCargo - (Objects_Prototypes(SkyBases(BaseIndex).Goods(ItemIndex).ProtoIndex).Weight *
            BuyAmount); 
        FreeCargo := FreeCargo + Cost;
        if FreeCargo < 0 then
            ShowDialog("You don't have that much free space in your ship cargo.");
            return;
        end if;
        if MoneyIndex = 0 then
            ShowDialog("You don't have charcollum to buy " & ItemName & ".");
            return;
        end if;
        if Cost > PlayerShip.Cargo.Element(MoneyIndex).Amount then
            ShowDialog("You don't have enough charcollum to buy so much " & ItemName & ".");
            return;
        end if;
        UpdateCargo(1, (0 - Cost));
        UpdateCargo(SkyBases(BaseIndex).Goods(ItemIndex).ProtoIndex, BuyAmount);
        GainExp(1, 4, 1);
        AddMessage("You bought" & Positive'Image(BuyAmount) & " " & ItemName & " for" & Positive'Image(Cost) & " Charcollum.");
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            ShowDialog("You must enter number as an amount to buy.");
    end BuyItems;

    procedure SellItems(ItemIndex : Positive; Amount : String) is
        SellAmount : Positive;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        ItemName : constant String := To_String(Objects_Prototypes(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name);
        Profit : Positive;
        BaseItemIndex : Natural := 0;
        FreeCargo : Integer := 0;
    begin
        SellAmount := Positive'Value(Amount);
        if PlayerShip.Cargo.Element(ItemIndex).Amount < SellAmount then
            ShowDialog("You dont have that much " & ItemName & " in ship cargo.");
            return;
        end if;
        for I in SkyBases(BaseIndex).Goods'Range loop
            if SkyBases(BaseIndex).Goods(I).ProtoIndex = PlayerShip.Cargo.Element(ItemIndex).ProtoIndex then
                BaseItemIndex := I;
                exit;
            end if;
        end loop;
        Profit := SkyBases(BaseIndex).Goods(BaseItemIndex).Price * SellAmount;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Mtype = CARGO then
                FreeCargo := FreeCargo + PlayerShip.Modules.Element(I).Max_Value;
            end if;
        end loop;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            FreeCargo := FreeCargo - (Objects_Prototypes(PlayerShip.Cargo.Element(I).ProtoIndex).Weight * 
                PlayerShip.Cargo.Element(I).Amount);
        end loop;
        FreeCargo := FreeCargo + (Objects_Prototypes(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Weight *
            SellAmount);
        FreeCargo := FreeCargo - Profit;
        if FreeCargo < 0 then
            ShowDialog("You don't have enough free cargo space in your ship for Charcollum.");
            return;
        end if;
        UpdateCargo(SkyBases(BaseIndex).Goods(BaseItemIndex).ProtoIndex, (0 - SellAmount));
        UpdateCargo(1, Profit);
        GainExp(1, 4, 1);
        AddMessage("You sold" & Positive'Image(SellAmount) & " " & ItemName & " for" & Positive'Image(Profit) & " Charcollum.");
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            ShowDialog("You must enter number as an amount to sell.");
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
            if BuyLetter /= ' ' then
                Change_Attributes(Line => Line_Position(2 + I), Column => 3, Count => 1, Color => 1);
            end if;
            if SellLetter /= ' ' then
                Change_Attributes(Line => Line_Position(2 + I), Column => 7, Count => 1, Color => 1);
            end if;
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
                Add(Str => "Enter amount of " &
                    To_String(Objects_Prototypes(SkyBases(BaseIndex).Goods(ItemIndex).ProtoIndex).Name)
                    & " to buy: ");
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
                    Add(Str => "Enter amount of " &
                        To_String(Objects_Prototypes(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name)
                        & " to sell: ");
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
