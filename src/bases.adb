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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ships; use Ships;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with UserInterface; use UserInterface;
with Crew; use Crew;
with Bases.UI; use Bases.UI;

package body Bases is
    
    procedure BuyItems(ItemIndex : Positive; Amount : String) is
        BuyAmount : Positive;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        ItemName : constant String := To_String(Items_List.Element(ItemIndex).Name);
        Cost : Positive;
        MoneyIndex : Natural;
    begin
        BuyAmount := Positive'Value(Amount);
        if not Items_List.Element(ItemIndex).Buyable(BaseType) then
            ShowDialog("You can't buy " & ItemName & " in this base.");
            return;
        end if;
        Cost := BuyAmount * Items_List.Element(ItemIndex).Prices(BaseType);
        Cost := Cost - Integer(Float'Floor(Float(Cost) *
                (Float(PlayerShip.Crew.Element(1).Skills(4, 1)) / 200.0)));
        MoneyIndex := FindMoney;
        if FreeCargo(Cost - (Items_List.Element(ItemIndex).Weight * BuyAmount)) < 0 then
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
        UpdateCargo(ItemIndex, BuyAmount);
        GainExp(1, 4, 1);
        AddMessage("You bought" & Positive'Image(BuyAmount) & " " & ItemName &
            " for" & Positive'Image(Cost) & " Charcollum.", TradeMessage);
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            ShowDialog("You must enter number as an amount to buy.");
    end BuyItems;

    procedure SellItems(ItemIndex : Positive; Amount : String) is
        SellAmount : Positive;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        ProtoIndex : constant Positive := PlayerShip.Cargo.Element(ItemIndex).ProtoIndex;
        ItemName : constant String := To_String(Items_List.Element(ProtoIndex).Name);
        Profit : Positive;
    begin
        SellAmount := Positive'Value(Amount);
        if PlayerShip.Cargo.Element(ItemIndex).Amount < SellAmount then
            ShowDialog("You dont have that much " & ItemName & " in ship cargo.");
            return;
        end if;
        Profit := Items_List.Element(ProtoIndex).Prices(BaseType) * SellAmount;
        Profit := Profit + Integer(Float'Floor(Float(Profit) *
                (Float(PlayerShip.Crew.Element(1).Skills(4, 1)) / 200.0)));
        if FreeCargo((Items_List.Element(ProtoIndex).Weight * SellAmount) - Profit) < 0 then
            ShowDialog("You don't have enough free cargo space in your ship for Charcollum.");
            return;
        end if;
        UpdateCargo(ProtoIndex, (0 - SellAmount));
        UpdateCargo(1, Profit);
        GainExp(1, 4, 1);
        AddMessage("You sold" & Positive'Image(SellAmount) & " " & ItemName & " for" & 
            Positive'Image(Profit) & " Charcollum.", TradeMessage);
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            ShowDialog("You must enter number as an amount to sell.");
    end SellItems;

    function GenerateBaseName return Unbounded_String is -- based on name generator from libtcod
        subtype PreSyllables_Range is Positive range BaseSyllablesPre.First_Index..BaseSyllablesPre.Last_Index;
        subtype StartSyllables_Range is Positive range BaseSyllablesStart.First_Index..BaseSyllablesStart.Last_Index;
        subtype EndSyllables_Range is Positive range BaseSyllablesEnd.First_Index..BaseSyllablesEnd.Last_Index;
        subtype PostSyllables_Range is Positive range BaseSyllablesPost.First_Index..BaseSyllablesPost.Last_Index;
        type Percent_Range is range 1..100;
        package Rand_PreSyllable is new Discrete_Random(PreSyllables_Range);
        package Rand_StartSyllable is new Discrete_Random(StartSyllables_Range);
        package Rand_EndSyllable is new Discrete_Random(EndSyllables_Range);
        package Rand_PostSyllable is new Discrete_Random(PostSyllables_Range);
        package Rand_Percent is new Discrete_Random(Percent_Range);
        Generator : Rand_PreSyllable.Generator;
        Generator2 : Rand_StartSyllable.Generator;
        Generator3 : Rand_EndSyllable.Generator;
        Generator4 : Rand_PostSyllable.Generator;
        Generator5 : Rand_Percent.Generator;
        NewName : Unbounded_String;
    begin
        Rand_PreSyllable.Reset(Generator);
        Rand_StartSyllable.Reset(Generator2);
        Rand_EndSyllable.Reset(Generator3);
        Rand_PostSyllable.Reset(Generator4);
        Rand_Percent.Reset(Generator5);
        NewName := Null_Unbounded_String;
        if Rand_Percent.Random(Generator5) < 16 then
            NewName := BaseSyllablesPre(Rand_PreSyllable.Random(Generator)) & " ";
        end if;
        NewName := NewName & BaseSyllablesStart.Element(Rand_StartSyllable.Random(Generator2)) & 
            BaseSyllablesEnd(Rand_EndSyllable.Random(Generator3));
        if Rand_Percent.Random(Generator5) < 16 then
            NewName := NewName & " " & BaseSyllablesPost(Rand_PostSyllable.Random(Generator4));
        end if;
        return NewName;
    end GenerateBaseName;

    procedure RepairShip is
        Cost, Time, ModuleIndex, MoneyIndex, RepairValue : Natural := 0;
    begin
        RepairCost(Cost, Time, ModuleIndex);
        if Cost = 0 then
            return;
        end if;
        MoneyIndex := FindMoney;
        if MoneyIndex = 0 then
            ShowDialog("You don't have Charcollum to pay for repairs.");
            DrawGame(Repairs_View);
            return;
        end if;
        if PlayerShip.Cargo.Element(MoneyIndex).Amount < Cost then
            ShowDialog("You don't have enough Charcollum to pay for repairs.");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Repair then
                GiveOrders(I, Rest);
            end if;
        end loop;
        if ModuleIndex > 0 then
            RepairValue := PlayerShip.Modules.Element(ModuleIndex).MaxDurability - PlayerShip.Modules.Element(ModuleIndex).Durability;
            UpdateModule(PlayerShip, ModuleIndex, "Durability", Positive'Image(RepairValue));
            AddMessage("You bought " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & " repair for" & 
                Positive'Image(Cost) & " Charcollum.", TradeMessage);
        else
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    RepairValue := PlayerShip.Modules.Element(I).MaxDurability - PlayerShip.Modules.Element(I).Durability;
                    UpdateModule(PlayerShip, I, "Durability", Positive'Image(RepairValue));
                end if;
            end loop;
            AddMessage("You bought whole ship repair for" & Positive'Image(Cost) & " Charcollum.", TradeMessage);
        end if;
        UpdateCargo(1, (0 - Cost));
        UpdateGame(Time);
        DrawGame(Repairs_View);
    end RepairShip;

end Bases;
