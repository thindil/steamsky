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
with ShipModules; use ShipModules;

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
            DrawGame(Repairs_View);
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

    procedure UpgradeShip(Install : Boolean; ModuleIndex : Positive) is
        MoneyIndex : constant Natural := FindMoney;
        HullIndex, ModulesAmount : Positive;
        ArmorIndex, FreeTurretIndex, CockpitIndex : Natural := 0;
    begin
        if MoneyIndex = 0 then
            ShowDialog("You don't have Charcollum to pay for modules.");
            return;
        end if;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            case Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType is
                when HULL =>
                    HullIndex := I;
                    ModulesAmount := PlayerShip.Modules.Element(I).Current_Value;
                when ARMOR =>
                    ArmorIndex := I;
                when COCKPIT =>
                    CockpitIndex := I;
                when TURRET =>
                    if PlayerShip.Modules.Element(I).Current_Value = 0 then
                        FreeTurretIndex := I;
                    end if;
                when others =>
                    null;
            end case;
        end loop;
        if Install then
            if PlayerShip.Cargo.Element(MoneyIndex).Amount < Modules_List.Element(ModuleIndex).Price then
                ShowDialog("You don't have enough Charcollum to pay for " & To_String(Modules_List.Element(ModuleIndex).Name) & ".");
                return;
            end if;
            if Modules_List.Element(ModuleIndex).MType /= HULL then
                if ModulesAmount = PlayerShip.Modules.Element(HullIndex).Max_Value and
                    Modules_List.Element(ModuleIndex).MType /= GUN then
                    ShowDialog("You don't have free space for more modules.");
                    return;
                end if;
                case Modules_List.Element(ModuleIndex).MType is
                    when ARMOR =>
                        if ArmorIndex > 0 then
                            ShowDialog("You have installed armor now. Remove it first, before install new.");
                            return;
                        end if;
                    when COCKPIT =>
                        if CockpitIndex > 0 then
                            ShowDialog("You can have only one cockpit on ship. Remove old first, if you want install new.");
                            return;
                        end if;
                    when GUN =>
                        if FreeTurretIndex = 0 then
                            ShowDialog("You don't have free turret for next gun. Install new turret or remove old gun first.");
                            return;
                        end if;
                    when others =>
                        null;
                end case;
                if Modules_List.Element(ModuleIndex).MType /= ARMOR then
                    ModulesAmount := ModulesAmount + 1;
                end if;
            else
                if PlayerShip.Modules.Element(HullIndex).Current_Value > Modules_List(ModuleIndex).MaxValue then
                    ShowDialog("This hull is too small for your ship. Remove some modules first.");
                    return;
                end if;
                PlayerShip.Modules.Delete(HullIndex, 1);
            end if;
            UpdateGame(Modules_List.Element(ModuleIndex).InstallTime);
            UpdateCargo(1, (0 - Modules_List.Element(ModuleIndex).Price));
            PlayerShip.Modules.Append(New_Item => (Name =>  Modules_List.Element(ModuleIndex).Name,
                ProtoIndex => ModuleIndex, 
                Weight => Modules_List.Element(ModuleIndex).Weight,
                Current_Value => Modules_List.Element(ModuleIndex).Value,
                Max_Value => Modules_List.Element(ModuleIndex).MaxValue,
                Durability => Modules_List.Element(ModuleIndex).Durability,
                MaxDurability => Modules_List.Element(ModuleIndex).Durability,
                Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE));
            case Modules_List.Element(ModuleIndex).MType is
                when GUN =>
                    UpdateModule(PlayerShip, FreeTurretIndex, "Current_Value", Positive'Image(PlayerShip.Modules.Last_Index));
                when HULL =>
                    UpdateModule(PlayerShip, PlayerShip.Modules.Last_Index, "Current_Value", Positive'Image(ModulesAmount));
                when others =>
                    UpdateModule(PlayerShip, HullIndex, "Current_Value", Positive'Image(ModulesAmount));
            end case;
            AddMessage("You installed " & To_String(Modules_List.Element(ModuleIndex).Name) & " on your ship for" &
                Positive'Image(Modules_List.Element(ModuleIndex).Price) & " Charcollum.", TradeMessage);
        else
            if FreeCargo((0 - Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price)) < 0 then
                ShowDialog("You don't have enough free space for Charcollum in ship cargo.");
                return;
            end if;
            case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                when TURRET =>
                    if PlayerShip.Modules.Element(ModuleIndex).Current_Value > 0 then
                        ShowDialog("You have installed gun in this turret, remove it before you remove this turret.");
                        return;
                    end if;
                when GUN =>
                    for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                        if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = TURRET and
                            PlayerShip.Modules.Element(I).Current_Value = ModuleIndex then
                            UpdateModule(PlayerShip, I, "Current_Value", "0");
                            exit;
                        end if;
                    end loop;
                when CARGO =>
                    if FreeCargo((0 - PlayerShip.Modules.Element(ModuleIndex).Max_Value)) < 0 then
                        ShowDialog("You can't sell this cargo bay, because you have items in it.");
                        return;
                    end if;
                when others =>
                    null;
            end case;
            if Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType /= GUN and
                Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType /= ARMOR then
                ModulesAmount := ModulesAmount - 1;
            end if;
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = HULL then
                    UpdateModule(PlayerShip, I, "Current_Value", Positive'Image(ModulesAmount));
                    exit;
                end if;
            end loop;
            if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                GiveOrders(PlayerShip.Modules.Element(ModuleIndex).Owner, Rest);
            end if;
            UpdateGame(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).InstallTime);
            UpdateCargo(1, Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price);
            AddMessage("You removed " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & " from your ship and earned" &
                Positive'Image(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price) & " Charcollum.", 
                TradeMessage);
            PlayerShip.Modules.Delete(ModuleIndex, 1);
        end if;
    end UpgradeShip;

end Bases;
