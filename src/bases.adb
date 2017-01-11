--    Copyright 2016-2017 Bartek thindil Jasicki
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
with Ada.Numerics.Generic_Elementary_Functions;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with UserInterface; use UserInterface;
with Bases.UI.Repair; use Bases.UI.Repair;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Events; use Events;
with Crafts; use Crafts;
with Utils; use Utils;

package body Bases is

    procedure GainRep(BaseIndex : Positive; Points : Integer) is
        NewPoints : Integer;
    begin
        if SkyBases(BaseIndex).Reputation(1) = -100 or SkyBases(BaseIndex).Reputation(1) = 100 then
            return;
        end if;
        NewPoints := SkyBases(BaseIndex).Reputation(2) + Points;
        while NewPoints < 0 loop
            SkyBases(BaseIndex).Reputation(1) := SkyBases(BaseIndex).Reputation(1) - 1;
            NewPoints := NewPoints + abs(SkyBases(BaseIndex).Reputation(1) * 50);
            if NewPoints >= 0 then
                SkyBases(BaseIndex).Reputation(2) := NewPoints;
                return;
            end if;
        end loop;
        while NewPoints > abs(SkyBases(BaseIndex).Reputation(1) * 50) loop
            NewPoints := NewPoints - abs(SkyBases(BaseIndex).Reputation(1) * 50);
            SkyBases(BaseIndex).Reputation(1) := SkyBases(BaseIndex).Reputation(1) + 1;
        end loop;
        SkyBases(BaseIndex).Reputation(2) := NewPoints;
    end GainRep;
    
    procedure BuyItems(ItemIndex : Positive; Amount : String) is
        BuyAmount, TraderIndex : Positive;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        ItemName : constant String := To_String(Items_List.Element(ItemIndex).Name);
        Cost, MoneyIndex : Natural;
    begin
        BuyAmount := Positive'Value(Amount);
        if not Items_List.Element(ItemIndex).Buyable(BaseType) then
            ShowDialog("You can't buy " & ItemName & " in this base.");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Cost := BuyAmount * Items_List.Element(ItemIndex).Prices(BaseType);
        Cost := Cost - Integer(Float'Floor(Float(Cost) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
        case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
            when -24..-1 =>
                Cost := Cost + Integer(Float'Floor(Float(Cost) * 0.05));
            when 26..50 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.05));
            when 51..75 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.1));
            when 76..100 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.15));
            when others =>
                null;
        end case;
        if Cost < 1 then
            Cost := 1;
        end if;
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
        UpdateCargo(PlayerShip, 1, (0 - Cost));
        UpdateCargo(PlayerShip, ItemIndex, BuyAmount);
        GainExp(1, 4, TraderIndex);
        GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
        AddMessage("You bought" & Positive'Image(BuyAmount) & " " & ItemName &
            " for" & Positive'Image(Cost) & " Charcollum.", TradeMessage);
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            return;
    end BuyItems;

    procedure SellItems(ItemIndex : Positive; Amount : String) is
        SellAmount, TraderIndex : Positive;
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
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Profit := Items_List.Element(ProtoIndex).Prices(BaseType) * SellAmount;
        Profit := Profit + Integer(Float'Floor(Float(Profit) * (Float(GetSkillLevel(4, TraderIndex)) / 200.0)));
        case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
            when -24..-1 =>
                Profit := Profit - Integer(Float'Floor(Float(Profit) * 0.05));
            when 26..50 =>
                Profit := Profit + Integer(Float'Floor(Float(Profit) * 0.05));
            when 51..75 =>
                Profit := Profit + Integer(Float'Floor(Float(Profit) * 0.1));
            when 76..100 =>
                Profit := Profit + Integer(Float'Floor(Float(Profit) * 0.15));
            when others =>
                null;
        end case;
        if FreeCargo((Items_List.Element(ProtoIndex).Weight * SellAmount) - Profit) < 0 then
            ShowDialog("You don't have enough free cargo space in your ship for Charcollum.");
            return;
        end if;
        UpdateCargo(PlayerShip, ProtoIndex, (0 - SellAmount));
        UpdateCargo(PlayerShip, 1, Profit);
        GainExp(1, 4, TraderIndex);
        GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
        AddMessage("You sold" & Positive'Image(SellAmount) & " " & ItemName & " for" & 
            Positive'Image(Profit) & " Charcollum.", TradeMessage);
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            return;
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
        TraderIndex : Positive;
    begin
        RepairCost(Cost, Time, ModuleIndex);
        if Cost = 0 then
            return;
        end if;
        MoneyIndex := FindMoney;
        if MoneyIndex = 0 then
            ShowDialog("You don't have Charcollum to pay for repairs.");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Cost := Cost - Integer(Float'Floor(Float(Cost) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
        case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
            when -24..-1 =>
                Cost := Cost + Integer(Float'Floor(Float(Cost) * 0.05));
            when 26..50 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.05));
            when 51..75 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.1));
            when 76..100 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.15));
            when others =>
                null;
        end case;
        if Cost < 1 then
            Cost := 1;
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
        UpdateCargo(PlayerShip, 1, (0 - Cost));
        GainExp(1, 4, TraderIndex);
        GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
        UpdateGame(Time);
    end RepairShip;

    procedure UpgradeShip(Install : Boolean; ModuleIndex : Positive) is
        MoneyIndex : constant Natural := FindMoney;
        HullIndex, ModulesAmount, TraderIndex : Positive;
        FreeTurretIndex, Price : Natural := 0;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
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
                when TURRET =>
                    if PlayerShip.Modules.Element(I).Current_Value = 0 then
                        FreeTurretIndex := I;
                    end if;
                when others =>
                    null;
            end case;
        end loop;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        if Install then
            Price := Modules_List.Element(ModuleIndex).Price;
            Price := Price - Integer(Float'Floor(Float(Price) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
            case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
                when -24..-1 =>
                    Price := Price + Integer(Float'Floor(Float(Price) * 0.05));
                when 26..50 =>
                    Price := Price - Integer(Float'Floor(Float(Price) * 0.05));
                when 51..75 =>
                    Price := Price - Integer(Float'Floor(Float(Price) * 0.1));
                when 76..100 =>
                    Price := Price - Integer(Float'Floor(Float(Price) * 0.15));
                when others =>
                    null;
            end case;
            if Price < 1 then
                Price := 1;
            end if;
            if PlayerShip.Cargo.Element(MoneyIndex).Amount < Price then
                ShowDialog("You don't have enough Charcollum to pay for " & To_String(Modules_List.Element(ModuleIndex).Name) & ".");
                return;
            end if;
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = Modules_List.Element(ModuleIndex).MType and
                    Modules_List.Element(ModuleIndex).Unique then
                    ShowDialog("You can't install another " & To_String(Modules_List.Element(ModuleIndex).Name) & 
                        " because you have installed one module that type. Remove old first.");
                    return;
                end if;
            end loop;
            if Modules_List.Element(ModuleIndex).MType /= HULL then
                ModulesAmount := ModulesAmount + Modules_List(ModuleIndex).Size;
                if ModulesAmount > PlayerShip.Modules.Element(HullIndex).Max_Value and Modules_List.Element(ModuleIndex).MType /= GUN then
                    ShowDialog("You don't have free modules space for more modules.");
                    return;
                end if;
                if Modules_List.Element(ModuleIndex).MType = GUN and FreeTurretIndex = 0 then
                    ShowDialog("You don't have free turret for next gun. Install new turret or remove old gun first.");
                    return;
                end if;
            else
                if Modules_List(ModuleIndex).MaxValue < ModulesAmount then
                    ShowDialog("This hull is too small for your ship. Remove some modules first.");
                    return;
                end if;
                PlayerShip.Modules.Delete(HullIndex, 1);
            end if;
            UpdateGame(Modules_List.Element(ModuleIndex).InstallTime);
            UpdateCargo(PlayerShip, 1, (0 - Price));
            GainExp(1, 4, TraderIndex);
            GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
            if Modules_List.Element(ModuleIndex).MType /= HULL then
                PlayerShip.Modules.Append(New_Item => (Name =>  Modules_List.Element(ModuleIndex).Name,
                    ProtoIndex => ModuleIndex, 
                    Weight => Modules_List.Element(ModuleIndex).Weight,
                    Current_Value => Modules_List.Element(ModuleIndex).Value,
                    Max_Value => Modules_List.Element(ModuleIndex).MaxValue,
                    Durability => Modules_List.Element(ModuleIndex).Durability,
                    MaxDurability => Modules_List.Element(ModuleIndex).Durability,
                    Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE));
            else
                PlayerShip.Modules.Insert(Before => HullIndex, 
                    New_Item => (Name =>  Modules_List.Element(ModuleIndex).Name,
                    ProtoIndex => ModuleIndex, 
                    Weight => Modules_List.Element(ModuleIndex).Weight,
                    Current_Value => Modules_List.Element(ModuleIndex).Value,
                    Max_Value => Modules_List.Element(ModuleIndex).MaxValue,
                    Durability => Modules_List.Element(ModuleIndex).Durability,
                    MaxDurability => Modules_List.Element(ModuleIndex).Durability,
                    Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE));
            end if;
            case Modules_List.Element(ModuleIndex).MType is
                when GUN =>
                    UpdateModule(PlayerShip, FreeTurretIndex, "Current_Value", Positive'Image(PlayerShip.Modules.Last_Index));
                when HULL =>
                    UpdateModule(PlayerShip, PlayerShip.Modules.Last_Index, "Current_Value", Positive'Image(ModulesAmount));
                when others =>
                    UpdateModule(PlayerShip, HullIndex, "Current_Value", Positive'Image(ModulesAmount));
            end case;
            AddMessage("You installed " & To_String(Modules_List.Element(ModuleIndex).Name) & " on your ship for" &
                Positive'Image(Price) & " Charcollum.", TradeMessage);
        else
            Damage := 1.0 - DamageFactor(Float(PlayerShip.Modules.Element(ModuleIndex).Durability) / 
                Float(PlayerShip.Modules.Element(ModuleIndex).MaxDurability));
            Price := Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price -
                Integer(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price) * 
                Float(Damage));
            Price := Price + Integer(Float'Floor(Float(Price) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
            case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
                when -24..-1 =>
                    Price := Price - Integer(Float'Floor(Float(Price) * 0.05));
                when 26..50 =>
                    Price := Price + Integer(Float'Floor(Float(Price) * 0.05));
                when 51..75 =>
                    Price := Price + Integer(Float'Floor(Float(Price) * 0.1));
                when 76..100 =>
                    Price := Price + Integer(Float'Floor(Float(Price) * 0.15));
                when others =>
                    null;
            end case;
            if FreeCargo((0 - Price)) < 0 then
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
            ModulesAmount := ModulesAmount - Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size;
            UpdateModule(PlayerShip, HullIndex, "Current_Value", Positive'Image(ModulesAmount));
            if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                GiveOrders(PlayerShip.Modules.Element(ModuleIndex).Owner, Rest);
            end if;
            if PlayerShip.UpgradeModule = ModuleIndex then
                PlayerShip.UpgradeModule := 0;
                for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(I).Order = Upgrading then
                        GiveOrders(I, Rest);
                    end if;
                end loop;
            end if;
            if PlayerShip.RepairModule = ModuleIndex then
                PlayerShip.RepairModule := 0;
            end if;
            UpdateGame(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).InstallTime);
            UpdateCargo(PlayerShip, 1, Price);
            GainExp(1, 4, TraderIndex);
            GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
            AddMessage("You removed " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & " from your ship and earned" &
                Positive'Image(Price) & " Charcollum.", 
                TradeMessage);
            PlayerShip.Modules.Delete(ModuleIndex, 1);
            if PlayerShip.RepairModule > ModuleIndex then
                PlayerShip.RepairModule := PlayerShip.RepairModule - 1;
            elsif PlayerShip.RepairModule = ModuleIndex then
                PlayerShip.RepairModule := 0;
            end if;
            if PlayerShip.UpgradeModule > ModuleIndex then
                PlayerShip.UpgradeModule := PlayerShip.UpgradeModule - 1;
            elsif PlayerShip.UpgradeModule = ModuleIndex then
                PlayerShip.UpgradeModule := 0;
            end if;
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = TURRET then
                    if PlayerShip.Modules.Element(I).Current_Value > ModuleIndex then
                        UpdateModule(PlayerShip, I, "Current_Value", Positive'Image(PlayerShip.Modules.Element(I).Current_Value - 1));
                    end if;
                end if;
            end loop;
        end if;
    end UpgradeShip;

    procedure GenerateRecruits(BaseIndex : Positive) is
        TimeDiff : Natural;
        MaxRecruits, RecruitsAmount, SkillsAmount, SkillNumber, SkillLevel : Positive;
        subtype Recruits_Range is Positive range 1..15;
        subtype Gender_Range is Positive range 1..2;
        subtype SkillsAmount_Range is Positive range Skills_Names.First_Index..Skills_Names.Last_Index;
        subtype SkillValue_Range is Positive range 1..100;
        package Rand_Recruits is new Discrete_Random(Recruits_Range);
        package Rand_Gender is new Discrete_Random(Gender_Range);
        package Rand_Skills is new Discrete_Random(SkillsAmount_Range);
        package Rand_Value is new Discrete_Random(SkillValue_Range);
        Generator : Rand_Recruits.Generator;
        Generator2 : Rand_Gender.Generator;
        Generator3 : Rand_Skills.Generator;
        Generator4 : Rand_Value.Generator;
        BaseRecruits : Recruit_Container.Vector;
        Skills : Skills_Container.Vector;
        Gender : Character;
        Price : Natural;
        SkillIndex : Integer;
        procedure UpdateRecruit(Recruit : in out Recruit_Data) is
        begin
            Recruit.Skills := Skills;
            Recruit.Price := Price;
        end UpdateRecruit;
    begin
        TimeDiff := (GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) -
            (SkyBases(BaseIndex).RecruitDate.Day + ((30 * SkyBases(BaseIndex).RecruitDate.Month) * SkyBases(BaseIndex).RecruitDate.Year));
        if TimeDiff < 30 then
            return;
        end if;
        Rand_Recruits.Reset(Generator);
        Rand_Gender.Reset(Generator2);
        Rand_Skills.Reset(Generator3);
        Rand_Value.Reset(Generator4);
        if SkyBases(BaseIndex).Population < 150 then
            MaxRecruits := 5;
        elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
            MaxRecruits := 10;
        else
            MaxRecruits := 15;
        end if;
        if MaxRecruits > (SkyBases(BaseIndex).Population / 10) then
            MaxRecruits := (SkyBases(BaseIndex).Population / 10) + 1;
        end if;
        loop
            RecruitsAmount := Rand_Recruits.Random(Generator);
            exit when RecruitsAmount < MaxRecruits;
        end loop;
        for I in 1..RecruitsAmount loop
            Skills.Clear;
            Price := 0;
            if Rand_Gender.Random(Generator2) = 1 then
                Gender := 'M';
            else
                Gender := 'F';
            end if;
            BaseRecruits.Append(New_Item => (Name => GenerateMemberName(Gender), Gender => Gender, 
                Price => 1, Skills => Skills));
            SkillsAmount := Rand_Skills.Random(Generator3);
            for J in 1..SkillsAmount loop
                SkillNumber := Rand_Skills.Random(Generator3);
                SkillLevel := Rand_Value.Random(Generator4);
                SkillIndex := 0;
                for K in Skills.First_Index..Skills.Last_Index loop
                    if Skills.Element(K)(1) = SkillNumber then
                        if Skills.Element(K)(2) < SkillLevel then
                            SkillIndex := K;
                        else
                            SkillIndex := -1;
                        end if;
                        exit;
                    end if;
                end loop;
                if SkillIndex = 0 then
                    Skills.Append(New_Item => (SkillNumber, SkillLevel, 0));
                elsif SkillIndex > 0 then
                    Skills.Replace_Element(Index => SkillIndex, New_Item => (SkillNumber, SkillLevel, 0));
                end if;
            end loop;
            for J in Skills.First_Index..Skills.Last_Index loop
                Price := Price + Skills.Element(J)(2);
            end loop;
            Price := Price * 100;
            BaseRecruits.Update_Element(Index => BaseRecruits.Last_Index,
                Process => UpdateRecruit'Access);
        end loop;
        SkyBases(BaseIndex).RecruitDate := GameDate;
        SkyBases(BaseIndex).Recruits := BaseRecruits;
    end GenerateRecruits;

    procedure HireRecruit(RecruitIndex : Positive) is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        MoneyIndex, Price : Natural;
        Recruit : constant Recruit_Data := SkyBases(BaseIndex).Recruits.Element(RecruitIndex);
        TraderIndex : Positive;
    begin
        MoneyIndex := FindMoney;
        if MoneyIndex = 0 then
            ShowDialog("You don't have Charcollum to hire anyone.");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Price := Recruit.Price;
        Price := Price - Integer(Float'Floor(Float(Price) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
        case SkyBases(BaseIndex).Reputation(1) is
            when -24..-1 =>
                Price := Price + Integer(Float'Floor(Float(Price) * 0.05));
            when 26..50 =>
                Price := Price - Integer(Float'Floor(Float(Price) * 0.05));
            when 51..75 =>
                Price := Price - Integer(Float'Floor(Float(Price) * 0.1));
            when 76..100 =>
                Price := Price - Integer(Float'Floor(Float(Price) * 0.15));
            when others =>
                null;
        end case;
        if Price < 1 then
            Price := 1;
        end if;
        if PlayerShip.Cargo.Element(MoneyIndex).Amount < Price then
            ShowDialog("You don't have enough Charcollum to hire " & To_String(Recruit.Name) & ".");
            return;
        end if;
        PlayerShip.Crew.Append(New_Item => (Name => Recruit.Name, Gender =>
            Recruit.Gender, Health => 100, Tired => 0, Skills =>
            Recruit.Skills, Hunger => 0, Thirst => 0, Order => Rest,
            PreviousOrder => Rest, OrderTime => 15)); 
        UpdateCargo(PlayerShip, 1, (0 - Price));
        GainExp(1, 4, TraderIndex);
        GainRep(BaseIndex, 1);
        AddMessage("You hired " & To_String(Recruit.Name) & " for" & Positive'Image(Price) & " Charcollum.", TradeMessage);
        SkyBases(BaseIndex).Recruits.Delete(Index => RecruitIndex, Count => 1);
        SkyBases(BaseIndex).Population := SkyBases(BaseIndex).Population - 1;
        UpdateGame(5);
    end HireRecruit;

    procedure AskForBases is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Radius, TempX, TempY : Integer;
        Amount, TmpBaseIndex : Natural;
        subtype Bases_Range is Positive range 1..1024;
        package Rand_Bases is new Discrete_Random(Bases_Range);
        Generator : Rand_Bases.Generator;
        TraderIndex : Positive;
        UnknownBases : Natural := 0;
    begin
        if SkyBases(BaseIndex).AskedForBases then
            ShowDialog("You can't ask again for direction to other bases in this base.");
            return;
        end if;
        if SkyBases(BaseIndex).Population < 150 then
            Amount := 10;
            Radius := 10;
        elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
            Amount := 20;
            Radius := 20;
        else
            Amount := 40;
            Radius := 40;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Bases_Loop:
        for X in -Radius..Radius loop
            for Y in -Radius..Radius loop
                TempX := PlayerShip.SkyX + X;
                if TempX < 1 then
                    TempX := 1;
                elsif TempX > 1024 then
                    TempX := 1024;
                end if;
                TempY := PlayerShip.SkyY + Y;
                if TempY < 1 then
                    TempY := 1;
                elsif TempY > 1024 then
                    TempY := 1024;
                end if;
                TmpBaseIndex := SkyMap(TempX, TempY).BaseIndex;
                if TmpBaseIndex > 0 then
                    if not SkyBases(TmpBaseIndex).Known then
                        SkyBases(TmpBaseIndex).Known := True;
                        Amount := Amount - 1;
                        exit Bases_Loop when Amount = 0;
                    end if;
                end if;
            end loop;
        end loop Bases_Loop;
        if Amount > 0 then
            if SkyBases(BaseIndex).Population < 150 then
                if Amount > 1 then
                    Amount := 1;
                end if;
            elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
                if Amount > 2 then
                    Amount := 2;
                end if;
            else
                if Amount > 4 then
                    Amount := 4;
                end if;
            end if;
            for I in SkyBases'Range loop
                if not SkyBases(I).Known then
                    UnknownBases := UnknownBases + 1;
                end if;
                exit when UnknownBases >= Amount;
            end loop;
            if UnknownBases >= Amount then
                Rand_Bases.Reset(Generator);
                loop
                    TmpBaseIndex := Rand_Bases.Random(Generator);
                    if not SkyBases(TmpBaseIndex).Known then
                        SkyBases(TmpBaseIndex).Known := True;
                        Amount := Amount - 1;
                    end if;
                    exit when Amount = 0;
                end loop;
            else
                for I in SkyBases'Range loop
                    if not SkyBases(I).Known then
                        SkyBases(I).Known := True;
                    end if;
                end loop;
            end if;
        end if;
        SkyBases(BaseIndex).AskedForBases := True;
        AddMessage(To_String(PlayerShip.Crew.Element(TraderIndex).Name) & " asked for directions to other bases.", OrderMessage);
        GainExp(1, 4, TraderIndex);
        GainRep(BaseIndex, 1);
        UpdateGame(30);
    end AskForBases;

    procedure AskForEvents is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        TimeDiff : Natural;
        MaxEvents, EventsAmount, TmpBaseIndex, TraderIndex : Positive;
        Event : Events_Types;
        EventX, EventY, EventTime, DiffX, DiffY : Positive;
        MinX, MinY, MaxX, MaxY : Integer;
        type Value_Type is digits 2 range 0.0..9999999.0;
        package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Value_Type);
        Enemies : Positive_Container.Vector;
        PlayerValue : Natural := 0;
    begin
        TimeDiff := (GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) - (SkyBases(BaseIndex).AskedForEvents.Day + ((30 *
            SkyBases(BaseIndex).AskedForEvents.Month) * SkyBases(BaseIndex).AskedForEvents.Year));
        if TimeDiff < 7 then
            ShowDialog("You asked for know events in this base not so long ago.");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        if SkyBases(BaseIndex).Population < 150 then
            MaxEvents := 5;
        elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
            MaxEvents := 10;
        else
            MaxEvents := 15;
        end if;
        EventsAmount := GetRandom(1, MaxEvents);
        MinX := PlayerShip.SkyX - 100;
        if MinX < 1 then
            MinX := 1;
        end if;
        MaxX := PlayerShip.SkyX + 100;
        if MaxX > 1024 then
            MaxX := 1024;
        end if;
        MinY := PlayerShip.SkyY - 100;
        if MinY < 1 then
            MinY := 1;
        end if;
        MaxY := PlayerShip.SkyY + 100;
        if MaxY > 1024 then
            MaxY := 1024;
        end if;
        if GetRandom(1, 100) < 95 then
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                case Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType is
                    when HULL | GUN | BATTERING_RAM =>
                        PlayerValue := PlayerValue + PlayerShip.Modules.Element(I).MaxDurability +
                        (PlayerShip.Modules.Element(I).Max_Value * 10);
                    when ARMOR =>
                        PlayerValue := PlayerValue + PlayerShip.Modules.Element(I).MaxDurability;
                    when others =>
                        null;
                end case;
            end loop;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if Slice(Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType, 1, 4) = "Ammo" then
                    PlayerValue := PlayerValue + (Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Value * 10);
                end if;
            end loop;
            for I in Enemies_List.First_Index..Enemies_List.Last_Index loop
                if Enemies_List.Element(I).CombatValue <= PlayerValue then
                    Enemies.Append(New_Item => I);
                end if;
            end loop;
        else
            for I in Enemies_List.First_Index..Enemies_List.Last_Index loop
                Enemies.Append(New_Item => I);
            end loop;
        end if;
        for I in 1..EventsAmount loop
            loop
                Event := Events_Types'Val(GetRandom(1, 4));
                exit when Event = EnemyShip or Event = AttackOnBase or Event = Disease;
            end loop;
            loop
                if Event = EnemyShip then
                    EventX := GetRandom(MinX, MaxX);
                    EventY := GetRandom(MinY, MaxY);
                    exit when SkyMap(EventX, EventY).BaseIndex = 0 and EventX /= PlayerShip.SkyX and EventY /= PlayerShip.SkyY and 
                        SkyMap(EventX, EventY).EventIndex = 0;
                else
                    TmpBaseIndex := GetRandom(1, 1024);
                    EventX := SkyBases(TmpBaseIndex).SkyX;
                    EventY := SkyBases(TmpBaseIndex).SkyY;
                    exit when EventX /= PlayerShip.SkyX and EventY /= PlayerShip.SkyY and SkyMap(EventX, EventY).EventIndex = 0;
                end if;
            end loop;
            DiffX := abs(PlayerShip.SkyX - EventX);
            DiffY := abs(PlayerShip.SkyY - EventY);
            EventTime := Positive(Value_Type(60) * Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2))));
            case Event is
                when EnemyShip =>
                    Events_List.Append(New_Item => (EnemyShip, EventX, EventY, GetRandom(EventTime, EventTime + 60), 
                        Enemies.Element(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
                when AttackOnBase =>
                    if SkyBases(SkyMap(EventX, EventY).BaseIndex).Known then
                        Events_List.Append(New_Item => (AttackOnBase, EventX, EventY, GetRandom(EventTime, EventTime + 120),
                            Enemies.Element(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
                    end if;
                when Disease =>
                    if SkyBases(SkyMap(EventX, EventY).BaseIndex).Known then
                        Events_List.Append(New_Item => (Disease, EventX, EventY, GetRandom(10080, 12000), 1));
                    end if;
                when others =>
                    null;
            end case;
            SkyMap(EventX, EventY).EventIndex := Events_List.Last_Index;
        end loop;
        SkyBases(BaseIndex).AskedForEvents := GameDate;
        AddMessage(To_String(PlayerShip.Crew.Element(TraderIndex).Name) & " asked for events in base.", OrderMessage);
        GainExp(1, 4, TraderIndex);
        GainRep(BaseIndex, 1);
        UpdateGame(30);
    end AskForEvents;

    procedure BuyRecipe(RecipeIndex : Positive) is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Cost, MoneyIndex : Natural;
        RecipeName : constant String := To_String(Items_List.Element(Recipes_List.Element(RecipeIndex).ResultIndex).Name);
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
        TraderIndex : Positive;
    begin
        if BaseType /= Recipes_List.Element(RecipeIndex).BaseType then
            ShowDialog("You can't buy this recipe in this base.");
            return;
        end if;
        if Known_Recipes.Find_Index(Item => RecipeIndex) /= Positive_Container.No_Index then
            ShowDialog("You already known this recipe.");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Cost := Items_List.Element(Recipes_List.Element(RecipeIndex).ResultIndex).Prices(BaseType) * 
            Recipes_List.Element(RecipeIndex).Difficulty * 100;
        Cost := Cost - Integer(Float'Floor(Float(Cost) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
        case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
            when -24..-1 =>
                Cost := Cost + Integer(Float'Floor(Float(Cost) * 0.05));
            when 26..50 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.05));
            when 51..75 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.1));
            when 76..100 =>
                Cost := Cost - Integer(Float'Floor(Float(Cost) * 0.15));
            when others =>
                null;
        end case;
        if Cost < 1 then
            Cost := 1;
        end if;
        MoneyIndex := FindMoney;
        if MoneyIndex = 0 then
            ShowDialog("You don't have charcollum to buy recipe for " & RecipeName & ".");
            return;
        end if;
        if Cost > PlayerShip.Cargo.Element(MoneyIndex).Amount then
            ShowDialog("You don't have enough charcollum to buy recipe for " & RecipeName & ".");
            return;
        end if;
        UpdateCargo(PlayerShip, 1, (0 - Cost));
        Known_Recipes.Append(New_Item => RecipeIndex);
        AddMessage("You bought recipe for " & RecipeName & " for" & Positive'Image(Cost) & " of charcollum.", TradeMessage);
        GainExp(1, 4, TraderIndex);
        GainRep(BaseIndex, 1);
        UpdateGame(5);
    end BuyRecipe;

end Bases;
