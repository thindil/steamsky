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
with Messages; use Messages;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Game; use Game;

package body Crew is

    procedure GiveOrders(MemberIndex : Positive; GivenOrder : Crew_Orders) is
        NewOrder : Crew_Orders;
        MemberName : constant String := To_String(PlayerShip.Crew.Element(MemberIndex).Name);
        HaveMaterial, RepairNeeded : Boolean := False;
        ModuleIndex : Natural := 0;
        MType : ModuleType := ENGINE;
        procedure UpdateOrder(Member : in out Member_Data) is
        begin
            Member.Order := NewOrder;
        end UpdateOrder;
    begin
        if GivenOrder = PlayerShip.Crew.Element(MemberIndex).Order then
            return;
        end if;
        if PlayerShip.Crew.Element(MemberIndex).Tired = 100 then
            ShowDialog(MemberName & " is too tired to work.");
            return;
        end if;
        if PlayerShip.Crew.Element(MemberIndex).Hunger = 100 then
            ShowDialog(MemberName & " is too hungry to work.");
            return;
        end if;
        if PlayerShip.Crew.Element(MemberIndex).Thirst = 100 then
            ShowDialog(MemberName & " is too thirsty to work.");
            return;
        end if;
        if GivenOrder = Repair then
            Repair_Loop:
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    RepairNeeded := True;
                    for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(J).ProtoIndex).IType = 
                            Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairMaterial then
                            HaveMaterial := True;
                            exit Repair_Loop;
                        end if;
                    end loop;
                end if;
            end loop Repair_Loop;
            if not RepairNeeded then
                ShowDialog("Your ship don't need repair.");
                return;
            end if;
            if not HaveMaterial then
                ShowDialog("You don't have repair materials.");
                return;
            end if;
        else
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = GivenOrder and PlayerShip.Crew.Element(I).Order /= Rest then
                    NewOrder := Rest;
                    PlayerShip.Crew.Update_Element(Index => I, Process => UpdateOrder'Access);
                    AddMessage(To_String(PlayerShip.Crew.Element(I).Name) & " going on break.", OrderMessage);
                end if;
            end loop;
        end if;
        if GivenOrder = Craft and PlayerShip.Craft = 0 then
            ShowDialog("You can't set crew member for manufacturing, because you don't set item to manufacture.");
            return;
        end if;
        if GivenOrder = Upgrading and PlayerShip.UpgradeModule = 0 then
            ShowDialog("You don't set yet module to upgrade.");
            return;
        end if;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            case GivenOrder is
                when Pilot =>
                    MType := COCKPIT;
                when Gunner =>
                    MType := TURRET;
                when Craft =>
                    MType := Recipes_List(PlayerShip.Craft).Workplace;
                when Rest =>
                    MType := CABIN;
                when others =>
                    exit;
            end case;
            if MType /= CABIN then
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = MType and 
                    PlayerShip.Modules.Element(I).Durability > 0 then
                    ModuleIndex := I;
                    exit;
                end if;
            else
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = CABIN and 
                    PlayerShip.Modules.Element(I).Durability > 0 and
                    PlayerShip.Modules.Element(I).Owner = MemberIndex then
                    ModuleIndex := I;
                    exit;
                end if;
            end if;
        end loop;
        if ModuleIndex = 0 then
            case GivenOrder is
                when Pilot =>
                    ShowDialog(MemberName & " can't starts piloting because cockpit is destroyed.");
                    return;
                when Gunner =>
                    ShowDialog(MemberName & " can't starts operating gun because turret is destroyed.");
                    return;
                when Craft =>
                    ShowDialog(MemberName & " can't starts manufacturing because workshop is destroyed.");
                    return;
                when Rest =>
                    for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                        if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = CABIN and 
                            PlayerShip.Modules.Element(I).Durability > 0 and
                            PlayerShip.Modules.Element(I).Owner = 0 then
                            UpdateModule(PlayerShip, I, "Owner", Positive'Image(MemberIndex));
                            AddMessage(MemberName & " take " & To_String(PlayerShip.Modules.Element(I).Name) &
                                " as own cabin.", OtherMessage);
                            exit;
                        end if;
                    end loop;
                when others =>
                    null;
            end case;
        end if;
        case GivenOrder is
            when Pilot =>
                AddMessage(MemberName & " starts piloting.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex, "Owner", Positive'Image(MemberIndex));
            when Engineer =>
                AddMessage(MemberName & " starts engineers duty.", OrderMessage);
            when Gunner =>
                AddMessage(MemberName & " starts operating gun.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex, "Owner", Positive'Image(MemberIndex));
            when Rest =>
                AddMessage(MemberName & " going on break.", OrderMessage);
                for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                    if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType /= CABIN and 
                        PlayerShip.Modules.Element(I).Owner = MemberIndex then
                        UpdateModule(PlayerShip, I, "Owner", "0");
                        exit;
                    end if;
                end loop;
            when Repair =>
                AddMessage(MemberName & " starts repair ship.", OrderMessage);
            when Craft =>
                AddMessage(MemberName & " starts manufacturing.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex, "Owner", Positive'Image(MemberIndex));
            when Upgrading =>
                AddMessage(MemberName & " starts upgrading " & To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name)
                    & ".", OrderMessage);
        end case;
        NewOrder := GivenOrder;
        PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateOrder'Access);
    end GiveOrders;

    function Consume(ItemType : Items_Types) return Boolean is
        ProtoIndex : Natural := 0;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = ItemType then
                ProtoIndex := PlayerShip.Cargo.Element(I).ProtoIndex;
                exit;
            end if;
        end loop;
        if ProtoIndex = 0 then
            return False;
        end if;
        UpdateCargo(ProtoIndex, (0 - 1));
        return True;
    end Consume;

    procedure GainExp(Amount : Natural; SkillNumber, CrewIndex : Positive) is
        SkillExp, SkillLevel : Natural;
        procedure UpdateSkill(Member : in out Member_Data) is
        begin
            Member.Skills(SkillNumber, 1) := SkillLevel;
            Member.Skills(SkillNumber, 2) := SkillExp;
        end UpdateSkill;
    begin
        if PlayerShip.Crew.Element(CrewIndex).Skills(SkillNumber, 1) = 100 then
            return;
        end if;
        SkillLevel := PlayerShip.Crew.Element(CrewIndex).Skills(SkillNumber, 1);
        SkillExp := PlayerShip.Crew.Element(CrewIndex).Skills(SkillNumber, 2) + Amount;
        if SkillExp >= (SkillLevel * 100) then
            SkillExp := SkillExp - (SkillLevel * 100);
            SkillLevel := SkillLevel + 1;
        end if;
        PlayerShip.Crew.Update_Element(Index => CrewIndex, Process => UpdateSkill'Access);
    end GainExp;

    function GenerateMemberName(Gender : Character) return Unbounded_String is -- based on name generator from libtcod
        subtype MaleStart_Range is Positive range MaleSyllablesStart.First_Index..MaleSyllablesStart.Last_Index;
        subtype MaleMiddle_Range is Positive range MaleSyllablesMiddle.First_Index..MaleSyllablesMiddle.Last_Index;
        subtype MaleEnd_Range is Positive range MaleSyllablesEnd.First_Index..MaleSyllablesEnd.Last_Index;
        subtype FemaleEnd_Range is Positive range FemaleSyllablesEnd.First_Index..FemaleSyllablesEnd.Last_Index;
        type Short_Range is range 1..2;
        package Rand_MaleStart is new Discrete_Random(MaleStart_Range);
        package Rand_MaleMid is new Discrete_Random(MaleMiddle_Range);
        package Rand_MaleEnd is new Discrete_Random(MaleEnd_Range);
        package Rand_FemaleEnd is new Discrete_Random(FemaleEnd_Range);
        package Rand_Short is new  Discrete_Random(Short_Range);
        Generator : Rand_MaleStart.Generator;
        Generator2 : Rand_MaleMid.Generator;
        Generator3 : Rand_MaleEnd.Generator;
        Generator4 : Rand_FemaleEnd.Generator;
        Generator5 : Rand_Short.Generator;
        NewName : Unbounded_String;
    begin
        Rand_MaleStart.Reset(Generator);
        Rand_MaleMid.Reset(Generator2);
        Rand_MaleEnd.Reset(Generator3);
        Rand_FemaleEnd.Reset(Generator4);
        Rand_Short.Reset(Generator5);
        if Rand_Short.Random(Generator5) = 1 then
            if Gender = 'M' then
                NewName := MaleSyllablesStart.Element(Rand_MaleStart.Random(Generator)) & 
                    MaleSyllablesMiddle.Element(Rand_MaleMid.Random(Generator2)) &
                    MaleSyllablesEnd.Element(Rand_MaleEnd.Random(Generator3));
            else
                NewName := MaleSyllablesStart.Element(Rand_MaleStart.Random(Generator)) & 
                    MaleSyllablesMiddle.Element(Rand_MaleMid.Random(Generator2)) &
                    FemaleSyllablesEnd.Element(Rand_FemaleEnd.Random(Generator4));
            end if;
        else
            if Gender = 'M' then
                NewName := MaleSyllablesStart.Element(Rand_MaleStart.Random(Generator)) & 
                    MaleSyllablesEnd.Element(Rand_MaleMid.Random(Generator2));
            else
                NewName := MaleSyllablesStart.Element(Rand_MaleStart.Random(Generator)) & 
                    FemaleSyllablesEnd(Rand_FemaleEnd.Random(Generator4));
            end if;
        end if;
        return NewName;
    end GenerateMemberName;

    procedure Death(MemberIndex : Positive; Reason : Unbounded_String) is
        procedure UpdateDeath(Member : in out Member_Data) is
        begin
            Member.Order := Rest;
            Member.Health := 0;
        end UpdateDeath;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Owner = MemberIndex then
                UpdateModule(PlayerShip, I, "Owner", "0");
            end if;
        end loop;
        if MemberIndex > 1 then
            PlayerShip.Crew.Delete(Index => MemberIndex, Count => 1);
            AddMessage(To_String(PlayerShip.Crew.Element(MemberIndex).Name) & " died from " &
                To_String(Reason) & ".", CombatMessage);
        else
            PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateDeath'Access);
            AddMessage("You died from " & To_String(Reason) & ".", CombatMessage);
        end if;
    end Death;

    procedure UpdateCrew(Times : Positive) is
        TiredLevel, HungerLevel, ThirstLevel : Integer := 0;
        HealthLevel : Integer := 100;
        I : Positive;
        DeathReason : Unbounded_String;
        CabinIndex : Natural;
        procedure UpdateMember(Member : in out Member_Data) is
            BackToWork : Boolean := True;
        begin
            Member.Tired := TiredLevel;
            if TiredLevel = 0 and Member.Order = Rest and Member.PreviousOrder /= Rest then
                if Member.PreviousOrder /= Repair then
                    for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                        if PlayerShip.Crew.Element(I).Order = Member.PreviousOrder then
                            BackToWork := False;
                            exit;
                        end if;
                    end loop;
                end if;
                if BackToWork then
                    Member.Order := Member.PreviousOrder;
                    AddMessage(To_String(Member.Name) & " back to work, fully rested.", OrderMessage);
                end if;
                Member.PreviousOrder := Rest;
            end if;
            if TiredLevel > 80 and Member.Order /= Rest then
                Member.PreviousOrder := Member.Order;
                Member.Order := Rest;
                AddMessage(To_String(Member.Name) & " is too tired to work, going rest.", OrderMessage);
            end if;
            if HungerLevel > 80 then
                if Consume(Food) then
                    HungerLevel := HungerLevel - 80;
                    if HungerLevel < 0 then
                        HungerLevel := 0;
                    end if;
                else
                    AddMessage(To_String(Member.Name) & " is hungry, but can't find anything to eat.", OtherMessage);
                end if;
            end if;
            Member.Hunger := HungerLevel;
            if ThirstLevel > 40 then
                if Consume(Drink) then
                    ThirstLevel := ThirstLevel - 40;
                    if ThirstLevel < 0 then
                        ThirstLevel := 0;
                    end if;
                else
                    AddMessage(To_String(Member.Name) & " is thirsty, but can't find anything to drink.", OtherMessage);
                end if;
            end if;
            Member.Thirst := ThirstLevel;
            Member.Health := HealthLevel;
        end UpdateMember;
    begin
        I := PlayerShip.Crew.First_Index;
        while I <= PlayerShip.Crew.Last_Index loop
            HealthLevel := PlayerShip.Crew.Element(I).Health;
            if PlayerShip.Crew.Element(I).Order = Rest then
                TiredLevel := 0;
                CabinIndex := 0;
                for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                    if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                        PlayerShip.Modules.Element(J).Owner = I then
                        CabinIndex := I;
                        exit;
                    end if;
                end loop;
                if PlayerShip.Crew.Element(I).Tired > 0 then
                    if CabinIndex > 0 then
                        TiredLevel := PlayerShip.Crew.Element(I).Tired - (Times
                        * PlayerShip.Modules.Element(CabinIndex).Current_Value);
                    else
                        TiredLevel := PlayerShip.Crew.Element(I).Tired - Times;
                    end if;
                    if TiredLevel < 0 then
                        TiredLevel := 0;
                    end if;
                end if;
                if HealthLevel > 0 and HealthLevel < 100 and CabinIndex > 0 then
                    HealthLevel := HealthLevel + Times;
                end if;
            else
                TiredLevel := PlayerShip.Crew.Element(I).Tired + Times;
                if TiredLevel > 100 then
                    TiredLevel := 100;
                end if;
                case PlayerShip.Crew.Element(I).Order is
                    when Pilot =>
                        GainExp(Times, 1, I);
                    when Engineer =>
                        GainExp(Times, 2, I);
                    when others =>
                        null;
                end case;
            end if;
            HungerLevel := PlayerShip.Crew.Element(I).Hunger + Times;
            if HungerLevel > 100 then
                HungerLevel := 100;
            end if;
            if PlayerShip.Crew.Element(I).Hunger = 100 then
                HealthLevel := HealthLevel - Times;
                if HealthLevel < 1 then
                    HealthLevel := 0;
                    DeathReason := To_Unbounded_String("starvation");
                end if;
            end if;
            ThirstLevel := PlayerShip.Crew.Element(I).Thirst + Times;
            if ThirstLevel > 100 then
                ThirstLevel := 100;
            end if;
            if PlayerShip.Crew.Element(I).Thirst = 100 then
                HealthLevel := HealthLevel - Times;
                if HealthLevel < 1 then
                    HealthLevel := 0;
                    DeathReason := To_Unbounded_String("dehydration");
                end if;
            end if;
            PlayerShip.Crew.Update_Element(Index => I, Process => UpdateMember'Access);
            if HealthLevel = 0 then
                Death(I, DeathReason);
            else
                I := I + 1;
            end if;
        end loop;
    end UpdateCrew;

end Crew;
