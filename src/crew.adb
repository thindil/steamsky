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
with Game; use Game;

package body Crew is

    procedure GiveOrders(MemberIndex : Positive; GivenOrder : Crew_Orders; ModuleIndex : Natural := 0) is
        NewOrder : Crew_Orders;
        MemberName : constant String := To_String(PlayerShip.Crew.Element(MemberIndex).Name);
        HaveMaterial, RepairNeeded : Boolean := False;
        ModuleIndex2 : Natural := 0;
        MType : ModuleType := ENGINE;
        procedure UpdateOrder(Member : in out Member_Data) is
        begin
            Member.Order := NewOrder;
            Member.OrderTime := 15;
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
        if GivenOrder = Craft then
            if ModuleIndex = 0 then
                ShowDialog("You must set which workplace you want to use.");
                return;
            end if;
            if PlayerShip.Modules.Element(ModuleIndex).Current_Value = 0 then
                ShowDialog("You can't set crew member for manufacturing, because you don't set item to manufacture in this module.");
                return;
            end if;
        end if;
        if GivenOrder = Gunner and ModuleIndex = 0 then
            ShowDialog("You must set which workplace you want to use.");
            return;
        end if;
        if GivenOrder = Upgrading and PlayerShip.UpgradeModule = 0 then
            ShowDialog("You don't set yet module to upgrade.");
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
        elsif GivenOrder = Pilot or GivenOrder = Engineer or GivenOrder = Upgrading or GivenOrder = Talk then
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = GivenOrder and PlayerShip.Crew.Element(I).Order /= Rest then
                    GiveOrders(I, Rest);
                    exit;
                end if;
            end loop;
        elsif GivenOrder = Gunner or GivenOrder = Craft then
            if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                GiveOrders(PlayerShip.Modules.Element(ModuleIndex).Owner, Rest);
            end if;
        end if;
        if ModuleIndex = 0 then
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                case GivenOrder is
                    when Pilot =>
                        MType := COCKPIT;
                    when Engineer =>
                        MType := ENGINE;
                    when Rest =>
                        MType := CABIN;
                    when others =>
                        exit;
                end case;
                if MType /= CABIN then
                    if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = MType and 
                        PlayerShip.Modules.Element(I).Durability > 0 and PlayerShip.Modules.Element(I).Owner = 0 then
                        ModuleIndex2 := I;
                        exit;
                    end if;
                else
                    if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = CABIN and 
                        PlayerShip.Modules.Element(I).Durability > 0 and
                        PlayerShip.Modules.Element(I).Owner = MemberIndex then
                        ModuleIndex2 := I;
                        exit;
                    end if;
                end if;
            end loop;
        else
            ModuleIndex2 := ModuleIndex;
        end if;
        if ModuleIndex2 = 0 then
            case GivenOrder is
                when Pilot =>
                    ShowDialog(MemberName & " can't starts piloting because cockpit is destroyed or you don't have cockpit.");
                    return;
                when Engineer =>
                    ShowDialog(MemberName & " can't starts engineers duty because all engines are destroyed or you don't have engine.");
                    return;
                when Gunner =>
                    ShowDialog(MemberName & " can't starts operating gun because all guns are destroyed or you don't have installed any.");
                    return;
                when Craft =>
                    ShowDialog(MemberName & " can't starts manufacturing because all workshops are destroyed or you don't have installed any.");
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
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType /= CABIN and 
                PlayerShip.Modules.Element(I).Owner = MemberIndex then
                UpdateModule(PlayerShip, I, "Owner", "0");
                exit;
            end if;
        end loop;
        case GivenOrder is
            when Pilot =>
                AddMessage(MemberName & " starts piloting.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex2, "Owner", Positive'Image(MemberIndex));
            when Engineer =>
                AddMessage(MemberName & " starts engineers duty.", OrderMessage);
            when Gunner =>
                AddMessage(MemberName & " starts operating gun.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex2, "Owner", Positive'Image(MemberIndex));
            when Rest =>
                AddMessage(MemberName & " going on break.", OrderMessage);
            when Repair =>
                AddMessage(MemberName & " starts repair ship.", OrderMessage);
            when Craft =>
                AddMessage(MemberName & " starts manufacturing.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex2, "Owner", Positive'Image(MemberIndex));
            when Upgrading =>
                AddMessage(MemberName & " starts upgrading " & To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name)
                    & ".", OrderMessage);
            when Talk =>
                AddMessage(MemberName & " was assigned to talking in bases.", OrderMessage);
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
        SkillExp, SkillLevel, SkillIndex : Natural := 0;
        procedure UpdateSkill(Skill : in out Skill_Array) is
        begin
            Skill(2) := SkillLevel;
            Skill(3) := SkillExp;
        end UpdateSkill;
        procedure UpdateSkills(Member : in out Member_Data) is
        begin
            if SkillIndex > 0 then
                Member.Skills.Update_Element(Index => SkillIndex, Process => UpdateSkill'Access);
            else
                Member.Skills.Append(New_Item => (SkillNumber, SkillLevel, SkillExp));
            end if;
        end UpdateSkills;
    begin
        for I in PlayerShip.Crew.Element(CrewIndex).Skills.First_Index..PlayerShip.Crew.Element(CrewIndex).Skills.Last_Index loop
            if PlayerShip.Crew.Element(CrewIndex).Skills.Element(I)(1) = SkillNumber then
                SkillIndex := I;
                exit;
            end if;
        end loop;
        if SkillIndex > 0 then
            if PlayerShip.Crew.Element(CrewIndex).Skills.Element(SkillIndex)(2) = 100 then
                return;
            end if;
            SkillLevel := PlayerShip.Crew.Element(CrewIndex).Skills.Element(SkillIndex)(2);
            SkillExp := PlayerShip.Crew.Element(CrewIndex).Skills.Element(SkillIndex)(3) + Amount;
        end if;
        if SkillExp >= (SkillLevel * 100) then
            SkillExp := SkillExp - (SkillLevel * 100);
            SkillLevel := SkillLevel + 1;
        end if;
        PlayerShip.Crew.Update_Element(Index => CrewIndex, Process => UpdateSkills'Access);
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
            AddMessage(To_String(PlayerShip.Crew.Element(MemberIndex).Name) & " died from " &
                To_String(Reason) & ".", CombatMessage);
            PlayerShip.Crew.Delete(Index => MemberIndex, Count => 1);
        else
            AddMessage("You died from " & To_String(Reason) & ".", CombatMessage);
            PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateDeath'Access);
        end if;
    end Death;

    procedure UpdateCrew(Minutes : Positive; TiredPoints : Natural) is
        TiredLevel, HungerLevel, ThirstLevel : Integer := 0;
        HealthLevel : Integer := 100;
        I : Natural;
        DeathReason : Unbounded_String;
        CabinIndex : Natural;
        Times : Natural;
        OrderTime, CurrentMinutes : Integer;
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
                    Member.OrderTime := 15;
                    AddMessage(To_String(Member.Name) & " back to work, fully rested.", OrderMessage);
                end if;
                Member.PreviousOrder := Rest;
            end if;
            if TiredLevel > 80 and Member.Order /= Rest then
                Member.PreviousOrder := Member.Order;
                Member.Order := Rest;
                Member.OrderTime := 15;
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
            if Member.Order /= Repair and Member.Order /= Craft and Member.Order /= Upgrading then
                Member.OrderTime := OrderTime;
            end if;
        end UpdateMember;
    begin
        I := PlayerShip.Crew.First_Index;
        while I <= PlayerShip.Crew.Last_Index loop
            CurrentMinutes := Minutes;
            OrderTime := PlayerShip.Crew.Element(I).OrderTime;
            Times := 0;
            while CurrentMinutes > 0 loop
                if CurrentMinutes >= OrderTime then
                    CurrentMinutes := CurrentMinutes - OrderTime;
                    Times := Times + 1;
                    OrderTime := 15;
                else
                    OrderTime := OrderTime - CurrentMinutes;
                    CurrentMinutes := 0;
                end if;
            end loop;
            HealthLevel := PlayerShip.Crew.Element(I).Health;
            HungerLevel := PlayerShip.Crew.Element(I).Hunger;
            ThirstLevel := PlayerShip.Crew.Element(I).Thirst;
            TiredLevel := PlayerShip.Crew.Element(I).Tired;
            if Times > 0 then
                if PlayerShip.Crew.Element(I).Order = Rest then
                    CabinIndex := 0;
                    for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                        if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                            PlayerShip.Modules.Element(J).Owner = I then
                            CabinIndex := J;
                            exit;
                        end if;
                    end loop;
                    if PlayerShip.Crew.Element(I).Tired > 0 then
                        if CabinIndex > 0 then
                            TiredLevel := TiredLevel - (Times * PlayerShip.Modules.Element(CabinIndex).Current_Value);
                        else
                            TiredLevel := TiredLevel - Times;
                        end if;
                        if TiredLevel < 0 then
                            TiredLevel := 0;
                        end if;
                    end if;
                    if HealthLevel > 0 and HealthLevel < 100 and CabinIndex > 0 then
                        HealthLevel := HealthLevel + Times;
                    end if;
                else
                    if PlayerShip.Crew.Element(I).Order /= Talk then
                        TiredLevel := TiredLevel + Times;
                    end if;
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
            end if;
            if TiredPoints > 0 then
                HungerLevel := HungerLevel + TiredPoints;
                if HungerLevel > 100 then
                    HungerLevel := 100;
                end if;
                if PlayerShip.Crew.Element(I).Hunger = 100 then
                    HealthLevel := HealthLevel - TiredPoints;
                    if HealthLevel < 1 then
                        HealthLevel := 0;
                        DeathReason := To_Unbounded_String("starvation");
                    end if;
                end if;
                ThirstLevel := ThirstLevel + TiredPoints;
                if ThirstLevel > 100 then
                    ThirstLevel := 100;
                end if;
                if PlayerShip.Crew.Element(I).Thirst = 100 then
                    HealthLevel := HealthLevel - TiredPoints;
                    if HealthLevel < 1 then
                        HealthLevel := 0;
                        DeathReason := To_Unbounded_String("dehydration");
                    end if;
                end if;
                if HealthLevel = 0 then
                    Death(I, DeathReason);
                end if;
            end if;
            if HealthLevel > 0 then
                PlayerShip.Crew.Update_Element(Index => I, Process => UpdateMember'Access);
                I := I + 1;
            end if;
        end loop;
    end UpdateCrew;

    function GetSkillLevel(MemberIndex, SkillIndex : Positive) return Natural is
        SkillLevel : Integer := 0;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
        BaseSkillLevel : Natural;
    begin
        for I in PlayerShip.Crew.Element(MemberIndex).Skills.First_Index..PlayerShip.Crew.Element(MemberIndex).Skills.Last_Index loop
            if PlayerShip.Crew.Element(MemberIndex).Skills.Element(I)(1) = SkillIndex then
                BaseSkillLevel := PlayerShip.Crew.Element(MemberIndex).Skills.Element(I)(2);
                Damage := 1.0 - DamageFactor(Float(PlayerShip.Crew.Element(MemberIndex).Health) / 100.0);
                SkillLevel := SkillLevel + (BaseSkillLevel - Integer(Float(BaseSkillLevel) * Float(Damage)));
                if PlayerShip.Crew.Element(MemberIndex).Thirst > 40 then
                    Damage := 1.0 - DamageFactor(Float(PlayerShip.Crew.Element(MemberIndex).Thirst) / 100.0);
                    SkillLevel := SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
                end if;
                if PlayerShip.Crew.Element(MemberIndex).Hunger > 80 then
                    Damage := 1.0 - DamageFactor(Float(PlayerShip.Crew.Element(MemberIndex).Hunger) / 100.0);
                    SkillLevel := SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
                end if;
                if SkillLevel < 0 then
                    SkillLevel := 0;
                end if;
                return SkillLevel;
            end if;
        end loop;
        return SkillLevel;
    end GetSkillLevel;

end Crew;
