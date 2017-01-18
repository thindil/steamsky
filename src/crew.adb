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

with Ships; use Ships;
with Messages; use Messages;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Game; use Game;
with Items; use Items;
with Utils; use Utils;
with Ships.Cargo; use Ships.Cargo;

package body Crew is

    procedure GiveOrders(MemberIndex : Positive; GivenOrder : Crew_Orders; ModuleIndex : Natural := 0) is
        NewOrder : Crew_Orders;
        MemberName : constant String := To_String(PlayerShip.Crew.Element(MemberIndex).Name);
        ModuleIndex2 : Natural := 0;
        MType : ModuleType := ENGINE;
        procedure UpdateOrder(Member : in out Member_Data) is
        begin
            Member.Order := NewOrder;
            if NewOrder = Rest then
                Member.PreviousOrder := Rest;
            end if;
            Member.OrderTime := 15;
        end UpdateOrder;
    begin
        if GivenOrder = PlayerShip.Crew.Element(MemberIndex).Order then
            return;
        end if;
        if GivenOrder = Pilot or GivenOrder = Engineer or GivenOrder = Upgrading or GivenOrder = Talk then
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = GivenOrder then
                    GiveOrders(I, Rest);
                    exit;
                end if;
            end loop;
        elsif GivenOrder = Gunner or GivenOrder = Craft or GivenOrder = Heal then
            if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                GiveOrders(PlayerShip.Modules.Element(ModuleIndex).Owner, Rest);
            end if;
        end if;
        if ModuleIndex = 0 and (GivenOrder = Pilot or GivenOrder = Engineer or GivenOrder = Rest) then
            case GivenOrder is
                when Pilot =>
                    MType := COCKPIT;
                when Engineer =>
                    MType := ENGINE;
                when Rest =>
                    MType := CABIN;
                when others =>
                    null;
            end case;
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if MType /= CABIN then
                    if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = MType and 
                        PlayerShip.Modules.Element(I).Durability > 0 
                    then
                        if PlayerShip.Modules.Element(I).Owner /= 0 then
                            GiveOrders(PlayerShip.Modules.Element(I).Owner, Rest);
                        end if;
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
            when Heal =>
                AddMessage(MemberName & " starts healing wounded crew members.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex2, "Owner", Positive'Image(MemberIndex));
            when Clean =>
                AddMessage(MemberName & " starts cleaning ship.", OrderMessage);
        end case;
        NewOrder := GivenOrder;
        PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateOrder'Access);
    end GiveOrders;

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
        NewName : Unbounded_String;
    begin
        if Gender = 'M' then
            NewName := MaleSyllablesStart.Element(GetRandom(MaleSyllablesStart.First_Index, MaleSyllablesStart.Last_Index)) &
                MaleVocals.Element(GetRandom(MaleVocals.First_Index, MaleVocals.Last_Index));
            if GetRandom(1, 100) < 36 then
                Append(NewName, MaleSyllablesMiddle.Element(GetRandom(MaleSyllablesMiddle.First_Index, MaleSyllablesMiddle.Last_Index)));
            end if;
            if GetRandom(1, 100) < 11 then
                Append(NewName, MaleConsonants.Element(GetRandom(MaleConsonants.First_Index, MaleConsonants.Last_Index)));
            end if;
            Append(NewName, MaleSyllablesEnd.Element(GetRandom(MaleSyllablesEnd.First_Index, MaleSyllablesEnd.Last_Index)));
        else
            NewName := FemaleSyllablesStart.Element(GetRandom(FemaleSyllablesStart.First_Index, FemaleSyllablesStart.Last_Index)) &
                FemaleVocals.Element(GetRandom(FemaleVocals.First_Index, FemaleVocals.Last_Index));
            if GetRandom(1, 100) < 36 then
                Append(NewName, FemaleSyllablesMiddle.Element(GetRandom(FemaleSyllablesMiddle.First_Index, 
                    FemaleSyllablesMiddle.Last_Index)));
            end if;
            if GetRandom(1, 100) < 11 then
                Append(NewName, FemaleSyllablesMiddle.Element(GetRandom(FemaleSyllablesMiddle.First_Index, 
                    FemaleSyllablesMiddle.Last_Index)));
            end if;
            Append(NewName, FemaleSyllablesEnd.Element(GetRandom(FemaleSyllablesEnd.First_Index, FemaleSyllablesEnd.Last_Index)));
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
        if MemberIndex > 1 then
            AddMessage(To_String(PlayerShip.Crew.Element(MemberIndex).Name) & " died from " &
                To_String(Reason) & ".", CombatMessage);
            PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 40, Amount => 1, Name => PlayerShip.Crew.Element(MemberIndex).Name &
                To_Unbounded_String("'s corpse"), Durability => 100));
            DeleteMember(MemberIndex);
        else
            AddMessage("You died from " & To_String(Reason) & ".", CombatMessage);
            PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateDeath'Access);
        end if;
    end Death;

    procedure UpdateCrew(Minutes : Positive; TiredPoints : Natural) is
        TiredLevel, HungerLevel, ThirstLevel : Integer := 0;
        HealthLevel : Integer := 100;
        DeathReason : Unbounded_String;
        CabinIndex, Times, RestAmount, I : Natural;
        OrderTime, CurrentMinutes, HealAmount : Integer;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
        NeedCleaning : Boolean := False;
        function Consume(ItemType : String) return Natural is
            ProtoIndex : Natural := 0;
            ConsumeValue : Natural := 0;
        begin
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = To_Unbounded_String(ItemType) then
                    ProtoIndex := PlayerShip.Cargo.Element(I).ProtoIndex;
                    ConsumeValue := Items_List.Element(ProtoIndex).Value;
                    exit;
                end if;
            end loop;
            if ProtoIndex = 0 then
                if ItemType = "Food" then
                    for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = To_Unbounded_String("RawFood") then
                            ProtoIndex := PlayerShip.Cargo.Element(I).ProtoIndex;
                            ConsumeValue := Items_List.Element(ProtoIndex).Value;
                            exit;
                        end if;
                    end loop;
                end if;
                if ProtoIndex = 0 then
                    return 0;
                end if;
            end if;
            UpdateCargo(PlayerShip, ProtoIndex, (0 - 1));
            return ConsumeValue;
        end Consume;
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
                HungerLevel := HungerLevel - Consume("Food");
                if HungerLevel < 0 then
                    HungerLevel := 0;
                elsif HungerLevel > 80 then
                    AddMessage(To_String(Member.Name) & " is hungry, but can't find anything to eat.", OtherMessage);
                end if;
            end if;
            Member.Hunger := HungerLevel;
            if ThirstLevel > 40 then
                ThirstLevel := ThirstLevel - Consume("Drink");
                if ThirstLevel < 0 then
                    ThirstLevel := 0;
                elsif ThirstLevel > 40 then
                    AddMessage(To_String(Member.Name) & " is thirsty, but can't find anything to drink.", OtherMessage);
                end if;
            end if;
            Member.Thirst := ThirstLevel;
            Member.Health := HealthLevel;
            if Member.Order /= Repair and Member.Order /= Craft and Member.Order /= Upgrading then
                Member.OrderTime := OrderTime;
            end if;
        end UpdateMember;
        procedure Heal(Member : in out Member_Data) is
        begin
            Member.Health := Member.Health + HealAmount;
            if Member.Health > 100 then
                Member.Health := 100;
            end if;
        end Heal;
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
                            Damage := 1.0 - DamageFactor(Float(PlayerShip.Modules.Element(CabinIndex).Durability) / 
                                Float(PlayerShip.Modules.Element(CabinIndex).MaxDurability));
                            RestAmount := PlayerShip.Modules.Element(CabinIndex).Current_Value - 
                                Natural(Float(PlayerShip.Modules.Element(CabinIndex).Current_Value) * Float(Damage));
                            if RestAmount = 0 then
                                RestAmount := 1;
                            end if;
                            TiredLevel := TiredLevel - (Times * RestAmount);
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
                        when Heal =>
                            HealAmount := Times * (GetSkillLevel(I, 10) / 20);
                            if HealAmount < Times then
                                HealAmount := Times;
                            end if;
                            for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                                if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = MEDICAL_ROOM and
                                    PlayerShip.Modules.Element(J).Durability = 0 
                                then
                                    HealAmount := -1;
                                    AddMessage("You don't have medical room to continue healing wounded crew members.", OrderMessage);
                                    exit;
                                end if;
                            end loop;
                            HealAmount := HealAmount * (-1);
                            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                                if Items_List.Element(PlayerShip.Cargo.Element(J).ProtoIndex).IType = To_Unbounded_String("Medicines") then
                                    HealAmount := abs(HealAmount);
                                    UpdateCargo(PlayerShip, PlayerShip.Cargo.Element(J).ProtoIndex, (0 - Times));
                                    exit;
                                end if;
                            end loop;
                            if HealAmount > 0 then
                                for J in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                                    if PlayerShip.Crew.Element(J).Health < 100 and J /= I then
                                        PlayerShip.Crew.Update_Element(Index => J, Process => Heal'Access);
                                        AddMessage(To_String(PlayerShip.Crew.Element(I).Name) & " healed " & 
                                            To_String(PlayerShip.Crew.Element(J).Name) & " a bit.", OrderMessage);
                                        GainExp(Times, 10, I);
                                        exit;
                                    end if;
                                end loop;
                                for J in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                                    if PlayerShip.Crew.Element(J).Health < 100 and J /= I then
                                        HealAmount := 0;
                                        exit;
                                    end if;
                                end loop;
                                if HealAmount > 0 then
                                    AddMessage(To_String(PlayerShip.Crew.Element(I).Name) & " finished healing wounded.", OrderMessage);
                                end if;
                            else
                                AddMessage("You don't have any medical supplies to continue healing wounded crew members.", OrderMessage);
                            end if;
                            if HealAmount /= 0 then
                                GiveOrders(I, Rest);
                            end if;
                        when Clean =>
                            for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                                if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                                    PlayerShip.Modules.Element(J).Current_Value < PlayerShip.Modules.Element(J).Max_Value
                                then
                                    if PlayerShip.Modules.Element(J).Current_Value + Times > PlayerShip.Modules.Element(J).Max_Value then
                                        UpdateModule(PlayerShip, J, "Current_Value", 
                                            Positive'Image(PlayerShip.Modules.Element(J).Max_Value));
                                    else
                                        UpdateModule(PlayerShip, J, "Current_Value", 
                                            Positive'Image(PlayerShip.Modules.Element(J).Current_Value + Times));
                                    end if;
                                    exit;
                                end if;
                            end loop;
                            for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                                if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                                    PlayerShip.Modules.Element(J).Current_Value < PlayerShip.Modules.Element(J).Max_Value
                                then
                                    NeedCleaning := True;
                                    exit;
                                end if;
                            end loop;
                            if not NeedCleaning then
                                for J in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                                    if PlayerShip.Crew.Element(J).Order = Clean then
                                        GiveOrders(J, Rest);
                                    end if;
                                end loop;
                            end if;
                            NeedCleaning := False;
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

    procedure DeleteMember(MemberIndex : Positive) is
    begin
        PlayerShip.Crew.Delete(Index => MemberIndex, Count => 1);
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Owner = MemberIndex then
                UpdateModule(PlayerShip, I, "Owner", "0");
            elsif PlayerShip.Modules.Element(I).Owner > MemberIndex then
                UpdateModule(PlayerShip, I, "Owner", Positive'Image(PlayerShip.Modules.Element(I).Owner - 1));
            end if;
        end loop;
    end DeleteMember;

end Crew;
