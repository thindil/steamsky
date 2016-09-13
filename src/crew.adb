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
        type Short_Range is range 1..2;
        type Start_Range is range 1..36;
        type Mid_Range is range 1..9;
        StartSyllMale : constant array(Start_Range) of Unbounded_String :=
            (To_Unbounded_String("Aer"), To_Unbounded_String("An"),
            To_Unbounded_String("Ar"), To_Unbounded_String("Ban"),
            To_Unbounded_String("Ber"), To_Unbounded_String("Beth"),
            To_Unbounded_String("Cut"), To_Unbounded_String("Dan"),
            To_Unbounded_String("Dar"), To_Unbounded_String("Dell"),
            To_Unbounded_String("Der"), To_Unbounded_String("Edr"),
            To_Unbounded_String("Er"), To_Unbounded_String("Eth"),
            To_Unbounded_String("Ett"), To_Unbounded_String("Fin"),
            To_Unbounded_String("Ian"), To_Unbounded_String("Iarr"),
            To_Unbounded_String("Ill"), To_Unbounded_String("Jed"),
            To_Unbounded_String("Kan"), To_Unbounded_String("Kar"),
            To_Unbounded_String("Ker"), To_Unbounded_String("Kurr"),
            To_Unbounded_String("Kyr"), To_Unbounded_String("Man"),
            To_Unbounded_String("Mar"), To_Unbounded_String("Mer"),
            To_Unbounded_String("Mir"), To_Unbounded_String("Tsal"),
            To_Unbounded_String("Tser"), To_Unbounded_String("Tsir"),
            To_Unbounded_String("Van"), To_Unbounded_String("Var"),
            To_Unbounded_String("Yur"), To_Unbounded_String("Yyr"));
        MiddleSyllMale : constant array(Mid_Range) of Unbounded_String :=
            (To_Unbounded_String("al"), To_Unbounded_String("an"),
            To_Unbounded_String("ar"), To_Unbounded_String("el"),
            To_Unbounded_String("en"), To_Unbounded_String("ess"),
            To_Unbounded_String("ian"), To_Unbounded_String("onn"),
            To_Unbounded_String("or"));
        EndSyllMale : constant array(Mid_Range) of Unbounded_String :=
            (To_Unbounded_String("ai"), To_Unbounded_String("an"),
            To_Unbounded_String("ar"), To_Unbounded_String("ath"),
            To_Unbounded_String("en"), To_Unbounded_String("eo"),
            To_Unbounded_String("is"), To_Unbounded_String("u"),
            To_Unbounded_String("or"));
        EndSyllFemale : constant array(Mid_Range) of Unbounded_String :=
            (To_Unbounded_String("a"), To_Unbounded_String("ae"),
            To_Unbounded_String("aelle"), To_Unbounded_String("ai"),
            To_Unbounded_String("ea"), To_Unbounded_String("i"),
            To_Unbounded_String("ia"), To_Unbounded_String("u"),
            To_Unbounded_String("wen"));
        package Rand_Start is new Discrete_Random(Start_Range);
        package Rand_Mid is new Discrete_Random(Mid_Range);
        package Rand_Short is new Discrete_Random(Short_Range);
        Generator : Rand_Start.Generator;
        Generator2 : Rand_Mid.Generator;
        Generator3 : Rand_Short.Generator;
        NewName : Unbounded_String;
    begin
        Rand_Start.Reset(Generator);
        Rand_Mid.Reset(Generator2);
        Rand_Short.Reset(Generator3);
        if Rand_Short.Random(Generator3) = 1 then
            if Gender = 'M' then
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & MiddleSyllMale(Rand_Mid.Random(Generator2)) &
                    EndSyllMale(Rand_Mid.Random(Generator2));
            else
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & MiddleSyllMale(Rand_Mid.Random(Generator2)) &
                    EndSyllFemale(Rand_Mid.Random(Generator2));
            end if;
        else
            if Gender = 'M' then
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & EndSyllMale(Rand_Mid.Random(Generator2));
            else
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & EndSyllFemale(Rand_Mid.Random(Generator2));
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

end Crew;
