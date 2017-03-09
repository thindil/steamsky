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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Statistics; use Statistics;
with Utils; use Utils;
with Ships.Cargo; use Ships.Cargo;

package body Ships is

    function HaveOrderRequirements(ShowInfo : Boolean := True) return Boolean is
        HaveCockpit, HaveEngine, HavePilot, HaveEngineer : Boolean := False;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType = COCKPIT and PlayerShip.Modules.Element(I).Durability > 0 then
                HaveCockpit := True;
            elsif Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType = ENGINE and PlayerShip.Modules.Element(I).Durability > 1 
            then
                HaveEngine := True;
            end if;
            if HaveEngine and HaveCockpit then
                exit;
            end if;
        end loop;
        if not HaveEngine then
            if ShowInfo then
                ShowDialog("You don't have working engine on ship or all engines are destroyed.");
            end if;
            return False;
        end if;
        if not HaveCockpit then
            if ShowInfo then
                ShowDialog("You don't have cockpit on ship or cockpit is destroyed.");
            end if;
            return False;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Pilot then
                HavePilot := True;
            elsif PlayerShip.Crew.Element(I).Order = Engineer then
                HaveEngineer := True;
            end if;
            if HavePilot and HaveEngineer then
                exit;
            end if;
        end loop;
        if not HavePilot then
            if ShowInfo then
                ShowDialog("You don't have pilot on duty.");
            end if;
            return False;
        end if;
        if not HaveEngineer then
            if ShowInfo then
                ShowDialog("You don't have enginner on duty.");
            end if;
            return False;
        end if;
        return True;
    end HaveOrderRequirements;

    function MoveShip(ShipIndex, X, Y: Integer) return Natural is
        NewX, NewY : Integer;
        TimePassed, FuelNeeded : Integer := 0;
        type SpeedType is digits 2;
        Speed : SpeedType;
    begin
        if ShipIndex = 0 then
            case PlayerShip.Speed is
                when DOCKED =>
                    ShowDialog("First you must undock ship from base.");
                    return 0;
                when FULL_STOP =>
                    ShowDialog("First you must set speed for ship.");
                    return 0;
                when others =>
                    null;
            end case;
            if not HaveOrderRequirements then
                return 0;
            end if;
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = ENGINE then
                    case PlayerShip.Speed is
                        when QUARTER_SPEED =>
                            FuelNeeded := FuelNeeded - (PlayerShip.Modules.Element(I).Current_Value / 4);
                        when HALF_SPEED =>
                            FuelNeeded := FuelNeeded - (PlayerShip.Modules.Element(I).Current_Value / 2);
                        when FULL_SPEED =>
                            FuelNeeded := FuelNeeded - PlayerShip.Modules.Element(I).Current_Value;
                        when others =>
                            null;
                    end case;
                end if;
            end loop;
            if FuelNeeded = 0 then
                FuelNeeded := -1;
            end if;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop -- Check for fuel
                if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then 
                    if PlayerShip.Cargo.Element(I).Amount < abs FuelNeeded then
                        ShowDialog("You don't have enough fuel (Charcollum).");
                        return 0;
                    end if;
                    exit;
                end if;
            end loop;
            NewX := PlayerShip.SkyX + X;
            NewY := PlayerShip.SkyY + Y;
        end if;
        if NewX < 1 or NewX > 1024 or NewY < 1 or NewY > 1024 then
            return 0;
        end if;
        if ShipIndex = 0 then
            PlayerShip.SkyX := NewX;
            PlayerShip.SkyY := NewY;
            UpdateCargo(PlayerShip, 1, FuelNeeded);
            Speed := (SpeedType(RealSpeed(PlayerShip)) / 1000.0);
            TimePassed := Integer(100.0 / Speed);
            if TimePassed > 0 then
                case PlayerShip.Speed is
                    when QUARTER_SPEED =>
                        if TimePassed < 60 then
                            TimePassed := 60;
                        end if;
                    when HALF_SPEED =>
                        if TimePassed < 30 then
                            TimePassed := 30;
                        end if;
                    when FULL_SPEED =>
                        if TimePassed < 15 then
                            TimePassed := 15;
                        end if;
                    when others =>
                        null;
                end case;
                GameStats.DistanceTraveled := GameStats.DistanceTraveled + 1;
                UpdateGame(TimePassed);
            end if;
        end if;
        return 1;
    end MoveShip;

    procedure DockShip(Docking : Boolean) is
        BaseIndex : constant Natural := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        MoneyIndex : constant Natural := FindCargo(1);
        DockingCost : Positive;
        TraderIndex : Natural := 0;
    begin
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
            ShowDialog("Here no base to dock or undock.");
            return;
        end if;
        if Docking and PlayerShip.Speed = DOCKED then
            ShowDialog("Ship is docked to base.");
            return;
        end if;
        if not Docking and PlayerShip.Speed > DOCKED then
            ShowDialog("Ship isn't docked to base.");
            return;
        end if;
        if not HaveOrderRequirements then
            return;
        end if;
        if Docking then
            if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Owner /= Abandoned then
                if MoneyIndex = 0 then
                    ShowDialog("You can't dock to base because you don't have Charcollum to pay for docking.");
                    return;
                end if;
                for Module of PlayerShip.Modules loop
                    if Modules_List.Element(Module.ProtoIndex).MType = HULL then
                        DockingCost := Module.Max_Value;
                        exit;
                    end if;
                end loop;
                for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(I).Order = Talk then
                        TraderIndex := I;
                        exit;
                    end if;
                end loop;
                if TraderIndex > 0 then
                    DockingCost := DockingCost - Integer(Float'Floor(Float(DockingCost) * (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
                end if;
                case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Reputation(1) is
                    when -24..-1 =>
                        DockingCost := DockingCost + Integer(Float'Floor(Float(DockingCost) * 0.05));
                    when 26..50 =>
                        DockingCost := DockingCost - Integer(Float'Floor(Float(DockingCost) * 0.05));
                    when 51..75 =>
                        DockingCost := DockingCost - Integer(Float'Floor(Float(DockingCost) * 0.1));
                    when 76..100 =>
                        DockingCost := DockingCost - Integer(Float'Floor(Float(DockingCost) * 0.15));
                    when others =>
                        null;
                end case;
                if DockingCost > PlayerShip.Cargo.Element(MoneyIndex).Amount then
                    ShowDialog("You can't dock to base because you don't have enough Charcollum to pay for docking.");
                    return;
                end if;
                UpdateCargo(PlayerShip, 1, (0 - DockingCost));
                AddMessage("Ship docked to base " & To_String(SkyBases(BaseIndex).Name) & ". It costs" & Positive'Image(DockingCost) &
                    " Charcollum.", OrderMessage);
                if TraderIndex > 0 then
                    GainExp(1, 4, TraderIndex);
                end if;
            else
                AddMessage("Ship docked to base " & To_String(SkyBases(BaseIndex).Name) & ".", OrderMessage);
            end if;
            PlayerShip.Speed := DOCKED;
            UpdateGame(10);
        else
            PlayerShip.Speed := QUARTER_SPEED;
            AddMessage("Ship undocked from base " &
                To_String(SkyBases(BaseIndex).Name), OrderMessage);
            UpdateGame(5);
        end if;
    end DockShip;

    procedure ChangeShipSpeed(SpeedValue : ShipSpeed; ShowInfo : Boolean := True) is
        HaveEngine, HaveEngineer : Boolean := False;
    begin
        if PlayerShip.Speed = DOCKED then
            if ShowInfo then
                ShowDialog("First undock from base before you set ship speed.");
            end if;
            return;
        end if;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType = ENGINE and PlayerShip.Modules.Element(I).Durability > 0 
            then
                HaveEngine := True;
                exit;
            end if;
        end loop;
        if not HaveEngine then
            if ShowInfo then
                ShowDialog("You don't have working engine on ship or all engines are destroyed.");
            end if;
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Engineer then
                HaveEngineer := True;
                exit;
            end if;
        end loop;
        if not HaveEngineer then
            if ShowInfo then
                ShowDialog("You don't have enginner on duty.");
            end if;
            return;
        end if;
        PlayerShip.Speed := SpeedValue;
    end ChangeShipSpeed;

    procedure UpdateModule(Ship : in out ShipRecord; ModuleIndex : Positive; Field : String; Value : String) is
        NewDurability, NewValue, NewMaxDurability, NewMaxValue, NewUpgradeProgress : Integer;
        NewName : Unbounded_String;
        NewOwner, NewWeight : Natural;
        NewUpgradeAction : ShipUpgrade;
        procedure UpdateMod(Module : in out ModuleData) is
        begin
            if Field = "Durability" then
                Module.Durability := NewDurability;
            elsif Field = "Name" then
                Module.Name := NewName;
            elsif Field = "Current_Value" then
                Module.Current_Value := NewValue;
            elsif Field = "Owner" then
                Module.Owner := NewOwner;
            elsif Field = "MaxDurability" then
                Module.MaxDurability := NewMaxDurability;
            elsif Field = "Max_Value" then
                Module.Max_Value := NewMaxValue;
            elsif Field = "UpgradeProgress" then
                Module.UpgradeProgress := NewUpgradeProgress;
            elsif Field = "UpgradeAction" then
                Module.UpgradeAction := NewUpgradeAction;
            elsif Field = "Weight" then
                Module.Weight := NewWeight;
            end if;
        end UpdateMod;
    begin
        if ModuleIndex > Positive(Ship.Modules.Length) then
            return;
        end if;
        if Field = "Durability" then
            NewDurability := Ship.Modules.Element(ModuleIndex).Durability + Integer'Value(Value);
            if NewDurability < 0 then
                NewDurability := 0;
            end if;
        elsif Field = "Name" then
            NewName := To_Unbounded_String(Value);
        elsif Field = "Current_Value" then
            NewValue := Integer'Value(Value);
        elsif Field = "Owner" then
            NewOwner := Natural'Value(Value);
        elsif Field = "MaxDurability" then
            NewMaxDurability := Ship.Modules.Element(ModuleIndex).MaxDurability + Integer'Value(Value);
        elsif Field = "Max_Value" then
            NewMaxValue := Ship.Modules.Element(ModuleIndex).Max_Value + Integer'Value(Value);
        elsif Field = "UpgradeProgress" then
            NewUpgradeProgress := Integer'Value(Value);
        elsif Field = "UpgradeAction" then
            NewUpgradeAction := ShipUpgrade'Value(Value);
        elsif Field = "Weight" then
            NewWeight := Ship.Modules.Element(ModuleIndex).Weight + Natural'Value(Value);
        end if;
        Ship.Modules.Update_Element(Index => ModuleIndex, Process => UpdateMod'Access);
    end UpdateModule;
    
    function CreateShip(ProtoIndex : Positive; Name : Unbounded_String; X, Y: Integer; Speed : ShipSpeed) return ShipRecord is
        TmpShip : ShipRecord;
        ShipModules : Modules_Container.Vector;
        ShipCrew : Crew_Container.Vector;
        ShipMissions : Mission_Container.Vector;
        NewName : Unbounded_String;
        TurretIndex, GunIndex, HullIndex, Amount : Natural := 0;
        Gender : Character;
        MemberName : Unbounded_String;
        TmpSkills : Skills_Container.Vector;
        ProtoShip : constant ProtoShipData := ProtoShips_List.Element(ProtoIndex);
        ShipCargo : Cargo_Container.Vector;
        procedure UpdateMod(Module : in out ModuleData) is
        begin
            if Modules_List.Element(Module.ProtoIndex).MType = CABIN then
                Module.Name := MemberName & To_Unbounded_String("'s Cabin");
            end if;
            Module.Owner := ShipCrew.Last_Index;
        end UpdateMod;
    begin
        for I in ProtoShip.Modules.First_Index..ProtoShip.Modules.Last_Index loop
            ShipModules.Append(New_Item => (Name => Modules_List.Element(ProtoShip.Modules(I)).Name,
            ProtoIndex => ProtoShip.Modules(I), 
            Weight => Modules_List.Element(ProtoShip.Modules(I)).Weight,
            Current_Value => Modules_List.Element(ProtoShip.Modules(I)).Value,
            Max_Value => Modules_List.Element(ProtoShip.Modules(I)).MaxValue,
            Durability => Modules_List.Element(ProtoShip.Modules(I)).Durability,
            MaxDurability => Modules_List.Element(ProtoShip.Modules(I)).Durability,
            Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE));
        end loop;
        if Name = Null_Unbounded_String then
            NewName := ProtoShip.Name;
        else
            NewName := Name;
        end if;
        for Member of ProtoShip.Crew loop
            if GetRandom(1, 100) < 50 then
                Gender := 'M';
            else
                Gender := 'F';
            end if;
            MemberName := GenerateMemberName(Gender);
            for Skill of Member.Skills loop
                if Skill(3) = 0 then
                    TmpSkills.Append(New_Item => Skill);
                else
                    TmpSkills.Append(New_Item => (Skill(1), GetRandom(Skill(2), Skill(3)), 0));
                end if;
            end loop;
            ShipCrew.Append(New_Item => (Name => MemberName, Gender => Gender,
                Health => 100, Tired => 0, Skills => TmpSkills, Hunger => 0, Thirst => 0, Order => Member.Order,
                PreviousOrder => Rest, OrderTime => 15, Orders => (others => 0)));
            TmpSkills.Clear;
            for I in ShipModules.First_Index..ShipModules.Last_Index loop
                if Modules_List.Element(ShipModules.Element(I).ProtoIndex).MType = CABIN and ShipModules.Element(I).Owner = 0 then
                    ShipModules.Update_Element(Index => I, Process => UpdateMod'Access);
                    exit;
                end if;
            end loop;
            for I in ShipModules.First_Index..ShipModules.Last_Index loop
                if ShipModules.Element(I).Owner = 0 then
                    if Modules_List.Element(ShipModules.Element(I).ProtoIndex).MType = GUN and Member.Order = Gunner then
                        ShipModules.Update_Element(Index => I, Process => UpdateMod'Access);
                        exit;
                    end if;
                elsif Modules_List.Element(ShipModules.Element(I).ProtoIndex).MType = COCKPIT and Member.Order = Pilot
                then
                    ShipModules.Update_Element(Index => I, Process => UpdateMod'Access);
                    exit;
                end if;
            end loop;
        end loop;
        for Item of ProtoShip.Cargo loop
            if Item(3) > 0 then
                Amount := GetRandom(Item(2), Item(3));
            else
                Amount := Item(2);
            end if;
            ShipCargo.Append(New_Item => (ProtoIndex => Item(1), Amount => Amount, Name => Null_Unbounded_String,
                Durability => 100));
        end loop;
        TmpShip := (Name => NewName, SkyX => X, SkyY => Y, Speed => Speed,
            Modules => ShipModules, Cargo => ShipCargo, Crew => ShipCrew,
            UpgradeModule => 0, DestinationX => 0, DestinationY => 0,
            RepairModule => 0, Missions => ShipMissions, Description => ProtoShip.Description);
        Amount := 0;
        for I in TmpShip.Modules.First_Index..TmpShip.Modules.Last_Index loop
            case Modules_List.Element(TmpShip.Modules.Element(I).ProtoIndex).MType is
                when TURRET =>
                    TurretIndex := I;
                when GUN =>
                    GunIndex := I;
                when HULL =>
                    HullIndex := I;
                when others =>
                    null;
            end case;
            if TurretIndex > 0 and GunIndex > 0 then
                UpdateModule(TmpShip, TurretIndex, "Current_Value", Positive'Image(GunIndex));
                TurretIndex := 0;
                GunIndex := 0;
            end if;
            Amount := Amount + Modules_List.Element(TmpShip.Modules.Element(I).ProtoIndex).Size;
        end loop;
        UpdateModule(TmpShip, HullIndex, "Current_Value", Natural'Image(Amount));
        return TmpShip;
    end CreateShip;

    function LoadShips return Boolean is
        ShipsFile : File_Type;
        RawData, FieldName, Value, SkillsValue : Unbounded_String;
        EqualIndex, StartIndex, EndIndex, Amount, XIndex, CombatValue, DotIndex, EndIndex2, StartIndex2 : Natural;
        TempRecord : ProtoShipData;
        TempModules : Positive_Container.Vector;
        TempCargo : Skills_Container.Vector;
        TempCrew : ProtoCrew_Container.Vector;
        TempSkills : Skills_Container.Vector;
        TempOrder : Crew_Orders;
        SkillsAmount : Positive;
        procedure UpdateMember(Member : in out ProtoCrewData) is
        begin
            Member.Order := TempOrder;
        end UpdateMember;
    begin
        if ProtoShips_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/ships.dat") then
            return False;
        end if;
        TempRecord := (Name => Null_Unbounded_String, Modules => TempModules, 
            Accuracy => (0, 0), CombatAI => NONE, Evasion => (0, 0), LootMin => 1,
            LootMax => 100, Perception => (0, 0), Cargo => TempCargo, CombatValue => 1,
            Crew => TempCrew, Description => Null_Unbounded_String, Owner => Poleis);
        Open(ShipsFile, In_File, "data/ships.dat");
        while not End_Of_File(ShipsFile) loop
            RawData := To_Unbounded_String(Get_Line(ShipsFile));
            if Element(RawData, 1) /= '[' then
                EqualIndex := Index(RawData, "=");
                FieldName := Head(RawData, EqualIndex - 2);
                Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
                if FieldName = To_Unbounded_String("Name") then
                    TempRecord.Name := Value;
                elsif FieldName = To_Unbounded_String("Modules") then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        TempRecord.Modules.Append(New_Item => Integer'Value(Slice(Value, StartIndex, EndIndex - 1)));
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("Accuracy") then
                    DotIndex := Index(Value, "..");
                    if DotIndex = 0 then
                        TempRecord.Accuracy := (Integer'Value(To_String(Value)), 0);
                    else
                        TempRecord.Accuracy := (Integer'Value(Slice(Value, 1, DotIndex - 1)), 
                            Integer'Value(Slice(Value, DotIndex + 2, Length(Value))));
                    end if;
                elsif FieldName = To_Unbounded_String("CombatAI") then
                    TempRecord.CombatAI := ShipCombatAI'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Evasion") then
                    DotIndex := Index(Value, "..");
                    if DotIndex = 0 then
                        TempRecord.Evasion := (Integer'Value(To_String(Value)), 0);
                    else
                        TempRecord.Evasion := (Integer'Value(Slice(Value, 1, DotIndex - 1)), 
                            Integer'Value(Slice(Value, DotIndex + 2, Length(Value))));
                    end if;
                elsif FieldName = To_Unbounded_String("LootMin") then
                    TempRecord.LootMin := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("LootMax") then
                    TempRecord.LootMax := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Perception") then
                    DotIndex := Index(Value, "..");
                    if DotIndex = 0 then
                        TempRecord.Perception := (Integer'Value(To_String(Value)), 0);
                    else
                        TempRecord.Perception := (Integer'Value(Slice(Value, 1, DotIndex - 1)), 
                            Integer'Value(Slice(Value, DotIndex + 2, Length(Value))));
                    end if;
                elsif FieldName = To_Unbounded_String("Cargo") then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        XIndex := Index(Value, "x", StartIndex);
                        DotIndex := Index(Value, "..", StartIndex);
                        if DotIndex = 0 or DotIndex > EndIndex then
                            TempRecord.Cargo.Append(New_Item => (Integer'Value(Slice(Value, XIndex + 1, EndIndex - 1)),
                                Integer'Value(Slice(Value, StartIndex, XIndex - 1)), 0));
                        else
                            TempRecord.Cargo.Append(New_Item => (Integer'Value(Slice(Value, XIndex + 1, EndIndex - 1)),
                                Integer'Value(Slice(Value, StartIndex, DotIndex - 1)), 
                                Integer'Value(Slice(Value, DotIndex + 2, XIndex - 1))));
                        end if;
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("Skills") then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, "; ") + 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, "; ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        SkillsValue := To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1));
                        StartIndex2 := 1;
                        SkillsAmount :=  Ada.Strings.Unbounded.Count(SkillsValue, ", ") + 1;
                        for J in 1..SkillsAmount loop
                            EndIndex2 := Index(SkillsValue, ", ", StartIndex2);
                            if EndIndex2 = 0 then
                                EndIndex2 := Length(SkillsValue) + 1;
                            end if;
                            XIndex := Index(SkillsValue, "x", StartIndex2);
                            DotIndex := Index(SkillsValue, "..", StartIndex2);
                            if DotIndex = 0 or DotIndex > EndIndex2 then
                                TempSkills.Append(New_Item => (Integer'Value(Slice(SkillsValue, StartIndex2, XIndex - 1)),
                                    Integer'Value(Slice(SkillsValue, XIndex + 1, EndIndex2 - 1)), 0));
                            else
                                TempSkills.Append(New_Item => (Integer'Value(Slice(SkillsValue, StartIndex2, XIndex - 1)),
                                    Integer'Value(Slice(SkillsValue, XIndex + 1, DotIndex - 1)), 
                                    Integer'Value(Slice(SkillsValue, DotIndex + 2, EndIndex2 - 1))));
                            end if;
                            StartIndex2 := EndIndex2 + 2;
                        end loop;
                        TempRecord.Crew.Append(New_Item => (Skills => TempSkills, Order => Rest));
                        TempSkills.Clear;
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("Orders") then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        TempOrder := Crew_Orders'Value(Slice(Value, StartIndex, EndIndex - 1));
                        TempRecord.Crew.Update_Element(Index => I, Process => UpdateMember'Access);
                        StartIndex := EndIndex + 2;
                    end loop;
                    TempOrder := Rest;
                elsif FieldName = To_Unbounded_String("Description") then
                    TempRecord.Description := Value;
                elsif FieldName = To_Unbounded_String("Owner") then
                    TempRecord.Owner := Bases_Owners'Value(To_String(Value));
                end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
                CombatValue := 0;
                for I in TempRecord.Modules.First_Index..TempRecord.Modules.Last_Index loop
                    case Modules_List.Element(TempRecord.Modules.Element(I)).MType is
                        when HULL | GUN | BATTERING_RAM =>
                            CombatValue := CombatValue + Modules_List.Element(TempRecord.Modules.Element(I)).Durability +
                                (Modules_List.Element(TempRecord.Modules.Element(I)).MaxValue * 10);
                        when ARMOR =>
                            CombatValue := CombatValue + Modules_List.Element(TempRecord.Modules.Element(I)).Durability;
                        when others =>
                            null;
                    end case;
                end loop;
                for I in TempRecord.Cargo.First_Index..TempRecord.Cargo.Last_Index loop
                    if Length(Items_List.Element(TempRecord.Cargo.Element(I)(1)).IType) >= 4 then
                        if Slice(Items_List.Element(TempRecord.Cargo.Element(I)(1)).IType, 1, 4) = "Ammo" then
                            CombatValue := CombatValue + (Items_List.Element(TempRecord.Cargo.Element(I)(1)).Value * 10);
                        end if;
                    end if;
                end loop;
                TempRecord.CombatValue := CombatValue;
                ProtoShips_List.Append(New_Item => TempRecord);
                TempRecord := (Name => Null_Unbounded_String, Modules => TempModules, 
                    Accuracy => (0, 0), CombatAI => NONE, Evasion => (0, 0), LootMin => 1, 
                    LootMax => 100, Perception => (0, 0), Cargo => TempCargo, CombatValue => 1,
                    Crew => TempCrew, Description => Null_Unbounded_String, Owner => Poleis);
                TempRecord.Name := Null_Unbounded_String;
            end if;
        end loop;
        Close(ShipsFile);
        return True;
    end LoadShips;

    function CountShipWeight(Ship : ShipRecord) return Positive is
        Weight : Natural := 0;
        CargoWeight : Positive;
    begin
        for I in Ship.Modules.First_Index..Ship.Modules.Last_Index loop
            Weight := Weight + Ship.Modules.Element(I).Weight;
        end loop;
        for I in Ship.Cargo.First_Index..Ship.Cargo.Last_Index loop
            CargoWeight := Ship.Cargo.Element(I).Amount * Items_List.Element(Ship.Cargo.Element(I).ProtoIndex).Weight;
            Weight := Weight + CargoWeight;
        end loop;
        return Weight;
    end CountShipWeight;

    function RealSpeed(Ship : ShipRecord) return Natural is
        Weight : Positive;
        BaseSpeed : Natural := 0;
        Speed : Integer := 0;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
    begin
        if Ship = PlayerShip then
            if not HaveOrderRequirements(False) then
                return 0;
            end if;
        end if;
        Weight := CountShipWeight(Ship) / 500;
        for I in Ship.Modules.First_Index..Ship.Modules.Last_Index loop
            if Modules_List.Element(Ship.Modules.Element(I).ProtoIndex).Mtype = ENGINE then
                BaseSpeed := Ship.Modules.Element(I).Max_Value * 100;
                Damage := 1.0 - DamageFactor(Float(Ship.Modules.Element(I).Durability) / Float(Ship.Modules.Element(I).MaxDurability));
                Speed := Speed + (BaseSpeed - Integer(Float(BaseSpeed) * Float(Damage)));
            end if;
        end loop;
        Speed := Speed - Integer((Float(Weight) / 100.0) * Float(Speed));
        for I in Ship.Crew.First_Index..Ship.Crew.Last_Index loop
            if Ship.Crew.Element(I).Order = Pilot then
                Speed := Speed + Integer(Float(Speed) * (Float(GetSkillLevel(I, 1)) / 300.0));
            elsif Ship.Crew.Element(I).Order = Engineer then
                Speed := Speed + Integer(Float(Speed) * (Float(GetSkillLevel(I, 2)) / 300.0));
            end if;
        end loop;
        case Ship.Speed is
            when QUARTER_SPEED =>
                Speed := Integer(Float(Speed) * 0.25);
            when HALF_SPEED =>
                Speed := Integer(Float(Speed) * 0.5);
            when FULL_SPEED =>
                null;
            when others =>
                Speed := 0;
        end case;
        if Speed < 0 then
            Speed := 0;
        end if;
        Speed := (Speed / 60);
        return Speed;
    end RealSpeed;

    procedure StartUpgrading(ModuleIndex, UpgradeType : Positive) is
        MaxValue : Natural;
        HaveMaterials, HaveTools : Boolean := False;
        UpgradeProgress : Positive;
        UpgradeAction : ShipUpgrade;
    begin
        if PlayerShip.Modules.Element(ModuleIndex).Durability = 0 and UpgradeType /= 3 then
            ShowDialog("You can't upgrade " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                " because is destroyed.");
            return;
        end if;
        case UpgradeType is
            when 1 => -- Upgrade durability
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Durability) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).MaxDurability = MaxValue then
                    ShowDialog("You can't improve more durability of " &
                        To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                    return;
                end if;
                UpgradeAction := DURABILITY;
                UpgradeProgress := 10;
            when 2 => -- Upgrade various max value of selected module
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                    when ENGINE =>
                        if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                            ShowDialog("You can't improve more power of " &
                                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                            return;
                        end if;
                        UpgradeProgress := 10;
                    when CABIN =>
                        if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                            ShowDialog("You can't improve more quality of " &
                                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                            return;
                        end if;
                        UpgradeProgress := 100;
                    when GUN | BATTERING_RAM =>
                        if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                            ShowDialog("You can't improve more damage of " &
                                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                            return;
                        end if;
                        UpgradeProgress := 100;
                    when HULL =>
                        if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                            ShowDialog("You can't enlarge more " &
                                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                            return;
                        end if;
                        UpgradeProgress := 500;
                    when others =>
                        ShowDialog(To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & 
                            " can't be upgraded in that way.");
                        return;
                end case;
                UpgradeAction := MAX_VALUE;
            when 3 => -- Upgrade various value of selected module
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Value) * 1.5);
                case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                    when ENGINE =>
                        if PlayerShip.Modules.Element(ModuleIndex).Current_Value = MaxValue then
                            ShowDialog("You can't reduce more fuel usage of " &
                                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                            return;
                        end if;
                        UpgradeProgress := 100;
                    when others =>
                        ShowDialog(To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & 
                            " can't be upgraded in that way.");
                        return;
                end case;
                UpgradeAction := VALUE;
            when 4 => -- Continue previous upgrade
                if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction = NONE then
                    ShowDialog(To_String(PlayerShip.Modules.Element(ModuleIndex).Name)
                        & " don't have set any upgrade yet.");
                    return;
                end if;
                UpgradeAction := PlayerShip.Modules.Element(ModuleIndex).UpgradeAction;
            when others =>
                return;
        end case;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if Items_List(PlayerShip.Cargo.Element(I).ProtoIndex).IType =
                Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).RepairMaterial
            then
                HaveMaterials := True;
            elsif Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = To_Unbounded_String("RepairTools") then
                HaveTools := True;
            end if;
            exit when HaveMaterials and HaveTools;
        end loop;
        if not HaveMaterials then
            for I in Items_List.First_Index..Items_List.Last_Index loop
                if Items_List(I).IType = Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).RepairMaterial then
                    ShowDialog("You don't have " & To_String(Items_List(I).Name) & " for upgrading " &
                        To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
                    return;
                end if;
            end loop;
        end if;
        if not HaveTools then
            ShowDialog("You don't have repair tools for upgrading " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
            return;
        end if;
        PlayerShip.UpgradeModule := ModuleIndex;
        if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /= UpgradeAction then
            UpdateModule(PlayerShip, ModuleIndex, "UpgradeProgress", Integer'Image(UpgradeProgress));
            UpdateModule(PlayerShip, ModuleIndex, "UpgradeAction", ShipUpgrade'Image(UpgradeAction));
        end if;
        AddMessage("You set " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
            " to upgrade.", OrderMessage);
    end StartUpgrading;

    procedure UpgradeShip(Minutes : Positive) is
        ResultAmount, UpgradePoints, WorkerIndex, UpgradeMaterial, UpgradeProgress, UpgradeTools : Natural := 0;
        MaxValue : Positive;
        WeightGain : Natural;
        Times : Natural := 0;
        OrderTime, CurrentMinutes : Integer;
        procedure UpdateMember(Member : in out Member_Data) is
        begin
            Member.OrderTime := OrderTime;
        end UpdateMember;
    begin
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Upgrading then
                WorkerIndex := I;
                exit;
            end if;
        end loop;
        if WorkerIndex = 0 then
            return;
        end if;
        CurrentMinutes := Minutes;
        OrderTime := PlayerShip.Crew.Element(WorkerIndex).OrderTime;
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
        PlayerShip.Crew.Update_Element(Index => WorkerIndex, Process => UpdateMember'Access);
        if Times = 0 then
            return;
        end if;
        UpgradePoints := 
            ((GetSkillLevel(WorkerIndex, Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).RepairSkill) / 10) * Times) + Times;
        while UpgradePoints > 0 and PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress > 0 loop
            ResultAmount := UpgradePoints;
            if ResultAmount > PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress then
                ResultAmount := PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress;
            end if;
            UpgradeMaterial := 0;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if Items_List(PlayerShip.Cargo.Element(I).ProtoIndex).IType =
                    Modules_List(PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex).RepairMaterial
                then
                    UpgradeMaterial := I;
                elsif Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = To_Unbounded_String("RepairTools") then
                    UpgradeTools := I;
                end if;
                exit when UpgradeMaterial > 0 and UpgradeTools > 0;
            end loop;
            if UpgradeMaterial = 0 then
                AddMessage("You don't have enough materials to upgrade " &
                    To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name), OrderMessage);
                GiveOrders(WorkerIndex, Rest);
                exit;
            end if;
            if UpgradeTools = 0 then
                AddMessage("You don't have repair tools to upgrade " & 
                    To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name), OrderMessage);
                GiveOrders(WorkerIndex, Rest);
                exit;
            end if;
            if ResultAmount > PlayerShip.Cargo.Element(UpgradeMaterial).Amount then
                ResultAmount := PlayerShip.Cargo.Element(UpgradeMaterial).Amount;
            end if;
            GainExp(ResultAmount, Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).RepairSkill,
                WorkerIndex);
            DamageCargo(UpgradeTools, WorkerIndex, 
                Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).RepairSkill);
            UpgradeProgress := PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress - ResultAmount;
            UpgradePoints := UpgradePoints - ResultAmount;
            UpdateCargo(PlayerShip, PlayerShip.Cargo.Element(UpgradeMaterial).ProtoIndex, (0 - ResultAmount));
            if UpgradeProgress = 0 then
                WeightGain := Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).Weight
                    / Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).Durability;
                if WeightGain < 1 then
                    WeightGain := 1;
                end if;
                case PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeAction is
                    when DURABILITY =>
                        UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "MaxDurability", "1");
                        UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "Weight", Natural'Image(WeightGain));
                        AddMessage(To_String(PlayerShip.Crew.Element(WorkerIndex).Name)
                            & " was upgraded durability of " & 
                            To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name) & 
                            ".", OrderMessage);
                        MaxValue := 
                            Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).Durability) 
                            * 1.5);
                        if PlayerShip.Modules.Element(PlayerShip.UpgradeModule).MaxDurability = MaxValue then
                            AddMessage("You reached maximum durability for " &
                                To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name)
                                & ".", OrderMessage);
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "0");
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeAction", "NONE");
                            PlayerShip.UpgradeModule := 0;
                            GiveOrders(WorkerIndex, Rest);
                            return;
                        else
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "10");
                        end if;
                    when MAX_VALUE =>
                        UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "Max_Value", "1");
                        case Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MType is
                            when HULL =>
                                WeightGain := WeightGain * 10;
                            when ENGINE =>
                                WeightGain := 1;
                            when others =>
                                null;
                        end case;
                        UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "Weight", Natural'Image(WeightGain));
                        AddMessage(To_String(PlayerShip.Crew.Element(WorkerIndex).Name)
                            & " was upgraded " & To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name) & 
                            ".", OrderMessage);
                        MaxValue :=
                            Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MaxValue) 
                            * 1.5);
                        if PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Max_Value = MaxValue then
                            AddMessage("You reached maximum upgrade for " &
                                To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name)
                                & ".", OrderMessage);
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "0");
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeAction", "NONE");
                            PlayerShip.UpgradeModule := 0;
                            GiveOrders(WorkerIndex, Rest);
                            return;
                        else
                            case Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MType is
                                when ENGINE =>
                                    UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "10");
                                when CABIN =>
                                    UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "100");
                                when GUN | BATTERING_RAM =>
                                    UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "100");
                                when HULL =>
                                    UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "500");
                                when others =>
                                    null;
                            end case;
                        end if;
                    when VALUE =>
                        UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "Current_Value",
                            Integer'Image(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Current_Value - 1));
                        case Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MType is
                            when ENGINE =>
                                WeightGain := WeightGain * 10;
                            when others =>
                                null;
                        end case;
                        UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "Weight", Natural'Image(WeightGain));
                        AddMessage(To_String(PlayerShip.Crew.Element(WorkerIndex).Name)
                            & " was upgraded " & To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name) & 
                            ".", OrderMessage);
                        MaxValue :=
                            Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).Value) 
                            / 2.0);
                        if PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Current_Value = MaxValue then
                            AddMessage("You reached maximum upgrade for " &
                                To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name)
                                & ".", OrderMessage);
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "0");
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeAction", "NONE");
                            PlayerShip.UpgradeModule := 0;
                            GiveOrders(WorkerIndex, Rest);
                            return;
                        else
                            case Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MType is
                                when ENGINE =>
                                    UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "100");
                                when others =>
                                    null;
                            end case;
                        end if;
                    when others =>
                        null;
                end case;
            else
                UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", Integer'Image(UpgradeProgress));
            end if;
        end loop;
    end UpgradeShip;

    procedure RepairShip(Minutes : Positive) is
        OrderTime, CurrentMinutes, RepairPoints : Integer;
        RepairNeeded, RepairStopped : Boolean := False;
        package Natural_Container is new Vectors(Positive, Natural);
        CrewRepairPoints : Natural_Container.Vector;
        procedure UpdateMember(Member : in out Member_Data) is
        begin
            Member.OrderTime := OrderTime;
        end UpdateMember;
        procedure UpdatePoints(Points : in out Natural) is
        begin
            Points := RepairPoints;
        end UpdatePoints;
        procedure RepairModule(ModuleIndex : Positive) is
            PointsIndex, PointsBonus, RepairMaterial, ToolsIndex : Natural;
            ProtoIndex, RepairValue : Positive;
        begin
            PointsIndex := 0;
            RepairNeeded := True;
            RepairStopped := False;
            for J in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(J).Order = Repair then
                    PointsIndex := PointsIndex + 1;
                    if CrewRepairPoints(PointsIndex) > 0 then
                        PointsBonus := (GetSkillLevel(J, 
                            Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairSkill) /  10) * 
                            CrewRepairPoints(PointsIndex);
                        RepairPoints := CrewRepairPoints(PointsIndex) + PointsBonus;
                        RepairMaterial := 0;
                        ToolsIndex := 0;
                        for K in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                            if Items_List.Element(PlayerShip.Cargo.Element(K).ProtoIndex).IType = 
                                Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairMaterial 
                            then
                                ProtoIndex := PlayerShip.Cargo.Element(K).ProtoIndex;
                                RepairMaterial := K;
                                -- Limit repair point depends on amount of repair materials
                                if PlayerShip.Cargo.Element(K).Amount < RepairPoints then
                                    RepairPoints := PlayerShip.Cargo.Element(K).Amount;
                                end if;
                            elsif Items_List.Element(PlayerShip.Cargo.Element(K).ProtoIndex).IType = To_Unbounded_String("RepairTools") 
                            then
                                ToolsIndex := K;
                            end if;
                            exit when RepairMaterial > 0 and ToolsIndex > 0;
                        end loop;
                        if RepairMaterial = 0 then
                            AddMessage("You don't have repair materials to continue repairs of " & 
                                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".", OrderMessage);
                            RepairStopped := True;
                            return;
                        end if;
                        if ToolsIndex = 0 then
                            if PointsIndex = 1 then
                                AddMessage("You don't have repair tools to continue repairs of " & 
                                    To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".", OrderMessage);
                            else
                                AddMessage(To_String(PlayerShip.Crew.Element(J).Name) & 
                                    " can't continue repairs due to lack of repair tools.", OrderMessage);
                            end if;
                            RepairStopped := True;
                            return;
                        end if;
                        -- Repair module
                        if PlayerShip.Modules.Element(ModuleIndex).Durability + RepairPoints >=
                            PlayerShip.Modules.Element(ModuleIndex).MaxDurability 
                        then
                            RepairValue := PlayerShip.Modules.Element(ModuleIndex).MaxDurability - 
                            PlayerShip.Modules.Element(ModuleIndex).Durability;
                            RepairNeeded := False;
                        else
                            RepairValue := RepairPoints;
                        end if;
                        UpdateCargo(PlayerShip, ProtoIndex, (0 - RepairValue));
                        UpdateModule(PlayerShip, ModuleIndex, "Durability", Integer'Image(RepairValue));
                        if RepairValue > CrewRepairPoints(PointsIndex) then
                            RepairValue := CrewRepairPoints(PointsIndex);
                            RepairPoints := 0;
                        else
                            RepairPoints := CrewRepairPoints(PointsIndex) - RepairValue;
                        end if;
                        GainExp(RepairValue, Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairSkill, J);
                        CrewRepairPoints.Update_Element(Index => PointsIndex, Process => UpdatePoints'Access);
                        DamageCargo(ToolsIndex, J, Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairSkill);
                        exit when not RepairNeeded;
                    end if;
                end if;
            end loop;
        end RepairModule;
    begin
        for J in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(J).Order = Repair then
                CurrentMinutes := Minutes;
                OrderTime := PlayerShip.Crew.Element(J).OrderTime;
                RepairPoints := 0;
                while CurrentMinutes > 0 loop
                    if CurrentMinutes >= OrderTime then
                        CurrentMinutes := CurrentMinutes - OrderTime;
                        RepairPoints := RepairPoints + 1;
                        OrderTime := 15;
                    else
                        OrderTime := OrderTime - CurrentMinutes;
                        CurrentMinutes := 0;
                    end if;
                end loop;
                CrewRepairPoints.Append(New_Item => RepairPoints);
                PlayerShip.Crew.Update_Element(Index => J, Process => UpdateMember'Access);
            end if;
        end loop;
        if CrewRepairPoints.Length = 0 then
            return;
        end if;
        if PlayerShip.RepairModule > 0 then
            if PlayerShip.Modules.Element(PlayerShip.RepairModule).Durability < 
                PlayerShip.Modules.Element(PlayerShip.RepairModule).MaxDurability
            then
                RepairModule(PlayerShip.RepairModule);
            end if;
        end if;
        Repair_Loop:
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                RepairModule(I);
            end if;
        end loop Repair_Loop;
        -- Send repair team on break if all is ok
        if not RepairNeeded or RepairStopped then
            if not RepairNeeded then
                AddMessage("All repairs are finished.", OrderMessage);
            end if;
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = Repair then
                    GiveOrders(I, Rest);
                end if;
            end loop;
        end if;
    end RepairShip;

    function GenerateShipName(Owner : Bases_Owners := Any) return Unbounded_String is -- based on name generator from libtcod
        NewName : Unbounded_String := Null_Unbounded_String;
        LettersAmount, NumbersAmount : Positive;
        subtype Letters is Character range 'A'..'Z';
        subtype Numbers is Character range '0'..'9';
    begin
        case Owner is
            when Any => 
                NewName := ShipSyllablesStart.Element(GetRandom(ShipSyllablesStart.First_Index, ShipSyllablesStart.Last_Index));
                if GetRandom(1, 100) < 51 then
                    Append(NewName, ShipSyllablesMiddle.Element(GetRandom(ShipSyllablesMiddle.First_Index, ShipSyllablesMiddle.Last_Index)));
                end if;
                Append(NewName,ShipSyllablesEnd.Element(GetRandom(ShipSyllablesEnd.First_Index, ShipSyllablesEnd.Last_Index)));
            when Drones =>
                LettersAmount := GetRandom(2, 5);
                for I in 1..LettersAmount loop
                    Append(NewName, Letters'Val(GetRandom(Letters'Pos(Letters'First), Letters'Pos(Letters'Last))));
                end loop;
                Append(NewName, '-');
                NumbersAmount := GetRandom(2, 4);
                for I in 1..NumbersAmount loop
                    Append(NewName, Numbers'Val(GetRandom(Numbers'Pos(Numbers'First), Numbers'Pos(Numbers'Last))));
                end loop;
            when others =>
                null;
        end case;
        return NewName;
    end GenerateShipName;

    function GetSkillLevel(MemberIndex, SkillIndex : Positive; Ship : ShipRecord := PlayerShip) return Natural is
        SkillLevel : Integer := 0;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
        BaseSkillLevel : Natural;
    begin
        for I in Ship.Crew.Element(MemberIndex).Skills.First_Index..Ship.Crew.Element(MemberIndex).Skills.Last_Index loop
            if Ship.Crew.Element(MemberIndex).Skills.Element(I)(1) = SkillIndex then
                BaseSkillLevel := Ship.Crew.Element(MemberIndex).Skills.Element(I)(2);
                Damage := 1.0 - DamageFactor(Float(Ship.Crew.Element(MemberIndex).Health) / 100.0);
                SkillLevel := SkillLevel + (BaseSkillLevel - Integer(Float(BaseSkillLevel) * Float(Damage)));
                if Ship.Crew.Element(MemberIndex).Thirst > 40 then
                    Damage := 1.0 - DamageFactor(Float(Ship.Crew.Element(MemberIndex).Thirst) / 100.0);
                    SkillLevel := SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
                end if;
                if Ship.Crew.Element(MemberIndex).Hunger > 80 then
                    Damage := 1.0 - DamageFactor(Float(Ship.Crew.Element(MemberIndex).Hunger) / 100.0);
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

    procedure Death(MemberIndex : Positive; Reason : Unbounded_String; Ship : in out ShipRecord) is
        procedure UpdateDeath(Member : in out Member_Data) is
        begin
            Member.Order := Rest;
            Member.Health := 0;
        end UpdateDeath;
    begin
        if MemberIndex > 1 then
            if Ship = PlayerShip then
                AddMessage(To_String(Ship.Crew.Element(MemberIndex).Name) & " died from " &
                    To_String(Reason) & ".", CombatMessage);
            end if;
            Ship.Cargo.Append(New_Item => (ProtoIndex => 40, Amount => 1, Name => Ship.Crew.Element(MemberIndex).Name &
                To_Unbounded_String("'s corpse"), Durability => 100));
            DeleteMember(MemberIndex, Ship);
        else
            if Ship = PlayerShip then
                AddMessage("You died from " & To_String(Reason) & ".", CombatMessage);
                PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateDeath'Access);
            else
                Ship.Cargo.Append(New_Item => (ProtoIndex => 40, Amount => 1, Name => Ship.Crew.Element(MemberIndex).Name &
                    To_Unbounded_String("'s corpse"), Durability => 100));
                DeleteMember(MemberIndex, Ship);
            end if;
        end if;
    end Death;

    procedure DeleteMember(MemberIndex : Positive; Ship : in out ShipRecord) is
    begin
        Ship.Crew.Delete(Index => MemberIndex, Count => 1);
        for I in Ship.Modules.First_Index..Ship.Modules.Last_Index loop
            if Ship.Modules.Element(I).Owner = MemberIndex then
                UpdateModule(Ship, I, "Owner", "0");
            elsif Ship.Modules.Element(I).Owner > MemberIndex then
                UpdateModule(Ship, I, "Owner", Positive'Image(Ship.Modules.Element(I).Owner - 1));
            end if;
        end loop;
    end DeleteMember;

    function FindMember(Order : Crew_Orders; Ship : ShipRecord := PlayerShip) return Natural is
    begin
        for I in Ship.Crew.First_Index..Ship.Crew.Last_Index loop
            if Ship.Crew.Element(I).Order = Order then
                return I;
            end if;
        end loop;
        return 0;
    end FindMember;

end Ships;
