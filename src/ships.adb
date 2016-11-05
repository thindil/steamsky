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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Maps; use Maps;
with Messages; use Messages;
with Bases; use Bases;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;

package body Ships is

    function HaveOrderRequirements(ShowInfo : Boolean := True) return Boolean is
        HaveCockpit, HaveEngine, HavePilot, HaveEngineer : Boolean := False;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType = COCKPIT and PlayerShip.Modules.Element(I).Durability > 0 then
                HaveCockpit := True;
            elsif Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType = ENGINE and PlayerShip.Modules.Element(I).Durability > 0 
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
            UpdateCargo(1, FuelNeeded);
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
                UpdateGame(TimePassed);
            end if;
        end if;
        return 1;
    end MoveShip;

    procedure DockShip(Docking : Boolean) is
        BaseIndex : constant Natural := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
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
            PlayerShip.Speed := DOCKED;
            AddMessage("Ship docked to base " &
                To_String(SkyBases(BaseIndex).Name), OrderMessage);
            UpdateGame(10);
        else
            PlayerShip.Speed := QUARTER_SPEED;
            AddMessage("Ship undocked from base " &
                To_String(SkyBases(BaseIndex).Name), OrderMessage);
            UpdateGame(5);
        end if;
    end DockShip;

    procedure ChangeShipSpeed(SpeedValue : ShipSpeed) is
    begin
        if PlayerShip.Speed = DOCKED then
            ShowDialog("First undock from base before you set ship speed.");
            return;
        end if;
        if not HaveOrderRequirements then
            return;
        end if;
        if PlayerShip.Speed = SpeedValue then
            return;
        end if;
        PlayerShip.Speed := SpeedValue;
    end ChangeShipSpeed;

    procedure UpdateCargo(ProtoIndex : Positive; Amount : Integer) is
        ItemIndex : Natural := 0;
        NewAmount : Natural;
        procedure UpdateItem(Item : in out CargoData) is
        begin
            Item.Amount := NewAmount;
        end UpdateItem;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = ProtoIndex then
                ItemIndex := I;
                exit;
            end if;
        end loop;
        if ItemIndex = 0 then
            PlayerShip.Cargo.Append(New_Item => (ProtoIndex => ProtoIndex, Amount =>
                Amount));
        else
            NewAmount := PlayerShip.Cargo.Element(ItemIndex).Amount + Amount;
            if NewAmount < 1 then
                PlayerShip.Cargo.Delete(Index => ItemIndex, Count => 1);
            else
                PlayerShip.Cargo.Update_Element(Index => ItemIndex, Process => UpdateItem'Access);
            end if;
        end if;
    end UpdateCargo;

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
    
    function FreeCargo(Amount : Integer) return Integer is
        FreeCargo : Integer := 0;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).Mtype = CARGO 
                and PlayerShip.Modules.Element(I).Durability > 0 then
                FreeCargo := FreeCargo + PlayerShip.Modules.Element(I).Max_Value;
            end if;
        end loop;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            FreeCargo := FreeCargo - (Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Weight * 
                PlayerShip.Cargo.Element(I).Amount);
        end loop;
        FreeCargo := FreeCargo + Amount;
        return FreeCargo;
    end FreeCargo;

    function CreateShip(ProtoIndex : Positive; Name : Unbounded_String; X, Y:
        Integer; Speed : ShipSpeed; Enemy : Boolean := False) return ShipRecord is
        TmpShip : ShipRecord;
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector;
        ShipCrew : Crew_Container.Vector;
        NewName : Unbounded_String;
        TurretIndex, GunIndex, HullIndex, Amount : Natural := 0;
    begin
        if not Enemy then
            for I in ProtoShips_List.Element(ProtoIndex).Modules.First_Index..ProtoShips_List.Element(ProtoIndex).Modules.Last_Index loop
                ShipModules.Append(New_Item => (Name => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Name,
                ProtoIndex => ProtoShips_List.Element(ProtoIndex).Modules(I), 
                Weight => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Weight,
                Current_Value => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Value,
                Max_Value => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).MaxValue,
                Durability => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Durability,
                MaxDurability => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Durability,
                Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE));
            end loop;
            if Name = Null_Unbounded_String then
                NewName := ProtoShips_List.Element(ProtoIndex).Name;
            else
                NewName := Name;
            end if;
        else
            for I in Enemies_List.Element(ProtoIndex).Modules.First_Index..Enemies_List.Element(ProtoIndex).Modules.Last_Index loop
                ShipModules.Append(New_Item => (Name => Modules_List.Element(Enemies_List.Element(ProtoIndex).Modules(I)).Name,
                ProtoIndex => Enemies_List.Element(ProtoIndex).Modules(I), 
                Weight => Modules_List.Element(Enemies_List.Element(ProtoIndex).Modules(I)).Weight,
                Current_Value => Modules_List.Element(Enemies_List.Element(ProtoIndex).Modules(I)).Value,
                Max_Value => Modules_List.Element(Enemies_List.Element(ProtoIndex).Modules(I)).MaxValue,
                Durability => Modules_List.Element(Enemies_List.Element(ProtoIndex).Modules(I)).Durability,
                MaxDurability => Modules_List.Element(Enemies_List.Element(ProtoIndex).Modules(I)).Durability,
                Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE));
            end loop;
            if Name = Null_Unbounded_String then
                NewName := Enemies_List.Element(ProtoIndex).Name;
            else
                NewName := Name;
            end if;
        end if;
        TmpShip := (Name => NewName, SkyX => X, SkyY => Y, Speed => Speed,
            Modules => ShipModules, Cargo => ShipCargo, Crew => ShipCrew,
            UpgradeModule => 0, DestinationX => 0, DestinationY => 0);
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
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex, StartIndex, EndIndex, Amount : Natural;
        TempRecord : ProtoShipData;
        TempModules : Positive_Container.Vector;
        Enemy : Boolean := False;
    begin
        if ProtoShips_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/ships.dat") then
            return False;
        end if;
        TempRecord := (Name => Null_Unbounded_String, Modules => TempModules, 
            Accuracy => 1, CombatAI => NONE, Evasion => 1, LootMin => 1, LootMax => 100);
        Open(ShipsFile, In_File, "data/ships.dat");
        Amount := 1;
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
                    TempRecord.Accuracy := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Enemy") then
                    Enemy := True;
                elsif FieldName = To_Unbounded_String("CombatAI") then
                    TempRecord.CombatAI := ShipCombatAI'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Evasion") then
                    TempRecord.Evasion := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("LootMin") then
                    TempRecord.LootMin := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("LootMax") then
                    TempRecord.LootMax := Integer'Value(To_String(Value));
                end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
                if not Enemy then
                    ProtoShips_List.Append(New_Item => TempRecord);
                else
                    Enemies_List.Append(New_Item => TempRecord);
                    Enemy := False;
                end if;
                TempRecord := (Name => Null_Unbounded_String, Modules => TempModules, 
                    Accuracy => 1, CombatAI => NONE, Evasion => 1, LootMin => 1, LootMax => 100);
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

    function FindMoney return Natural is
        MoneyIndex : Natural := 0;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
                MoneyIndex := I;
                exit;
            end if;
        end loop;
        return MoneyIndex;
    end FindMoney;

    procedure StartUpgrading(ModuleIndex, UpgradeType : Positive) is
        MaxValue : Natural;
        HaveMaterials : Boolean := False;
        MaterialIndex : Positive;
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
                MaterialIndex := PlayerShip.Cargo.Element(I).ProtoIndex;
                exit;
            end if;
        end loop;
        if not HaveMaterials then
            for I in Items_List.First_Index..Items_List.Last_Index loop
                if Items_List(I).IType = Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).RepairMaterial then
                    MaterialIndex := I;
                    exit;
                end if;
            end loop;
            ShowDialog("You don't have " & To_String(Items_List(MaterialIndex).Name) & " for upgrading " &
                To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".");
        else
            PlayerShip.UpgradeModule := ModuleIndex;
            if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /= UpgradeAction then
                UpdateModule(PlayerShip, ModuleIndex, "UpgradeProgress", Integer'Image(UpgradeProgress));
                UpdateModule(PlayerShip, ModuleIndex, "UpgradeAction", ShipUpgrade'Image(UpgradeAction));
            end if;
            AddMessage("You set " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                " to upgrade.", OrderMessage);
        end if;
    end StartUpgrading;

    procedure UpgradeShip(Minutes : Positive) is
        ResultAmount, UpgradePoints, WorkerIndex, UpgradeMaterial, UpgradeProgress : Natural := 0;
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
                    exit;
                end if;
            end loop;
            if UpgradeMaterial = 0 then
                AddMessage("You don't have enough materials to upgrade " &
                To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name),
                    OrderMessage);
                GiveOrders(WorkerIndex, Rest);
                exit;
            end if;
            if ResultAmount > PlayerShip.Cargo.Element(UpgradeMaterial).Amount then
                ResultAmount := PlayerShip.Cargo.Element(UpgradeMaterial).Amount;
            end if;
            GainExp(ResultAmount, Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).RepairSkill,
                WorkerIndex);
            UpgradeProgress := PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress - ResultAmount;
            UpgradePoints := UpgradePoints - ResultAmount;
            UpdateCargo(PlayerShip.Cargo.Element(UpgradeMaterial).ProtoIndex, (0 - ResultAmount));
            if UpgradeProgress = 0 then
                WeightGain := Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).Weight
                    / 100;
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
                            GiveOrders(WorkerIndex, Rest);
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "0");
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeAction", "NONE");
                            PlayerShip.UpgradeModule := 0;
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
                            GiveOrders(WorkerIndex, Rest);
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "0");
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeAction", "NONE");
                            PlayerShip.UpgradeModule := 0;
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
                            GiveOrders(WorkerIndex, Rest);
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeProgress", "0");
                            UpdateModule(PlayerShip, PlayerShip.UpgradeModule, "UpgradeAction", "NONE");
                            PlayerShip.UpgradeModule := 0;
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
        RepairMaterial, PointsBonus, PointsIndex : Natural;
        ProtoIndex, RepairValue : Positive;
        OrderTime, CurrentMinutes, RepairPoints : Integer;
        RepairNeeded : Boolean := False;
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
        Repair_Loop:
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            RepairMaterial := 0;
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                PointsIndex := 0;
                RepairNeeded := True;
                for J in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(J).Order = Repair then
                        PointsIndex := PointsIndex + 1;
                        if CrewRepairPoints(PointsIndex) > 0 then
                            PointsBonus := (GetSkillLevel(J, Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairSkill) / 
                                10) * CrewRepairPoints(PointsIndex);
                            RepairPoints := CrewRepairPoints(PointsIndex) + PointsBonus;
                            for K in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                                if Items_List.Element(PlayerShip.Cargo.Element(K).ProtoIndex).IType = 
                                    Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairMaterial then
                                    ProtoIndex := PlayerShip.Cargo.Element(K).ProtoIndex;
                                    RepairMaterial := J;
                                    -- Limit repair point depends on amount of repair materials
                                    if PlayerShip.Cargo.Element(K).Amount < RepairPoints then
                                        RepairPoints := PlayerShip.Cargo.Element(K).Amount;
                                    end if;
                                    exit;
                                end if;
                            end loop;
                            if RepairMaterial = 0 then
                                AddMessage("You don't have repair materials to continue repairs.",
                                    OrderMessage);
                                for K in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                                    if PlayerShip.Crew.Element(K).Order = Repair then
                                        GiveOrders(K, Rest);
                                    end if;
                                end loop;
                                exit Repair_Loop;
                            end if;
                            -- Repair module
                            if PlayerShip.Modules.Element(I).Durability + RepairPoints > PlayerShip.Modules.Element(I).MaxDurability then
                                RepairValue := PlayerShip.Modules.Element(I).MaxDurability - PlayerShip.Modules.Element(I).Durability;
                                RepairNeeded := False;
                            else
                                RepairValue := RepairPoints;
                            end if;
                            UpdateCargo(ProtoIndex, (0 - RepairValue));
                            UpdateModule(PlayerShip, I, "Durability", Integer'Image(RepairValue));
                            if RepairValue > CrewRepairPoints(PointsIndex) then
                                RepairValue := CrewRepairPoints(PointsIndex);
                                RepairPoints := 0;
                            else
                                RepairPoints := CrewRepairPoints(PointsIndex) - RepairValue;
                            end if;
                            GainExp(RepairValue, Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairSkill, J);
                            CrewRepairPoints.Update_Element(Index => PointsIndex, Process => UpdatePoints'Access);
                            exit when not RepairNeeded;
                        end if;
                    end if;
                end loop;
            end if;
        end loop Repair_Loop;
        -- Send repair team on break if all is ok
        if not RepairNeeded then
            AddMessage("All repairs are finished.", OrderMessage);
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = Repair then
                    GiveOrders(I, Rest);
                end if;
            end loop;
        end if;
    end RepairShip;

end Ships;
