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
with Crafts; use Crafts;
with ShipModules; use ShipModules;

package body Ships is

    procedure MoveShip(ShipIndex, X, Y: Integer) is
        NewX, NewY : Integer;
        PilotIndex, EngineerIndex : Natural := 0;
        FuelNeeded : Integer;
        TimePassed : Integer := 0;
    begin
        if ShipIndex = 0 then
            if PlayerShip.Speed = DOCKED then
                ShowDialog("First you must undock ship from base.");
                return;
            end if;
            if PlayerShip.Speed = FULL_STOP then
                ShowDialog("First you must set speed for ship.");
                return;
            end if;
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = Pilot then
                    PilotIndex := I;
                end if;
                if PlayerShip.Crew.Element(I).Order = Engineer then
                    EngineerIndex := I;
                end if;
            end loop;
            if PilotIndex = 0 then
                ShowDialog("You don't have pilot on duty.");
                return;
            end if;
            if EngineerIndex = 0 then
                ShowDialog("You don't have engineer on duty.");
                return;
            end if;
            case PlayerShip.Speed is
                when QUARTER_SPEED =>
                    FuelNeeded := -1;
                when HALF_SPEED =>
                    FuelNeeded := -2;
                when FULL_SPEED =>
                    FuelNeeded := -4;
                when others =>
                    return;
            end case;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop -- Check for fuel
                if PlayerShip.Cargo.Element(I).ProtoIndex = 1 and PlayerShip.Cargo.Element(I).Amount < abs FuelNeeded then
                    ShowDialog("You don't have enough fuel (charcollum).");
                    return;
                end if;
            end loop;
            NewX := PlayerShip.SkyX + X;
            NewY := PlayerShip.SkyY + Y;
        end if;
        if NewX < 1 or NewX > 1024 or NewY < 1 or NewY > 1024 then
            return;
        end if;
        if ShipIndex = 0 then
            PlayerShip.SkyX := NewX;
            PlayerShip.SkyY := NewY;
            UpdateCargo(1, FuelNeeded);
            case PlayerShip.Speed is
                when QUARTER_SPEED =>
                    TimePassed := 120;
                when HALF_SPEED =>
                    TimePassed := 60;
                when FULL_SPEED =>
                    TimePassed := 30;
                when others =>
                    null;
            end case;
            if TimePassed > 0 then
                TimePassed := TimePassed - Integer(Float'Floor(Float(TimePassed) *
                    (Float(PlayerShip.Crew.Element(PilotIndex).Skills(1, 1)) / 200.0)));
                TimePassed := TimePassed - Integer(Float'Floor(Float(TimePassed) *
                    (Float(PlayerShip.Crew.Element(EngineerIndex).Skills(2, 1)) / 200.0)));
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
        if Docking then
            PlayerShip.Speed := DOCKED;
            if not SkyBases(BaseIndex).Visited then
                SkyBases(BaseIndex).Visited := True;
            end if;
            AddMessage("Ship docked to base " & To_String(SkyBases(BaseIndex).Name));
            UpdateGame(10);
        else
            PlayerShip.Speed := QUARTER_SPEED;
            AddMessage("Ship undocked from base " & To_String(SkyBases(BaseIndex).Name));
            UpdateGame(5);
        end if;
    end DockShip;

    procedure ChangeShipSpeed(SpeedValue : ShipSpeed) is
    begin
        if PlayerShip.Speed = DOCKED then
            ShowDialog("First undock from base before you set ship speed.");
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
        NewDurability : Integer;
        NewName : Unbounded_String;
        procedure UpdateMod(Module : in out ModuleData) is
        begin
            Module.Durability := NewDurability;
            Module.Name := NewName;
        end UpdateMod;
    begin
        if ModuleIndex > Positive(Ship.Modules.Length) then
            return;
        end if;
        NewDurability := Ship.Modules.Element(ModuleIndex).Durability;
        NewName := Ship.Modules.Element(ModuleIndex).Name;
        if Field = "Durability" then
            NewDurability := NewDurability + Integer'Value(Value);
            if NewDurability < 0 then
                NewDurability := 0;
            end if;
        elsif Field = "Name" then
            NewName := To_Unbounded_String(Value);
        end if;
        Ship.Modules.Update_Element(Index => ModuleIndex, Process => UpdateMod'Access);
    end UpdateModule;
    
    function FreeCargo(Amount : Integer) return Integer is
        FreeCargo : Integer := 0;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).Mtype = CARGO then
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

    function CreateShip(ProtoIndex : Positive; Name : Unbounded_String; X, Y: Integer; Speed : ShipSpeed) return ShipRecord is
        TmpShip : ShipRecord;
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector;
        ShipCrew : Crew_Container.Vector;
        NewName : Unbounded_String;
    begin
        for I in ProtoShips_List.Element(ProtoIndex).Modules.First_Index..ProtoShips_List.Element(ProtoIndex).Modules.Last_Index loop
            ShipModules.Append(New_Item => (Name => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Name,
                ProtoIndex => ProtoShips_List.Element(ProtoIndex).Modules(I), 
                Weight => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Weight,
                Current_Value => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Value,
                Max_Value => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).MaxValue,
                Durability => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Durability,
                MaxDurability => Modules_List.Element(ProtoShips_List.Element(ProtoIndex).Modules(I)).Durability));
        end loop;
        if Name = Null_Unbounded_String then
            NewName := ProtoShips_List.Element(ProtoIndex).Name;
        else
            NewName := Name;
        end if;
        TmpShip := (Name => NewName, SkyX => X, SkyY => Y, Speed => Speed, Craft => 0,
            Modules => ShipModules, Cargo => ShipCargo, Crew => ShipCrew);
        return TmpShip;
    end CreateShip;

    function LoadShips return Boolean is
        ShipsFile : File_Type;
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex, StartIndex, EndIndex, Amount : Natural;
        TempRecord : ProtoShipData;
        TempModules : ProtoModules_Container.Vector;
    begin
        if ProtoShips_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/ships.dat") then
            return False;
        end if;
        TempRecord := (Name => Null_Unbounded_String, Modules => TempModules, 
            DamageRange => 1, Accuracy => 1);
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
                elsif FieldName = To_Unbounded_String("Amount") then
                    Amount := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Modules") then
                    StartIndex := 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := StartIndex + 1;
                        end if;
                        TempRecord.Modules.Append(New_Item => Integer'Value(Slice(Value, StartIndex, EndIndex - 1)));
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("DamageRange") then
                    TempRecord.DamageRange := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Accuracy") then
                    TempRecord.Accuracy := Integer'Value(To_String(Value));
                end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
                ProtoShips_List.Append(New_Item => TempRecord);
                TempRecord := (Name => Null_Unbounded_String, Modules => TempModules, 
                    DamageRange => 1, Accuracy => 1);
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
        Weight := CountShipWeight(Ship) / 500;
        for I in Ship.Modules.First_Index..Ship.Modules.Last_Index loop
            if Modules_List.Element(Ship.Modules.Element(I).ProtoIndex).Mtype = ENGINE then
                BaseSpeed := Ship.Modules.Element(I).Max_Value * 10;
                Damage := 1.0 - DamageFactor(Float(Ship.Modules.Element(I).Durability) / Float(Ship.Modules.Element(I).MaxDurability));
                exit;
            end if;
        end loop;
        Speed := BaseSpeed - Integer(Float(BaseSpeed) * Float(Damage));
        Speed := Speed - Integer((Float(Weight) / 100.0) * Float(BaseSpeed));
        for I in Ship.Crew.First_Index..Ship.Crew.Last_Index loop
            if Ship.Crew.Element(I).Order = Pilot then
                Speed := Speed + Integer(Float(BaseSpeed) * (Float(Ship.Crew.Element(I).Skills(2, 1)) / 200.0));
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

    procedure ShowShipInfo is
        Weight : Integer;
        DamagePercent : Natural;
    begin
        Weight := CountShipWeight(PlayerShip);
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Name: " & To_String(PlayerShip.Name));
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Manufacturing: ");
        if PlayerShip.Craft = 0 then
            Add(Str => "Nothing");
        else
            Add(Str => To_String(Items_List.Element(Recipes_List.Element(PlayerShip.Craft).ResultIndex).Name));
        end if;
        Move_Cursor(Line => 6, Column => 2);
        Add(Str => "STATUS:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Move_Cursor(Line => Line_Position(6 + I), Column => 2);
            Add(Str => To_String(PlayerShip.Modules.Element(I).Name) & ": ");
            DamagePercent := 100 -  Natural((Float(PlayerShip.Modules.Element(I).Durability) /
                Float(PlayerShip.Modules.Element(I).MaxDurability)) * 100.0);
            if DamagePercent = 0 then
                Add(Str => "Ok");
            elsif DamagePercent > 0 and DamagePercent < 20 then
                Add(Str => "Slighty damaged");
            elsif DamagePercent > 19 and DamagePercent < 50 then
                Add(Str => "Damaged");
            elsif DamagePercent > 49 and DamagePercent < 80 then
                Add(Str => "Heavily damaged");
            elsif DamagePercent > 79 and DamagePercent < 100 then
                Add(Str => "Almost destroyed");
            else
                Add(Str => "Destroyed");
            end if;
        end loop;
        Move_Cursor(Line => 4, Column => 2);
        Add(Str => "Weight:" & Integer'Image(Weight) & "kg");
    end ShowShipInfo;

    procedure ShowCargoInfo is
        CargoWeight : Positive;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            CargoWeight := PlayerShip.Cargo.Element(I).Amount * Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Weight;
            Move_Cursor(Line => Line_Position(1 + I), Column => 2);
            Add(Str => Positive'Image(PlayerShip.Cargo.Element(I).Amount) & "x" &
                To_String(Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Name) & " (" &
                Positive'Image(CargoWeight) & "kg )");
        end loop;
    end ShowCargoInfo;

    function ShipInfoKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                return Ship_Info;
        end case;
    end ShipInfoKeys;

end Ships;
