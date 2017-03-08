--    Copyright 2017 Bartek thindil Jasicki
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

with Items; use Items;
with ShipModules; use ShipModules;
with Utils; use Utils;

package body Ships.Cargo is

    procedure UpdateCargo(Ship : in out ShipRecord; ProtoIndex : Positive; Amount : Integer; Durability : Natural := 100) is
        ItemIndex : Natural := 0;
        NewAmount : Natural;
        procedure UpdateItem(Item : in out CargoData) is
        begin
            Item.Amount := NewAmount;
        end UpdateItem;
    begin
        for I in Ship.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if Ship.Cargo.Element(I).ProtoIndex = ProtoIndex and Ship.Cargo.Element(I).Durability = Durability then
                ItemIndex := I;
                exit;
            end if;
        end loop;
        if ItemIndex = 0 then
            Ship.Cargo.Append(New_Item => (ProtoIndex => ProtoIndex, Amount => Amount, Name => Null_Unbounded_String, 
                Durability => Durability));
        else
            NewAmount := Ship.Cargo.Element(ItemIndex).Amount + Amount;
            if NewAmount < 1 then
                Ship.Cargo.Delete(Index => ItemIndex, Count => 1);
                for I in Ship.Modules.First_Index..Ship.Modules.Last_Index loop
                    if Modules_List.Element(Ship.Modules.Element(I).ProtoIndex).MType = GUN then
                        if Ship.Modules.Element(I).Current_Value > ItemIndex then
                            UpdateModule(Ship, I, "Current_Value", Natural'Image(Ship.Modules.Element(I).Current_Value - 1));
                        elsif Ship.Modules.Element(I).Current_Value = ItemIndex then
                            UpdateModule(Ship, I, "Current_Value", "0");
                        end if;
                    end if;
                end loop;
            else
                Ship.Cargo.Update_Element(Index => ItemIndex, Process => UpdateItem'Access);
            end if;
        end if;
    end UpdateCargo;

    function FreeCargo(Amount : Integer) return Integer is
        FreeCargo : Integer := 0;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).Mtype = ShipModules.CARGO 
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

    function FindCargo(ProtoIndex : Natural := 0; ItemType : Unbounded_String := Null_Unbounded_String) return Natural is
        CargoIndex : Natural := 0;
    begin
        if ProtoIndex > 0 then
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = ProtoIndex then
                    CargoIndex := I;
                    exit;
                end if;
            end loop;
        elsif ItemType /= Null_Unbounded_String then
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = ItemType then
                    CargoIndex := I;
                    exit;
                end if;
            end loop;
        end if;
        return CargoIndex;
    end FindCargo;

    function GetCargoName(CargoIndex : Positive) return String is
    begin
        if PlayerShip.Cargo.Element(CargoIndex).Name /= Null_Unbounded_String then
            return To_String(PlayerShip.Cargo.Element(CargoIndex).Name);
        end if;
        return To_String(Items_List.Element(PlayerShip.Cargo.Element(CargoIndex).ProtoIndex).Name);
    end GetCargoName;

    procedure DamageCargo(CargoIndex : Positive; CrewIndex, SkillIndex : Natural := 0) is
        DamageChance : Integer := 0;
        SelectedItem : constant CargoData := PlayerShip.Cargo.Element(CargoIndex);
        I : Positive := PlayerShip.Cargo.First_Index;
        NewAmount : Positive;
        procedure UpdateItem(DamagedItem : in out CargoData) is
        begin
            if DamagedItem.Amount > 1 and DamagedItem.Durability > 1 then
                DamagedItem.Amount := 1;
            end if;
            DamagedItem.Durability := DamagedItem.Durability - 1;
        end UpdateItem;
        procedure UpdateItemAmount(Item : in out CargoData) is
        begin
            Item.Amount := NewAmount;
        end UpdateItemAmount;
    begin
        if CrewIndex > 0 then
            DamageChance := Items_List.Element(SelectedItem.ProtoIndex).Value - (GetSkillLevel(CrewIndex, SkillIndex) / 5);
            if DamageChance < 0 then
                DamageChance := 0;
            end if;
        end if;
        if GetRandom(1, 100) > DamageChance then -- Cargo not damaged
            return;
        end if;
        if SelectedItem.Amount > 1 then
            PlayerShip.Cargo.Append(New_Item => (ProtoIndex => SelectedItem.ProtoIndex, Amount => (SelectedItem.Amount - 1), 
            Name => SelectedItem.Name, Durability => SelectedItem.Durability));
        end if;
        PlayerShip.Cargo.Update_Element(Index => CargoIndex, Process => UpdateItem'Access);
        if PlayerShip.Cargo.Element(CargoIndex).Durability = 0 then -- Cargo destroyed
            UpdateCargo(PlayerShip, PlayerShip.Cargo.Element(CargoIndex).ProtoIndex, -1, 0);
            return;
        end if;
        while I <= PlayerShip.Cargo.Last_Index loop
            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = PlayerShip.Cargo.Element(J).ProtoIndex and
                    PlayerShip.Cargo.Element(I).Durability = PlayerShip.Cargo.Element(J).Durability and
                    I /= J
                then
                    NewAmount := PlayerShip.Cargo.Element(I).Amount + PlayerShip.Cargo.Element(J).Amount;
                    PlayerShip.Cargo.Update_Element(Index => I, Process => UpdateItemAmount'Access);
                    PlayerShip.Cargo.Delete(Index => J, Count => 1);
                    I := I - 1;
                    exit;
                end if;
            end loop;
            I := I + 1;
        end loop;
    end DamageCargo;

end Ships.Cargo;
