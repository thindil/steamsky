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

    procedure UpdateCargo(Ship : in out ShipRecord; ProtoIndex : Positive; Amount : Integer) is
        ItemIndex : Natural := 0;
        NewAmount : Natural;
        procedure UpdateItem(Item : in out CargoData) is
        begin
            Item.Amount := NewAmount;
        end UpdateItem;
    begin
        for I in Ship.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if Ship.Cargo.Element(I).ProtoIndex = ProtoIndex then
                ItemIndex := I;
                exit;
            end if;
        end loop;
        if ItemIndex = 0 then
            Ship.Cargo.Append(New_Item => (ProtoIndex => ProtoIndex, Amount => Amount, Name => Null_Unbounded_String, Durability => 100));
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

    function GetCargoName(CargoIndex : Positive) return String is
    begin
        if PlayerShip.Cargo.Element(CargoIndex).Name /= Null_Unbounded_String then
            return To_String(PlayerShip.Cargo.Element(CargoIndex).Name);
        end if;
        return To_String(Items_List.Element(PlayerShip.Cargo.Element(CargoIndex).ProtoIndex).Name);
    end GetCargoName;

    procedure DamageCargo(CargoIndex : Positive; CrewIndex, SkillIndex : Natural := 0) is
        DamageChance : Integer := 0;
        procedure UpdateItem(DamagedItem : in out CargoData) is
        begin
            if DamagedItem.Durability - 1 = 0 then
                UpdateCargo(PlayerShip, CargoIndex, -1);
            else
                DamagedItem.Durability := DamagedItem.Durability - 1;
            end if;
        end UpdateItem;
    begin
        if CrewIndex > 0 then
            DamageChance := Items_List.Element(PlayerShip.Cargo.Element(CargoIndex).ProtoIndex).Value -
                (GetSkillLevel(CrewIndex, SkillIndex) / 5);
            if DamageChance < 0 then
                DamageChance := 0;
            end if;
        end if;
        if GetRandom(1, 100) <= DamageChance then
            PlayerShip.Cargo.Update_Element(Index => CargoIndex, Process => UpdateItem'Access);
        end if;
    end DamageCargo;

end Ships.Cargo;
