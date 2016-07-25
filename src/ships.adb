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

with Maps; use Maps;
with Messages; use Messages;
with Bases; use Bases;
with Prototypes; use Prototypes;
with UserInterface; use UserInterface;

package body Ships is

    function MoveShip(ShipIndex, X, Y: Integer) return Boolean is
        NewX, NewY : Integer;
        HavePilot, HaveEngineer : Boolean := False;
        FuelNeeded : Integer;
    begin
        if ShipIndex = 0 then
            if PlayerShip.Speed < QUARTER_SPEED then
                return False;
            end if;
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = Pilot then
                    HavePilot := True;
                end if;
                if PlayerShip.Crew.Element(I).Order = Engineer then
                    HaveEngineer := True;
                end if;
            end loop;
            if not HavePilot or not HaveEngineer then
                return False;
            end if;
            case PlayerShip.Speed is
                when QUARTER_SPEED =>
                    FuelNeeded := -1;
                when HALF_SPEED =>
                    FuelNeeded := -2;
                when FULL_SPEED =>
                    FuelNeeded := -4;
                when others =>
                    return False;
            end case;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop -- Check for fuel
                if PlayerShip.Cargo.Element(I).ProtoIndex = 1 and
                    PlayerShip.Cargo.Element(I).Amount < abs FuelNeeded then
                    return False;
                end if;
            end loop;
            NewX := PlayerShip.SkyX + X;
            NewY := PlayerShip.SkyY + Y;
        end if;
        if NewX < 1 or NewX > 1024 or NewY < 1 or NewY > 1024 then
            return False;
        end if;
        if ShipIndex = 0 then
            PlayerShip.SkyX := NewX;
            PlayerShip.SkyY := NewY;
            UpdateCargo(1, FuelNeeded);
            case PlayerShip.Speed is
                when QUARTER_SPEED =>
                    UpdateGame(120);
                when HALF_SPEED =>
                    UpdateGame(60);
                when FULL_SPEED =>
                    UpdateGame(30);
                when others =>
                    null;
            end case;
        end if;
        return True;
    end MoveShip;

    procedure DockShip(Docking : Boolean) is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
    begin
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
            return;
        end if;
        if Docking and PlayerShip.Speed = DOCKED then
            return;
        end if;
        if not Docking and PlayerShip.Speed > DOCKED then
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
        if PlayerShip.Speed = DOCKED or PlayerShip.Speed = SpeedValue then
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

    procedure ShowShipInfo is
        Weight : Integer;
        CargoWeight : Positive;
    begin
        Weight := 0;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Speed: ");
        case PlayerShip.Speed is
            when DOCKED =>
                Add(Str => "Stopped (Docked to base)");
            when FULL_STOP =>
                Add(Str => "Stopped");
            when QUARTER_SPEED =>
                Add(Str => "Quarter speed");
            when HALF_SPEED =>
                Add(Str => "Half speed");
            when FULL_SPEED =>
                Add(Str => "Full speed");
        end case;
        Move_Cursor(Line => 4, Column => 2);
        Add(Str => "STATUS:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Move_Cursor(Line => Line_Position(4 + I), Column => 2);
            Add(Str => To_String(PlayerShip.Modules.Element(I).Name) & ": ");
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                Add(Str => "Damaged");
            else
                Add(Str => "OK");
            end if;
            Weight := Weight + PlayerShip.Modules.Element(I).Weight;
        end loop;
        Move_Cursor(Line => 4, Column => (Columns / 2));
        Add(Str => "CARGO:");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            CargoWeight := PlayerShip.Cargo.Element(I).Amount * Objects_Prototypes(PlayerShip.Cargo.Element(I).ProtoIndex).Weight;
            Move_Cursor(Line => Line_Position(4 + I), Column => (Columns / 2));
            Add(Str => Positive'Image(PlayerShip.Cargo.Element(I).Amount) & "x" &
                To_String(Objects_Prototypes(PlayerShip.Cargo.Element(I).ProtoIndex).Name) & " (" &
                Positive'Image(CargoWeight) & "kg )");
            Weight := Weight + CargoWeight;
        end loop;
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Weight: " & Integer'Image(Weight) & "kg");
    end ShowShipInfo;

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
