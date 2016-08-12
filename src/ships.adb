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
with Crafts; use Crafts;

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
                ShowDialog("You dont have pilot on duty.");
                return;
            end if;
            if EngineerIndex = 0 then
                ShowDialog("You dont have engineer on duty.");
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
                    ShowDialog("You dont have enough fuel (charcollum).");
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

    procedure UpdateModule(ModuleIndex : Positive; Field : String; Value : Integer) is
        NewDurability : Integer;
        procedure UpdateMod(Module : in out ModuleData) is
        begin
            Module.Durability := NewDurability;
        end UpdateMod;
    begin
        if ModuleIndex > Positive(PlayerShip.Modules.Length) then
            return;
        end if;
        NewDurability := PlayerShip.Modules.Element(ModuleIndex).Durability;
        if Field = "Durability" then
            NewDurability := NewDurability + Value;
            if NewDurability < 0 then
                NewDurability := 0;
            end if;
        end if;
        PlayerShip.Modules.Update_Element(Index => ModuleIndex, Process => UpdateMod'Access);
    end UpdateModule;
    
    function FreeCargo(Amount : Integer) return Integer is
        FreeCargo : Integer := 0;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Mtype = CARGO then
                FreeCargo := FreeCargo + PlayerShip.Modules.Element(I).Max_Value;
            end if;
        end loop;
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            FreeCargo := FreeCargo - (Items.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Weight * 
                PlayerShip.Cargo.Element(I).Amount);
        end loop;
        FreeCargo := FreeCargo + Amount;
        return FreeCargo;
    end FreeCargo;

    procedure ShowShipInfo is
        Weight : Integer;
        CargoWeight : Positive;
    begin
        Weight := 0;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Name: " & To_String(PlayerShip.Name));
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Manufacturing: ");
        if PlayerShip.Craft = 0 then
            Add(Str => "Nothing");
        else
            Add(Str => To_String(Items.Element(Recipes(PlayerShip.Craft).ResultIndex).Name));
        end if;
        Move_Cursor(Line => 5, Column => 2);
        Add(Str => "STATUS:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Move_Cursor(Line => Line_Position(5 + I), Column => 2);
            Add(Str => To_String(PlayerShip.Modules.Element(I).Name) & ": ");
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                Add(Str => "Damaged");
            else
                Add(Str => "OK");
            end if;
            Weight := Weight + PlayerShip.Modules.Element(I).Weight;
        end loop;
        Move_Cursor(Line => 5, Column => (Columns / 2));
        Add(Str => "CARGO:");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            CargoWeight := PlayerShip.Cargo.Element(I).Amount * Items.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Weight;
            Move_Cursor(Line => Line_Position(5 + I), Column => (Columns / 2));
            Add(Str => Positive'Image(PlayerShip.Cargo.Element(I).Amount) & "x" &
                To_String(Items.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Name) & " (" &
                Positive'Image(CargoWeight) & "kg )");
            Weight := Weight + CargoWeight;
        end loop;
        Move_Cursor(Line => 2, Column => (Columns / 2));
        Add(Str => "Weight:" & Integer'Image(Weight) & "kg");
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
