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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ships; use Ships;
with Crew; use Crew;
with UserInterface; use UserInterface;
with Messages; use Messages;

package body Combat is
    
    type Enemy_Record is -- Data structure for enemies
        record
            Name : Unbounded_String;
            Durability : Integer;
            MaxDurability : Natural;
            Damage : Positive;
            DamageRange : Natural;
            Accuracy : Positive;
            Distance : Natural;
            Speed : ShipSpeed; 
        end record;
    Enemy : Enemy_Record;
    PilotOrder, EngineerOrder, GunnerOrder : Positive;
    PilotOrders : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Go closer"), 
        To_Unbounded_String("Keep distance"), To_Unbounded_String("Evade"),
        To_Unbounded_String("Escape"));
    EngineerOrders : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Full stop"), 
        To_Unbounded_String("Quarter speed"), To_Unbounded_String("Half speed"),
        To_Unbounded_String("Full speed"));
    GunnerOrders : constant array (1..3) of Unbounded_String := (To_Unbounded_String("Don't shoot"),
        To_Unbounded_String("Precise fire"), To_Unbounded_String("Fire at will"));
    DistanceNames : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Close"),
        To_Unbounded_String("Short"), To_Unbounded_String("Medium"),
        To_Unbounded_String("Long"));
    Order : Crew_Orders;

    procedure StartCombat(EnemyType : Enemy_Types) is
    begin
        case EnemyType is
            when SmallPirateShip =>
                Enemy := (Name => To_Unbounded_String("Small pirates ship"),
                    Durability => 100, Damage => 5, DamageRange => 2, Accuracy
                    => 1, MaxDurability => 100, Distance => 4, Speed => HALF_SPEED);
            when SmallUndeadShip =>
                Enemy := (Name => To_Unbounded_String("Small undead ship"),
                    Durability => 100, Damage => 10, DamageRange => 1, Accuracy
                    => 1, MaxDurability => 100, Distance => 4, Speed => HALF_SPEED);
            when SmallDrone =>
                Enemy := (Name => To_Unbounded_String("Small clockwork drone"),
                    Durability => 50, Damage => 5, DamageRange => 1, Accuracy
                    => 1, MaxDurability => 50, Distance => 4, Speed => HALF_SPEED);
        end case;
        PilotOrder := 2;
        EngineerOrder := 3;
        GunnerOrder := 1;
    end StartCombat;

    procedure CombatTurn is
        type Roll_Range is range 1..100;
        package Rand_Roll is new Discrete_Random(Roll_Range);
        Generator : Rand_Roll.Generator;
        AccuracyBonus, EvadeBonus : Integer := 0;
        PilotIndex, EngineerIndex, GunnerIndex, WeaponIndex, AmmoIndex,
            ArmorIndex : Natural := 0;
        Shoots : Integer;
        HitChance : Integer;
        ShootMessage : Unbounded_String;
    begin
        Rand_Roll.Reset(Generator);
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            case PlayerShip.Crew.Element(I).Order is
                when Pilot =>
                    PilotIndex := I;
                when Engineer =>
                    EngineerIndex := I;
                when Gunner =>
                    GunnerIndex := I;
                when others =>
                    null;
            end case;
        end loop;
        if PilotIndex > 0 then
            case PilotOrder is
                when 1 =>
                    AccuracyBonus := 20;
                    EvadeBonus := -10;
                when 2 =>
                    AccuracyBonus := 10;
                    EvadeBonus := 0;
                when 3 =>
                    AccuracyBonus := 0;
                    EvadeBonus := 10;
                when 4 =>
                    AccuracyBonus := -10;
                    EvadeBonus := 20;
                when others =>
                    null;
            end case;
        end if;
        if EngineerIndex > 0 then
            case EngineerOrder is
                when 1 =>
                    AccuracyBonus := 40;
                    EvadeBonus := -40;
                when 2 =>
                    AccuracyBonus := AccuracyBonus + 10;
                    EvadeBonus := EvadeBonus - 10;
                when 4 =>
                    AccuracyBonus := AccuracyBonus - 10;
                    EvadeBonus := EvadeBonus + 10;
                when others =>
                    null;
            end case;
            ChangeShipSpeed(ShipSpeed'Val(EngineerOrder));
        end if;
        if PilotIndex > 0 and EngineerIndex > 0 then
            if PilotOrder < 4 and EngineerOrder < 4 and Enemy.Distance > 1 then
                Enemy.Distance := Enemy.Distance - 1;
            end if;
            if PilotOrder = 4 and EngineerOrder = 4 then
                Enemy.Distance := Enemy.Distance + 1;
            end if;
        end if;
        case Enemy.Distance is
            when 1 =>
                AccuracyBonus := AccuracyBonus + 20;
                EvadeBonus := EvadeBonus - 10;
            when 2 =>
                AccuracyBonus := AccuracyBonus + 10;
            when 4 =>
                AccuracyBonus := AccuracyBonus - 10;
                EvadeBonus := EvadeBonus + 10;
            when others =>
                null;
        end case;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Durability > 0 then
                case PlayerShip.Modules.Element(I).MType is
                    when GUN =>
                        WeaponIndex := I;
                    when ARMOR =>
                        ArmorIndex := I;
                    when others =>
                        null;
                end case;
            end if;
        end loop;
        if GunnerIndex = 0 then
            Shoots := -1;
        else
            case GunnerOrder is
                when 2 =>
                    AccuracyBonus := AccuracyBonus + 20;
                    Shoots := 2;
                when 3 =>
                    Shoots := 4;
                when others =>
                    Shoots := 0;
            end case;
        end if;
        if WeaponIndex = 0 then
            Shoots := -3;
        else
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = PlayerShip.Modules.Element(WeaponIndex).Current_Value then
                    AmmoIndex := I;
                    exit;
                end if;
            end loop;
        end if;
        if AmmoIndex = 0 then
            Shoots := -2;
        elsif PlayerShip.Cargo.Element(AmmoIndex).Amount < Shoots then
            Shoots := PlayerShip.Cargo.Element(AmmoIndex).Amount;
        end if;
        if Shoots = -3 then
            AddMessage("You don't have gun to shoot!");
        elsif Shoots = -2 then
            AddMessage("You don't have ammo to your gun!");
        elsif Shoots > 0 then -- Player attacks
            HitChance := AccuracyBonus + PlayerShip.Crew.Element(GunnerIndex).Skills(3, 1);
            for I in 1..Shoots loop
                ShootMessage := PlayerShip.Crew.Element(GunnerIndex).Name & To_Unbounded_String(" shoots to ") & 
                    Enemy.Name;
                if Integer(Rand_Roll.Random(Generator)) + HitChance > Integer(Rand_Roll.Random(Generator)) then
                    Enemy.Durability := Enemy.Durability - PlayerShip.Modules.Element(WeaponIndex).Max_Value;
                    ShootMessage := ShootMessage & To_Unbounded_String(" and hit.");
                else
                    ShootMessage := ShootMessage & To_Unbounded_String(" and miss.");
                end if;
                AddMessage(To_String(ShootMessage));
                if Enemy.Durability < 1 then
                    Enemy.Durability := 0;
                    Shoots := I;
                    AddMessage(To_String(Enemy.Name) & " is destroyed!");
                    exit;
                end if;
            end loop;
            UpdateCargo(PlayerShip.Cargo.Element(AmmoIndex).ProtoIndex, (1 - Shoots));
            GainExp(Shoots, 3, GunnerIndex);
        end if;
        if Enemy.Durability > 0 then -- Enemy attack
            HitChance := Enemy.Accuracy - EvadeBonus;
            ShootMessage := Enemy.Name & To_Unbounded_String(" attacks you and ");
            if Integer(Rand_Roll.Random(Generator)) + HitChance > Integer(Rand_Roll.Random(Generator)) then
                ShootMessage := ShootMessage & To_Unbounded_String("hits in ");
                if ArmorIndex > 0 then
                    UpdateModule(ArmorIndex, "Durability", (1 - Enemy.Damage));
                    ShootMessage := ShootMessage & To_Unbounded_String("armor.");
                end if;
            else
                ShootMessage := ShootMessage & To_Unbounded_String("miss.");
            end if;
            AddMessage(To_String(ShootMessage));
        end if;
        UpdateGame(1);
    end CombatTurn;

    function CombatOrders(Key : Key_Code) return GameStates is
        KeyMax : Key_Code;
        MemberIndex : Natural := 0;
        OrderIndex : Positive;
    begin
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Order then
                MemberIndex := I;
                exit;
            end if;
        end loop;
        if MemberIndex > 0 then
            case Order is
                when Pilot =>
                    KeyMax := Key_Code(PlayerShip.Crew.Length) + PilotOrders'Length + 96;
                when Engineer =>
                    KeyMax := Key_Code(PlayerShip.Crew.Length) + EngineerOrders'Length + 96;
                when Gunner =>
                    KeyMax := Key_Code(PlayerShip.Crew.Length) + GunnerOrders'Length + 96;
                when others =>
                    null;
            end case;
        else
            KeyMax := Key_Code(PlayerShip.Crew.Length) + 96;
        end if;
        if Key < 97 or Key > KeyMax then -- check if key is valid
            return Combat_Orders;
        end if;
        OrderIndex := Positive(Key - 96);
        if MemberIndex = 0 then -- assign someone to position
            GiveOrders(OrderIndex, Order);
        else
            case Order is
                when Pilot =>
                    if OrderIndex <= PilotOrders'Length then
                        PilotOrder := OrderIndex;
                        AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                            " was set on: " & To_String(PilotOrders(PilotOrder)));
                    else
                        GiveOrders((OrderIndex - PilotOrders'Length), Order);
                    end if;
                when Engineer =>
                    if OrderIndex <= EngineerOrders'Length then
                        EngineerOrder := OrderIndex;
                        AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                            " was set on: " & To_String(EngineerOrders(EngineerOrder)));
                    else
                        GiveOrders((OrderIndex - EngineerOrders'Length), Order);
                    end if;
                when Gunner =>
                    if OrderIndex <= GunnerOrders'Length then
                        GunnerOrder := OrderIndex;
                        AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                            " was set on: " & To_String(GunnerOrders(GunnerOrder)));
                    else
                        GiveOrders((OrderIndex - GunnerOrders'Length), Order);
                    end if;
                when others =>
                    null;
            end case;
        end if;
        DrawGame(Combat_State);
        return Combat_State;
    end CombatOrders;

    procedure ShowCombat is
        PilotName, EngineerName, GunnerName : Unbounded_String :=
            To_Unbounded_String("Vacant");
    begin
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            case PlayerShip.Crew.Element(I).Order is
                when Pilot =>
                    PilotName := PlayerShip.Crew.Element(I).Name;
                when Engineer =>
                    EngineerName := PlayerShip.Crew.Element(I).Name;
                when Gunner =>
                    GunnerName := PlayerShip.Crew.Element(I).Name;
                when others =>
                    null;
            end case;
        end loop;
        Move_Cursor(Line => 1, Column => 2);
        Add(Str => "Pilot: " & To_String(PilotName));
        if PilotName /= To_Unbounded_String("Vacant") then
            Add(Str => " -> " & To_String(PilotOrders(PilotOrder)));
        end if;
        Change_Attributes(Line => 1, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Engineer: " & To_String(EngineerName));
        if EngineerName /= To_Unbounded_String("Vacant") then
            Add(Str => " -> " & To_String(EngineerOrders(EngineerOrder)));
        end if;
        Change_Attributes(Line => 2, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Gunner: " & To_String(GunnerName));
        if GunnerName /= To_Unbounded_String("Vacant") then
            Add(Str => " -> " & To_String(GunnerOrders(GunnerOrder)));
        end if;
        Change_Attributes(Line => 3, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Line => 5, Column => 2);
        Add(Str => "Ship status:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Move_Cursor(Line => Line_Position(6 + I), Column => 2);
            Add(Str => To_String(PlayerShip.Modules.Element(I).Name) & ": ");
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                Add(Str => "Damaged");
            else
                Add(Str => "OK");
            end if;
        end loop;
        Move_Cursor(Line => 5, Column => (Columns / 2));
        Add(Str => "Enemy status:");
        Move_Cursor(Line => 7, Column => (Columns / 2));
        Add(Str => "Enemy: " & To_String(Enemy.Name));
        Move_Cursor(Line => 8, Column => (Columns / 2));
        Add(Str => "Distance: " & To_String(DistanceNames(Enemy.Distance)));
        Move_Cursor(Line => 9, Column => (Columns / 2));
        Add(Str => "Status: ");
        if Enemy.Durability = Enemy.MaxDurability then
            Add(Str => "Ok");
        else
            Add(Str => "Damaged");
        end if;
        Move_Cursor(Line => 10, Column => (Columns / 2));
        Add(Str => "Speed: ");
        case Enemy.Speed is
            when FULL_STOP =>
                Add(Str => "Stopped");
            when QUARTER_SPEED =>
                Add(Str => "Slow");
            when HALF_SPEED =>
                Add(Str => "Medium");
            when FULL_SPEED =>
                Add(Str => "Fast");
            when others =>
                null;
        end case;
        Move_Cursor(Line => 13, Column => (Columns / 2));
        if Enemy.Durability > 0 then
            Add(Str => "SPACE for next turn");
        else
            Add(Str => "SPACE for back to sky map");
        end if;
        Change_Attributes(Line => 13, Column => (Columns / 2),
            Count => 5, Color => 1);
        for I in -10..-1 loop
            Move_Cursor(Line => Lines + Line_Position(I), Column => 2);
            Add(Str => GetMessage((I + 1)));
        end loop;
        LastMessage := To_Unbounded_String("");
    end ShowCombat;

    procedure ShowOrdersMenu is
        OrdersWindow : Window;
        Line : Line_Position := 0;
        MemberIndex : Natural := 0;
    begin
        OrdersWindow := Create(10, 20, (Lines / 2) - 5, (Columns / 2) - 10);
        Box(OrdersWindow);
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Order then
                MemberIndex := I;
                exit;
            end if;
        end loop;
        if MemberIndex > 0 then
            case Order is
                when Pilot =>
                    for I in PilotOrders'Range loop
                        Move_Cursor(Win => OrdersWindow, Line => Line_Position(I), Column => 1);
                        Add(Win => OrdersWindow, Str => Character'Val(96 + I) &
                            " " & To_String(PilotOrders(I)));
                        Change_Attributes(Win => OrdersWindow, Line => Line_Position(I), Column => 1, 
                            Count => 1, Color => 1);
                    end loop;
                    Line := PilotOrders'Length;
                when Engineer =>
                    for I in EngineerOrders'Range loop
                        Move_Cursor(Win => OrdersWindow, Line => Line_Position(I), Column => 1);
                        Add(Win => OrdersWindow, Str => Character'Val(96 + I) &
                            " " & To_String(EngineerOrders(I)));
                        Change_Attributes(Win => OrdersWindow, Line => Line_Position(I), Column => 1, 
                            Count => 1, Color => 1);
                    end loop;
                    Line := EngineerOrders'Length;
                when Gunner =>
                    for I in GunnerOrders'Range loop
                        Move_Cursor(Win => OrdersWindow, Line => Line_Position(I), Column => 1);
                        Add(Win => OrdersWindow, Str => Character'Val(96 + I) &
                            " " & To_String(GunnerOrders(I)));
                        Change_Attributes(Win => OrdersWindow, Line => Line_Position(I), Column => 1, 
                            Count => 1, Color => 1);
                    end loop;
                    Line := GunnerOrders'Length;
                when others =>
                    null;
            end case;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if I /= MemberIndex then
                Line := Line + 1;
                Move_Cursor(Win => OrdersWindow, Line => Line, Column => 1);
                Add(Win => OrdersWindow, Str => Character'Val(96 + Integer(Line)) & 
                    " assign " & To_String(PlayerShip.Crew.Element(I).Name));
                Change_Attributes(Win => OrdersWindow, Line => Line, Column => 1, 
                    Count => 1, Color => 1);
            end if;
        end loop;
        Move_Cursor(Win => OrdersWindow, Line => 8, Column => 1);
        Add(Win => OrdersWindow, Str => "Quit");
        Change_Attributes(Win => OrdersWindow, Line => 8, Column => 1, 
            Count => 1, Color => 1);
        Refresh(OrdersWindow);
    end ShowOrdersMenu;

    function CombatKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('p') | Character'Pos('P') => -- Give orders to pilot
                Order := Pilot;
                Refresh_Without_Update;
                ShowOrdersMenu;
                Update_Screen;
                return Combat_Orders;
            when Character'Pos('e') | Character'Pos('E') => -- Give orders to engineer
                Order := Engineer;
                Refresh_Without_Update;
                ShowOrdersMenu;
                Update_Screen;
                return Combat_Orders;
            when Character'Pos('g') | Character'Pos('G') => -- Give orders to gunner
                Order := Gunner;
                Refresh_Without_Update;
                ShowOrdersMenu;
                Update_Screen;
                return Combat_Orders;
            when Character'Pos(' ') => -- Next combat turn or back to sky map if end combat
                if Enemy.Durability > 0 then
                    CombatTurn;
                    DrawGame(Combat_State);
                    return Combat_State;
                else
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                end if;
            when others =>
                return Combat_State;
        end case;
    end CombatKeys;

    function CombatOrdersKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Dont give any new order
                DrawGame(Combat_State);
                return Combat_State;
            when others =>
                return CombatOrders(Key);
        end case;
    end CombatOrdersKeys;

end Combat;
