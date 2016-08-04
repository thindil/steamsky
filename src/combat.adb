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
with Ships; use Ships;
with Crew; use Crew;
with UserInterface; use UserInterface;

package body Combat is
    
    type Enemy_Record is -- Data structure for enemies
        record
            Name : Unbounded_String;
            Durability : Natural;
            MaxDurability : Natural;
            Damage : Positive;
            DamageRange : Natural;
            Accuracy : Positive;
            Distance : Unbounded_String;
            Speed : ShipSpeed; 
        end record;
    Enemy : Enemy_Record;

    procedure StartCombat(EnemyType : Enemy_Types) is
    begin
        case EnemyType is
            when SmallPirateShip =>
                Enemy := (Name => To_Unbounded_String("Small pirates ship"),
                    Durability => 100, Damage => 5, DamageRange => 1, Accuracy
                    => 1, MaxDurability => 100, Distance =>
                    To_Unbounded_String("Long"), Speed => FULL_SPEED);
            when SmallUndeadShip =>
                Enemy := (Name => To_Unbounded_String("Small undead ship"),
                    Durability => 100, Damage => 10, DamageRange => 0, Accuracy
                    => 1, MaxDurability => 100, Distance =>
                    To_Unbounded_String("Long"), Speed => FULL_SPEED);
            when SmallDrone =>
                Enemy := (Name => To_Unbounded_String("Small clockwork drone"),
                    Durability => 50, Damage => 5, DamageRange => 0, Accuracy
                    => 1, MaxDurability => 50, Distance =>
                    To_Unbounded_String("Long"), Speed => FULL_SPEED);
        end case;
    end StartCombat;

    procedure CombatTurn is
    begin
        UpdateGame(1);
    end CombatTurn;

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
        Change_Attributes(Line => 1, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Engineer: " & To_String(EngineerName));
        Change_Attributes(Line => 2, Column => 2,
            Count => 1, Color => 1);
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Gunner: " & To_String(GunnerName));
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
        Add(Str => "Distance: " & To_String(Enemy.Distance));
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
        Add(Str => "SPACE for next turn");
        Change_Attributes(Line => 13, Column => (Columns / 2),
            Count => 5, Color => 1);
    end ShowCombat;

    procedure ShowOrdersMenu(Order : Crew_Orders) is
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
                    Move_Cursor(Win => OrdersWindow, Line => 1, Column => 1);
                    Add(Win => OrdersWindow, Str => "a Go closer");
                    Change_Attributes(Win => OrdersWindow, Line => 1, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 2, Column => 1);
                    Add(Win => OrdersWindow, Str => "b Keep distance");
                    Change_Attributes(Win => OrdersWindow, Line => 2, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 3, Column => 1);
                    Add(Win => OrdersWindow, Str => "c Evade");
                    Change_Attributes(Win => OrdersWindow, Line => 3, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 4, Column => 1);
                    Add(Win => OrdersWindow, Str => "d Escape");
                    Change_Attributes(Win => OrdersWindow, Line => 4, Column => 1, 
                        Count => 1, Color => 1);
                    Line := 4;
                when Engineer =>
                    Move_Cursor(Win => OrdersWindow, Line => 1, Column => 1);
                    Add(Win => OrdersWindow, Str => "a Full stop");
                    Change_Attributes(Win => OrdersWindow, Line => 1, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 2, Column => 1);
                    Add(Win => OrdersWindow, Str => "b Quarter speed");
                    Change_Attributes(Win => OrdersWindow, Line => 2, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 3, Column => 1);
                    Add(Win => OrdersWindow, Str => "c Half speed");
                    Change_Attributes(Win => OrdersWindow, Line => 3, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 4, Column => 1);
                    Add(Win => OrdersWindow, Str => "d Full speed");
                    Change_Attributes(Win => OrdersWindow, Line => 4, Column => 1, 
                        Count => 1, Color => 1);
                    Line := 4;
                when Gunner =>
                    Move_Cursor(Win => OrdersWindow, Line => 1, Column => 1);
                    Add(Win => OrdersWindow, Str => "a Stop shooting");
                    Change_Attributes(Win => OrdersWindow, Line => 1, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 1, Column => 1);
                    Add(Win => OrdersWindow, Str => "b Precise fire");
                    Change_Attributes(Win => OrdersWindow, Line => 2, Column => 1, 
                        Count => 1, Color => 1);
                    Move_Cursor(Win => OrdersWindow, Line => 1, Column => 1);
                    Add(Win => OrdersWindow, Str => "c Fire at will");
                    Change_Attributes(Win => OrdersWindow, Line => 3, Column => 1, 
                        Count => 1, Color => 1);
                    Line := 3;
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
                Refresh_Without_Update;
                ShowOrdersMenu(Pilot);
                Update_Screen;
                return Combat_Orders;
            when Character'Pos('e') | Character'Pos('E') => -- Give orders to engineer
                Refresh_Without_Update;
                ShowOrdersMenu(Engineer);
                Update_Screen;
                return Combat_Orders;
            when Character'Pos('g') | Character'Pos('G') => -- Give orders to gunner
                Refresh_Without_Update;
                ShowOrdersMenu(Gunner);
                Update_Screen;
                return Combat_Orders;
            when Character'Pos(' ') => -- Next combat turn
                CombatTurn;
                DrawGame(Combat_State);
                return Combat_State;
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu (test code)
                DrawGame(Quit_Confirm);
                return Quit_Confirm;
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
                return Combat_Orders;
        end case;
    end CombatOrdersKeys;

end Combat;
