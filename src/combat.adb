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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ships; use Ships;
with Crew; use Crew;

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
    end ShowCombat;

end Combat;
