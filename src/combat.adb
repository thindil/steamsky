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

package body Combat is
    
    type Enemy_Record is -- Data structure for enemies
        record
            Name : Unbounded_String;
            Durability : Natural;
            Damage : Positive;
            DamageRange : Natural;
        end record;
    Enemy : Enemy_Record;

    procedure StartCombat(EnemyType : Enemy_Types) is
    begin
        case EnemyType is
            when SmallPirateShip =>
                Enemy := (Name => To_Unbounded_String("Small pirates ship"),
                    Durability => 100, Damage => 5, DamageRange => 1);
            when SmallUndeadShip =>
                Enemy := (Name => To_Unbounded_String("Small undead ship"),
                    Durability => 100, Damage => 10, DamageRange => 0);
            when SmallDrone =>
                Enemy := (Name => To_Unbounded_String("Small clockwork drone"),
                    Durability => 50, Damage => 5, DamageRange => 0);
        end case;
    end StartCombat;

end Combat;
