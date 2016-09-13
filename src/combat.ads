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

package Combat is
    
    EnemyName : Unbounded_String; -- Name of enemy;
    PilotOrder, EngineerOrder, GunnerOrder : Positive; -- Orders for crew members
    type Enemy_Record is -- Data structure for enemies
        record
            Ship : ShipRecord; -- Ship data for enemy
            DamageRange : Natural; -- Range from which enemy starts attack
            Accuracy : Positive; -- Bonus to accuracy
            Distance : Integer; -- Current distance to enemy
            CombatAI : ShipCombatAI; -- Enemy in combat AI type
            Evasion : Positive; -- Bonus to evasion
            LootMin : Positive; -- Minimal amount of loot from ship
            LootMax : Positive; -- Maximum amount of loot from ship
        end record;
    Enemy : Enemy_Record; -- Enemy informations
    EndCombat : Boolean; -- True if combat ends
    MessagesStarts : Natural; -- Start index for showing messages

    procedure StartCombat(EnemyIndex : Positive); -- Generate enemy and start battle
    procedure CombatTurn; -- Count damage/ships actions, etc

end Combat;
