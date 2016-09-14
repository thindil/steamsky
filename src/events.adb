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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ships; use Ships;
with Maps; use Maps;
with Combat; use Combat;

package body Events is

    function CheckForEvent (OldState : GameStates) return GameStates is
        type Percent_Range is range 1..100;
        subtype Combat_Range is Positive range Enemies_List.First_Index..Enemies_List.Last_Index; 
        package Rand_Roll is new Discrete_Random(Percent_Range);
        package Rand_Combat is new Discrete_Random(Combat_Range);
        Generator : Rand_Roll.Generator;
        Generator2 : Rand_Combat.Generator;
    begin
        Rand_Roll.Reset(Generator);
        Rand_Combat.Reset(Generator2);
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then -- Outside bases
            if Rand_Roll.Random(Generator) < 70 then -- Combat
                StartCombat(Rand_Combat.Random(Generator2));
                return Combat_Confirm;
            end if;
        end if;
        return OldState;
    end CheckForEvent;

end Events;
