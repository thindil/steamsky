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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ships; use Ships;
with Maps; use Maps;
with Combat; use Combat;
with Messages; use Messages;
with Crew; use Crew;

package body Events is

    function CheckForEvent(OldState : GameStates) return GameStates is
        type Percent_Range is range 1..100;
        subtype Combat_Range is Positive range Enemies_List.First_Index..Enemies_List.Last_Index; 
        package Rand_Roll is new Discrete_Random(Percent_Range);
        package Rand_Combat is new Discrete_Random(Combat_Range);
        Generator : Rand_Roll.Generator;
        Generator2 : Rand_Combat.Generator;
        Roll : Percent_Range;
        TimePassed : Integer;
        PilotIndex : Natural := 0;
    begin
        Rand_Roll.Reset(Generator);
        Rand_Combat.Reset(Generator2);
        Event := None;
        if Rand_Roll.Random(Generator) < 7 then -- Event happen
            Roll := Rand_Roll.Random(Generator);
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then -- Outside bases
                case Roll is
                    when 1..20 => -- Bad weather
                        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                            if PlayerShip.Crew.Element(I).Order = Pilot then
                                PilotIndex := I;
                                exit;
                            end if;
                        end loop;
                        if PilotIndex > 0 then
                            AddMessage("Sudden bad weather makes your travel takes longer.", OtherMessage);
                            TimePassed := 60 - GetSkillLevel(PilotIndex, 1);
                            if TimePassed < 1 then
                                TimePassed := 1;
                            end if;
                            GainExp(1, 1, PilotIndex);
                            UpdateCargo(1, 1);
                            UpdateGame(TimePassed);
                        end if;
                        return OldState;
                    when others => -- Combat
                        Event := EnemyShip;
                        return StartCombat(Rand_Combat.Random(Generator2));
                end case;
            end if;
        end if;
        return OldState;
    end CheckForEvent;

end Events;
