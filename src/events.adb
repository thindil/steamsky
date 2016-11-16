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
        for I in Events_List.First_Index..Events_List.Last_Index loop
            if Events_List.Element(I).SkyX = PlayerShip.SkyX and Events_List.Element(I).SkyY = PlayerShip.SkyY then
                case Events_List.Element(I).EType is
                    when EnemyShip =>
                        return StartCombat(Events_List.Element(I).Data);
                    when others =>
                        return OldState;
                end case;
            end if;
        end loop;
        Rand_Roll.Reset(Generator);
        Rand_Combat.Reset(Generator2);
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
                    when others => -- Combat
                        Events_List.Append(New_Item => (EnemyShip, PlayerShip.SkyX, PlayerShip.SkyY, 30, Rand_Combat.Random(Generator2)));
                        return StartCombat(Events_List.Element(Events_List.Last_Index).Data);
                end case;
            else
                if PlayerShip.Speed /= DOCKED then -- Full docks
                    Events_List.Append(New_Item => (FullDocks, PlayerShip.SkyX, PlayerShip.SkyY, 15, Rand_Combat.Random(Generator2)));
                    AddMessage("You can't dock to base now, because its docks are full.", OtherMessage);
                end if;
            end if;
        end if;
        return OldState;
    end CheckForEvent;

    procedure UpdateEvents(Minutes : Positive) is
        CurrentIndex : Positive := Events_List.First_Index;
        NewTime : Integer;
        procedure UpdateEvent(Event : in out EventData) is
        begin
            Event.Time := NewTime;
        end UpdateEvent;
    begin
        if Events_List.Length = 0 then
            return;
        end if;
        while CurrentIndex <= Events_List.Last_Index loop
            NewTime := Events_List.Element(CurrentIndex).Time - Minutes;
            if NewTime < 1 then
                Events_List.Delete(Index => CurrentIndex, Count => 1);
            else
                Events_List.Update_Element(Index => CurrentIndex, Process => UpdateEvent'Access);
                CurrentIndex := CurrentIndex + 1;
            end if;
        end loop;
    end UpdateEvents;

end Events;
