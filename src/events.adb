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
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Ships; use Ships;
with Maps; use Maps;
with Combat; use Combat;
with Messages; use Messages;
with Crew; use Crew;
with UserInterface; use UserInterface;
with Bases; use Bases;

package body Events is

    EventsMenu : Menu;
    MenuWindow : Window;

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

    procedure ShowEventInfo is
        InfoWindow : Window;
        EventIndex : constant Positive := Get_Index(Current(EventsMenu));
    begin
        InfoWindow := Create(10, (Columns / 2), 4, (Columns / 2));
        Add(Win => InfoWindow, Str => "X:" & Positive'Image(Events_List.Element(EventIndex).SkyX) & " Y:" &
            Positive'Image(Events_List.Element(EventIndex).SkyY));
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        case Events_List.Element(EventIndex).EType is
            when EnemyShip =>
                Add(Win => InfoWindow, Str => To_String(Enemies_List.Element(Events_List.Element(EventIndex).Data).Name));
            when FullDocks =>
                Add(Win => InfoWindow, Str => To_String(SkyBases(SkyMap(Events_List.Element(EventIndex).SkyX,
                    Events_List.Element(EventIndex).SkyY).BaseIndex).Name));
            when others =>
                null;
        end case;
        Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
        Add(Win => InfoWindow, Str => "Press SPACE to show event on map");
        Change_Attributes(Win => InfoWindow, Line => 4, Column => 6, Count => 5, Color => 1);
        Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
        Add(Win => InfoWindow, Str => "Press ENTER to set event as a destination for ship");
        Change_Attributes(Win => InfoWindow, Line => 5, Column => 6, Count => 5, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowEventInfo;

    procedure ShowEvents is
        Events_Items : constant Item_Array_Access := new Item_Array(Events_List.First_Index..(Events_List.Last_Index + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
    begin
        for I in Events_List.First_Index..Events_List.Last_Index loop
            case Events_List.Element(I).EType is
                when EnemyShip =>
                    Events_Items.all(I) := New_Item("Enemy ship spotted");
                when FullDocks =>
                    Events_Items.all(I) := New_Item("Full docks in base");
                when others =>
                    null;
            end case;
        end loop;
        Events_Items.all(Events_Items'Last) := Null_Item;
        if Events_Items.all(1) /= Null_Item then
            EventsMenu := New_Menu(Events_Items);
            Set_Options(EventsMenu, (Show_Descriptions => False, others => True));
            Set_Format(EventsMenu, Lines - 4, 1);
            Set_Mark(EventsMenu, "");
            Scale(EventsMenu, MenuHeight, MenuLength);
            MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
            Set_Window(EventsMenu, MenuWindow);
            Set_Sub_Window(EventsMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
            Post(EventsMenu);
            ShowEventInfo;
            Refresh(MenuWindow);
        else
            Move_Cursor(Line => (Lines / 3), Column => (Columns / 2) - 21);
            Add(Str => "You don't know about any events.");
            Refresh;
        end if;
    end ShowEvents;

    function ShowEventsKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
        EventIndex : Positive;
    begin
        if EventsMenu /= Null_Menu then
            EventIndex := Get_Index(Current(EventsMenu));
            case Key is
                when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                    Post(EventsMenu, False);
                    Delete(EventsMenu);
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when KEY_UP => -- Select previous event
                    Result := Driver(EventsMenu, M_Up_Item);
                    if Result = Request_Denied then
                        Result := Driver(EventsMenu, M_Last_Item);
                    end if;
                when KEY_DOWN => -- Select next event
                    Result := Driver(EventsMenu, M_Down_Item);
                    if Result = Request_Denied then
                        Result := Driver(EventsMenu, M_First_Item);
                    end if;
                when 32 => -- Show selected event on map
                    MoveMap(Events_List.Element(EventIndex).SkyX, Events_List.Element(EventIndex).SkyY);
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when 10 => -- Set event as destination point for ship
                    if Events_List.Element(EventIndex).SkyX = PlayerShip.SkyX and 
                        Events_List.Element(EventIndex).SkyY = PlayerShip.SkyY 
                    then
                        ShowDialog("You are at this event now.");
                        DrawGame(Events_View);
                        return Events_View;
                    end if;
                    PlayerShip.DestinationX := Events_List.Element(EventIndex).SkyX;
                    PlayerShip.DestinationY := Events_List.Element(EventIndex).SkyY;
                    AddMessage("You set travel destination for your ship.", OrderMessage);
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when others =>
                    null;
            end case;
            if Result = Menu_Ok then
                ShowEventInfo;
                Refresh(MenuWindow);
            end if;
        else
            case Key is
                when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when others =>
                    null;
            end case;
        end if;
        return Events_View;
    end ShowEventsKeys;

end Events;
