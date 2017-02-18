--    Copyright 2016-2017 Bartek thindil Jasicki
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
with Ada.Numerics.Generic_Elementary_Functions;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Maps; use Maps;
with Combat; use Combat;
with Messages; use Messages;
with Crew; use Crew;
with UserInterface; use UserInterface;
with Bases; use Bases;
with ShipModules; use ShipModules;
with Items; use Items;
with Utils; use Utils;

package body Events is

    EventsMenu : Menu;
    MenuWindow : Window;

    function CheckForEvent(OldState : GameStates) return GameStates is
        TimePassed : Integer;
        CrewIndex, PlayerValue : Natural := 0;
        Roll, Roll2 : Positive;
        Enemies, Engines : Positive_Container.Vector;
    begin
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            case Events_List.Element(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex).EType is
                when EnemyShip =>
                    return StartCombat(Events_List.Element(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex).Data);
                when others =>
                    return OldState;
            end case;
        end if;
        if GetRandom(1, 100) < 7 then -- Event happen
            Roll := GetRandom(1, 100);
            if GetRandom(1, 100) < 95 then
                for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                    case Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType is
                        when HULL | GUN | BATTERING_RAM =>
                            PlayerValue := PlayerValue + PlayerShip.Modules.Element(I).MaxDurability +
                            (PlayerShip.Modules.Element(I).Max_Value * 10);
                        when ARMOR =>
                            PlayerValue := PlayerValue + PlayerShip.Modules.Element(I).MaxDurability;
                        when others =>
                            null;
                    end case;
                end loop;
                for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                    if Length(Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType) >= 4 then
                        if Slice(Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType, 1, 4) = "Ammo" then
                            PlayerValue := PlayerValue + (Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Value * 10);
                        end if;
                    end if;
                end loop;
                for I in Enemies_List.First_Index..Enemies_List.Last_Index loop
                    if Enemies_List.Element(I).CombatValue <= PlayerValue then
                        Enemies.Append(New_Item => I);
                    end if;
                end loop;
            else
                for I in Enemies_List.First_Index..Enemies_List.Last_Index loop
                    Enemies.Append(New_Item => I);
                end loop;
            end if;
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then -- Outside bases
                case Roll is
                    when 1..5 => -- Engine damaged
                        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                            if PlayerShip.Crew.Element(I).Order = Engineer then
                                CrewIndex := I;
                                exit;
                            end if;
                        end loop;
                        if CrewIndex > 0 and PlayerShip.Speed /= FULL_STOP then
                            Roll2 := GetRandom(1, 100);
                            case PlayerShip.Speed is
                                when QUARTER_SPEED =>
                                    if Roll2 < 21 then
                                        Roll2 := 1;
                                    else
                                        Roll2 := Roll2 - 20;
                                    end if;
                                when FULL_SPEED =>
                                    Roll2 :=  Roll2 + 20;
                                when others =>
                                    null;
                            end case;
                            if Roll2 > GetSkillLevel(CrewIndex, 2) then
                                AddMessage("One of your engines is taking damage.", OtherMessage);
                                for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                                    if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = ENGINE then
                                        Engines.Append(New_Item => I);
                                    end if;
                                end loop;
                                UpdateModule(PlayerShip, Engines.Element(GetRandom(Engines.First_Index, Engines.Last_Index)), 
                                    "Durability", "-1");
                            else
                                AddMessage(To_String(PlayerShip.Crew.Element(CrewIndex).Name) & " has prevented engine damage.",
                                    OtherMessage);
                            end if;
                            GainExp(1, 2, CrewIndex);
                        end if;
                    when 6..20 => -- Bad weather
                        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                            if PlayerShip.Crew.Element(I).Order = Pilot then
                                CrewIndex := I;
                                exit;
                            end if;
                        end loop;
                        if CrewIndex > 0 then
                            AddMessage("Sudden bad weather makes your travel takes longer.", OtherMessage);
                            TimePassed := 60 - GetSkillLevel(CrewIndex, 1);
                            if TimePassed < 1 then
                                TimePassed := 1;
                            end if;
                            GainExp(1, 1, CrewIndex);
                            UpdateCargo(PlayerShip, 1, -1);
                            UpdateGame(TimePassed);
                        end if;
                    when others => -- Combat
                        Events_List.Append(New_Item => (EnemyShip, PlayerShip.SkyX, PlayerShip.SkyY, GetRandom(30, 45), 
                            Enemies.Element(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
                        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex := Events_List.Last_Index;
                        return StartCombat(Events_List.Element(Events_List.Last_Index).Data);
                end case;
            else
                if PlayerShip.Speed /= DOCKED and SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Owner /= Abandoned then
                    if Roll = 21 and (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Owner = Drones or 
                        SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Owner = Undead)
                    then
                        Roll := 22;
                    end if;
                    case Roll is
                        when 1..20 => -- Base is attacked
                            Events_List.Append(New_Item => (AttackOnBase, PlayerShip.SkyX, PlayerShip.SkyY, GetRandom(60, 90), 
                                Enemies.Element(GetRandom(Enemies.First_Index, Enemies.Last_Index))));
                            AddMessage("You can't dock to base now, because base is under attack. You can help defend it.", OtherMessage);
                            return StartCombat(Events_List.Element(Events_List.Last_Index).Data);
                        when 21 => -- Disease in base
                            Events_List.Append(New_Item => (Disease, PlayerShip.SkyX, PlayerShip.SkyY, GetRandom(10080, 12000), 1));
                            AddMessage("You can't dock to base now, it is closed due to disease.", OtherMessage);
                        when others => -- Full docks
                            Events_List.Append(New_Item => (FullDocks, PlayerShip.SkyX, PlayerShip.SkyY, GetRandom(15, 30), 1));
                            AddMessage("You can't dock to base now, because its docks are full.", OtherMessage);
                    end case;
                    SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex := Events_List.Last_Index;
                end if;
            end if;
        end if;
        return OldState;
    end CheckForEvent;

    procedure UpdateEvents(Minutes : Positive) is
        CurrentIndex : Positive := Events_List.First_Index;
        NewTime : Integer;
        EventsAmount : constant Natural := Natural(Events_List.Length);
        procedure UpdateEvent(Event : in out EventData) is
        begin
            Event.Time := NewTime;
        end UpdateEvent;
    begin
        if EventsAmount = 0 then
            return;
        end if;
        while CurrentIndex <= Events_List.Last_Index loop
            NewTime := Events_List.Element(CurrentIndex).Time - Minutes;
            if NewTime < 1 then
                SkyMap(Events_List.Element(CurrentIndex).SkyX, Events_List.Element(CurrentIndex).SkyY).EventIndex := 0;
                Events_List.Delete(Index => CurrentIndex, Count => 1);
            else
                Events_List.Update_Element(Index => CurrentIndex, Process => UpdateEvent'Access);
                CurrentIndex := CurrentIndex + 1;
            end if;
        end loop;
        if EventsAmount > Natural(Events_List.Length) then
            for I in Events_List.First_Index..Events_List.Last_Index loop
                SkyMap(Events_List.Element(I).SkyX, Events_List.Element(I).SkyY).EventIndex := I;
            end loop;
        end if;
    end UpdateEvents;

    procedure DeleteEvent(EventIndex : Positive) is
    begin
        SkyMap(Events_List.Element(EventIndex).SkyX, Events_List.Element(EventIndex).SkyY).EventIndex := 0;
        Events_List.Delete(Index => EventIndex, Count => 1);
        for I in Events_List.First_Index..Events_List.Last_Index loop
            SkyMap(Events_List.Element(I).SkyX, Events_List.Element(I).SkyY).EventIndex := I;
        end loop;
    end DeleteEvent;

    procedure ShowEventInfo is
        InfoWindow : Window;
        EventIndex : constant Positive := Get_Index(Current(EventsMenu));
        type Value_Type is digits 2 range 0.0..9999999.0;
        package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Value_Type);
        DiffX, DiffY : Natural;
        Distance : Value_Type;
    begin
        InfoWindow := Create(10, (Columns / 2), 4, (Columns / 2));
        Add(Win => InfoWindow, Str => "X:" & Positive'Image(Events_List.Element(EventIndex).SkyX) & " Y:" &
            Positive'Image(Events_List.Element(EventIndex).SkyY));
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        case Events_List.Element(EventIndex).EType is
            when EnemyShip =>
                Add(Win => InfoWindow, Str => To_String(Enemies_List.Element(Events_List.Element(EventIndex).Data).Name));
            when FullDocks | AttackOnBase | Disease =>
                Add(Win => InfoWindow, Str => To_String(SkyBases(SkyMap(Events_List.Element(EventIndex).SkyX,
                    Events_List.Element(EventIndex).SkyY).BaseIndex).Name));
            when others =>
                null;
        end case;
        DiffX := abs(PlayerShip.SkyX - Events_List.Element(EventIndex).SkyX);
        DiffY := abs(PlayerShip.SkyY - Events_List.Element(EventIndex).SkyY);
        Distance := Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2)));
        Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
        Add(Win => InfoWindow, Str => "Distance:" & Integer'Image(Integer(Value_Type'Floor(Distance))));
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
                when AttackOnBase =>
                    Events_Items.all(I) := New_Item("Base is under attack");
                when Disease =>
                    Events_Items.all(I) := New_Item("Disease in base");
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
                    Result := Driver(EventsMenu, Key);
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    else
                        Result := Driver(EventsMenu, M_CLEAR_PATTERN);
                        Result := Driver(EventsMenu, Key);
                        if Result = Menu_Ok then
                            Refresh(MenuWindow);
                        end if;
                    end if;
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
