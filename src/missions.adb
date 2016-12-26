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
with Ada.Numerics.Generic_Elementary_Functions;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Ships; use Ships;
with Maps; use Maps;
with Items; use Items;
with Bases; use Bases;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Crew; use Crew;

package body Missions is

    MissionsMenu : Menu;
    MenuWindow : Window;

    procedure GenerateMissions(BaseIndex : Positive) is
        TimeDiff : Natural;
        MissionsAmount, MissionX, MissionY, TmpBaseIndex, DiffX, DiffY : Positive;
        Mission : Mission_Data;
        MissionsItems : Positive_Container.Vector;
        MinX, MinY, MaxX, MaxY : Integer;
        type Value_Type is digits 2 range 0.0..9999999.0;
        package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Value_Type);
        function GetRandom(Min : Natural; Max : Positive) return Natural is
            subtype Rand_Range is Natural range Min..Max;
            package Rand_Roll is new Discrete_Random(Rand_Range);
            Generator : Rand_Roll.Generator;
        begin
            Rand_Roll.Reset(Generator);
            return Rand_Roll.Random(Generator);
        end GetRandom;
    begin
        TimeDiff := (GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) - (SkyBases(BaseIndex).MissionsDate.Day + 
            ((30 * SkyBases(BaseIndex).MissionsDate.Month) * SkyBases(BaseIndex).MissionsDate.Year));
        if TimeDiff < 7 then
            return;
        end if;
        if SkyBases(BaseIndex).Population < 150 then
            MissionsAmount := GetRandom(1, 5);
        elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
            MissionsAmount := GetRandom(1, 10);
        else
            MissionsAmount := GetRandom(1, 15);
        end if;
        for I in Items_List.First_Index..Items_List.Last_Index loop
            if Items_List.Element(I).IType = To_Unbounded_String("MissionItem") then
                MissionsItems.Append(New_Item => I);
            end if;
        end loop;
        MinX := PlayerShip.SkyX - 100;
        if MinX < 1 then
            MinX := 1;
        end if;
        MaxX := PlayerShip.SkyX + 100;
        if MaxX > 1024 then
            MaxX := 1024;
        end if;
        MinY := PlayerShip.SkyY - 100;
        if MinY < 1 then
            MinY := 1;
        end if;
        MaxY := PlayerShip.SkyY + 100;
        if MaxY > 1024 then
            MaxY := 1024;
        end if;
        SkyBases(BaseIndex).Missions.Clear;
        for I in 1..MissionsAmount loop
            Mission.MType := Missions_Types'Val(GetRandom(0, Missions_Types'Pos(Missions_Types'Last)));
            case Mission.MType is
                when Deliver => 
                    Mission.Target := MissionsItems.Element(GetRandom(MissionsItems.First_Index, MissionsItems.Last_Index));
                when Kill =>
                    Mission.Target := GetRandom(Enemies_List.First_Index, Enemies_List.Last_Index);
                when Explore =>
                    Mission.Target := 0;
            end case;
            loop
                if Mission.MType /= Deliver then
                    MissionX := GetRandom(MinX, MaxX);
                    MissionY := GetRandom(MinY, MaxY);
                    exit when SkyMap(MissionX, MissionY).BaseIndex = 0 and MissionX /= PlayerShip.SkyX and MissionY /= PlayerShip.SkyY;
                else
                    TmpBaseIndex := GetRandom(1, 1024);
                    MissionX := SkyBases(TmpBaseIndex).SkyX;
                    MissionY := SkyBases(TmpBaseIndex).SkyY;
                    exit when MissionX /= PlayerShip.SkyX and MissionY /= PlayerShip.SkyY;
                end if;
            end loop;
            Mission.TargetX := MissionX;
            Mission.TargetY := MissionY;
            DiffX := abs(PlayerShip.SkyX - MissionX);
            DiffY := abs(PlayerShip.SkyY - MissionY);
            Mission.Time := Positive(Value_Type(60) * Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2))));
            Mission.Reward := (Mission.Time / 5);
            Mission.StartBase := BaseIndex;
            SkyBases(BaseIndex).Missions.Append(New_Item => Mission);
        end loop;
        SkyBases(BaseIndex).MissionsDate := GameDate;
    end GenerateMissions;

    procedure AcceptMission(MissionIndex : Positive) is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        MissionsLimit : Integer;
        Mission : Mission_Data := SkyBases(BaseIndex).Missions.Element(MissionIndex);
        AcceptMessage : Unbounded_String;
        TraderIndex : Positive;
    begin
        if SkyBases(BaseIndex).Reputation(1) < 0 then
            ShowDialog("Your reputation in this base is too low to receive any mission.");
            return;
        end if;
        case SkyBases(BaseIndex).Reputation(1) is
            when 0..25 =>
                MissionsLimit := 1;
            when 26..50 =>
                MissionsLimit := 3;
            when 51..75 =>
                MissionsLimit := 5;
            when 76..100 =>
                MissionsLimit := 10;
            when others =>
                MissionsLimit := 0;
        end case;
        for I in PlayerShip.Missions.First_Index..PlayerShip.Missions.Last_Index loop
            if PlayerShip.Missions.Element(I).StartBase = BaseIndex then
                MissionsLimit := MissionsLimit - 1;
            end if;
            if MissionsLimit < 1 then
                ShowDialog("You can't take any more missions from this base. ");
                return;
            end if;
        end loop;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Mission.StartBase := BaseIndex;
        SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := PlayerShip.Missions.Last_Index;
        AcceptMessage := To_Unbounded_String("You accepted mission ");
        case Mission.MType is
            when Deliver =>
                Append(AcceptMessage, "'Deliver " & To_String(Items_List.Element(Mission.Target).Name) & "'.");
                UpdateCargo(PlayerShip, Mission.Target, 1);
                for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                    if PlayerShip.Cargo.Element(I).ProtoIndex = Mission.Target then
                        Mission.Target := I;
                        exit;
                    end if;
                end loop;
            when Kill =>
                Append(AcceptMessage, "'Destroy " & To_String(Enemies_List.Element(Mission.Target).Name) & "'.");
            when Explore =>
                Append(AcceptMessage, "'Explore selected area'.");
        end case;
        SkyBases(BaseIndex).Missions.Delete(Index => MissionIndex, Count => 1);
        PlayerShip.Missions.Append(New_Item => Mission);
        AddMessage(To_String(AcceptMessage), OtherMessage);
        GainExp(1, 4, TraderIndex);
        UpdateGame(5);
    end AcceptMission;

    procedure UpdateMissions(Minutes : Positive) is
        Time : Integer;
        I : Positive := PlayerShip.Missions.First_Index;
        FailMessage : Unbounded_String := To_Unbounded_String("You failed mission ");
        procedure UpdateMission(Mission : in out Mission_Data) is
        begin
            Mission.Time := Time;
        end UpdateMission;
    begin
        while I <= PlayerShip.Missions.Last_Index loop
            Time := PlayerShip.Missions.Element(I).Time - Minutes;
            if Time < 1 then
                GainRep(PlayerShip.Missions.Element(I).StartBase, -5);
                case PlayerShip.Missions.Element(I).MType is
                    when Deliver =>
                        Append(FailMessage, "'Deliver " & To_String(Items_List.Element(PlayerShip.Missions.Element(I).Target).Name) 
                            & "'.");
                    when Kill =>
                        Append(FailMessage, "'Destroy " & To_String(Enemies_List.Element(PlayerShip.Missions.Element(I).Target).Name) 
                            & "'.");
                    when Explore =>
                        Append(FailMessage, "'Explore selected area'.");
                end case;
                AddMessage(To_String(FailMessage), OtherMessage);
                PlayerShip.Missions.Delete(Index => I, Count => 1);
            else
                PlayerShip.Missions.Update_Element(Index => I, Process => UpdateMission'Access);
                I := I + 1;
            end if;
        end loop;
    end UpdateMissions;

    procedure ShowMissionInfo is
        Mission : constant Mission_Data := PlayerShip.Missions.Element(Get_Index(Current(MissionsMenu)));
        InfoWindow : Window;
        CurrentLine : Line_Position := 1;
        DiffX, DiffY : Positive;
        MinutesDiff : Natural;
        MissionTime : Date_Record := (Year => 0, Month => 0, Day => 0, Hour => 0, Minutes => 0);
        type Value_Type is digits 2 range 0.0..9999999.0;
        package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Value_Type);
        Distance : Value_Type;
    begin
        InfoWindow := Create(10, (Columns / 2), 3, (Columns / 2));
        case Mission.MType is
            when Deliver =>
                Add(Win => InfoWindow, Str => "Item: " & 
                    To_String(Items_List.Element(PlayerShip.Cargo.Element(Mission.Target).ProtoIndex).Name));
                Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
                Add(Win => InfoWindow, Str => "To base: " & To_String(SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex).Name));
                CurrentLine := 2;
            when Explore =>
                Add(Win => InfoWindow, Str => "Explore selected area");
            when Kill =>
                Add(Win => InfoWindow, Str => "Target: " & To_String(Enemies_List.Element(Mission.Target).Name));
        end case;
        DiffX := abs(PlayerShip.SkyX - Mission.TargetX);
        DiffY := abs(PlayerShip.SkyY - Mission.TargetY);
        Distance := Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2)));
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Distance:" & Integer'Image(Integer(Value_Type'Floor(Distance))));
        MinutesDiff := Mission.Time;
        while MinutesDiff > 0 loop
            if MinutesDiff >= 518400 then
                MissionTime.Year := MissionTime.Year + 1;
                MinutesDiff := MinutesDiff - 518400;
            elsif MinutesDiff >= 43200 then
                MissionTime.Month := MissionTime.Month + 1;
                MinutesDiff := MinutesDiff - 43200;
            elsif MinutesDiff >= 1440 then
                MissionTime.Day := MissionTime.Day + 1;
                MinutesDiff := MinutesDiff - 1440;
            elsif MinutesDiff >= 60 then
                MissionTime.Hour := MissionTime.Hour + 1;
                MinutesDiff := MinutesDiff - 60;
            else
                MissionTime.Minutes := MinutesDiff;
                MinutesDiff := 0;
            end if;
        end loop;
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Time limit:");
        if MissionTime.Year > 0 then
            Add(Win => InfoWindow, Str => Positive'Image(MissionTime.Year) & "y");
        end if;
        if MissionTime.Month > 0 then
            Add(Win => InfoWindow, Str => Positive'Image(MissionTime.Month) & "m");
        end if;
        if MissionTime.Day > 0 then
            Add(Win => InfoWindow, Str => Positive'Image(MissionTime.Day) & "d");
        end if;
        if MissionTime.Hour > 0 then
            Add(Win => InfoWindow, Str => Positive'Image(MissionTime.Hour) & "h");
        end if;
        if MissionTime.Minutes > 0 then
            Add(Win => InfoWindow, Str => Positive'Image(MissionTime.Minutes) & "mins");
        end if;
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Reward:" & Positive'Image(Mission.Reward) & " Charcollum");
        Move_Cursor(Win => InfoWindow, Line => CurrentLine + 2, Column => 0);
        Add(Win => InfoWindow, Str => "Press SPACE to show mission on map");
        Change_Attributes(Win => InfoWindow, Line => CurrentLine + 2, Column => 6, Count => 5, Color => 1);
        Move_Cursor(Win => InfoWindow, Line => CurrentLine + 3, Column => 0);
        Add(Win => InfoWindow, Str => "Press ENTER to set mission as a destination for ship");
        Change_Attributes(Win => InfoWindow, Line => CurrentLine + 3, Column => 6, Count => 5, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowMissionInfo;

    procedure ShowMissions is
        Missions_Items : constant Item_Array_Access := new Item_Array(PlayerShip.Missions.First_Index..(PlayerShip.Missions.Last_Index + 
            1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
    begin
        if PlayerShip.Missions.Length = 0 then
            Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
            Add(Str => "You didn't accepted any mission yet.");
            Refresh;
            return;
        end if;
        for I in PlayerShip.Missions.First_Index..PlayerShip.Missions.Last_Index loop
            case PlayerShip.Missions.Element(I).MType is
                when Deliver =>
                    Missions_Items.all(I) := New_Item("Deliver item to base");
                when Explore =>
                    Missions_Items.all(I) := New_Item("Explore area");
                when Kill =>
                    Missions_Items.all(I) := New_Item("Destroy ship");
            end case;
        end loop;
        Missions_Items.all(Missions_Items'Last) := Null_Item;
        MissionsMenu := New_Menu(Missions_Items);
        Set_Format(MissionsMenu, Lines - 10, 1);
        Set_Mark(MissionsMenu, "");
        Scale(MissionsMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(MissionsMenu, MenuWindow);
        Set_Sub_Window(MissionsMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(MissionsMenu);
        ShowMissionInfo;
        Refresh(MenuWindow);
    end ShowMissions;

    function ShowMissionsKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
        MissionIndex : Positive;
    begin
        if MissionsMenu /= Null_Menu then
            MissionIndex := Get_Index(Current(MissionsMenu));
            case Key is
                when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                    Post(MissionsMenu, False);
                    Delete(MissionsMenu);
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when KEY_UP => -- Select previous event
                    Result := Driver(MissionsMenu, M_Up_Item);
                    if Result = Request_Denied then
                        Result := Driver(MissionsMenu, M_Last_Item);
                    end if;
                when KEY_DOWN => -- Select next event
                    Result := Driver(MissionsMenu, M_Down_Item);
                    if Result = Request_Denied then
                        Result := Driver(MissionsMenu, M_First_Item);
                    end if;
                when 32 => -- Show selected event on map
                    MoveMap(PlayerShip.Missions.Element(MissionIndex).TargetX, PlayerShip.Missions.Element(MissionIndex).TargetY);
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when 10 => -- Set event as destination point for ship
                    if PlayerShip.Missions.Element(MissionIndex).TargetX = PlayerShip.SkyX and 
                        PlayerShip.Missions.Element(MissionIndex).TargetY = PlayerShip.SkyY 
                    then
                        ShowDialog("You are at this target now.");
                        DrawGame(Missions_View);
                        return Missions_View;
                    end if;
                    PlayerShip.DestinationX := PlayerShip.Missions.Element(MissionIndex).TargetX;
                    PlayerShip.DestinationY := PlayerShip.Missions.Element(MissionIndex).TargetY;
                    AddMessage("You set travel destination for your ship.", OrderMessage);
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                when others =>
                    Result := Driver(MissionsMenu, Key);
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    else
                        Result := Driver(MissionsMenu, M_CLEAR_PATTERN);
                        Result := Driver(MissionsMenu, Key);
                        if Result = Menu_Ok then
                            Refresh(MenuWindow);
                        end if;
                    end if;
            end case;
            if Result = Menu_Ok then
                ShowMissionInfo;
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
        return Missions_View;
    end ShowMissionsKeys;

end Missions;
