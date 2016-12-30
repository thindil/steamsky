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
with Ships; use Ships;
with Maps; use Maps;
with Items; use Items;
with Bases; use Bases;
with Messages; use Messages;
with Crew; use Crew;
with Combat; use Combat;
with UserInterface; use UserInterface;

package body Missions is

    procedure GenerateMissions(BaseIndex : Positive) is
        TimeDiff : Natural;
        MissionsAmount, MissionX, MissionY, TmpBaseIndex, DiffX, DiffY : Positive;
        Mission : Mission_Data;
        MissionsItems, BasesInRange : Positive_Container.Vector;
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
        case SkyBases(BaseIndex).Reputation(1) is
            when 1..25 =>
                MissionsAmount := MissionsAmount + 1;
            when 26..50 =>
                MissionsAmount := MissionsAmount + 3;
            when 51..75 =>
                MissionsAmount := MissionsAmount + 5;
            when 76..100 =>
                MissionsAmount := MissionsAmount + 10;
            when others =>
                null;
        end case;
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
        for I in SkyBases'Range loop
            if I /= BaseIndex and SkyBases(I).SkyX in MinX..MaxX and SkyBases(I).SkyY in MinY..MaxY then
                BasesInRange.Append(New_Item => I);
            end if;
        end loop;
        while MissionsAmount > Positive(BasesInRange.Length) loop
            TmpBaseIndex := GetRandom(1, 1024);
            if BasesInRange.Find_Index(Item => TmpBaseIndex) = Positive_Container.No_Index then
                BasesInRange.Append(New_Item => TmpBaseIndex);
            end if;
        end loop;
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
                    TmpBaseIndex := GetRandom(BasesInRange.First_Index, BasesInRange.Last_Index);
                    MissionX := SkyBases(BasesInRange.Element(TmpBaseIndex)).SkyX;
                    MissionY := SkyBases(BasesInRange.Element(TmpBaseIndex)).SkyY;
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
        end loop;
        if MissionsLimit < 1 then
            ShowDialog("You can't take any more missions from this base. ");
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                TraderIndex := I;
                exit;
            end if;
        end loop;
        Mission.StartBase := BaseIndex;
        AcceptMessage := To_Unbounded_String("You accepted mission ");
        case Mission.MType is
            when Deliver =>
                Append(AcceptMessage, "'Deliver " & To_String(Items_List.Element(Mission.Target).Name) & "'.");
                UpdateCargo(PlayerShip, Mission.Target, 1);
            when Kill =>
                Append(AcceptMessage, "'Destroy " & To_String(Enemies_List.Element(Mission.Target).Name) & "'.");
            when Explore =>
                Append(AcceptMessage, "'Explore selected area'.");
        end case;
        SkyBases(BaseIndex).Missions.Delete(Index => MissionIndex, Count => 1);
        PlayerShip.Missions.Append(New_Item => Mission);
        SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := PlayerShip.Missions.Last_Index;
        AddMessage(To_String(AcceptMessage), OtherMessage);
        GainExp(1, 4, TraderIndex);
        UpdateGame(5);
    end AcceptMission;

    procedure UpdateMissions(Minutes : Positive) is
        Time : Integer;
        I : Positive := PlayerShip.Missions.First_Index;
        procedure UpdateMission(Mission : in out Mission_Data) is
        begin
            Mission.Time := Time;
        end UpdateMission;
    begin
        while I <= PlayerShip.Missions.Last_Index loop
            Time := PlayerShip.Missions.Element(I).Time - Minutes;
            if Time < 1 then
                DeleteMission(I);
            else
                PlayerShip.Missions.Update_Element(Index => I, Process => UpdateMission'Access);
                I := I + 1;
            end if;
        end loop;
    end UpdateMissions;

    function FinishMission(MissionIndex : Positive) return GameStates is
        function GetRandom(Min, Max : Positive) return Positive is
            subtype Rand_Range is Positive range Min..Max;
            package Rand_Roll is new Discrete_Random(Rand_Range);
            Generator : Rand_Roll.Generator;
        begin
            Rand_Roll.Reset(Generator);
            return Rand_Roll.Random(Generator);
        end GetRandom;
    begin
        case PlayerShip.Missions.Element(MissionIndex).MType is
            when Deliver =>
                DockShip(True);
                UpdateGame(5);
                AddMessage("You finished mission 'Deliver " & 
                    To_String(Items_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) & "'.", OtherMessage);
            when Kill =>
                UpdateGame(GetRandom(15, 45));
                return StartCombat(PlayerShip.Missions.Element(MissionIndex).Target, False);
            when Explore =>
                UpdateGame(GetRandom(45, 75));
                AddMessage("You finished mission 'Explore selected area'.", OtherMessage);
        end case;
        DeleteMission(MissionIndex, False);
        return Sky_Map_View;
    end FinishMission;

    procedure DeleteMission(MissionIndex : Positive; Failed : Boolean := True) is
        MessageText : Unbounded_String := To_Unbounded_String("You failed mission ");
    begin
        if Failed then
            GainRep(PlayerShip.Missions.Element(MissionIndex).StartBase, -5);
            case PlayerShip.Missions.Element(MissionIndex).MType is
                when Deliver =>
                    Append(MessageText, "'Deliver " & To_String(Items_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) 
                        & "'.");
                when Kill =>
                    Append(MessageText, "'Destroy " & To_String(Enemies_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) 
                        & "'.");
                when Explore =>
                    Append(MessageText, "'Explore selected area'.");
            end case;
            AddMessage(To_String(MessageText), OtherMessage);
        else
            case PlayerShip.Missions.Element(MissionIndex).MType is
                when Deliver =>
                    GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 3);
                    GainRep(PlayerShip.Missions.Element(MissionIndex).StartBase, 2);
                when others =>
                    GainRep(PlayerShip.Missions.Element(MissionIndex).StartBase, 5);
            end case;
        end if;
        SkyMap(PlayerShip.Missions(MissionIndex).TargetX, PlayerShip.Missions(MissionIndex).TargetY).MissionIndex := 0;
        if PlayerShip.Missions.Element(MissionIndex).MType = Deliver then
            UpdateCargo(PlayerShip, PlayerShip.Missions.Element(MissionIndex).Target, -1);
        end if;
        PlayerShip.Missions.Delete(Index => MissionIndex, Count => 1);
    end DeleteMission;

end Missions;
