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
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Maps; use Maps;
with Items; use Items;
with Bases; use Bases;
with Messages; use Messages;
with Crew; use Crew;
with UserInterface; use UserInterface;
with Statistics; use Statistics;
with Game; use Game;
with Utils; use Utils;
with ShipModules; use ShipModules;

package body Missions is

    procedure GenerateMissions(BaseIndex : Positive) is
        TimeDiff : Natural;
        MissionsAmount, MissionX, MissionY, TmpBaseIndex, DiffX, DiffY : Positive;
        Mission : Mission_Data;
        MissionsItems, BasesInRange : Positive_Container.Vector;
        MinX, MinY, MaxX, MaxY : Integer;
        type Value_Type is digits 2 range 0.0..9999999.0;
        package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Value_Type);
        Enemies : Positive_Container.Vector;
        PlayerValue : Natural := 0;
    begin
        TimeDiff := (GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) - (SkyBases(BaseIndex).MissionsDate.Day + 
            ((30 * SkyBases(BaseIndex).MissionsDate.Month) * SkyBases(BaseIndex).MissionsDate.Year));
        if TimeDiff < 7 or SkyBases(BaseIndex).Owner = Abandoned then
            return;
        end if;
        case SkyBases(BaseIndex).Population is
            when 1..149 =>
                MissionsAmount := GetRandom(1, 5);
            when 150..299 =>
                MissionsAmount := GetRandom(1, 10);
            when others =>
                MissionsAmount := GetRandom(1, 15);
        end case;
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
            if I /= BaseIndex and SkyBases(I).SkyX in MinX..MaxX and SkyBases(I).SkyY in MinY..MaxY and SkyBases(I).Owner /= Abandoned 
            then
                BasesInRange.Append(New_Item => I);
            end if;
        end loop;
        while MissionsAmount > Positive(BasesInRange.Length) loop
            TmpBaseIndex := GetRandom(1, 1024);
            if BasesInRange.Find_Index(Item => TmpBaseIndex) = Positive_Container.No_Index and SkyBases(TmpBaseIndex).Owner /= Abandoned 
            then
                BasesInRange.Append(New_Item => TmpBaseIndex);
            end if;
        end loop;
        SkyBases(BaseIndex).Missions.Clear;
        for Module of PlayerShip.Modules loop
            case Modules_List.Element(Module.ProtoIndex).MType is
                when HULL | GUN | BATTERING_RAM =>
                    PlayerValue := PlayerValue + Module.MaxDurability +
                    (Module.Max_Value * 10);
                when ARMOR =>
                    PlayerValue := PlayerValue + Module.MaxDurability;
                when others =>
                    null;
            end case;
        end loop;
        for Item of PlayerShip.Cargo loop
            if Length(Items_List.Element(Item.ProtoIndex).IType) >= 4 then
                if Slice(Items_List.Element(Item.ProtoIndex).IType, 1, 4) = "Ammo" then
                    PlayerValue := PlayerValue + (Items_List.Element(Item.ProtoIndex).Value * 10);
                end if;
            end if;
        end loop;
        for I in ProtoShips_List.First_Index..ProtoShips_List.Last_Index loop
            if ProtoShips_List.Element(I).CombatValue <= PlayerValue and (ProtoShips_List.Element(I).Owner /= Poleis and 
                ProtoShips_List.Element(I).Owner /= Independent)
            then
                Enemies.Append(New_Item => I);
            end if;
        end loop;
        for I in 1..MissionsAmount loop
            Mission.MType := Missions_Types'Val(GetRandom(0, Missions_Types'Pos(Missions_Types'Last)));
            case Mission.MType is
                when Deliver => 
                    Mission.Target := MissionsItems.Element(GetRandom(MissionsItems.First_Index, MissionsItems.Last_Index));
                when Kill =>
                    Mission.Target := Enemies.Element(GetRandom(Enemies.First_Index, Enemies.Last_Index));
                when Patrol =>
                    Mission.Target := 0;
                when Explore =>
                    Mission.Target := 1;
                    for J in 1..10 loop
                        MissionX := GetRandom(MinX, MaxX);
                        MissionY := GetRandom(MinY, MaxY);
                        if not SkyMap(MissionX, MissionY).Visited then
                            Mission.Target := 0;
                            exit;
                        end if;
                    end loop;
                    if Mission.Target = 1 then
                        Mission.Target := 0;
                        Mission.MType := Patrol;
                    end if;
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
            case Mission.MType is
                when Deliver =>
                    Mission.Time := Positive(Value_Type(80) * Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2))));
                    Mission.Reward := (Mission.Time / 4);
                when Kill => 
                    Mission.Time := Positive(Value_Type(180) * Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2))));
                    Mission.Reward := (Mission.Time / 4);
                when Patrol | Explore =>
                    Mission.Time := Positive(Value_Type(180) * Value_Functions.Sqrt(Value_Type((DiffX ** 2) + (DiffY ** 2))));
                    Mission.Reward := (Mission.Time / 5);
            end case;
            Mission.StartBase := BaseIndex;
            Mission.Finished := False;
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
        for Mission of PlayerShip.Missions loop
            if Mission.StartBase = BaseIndex then
                MissionsLimit := MissionsLimit - 1;
            end if;
            exit when MissionsLimit = 0;
        end loop;
        if MissionsLimit < 1 then
            ShowDialog("You can't take any more missions from this base. ");
            return;
        end if;
        if Mission.MType = Deliver then
            if FreeCargo((0 - Items_List.Element(Mission.Target).Weight)) < 0 then
                ShowDialog("You don't have enough cargo space for take this mission.");
                return;
            end if;
        end if;
        TraderIndex := FindMember(Talk);
        Mission.StartBase := BaseIndex;
        Mission.Finished := False;
        AcceptMessage := To_Unbounded_String("You accepted mission ");
        case Mission.MType is
            when Deliver =>
                Append(AcceptMessage, "'Deliver " & To_String(Items_List.Element(Mission.Target).Name) & "'.");
                UpdateCargo(PlayerShip, Mission.Target, 1);
            when Kill =>
                Append(AcceptMessage, "'Destroy " & To_String(ProtoShips_List.Element(Mission.Target).Name) & "'.");
            when Patrol =>
                Append(AcceptMessage, "'Patrol selected area'.");
            when Explore => 
                Append(AcceptMessage, "'Explore selected area'.");
        end case;
        SkyBases(BaseIndex).Missions.Delete(Index => MissionIndex, Count => 1);
        PlayerShip.Missions.Append(New_Item => Mission);
        SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := PlayerShip.Missions.Last_Index;
        AddMessage(To_String(AcceptMessage), MissionMessage);
        GainExp(1, 4, TraderIndex);
        GameStats.AcceptedMissions := GameStats.AcceptedMissions + 1;
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

    procedure FinishMission(MissionIndex : Positive) is
    begin
        if PlayerShip.Speed /= DOCKED then
            DockShip(True);
        end if;
        UpdateGame(5);
        case PlayerShip.Missions.Element(MissionIndex).MType is
            when Deliver =>
                AddMessage("You finished mission 'Deliver " & 
                    To_String(Items_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) & "'.", MissionMessage);
            when Kill =>
                AddMessage("You finished mission 'Destroy " & 
                    To_String(ProtoShips_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) & "'.", MissionMessage);
            when Patrol =>
                AddMessage("You finished mission 'Patrol selected area'.", MissionMessage);
            when Explore =>
                AddMessage("You finished mission 'Explore selected area'.", MissionMessage);
        end case;
        DeleteMission(MissionIndex, False);
        GameStats.FinishedMissions := GameStats.FinishedMissions + 1;
    end FinishMission;

    procedure DeleteMission(MissionIndex : Positive; Failed : Boolean := True) is
        MessageText : Unbounded_String := To_Unbounded_String("You failed mission ");
        Mission : constant Mission_Data := PlayerShip.Missions.Element(MissionIndex);
        FreeSpace, RewardAmount : Integer;
    begin
        if Failed then
            GainRep(Mission.StartBase, -5);
            case Mission.MType is
                when Deliver =>
                    Append(MessageText, "'Deliver " & To_String(Items_List.Element(Mission.Target).Name) 
                        & "'.");
                when Kill =>
                    Append(MessageText, "'Destroy " & To_String(ProtoShips_List.Element(Mission.Target).Name) 
                        & "'.");
                when Patrol =>
                    Append(MessageText, "'Patrol selected area'.");
                when Explore =>
                    Append(MessageText, "'Explore selected area'.");
            end case;
            AddMessage(To_String(MessageText), MissionMessage);
        else
            case Mission.MType is
                when Deliver =>
                    GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 3);
                    GainRep(Mission.StartBase, 2);
                when others =>
                    GainRep(Mission.StartBase, 5);
            end case;
            RewardAmount := Mission.Reward;
            FreeSpace := FreeCargo((0 - RewardAmount));
            if FreeSpace < 0 then
                RewardAmount := RewardAmount + FreeSpace;
            end if;
            if RewardAmount > 0 then
                AddMessage("You received" & Integer'Image(RewardAmount) & " Charcollum for finished mission.", MissionMessage);
                UpdateCargo(PlayerShip, 1, RewardAmount);
            end if;
        end if;
        SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := 0;
        SkyMap(SkyBases(Mission.StartBase).SkyX, SkyBases(Mission.StartBase).SkyY).MissionIndex := 0;
        if Mission.MType = Deliver then
            UpdateCargo(PlayerShip, Mission.Target, -1);
        end if;
        PlayerShip.Missions.Delete(Index => MissionIndex, Count => 1);
        for I in PlayerShip.Missions.First_Index..PlayerShip.Missions.Last_Index loop
            if PlayerShip.Missions.Element(I).Finished then
                SkyMap(SkyBases(PlayerShip.Missions.Element(I).StartBase).SkyX, 
                    SkyBases(PlayerShip.Missions.Element(I).StartBase).SkyY).MissionIndex := I;
            else
                SkyMap(PlayerShip.Missions.Element(I).TargetX, PlayerShip.Missions.Element(I).TargetY).MissionIndex := I;
            end if;
        end loop;
    end DeleteMission;

    procedure UpdateMission(MissionIndex : Positive) is
        Mission : constant Mission_Data := PlayerShip.Missions.Element(MissionIndex);
        MessageText : Unbounded_String := To_Unbounded_String("Return to ") & SkyBases(Mission.StartBase).Name & 
            To_Unbounded_String(" to finish mission ");
        procedure UpdateFinished(Mission : in out Mission_Data) is
        begin
            Mission.Finished := True;
        end UpdateFinished;
    begin
        SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := 0;
        PlayerShip.Missions.Update_Element(Index => MissionIndex, Process => UpdateFinished'Access);
        SkyMap(SkyBases(Mission.StartBase).SkyX, SkyBases(Mission.StartBase).SkyY).MissionIndex := MissionIndex;
        case PlayerShip.Missions.Element(MissionIndex).MType is
            when Deliver =>
                Append(MessageText, "'Deliver " & To_String(Items_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) 
                & "'.");
            when Kill =>
                Append(MessageText, "'Destroy " & To_String(ProtoShips_List.Element(PlayerShip.Missions.Element(MissionIndex).Target).Name) 
                & "'.");
            when Patrol =>
                Append(MessageText, "'Patrol selected area'.");
            when Explore =>
                Append(MessageText, "'Explore selected area'.");
        end case;
        AddMessage(To_String(MessageText), MissionMessage);
    end UpdateMission;

end Missions;
