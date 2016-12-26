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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Messages; use Messages;
with Crafts; use Crafts;
with Items; use Items;
with Events; use Events;
with ShipModules; use ShipModules;
with Config; use Config;
with Statistics; use Statistics;
with Missions; use Missions;

package body Game is
    
    SaveVersion : constant String := "0.7";

    procedure NewGame(CharName, ShipName : Unbounded_String; Gender : Character) is
        type Rand_Range is range 1..1024;
        type Bases_Range is range 0..3;
        type Gender_Range is range 1..2;
        type Population_Range is range 10..500;
        package Rand_Int is new Discrete_Random(Rand_Range);
        package Rand_Base is new Discrete_Random(Bases_Range);
        package Rand_Gender is new Discrete_Random(Gender_Range);
        package Rand_Population is new Discrete_Random(Population_Range);
        Generator : Rand_Int.Generator;
        Generator2 : Rand_Base.Generator;
        Generator3 : Rand_Gender.Generator;
        Generator4 : Rand_Population.Generator;
        PosX, PosY : Rand_Range;
        RandomBase : Rand_Range;
        PilotName, EngineerName, GunnerName : Unbounded_String;
        ValidLocation : Boolean;
        TempX, TempY : Integer;
        PilotGender, EngineerGender, GunnerGender : Character;
        TmpSkills : Skills_Container.Vector;
        TmpRecruits : Recruit_Container.Vector;
        TmpMissions : Mission_Container.Vector;
    begin
        -- Save new game configuration
        NewGameSettings := (PlayerName => CharName, PlayerGender => Gender, ShipName => ShipName);
        SaveConfig;
        -- Set game statistics
        ClearGameStats;
        -- Set Game time
        GameDate := (Year => 1600, Month => 3, Day => 1, Hour => 8, Minutes => 0);
        -- Generate world
        Rand_Int.Reset(Generator);
        Rand_Base.Reset(Generator2);
        Rand_Gender.Reset(Generator3);
        Rand_Population.Reset(Generator4);
        SkyMap := (others => (others => (BaseIndex => 0, Visited => False, EventIndex => 0, MissionIndex => 0)));
        for I in Rand_Range loop
            loop
                ValidLocation := True;
                PosX := Rand_Int.Random(Generator);
                PosY := Rand_Int.Random(Generator);
                for J in -5..5 loop
                    TempX := Integer(PosX) + J;
                    if TempX < 1 then
                        TempX := 1;
                    end if;
                    if TempX > Integer(Rand_Range'Last) then
                        TempX := Integer(Rand_Range'Last);
                    end if;
                    for K in -5..5 loop
                        TempY := Integer(PosY) + K;
                        if TempY < 1 then
                            TempY := 1;
                        end if;
                        if TempY > Integer(Rand_Range'Last) then
                            TempY := Integer(Rand_Range'Last);
                        end if;
                        if SkyMap(TempX, TempY).BaseIndex > 0 then
                            ValidLocation := False;
                            exit;
                        end if;
                    end loop;
                    if not ValidLocation then
                        exit;
                    end if;
                end loop;
                if SkyMap(Integer(PosX), Integer(PosY)).BaseIndex > 0 then
                    ValidLocation := False;
                end if;
                exit when ValidLocation;
            end loop;
            SkyMap(Integer(PosX), Integer(PosY)) := (BaseIndex => Integer(I), Visited => False, EventIndex => 0, MissionIndex => 0);
            SkyBases(Integer(I)) := (Name => GenerateBaseName, Visited => (0, 0, 0, 0, 0), 
                SkyX => Integer(PosX), SkyY => Integer(PosY), BaseType =>
                Bases_Types'Val(Rand_Base.Random(Generator2)), Population =>
                Natural(Rand_Population.Random(Generator4)), RecruitDate => 
                (0, 0, 0, 0, 0), Recruits => TmpRecruits, Known => False,
                AskedForBases => False, AskedForEvents => (0, 0, 0, 0, 0),
                Reputation => (0, 0), MissionsDate => (0, 0, 0, 0, 0), Missions
                => TmpMissions);
        end loop;
        -- Place player ship in random large base
        loop
            RandomBase := Rand_Int.Random(Generator);
            exit when SkyBases(Integer(RandomBase)).Population > 299;
        end loop;
        -- Generate names for crew
        if Rand_Gender.Random(Generator3) = 1 then
            PilotGender := 'M';
        else
            PilotGender := 'F';
        end if;
        PilotName := GenerateMemberName(PilotGender);
        if Rand_Gender.Random(Generator3) = 1 then
            EngineerGender := 'M';
        else
            EngineerGender := 'F';
        end if;
        EngineerName := GenerateMemberName(EngineerGender);
        if Rand_Gender.Random(Generator3) = 1 then
            GunnerGender := 'M';
        else
            GunnerGender := 'F';
        end if;
        GunnerName := GenerateMemberName(GunnerGender);
        -- Create player ship with modules
        PlayerShip := CreateShip(1, ShipName, SkyBases(Integer(RandomBase)).SkyX,
            SkyBases(Integer(RandomBase)).SkyY, DOCKED);
        UpdateModule(PlayerShip, 4, "Name", To_String(CharName) & "'s Cabin");
        UpdateModule(PlayerShip, 4, "Owner", "1");
        UpdateModule(PlayerShip, 5, "Name", To_String(PilotName) & "'s Cabin");
        UpdateModule(PlayerShip, 5, "Owner", "2");
        UpdateModule(PlayerShip, 6, "Name", To_String(EngineerName) & "'s Cabin");
        UpdateModule(PlayerShip, 6, "Owner", "3");
        UpdateModule(PlayerShip, 7, "Name", To_String(GunnerName) & "'s Cabin");
        UpdateModule(PlayerShip, 7, "Owner", "4");
        UpdateModule(PlayerShip, 13, "Owner", "4");
        -- Add crew to ship
        TmpSkills.Append(New_Item => (4, 5, 0));
        PlayerShip.Crew.Append(New_Item => (Name => CharName, Gender => Gender,
            Health => 100, Tired => 0, Skills => TmpSkills, Hunger => 0, Thirst => 0, Order => Talk,
            PreviousOrder => Rest, OrderTime => 15)); 
        TmpSkills.Replace_Element(Index => 1, New_Item => (1, 5, 0));
        PlayerShip.Crew.Append(New_Item => (Name => PilotName, Gender => PilotGender,
            Health => 100, Tired => 0, Skills => TmpSkills, Hunger => 0, Thirst => 0, Order => Pilot,
            PreviousOrder => Rest, OrderTime => 15)); 
        TmpSkills.Replace_Element(Index => 1, New_Item => (2, 5, 0));
        PlayerShip.Crew.Append(New_Item => (Name => EngineerName, Gender => EngineerGender,
            Health => 100, Tired => 0, Skills => TmpSkills, Hunger => 0, Thirst => 0, Order => Engineer,
            PreviousOrder => Rest, OrderTime => 15)); 
        TmpSkills.Replace_Element(Index => 1, New_Item => (3, 5, 0));
        PlayerShip.Crew.Append(New_Item => (Name => GunnerName, Gender => GunnerGender,
            Health => 100, Tired => 0, Skills => TmpSkills, Hunger => 0, Thirst => 0, Order => Gunner,
            PreviousOrder => Rest, OrderTime => 15)); 
        -- Set known recipes
        Known_Recipes.Append(New_Item => 1);
        Known_Recipes.Append(New_Item => 2);
        Known_Recipes.Append(New_Item => 4);
        Known_Recipes.Append(New_Item => 20);
        -- Set current map field/sky base info
        SkyBases(Integer(RandomBase)).Visited := GameDate;
        SkyBases(Integer(RandomBase)).Known := True;
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited := True;
        GenerateRecruits(Integer(RandomBase));
        GenerateMissions(Integer(RandomBase));
    end NewGame;

    procedure UpdateGame(Minutes : Positive) is
        AddedHours, AddedMinutes : Natural;
        BaseIndex : constant Natural := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        TiredPoints : Natural := 0;
    begin
        for I in 1..Minutes loop
            if ((GameDate.Minutes + I) rem 15) = 0 then
                TiredPoints := TiredPoints + 1;
            end if;
        end loop;
        -- Update game time
        AddedMinutes := Minutes rem 60;
        AddedHours := Minutes / 60;
        GameDate.Minutes := GameDate.Minutes + AddedMinutes;
        if GameDate.Minutes > 59 then
            GameDate.Minutes := GameDate.Minutes - 60;
            GameDate.Hour := GameDate.Hour + 1;
        end if;
        GameDate.Hour := GameDate.Hour + AddedHours;
        if GameDate.Hour > 23 then
            GameDate.Hour := GameDate.Hour - 24;
            GameDate.Day := GameDate.Day + 1;
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = CABIN and 
                    PlayerShip.Modules.Element(I).Current_Value > 0
                then
                    UpdateModule(PlayerShip, I, "Current_Value", Natural'Image(PlayerShip.Modules.Element(I).Current_Value - 1));
                end if;
            end loop;
        end if;
        if GameDate.Day > 30 then
            GameDate.Day := 1;
            GameDate.Month := GameDate.Month + 1;
        end if;
        if GameDate.Month > 12 then
            GameDate.Month := 1;
            GameDate.Year := GameDate.Year + 1;
        end if;
        -- Update crew
        UpdateCrew(Minutes, TiredPoints);
        -- Repair ship (if needed)
        RepairShip(Minutes);
        -- Craft items
        Manufacturing(Minutes);
        -- Upgrade ship module
        UpgradeShip(Minutes);
        -- Update base
        if BaseIndex > 0 then
            if SkyBases(BaseIndex).Visited.Year = 0 then
                GameStats.BasesVisited := GameStats.BasesVisited + 1;
            end if;
            SkyBases(BaseIndex).Visited := GameDate;
            if not SkyBases(BaseIndex).Known then
                SkyBases(BaseIndex).Known := True;
                AddMessage("You discovered base " & To_String(SkyBases(BaseIndex).Name) & ".", OtherMessage);
            end if;
            GenerateRecruits(BaseIndex);
            GenerateMissions(BaseIndex);
        end if;
        -- Update map cell
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited = False then
            GameStats.MapVisited := GameStats.MapVisited + 1;
            SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).Visited := True;
        end if;
        -- Update events
        UpdateEvents(Minutes);
    end UpdateGame;

    procedure SaveGame is
        SaveGame : File_Type;
        RawValue : Unbounded_String;
        Messages : Natural := 10;
        StartLoop : Positive;
        Message : Message_Data;
        VisitedFields : Natural := 0;
    begin
        Create(SaveGame, Out_File, "data/savegame.dat");
        -- Save version
        Put(SaveGame, SaveVersion & ";");
        -- Save game date
        RawValue := To_Unbounded_String(Integer'Image(GameDate.Year));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(GameDate.Month));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(GameDate.Day));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(GameDate.Hour));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(GameDate.Minutes));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        -- Save map
        for X in 1..1024 loop
            for Y in 1..1024 loop
                if SkyMap(X, Y).Visited then
                    VisitedFields := VisitedFields + 1;
                end if;
            end loop;
        end loop;
        RawValue := To_Unbounded_String(Integer'Image(VisitedFields));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for X in 1..1024 loop
            for Y in 1..1024 loop
                if SkyMap(X, Y).Visited then
                    RawValue := To_Unbounded_String(Integer'Image(X));
                    Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                    RawValue := To_Unbounded_String(Integer'Image(Y));
                    Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                end if;
            end loop;
        end loop;
        -- Save bases
        for I in SkyBases'Range loop
            Put(SaveGame, To_String(SkyBases(I).Name) & ";");
            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Year));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            if SkyBases(I).Visited.Year > 0 then
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Month));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Day));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Hour));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Minutes));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end if;
            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyX));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyY));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Bases_Types'Pos(SkyBases(I).BaseType)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Population));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            if SkyBases(I).Visited.Year > 0 then
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Year));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Month));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Day));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(SkyBases(I).Recruits.Length'Img);
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                if SkyBases(I).Recruits.Length > 0 then
                    for J in SkyBases(I).Recruits.First_Index..SkyBases(I).Recruits.Last_Index loop
                        Put(SaveGame, To_String(SkyBases(I).Recruits.Element(J).Name) & ";");
                        Put(SaveGame, SkyBases(I).Recruits.Element(J).Gender & ";");
                        RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Recruits.Element(J).Price));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        RawValue := To_Unbounded_String(SkyBases(I).Recruits.Element(J).Skills.Length'Img);
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        for K in SkyBases(I).Recruits.Element(J).Skills.First_Index..SkyBases(I).Recruits.Element(J).Skills.Last_Index loop
                            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Recruits.Element(J).Skills.Element(K)(1)));
                            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Recruits.Element(J).Skills.Element(K)(2)));
                            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Recruits.Element(J).Skills.Element(K)(3)));
                            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        end loop;
                    end loop;
                end if;
                if SkyBases(I).AskedForBases then
                    Put(SaveGame, "Y;");
                else
                    Put(SaveGame, "N;");
                end if;
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).AskedForEvents.Year));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).AskedForEvents.Month));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).AskedForEvents.Day));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(1)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(2)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).MissionsDate.Year));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).MissionsDate.Month));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).MissionsDate.Day));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(SkyBases(I).Missions.Length'Img);
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                if SkyBases(I).Missions.Length > 0 then
                    for J in SkyBases(I).Missions.First_Index..SkyBases(I).Missions.Last_Index loop
                        RawValue := To_Unbounded_String(Integer'Image(Missions_Types'Pos(SkyBases(I).Missions.Element(J).MType)));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Missions.Element(J).Target));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Missions.Element(J).Time));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Missions.Element(J).TargetX));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Missions.Element(J).TargetY));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                        RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Missions.Element(J).Reward));
                        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                    end loop;
                end if;
            end if;
            if SkyBases(I).Known then
                Put(SaveGame, "Y;");
            else
                Put(SaveGame, "N;");
            end if;
        end loop;
        -- Save player ship
        Put(SaveGame, To_String(PlayerShip.Name) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.UpgradeModule));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationX));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationY));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.RepairModule));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(PlayerShip.Modules.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Put(SaveGame, To_String(PlayerShip.Modules.Element(I).Name) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).ProtoIndex));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).Weight));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).Current_Value));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).Max_Value));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).Durability));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).MaxDurability));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).Owner));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Modules.Element(I).UpgradeProgress));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(ShipUpgrade'Pos(PlayerShip.Modules.Element(I).UpgradeAction)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        RawValue := To_Unbounded_String(PlayerShip.Cargo.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Cargo.Element(I).ProtoIndex));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Cargo.Element(I).Amount));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        RawValue := To_Unbounded_String(PlayerShip.Crew.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            Put(SaveGame, To_String(PlayerShip.Crew.Element(I).Name) & ";");
            Put(SaveGame, PlayerShip.Crew.Element(I).Gender & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Health));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Tired));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Hunger));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Thirst));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Crew_Orders'Pos(PlayerShip.Crew.Element(I).Order)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Crew_Orders'Pos(PlayerShip.Crew.Element(I).PreviousOrder)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).OrderTime));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(PlayerShip.Crew.Element(I).Skills.Length'Img);
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            for J in PlayerShip.Crew.Element(I).Skills.First_Index..PlayerShip.Crew.Element(I).Skills.Last_Index loop
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills.Element(J)(1)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills.Element(J)(2)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills.Element(J)(3)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end loop;
        end loop;
        RawValue := To_Unbounded_String(PlayerShip.Missions.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        if PlayerShip.Missions.Length > 0 then
            for I in PlayerShip.Missions.First_Index..PlayerShip.Missions.Last_Index loop
                RawValue := To_Unbounded_String(Integer'Image(Missions_Types'Pos(PlayerShip.Missions.Element(I).MType)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Missions.Element(I).Target));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Missions.Element(I).Time));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Missions.Element(I).TargetX));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Missions.Element(I).TargetY));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Missions.Element(I).Reward));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Missions.Element(I).StartBase));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end loop;
        end if;
        -- Save known recipes
        RawValue := To_Unbounded_String(Known_Recipes.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in Known_Recipes.First_Index..Known_Recipes.Last_Index loop
            RawValue := To_Unbounded_String(Integer'Image(Known_Recipes.Element(I)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        -- Save messages
        if Messages > MessagesAmount then
            Messages := MessagesAmount;
        end if;
        RawValue := To_Unbounded_String(Messages'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        if Messages > 0 then
            StartLoop := MessagesAmount - Messages + 1;
            for I in StartLoop..MessagesAmount loop
                Message := GetMessage(I);
                RawValue := To_Unbounded_String(Integer'Image(Message_Type'Pos(Message.MType)));
                Put(SaveGame, To_String(Message.Message) & ";" & To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end loop;
        end if;
        -- Save events
        RawValue := To_Unbounded_String(Events_List.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in Events_List.First_Index..Events_List.Last_Index loop
            RawValue := To_Unbounded_String(Integer'Image(Events_Types'Pos(Events_List.Element(I).Etype)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Events_List.Element(I).SkyX));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Events_List.Element(I).SkyY));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Events_List.Element(I).Time));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Events_List.Element(I).Data));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        -- Save game statistics
        RawValue := To_Unbounded_String(GameStats.DestroyedShips.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in GameStats.DestroyedShips.First_Index..GameStats.DestroyedShips.Last_Index loop
            RawValue := To_Unbounded_String(Integer'Image(GameStats.DestroyedShips.Element(I).ProtoIndex));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(GameStats.DestroyedShips.Element(I).Amount));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        RawValue := To_Unbounded_String(Positive'Image(GameStats.BasesVisited));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Positive'Image(GameStats.MapVisited));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Positive'Image(GameStats.DistanceTraveled));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Positive'Image(GameStats.CraftingOrders));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        Close(SaveGame);
    end SaveGame;

    function LoadGame return Unbounded_String is
        SaveGame : File_Type;
        VectorLength, SkillsLength : Natural;
        Skills : Skills_Container.Vector;
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector; 
        ShipCrew : Crew_Container.Vector;
        Message : Unbounded_String;
        MType : Message_Type;
        BaseRecruits : Recruit_Container.Vector;
        VisitedFields : Positive;
        BaseMissions : Mission_Container.Vector;
        function ReadData return Unbounded_String is
            RawData : Unbounded_String := To_Unbounded_String("");
            Char : Character;
        begin
            Get(SaveGame, Char);
            while Char not in ';' loop
                Append(RawData, Char);
                Get(SaveGame, Char);
            end loop;
            return RawData;
        end ReadData;
        procedure UpdateMember(Member : in out Member_Data) is
        begin
            Member.Skills := Skills;
        end UpdateMember;
        procedure UpdateRecruit(Recruit : in out Recruit_Data) is
        begin
            Recruit.Skills := Skills;
        end UpdateRecruit;
    begin
        Open(SaveGame, In_File, "data/savegame.dat");
        -- Check save version
        if ReadData /= SaveVersion then
            Close(SaveGame);
            return To_Unbounded_String("This saved game is incompatible with this version of game and can't be loaded.");
        end if;
        -- Load game date
        GameDate.Year := Natural'Value(To_String(ReadData));
        GameDate.Month := Natural'Value(To_String(ReadData));
        GameDate.Day := Natural'Value(To_String(ReadData));
        GameDate.Hour := Natural'Value(To_String(ReadData));
        GameDate.Minutes := Natural'Value(To_String(ReadData));
        -- Load sky map
        SkyMap := (others => (others => (BaseIndex => 0, Visited => False, EventIndex => 0, MissionIndex => 0)));
        VisitedFields := Positive'Value(To_String(ReadData));
        for I in 1..VisitedFields loop
            SkyMap(Positive'Value(To_String(ReadData)), Positive'Value(To_String(ReadData))).Visited := True;
        end loop;
        -- Load sky bases
        for I in SkyBases'Range loop
            SkyBases(I) := (Name => ReadData, Visited => (0, 0, 0, 0, 0), SkyX => 0, SkyY => 0,
                BaseType => Industrial, Population => 0, RecruitDate => (0, 0, 0, 0, 0), 
                Recruits => BaseRecruits, Known => False, AskedForBases => False, 
                AskedForEvents => (0, 0, 0, 0, 0), Reputation => (0, 0),
                MissionsDate => (0, 0, 0, 0,0), Missions => BaseMissions);
            SkyBases(I).Visited.Year := Natural'Value(To_String(ReadData));
            if SkyBases(I).Visited.Year > 0 then
                SkyBases(I).Visited.Month := Natural'Value(To_String(ReadData));
                SkyBases(I).Visited.Day := Natural'Value(To_String(ReadData));
                SkyBases(I).Visited.Hour := Natural'Value(To_String(ReadData));
                SkyBases(I).Visited.Minutes := Natural'Value(To_String(ReadData));
            end if;
            SkyBases(I).SkyX := Integer'Value(To_String(ReadData));
            SkyBases(I).SkyY := Integer'Value(To_String(ReadData));
            SkyBases(I).BaseType := Bases_Types'Val(Integer'Value(To_String(ReadData)));
            SkyBases(I).Population := Natural'Value(To_String(ReadData));
            if SkyBases(I).Visited.Year > 0 then
                SkyBases(I).RecruitDate.Year := Natural'Value(To_String(ReadData));
                SkyBases(I).RecruitDate.Month := Natural'Value(To_String(ReadData));
                SkyBases(I).RecruitDate.Day := Natural'Value(To_String(ReadData));
                VectorLength := Natural'Value(To_String(ReadData));
                if VectorLength > 0 then
                    for J in 1..VectorLength loop
                        Skills.Clear;
                        BaseRecruits.Append(New_Item => (Name => ReadData, Gender => Element(ReadData, 1), 
                            Price => Positive'Value(To_String(ReadData)), Skills => Skills));
                        SkillsLength := Positive'Value(To_String(ReadData));
                        for K in 1..SkillsLength loop
                            Skills.Append(New_Item => (Natural'Value(To_String(ReadData)),
                            Natural'Value(To_String(ReadData)), Natural'Value(To_String(ReadData))));
                        end loop;
                        BaseRecruits.Update_Element(Index => BaseRecruits.Last_Index,
                            Process => UpdateRecruit'Access);
                    end loop;
                    SkyBases(I).Recruits := BaseRecruits;
                    BaseRecruits.Clear;
                end if;
                if ReadData = To_Unbounded_String("Y") then
                    SkyBases(I).AskedForBases := True;
                end if;
                SkyBases(I).AskedForEvents.Year := Natural'Value(To_String(ReadData));
                SkyBases(I).AskedForEvents.Month := Natural'Value(To_String(ReadData));
                SkyBases(I).AskedForEvents.Day := Natural'Value(To_String(ReadData));
                SkyBases(I).Reputation(1) := Integer'Value(To_String(ReadData));
                SkyBases(I).Reputation(2) := Integer'Value(To_String(ReadData));
                SkyBases(I).MissionsDate.Year := Natural'Value(To_String(ReadData));
                SkyBases(I).MissionsDate.Month := Natural'Value(To_String(ReadData));
                SkyBases(I).MissionsDate.Day := Natural'Value(To_String(ReadData));
                VectorLength := Natural'Value(To_String(ReadData));
                if VectorLength > 0 then
                    for J in 1..VectorLength loop
                        BaseMissions.Append(New_Item => (MType => Missions_Types'Val(Integer'Value(To_String(ReadData))),
                            Target => Natural'Value(To_String(ReadData)), Time => Integer'Value(To_String(ReadData)), 
                            TargetX => Integer'Value(To_String(ReadData)), TargetY => Integer'Value(To_String(ReadData)),
                            Reward => Integer'Value(To_String(ReadData)), StartBase => I));
                    end loop;
                    SkyBases(I).Missions := BaseMissions;
                    BaseMissions.Clear;
                end if;
            end if;
            if ReadData = To_Unbounded_String("Y") then
                SkyBases(I).Known := True;
            end if;
            SkyMap(SkyBases(I).SkyX, SkyBases(I).SkyY).BaseIndex := I;
        end loop;
        -- Load player ship
        PlayerShip.Name := ReadData;
        PlayerShip.SkyX := Integer'Value(To_String(ReadData));
        PlayerShip.SkyY := Integer'Value(To_String(ReadData));
        PlayerShip.Speed := ShipSpeed'Val(Integer'Value(To_String(ReadData)));
        PlayerShip.UpgradeModule := Integer'Value(To_String(ReadData));
        PlayerShip.DestinationX := Integer'Value(To_String(ReadData));
        PlayerShip.DestinationY := Integer'Value(To_String(ReadData));
        PlayerShip.RepairModule := Integer'Value(To_String(ReadData));
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            ShipModules.Append(New_Item => (Name => ReadData, ProtoIndex =>
                Integer'Value(To_String(ReadData)), Weight =>
                Natural'Value(To_String(ReadData)), Current_Value =>
                Integer'Value(To_String(ReadData)), Max_Value =>
                Integer'Value(To_String(ReadData)), Durability =>
                Integer'Value(To_String(ReadData)), MaxDurability =>
                Integer'Value(To_String(ReadData)), Owner =>
                Integer'Value(To_String(ReadData)), UpgradeProgress =>
                Integer'Value(To_String(ReadData)), UpgradeAction =>
                ShipUpgrade'Val(Integer'Value(To_String(ReadData)))));
        end loop;
        PlayerShip.Modules := ShipModules;
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            ShipCargo.Append(New_Item => (ProtoIndex =>
                Positive'Value(To_String(ReadData)), Amount =>
                Positive'Value(To_String(ReadData))));
        end loop;
        PlayerShip.Cargo := ShipCargo;
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            Skills.Clear;
            ShipCrew.Append(New_Item => (Name => ReadData, Gender => Element(ReadData, 1), 
                Health => Natural'Value(To_String(ReadData)), Tired =>
                Natural'Value(To_String(ReadData)), Skills => Skills, Hunger => 
                Natural'Value(To_String(ReadData)), Thirst =>
                Natural'Value(To_String(ReadData)), Order =>
                Crew_Orders'Val(Integer'Value(To_String(ReadData))), 
                PreviousOrder => Crew_Orders'Val(Integer'Value(To_String(ReadData))), 
                OrderTime => Integer'Value(To_String(ReadData))));
            SkillsLength := Positive'Value(To_String(ReadData));
            for J in 1..SkillsLength loop
                Skills.Append(New_Item => (Natural'Value(To_String(ReadData)),
                    Natural'Value(To_String(ReadData)), Natural'Value(To_String(ReadData))));
            end loop;
            ShipCrew.Update_Element(Index => ShipCrew.Last_Index,
                Process => UpdateMember'Access);
        end loop;
        PlayerShip.Crew := ShipCrew;
        VectorLength := Natural'Value(To_String(ReadData));
        if VectorLength > 0 then
            for I in 1..VectorLength loop
                BaseMissions.Append(New_Item => (MType => Missions_Types'Val(Integer'Value(To_String(ReadData))),
                    Target => Natural'Value(To_String(ReadData)), Time => Integer'Value(To_String(ReadData)), 
                    TargetX => Integer'Value(To_String(ReadData)), TargetY => Integer'Value(To_String(ReadData)),
                    Reward => Integer'Value(To_String(ReadData)), StartBase => Integer'Value(To_String(ReadData))));
                SkyMap(BaseMissions.Element(I).TargetX, BaseMissions.Element(I).TargetY).MissionIndex := I;
            end loop;
            PlayerShip.Missions := BaseMissions;
        end if;
        -- Load known recipes
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            Known_Recipes.Append(New_Item => Positive'Value(To_String(ReadData)));
        end loop;
        -- Load messages
        VectorLength := Integer'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            Message := ReadData;
            MType := Message_Type'Val(Integer'Value(To_String(ReadData)));
            RestoreMessage(Message, MType);
        end loop;
        -- Load events
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            Events_List.Append(New_Item => (Etype =>
                Events_Types'Val(Integer'Value(To_String(ReadData))), SkyX =>
                Integer'Value(To_String(ReadData)), SkyY => Integer'Value(To_String(ReadData)), 
                Time => Integer'Value(To_String(ReadData)), Data => Integer'Value(To_String(ReadData))));
            SkyMap(Events_List.Element(I).SkyX, Events_List.Element(I).SkyY).EventIndex := I;
        end loop;
        -- Load game statistics
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            GameStats.DestroyedShips.Append(New_Item => (ProtoIndex => Positive'Value(To_String(ReadData)),
                Amount => Positive'Value(To_String(ReadData))));
        end loop;
        GameStats.BasesVisited := Positive'Value(To_String(ReadData));
        GameStats.MapVisited := Positive'Value(To_String(ReadData));
        GameStats.DistanceTraveled := Positive'Value(To_String(ReadData));
        GameStats.CraftingOrders := Positive'Value(To_String(ReadData));
        Close(SaveGame);
        return Null_Unbounded_String;
    exception
        when CONSTRAINT_ERROR | END_ERROR =>
            Close(SaveGame);
            return To_Unbounded_String("Can't load savegame file. Invalid data.");
    end LoadGame;

    function LoadData return Boolean is
        DataFile : File_Type;
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex, StartIndex, EndIndex, Amount : Natural;
        FieldsNames : constant array (1..18) of Unbounded_String := (To_Unbounded_String("BasesSyllablesPre"),
            To_Unbounded_String("BasesSyllablesStart"), To_Unbounded_String("BasesSyllablesEnd"), 
            To_Unbounded_String("BasesSyllablesPost"), To_Unbounded_String("MaleSyllablesStart"), 
            To_Unbounded_String("MaleSyllablesMiddle"), To_Unbounded_String("MaleSyllablesEnd"), 
            To_Unbounded_String("FemaleSyllablesEnd"), To_Unbounded_String("SkillsNames"),
            To_Unbounded_String("ItemsTypes"), To_Unbounded_String("MaleVocals"),
            To_Unbounded_String("MaleConsonants"), To_Unbounded_String("FemaleSyllablesStart"),
            To_Unbounded_String("FemaleSyllablesMiddle"), To_Unbounded_String("FemaleVocals"), 
            To_Unbounded_String("ShipSyllablesStart"), To_Unbounded_String("ShipSyllablesMiddle"),
            To_Unbounded_String("ShipSyllablesEnd"));
    begin
        if BaseSyllablesStart.Length > 0 then
            return True;
        end if;
        if not Exists("data/game.dat") then
            return False;
        end if;
        Open(DataFile, In_File, "data/game.dat");
        while not End_Of_File(DataFile) loop
            RawData := To_Unbounded_String(Get_Line(DataFile));
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            for I in FieldsNames'Range loop
                if FieldName = FieldsNames(I) then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                    for J in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        case I is
                            when 1 =>
                                BaseSyllablesPre.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 2 =>
                                BaseSyllablesStart.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 3 =>
                                BaseSyllablesEnd.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 4 =>
                                BaseSyllablesPost.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 5 =>
                                MaleSyllablesStart.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 6 =>
                                MaleSyllablesMiddle.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 7 =>
                                MaleSyllablesEnd.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 8 =>
                                FemaleSyllablesEnd.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 9 =>
                                Skills_Names.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 10 =>
                                Items_Types.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 11 =>
                                MaleVocals.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 12 =>
                                MaleConsonants.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 13 =>
                                FemaleSyllablesStart.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 14 =>
                                FemaleSyllablesMiddle.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 15 =>
                                FemaleVocals.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 16 =>
                                ShipSyllablesStart.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 17 =>
                                ShipSyllablesMiddle.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                            when 18 =>
                                ShipSyllablesEnd.Append(New_Item => To_Unbounded_String(Slice(Value, StartIndex, EndIndex - 1)));
                        end case;
                        StartIndex := EndIndex + 2;
                    end loop;
                    exit;
                end if;
            end loop;
        end loop;
        Close(DataFile);
        return True;
    end LoadData;
end Game;
