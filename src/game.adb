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
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Messages; use Messages;
with Prototypes; use Prototypes;

package body Game is

    procedure NewGame(CharName, ShipName : Unbounded_String) is
        type Rand_Range is range 1..1024;
        type Bases_Range is range 0..2;
        package Rand_Int is new Discrete_Random(Rand_Range);
        package Rand_Base is new Discrete_Random(Bases_Range);
        Generator : Rand_Int.Generator;
        Generator2 : Rand_Base.Generator;
        PosX, PosY : Rand_Range;
        RandomBase : Rand_Range;
        BaseType : Bases_Range;
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector; 
        ShipCrew : Crew_Container.Vector;
        PilotName, EngineerName, GunnerName : Unbounded_String;
    begin
        -- Set Game time
        GameDate := (Year => 1600, Month => 3, Day => 1, Hour => 8, Minutes => 0);
        -- Generate world
        Rand_Int.Reset(Generator);
        Rand_Base.Reset(Generator2);
        SkyMap := (others => (others => (BaseIndex => 0)));
        for I in Rand_Range loop
            PosX := Rand_Int.Random(Generator);
            PosY := Rand_Int.Random(Generator);
            BaseType := Rand_Base.Random(Generator2);
            SkyMap(Integer(PosX), Integer(PosY)) := (BaseIndex => Integer(I));
            SkyBases(Integer(I)) := (Name => GenerateBaseName, Visited => False, 
                SkyX => Integer(PosX), SkyY => Integer(PosY), BaseType => Bases_Types'Val(BaseType));
        end loop;
        -- Place player ship in random base
        RandomBase := Rand_Int.Random(Generator);
        -- Generate names for crew
        PilotName := GenerateMemberName;
        EngineerName := GenerateMemberName;
        GunnerName := GenerateMemberName;
        -- Create player ship with modules
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Hawk hull"), 
            MType => HULL, Weight => 4000, Current_Value => 10,
            Max_Value => 10, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Bronze armor"), 
            MType => ARMOR, Weight => 4000, Current_Value => 5,
            Max_Value => 10, Durability => 200, MaxDurability => 200));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Small engine"), 
            MType => ENGINE, Weight => 1000, Current_Value => 0,
            Max_Value => 2000, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => CharName & To_Unbounded_String("'s Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => PilotName & To_Unbounded_String("'s Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => EngineerName & To_Unbounded_String("'s Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => GunnerName & To_Unbounded_String("'s Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Cocpit"), 
            MTYPE => COCPIT, Weight => 200, Current_Value => 0, Max_Value =>
            0, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Alchemy Lab"), 
            MTYPE => ALCHEMY_LAB, Weight => 400, Current_Value => 0, Max_Value =>
            0, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Cargo space"), 
            MTYPE => CARGO, Weight => 0, Current_Value => 0, Max_Value =>
            5000, Durability => 10, MaxDurability => 10));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Cargo space"), 
            MTYPE => CARGO, Weight => 0, Current_Value => 0, Max_Value =>
            5000, Durability => 10, MaxDurability => 10));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Turret"), 
            MTYPE => TURRET, Weight => 50, Current_Value => 0, Max_Value =>
            0, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("20mm gun"), 
            MTYPE => GUN, Weight => 30, Current_Value => 4, Max_Value =>
            10, Durability => 20, MaxDurability => 20));
        -- Add cargo to ship
        ShipCargo.Append(New_Item => (ProtoIndex => 1, Amount => 2000));
        ShipCargo.Append(New_Item => (ProtoIndex => 2, Amount => 100));
        ShipCargo.Append(New_Item => (ProtoIndex => 3, Amount => 200));
        ShipCargo.Append(New_Item => (ProtoIndex => 4, Amount => 500));
        ShipCargo.Append(New_Item => (ProtoIndex => 5, Amount => 100));
        -- Add crew to ship
        ShipCrew.Append(New_Item => (Name => CharName,
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (0, 0), (5,0)), 
            Hunger => 0, Thirst => 0, Order => Rest)); 
        ShipCrew.Append(New_Item => (Name => PilotName,
            Health => 100, Tired => 0, Skills => ((5, 0), (0, 0), (0, 0), (0,0)), 
            Hunger => 0, Thirst => 0, Order => Pilot)); 
        ShipCrew.Append(New_Item => (Name => EngineerName,
            Health => 100, Tired => 0, Skills => ((0, 0), (5, 0), (0, 0), (0,0)), 
            Hunger => 0, Thirst => 0, Order => Engineer)); 
        ShipCrew.Append(New_Item => (Name => GunnerName,
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (5, 0), (0, 0)),
            Hunger => 0, Thirst => 0, Order => Rest)); 
        PlayerShip := (Name => ShipName, SkyX => SkyBases(Integer(RandomBase)).SkyX, SkyY =>
            SkyBases(Integer(RandomBase)).SkyY, Speed => DOCKED, Modules =>
            ShipModules, Cargo => ShipCargo, Crew => ShipCrew);
        SkyBases(Integer(RandomBase)).Visited := True;
    end NewGame;

    procedure UpdateGame(Minutes : Positive) is
        TiredLevel, HungerLevel, ThirstLevel : Integer := 0;
        AddedHours, AddedMinutes : Natural;
        TiredPoints : Natural := 0;
        HealthLevel : Integer := 100;
        RepairPoints : Natural := 0;
        ProtoIndex : Positive;
        procedure UpdateMember(Member : in out Member_Data) is
        begin
            Member.Tired := TiredLevel;
            if TiredLevel > 80 and Member.Order /= Rest then
                Member.Order := Rest;
                AddMessage(To_String(Member.Name) & " is too tired to work, going rest.");
            end if;
            if HungerLevel > 80 then
                if Consume(Food) then
                    HungerLevel := HungerLevel - 80;
                    if HungerLevel < 0 then
                        HungerLevel := 0;
                    end if;
                else
                    AddMessage(To_String(Member.Name) & " is hungry, but can't find anything to eat.");
                end if;
            end if;
            Member.Hunger := HungerLevel;
            if ThirstLevel > 40 then
                if Consume(Drink) then
                    ThirstLevel := ThirstLevel - 40;
                    if ThirstLevel < 0 then
                        ThirstLevel := 0;
                    end if;
                else
                    AddMessage(To_String(Member.Name) & " is thirsty, but can't find anything to drink.");
                end if;
            end if;
            Member.Thirst := ThirstLevel;
            Member.Health := HealthLevel;
        end UpdateMember;
    begin
        for I in 1..Minutes loop
            if ((GameDate.Minutes + I) rem 15) = 0 then
                TiredPoints := TiredPoints + 1;
            end if;
        end loop;
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
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            HealthLevel := PlayerShip.Crew.Element(I).Health;
            if PlayerShip.Crew.Element(I).Order = Rest then
                TiredLevel := 0;
                if PlayerShip.Crew.Element(I).Tired > 0 then
                    TiredLevel := PlayerShip.Crew.Element(I).Tired - Minutes;
                    if TiredLevel < 0 then
                        TiredLevel := 0;
                    end if;
                end if;
                if HealthLevel < 100 then
                    HealthLevel := HealthLevel + TiredPoints;
                end if;
            else
                TiredLevel := PlayerShip.Crew.Element(I).Tired + TiredPoints;
                if TiredLevel > 100 then
                    TiredLevel := 100;
                end if;
                case PlayerShip.Crew.Element(I).Order is
                    when Pilot =>
                        GainExp(TiredPoints, 1, I);
                    when Engineer =>
                        GainExp(TiredPoints, 2, I);
                    when Repair =>
                        RepairPoints := RepairPoints + TiredPoints;
                        GainExp(TiredPoints, 2, I);
                    when others =>
                        null;
                end case;
            end if;
            HungerLevel := PlayerShip.Crew.Element(I).Hunger + TiredPoints;
            if HungerLevel > 100 then
                HungerLevel := 100;
            end if;
            if PlayerShip.Crew.Element(I).Hunger = 100 then
                HealthLevel := HealthLevel - TiredPoints;
            end if;
            ThirstLevel := PlayerShip.Crew.Element(I).Thirst + TiredPoints;
            if ThirstLevel > 100 then
                ThirstLevel := 100;
            end if;
            if PlayerShip.Crew.Element(I).Thirst = 100 then
                HealthLevel := HealthLevel - TiredPoints;
            end if;
            if HealthLevel < 0 then
                HealthLevel := 0;
            end if;
            PlayerShip.Crew.Update_Element(Index => I, Process => UpdateMember'Access);
        end loop;
        -- Repair ship (if needed)
        if RepairPoints > 0 then
            -- Limit repair point depends on amount of repair materials
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if Objects_Prototypes(PlayerShip.Cargo.Element(I).ProtoIndex).Itype = RepairMaterial then
                    if PlayerShip.Cargo.Element(I).Amount < RepairPoints then
                        RepairPoints := PlayerShip.Cargo.Element(I).Amount;
                    end if;
                    ProtoIndex := PlayerShip.Cargo.Element(I).ProtoIndex;
                    exit;
                end if;
            end loop;
            -- Repair modules
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    if PlayerShip.Modules.Element(I).Durability + RepairPoints > PlayerShip.Modules.Element(I).MaxDurability then
                        RepairPoints := PlayerShip.Modules.Element(I).MaxDurability - (PlayerShip.Modules.Element(I).Durability +
                            RepairPoints);
                        UpdateCargo(ProtoIndex, (PlayerShip.Modules.Element(I).Durability - 
                            PlayerShip.Modules.Element(I).MaxDurability));
                        UpdateModule(I, "Durability", (PlayerShip.Modules.Element(I).MaxDurability - 
                            PlayerShip.Modules.Element(I).Durability));
                    else
                        UpdateCargo(ProtoIndex, (1 - RepairPoints));
                        UpdateModule(I, "Durability", RepairPoints);
                        RepairPoints := 0;
                    end if;
                    if RepairPoints = 0 then
                        exit;
                    end if;
                end if;
            end loop;
            -- Send repair team on break if all is ok
            if RepairPoints > 0 then
                AddMessage("All repairs are finished.");
                for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(I).Order = Repair then
                        GiveOrders(I, Rest);
                    end if;
                end loop;
            end if;
        end if;
    end UpdateGame;

    procedure SaveGame is
        SaveGame : File_Type;
        RawValue : Unbounded_String;
    begin
        Create(SaveGame, Out_File, "data/savegame.dat");
        -- Save version
        Put(SaveGame, "0.2;");
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
        -- Save bases
        for I in SkyBases'Range loop
            Put(SaveGame, To_String(SkyBases(I).Name) & ";");
            if SkyBases(I).Visited then
                Put(SaveGame, "1;");
            else
                Put(SaveGame, "0;");
            end if;
            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyX));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyY));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Bases_Types'Pos(SkyBases(I).BaseType)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        -- Save player ship
        Put(SaveGame, To_String(PlayerShip.Name) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        RawValue := To_Unbounded_String(PlayerShip.Modules.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Put(SaveGame, To_String(PlayerShip.Modules.Element(I).Name) & ";");
            RawValue := To_Unbounded_String(Integer'Image(ModuleType'Pos(PlayerShip.Modules.Element(I).MType)));
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
        end loop;
        RawValue := To_Unbounded_String(PlayerShip.Cargo.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Cargo.Element(I).ProtoIndex));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Cargo.Element(I).Amount));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        end loop;
        RawValue := To_Unbounded_String(PlayerShip.Cargo.Length'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            Put(SaveGame, To_String(PlayerShip.Crew.Element(I).Name) & ";");
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
            for J in Skills_Array'Range loop
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills(J, 1)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills(J, 2)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end loop;
        end loop;
        Close(SaveGame);
    end SaveGame;

    function LoadGame return Boolean is
        SaveGame : File_Type;
        VectorLength : Positive;
        Skills : Skills_Array := ((0, 0), (0, 0), (0, 0), (0 ,0));
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector; 
        ShipCrew : Crew_Container.Vector;
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
    begin
        Open(SaveGame, In_File, "data/savegame.dat");
        -- Check save version
        if ReadData /= "0.2" then
            Close(SaveGame);
            return False;
        end if;
        -- Load game date
        GameDate.Year := Natural'Value(To_String(ReadData));
        GameDate.Month := Natural'Value(To_String(ReadData));
        GameDate.Day := Natural'Value(To_String(ReadData));
        GameDate.Hour := Natural'Value(To_String(ReadData));
        GameDate.Minutes := Natural'Value(To_String(ReadData));
        -- Load sky bases
        for I in SkyBases'Range loop
            SkyBases(I) := (Name => ReadData, Visited => False, SkyX => 0, SkyY => 0,
                BaseType => Industrial);
            if To_String(ReadData) = "1" then
                SkyBases(I).Visited := True;
            end if;
            SkyBases(I).SkyX := Integer'Value(To_String(ReadData));
            SkyBases(I).SkyY := Integer'Value(To_String(ReadData));
            SkyBases(I).BaseType := Bases_Types'Val(Integer'Value(To_String(ReadData)));
            SkyMap(SkyBases(I).SkyX, SkyBases(I).SkyY).BaseIndex := I;
        end loop;
        -- Load player ship
        PlayerShip.Name := ReadData;
        PlayerShip.SkyX := Integer'Value(To_String(ReadData));
        PlayerShip.SkyY := Integer'Value(To_String(ReadData));
        PlayerShip.Speed := ShipSpeed'Val(Integer'Value(To_String(ReadData)));
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            ShipModules.Append(New_Item => (Name => ReadData, Mtype =>
                ModuleType'Val(Integer'Value(To_String(ReadData))), Weight =>
                Natural'Value(To_String(ReadData)), Current_Value =>
                Integer'Value(To_String(ReadData)), Max_Value =>
                Integer'Value(To_String(ReadData)), Durability =>
                Integer'Value(To_String(ReadData)), MaxDurability =>
                Integer'Value(To_String(ReadData))));
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
            ShipCrew.Append(New_Item => (Name => ReadData, Health =>
                Natural'Value(To_String(ReadData)), Tired =>
                Natural'Value(To_String(ReadData)), Skills => Skills, Hunger => 
                Natural'Value(To_String(ReadData)), Thirst =>
                Natural'Value(To_String(ReadData)), Order =>
                Crew_Orders'Val(Integer'Value(To_String(ReadData)))));
            for J in Skills_Array'Range loop
                Skills(J, 1) := Natural'Value(To_String(ReadData));
                Skills(J, 2) := Natural'Value(To_String(ReadData));
            end loop;
            ShipCrew.Update_Element(Index => ShipCrew.Last_Index,
                Process => UpdateMember'Access);
        end loop;
        PlayerShip.Crew := ShipCrew;
        Close(SaveGame);
        return True;
    end LoadGame;
end Game;
