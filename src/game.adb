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
with Ada.Text_IO; use Ada.Text_IO;
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;

package body Game is

    procedure NewGame is
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
        Goods : Goods_Array;
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
            case BaseType is
                when Bases_Types'Pos(Industrial) =>
                    Goods := ((2, 2, False), (3, 2, False), (4, 3, True));
                when Bases_Types'Pos(Agricultural) =>
                    Goods := ((2, 1, True), (3, 1, True), (4, 5, False));
                when Bases_Types'Pos(Refinery) =>
                    Goods := ((2, 2, False), (3, 2, False), (4, 5, False));
            end case;
            SkyMap(Integer(PosX), Integer(PosY)) := (BaseIndex => Integer(I));
            SkyBases(Integer(I)) := (Name => To_Unbounded_String("Base" & Rand_Range'Image(I)),
                Visited => False, SkyX => Integer(PosX), SkyY => Integer(PosY),
                BaseType => Bases_Types'Val(BaseType), Goods => Goods);
        end loop;
        -- Place player ship in random base
        RandomBase := Rand_Int.Random(Generator);
        -- Create player ship with modules
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Hawk hull"), 
            MType => HULL, Weight => 8000, Current_Value => 10,
            Max_Value => 10, Durability => 500, MaxDurability => 500));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Small engine"), 
            MType => ENGINE, Weight => 1000, Current_Value => 0,
            Max_Value => 2000, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Your Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Pilot Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Engineer Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Gunner Cabin"), 
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
            MTYPE => GUN, Weight => 30, Current_Value => 1, Max_Value =>
            10, Durability => 20, MaxDurability => 20));
        -- Add cargo to ship
        ShipCargo.Append(New_Item => (ProtoIndex => 1, Amount => 2000));
        ShipCargo.Append(New_Item => (ProtoIndex => 2, Amount => 100));
        ShipCargo.Append(New_Item => (ProtoIndex => 3, Amount => 200));
        ShipCargo.Append(New_Item => (ProtoIndex => 4, Amount => 500));
        -- Add crew to ship
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("You"),
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (0, 0), (1,0)), 
            Hunger => 0, Thirst => 0, Order => Duty)); 
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("Pilot"),
            Health => 100, Tired => 0, Skills => ((1, 0), (0, 0), (0, 0), (0,0)), 
            Hunger => 0, Thirst => 0, Order => Pilot)); 
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("Engineer"),
            Health => 100, Tired => 0, Skills => ((0, 0), (1, 0), (0, 0), (0,0)), 
            Hunger => 0, Thirst => 0, Order => Engineer)); 
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("Gunner"),
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (1, 0), (0, 0)),
            Hunger => 0, Thirst => 0, Order => Gunner)); 
        PlayerShip := (SkyX => SkyBases(Integer(RandomBase)).SkyX, SkyY =>
            SkyBases(Integer(RandomBase)).SkyY, Speed => DOCKED, Modules =>
            ShipModules, Cargo => ShipCargo, Crew => ShipCrew);
        SkyBases(Integer(RandomBase)).Visited := True;
    end NewGame;

    procedure UpdateGame(Minutes : Positive) is
        AddedHours, AddedMinutes : Natural;
    begin
        -- Update time
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
    end UpdateGame;

    procedure SaveGame is
        SaveGame : File_Type;
        RawValue : Unbounded_String;
    begin
        Create(SaveGame, Out_File, "data/savegame.dat");
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
            for J in Goods_Array'Range loop
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Goods(J).ProtoIndex));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).Goods(J).Price));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                if SkyBases(I).Goods(J).Buyable then
                    Put(SaveGame, "1;");
                else
                    Put(SaveGame, "0;");
                end if;
            end loop;
        end loop;
        -- Save player ship
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

    procedure LoadGame is
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
        -- Load game date
        GameDate.Year := Natural'Value(To_String(ReadData));
        GameDate.Month := Natural'Value(To_String(ReadData));
        GameDate.Day := Natural'Value(To_String(ReadData));
        GameDate.Hour := Natural'Value(To_String(ReadData));
        GameDate.Minutes := Natural'Value(To_String(ReadData));
        -- Load sky bases
        for I in SkyBases'Range loop
            SkyBases(I) := (Name => ReadData, Visited => False, SkyX => 0, SkyY => 0,
                BaseType => Industrial, Goods => ((2, 2, False), (3, 2, False),
                (4, 3, False)));
            if To_String(ReadData) = "1" then
                SkyBases(I).Visited := True;
            end if;
            SkyBases(I).SkyX := Integer'Value(To_String(ReadData));
            SkyBases(I).SkyY := Integer'Value(To_String(ReadData));
            SkyBases(I).BaseType := Bases_Types'Val(Integer'Value(To_String(ReadData)));
            for J in Goods_Array'Range loop
                SkyBases(I).Goods(J).ProtoIndex := Positive'Value(To_String(ReadData));
                SkyBases(I).Goods(J).Price := Positive'Value(To_String(ReadData));
                if To_String(ReadData) = "1" then
                    SkyBases(I).Goods(J).Buyable := True;
                end if;
            end loop;
            SkyMap(SkyBases(I).SkyX, SkyBases(I).SkyY).BaseIndex := I;
        end loop;
        -- Load player ship
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
    end LoadGame;
end Game;
