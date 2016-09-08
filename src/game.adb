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
with Items; use Items;
with Crafts; use Crafts;
with ShipModules; use ShipModules;

package body Game is
    
    SaveVersion : constant String := "0.3";

    procedure NewGame(CharName, ShipName : Unbounded_String; Gender : Character) is
        type Rand_Range is range 1..1024;
        type Bases_Range is range 0..2;
        type Gender_Range is range 1..2;
        package Rand_Int is new Discrete_Random(Rand_Range);
        package Rand_Base is new Discrete_Random(Bases_Range);
        package Rand_Gender is new Discrete_Random(Gender_Range);
        Generator : Rand_Int.Generator;
        Generator2 : Rand_Base.Generator;
        Generator3 : Rand_Gender.Generator;
        PosX, PosY : Rand_Range;
        RandomBase : Rand_Range;
        BaseType : Bases_Range;
        PilotName, EngineerName, GunnerName : Unbounded_String;
        ValidLocation : Boolean;
        TempX, TempY : Integer;
        PilotGender, EngineerGender, GunnerGender : Character;
    begin
        -- Set Game time
        GameDate := (Year => 1600, Month => 3, Day => 1, Hour => 8, Minutes => 0);
        -- Generate world
        Rand_Int.Reset(Generator);
        Rand_Base.Reset(Generator2);
        Rand_Gender.Reset(Generator3);
        SkyMap := (others => (others => (BaseIndex => 0)));
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
            BaseType := Rand_Base.Random(Generator2);
            SkyMap(Integer(PosX), Integer(PosY)) := (BaseIndex => Integer(I));
            SkyBases(Integer(I)) := (Name => GenerateBaseName, Visited => False, 
                SkyX => Integer(PosX), SkyY => Integer(PosY), BaseType => Bases_Types'Val(BaseType));
        end loop;
        -- Place player ship in random base
        RandomBase := Rand_Int.Random(Generator);
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
        UpdateModule(PlayerShip, 1, "Current_Value", "10");
        UpdateModule(PlayerShip, 4, "Name", To_String(CharName) & "'s Cabin");
        UpdateModule(PlayerShip, 4, "Owner", "1");
        UpdateModule(PlayerShip, 5, "Name", To_String(PilotName) & "'s Cabin");
        UpdateModule(PlayerShip, 5, "Owner", "2");
        UpdateModule(PlayerShip, 6, "Name", To_String(EngineerName) & "'s Cabin");
        UpdateModule(PlayerShip, 6, "Owner", "3");
        UpdateModule(PlayerShip, 7, "Name", To_String(GunnerName) & "'s Cabin");
        UpdateModule(PlayerShip, 7, "Owner", "4");
        -- Add cargo to ship
        PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 1, Amount => 2000));
        PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 2, Amount => 100));
        PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 3, Amount => 200));
        PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 4, Amount => 500));
        PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 5, Amount => 100));
        PlayerShip.Cargo.Append(New_Item => (ProtoIndex => 13, Amount => 20));
        -- Add crew to ship
        PlayerShip.Crew.Append(New_Item => (Name => CharName, Gender => Gender,
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (0, 0),
            (5,0), (0, 0), (0, 0), (0, 0)), Hunger => 0, Thirst => 0, Order => Rest,
            PreviousOrder => Rest)); 
        PlayerShip.Crew.Append(New_Item => (Name => PilotName, Gender => PilotGender,
            Health => 100, Tired => 0, Skills => ((5, 0), (0, 0), (0, 0),
            (0,0), (0, 0), (0, 0), (0, 0)), Hunger => 0, Thirst => 0, Order => Pilot,
            PreviousOrder => Rest)); 
        PlayerShip.Crew.Append(New_Item => (Name => EngineerName, Gender => EngineerGender,
            Health => 100, Tired => 0, Skills => ((0, 0), (5, 0), (0, 0),
            (0,0), (0, 0), (0, 0), (0, 0)), Hunger => 0, Thirst => 0, Order => Engineer,
            PreviousOrder => Rest)); 
        PlayerShip.Crew.Append(New_Item => (Name => GunnerName, Gender => GunnerGender,
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (5, 0), (0,
            0), (0, 0), (0, 0), (0, 0)), Hunger => 0, Thirst => 0, Order => Rest,
            PreviousOrder => Rest)); 
        SkyBases(Integer(RandomBase)).Visited := True;
    end NewGame;

    procedure UpdateGame(Minutes : Positive) is
        TiredLevel, HungerLevel, ThirstLevel : Integer := 0;
        AddedHours, AddedMinutes : Natural;
        TiredPoints : Natural := 0;
        HealthLevel : Integer := 100;
        RepairPoints : Natural := 0;
        ProtoIndex, I : Positive;
        CrafterIndex, ModuleIndex, ResultAmount : Natural := 0;
        Amount : Integer;
        Recipe : Craft_Data;
        MaterialIndexes : array(1..10) of Natural := (others => 0);
        RepairMaterial : Natural := 0;
        DeathReason : Unbounded_String;
        procedure UpdateMember(Member : in out Member_Data) is
            BackToWork : Boolean := True;
        begin
            Member.Tired := TiredLevel;
            if TiredLevel = 0 and Member.Order = Rest and Member.PreviousOrder /= Rest then
                if Member.PreviousOrder /= Repair then
                    for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                        if PlayerShip.Crew.Element(I).Order = Member.PreviousOrder then
                            BackToWork := False;
                            exit;
                        end if;
                    end loop;
                end if;
                if BackToWork then
                    Member.Order := Member.PreviousOrder;
                    AddMessage(To_String(Member.Name) & " back to work, fully rested.", OrderMessage);
                end if;
                Member.PreviousOrder := Rest;
            end if;
            if TiredLevel > 80 and Member.Order /= Rest then
                Member.PreviousOrder := Member.Order;
                Member.Order := Rest;
                AddMessage(To_String(Member.Name) & " is too tired to work, going rest.", OrderMessage);
            end if;
            if HungerLevel > 80 then
                if Consume(Food) then
                    HungerLevel := HungerLevel - 80;
                    if HungerLevel < 0 then
                        HungerLevel := 0;
                    end if;
                else
                    AddMessage(To_String(Member.Name) & " is hungry, but can't find anything to eat.", OtherMessage);
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
                    AddMessage(To_String(Member.Name) & " is thirsty, but can't find anything to drink.", OtherMessage);
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
        I := PlayerShip.Crew.First_Index;
        while I <= PlayerShip.Crew.Last_Index loop
            HealthLevel := PlayerShip.Crew.Element(I).Health;
            if PlayerShip.Crew.Element(I).Order = Rest then
                TiredLevel := 0;
                if PlayerShip.Crew.Element(I).Tired > 0 then
                    TiredLevel := PlayerShip.Crew.Element(I).Tired - Minutes;
                    if TiredLevel < 0 then
                        TiredLevel := 0;
                    end if;
                end if;
                if HealthLevel > 0 and HealthLevel < 100 then
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
                        if TiredPoints > 0 then
                            RepairPoints := RepairPoints + TiredPoints + (PlayerShip.Crew.Element(I).Skills(2, 1) / 10);
                        end if;
                        GainExp(TiredPoints, 2, I);
                    when Craft =>
                        CrafterIndex := I;
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
                if HealthLevel < 1 then
                    HealthLevel := 0;
                    DeathReason := To_Unbounded_String("starvation");
                end if;
            end if;
            ThirstLevel := PlayerShip.Crew.Element(I).Thirst + TiredPoints;
            if ThirstLevel > 100 then
                ThirstLevel := 100;
            end if;
            if PlayerShip.Crew.Element(I).Thirst = 100 then
                HealthLevel := HealthLevel - TiredPoints;
                if HealthLevel < 1 then
                    HealthLevel := 0;
                    DeathReason := To_Unbounded_String("dehydration");
                end if;
            end if;
            PlayerShip.Crew.Update_Element(Index => I, Process => UpdateMember'Access);
            if HealthLevel = 0 then
                Death(I, DeathReason);
            else
                I := I + 1;
            end if;
        end loop;
        -- Repair ship (if needed)
        if RepairPoints > 0 then
            Repair_Loop:
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                RepairMaterial := 0;
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    Material_Loop:
                    for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(J).ProtoIndex).IType = 
                            Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairMaterial then
                            ProtoIndex := PlayerShip.Cargo.Element(J).ProtoIndex;
                            RepairMaterial := J;
                            -- Limit repair point depends on amount of repair materials
                            if PlayerShip.Cargo.Element(J).Amount < RepairPoints then
                                RepairPoints := PlayerShip.Cargo.Element(J).Amount;
                            end if;
                            exit Material_Loop;
                        end if;
                    end loop Material_Loop;
                    if RepairMaterial = 0 then
                        AddMessage("You don't have repair materials to continue repairs.",
                            OrderMessage);
                        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                            if PlayerShip.Crew.Element(I).Order = Repair then
                                GiveOrders(I, Rest);
                            end if;
                        end loop;
                        exit Repair_Loop;
                    end if;
                    -- Repair module
                    if PlayerShip.Modules.Element(I).Durability + RepairPoints > PlayerShip.Modules.Element(I).MaxDurability then
                        RepairPoints := (PlayerShip.Modules.Element(I).Durability + RepairPoints) - 
                            PlayerShip.Modules.Element(I).MaxDurability;
                        UpdateCargo(ProtoIndex, (PlayerShip.Modules.Element(I).Durability - 
                            PlayerShip.Modules.Element(I).MaxDurability));
                        UpdateModule(PlayerShip, I, "Durability", Integer'Image(PlayerShip.Modules.Element(I).MaxDurability - 
                            PlayerShip.Modules.Element(I).Durability));
                    else
                        UpdateCargo(ProtoIndex, (0 - RepairPoints));
                        UpdateModule(PlayerShip, I, "Durability", Integer'Image(RepairPoints));
                        RepairPoints := 0;
                    end if;
                    exit Repair_Loop when RepairPoints = 0;
                end if;
            end loop Repair_Loop;
            -- Send repair team on break if all is ok
            if RepairPoints > 0 then
                AddMessage("All repairs are finished.", OrderMessage);
                for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(I).Order = Repair then
                        GiveOrders(I, Rest);
                    end if;
                end loop;
            end if;
        end if;
        -- Craft items
        if CrafterIndex > 0 and TiredPoints > 0 then
            Recipe := Recipes_List.Element(PlayerShip.Craft);
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = Recipe.Workplace and
                    PlayerShip.Modules.ELement(I).Durability > 0 then
                    ModuleIndex := I;
                    exit;
                end if;
            end loop;
            if ModuleIndex = 0 then
                AddMessage("You don't have workplace for manufacturing selected " & 
                        To_String(Items_List.Element(Recipe.ResultIndex).Name) & ".", CraftMessage);
                GiveOrders(CrafterIndex, Rest);
                PlayerShip.Craft := 0;
                return;
            end if;
            Craft_Loop:
            for I in 1..TiredPoints loop
                MaterialIndexes := (others => 0);
                for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                    for K in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(J).ProtoIndex).IType = Recipe.MaterialTypes(K) then
                            MaterialIndexes(K) := J;
                        end if;
                    end loop;
                end loop;
                for J in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                    if MaterialIndexes(J) = 0 then
                        AddMessage("You don't have any crafting materials for manufacturing " & 
                        To_String(Items_List.Element(Recipe.ResultIndex).Name) & ".", CraftMessage);
                        GiveOrders(CrafterIndex, Rest);
                        PlayerShip.Craft := 0;
                        exit Craft_Loop;
                    end if;
                end loop;
                Amount := 0;
                for J in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                    Amount := Amount + Items_List.Element(PlayerShip.Cargo.Element(MaterialIndexes(J)).ProtoIndex).Weight * 
                        Recipe.MaterialAmounts.Element(J);
                end loop;
                ResultAmount := Recipe.ResultAmount + Integer(Float'Floor(Float(Recipe.ResultAmount) *
                    (Float(PlayerShip.Crew.Element(CrafterIndex).Skills(Recipe.Skill, 1)) / 100.0)));
                Amount := Amount - (Items_List.Element(Recipe.ResultIndex).Weight * ResultAmount);
                if FreeCargo(Amount) < 0 then
                    AddMessage("You don't have free cargo space for manufacturing " & 
                        To_String(Items_List.Element(Recipe.ResultIndex).Name) & ".", CraftMessage);
                    GiveOrders(CrafterIndex, Rest);
                    PlayerShip.Craft := 0;
                    exit Craft_Loop;
                end if;
                for J in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                    if PlayerShip.Cargo.Element(MaterialIndexes(J)).Amount < Recipe.MaterialAmounts.Element(J) then
                        AddMessage("You don't have enough crafting materials for manufacturing " & 
                        To_String(Items_List.Element(Recipe.ResultIndex).Name) & 
                        ".", CraftMessage);
                        GiveOrders(CrafterIndex, Rest);
                        PlayerShip.Craft := 0;
                        exit Craft_Loop;
                    end if;
                end loop;
                GainExp(1, Recipe.Skill, CrafterIndex);
                Amount := 0;
                for J in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                    UpdateCargo(PlayerShip.Cargo.Element(MaterialIndexes(J)).ProtoIndex, (0 - Recipe.MaterialAmounts.Element(J)));
                end loop;
                UpdateCargo(Recipes_List.Element(PlayerShip.Craft).ResultIndex, ResultAmount);
                AddMessage(To_String(PlayerShip.Crew.Element(CrafterIndex).Name) & " was manufactured" & Integer'Image(ResultAmount) & 
                    " " & To_String(Items_List.Element(Recipe.ResultIndex).Name) & 
                    ".", CraftMessage);
            end loop Craft_Loop;
        end if;
    end UpdateGame;

    procedure SaveGame is
        SaveGame : File_Type;
        RawValue : Unbounded_String;
        Messages : Natural := 10;
        StartLoop : Positive;
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
        RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Craft));
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
            for J in Skills_Array'Range loop
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills(J, 1)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
                RawValue := To_Unbounded_String(Integer'Image(PlayerShip.Crew.Element(I).Skills(J, 2)));
                Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end loop;
        end loop;
        if Messages > MessagesAmount then
            Messages := MessagesAmount;
        end if;
        RawValue := To_Unbounded_String(Messages'Img);
        Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
        if Messages > 0 then
            StartLoop := MessagesAmount - Messages + 1;
            for I in StartLoop..MessagesAmount loop
                RawValue := To_Unbounded_String(Integer'Image(Message_Type'Pos(GetMessageType(I))));
                Put(SaveGame, GetMessage(I) & ";" & To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            end loop;
        end if;
        Close(SaveGame);
    end SaveGame;

    function LoadGame return Boolean is
        SaveGame : File_Type;
        VectorLength : Positive;
        Skills : Skills_Array := (others => (0, 0));
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector; 
        ShipCrew : Crew_Container.Vector;
        Messages : Natural;
        Message : Unbounded_String;
        MType : Message_Type;
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
        if ReadData /= SaveVersion then
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
        PlayerShip.Craft := Integer'Value(To_String(ReadData));
        VectorLength := Positive'Value(To_String(ReadData));
        for I in 1..VectorLength loop
            ShipModules.Append(New_Item => (Name => ReadData, ProtoIndex =>
                Integer'Value(To_String(ReadData)), Weight =>
                Natural'Value(To_String(ReadData)), Current_Value =>
                Integer'Value(To_String(ReadData)), Max_Value =>
                Integer'Value(To_String(ReadData)), Durability =>
                Integer'Value(To_String(ReadData)), MaxDurability =>
                Integer'Value(To_String(ReadData)), Owner =>
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
            ShipCrew.Append(New_Item => (Name => ReadData, Gender => Element(ReadData, 1), 
                Health => Natural'Value(To_String(ReadData)), Tired =>
                Natural'Value(To_String(ReadData)), Skills => Skills, Hunger => 
                Natural'Value(To_String(ReadData)), Thirst =>
                Natural'Value(To_String(ReadData)), Order =>
                Crew_Orders'Val(Integer'Value(To_String(ReadData))), 
                PreviousOrder => Crew_Orders'Val(Integer'Value(To_String(ReadData)))));
            for J in Skills_Array'Range loop
                Skills(J, 1) := Natural'Value(To_String(ReadData));
                Skills(J, 2) := Natural'Value(To_String(ReadData));
            end loop;
            ShipCrew.Update_Element(Index => ShipCrew.Last_Index,
                Process => UpdateMember'Access);
        end loop;
        PlayerShip.Crew := ShipCrew;
        Messages := Integer'Value(To_String(ReadData));
        for I in 1..Messages loop
            Message := ReadData;
            MType := Message_Type'Val(Integer'Value(To_String(ReadData)));
            RestoreMessage(Message, MType);
        end loop;
        Close(SaveGame);
        return True;
    end LoadGame;
end Game;
