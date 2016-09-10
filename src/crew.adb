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
with Ships; use Ships;
with Messages; use Messages;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Crafts; use Crafts;

package body Crew is

    MemberIndex : Natural;

    procedure GiveOrders(MemberIndex : Positive; GivenOrder : Crew_Orders) is
        NewOrder : Crew_Orders;
        MemberName : constant String := To_String(PlayerShip.Crew.Element(MemberIndex).Name);
        HaveMaterial, RepairNeeded : Boolean := False;
        ModuleIndex : Natural := 0;
        MType : ModuleType := ENGINE;
        procedure UpdateOrder(Member : in out Member_Data) is
        begin
            Member.Order := NewOrder;
        end UpdateOrder;
    begin
        if GivenOrder = PlayerShip.Crew.Element(MemberIndex).Order then
            return;
        end if;
        if PlayerShip.Crew.Element(MemberIndex).Tired = 100 then
            ShowDialog(MemberName & " is too tired to work.");
            return;
        end if;
        if PlayerShip.Crew.Element(MemberIndex).Hunger = 100 then
            ShowDialog(MemberName & " is too hungry to work.");
            return;
        end if;
        if PlayerShip.Crew.Element(MemberIndex).Thirst = 100 then
            ShowDialog(MemberName & " is too thirsty to work.");
            return;
        end if;
        if GivenOrder = Repair then
            Repair_Loop:
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    RepairNeeded := True;
                    for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(J).ProtoIndex).IType = 
                            Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairMaterial then
                            HaveMaterial := True;
                            exit Repair_Loop;
                        end if;
                    end loop;
                end if;
            end loop Repair_Loop;
            if not RepairNeeded then
                ShowDialog("Your ship don't need repair.");
                return;
            end if;
            if not HaveMaterial then
                ShowDialog("You don't have repair materials.");
                return;
            end if;
        else
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = GivenOrder and PlayerShip.Crew.Element(I).Order /= Rest then
                    NewOrder := Rest;
                    PlayerShip.Crew.Update_Element(Index => I, Process => UpdateOrder'Access);
                    AddMessage(To_String(PlayerShip.Crew.Element(I).Name) & " going on break.", OrderMessage);
                end if;
            end loop;
        end if;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            case GivenOrder is
                when Pilot =>
                    MType := COCKPIT;
                when Gunner =>
                    MType := TURRET;
                when Craft =>
                    MType := Recipes_List(PlayerShip.Craft).Workplace;
                when Rest =>
                    null;
                when others =>
                    exit;
            end case;
            if MType /= ENGINE then
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = MType and 
                    PlayerShip.Modules.Element(I).Durability > 0 then
                    ModuleIndex := I;
                    exit;
                end if;
            else
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = MType and 
                    PlayerShip.Modules.Element(I).Durability > 0 and
                    PlayerShip.Modules.Element(I).Owner = MemberIndex then
                    ModuleIndex := I;
                    exit;
                end if;
            end if;
        end loop;
        if ModuleIndex = 0 then
            case GivenOrder is
                when Pilot =>
                    ShowDialog(MemberName & " can't starts piloting because cockpit is destroyed.");
                    return;
                when Gunner =>
                    ShowDialog(MemberName & " can't starts operating gun because turret is destroyed.");
                    return;
                when Craft =>
                    ShowDialog(MemberName & " can't starts manufacturing because workshop is destroyed.");
                    return;
                when Rest =>
                    ShowDialog(MemberName & " can't rest because his/her cabin is destroyed.");
                    return;
                when others =>
                    null;
            end case;
        end if;
        case GivenOrder is
            when Pilot =>
                AddMessage(MemberName & " starts piloting.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex, "Owner", Positive'Image(MemberIndex));
            when Engineer =>
                AddMessage(MemberName & " starts engineers duty.", OrderMessage);
            when Gunner =>
                AddMessage(MemberName & " starts operating gun.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex, "Owner", Positive'Image(MemberIndex));
            when Rest =>
                AddMessage(MemberName & " going on break.", OrderMessage);
                for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                    if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType /= CABIN and 
                        PlayerShip.Modules.Element(I).Owner = MemberIndex then
                        UpdateModule(PlayerShip, I, "Owner", "0");
                        exit;
                    end if;
                end loop;
            when Repair =>
                AddMessage(MemberName & " starts repair ship.", OrderMessage);
            when Craft =>
                AddMessage(MemberName & " starts manufacturing.", OrderMessage);
                UpdateModule(PlayerShip, ModuleIndex, "Owner", Positive'Image(MemberIndex));
        end case;
        NewOrder := GivenOrder;
        PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateOrder'Access);
    end GiveOrders;

    function Consume(ItemType : Items_Types) return Boolean is
        ProtoIndex : Natural := 0;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = ItemType then
                ProtoIndex := PlayerShip.Cargo.Element(I).ProtoIndex;
                exit;
            end if;
        end loop;
        if ProtoIndex = 0 then
            return False;
        end if;
        UpdateCargo(ProtoIndex, (0 - 1));
        return True;
    end Consume;

    procedure GainExp(Amount : Natural; SkillNumber, CrewIndex : Positive) is
        SkillExp, SkillLevel : Natural;
        procedure UpdateSkill(Member : in out Member_Data) is
        begin
            Member.Skills(SkillNumber, 1) := SkillLevel;
            Member.Skills(SkillNumber, 2) := SkillExp;
        end UpdateSkill;
    begin
        if PlayerShip.Crew.Element(CrewIndex).Skills(SkillNumber, 1) = 100 then
            return;
        end if;
        SkillLevel := PlayerShip.Crew.Element(CrewIndex).Skills(SkillNumber, 1);
        SkillExp := PlayerShip.Crew.Element(CrewIndex).Skills(SkillNumber, 2) + Amount;
        if SkillExp >= (SkillLevel * 100) then
            SkillExp := SkillExp - (SkillLevel * 100);
            SkillLevel := SkillLevel + 1;
        end if;
        PlayerShip.Crew.Update_Element(Index => CrewIndex, Process => UpdateSkill'Access);
    end GainExp;

    function GenerateMemberName(Gender : Character) return Unbounded_String is -- based on name generator from libtcod
        type Short_Range is range 1..2;
        type Start_Range is range 1..36;
        type Mid_Range is range 1..9;
        StartSyllMale : constant array(Start_Range) of Unbounded_String :=
            (To_Unbounded_String("Aer"), To_Unbounded_String("An"),
            To_Unbounded_String("Ar"), To_Unbounded_String("Ban"),
            To_Unbounded_String("Ber"), To_Unbounded_String("Beth"),
            To_Unbounded_String("Cut"), To_Unbounded_String("Dan"),
            To_Unbounded_String("Dar"), To_Unbounded_String("Dell"),
            To_Unbounded_String("Der"), To_Unbounded_String("Edr"),
            To_Unbounded_String("Er"), To_Unbounded_String("Eth"),
            To_Unbounded_String("Ett"), To_Unbounded_String("Fin"),
            To_Unbounded_String("Ian"), To_Unbounded_String("Iarr"),
            To_Unbounded_String("Ill"), To_Unbounded_String("Jed"),
            To_Unbounded_String("Kan"), To_Unbounded_String("Kar"),
            To_Unbounded_String("Ker"), To_Unbounded_String("Kurr"),
            To_Unbounded_String("Kyr"), To_Unbounded_String("Man"),
            To_Unbounded_String("Mar"), To_Unbounded_String("Mer"),
            To_Unbounded_String("Mir"), To_Unbounded_String("Tsal"),
            To_Unbounded_String("Tser"), To_Unbounded_String("Tsir"),
            To_Unbounded_String("Van"), To_Unbounded_String("Var"),
            To_Unbounded_String("Yur"), To_Unbounded_String("Yyr"));
        MiddleSyllMale : constant array(Mid_Range) of Unbounded_String :=
            (To_Unbounded_String("al"), To_Unbounded_String("an"),
            To_Unbounded_String("ar"), To_Unbounded_String("el"),
            To_Unbounded_String("en"), To_Unbounded_String("ess"),
            To_Unbounded_String("ian"), To_Unbounded_String("onn"),
            To_Unbounded_String("or"));
        EndSyllMale : constant array(Mid_Range) of Unbounded_String :=
            (To_Unbounded_String("ai"), To_Unbounded_String("an"),
            To_Unbounded_String("ar"), To_Unbounded_String("ath"),
            To_Unbounded_String("en"), To_Unbounded_String("eo"),
            To_Unbounded_String("is"), To_Unbounded_String("u"),
            To_Unbounded_String("or"));
        EndSyllFemale : constant array(Mid_Range) of Unbounded_String :=
            (To_Unbounded_String("a"), To_Unbounded_String("ae"),
            To_Unbounded_String("aelle"), To_Unbounded_String("ai"),
            To_Unbounded_String("ea"), To_Unbounded_String("i"),
            To_Unbounded_String("ia"), To_Unbounded_String("u"),
            To_Unbounded_String("wen"));
        package Rand_Start is new Discrete_Random(Start_Range);
        package Rand_Mid is new Discrete_Random(Mid_Range);
        package Rand_Short is new Discrete_Random(Short_Range);
        Generator : Rand_Start.Generator;
        Generator2 : Rand_Mid.Generator;
        Generator3 : Rand_Short.Generator;
        NewName : Unbounded_String;
    begin
        Rand_Start.Reset(Generator);
        Rand_Mid.Reset(Generator2);
        Rand_Short.Reset(Generator3);
        if Rand_Short.Random(Generator3) = 1 then
            if Gender = 'M' then
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & MiddleSyllMale(Rand_Mid.Random(Generator2)) &
                    EndSyllMale(Rand_Mid.Random(Generator2));
            else
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & MiddleSyllMale(Rand_Mid.Random(Generator2)) &
                    EndSyllFemale(Rand_Mid.Random(Generator2));
            end if;
        else
            if Gender = 'M' then
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & EndSyllMale(Rand_Mid.Random(Generator2));
            else
                NewName := StartSyllMale(Rand_Start.Random(Generator)) & EndSyllFemale(Rand_Mid.Random(Generator2));
            end if;
        end if;
        return NewName;
    end GenerateMemberName;

    procedure Death(MemberIndex : Positive; Reason : Unbounded_String) is
        procedure UpdateDeath(Member : in out Member_Data) is
        begin
            Member.Order := Rest;
            Member.Health := 0;
        end UpdateDeath;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Owner = MemberIndex then
                UpdateModule(PlayerShip, I, "Owner", "0");
            end if;
        end loop;
        if MemberIndex > 1 then
            PlayerShip.Crew.Delete(Index => MemberIndex, Count => 1);
            AddMessage(To_String(PlayerShip.Crew.Element(MemberIndex).Name) & " died from " &
                To_String(Reason) & ".", CombatMessage);
        else
            PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateDeath'Access);
            AddMessage("You died from " & To_String(Reason) & ".", CombatMessage);
        end if;
    end Death;

    procedure ShowCrewInfo(Key : Key_Code) is
        Health, Tired, Hungry, Thirsty, SkillLevel, OrderName : Unbounded_String;
        Skills_Names : constant array (Skills_Array'Range) of Unbounded_String := (To_Unbounded_String("Piloting"), 
            To_Unbounded_String("Engineering"), To_Unbounded_String("Gunnery"), 
            To_Unbounded_String("Bartering"), To_Unbounded_String("Alchemy"),
            To_Unbounded_String("Cooking"), To_Unbounded_String("Gunsmith"));
        SkillLine : Line_Position := 4;
    begin
        if Key /= KEY_NONE then
            Erase;
            Refresh;
            ShowGameHeader(Crew_Info);
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            Move_Cursor(Line => Line_Position(2 + I), Column => 2);
            Add(Str => Character'Val(96 + I) & " " & To_String(PlayerShip.Crew.Element(I).Name));
            if PlayerShip.Crew.Element(I).Health = 100 then
                Health := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Health < 100 and PlayerShip.Crew.Element(I).Health > 50 then
                Health := To_Unbounded_String(" [Wounded]");
            elsif PlayerShip.Crew.Element(I).Health < 51 and PlayerShip.Crew.Element(I).Health > 0 then
                Health := To_Unbounded_String(" [Heavily Wounded]");
            else
                Health := To_Unbounded_String(" [Dead]");
            end if;
            if PlayerShip.Crew.Element(I).Tired < 41 then
                Tired := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Tired > 40 and PlayerShip.Crew(I).Tired < 81 then
                Tired := To_Unbounded_String(" [Tired]");
            elsif PlayerShip.Crew.Element(I).Tired > 80 and PlayerShip.Crew(I).Tired < 100 then
                Tired := To_Unbounded_String(" [Very tired]");
            else
                Tired := To_Unbounded_String(" [Unconscious]");
            end if;
            if PlayerShip.Crew.Element(I).Hunger < 41 then
                Hungry := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Hunger > 40 and PlayerShip.Crew(I).Hunger < 81 then
                Hungry := To_Unbounded_String(" [Hungry]");
            elsif PlayerShip.Crew.Element(I).Hunger > 80 and PlayerShip.Crew(I).Hunger < 100 then
                Hungry := To_Unbounded_String(" [Very hungry]");
            else
                Hungry := To_Unbounded_String(" [Starving]");
            end if;
            if PlayerShip.Crew.Element(I).Thirst < 40 then
                Thirsty := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Thirst > 40 and PlayerShip.Crew(I).Thirst < 81 then
                Thirsty := To_Unbounded_String(" [Thirsty]");
            elsif PlayerShip.Crew.Element(I).Thirst > 80 and PlayerShip.Crew(I).Thirst < 100 then
                Thirsty := To_Unbounded_String(" [Very thirsty]");
            else
                Thirsty := To_Unbounded_String(" [Dehydrated]");
            end if;
            Add(Str => To_String(Health) & To_String(Tired) & To_String(Hungry)
                & To_String(Thirsty));
            Change_Attributes(Line => Line_Position(2 + I), Column => 2, Count => 1, Color => 1);
        end loop;
        if Key /= KEY_NONE then -- Show details about selected crew member
            if (Key >= Key_Code(96 + PlayerShip.Crew.First_Index)) and (Key <= Key_Code(96 + PlayerShip.Crew.Last_Index)) then
                MemberIndex := Integer(Key) - 96;
                Move_Cursor(Line => 2, Column => (Columns / 2));
                Add(Str => "Name: " & To_String(PlayerShip.Crew.Element(MemberIndex).Name));
                Move_Cursor(Line => 3, Column => (Columns / 2));
                Add(Str => "Gender: ");
                if PlayerShip.Crew.Element(MemberIndex).Gender = 'M' then
                    Add(Str => "Male");
                else
                    Add(Str => "Female");
                end if;
                for J in PlayerShip.Crew.Element(MemberIndex).Skills'Range loop
                    SkillLevel := To_Unbounded_String("");
                    if PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 0 and 
                        PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 20 then
                        SkillLevel := To_Unbounded_String("Novice");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 21 and 
                        PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 40 then
                        SkillLevel := To_Unbounded_String("Beginner");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 41 and 
                        PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 60 then
                        SkillLevel := To_Unbounded_String("Competent");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 61 and 
                        PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 80 then
                        SkillLevel := To_Unbounded_String("Expert");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 81 and 
                        PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 100 then
                            SkillLevel := To_Unbounded_String("Master");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 99 then
                        SkillLevel := To_Unbounded_String("Grandmaster");
                    end if;
                    if SkillLevel /= "" then
                        Move_Cursor(Line => SkillLine, Column => (Columns / 2));
                        Add(Str => To_String(Skills_Names(J)) & ": " & To_String(SkillLevel));
                        SkillLine := SkillLine + 1;
                    end if;
                end loop;
                SkillLine := SkillLine + 1;
                case PlayerShip.Crew.Element(MemberIndex).Order is
                    when Pilot =>
                        OrderName := To_Unbounded_String("Piloting");
                    when Engineer =>
                        OrderName := To_Unbounded_String("Engineering");
                    when Gunner =>
                        OrderName := To_Unbounded_String("Gunner");
                    when Rest =>
                        OrderName := To_Unbounded_String("On break");
                    when Repair =>
                        OrderName := To_Unbounded_String("Repair ship");
                    when Craft =>
                        OrderName := To_Unbounded_String("Manufacturing");
                end case;
                Move_Cursor(Line => SkillLine, Column => (Columns / 2));
                Add(Str => "Order: " & To_String(OrderName));
                if PlayerShip.Crew.Element(MemberIndex).Health > 0 and PlayerShip.Crew.Element(MemberIndex).Tired < 100 and
                    PlayerShip.Crew.Element(MemberIndex).Hunger < 100 and PlayerShip.Crew.Element(MemberIndex).Thirst < 100 then
                    Change_Attributes(Line => SkillLine, Column => (Columns / 2), Count => 1, Color => 1);
                end if;
            else
                MemberIndex := 0;
            end if;
        end if;
    end ShowCrewInfo;

    procedure ShowOrdersMenu is
        OrdersWindow : Window;
        OrdersNames : constant array (1..6) of Unbounded_String := (To_Unbounded_String("Piloting"), 
            To_Unbounded_String("Engineering"), To_Unbounded_String("Gunner"),
            To_Unbounded_String("On break"), To_Unbounded_String("Repair ship"), 
            To_Unbounded_String("Manufacturing"));
    begin
        OrdersWindow := Create(10, 20, (Lines / 2) - 5, (Columns / 2) - 10);
        Box(OrdersWindow);
        for I in OrdersNames'Range loop
            Move_Cursor(OrdersWindow, Line => Line_Position(I + 1), Column => 5);
            Add(OrdersWindow, Str => To_String(OrdersNames(I)));
            Change_Attributes(OrdersWindow, Line => Line_Position(I + 1), Column => 5, Count => 1, Color => 1);
        end loop;
        Move_Cursor(OrdersWindow, Line => 8, Column => 5);
        Add(OrdersWindow, Str => "Quit");
        Change_Attributes(OrdersWindow, Line => 8, Column => 5, Count => 1, Color => 1);
        Refresh(OrdersWindow);
    end ShowOrdersMenu;

    function CrewInfoKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('o') | Character'Pos('O') => -- Give orders to selected crew member
                if MemberIndex > 0 then
                    if PlayerShip.Crew.Element(MemberIndex).Health > 0 and PlayerShip.Crew.Element(MemberIndex).Tired < 100 and
                        PlayerShip.Crew.Element(MemberIndex).Hunger < 100 and PlayerShip.Crew.Element(MemberIndex).Thirst < 100 then
                        ShowCrewInfo(Key_Code(MemberIndex + 96));
                        Refresh_Without_Update;
                        ShowOrdersMenu;
                        Update_Screen;
                        return Giving_Orders;
                    else
                        ShowCrewInfo(Key);
                        return Crew_Info;
                    end if;
                else
                    ShowCrewInfo(Key);
                    return Crew_Info;
                end if;
            when others =>
                ShowCrewInfo(Key);
                return Crew_Info;
        end case;
    end CrewInfoKeys;

    function CrewOrdersKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to crew info
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('p') | Character'Pos('P') => -- Give order piloting
                GiveOrders(MemberIndex, Pilot);
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('e') | Character'Pos('E') => -- Give order engineering
                GiveOrders(MemberIndex, Engineer);
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('g') | Character'Pos('G') => -- Give order gunnery
                GiveOrders(MemberIndex, Gunner);
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('o') | Character'Pos('O') => -- Give order rest
                GiveOrders(MemberIndex, Rest);
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('r') | Character'Pos('R') => -- Give order repair
                GiveOrders(MemberIndex, Repair);
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('m') | Character'Pos('M') => -- Give order manufacturing
                GiveOrders(MemberIndex, Craft);
                MemberIndex := 0;
                DrawGame(Crew_Info);
                return Crew_Info;
            when others =>
                return Giving_Orders;
        end case;
    end CrewOrdersKeys;

end Crew;
