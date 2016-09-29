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

with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;
with Ships; use Ships;

package body Crew.UI is

    CrewMenu : Menu;
    MenuWindow : Window;

    procedure ShowMemberInfo is
        InfoWindow : Window;
        Member : constant Member_Data := PlayerShip.Crew.Element(Get_Index(Current(CrewMenu)));
        CurrentLine : Line_Position := 1;
        Health, Tired, Hungry, Thirsty, SkillLevel, OrderName : Unbounded_String := Null_Unbounded_String;
    begin
        if Member.Health < 100 and Member.Health > 80 then
            Health := To_Unbounded_String("Slightly wounded");
        elsif Member.Health < 81 and Member.Health > 50 then
            Health := To_Unbounded_String("Wounded");
        elsif Member.Health < 51 then
            Health := To_Unbounded_String("Heavily Wounded");
        end if;
        if Member.Tired > 20 and Member.Tired < 41 then
            Tired := To_Unbounded_String("Bit tired");
        elsif Member.Tired > 40 and Member.Tired < 81 then
            Tired := To_Unbounded_String("Tired");
        elsif Member.Tired > 80 and Member.Tired < 100 then
            Tired := To_Unbounded_String("Very tired");
        elsif Member.Tired = 100 then
            Tired := To_Unbounded_String("Unconscious");
        end if;
        if Member.Thirst > 20 and Member.Thirst < 41 then
            Thirsty := To_Unbounded_String("Bit thirsty");
        elsif Member.Thirst > 40 and Member.Thirst < 81 then
            Thirsty := To_Unbounded_String("Thirsty");
        elsif Member.Thirst > 80 and Member.Thirst < 100 then
            Thirsty := To_Unbounded_String("Very thirsty");
        elsif Member.Thirst = 100 then
            Thirsty := To_Unbounded_String("Dehydrated");
        end if;
        if Member.Hunger > 20 and Member.Hunger < 41 then
            Hungry := To_Unbounded_String("Bit hungry");
        elsif Member.Hunger > 40 and Member.Hunger < 81 then
            Hungry := To_Unbounded_String("Hungry");
        elsif Member.Hunger > 80 and Member.Hunger < 100 then
            Hungry := To_Unbounded_String("Very hungry");
        elsif Member.Hunger = 100 then
            Hungry := To_Unbounded_String("Starving");
        end if;
        InfoWindow := Create((Lines - 5), (Columns / 2), 3, (Columns / 2));
        if Member.Gender = 'M' then
            Add(Win => InfoWindow, Str => "Male");
        else
            Add(Win => InfoWindow, Str => "Female");
        end if;
        if Health /= Null_Unbounded_String then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => To_String(Health));
            CurrentLine := CurrentLine + 1;
        end if;
        if Tired /= Null_Unbounded_String then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => To_String(Tired));
            CurrentLine := CurrentLine + 1;
        end if;
        if Thirsty /= Null_Unbounded_String then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => To_String(Thirsty));
            CurrentLine := CurrentLine + 1;
        end if;
        if Hungry /= Null_Unbounded_String then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => To_String(Hungry));
            CurrentLine := CurrentLine + 1;
        end if;
        CurrentLine := CurrentLine + 1;
        for I in Member.Skills'Range loop
            SkillLevel := To_Unbounded_String("");
            if Member.Skills(I, 1) > 0 and Member.Skills(I, 1) < 20 then
                SkillLevel := To_Unbounded_String("Novice");
            elsif Member.Skills(I, 1) > 21 and Member.Skills(I, 1) < 40 then
                SkillLevel := To_Unbounded_String("Beginner");
            elsif Member.Skills(I, 1) > 41 and Member.Skills(I, 1) < 60 then
                SkillLevel := To_Unbounded_String("Competent");
            elsif Member.Skills(I, 1) > 61 and Member.Skills(I, 1) < 80 then
                SkillLevel := To_Unbounded_String("Expert");
            elsif Member.Skills(I, 1) > 81 and Member.Skills(I, 1) < 100 then
                SkillLevel := To_Unbounded_String("Master");
            elsif Member.Skills(I, 1) > 99 then
                SkillLevel := To_Unbounded_String("Grandmaster");
            end if;
            if SkillLevel /= "" then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
                Add(Win => InfoWindow, Str => To_String(Skills_Names(I)) & ": " & To_String(SkillLevel));
                CurrentLine := CurrentLine + 1;
            end if;
        end loop;
        CurrentLine := CurrentLine + 1;
        case Member.Order is
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
            when Upgrading =>
                OrderName := To_Unbounded_String("Upgrading module");
        end case;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Order: " & To_String(OrderName));
        if Member.Tired < 100 and Member.Hunger < 100 and Member.Thirst < 100 then
            Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 0, Count => 1, Color => 1);
        end if;
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowMemberInfo;

    procedure ShowCrewInfo is
        Crew_Items: constant Item_Array_Access := new Item_Array(1..(PlayerShip.Crew.Last_Index + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
    begin
        Move_Cursor(Line => 3, Column => 2);
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            Crew_Items.all(I) := New_Item(To_String(PlayerShip.Crew.Element(I).Name));
        end loop;
        Crew_Items.all(Crew_Items'Last) := Null_Item;
        CrewMenu := New_Menu(Crew_Items);
        Set_Format(CrewMenu, Lines - 10, 1);
        Set_Mark(CrewMenu, "");
        Scale(CrewMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(CrewMenu, MenuWindow);
        Set_Sub_Window(CrewMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(CrewMenu);
        ShowMemberInfo;
        Refresh(MenuWindow);
    end ShowCrewInfo;

    procedure ShowOrdersMenu is
        OrdersWindow : Window;
        OrdersNames : constant array (1..7) of Unbounded_String := (To_Unbounded_String("Piloting"), 
            To_Unbounded_String("Engineering"), To_Unbounded_String("Gunner"),
            To_Unbounded_String("On break"), To_Unbounded_String("Repair ship"), 
            To_Unbounded_String("Manufacturing"), To_Unbounded_String("Upgrade module"));
    begin
        OrdersWindow := Create(10, 17, (Lines / 3), (Columns / 2) - 8);
        Box(OrdersWindow);
        for I in OrdersNames'Range loop
            Move_Cursor(OrdersWindow, Line => Line_Position(I), Column => 2);
            Add(OrdersWindow, Str => To_String(OrdersNames(I)));
            Change_Attributes(OrdersWindow, Line => Line_Position(I), Column => 2, Count => 1, Color => 1);
        end loop;
        Move_Cursor(OrdersWindow, Line => 8, Column => 2);
        Add(OrdersWindow, Str => "Quit");
        Change_Attributes(OrdersWindow, Line => 8, Column => 2, Count => 1, Color => 1);
        Refresh(OrdersWindow);
    end ShowOrdersMenu;

    function CrewInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Driver_Result;
        MemberIndex : constant Positive := Get_Index(Current(CrewMenu));
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map or combat screen
                DrawGame(OldState);
                return OldState;
            when Character'Pos('o') | Character'Pos('O') => -- Give orders to selected crew member
                if PlayerShip.Crew.Element(MemberIndex).Tired < 100 and PlayerShip.Crew.Element(MemberIndex).Hunger < 100 and 
                    PlayerShip.Crew.Element(MemberIndex).Thirst < 100 then
                    ShowOrdersMenu;
                    Update_Screen;
                    return Giving_Orders;
                end if;
            when 56 | KEY_UP => -- Select previous crew member
                Result := Driver(CrewMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(CrewMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowMemberInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next crew member
                Result := Driver(CrewMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(CrewMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowMemberInfo;
                    Refresh(MenuWindow);
                end if;
            when others =>
                null;
        end case;
        return Crew_Info;
    end CrewInfoKeys;

    function CrewOrdersKeys(Key : Key_Code) return GameStates is
        MemberIndex : constant Positive := Get_Index(Current(CrewMenu));
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to crew info
                null;
            when Character'Pos('p') | Character'Pos('P') => -- Give order piloting
                GiveOrders(MemberIndex, Pilot);
            when Character'Pos('e') | Character'Pos('E') => -- Give order engineering
                GiveOrders(MemberIndex, Engineer);
            when Character'Pos('g') | Character'Pos('G') => -- Give order gunnery
                GiveOrders(MemberIndex, Gunner);
            when Character'Pos('o') | Character'Pos('O') => -- Give order rest
                GiveOrders(MemberIndex, Rest);
            when Character'Pos('r') | Character'Pos('R') => -- Give order repair
                GiveOrders(MemberIndex, Repair);
            when Character'Pos('m') | Character'Pos('M') => -- Give order manufacturing
                GiveOrders(MemberIndex, Craft);
            when Character'Pos('u') | Character'Pos('U') => -- Give order upgrading
                GiveOrders(MemberIndex, Upgrading);
            when others =>
                return Giving_Orders;
        end case;
        DrawGame(Crew_Info);
        return Crew_Info;
    end CrewOrdersKeys;

end Crew.UI;
