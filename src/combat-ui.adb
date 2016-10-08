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
with Crew; use Crew;
with Messages; use Messages;
with UserInterface; use UserInterface;

package body Combat.UI is

    PilotOrders : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Go closer"), 
        To_Unbounded_String("Keep distance"), To_Unbounded_String("Evade"),
        To_Unbounded_String("Escape"));
    EngineerOrders : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Full stop"), 
        To_Unbounded_String("Quarter speed"), To_Unbounded_String("Half speed"),
        To_Unbounded_String("Full speed"));
    GunnerOrders : constant array (1..6) of Unbounded_String := (To_Unbounded_String("Don't shoot"),
        To_Unbounded_String("Precise fire"), To_Unbounded_String("Fire at will"), 
        To_Unbounded_String("Aim for engine"), To_Unbounded_String("Aim in weapon"), 
        To_Unbounded_String("Aim in hull"));
    Order : Crew_Orders;
    CrewMenu : Menu;
    MenuWindow : Window;

    function CombatOrders(Key : Key_Code) return GameStates is
        KeyMax : Key_Code;
        MemberIndex : Natural := 0;
        OrderIndex : Positive;
        ModuleIndex : Natural := 0;
        procedure UpdateGun(Gun : in out GunsInfoArray) is
        begin
            Gun(2) := OrderIndex;
        end UpdateGun;
    begin
        if Order = Pilot or Order = Engineer then
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = Order then
                    MemberIndex := I;
                    exit;
                end if;
            end loop;
        else
            ModuleIndex := Guns.Element(Positive'Value(Description(Current(CrewMenu))))(1);
            MemberIndex := PlayerShip.Modules.Element(ModuleIndex).Owner;
        end if;
        if MemberIndex > 0 then
            case Order is
                when Pilot =>
                    KeyMax := Key_Code(PlayerShip.Crew.Length) + PilotOrders'Length + 96;
                when Engineer =>
                    KeyMax := Key_Code(PlayerShip.Crew.Length) + EngineerOrders'Length + 96;
                when Gunner =>
                    KeyMax := Key_Code(PlayerShip.Crew.Length) + GunnerOrders'Length + 96;
                when others =>
                    null;
            end case;
        else
            KeyMax := Key_Code(PlayerShip.Crew.Length) + 96;
        end if;
        if Key < 97 or Key > KeyMax then -- check if key is valid
            return Combat_Orders;
        end if;
        OrderIndex := Positive(Key - 96);
        if MemberIndex = 0 then -- assign someone to position
            GiveOrders(OrderIndex, Order, ModuleIndex);
        else
            case Order is
                when Pilot =>
                    if OrderIndex <= PilotOrders'Length then
                        PilotOrder := OrderIndex;
                        AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                            " was set on: " & To_String(PilotOrders(PilotOrder)), CombatMessage);
                    else
                        if (OrderIndex - PilotOrders'Length) < MemberIndex then
                            GiveOrders((OrderIndex - PilotOrders'Length), Order);
                        else
                            if (OrderIndex - PilotOrders'Length + 1) <= PlayerShip.Crew.Last_Index then
                                GiveOrders((OrderIndex - PilotOrders'Length + 1), Order);
                            else
                                return Combat_Orders;
                            end if;
                        end if;
                    end if;
                when Engineer =>
                    if OrderIndex <= EngineerOrders'Length then
                        EngineerOrder := OrderIndex;
                        AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                            " was set on: " & To_String(EngineerOrders(EngineerOrder)),
                            CombatMessage);
                    else
                        if (OrderIndex - EngineerOrders'Length) < MemberIndex then
                            GiveOrders((OrderIndex - EngineerOrders'Length), Order);
                        else
                            if (OrderIndex - EngineerOrders'Length + 1) <= PlayerShip.Crew.Last_Index then
                                GiveOrders((OrderIndex - EngineerOrders'Length + 1), Order);
                            else
                                return Combat_Orders;
                            end if;
                        end if;
                    end if;
                when Gunner =>
                    if OrderIndex <= GunnerOrders'Length then
                        Guns.Update_Element(Index => Positive'Value(Description(Current(CrewMenu))), 
                            Process => UpdateGun'Access);
                        AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                            " was set on: " & To_String(GunnerOrders(OrderIndex)), CombatMessage);
                    else
                        if (OrderIndex - GunnerOrders'Length) < MemberIndex then
                            GiveOrders((OrderIndex - GunnerOrders'Length), Order, ModuleIndex);
                        else
                            if (OrderIndex - GunnerOrders'Length + 1) <= PlayerShip.Crew.Last_Index then
                                GiveOrders((OrderIndex - GunnerOrders'Length + 1), Order, ModuleIndex);
                            else
                                return Combat_Orders;
                            end if;
                        end if;
                    end if;
                when others =>
                    null;
            end case;
        end if;
        DrawGame(Combat_State);
        return Combat_State;
    end CombatOrders;

    procedure ShowCombat is
        PilotName, EngineerName, GunnerName : Unbounded_String := To_Unbounded_String("Vacant");
        LoopStart : Integer;
        DamagePercent : Natural;
        CurrentLine : Line_Position := 12;
        Message : Unbounded_String;
        Crew_Items : Item_Array_Access;
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuOptions : Menu_Option_Set;
    begin
        Crew_Items := new Item_Array(1..Natural(Guns.Length) + 3);
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            case PlayerShip.Crew.Element(I).Order is
                when Pilot =>
                    PilotName := PlayerShip.Crew.Element(I).Name;
                when Engineer =>
                    EngineerName := PlayerShip.Crew.Element(I).Name;
                when others =>
                    null;
            end case;
        end loop;
        if PilotName /= To_Unbounded_String("Vacant") then
            PilotName := To_Unbounded_String("Pilot: ") & PilotName & To_Unbounded_String(" -> ") & 
                PilotOrders(PilotOrder);
        else
            PilotName := To_Unbounded_String("Pilot: ") & PilotName;
        end if;
        Crew_Items.all(1) := New_Item(To_String(PilotName));
        if EngineerName /= To_Unbounded_String("Vacant") then
            EngineerName := To_Unbounded_String("Engineer: ") & EngineerName & To_Unbounded_String(" -> ") & 
                EngineerOrders(EngineerOrder);
        else
            EngineerName := To_Unbounded_String("Engineer: ") & EngineerName;
        end if;
        Crew_Items.all(2) := New_Item(To_String(EngineerName), "0");
        for I in Guns.First_Index..Guns.Last_Index loop
            GunnerName := PlayerShip.Modules.Element(Guns.Element(I)(1)).Name & ": ";
            if PlayerShip.Modules.Element(Guns.Element(I)(1)).Owner = 0 then
                GunnerName := GunnerName & To_Unbounded_String("Vacant");
            else
                GunnerName := GunnerName & PlayerShip.Crew.Element(PlayerShip.Modules.Element(Guns.Element(I)(1)).Owner).Name & 
                    " -> " & GunnerOrders(Guns.Element(I)(2));
            end if;
            Crew_Items.all(I + 2) := New_Item(To_String(GunnerName), Positive'Image(I));
        end loop;
        Crew_Items.all(Crew_Items'Last) := Null_Item;
        CrewMenu := New_Menu(Crew_Items);
        Set_Format(CrewMenu, 4, 1);
        MenuOptions := Get_Options(CrewMenu);
        MenuOptions.Show_Descriptions := False;
        Set_Options(CrewMenu, MenuOptions);
        Set_Mark(CrewMenu, "");
        Scale(CrewMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 1, 2);
        Set_Window(CrewMenu, MenuWindow);
        Set_Sub_Window(CrewMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(CrewMenu);
        Move_Cursor(Line => 5, Column => 2);
        Add(Str => "ENTER to give orders");
        Change_Attributes(Line => 5, Column => 2, Count => 5, Color => 1);
        Move_Cursor(Line => 7, Column => 2);
        Add(Str => "Crew Info");
        Change_Attributes(Line => 7, Column => 2, Count => 1, Color => 1);
        Move_Cursor(Line => 8, Column => 2);
        Add(Str => "Ship cargo");
        Change_Attributes(Line => 8, Column => 8, Count => 1, Color => 1);
        Move_Cursor(Line => 9, Column => 2);
        Add(Str => "Ship modules");
        Change_Attributes(Line => 9, Column => 2, Count => 1, Color => 1);
        Move_Cursor(Line => 11, Column => 2);
        Add(Str => "Damage:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            DamagePercent := 100 - Natural((Float(PlayerShip.Modules.Element(I).Durability) /
                Float(PlayerShip.Modules.Element(I).MaxDurability)) * 100.0);
            if DamagePercent > 0 then
                Move_Cursor(Line => CurrentLine, Column => 2);
                Add(Str => To_String(PlayerShip.Modules.Element(I).Name));
                if DamagePercent > 19 and DamagePercent < 50 then
                    Change_Attributes(Line => CurrentLine, Column => 2,
                        Count => Length(PlayerShip.Modules.Element(I).Name), 
                        Color => 2);
                elsif DamagePercent > 49 and DamagePercent < 80 then
                    Change_Attributes(Line => CurrentLine, Column => 2,
                        Count => Length(PlayerShip.Modules.Element(I).Name), 
                        Color => 1);
                elsif DamagePercent > 79 and DamagePercent < 100 then
                    Change_Attributes(Line => CurrentLine, Column => 2,
                        Count => Length(PlayerShip.Modules.Element(I).Name), 
                        Color => 3);
                elsif DamagePercent = 100 then
                    Change_Attributes(Line => CurrentLine, Column => 2,
                        Count => Length(PlayerShip.Modules.Element(I).Name), 
                        Color => 4);
                end if;
                CurrentLine := CurrentLine + 1;
            end if;
        end loop;
        Move_Cursor(Line => 5, Column => (Columns / 2));
        Add(Str => "Enemy status:");
        Move_Cursor(Line => 7, Column => (Columns / 2));
        Add(Str => "Enemy: " & To_String(EnemyName));
        Move_Cursor(Line => 8, Column => (Columns / 2));
        Add(Str => "Distance: ");
        if Enemy.Distance >= 15000 then
            Add(Str => "Escaped");
        elsif Enemy.Distance < 15000 and Enemy.Distance >= 10000 then
            Add(Str => "Long");
        elsif Enemy.Distance < 10000 and Enemy.Distance >= 5000 then
            Add(Str => "Medium");
        elsif Enemy.Distance < 5000 and Enemy.Distance >= 1000 then
            Add(Str => "Short");
        else
            Add(Str => "Close");
        end if;
        Move_Cursor(Line => 9, Column => (Columns / 2));
        Add(Str => "Status: ");
        if Enemy.Distance < 15000 then
            if Enemy.Ship.Modules.Element(1).Durability = Enemy.Ship.Modules.Element(1).MaxDurability then
                Add(Str => "Ok");
            elsif Enemy.Ship.Modules.Element(1).Durability > 0 then
                Add(Str => "Damaged");
            else
                Add(Str => "Destroyed");
            end if;
        else
            Add(Str => "Unknown");
        end if;
        Move_Cursor(Line => 10, Column => (Columns / 2));
        Add(Str => "Speed: ");
        if Enemy.Distance < 15000 then
            case Enemy.Ship.Speed is
                when FULL_STOP =>
                    Add(Str => "Stopped");
                when QUARTER_SPEED =>
                    Add(Str => "Slow");
                when HALF_SPEED =>
                    Add(Str => "Medium");
                when FULL_SPEED =>
                    Add(Str => "Fast");
                when others =>
                    null;
            end case;
        else
            Add(Str => "Unknown");
        end if;
        Move_Cursor(Line => 13, Column => (Columns / 2));
        if not EndCombat then
            Add(Str => "SPACE for next turn");
            Change_Attributes(Line => 13, Column => (Columns / 2),
                Count => 5, Color => 1);
        else
            Add(Str => "Hit any key for back to sky map");
            Change_Attributes(Line => 13, Column => (Columns / 2),
                Count => 3, Color => 1);
        end if;
        if MessagesStarts < MessagesAmount then
            LoopStart := MessagesStarts - MessagesAmount;
            if LoopStart < -10 then
                LoopStart := -10;
            end if;
            CurrentLine := Lines - 11;
            Move_Cursor(Line => CurrentLine, Column => 2);
            for I in LoopStart..-1 loop
                Message := To_Unbounded_String(GetMessage((I + 1), Default));
                CurrentLine := CurrentLine + 1;
                if Length(Message) > Integer(Columns - 2) then
                    CurrentLine := CurrentLine + (Line_Position(Length(Message)) / Line_Position(Columns - 2));
                end if;
                exit when CurrentLine >= Lines;
                Add(Str => To_String(Message));
                Move_Cursor(Line => CurrentLine, Column => 2);
            end loop;
        end if;
        LastMessage := To_Unbounded_String("");
        Refresh;
        Refresh(MenuWindow);
    end ShowCombat;

    procedure ShowOrdersMenu is
        OrdersWindow : Window;
        Line : Line_Position := 0;
        MemberIndex : Natural := 0;
        Height : Line_Position := 1;
        SkillIndex, SkillValue : Natural := 0;
    begin   
        if Order = Pilot or Order = Engineer then
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                if PlayerShip.Crew.Element(I).Order = Order then
                    MemberIndex := I;
                    exit;
                end if;
            end loop;
        else
            MemberIndex := PlayerShip.Modules.Element(Guns.Element(Positive'Value(Description(Current(CrewMenu))))(1)).Owner;
        end if;
        if MemberIndex > 0 then
            case Order is
                when Pilot =>
                    Height := PilotOrders'Length;
                when Engineer =>
                    Height := EngineerOrders'Length;
                when Gunner =>
                    Height := GunnerOrders'Length;
                when others =>
                    null;
            end case;
        end if;
        Height := Height + 3 + Line_Position(PlayerShip.Crew.Last_Index);
        OrdersWindow := Create(Height, 26, (Lines / 2) - 5, (Columns / 2) - 13);
        Box(OrdersWindow);
        if MemberIndex > 0 then
            case Order is
                when Pilot =>
                    for I in PilotOrders'Range loop
                        Move_Cursor(Win => OrdersWindow, Line => Line_Position(I), Column => 1);
                        Add(Win => OrdersWindow, Str => Character'Val(96 + I) &
                            " " & To_String(PilotOrders(I)));
                        Change_Attributes(Win => OrdersWindow, Line => Line_Position(I), Column => 1, 
                            Count => 1, Color => 1);
                    end loop;
                    Line := PilotOrders'Length;
                when Engineer =>
                    for I in EngineerOrders'Range loop
                        Move_Cursor(Win => OrdersWindow, Line => Line_Position(I), Column => 1);
                        Add(Win => OrdersWindow, Str => Character'Val(96 + I) &
                            " " & To_String(EngineerOrders(I)));
                        Change_Attributes(Win => OrdersWindow, Line => Line_Position(I), Column => 1, 
                            Count => 1, Color => 1);
                    end loop;
                    Line := EngineerOrders'Length;
                when Gunner =>
                    for I in GunnerOrders'Range loop
                        Move_Cursor(Win => OrdersWindow, Line => Line_Position(I), Column => 1);
                        Add(Win => OrdersWindow, Str => Character'Val(96 + I) &
                            " " & To_String(GunnerOrders(I)));
                        Change_Attributes(Win => OrdersWindow, Line => Line_Position(I), Column => 1, 
                            Count => 1, Color => 1);
                    end loop;
                    Line := GunnerOrders'Length;
                when others =>
                    null;
            end case;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            case Order is
                when Pilot =>
                    if PlayerShip.Crew.Element(I).Skills(1, 1) > SkillValue then
                        SkillIndex := I;
                        SkillValue := PlayerShip.Crew.Element(I).Skills(1, 1);
                    end if;
                when Engineer =>
                    if PlayerShip.Crew.Element(I).Skills(2, 1) > SkillValue then
                        SkillIndex := I;
                        SkillValue := PlayerShip.Crew.Element(I).Skills(2, 1);
                    end if;
                when Gunner =>
                    if PlayerShip.Crew.Element(I).Skills(3, 1) > SkillValue then
                        SkillIndex := I;
                        SkillValue := PlayerShip.Crew.Element(I).Skills(3, 1);
                    end if;
                when others =>
                    null;
            end case;
        end loop;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if I /= MemberIndex then
                Line := Line + 1;
                Move_Cursor(Win => OrdersWindow, Line => Line, Column => 1);
                Add(Win => OrdersWindow, Str => Character'Val(96 + Integer(Line)) & 
                    " assign " & To_String(PlayerShip.Crew.Element(I).Name));
                case Order is
                    when Pilot =>
                        if PlayerShip.Crew.Element(I).Skills(1, 1) > 0 then
                            Add(Win => OrdersWindow, Str => " +");
                        end if;
                    when Engineer =>
                        if PlayerShip.Crew.Element(I).Skills(2, 1) > 0 then
                            Add(Win => OrdersWindow, Str => " +");
                        end if;
                    when Gunner =>
                        if PlayerShip.Crew.Element(I).Skills(3, 1) > 0 then
                            Add(Win => OrdersWindow, Str => " +");
                        end if;
                    when others =>
                        null;
                end case;
                if I = SkillIndex then
                    Add(Win => OrdersWindow, Str => "+");
                end if;
                if PlayerShip.Crew.Element(I).Order /= Rest then
                    Add(Win => OrdersWindow, Str => " -");
                end if;
                Change_Attributes(Win => OrdersWindow, Line => Line, Column => 1, 
                    Count => 1, Color => 1);
            end if;
        end loop;
        Move_Cursor(Win => OrdersWindow, Line => (Height - 2), Column => 1);
        Add(Win => OrdersWindow, Str => "Quit");
        Change_Attributes(Win => OrdersWindow, Line => (Height - 2), Column => 1, 
            Count => 1, Color => 1);
        Refresh(OrdersWindow);
    end ShowOrdersMenu;

    function CombatKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        if not EndCombat then
            case Key is
                when 56 | KEY_UP => -- Select previous item to trade
                    Result := Driver(CrewMenu, M_Up_Item);
                    if Result = Request_Denied then
                        Result := Driver(CrewMenu, M_Last_Item);
                    end if;
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    end if;
                    return Combat_State;
                when 50 | KEY_DOWN => -- Select next item to trade
                    Result := Driver(CrewMenu, M_Down_Item);
                    if Result = Request_Denied then
                        Result := Driver(CrewMenu, M_First_Item);
                    end if;
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    end if;
                    return Combat_State;
                when 10 => -- Give orders to selected position
                    case Get_Index(Current(CrewMenu)) is
                        when 1 =>
                            Order := Pilot;
                        when 2 =>
                            Order := Engineer;
                        when others =>
                            Order := Gunner;
                    end case;
                    Refresh_Without_Update;
                    ShowOrdersMenu;
                    Update_Screen;
                    return Combat_Orders;
                when Character'Pos(' ') => -- Next combat turn or back to sky map if end combat
                    CombatTurn;
                    DrawGame(Combat_State);
                    return Combat_State;
                when Character'Pos('a') | Character'Pos('A') => -- Show ship cargo
                    DrawGame(Cargo_Info);
                    return Cargo_Info;
                when Character'Pos('s') | Character'Pos('S') => -- Show ship info
                    DrawGame(Ship_Info);
                    return Ship_Info;
                when Character'Pos('c') | Character'Pos('C') => -- Show crew info
                    DrawGame(Crew_Info);
                    return Crew_Info;
                when others =>
                    return Combat_State;
            end case;
        else
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
        end if;
    end CombatKeys;

    function CombatOrdersKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Dont give any new order
                DrawGame(Combat_State);
                return Combat_State;
            when others =>
                return CombatOrders(Key);
        end case;
    end CombatOrdersKeys;

end Combat.UI;
