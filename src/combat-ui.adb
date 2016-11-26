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
with ShipModules; use ShipModules;
with Events; use Events;
with Maps; use Maps;
with Bases; use Bases;
with Items; use Items;

package body Combat.UI is

    PilotOrders : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Go closer"), 
        To_Unbounded_String("Keep distance"), To_Unbounded_String("Evade"),
        To_Unbounded_String("Escape"));
    EngineerOrders : constant array (1..4) of Unbounded_String := (To_Unbounded_String("All stop"), 
        To_Unbounded_String("Quarter speed"), To_Unbounded_String("Half speed"),
        To_Unbounded_String("Full speed"));
    GunnerOrders : constant array (1..6) of Unbounded_String := (To_Unbounded_String("Don't shoot"),
        To_Unbounded_String("Precise fire"), To_Unbounded_String("Fire at will"), 
        To_Unbounded_String("Aim for engine"), To_Unbounded_String("Aim in weapon"), 
        To_Unbounded_String("Aim in hull"));
    Order : Crew_Orders;
    CrewMenu : Menu;
    OrdersMenu : Menu;
    MenuWindow : Window;
    MenuWindow2 : Window;
    CurrentMenuIndex : Positive := 1;

    procedure CombatOrders is
        MemberIndex : Natural := 0;
        CrewIndex : constant Integer := Positive'Value(Description(Current(OrdersMenu)));
        OrderIndex : constant Positive := Get_Index(Current(OrdersMenu));
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
        if CrewIndex > 0 then
            GiveOrders(CrewIndex, Order, ModuleIndex);
        elsif CrewIndex = 0 then
            if Name(Current(OrdersMenu)) = "Quit" then
                return;
            end if;
            case Order is
                when Pilot =>
                    PilotOrder := OrderIndex;
                    AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                        " was set on: " & To_String(PilotOrders(PilotOrder)), CombatMessage);
                when Engineer =>
                    EngineerOrder := OrderIndex;
                    AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                        " was set on: " & To_String(EngineerOrders(EngineerOrder)), CombatMessage);
                when Gunner =>
                    Guns.Update_Element(Index => Positive'Value(Description(Current(CrewMenu))), 
                        Process => UpdateGun'Access);
                    AddMessage("Order for " & To_String(PlayerShip.Crew.Element(MemberIndex).Name) & 
                        " was set on: " & To_String(GunnerOrders(OrderIndex)), CombatMessage);
                when others =>
                    null;
            end case;
        else
            UpdateModule(PlayerShip, ModuleIndex, "Current_Value", Positive'Image(abs(CrewIndex)));
            AddMessage("You assigned " & To_String(Items_List.Element(PlayerShip.Cargo.Element(abs(CrewIndex)).ProtoIndex).Name)
                & " to " & To_String(PlayerShip.Modules.Element(ModuleIndex).Name) & ".", OrderMessage);
        end if;
    end CombatOrders;

    procedure ShowCombat is
        PilotName, EngineerName, GunnerName : Unbounded_String := To_Unbounded_String("Vacant");
        LoopStart : Integer;
        DamagePercent : Natural;
        CurrentLine : Line_Position := 13;
        Message : Message_Data;
        Crew_Items : Item_Array_Access;
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuOptions : Menu_Option_Set;
        EnemyStatus : Unbounded_String;
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
        Set_Current(CrewMenu, Crew_Items.all(CurrentMenuIndex));
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
        Move_Cursor(Line => 10, Column => 2);
        Add(Str => "Messages");
        Change_Attributes(Line => 10, Column => 2, Count => 1, Color => 1);
        Move_Cursor(Line => 12, Column => 2);
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
            if Enemy.Ship.Modules.Element(1).Durability = 0 then
                Add(Str => "Destroyed");
            else
                EnemyStatus := To_Unbounded_String("Ok");
                for I in Enemy.Ship.Modules.First_Index..Enemy.Ship.Modules.Last_Index loop
                    if Enemy.Ship.Modules.Element(I).Durability < Enemy.Ship.Modules.Element(I).MaxDurability then
                        EnemyStatus := To_Unbounded_String("Damaged");
                        exit;
                    end if;
                end loop;
                Add(Str => To_String(EnemyStatus));
                for I in Enemy.Ship.Modules.First_Index..Enemy.Ship.Modules.Last_Index loop
                    if Enemy.Ship.Modules.Element(I).Durability > 0 then
                        case Modules_List.Element(Enemy.Ship.Modules.Element(I).ProtoIndex).MType is
                            when ARMOR =>
                                Add(Str => " (armored)");
                            when GUN =>
                                Add(Str => " (gun)");
                            when BATTERING_RAM =>
                                Add(Str => " (battering ram)");
                            when others =>
                                null;
                        end case;
                    end if;
                end loop;
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
        LoopStart := 0 - MessagesAmount;
        if LoopStart < -10 then
            LoopStart := -10;
        end if;
        CurrentLine := Lines - 11;
        Move_Cursor(Line => CurrentLine, Column => 2);
        for I in reverse LoopStart..-1 loop
            Message := GetMessage((I + 1));
            if Message.MessageIndex < MessagesStarts then
                exit;
            end if;
            CurrentLine := CurrentLine + 1;
            if Length(Message.Message) > Integer(Columns - 2) then
                CurrentLine := CurrentLine + (Line_Position(Length(Message.Message)) / Line_Position(Columns - 2));
            end if;
            exit when CurrentLine >= Lines;
            Add(Str => To_String(Message.Message));
            Move_Cursor(Line => CurrentLine, Column => 2);
        end loop;
        LastMessage := To_Unbounded_String("");
        Refresh;
        Refresh(MenuWindow);
    end ShowCombat;

    procedure ShowOrdersMenu is
        Orders_Items : Item_Array_Access;
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuOptions : Menu_Option_Set;
        MemberIndex : Natural := 0;
        SkillIndex, SkillValue : Natural := 0;
        MenuIndex, LastIndex : Positive := 1;
        SkillString : Unbounded_String;
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
                    LastIndex := PilotOrders'Length + 1 + PlayerShip.Crew.Last_Index;
                    LastIndex := LastIndex + PlayerShip.Cargo.Last_Index;
                    Orders_Items := new Item_Array(1..LastIndex);
                    for I in PilotOrders'Range loop
                        Orders_Items.all(I) := New_Item(To_String(PilotOrders(I)), "0");
                        MenuIndex := MenuIndex + 1;
                    end loop;
                when Engineer =>
                    LastIndex := EngineerOrders'Length + 1 + PlayerShip.Crew.Last_Index;
                    LastIndex := LastIndex + PlayerShip.Cargo.Last_Index;
                    Orders_Items := new Item_Array(1..LastIndex);
                    for I in EngineerOrders'Range loop
                        Orders_Items.all(I) := New_Item(To_String(EngineerOrders(I)), "0");
                        MenuIndex := MenuIndex + 1;
                    end loop;
                when Gunner =>
                    LastIndex := GunnerOrders'Length + 1 + PlayerShip.Crew.Last_Index;
                    LastIndex := LastIndex + PlayerShip.Cargo.Last_Index;
                    Orders_Items := new Item_Array(1..LastIndex);
                    for I in GunnerOrders'Range loop
                        Orders_Items.all(I) := New_Item(To_String(GunnerOrders(I)), "0");
                        MenuIndex := MenuIndex + 1;
                    end loop;
                when others =>
                    null;
            end case;
        else
            LastIndex := PlayerShip.Crew.Last_Index + 2;
            LastIndex := LastIndex + PlayerShip.Cargo.Last_Index;
            Orders_Items := new Item_Array(1..LastIndex);
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            case Order is
                when Pilot =>
                    if GetSkillLevel(I, 1) > SkillValue then
                        SkillIndex := I;
                        SkillValue := GetSkillLevel(I, 1);
                    end if;
                when Engineer =>
                    if GetSkillLevel(I, 2) > SkillValue then
                        SkillIndex := I;
                        SkillValue := GetSkillLevel(I, 2);
                    end if;
                when Gunner =>
                    if GetSkillLevel(I, 3) > SkillValue then
                        SkillIndex := I;
                        SkillValue := GetSkillLevel(I, 3);
                    end if;
                when others =>
                    null;
            end case;
        end loop;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if I /= MemberIndex then
                SkillString := Null_Unbounded_String;
                case Order is
                    when Pilot =>
                        if GetSkillLevel(I, 1) > 0 then
                            SkillString := To_Unbounded_String(" +");
                        end if;
                    when Engineer =>
                        if GetSkillLevel(I, 2) > 0 then
                            SkillString := To_Unbounded_String(" +");
                        end if;
                    when Gunner =>
                        if GetSkillLevel(I, 3) > 0 then
                            SkillString := To_Unbounded_String(" +");
                        end if;
                    when others =>
                        null;
                end case;
                if I = SkillIndex then
                    SkillString := SkillString & To_Unbounded_String("+");
                end if;
                if PlayerShip.Crew.Element(I).Order /= Rest then
                    SkillString := SkillString & To_Unbounded_String(" -");
                end if;
                Orders_Items.all(MenuIndex) := New_Item("Assign " & To_String(PlayerShip.Crew.Element(I).Name) & 
                To_String(SkillString), Positive'Image(I));
                MenuIndex := MenuIndex + 1;
            end if;
        end loop;
        if Order = Gunner and MemberIndex > 0 then
            for J in Guns.First_Index..Guns.Last_Index loop
                if PlayerShip.Modules.Element(Guns.Element(J)(1)).Owner = MemberIndex then
                    for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType =
                            Items_Types.Element(Modules_List.Element(PlayerShip.Modules.Element(Guns.Element(J)(1)).ProtoIndex).Value) and
                            I /= PlayerShip.Modules.Element(Guns.Element(J)(1)).Current_Value
                        then
                            Orders_Items.all(MenuIndex) := 
                                New_Item("Use " & To_String(Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Name), 
                                Positive'Image((0 - I)));
                            MenuIndex := MenuIndex + 1;
                        end if;
                    end loop;
                    exit;
                end if;
            end loop;
        end if;
        Orders_Items.all(MenuIndex) := New_Item("Quit", "0");
        MenuIndex := MenuIndex + 1;
        for I in MenuIndex..LastIndex loop
            Orders_Items.all(I) := Null_Item;
        end loop;
        OrdersMenu := New_Menu(Orders_Items);
        MenuOptions := Get_Options(OrdersMenu);
        MenuOptions.Show_Descriptions := False;
        Set_Options(OrdersMenu, MenuOptions);
        Set_Mark(OrdersMenu, "");
        Scale(OrdersMenu, MenuHeight, MenuLength);
        MenuWindow2 := Create(MenuHeight + 2, MenuLength + 2, ((Lines / 3) - (MenuHeight / 2)), ((Columns / 2) - (MenuLength / 2)));
        Box(MenuWindow2);
        Set_Window(OrdersMenu, MenuWindow2);
        Set_Sub_Window(OrdersMenu, Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
        Post(OrdersMenu);
        Refresh;
        Refresh(MenuWindow2);
    end ShowOrdersMenu;

    function CombatKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        if not EndCombat then
            case Key is
                when 56 | KEY_UP => -- Select previous crew position
                    Result := Driver(CrewMenu, M_Up_Item);
                    if Result = Request_Denied then
                        Result := Driver(CrewMenu, M_Last_Item);
                    end if;
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    end if;
                    CurrentMenuIndex := Get_Index(Current(CrewMenu));
                    return Combat_State;
                when 50 | KEY_DOWN => -- Select next crew position
                    Result := Driver(CrewMenu, M_Down_Item);
                    if Result = Request_Denied then
                        Result := Driver(CrewMenu, M_First_Item);
                    end if;
                    if Result = Menu_Ok then
                        Refresh(MenuWindow);
                    end if;
                    CurrentMenuIndex := Get_Index(Current(CrewMenu));
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
                    ShowOrdersMenu;
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
                when Character'Pos('m') | Character'Pos('M') => -- Show messages list
                    DrawGame(Messages_View);
                    return Messages_View;
                when others =>
                    return Combat_State;
            end case;
        else
            CurrentMenuIndex := 1;
            PlayerShip.Speed := OldSpeed;
            EnemyName := Null_Unbounded_String;
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
                if Events_List.Element(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex).EType = AttackOnBase then
                    GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 5);
                end if;
                Events_List.Delete(Index => SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex, Count => 1);
                SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex := 0;
            end if;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
        end if;
    end CombatKeys;

    function CombatOrdersKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when 56 | KEY_UP => -- Select previous order
                Result := Driver(OrdersMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(OrdersMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 50 | KEY_DOWN => -- Select next order
                Result := Driver(OrdersMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(OrdersMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 10 => -- Give order
                CombatOrders;
                DrawGame(Combat_State);
                return Combat_State;
            when others =>
                null;
        end case;
        return Combat_Orders;
    end CombatOrdersKeys;

end Combat.UI;
