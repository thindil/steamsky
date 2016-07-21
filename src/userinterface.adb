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
with Ada.Containers.Vectors; use Ada.Containers;
with Terminal_Interface.Curses_Constants; use Terminal_Interface.Curses_Constants;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;
with Bases; use Bases;

package body UserInterface is

    MemberIndex : Natural;
    package Messages_Container is new Vectors(Positive, Unbounded_String);
    Messages_List : Messages_Container.Vector;

    procedure ShowMainMenu is
        Visibility : Cursor_Visibility := Invisible;
    begin
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);

        -- Game logo
        Move_Cursor(Line => Lines / 5, Column => (Columns - 15) / 2);
        Add(Str => "STEAM SKY");
        Move_Cursor(Line => (Lines / 5) + 1, Column => (Columns - 12) / 2);
        -- Game version
        Add(Str => "ver 0.1");
        Move_Cursor(Line => (Lines / 3) + 1, Column => (Columns - 12) / 2);
        -- Game menu
        Add(Str => "New game");
        Init_Pair(1, Color_Yellow, Color_Black);
        Change_Attributes(Line => (Lines / 3) + 1, Column => (Columns - 12) / 2,
            Count => 1, Color => 1);
        Move_Cursor(Line => (Lines / 3) + 2, Column => (Columns - 12) / 2);
        Add(Str => "Quit game");
        Change_Attributes(Line => (Lines / 3) + 2, Column => (Columns - 12) / 2,
            Count => 1, Color => 1);
        -- Copyright
        Move_Cursor(Line => Lines - 1, Column => (Columns - 20) / 2);
        Add(Str => "2016 Bartek thindil Jasicki");
    end ShowMainMenu;

    function FormatedTime return String is
        Result : Unbounded_String := To_Unbounded_String("");
        RawImage : Unbounded_String;
        TimeArray : constant array(1..5) of Natural := (GameDate.Year,
            GameDate.Month, GameDate.Day, GameDate.Hour, GameDate.Minutes);
    begin
        for I in TimeArray'Range loop
            RawImage := To_Unbounded_String(Natural'Image(TimeArray(I)));
            case I is
                when 1 =>
                    Result := Result & Trim(RawImage, Ada.Strings.Left);
                when 2 | 3 =>
                    Result := Result & To_Unbounded_String("-") & Trim(RawImage, Ada.Strings.Left);
                when 4 =>
                    Result := Result & RawImage;
                when 5 =>
                    if TimeArray(5) < 10 then
                        Result := Result & ":0" & Trim(RawImage, Ada.Strings.Left);
                    else
                        Result := Result & ":" & Trim(RawImage, Ada.Strings.Left);
                    end if;
            end case;
        end loop;
        return To_String(Result);
    end FormatedTime;

    procedure AddMessage(Message : String) is
    begin
        Messages_List.Append(New_Item => To_Unbounded_String(FormatedTime) & ": " & To_Unbounded_String(Message));
    end AddMessage;

    procedure ShowGameMenu is
    begin
        Add(Str => "[Ship] [Crew] [Messages] [Help] [Quit]");
        Change_Attributes(Line => 0, Column => 1, Count => 1, Color => 1);
        Change_Attributes(Line => 0, Column => 8, Count => 1, Color => 1);
        Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
        --Change_Attributes(Line => 0, Column => 26, Count => 1, Color => 1);
        Change_Attributes(Line => 0, Column => 33, Count => 1, Color => 1);
        Move_Cursor(Line => 0, Column => (Columns / 3));
        Add(Str => FormatedTime);
    end ShowGameMenu;

    procedure ShowSpeedControl is
        SpeedWindow : Window;
    begin
        SpeedWindow := Create(10, 20, (Lines / 3), (Columns / 3));
        Box(SpeedWindow);
        if PlayerShip.Speed = DOCKED then
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 5);
            Add(Win => SpeedWindow, Str => "Undock");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 4, Column => 5);
            Add(Win => SpeedWindow, Str => "Trade");
            Change_Attributes(Win => SpeedWindow, Line => 4, Column => 5, 
                Count => 1, Color => 1);
        else
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
                Move_Cursor(Win => SpeedWindow, Line => 2, Column => 5);
                Add(Win => SpeedWindow, Str => "Dock");
                Change_Attributes(Win => SpeedWindow, Line => 2, Column => 5, 
                    Count => 1, Color => 1);
            end if;
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 5);
            Add(Win => SpeedWindow, Str => "Full stop");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 4, Column => 5);
            Add(Win => SpeedWindow, Str => "Quarter speed");
            Change_Attributes(Win => SpeedWindow, Line => 4, Column => 7, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 5, Column => 5);
            Add(Win => SpeedWindow, Str => "Half speed");
            Change_Attributes(Win => SpeedWindow, Line => 5, Column => 5, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 6, Column => 5);
            Add(Win => SpeedWindow, Str => "Full speed");
            Change_Attributes(Win => SpeedWindow, Line => 6, Column => 7, 
                Count => 1, Color => 1);
        end if;
        Move_Cursor(Win => SpeedWindow, Line => 8, Column => 5);
        Add(Win => SpeedWindow, Str => "Quit");
        Change_Attributes(Win => SpeedWindow, Line => 8, Column => 5, Count => 1,
            Color => 1);
        Refresh(SpeedWindow);
    end ShowSpeedControl;

    procedure ShowShipInfo is
        Weight : Integer;
    begin
        Weight := 0;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Speed: ");
        case PlayerShip.Speed is
            when DOCKED =>
                Add(Str => "Stopped (Docked to base)");
            when FULL_STOP =>
                Add(Str => "Stopped");
            when QUARTER_SPEED =>
                Add(Str => "Quarter speed");
            when HALF_SPEED =>
                Add(Str => "Half speed");
            when FULL_SPEED =>
                Add(Str => "Full speed");
        end case;
        Move_Cursor(Line => 4, Column => 2);
        Add(Str => "STATUS:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Move_Cursor(Line => Line_Position(4 + I), Column => 2);
            Add(Str => To_String(PlayerShip.Modules.Element(I).Name) & ": ");
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                Add(Str => "Damaged");
            else
                Add(Str => "OK");
            end if;
            Weight := Weight + PlayerShip.Modules.Element(I).Weight;
        end loop;
        Move_Cursor(Line => 4, Column => (Columns / 2));
        Add(Str => "CARGO:");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            Move_Cursor(Line => Line_Position(4 + I), Column => (Columns / 2));
            Add(Str => Positive'Image(PlayerShip.Cargo.Element(I).Amount) & "x" &
                To_String(PlayerShip.Cargo.Element(I).Name) & " (" &
                Positive'Image(PlayerShip.Cargo.Element(I).Weight) & "kg )");
            Weight := Weight + PlayerShip.Cargo.Element(I).Weight;
        end loop;
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Weight: " & Integer'Image(Weight) & "kg");
        Move_Cursor(Line => (Lines - 2), Column => 2);
        Add(Str => "Q for close this info");
        Change_Attributes(Line => (Lines - 2), Column => 2, Count => 1, Color => 1);
    end ShowShipInfo;

    procedure ShowCrewInfo(Key : Key_Code) is
        Health, Tired, Hungry, Thirsty, SkillLevel, OrderName : Unbounded_String;
        Skills_Names : constant array (1..4) of Unbounded_String := (To_Unbounded_String("Piloting"), 
            To_Unbounded_String("Engineering"), To_Unbounded_String("Gunnery"), 
            To_Unbounded_String("Bartering"));
    begin
        if Key /= KEY_NONE then
            Erase;
            Refresh;
            ShowGameMenu;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            Move_Cursor(Line => Line_Position(2 + I), Column => 2);
            Add(Str => Character'Val(96 + I) & " " & To_String(PlayerShip.Crew.Element(I).Name));
            Change_Attributes(Line => Line_Position(2 + I), Column => 2, Count => 1, Color => 1);
            if PlayerShip.Crew.Element(I).Health = 100 then
                Health := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Health < 100 and PlayerShip.Crew.Element(I).Health > 50 then
                Health := To_Unbounded_String(" Wounded");
            elsif PlayerShip.Crew.Element(I).Health < 51 and PlayerShip.Crew.Element(I).Health > 0 then
                Health := To_Unbounded_String(" Heavily Wounded");
            else
                Health := To_Unbounded_String(" Dead");
            end if;
            if PlayerShip.Crew.Element(I).Tired = 0 then
                Tired := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Tired > 0 and PlayerShip.Crew(I).Tired < 41 then
                Tired := To_Unbounded_String(" Tired");
            elsif PlayerShip.Crew.Element(I).Tired > 40 and PlayerShip.Crew(I).Tired < 100 then
                Tired := To_Unbounded_String(" Very tired");
            else
                Tired := To_Unbounded_String(" Unconscious");
            end if;
            if PlayerShip.Crew.Element(I).Hunger = 0 then
                Hungry := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Hunger > 0 and PlayerShip.Crew(I).Hunger < 41 then
                Hungry := To_Unbounded_String(" Hungry");
            elsif PlayerShip.Crew.Element(I).Hunger > 40 and PlayerShip.Crew(I).Hunger < 100 then
                Hungry := To_Unbounded_String(" Very hungry");
            else
                Hungry := To_Unbounded_String(" Starving");
            end if;
            if PlayerShip.Crew.Element(I).Thirst = 0 then
                Thirsty := To_Unbounded_String("");
            elsif PlayerShip.Crew.Element(I).Thirst > 0 and PlayerShip.Crew(I).Thirst < 41 then
                Thirsty := To_Unbounded_String(" Thirsty");
            elsif PlayerShip.Crew.Element(I).Thirst > 40 and PlayerShip.Crew(I).Thirst < 100 then
                Thirsty := To_Unbounded_String(" Very thirsty");
            else
                Thirsty := To_Unbounded_String(" Dehydrated");
            end if;
            Add(Str => To_String(Health) & To_String(Tired) & To_String(Hungry)
                & To_String(Thirsty));
        end loop;
        if Key /= KEY_NONE then -- Show details about selected crew member
            if (Key >= Key_Code(96 + PlayerShip.Crew.First_Index)) and (Key <= Key_Code(96 + PlayerShip.Crew.Last_Index)) then
                MemberIndex := Integer(Key) - 96;
                for J in PlayerShip.Crew.Element(MemberIndex).Skills'Range loop
                    SkillLevel := To_Unbounded_String("");
                    if PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 0 and PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 30 then
                        SkillLevel := To_Unbounded_String("Novice");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 31 and PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) < 80 then
                        SkillLevel := To_Unbounded_String("Competent");
                    elsif PlayerShip.Crew.Element(MemberIndex).Skills(J, 1) > 79 then
                        SkillLevel := To_Unbounded_String("Expert");
                    end if;
                    if SkillLevel /= "" then
                        Move_Cursor(Line => Line_Position(2 + J), Column => (Columns / 2));
                        Add(Str => To_String(Skills_Names(J)) & ": " & To_String(SkillLevel));
                    end if;
                end loop;
                case PlayerShip.Crew.Element(MemberIndex).Order is
                    when Duty =>
                        OrderName := To_Unbounded_String("On duty");
                    when Pilot =>
                        OrderName := To_Unbounded_String("Piloting");
                    when Engineer =>
                        OrderName := To_Unbounded_String("Engineering");
                    when Gunner =>
                        OrderName := To_Unbounded_String("Gunner");
                    when Rest =>
                        OrderName := To_Unbounded_String("On break");
                end case;
                Move_Cursor(Line => 8, Column => (Columns / 2));
                Add(Str => "Order: " & To_String(OrderName));
                Change_Attributes(Line => 8, Column => (Columns / 2), Count => 1, Color => 1);
            else
                MemberIndex := 0;
            end if;
        end if;
        Move_Cursor(Line => (Lines - 2), Column => 2);
        Add(Str => "Q for close this info");
        Change_Attributes(Line => (Lines - 2), Column => 2, Count => 1, Color => 1);
    end ShowCrewInfo;

    procedure ShowOrdersMenu is
        OrdersWindow : Window;
        OrdersNames : constant array (1..5) of Unbounded_String := (To_Unbounded_String("Duty"), 
            To_Unbounded_String("Piloting"), To_Unbounded_String("Engineering"), 
            To_Unbounded_String("Gunner"), To_Unbounded_String("On break"));
        StartIndex : Integer;
    begin
        OrdersWindow := Create(10, 20, (Lines / 3), (Columns / 3));
        Box(OrdersWindow);
        if MemberIndex = 1 then
            StartIndex := 1;
        else
            StartIndex := 2;
        end if;
        for I in StartIndex..OrdersNames'Last loop
            Move_Cursor(OrdersWindow, Line => Line_Position(I + 1), Column => 5);
            Add(OrdersWindow, Str => To_String(OrdersNames(I)));
            Change_Attributes(OrdersWindow, Line => Line_Position(I + 1), Column => 5, Count => 1, Color => 1);
        end loop;
        Move_Cursor(OrdersWindow, Line => 8, Column => 5);
        Add(OrdersWindow, Str => "Quit");
        Change_Attributes(OrdersWindow, Line => 8, Column => 5, Count => 1, Color => 1);
        Refresh(OrdersWindow);
    end ShowOrdersMenu;
    
    procedure ShowMessages is
        LoopStart : Positive;
        LinePos : Line_Position := 2;
    begin
        if Messages_List.Length = 0 then
            Move_Cursor(Line => (Lines / 2), Column => (Columns / 2));
            Add(Str => "No messages yet.");
            return;
        end if;
        if Messages_List.Last_Index > Positive(Lines - 2) then
            LoopStart := Messages_List.Last_Index - Positive(Lines - 2);
        else
            LoopStart := Messages_List.First_Index;
        end if;
        for I in LoopStart..Messages_List.Last_Index loop
            Move_Cursor(Line => LinePos, Column => 2);
            Add(Str => To_String(Messages_List.Element(I)));
            LinePos := LinePos + 1;
        end loop;
        Move_Cursor(Line => (Lines - 2), Column => 2);
        Add(Str => "Q for close this info");
        Change_Attributes(Line => (Lines - 2), Column => 2, Count => 1, Color => 1);
    end ShowMessages;

    procedure ShowTrade(Key : Key_Code) is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        BuyLetter, SellLetter : Character;
        FoundCargo : Boolean := False;
    begin
        Move_Cursor(Line => 1, Column => 2);
        Add(Str => "BUY SELL");
        for I in SkyBases(BaseIndex).Goods'Range loop
            if SkyBases(BaseIndex).Goods(I).Buyable then
                BuyLetter := Character'Val(96 + I);
            else
                BuyLetter := ' ';
            end if;
            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(J).Name = SkyBases(BaseIndex).Goods(I).Name then
                    FoundCargo := True;
                    exit;
                end if;
            end loop;
            if FoundCargo then
                SellLetter := Character'Val(64 + I);
            else
                SellLetter := ' ';
            end if;
            Move_Cursor(Line => Line_Position(1 + I), Column => 3);
            Add(Str => BuyLetter & "   " & SellLetter & "   " &
                To_String(SkyBases(BaseIndex).Goods(I).Name) & " Price:" &
                Positive'Image(SkyBases(BaseIndex).Goods(I).Price) & 
                " charcollum");
        end loop;
        if Key /= KEY_NONE then -- start buying/selling items from/to base
            null;
        end if;
        Move_Cursor(Line => (Lines - 2), Column => 2);
        Add(Str => "Q for close this info");
        Change_Attributes(Line => (Lines - 2), Column => 2, Count => 1, Color => 1);
    end ShowTrade;

    procedure DrawGame(CurrentState : GameStates) is
    begin
        Erase;
        Refresh;
        ShowGameMenu;
        case CurrentState is
            when Sky_Map_View =>
                ShowSkyMap;
            when Control_Speed =>
                ShowSpeedControl;
            when Ship_Info =>
                ShowShipInfo;
            when Crew_Info =>
                ShowCrewInfo(KEY_NONE);
            when Giving_Orders =>
                ShowOrdersMenu;
            when Messages_View =>
                ShowMessages;
            when Trade_View =>
                ShowTrade(KEY_NONE);
            when others =>
                null;
        end case;
    end DrawGame;

    function MainMenuKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Quit game
                return Quit;
            when Character'Pos('n') | Character'Pos('N') => -- New game
                -- Start new game
                NewGame;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others => 
                return Main_Menu;
        end case;
    end MainMenuKeys;
    
    function GameMenuKeys(CurrentState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu
                Messages_List.Clear;
                Erase;
                Refresh;
                ShowMainMenu;
                return Main_Menu;
            when Character'Pos('s') | Character'Pos('S') => -- Ship info screen
                DrawGame(Ship_Info);
                return Ship_Info;
            when Character'Pos('c') | Character'Pos('C') => -- Crew info screen
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('m') | Character'Pos('M') => -- Messages list screen
                DrawGame(Messages_View);
                return Messages_View;
            when others =>
                return CurrentState;
        end case;
    end GameMenuKeys;

    function SpeedMenuKeys(OldState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('t') | Character'Pos('T') => -- Trade with base
                if PlayerShip.Speed = DOCKED then
                    DrawGame(Trade_View);
                    return Trade_View;
                else
                    return Control_Speed;
                end if;
            when Character'Pos('u') | Character'Pos('U') => -- Undock ship from base
                DockShip(False);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('d') | Character'Pos('D') => -- Dock ship to base
                DockShip(True);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('f') | Character'Pos('F') => -- Full stop
                ChangeShipSpeed(FULL_STOP);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('a') | Character'Pos('A') => -- Quarter speed
                ChangeShipSpeed(QUARTER_SPEED);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('h') | Character'Pos('H') => -- Half speed
                ChangeShipSpeed(HALF_SPEED);
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('l') | Character'Pos('L') => -- Full speed
                ChangeShipSpeed(FULL_SPEED);
                DrawGame(Sky_Map_View);
                return OldState;
            when others =>
                return Control_Speed;
        end case;
    end SpeedMenuKeys;

    function ShipInfoKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                return Ship_Info;
        end case;
    end ShipInfoKeys;

    function CrewInfoKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('o') | Character'Pos('O') => -- Give orders to selected crew member
                if MemberIndex > 0 then
                    DrawGame(Giving_Orders);
                    return Giving_Orders;
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
            when Character'Pos('d') | Character'Pos('D') => -- Give order on duty
                GiveOrders(MemberIndex, Duty);
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
            when others =>
                return Giving_Orders;
        end case;
    end CrewOrdersKeys;


    function MessagesKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                DrawGame(Messages_View);
                return Messages_View;
        end case;
    end MessagesKeys;

    function TradeKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                DrawGame(Trade_View);
                return Trade_View;
        end case;
    end TradeKeys;

end UserInterface;
