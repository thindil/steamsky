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
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Maps; use Maps;
with Ships; use Ships;
with Ships.UI; use Ships.UI;
with Crew; use Crew;
with Crew.UI; use Crew.UI;
with Bases; use Bases;
with Bases.UI; use Bases.UI;
with Messages; use Messages;
with Combat; use Combat;
with Combat.UI; use Combat.UI;
with Crafts; use Crafts;
with Crafts.UI; use Crafts.UI;
with Help; use Help;
with MainMenu; use MainMenu;
with Events; use Events;
with ShipModules; use ShipModules;

package body UserInterface is

    DialogPanel : Panel := Null_Panel;

    procedure ShowGameHeader(CurrentState : GameStates) is
        Speed : Unbounded_String;
        HavePilot, HaveEngineer, HaveRepair, HaveUpgrade, HaveTrader : Boolean := False;
        GunnersCheck, CraftersCheck : Natural := 0;
    begin
        case CurrentState is
            when Sky_Map_View | Control_Speed | Wait_Order =>
                Add(Str => "[Menu]");
                Change_Attributes(Line => 0, Column => 2, Count => 1, Color => 1);
            when Ship_Info =>
                Add(Str => "Ship Informations [Quit]");
                Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            when Crew_Info | Giving_Orders =>
                Add(Str => "Crew Informations [Quit]");
                Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            when Messages_View =>
                Add(Str => "Last Messages [Quit]");
                Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
            when Trade_View =>
                Add(Str => "Trade with base [Quit]");
                Change_Attributes(Line => 0, Column => 17, Count => 1, Color => 1);
            when Help_View =>
                Add(Str => "Help Index [Quit]");
                Change_Attributes(Line => 0, Column => 12, Count => 1, Color => 1);
            when Craft_View =>
                Add(Str => "Manufacturing [Quit]");
                Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
            when Cargo_Info =>
                Add(Str => "Ship Cargo [Quit]");
                Change_Attributes(Line => 0, Column => 12, Count => 1, Color => 1);
            when Help_Topic =>
                Add(Str => "Help [Menu] [Quit]");
                Change_Attributes(Line => 0, Column => 6, Count => 1, Color => 1);
                Change_Attributes(Line => 0, Column => 13, Count => 1, Color => 1);
            when Repairs_View =>
                Add(Str => "Ship repairs [Quit]");
                Change_Attributes(Line => 0, Column => 14, Count => 1, Color => 1);
            when Shipyard_View =>
                Add(Str => "Shipyard [Quit]");
                Change_Attributes(Line => 0, Column => 10, Count => 1, Color => 1);
            when Recruits_View =>
                Add(Str => "Recruit new crew members [Quit]");
                Change_Attributes(Line => 0, Column => 26, Count => 1, Color => 1);
            when others =>
                null;
        end case;
        if CurrentState /= Help_View and CurrentState /= Quit_Confirm and
            CurrentState /= Help_Topic then
            case PlayerShip.Speed is
                when DOCKED =>
                    Speed := To_Unbounded_String("Docked");
                when FULL_STOP =>
                    Speed := To_Unbounded_String("Stopped");
                when QUARTER_SPEED =>
                    Speed := To_Unbounded_String("Quarter Speed");
                when HALF_SPEED =>
                    Speed := To_Unbounded_String("Half Speed");
                when FULL_SPEED =>
                    Speed := To_Unbounded_String("Full Speed");
            end case;
            Move_Cursor(Line => 0, Column => (Columns / 3));
            Add(Str => FormatedTime & " Speed: " & To_String(Speed));
            Move_Cursor(Line => 0, Column => (Columns - 22));
            Add(Str => "[P][E][G][R][M][U][T]");
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                case Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType is
                    when GUN =>
                        if PlayerShip.Modules.Element(I).Owner > 0 and GunnersCheck = 0 then
                            GunnersCheck := 1;
                        elsif PlayerShip.Modules.Element(I).Owner = 0 and GunnersCheck = 1 then
                            GunnersCheck := 2;
                        end if;
                    when ALCHEMY_LAB | FURNACE =>
                        if PlayerShip.Modules.Element(I).Current_Value > 0 then
                            if PlayerShip.Modules.Element(I).Owner > 0 and CraftersCheck < 2 then
                                CraftersCheck := 1;
                            else
                                CraftersCheck := 2;
                            end if;
                        end if;
                    when others =>
                        null;
                end case;
            end loop;
            for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                case PlayerShip.Crew.Element(I).Order is
                    when Pilot =>
                        HavePilot := True;
                        Change_Attributes(Line => 0, Column => (Columns - 21), Count => 1, Color => 2);
                    when Engineer =>
                        HaveEngineer := True;
                        Change_Attributes(Line => 0, Column => (Columns - 18), Count => 1, Color => 2);
                    when Repair =>
                        HaveRepair := True;
                        Change_Attributes(Line => 0, Column => (Columns - 12), Count => 1, Color => 2);
                    when Upgrading =>
                        HaveUpgrade := True;
                        Change_Attributes(Line => 0, Column => (Columns - 6), Count => 1, Color => 2);
                    when Talk =>
                        HaveTrader := True;
                        Change_Attributes(Line => 0, Column => (Columns - 3), Count => 1, Color => 2);
                    when others =>
                        null;
                end case;
            end loop;
            if not HavePilot then
                Change_Attributes(Line => 0, Column => (Columns - 21), Count => 1, Color => 1);
            end if;
            if not HaveEngineer then
                Change_Attributes(Line => 0, Column => (Columns - 18), Count => 1, Color => 1);
            end if;
            if GunnersCheck = 1 then
                Change_Attributes(Line => 0, Column => (Columns - 15), Count => 1, Color => 2);
            elsif GunnersCheck = 2 then
                Change_Attributes(Line => 0, Column => (Columns - 15), Count => 1, Color => 1);
            end if;
            if not HaveRepair then
                for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                    if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                        Change_Attributes(Line => 0, Column => (Columns - 12), Count => 1, Color => 1);
                        exit;
                    end if;
                end loop;
            end if;
            if CraftersCheck = 1 then
                Change_Attributes(Line => 0, Column => (Columns - 9), Count => 1, Color => 2);
            elsif CraftersCheck = 2 then
                Change_Attributes(Line => 0, Column => (Columns - 9), Count => 1, Color => 1);
            end if;
            if not HaveUpgrade and PlayerShip.UpgradeModule > 0 then
                Change_Attributes(Line => 0, Column => (Columns - 6), Count => 1, Color => 1);
            end if;
            if not HaveTrader and PlayerShip.Speed = DOCKED then
                Change_Attributes(Line => 0, Column => (Columns - 3), Count => 1, Color => 1);
            end if;
        end if;
    end ShowGameHeader;

    procedure ShowSpeedControl is
        SpeedWindow : Window;
        WindowHeight : Line_Position := 6;
        NeedRepair, IsShipyard : Boolean := False;
        AskedForBases : Boolean := True;
        CurrentLine : Line_Position := 4;
    begin
        if PlayerShip.Speed = DOCKED then
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    NeedRepair := True;
                    WindowHeight := WindowHeight + 1;
                    exit;
                end if;
            end loop;
            if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).BaseType = SHIPYARD then
                WindowHeight := WindowHeight + 1;
                IsShipyard := True;
            end if;
            if not SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).AskedForBases then
                WindowHeight := WindowHeight + 1;
                AskedForBases := False;
            end if;
            SpeedWindow := Create(WindowHeight, 17, (Lines / 3), (Columns / 2) - 9);
            Box(SpeedWindow);
            Move_Cursor(Win => SpeedWindow, Line => 1, Column => 2);
            Add(Win => SpeedWindow, Str => "Undock");
            Change_Attributes(Win => SpeedWindow, Line => 1, Column => 2, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 2, Column => 2);
            Add(Win => SpeedWindow, Str => "Trade");
            Change_Attributes(Win => SpeedWindow, Line => 2, Column => 2, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 2);
            Add(Win => SpeedWindow, Str => "Recruit");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 3, 
                Count => 1, Color => 1);
            if not AskedForBases then
                Move_Cursor(Win => SpeedWindow, Line => 4, Column => 2);
                Add(Win => SpeedWindow, Str => "Ask for bases");
                Change_Attributes(Win => SpeedWindow, Line => 4, Column => 10, 
                    Count => 1, Color => 1);
                CurrentLine := CurrentLine + 1;
            end if;
            if NeedRepair then
                Move_Cursor(Win => SpeedWindow, Line => CurrentLine, Column => 2);
                Add(Win => SpeedWindow, Str => "Repair");
                Change_Attributes(Win => SpeedWindow, Line => CurrentLine, Column => 2, 
                    Count => 1, Color => 1);
                CurrentLine := CurrentLine + 1;
            end if;
            if IsShipyard then
                Move_Cursor(Win =>SpeedWindow, Line => CurrentLine, Column => 2);
                Add(Win => SpeedWindow, Str => "Shipyard");
                Change_Attributes(Win => SpeedWindow, Line => CurrentLine, Column => 2, 
                    Count => 1, Color => 1);
                CurrentLine := CurrentLine + 1;
            end if;
            Move_Cursor(Win => SpeedWindow, Line => CurrentLine, Column => 2);
            Add(Win => SpeedWindow, Str => "Quit");
            Change_Attributes(Win => SpeedWindow, Line => CurrentLine, Column => 2, Count => 1,
                Color => 1);
        else
            SpeedWindow := Create(8, 17, (Lines / 3), (Columns / 2) - 8);
            Box(SpeedWindow);
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
                Move_Cursor(Win => SpeedWindow, Line => 1, Column => 2);
                Add(Win => SpeedWindow, Str => "Dock");
                Change_Attributes(Win => SpeedWindow, Line => 1, Column => 2, 
                    Count => 1, Color => 1);
            end if;
            Move_Cursor(Win => SpeedWindow, Line => 2, Column => 2);
            Add(Win => SpeedWindow, Str => "Full stop");
            Change_Attributes(Win => SpeedWindow, Line => 2, Column => 2, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 3, Column => 2);
            Add(Win => SpeedWindow, Str => "Quarter speed");
            Change_Attributes(Win => SpeedWindow, Line => 3, Column => 4, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 4, Column => 2);
            Add(Win => SpeedWindow, Str => "Half speed");
            Change_Attributes(Win => SpeedWindow, Line => 4, Column => 2, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 5, Column => 2);
            Add(Win => SpeedWindow, Str => "Full speed");
            Change_Attributes(Win => SpeedWindow, Line => 5, Column => 4, 
                Count => 1, Color => 1);
            Move_Cursor(Win => SpeedWindow, Line => 6, Column => 2);
            Add(Win => SpeedWindow, Str => "Quit");
            Change_Attributes(Win => SpeedWindow, Line => 6, Column => 2, Count => 1,
                Color => 1);
        end if;
        Refresh(SpeedWindow);
    end ShowSpeedControl;

    procedure ShowConfirm(Message : String) is
        ConfirmWindow : Window;
        Width : Positive;
        Height : Positive := 1;
    begin
        Width := Message'Length + 8;
        if Width >= Positive(Columns - 4) then
            Height := (Width / Positive(Columns - 4) + 2);
            Width := (Width / Height) + 2;
        end if;
        Height := Height + 2;
        ConfirmWindow := New_Window(Line_Position(Height),
            Column_Position(Width), ((Lines / 2) - Line_Position(Height / 2)),
            ((Columns / 2) - Column_Position(Width / 2)));
        Box(ConfirmWindow);
        Add(Win => ConfirmWindow, Str => Message & " (Y/N)", Line => 1, Column => 1);
        Refresh(ConfirmWindow);
    end ShowConfirm;

    procedure ShowDialog(Message : String) is
        DialogWindow : Window;
        Width : Positive;
        Height : Positive := 1;
    begin
        Width := Message'Length + 2;
        if Width >= Positive(Columns - 4) then
            Height := (Width / Positive(Columns - 4) + 2);
            Width := (Width / Height) + 2;
        end if;
        Height := Height + 2;
        DialogWindow := New_Window(Line_Position(Height),
            Column_Position(Width), ((Lines / 2) - Line_Position(Height / 2)),
            ((Columns / 2) - Column_Position(Width / 2)));
        Box(DialogWindow);
        Add(Win => DialogWindow, Str => Message, Line => 1, Column => 1);
        if DialogPanel = Null_Panel then
            DialogPanel := New_Panel(DialogWindow);
        else
            Replace(DialogPanel, DialogWindow);
        end if;
        if Is_Hidden(DialogPanel) then
            Show(DialogPanel);
        end if;
    end ShowDialog;

    function HideDialog return Boolean is
    begin
        if not Is_Hidden(DialogPanel) then
            Hide(DialogPanel);
            return True;
        end if;
        return False;
    end HideDialog;

    procedure ShowWaitOrder is
        WaitWindow : Window;
        WaitLines : Line_Position := 9;
        WaitColumns : Column_Position := 23;
        NeedHealing, NeedRest : Boolean := False;
        WaitOrders : constant array (1..6) of Unbounded_String :=
            (To_Unbounded_String("Wait 1 minute"), To_Unbounded_String("Wait 5 minutes"), 
            To_Unbounded_String("Wait 10 minutes"), To_Unbounded_String("Wait 15 minutes"), 
            To_Unbounded_String("Wait 30 minutes"), To_Unbounded_String("Wait 1 hour"));
        CurrentLine : Line_Position := 0;
    begin
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Tired > 0 and PlayerShip.Crew.Element(I).Order = Rest then
                NeedRest := True;
            end if;
            if PlayerShip.Crew.Element(I).Health < 100 and PlayerShip.Crew.Element(I).Health > 0 
                and PlayerShip.Crew.Element(I).Order = Rest 
            then
                for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                    if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                        PlayerShip.Modules.Element(J).Owner = I 
                    then
                        NeedHealing := True;
                        exit;
                    end if;
                end loop;
            end if;
        end loop;
        if NeedRest then
            WaitLines := WaitLines + 1;
            WaitColumns := 31;
        end if;
        if NeedHealing then
            WaitLines := WaitLines + 1;
            WaitColumns := 31;
        end if;
        WaitWindow := Create(WaitLines, WaitColumns, (Lines / 2) - (WaitLines / 2), (Columns / 2) - (WaitColumns / 2));
        Box(WaitWindow);
        for I in WaitOrders'Range loop
            CurrentLine := CurrentLine + 1;
            Move_Cursor(Win => WaitWindow, Line => CurrentLine, Column => 1);
            Add(Win => WaitWindow, Str => Integer'Image(I) & " " & To_String(WaitOrders(I)));
            Change_Attributes(Win => WaitWindow, Line => CurrentLine, Column => 2, Count => 1,
                Color => 1);
        end loop;
        if NeedRest then
            CurrentLine := CurrentLine + 1;
            Move_Cursor(Win => WaitWindow, Line => CurrentLine, Column => 1);
            Add(Win => WaitWindow, Str => Line_Position'Image(CurrentLine) & " " & 
                "Wait until crew is rested");
            Change_Attributes(Win => WaitWindow, Line => CurrentLine, Column => 2, Count => 1,
                Color => 1);
        end if;
        if NeedHealing then
            CurrentLine := CurrentLine + 1;
            Move_Cursor(Win => WaitWindow, Line => CurrentLine, Column => 1);
            Add(Win => WaitWindow, Str => Line_Position'Image(CurrentLine) & " " & 
                "Wait until crew is healed");
            Change_Attributes(Win => WaitWindow, Line => CurrentLine, Column => 2, Count => 1,
                Color => 1);
        end if;
        Move_Cursor(Win => WaitWindow, Line => WaitLines - 2, Column => 2);
        Add(Win => WaitWindow, Str =>  "Quit");
        Change_Attributes(Win => WaitWindow, Line => WaitLines - 2, Column => 2, Count => 1,
            Color => 1);
        Refresh(WaitWindow);
    end ShowWaitOrder;

    procedure ShowGameMenu is
        MenuWindow : Window;
    begin
        MenuWindow := Create(14, 32, (Lines / 3), (Columns / 2) - 16);
        Box(MenuWindow);
        Move_Cursor(Win => MenuWindow, Line => 2, Column => 7);
        Add(Win => MenuWindow, Str => "Ship informations");
        Change_Attributes(Win => MenuWindow, Line => 2, Column => 7, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 3, Column => 7);
        Add(Win => MenuWindow, Str => "Ship cargo");
        Change_Attributes(Win => MenuWindow, Line => 3, Column => 13, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 4, Column => 7);
        Add(Win => MenuWindow, Str => "Crew informations");
        Change_Attributes(Win => MenuWindow, Line => 4, Column => 7, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 5, Column => 7);
        Add(Win => MenuWindow, Str => "Ship orders");
        Change_Attributes(Win => MenuWindow, Line => 5, Column => 12, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 6, Column => 7);
        Add(Win => MenuWindow, Str => "Crafting");
        Change_Attributes(Win => MenuWindow, Line => 6, Column => 8, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 7, Column => 7);
        Add(Win => MenuWindow, Str => "Last messages");
        Change_Attributes(Win => MenuWindow, Line => 7, Column => 12, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 8, Column => 7);
        Add(Win => MenuWindow, Str => "Wait orders");
        Change_Attributes(Win => MenuWindow, Line => 8, Column => 7, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 9, Column => 7);
        Add(Win => MenuWindow, Str => "Help");
        Change_Attributes(Win => MenuWindow, Line => 9, Column => 7, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 10, Column => 7);
        Add(Win => MenuWindow, Str => "Quit from game");
        Change_Attributes(Win => MenuWindow, Line => 10, Column => 7, Count => 1, Color => 1);
        Move_Cursor(Win => MenuWindow, Line => 12, Column => 2);
        Add(Win => MenuWindow, Str => "Any other key hide this menu");
        Refresh(MenuWindow);
    end ShowGameMenu;

    procedure DrawGame(CurrentState : GameStates) is
    begin
        Erase;
        ShowGameHeader(CurrentState);
        case CurrentState is
            when Sky_Map_View =>
                ShowSkyMap;
            when Control_Speed =>
                ShowSkyMap;
                ShowSpeedControl;
            when Ship_Info =>
                ShowShipInfo;
            when Crew_Info =>
                ShowCrewInfo;
            when Messages_View =>
                ShowMessages;
            when Trade_View =>
                ShowTrade;
            when Help_View =>
                ShowHelpMenu;
            when Quit_Confirm =>
                Refresh_Without_Update;
                ShowConfirm("Are you sure want to quit game?");
            when Combat_Confirm =>
                Refresh_Without_Update;
                ShowConfirm(To_String(EnemyName) & " is near, attack?");
            when Combat_State =>
                ShowCombat;
            when Craft_View =>
                ShowRecipes;
            when Wait_Order =>
                ShowSkyMap;
                ShowWaitOrder;
            when Cargo_Info =>
                ShowCargoInfo;
            when Help_Topic =>
                ShowHelp(True);
            when Repairs_View =>
                ShowRepair;
            when Clear_Confirm => 
                Refresh_Without_Update;
                ShowConfirm("Are you sure want to clear all messages?");
            when Shipyard_View =>
                ShowShipyard;
            when Recruits_View =>
                ShowRecruits;
            when Dismiss_Confirm =>
                Refresh_Without_Update;
                ShowConfirm("Are you sure want to dismiss this crew member?");
            when others =>
                null;
        end case;
        if LastMessage /= To_Unbounded_String("") then
            Move_Cursor(Line => 1, Column => 2);
            Add(Str => To_String(LastMessage));
            LastMessage := To_Unbounded_String("");
        end if;
        Update_Panels;
        Update_Screen;
    end DrawGame;

    function GameMenuKeys(CurrentState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to main menu
                DrawGame(Quit_Confirm);
                return Quit_Confirm;
            when Character'Pos('s') | Character'Pos('S') => -- Ship info screen
                DrawGame(Ship_Info);
                return Ship_Info;
            when Character'Pos('c') | Character'Pos('C') => -- Crew info screen
                DrawGame(Crew_Info);
                return Crew_Info;
            when Character'Pos('r') | Character'Pos('R') => -- Crafting screen
                DrawGame(Craft_View);
                return Craft_View;
            when Character'Pos('m') | Character'Pos('M') => -- Messages list screen
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('h') | Character'Pos('H') => -- Help screen
                DrawGame(Help_View);
                return Help_View;
            when Character'Pos('e') | Character'Pos('E') => -- Show game menu
                DrawGame(CurrentState);
                ShowGameMenu;
                return CurrentState;
            when Character'Pos('a') | Character'Pos('A') => -- Cargo info screen
                DrawGame(Cargo_Info);
                return Cargo_Info;
            when others =>
                DrawGame(CurrentState);
                return CurrentState;
        end case;
    end GameMenuKeys;

    function SpeedMenuKeys(OldState : GameStates; Key : Key_Code) return GameStates is
        HaveTrader : Boolean := False;
    begin
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Talk then
                HaveTrader := True;
            end if;
        end loop;
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return OldState;
            when Character'Pos('t') | Character'Pos('T') => -- Trade with base
                if not HaveTrader then
                    ShowDialog("You don't have anyone assigned to trading.");
                    DrawGame(Sky_Map_View);
                    return OldState;
                end if;
                if PlayerShip.Speed = DOCKED then
                    DrawGame(Trade_View);
                    return Trade_View;
                else
                    return Control_Speed;
                end if;
            when Character'Pos('e') | Character'Pos('E') => -- Recruit new crew members in base
                if not HaveTrader then
                    ShowDialog("You don't have anyone assigned to trading.");
                    DrawGame(Sky_Map_View);
                    return OldState;
                end if;
                if PlayerShip.Speed = DOCKED then
                    DrawGame(Recruits_View);
                    return Recruits_View;
                else
                    return Control_Speed;
                end if;
            when Character'Pos('b') | Character'Pos('B') => -- Ask for other bases
                if not HaveTrader then
                    ShowDialog("You don't have anyone assigned to trading.");
                    DrawGame(Sky_Map_View);
                    return OldState;
                end if;
                if PlayerShip.Speed = DOCKED then
                    AskForBases;
                    DrawGame(Sky_Map_View);
                    return OldState;
                else
                    return Control_Speed;
                end if;
            when Character'Pos('r') | Character'Pos('R') => -- Repair ship in base
                if not HaveTrader then
                    ShowDialog("You don't have anyone assigned to trading.");
                    DrawGame(Sky_Map_View);
                    return OldState;
                end if;
                if PlayerShip.Speed = DOCKED then
                    DrawGame(Repairs_View);
                    return Repairs_View;
                else
                    return Control_Speed;
                end if;
            when Character'Pos('s') | Character'Pos('S') => -- Shipyard in base
                if not HaveTrader then
                    ShowDialog("You don't have anyone assigned to trading.");
                    DrawGame(Sky_Map_View);
                    return OldState;
                end if;
                if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).BaseType = SHIPYARD then
                    DrawGame(Shipyard_View);
                    return Shipyard_View;
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

    function ConfirmKeys(OldState : GameStates; Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('n') | Character'Pos('N') => -- Back to old screen
                if OldState = Clear_Confirm then
                    DrawGame(Messages_View);
                    return Messages_View;
                elsif OldState = Dismiss_Confirm then
                    DrawGame(Crew_Info);
                    return Crew_Info;
                else
                    DrawGame(Sky_Map_View);
                    return Sky_Map_View;
                end if;
            when Character'Pos('y') | Character'Pos('Y') => -- Confirm action
                if OldState = Quit_Confirm then
                    SaveGame;
                    ClearMessages;
                    Erase;
                    Refresh;
                    ShowMainMenu;
                    return Main_Menu;
                elsif OldState = Combat_Confirm then
                    DrawGame(Combat_State);
                    return Combat_State;
                elsif OldState = Clear_Confirm then 
                    ClearMessages;
                    DrawGame(Messages_View);
                    return Messages_View;
                else
                    DismissMember;
                    DrawGame(Crew_Info);
                    return Crew_Info;
                end if;
            when others =>
                DrawGame(OldState);
                return OldState;
        end case;
    end ConfirmKeys;

    function WaitMenuKeys(OldState : GameStates; Key : Key_Code) return GameStates is
        TimeNeeded, CabinIndex, TempTimeNeeded : Natural := 0;
        ReturnState : GameStates;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos('1') => -- Wait 1 minute
                UpdateGame(1);
            when Character'Pos('2') => -- Wait 5 minutes
                UpdateGame(5);
            when Character'Pos('3') => -- Wait 10 minutes
                UpdateGame(10);
            when Character'Pos('4') => -- Wait 15 minutes
                UpdateGame(15);
            when Character'Pos('5') => -- Wait 30 minute
                UpdateGame(30);
            when Character'Pos('6') => -- Wait 1 hour
                UpdateGame(60);
            when Character'Pos('7') => -- Wait until crew is rested
                for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(I).Tired > 0 and PlayerShip.Crew.Element(I).Order = Rest then
                        CabinIndex := 0;
                        TempTimeNeeded := 0;
                        for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                            if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                                PlayerShip.Modules.Element(J).Owner = I then
                                CabinIndex := J;
                                exit;
                            end if;
                        end loop;
                        if CabinIndex > 0 then
                            TempTimeNeeded := (PlayerShip.Crew.Element(I).Tired / PlayerShip.Modules.Element(CabinIndex).Current_Value) * 
                                15;
                            if TempTimeNeeded = 0 then
                                TempTimeNeeded := 15;
                            end if;
                        else
                            TempTimeNeeded := PlayerShip.Crew.Element(I).Tired * 15;
                        end if;
                        TempTimeNeeded := TempTimeNeeded + 15;
                        if TempTimeNeeded > TimeNeeded then
                            TimeNeeded := TempTimeNeeded;
                        end if;
                    end if;
                end loop;
                if TimeNeeded > 0 then
                    UpdateGame(TimeNeeded);
                else
                    return Wait_Order;
                end if;
            when Character'Pos('8') => -- Wait until crew is healed
                for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                    if PlayerShip.Crew.Element(I).Health < 100 and PlayerShip.Crew.Element(I).Health > 0  and
                        PlayerShip.Crew.Element(I).Order = Rest 
                    then
                        for J in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                            if Modules_List.Element(PlayerShip.Modules.Element(J).ProtoIndex).MType = CABIN and
                                PlayerShip.Modules.Element(J).Owner = I 
                            then
                                if TimeNeeded < (100 - PlayerShip.Crew.Element(I).Health) * 15 then
                                    TimeNeeded := (100 - PlayerShip.Crew.Element(I).Health) * 15;
                                end if;
                                exit;
                            end if;
                        end loop;
                    end if;
                end loop;
                if TimeNeeded > 0 then
                    UpdateGame(TimeNeeded);
                else
                    return Wait_Order;
                end if;
            when others =>
                return Wait_Order;
        end case;
        ReturnState := CheckForEvent(OldState);
        DrawGame(ReturnState);
        return ReturnState;
    end WaitMenuKeys;

end UserInterface;
