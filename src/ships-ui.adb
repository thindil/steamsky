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
with Crafts; use Crafts;
with Items; use Items;
with ShipModules; use ShipModules;
with UserInterface; use UserInterface;
with Messages; use Messages;

package body Ships.UI is

    ModulesMenu : Menu;
    MenuWindow : Window;

    procedure ShowModuleInfo is
        InfoWindow : Window;
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        DamagePercent : Natural;
        MAmount : Natural := 0;
        CurrentLine : Line_Position := 6;
        MaxUpgrade, UpgradePercent : Natural;
    begin
        InfoWindow := Create(10, (Columns / 2), 8, (Columns / 2));
        Add(Win => InfoWindow, Str => "Status: ");
        DamagePercent := 100 -
            Natural((Float(PlayerShip.Modules.Element(ModuleIndex).Durability) /
            Float(PlayerShip.Modules.Element(ModuleIndex).MaxDurability)) * 100.0);
        if DamagePercent = 0 then
            Add(Win => InfoWindow, Str => "Ok");
        elsif DamagePercent > 0 and DamagePercent < 20 then
            Add(Win => InfoWindow, Str => "Slightly damaged");
        elsif DamagePercent > 19 and DamagePercent < 50 then
            Add(Win => InfoWindow, Str => "Damaged");
        elsif DamagePercent > 49 and DamagePercent < 80 then
            Add(Win => InfoWindow, Str => "Heavily damaged");
        elsif DamagePercent > 79 and DamagePercent < 100 then
            Add(Win => InfoWindow, Str => "Almost destroyed");
        else
            Add(Win => InfoWindow, Str => "Destroyed");
        end if;
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => "Weight:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Weight) &
            " kg");
        Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
        Add(Win => InfoWindow, Str => "Repair material: ");
        for I in Items_List.First_Index..Items_List.Last_Index loop
            if Items_List.Element(I).IType = Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairMaterial
                then
                if MAmount > 0 then
                    Add(Win => InfoWindow, Str => " or ");
                end if;
                Add(Win => InfoWindow, Str => To_String(Items_List.Element(I).Name));
                MAmount := MAmount + 1;
            end if;
        end loop;
        Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
        Add(Win => InfoWindow, Str => "Repair skill: " &
            To_String(Skills_Names(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairSkill)));
        Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
        case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
            when ENGINE =>
                Add(Win => InfoWindow, Str => "Max power:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value));
            when CARGO =>
                Add(Win => InfoWindow, Str => "Max cargo:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value) &
                    " kg");
            when HULL =>
                Add(Win => InfoWindow, Str => "Modules:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Current_Value) &
                " /" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value));
            when CABIN =>
                if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                    Add(Win => InfoWindow, Str => "Owner: " &
                        To_String(PlayerShip.Crew.Element(PlayerShip.Modules.Element(ModuleIndex).Owner).Name));
                else
                    Add(Win => InfoWindow, Str => "Owner: none");
                end if;
            when GUN =>
                Add(Win => InfoWindow, Str => "Ammunition: " &  
                    To_String(Items_List.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).Name));
            when TURRET =>
                Add(Win => InfoWindow, Str => "Weapon: " &
                    To_String(PlayerShip.Modules.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).Name));
            when others =>
                null;
        end case;
        if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /= NONE then
            Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
            Add(Win => InfoWindow, Str => "Upgrading: ");
            case PlayerShip.Modules.Element(ModuleIndex).UpgradeAction is
                when DURABILITY => 
                    Add(Win => InfoWindow, Str => "durability");
                    MaxUpgrade := 10;
                when MAX_VALUE =>
                    case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                        when ENGINE =>
                            Add(Win => InfoWindow, Str => "power");
                            MaxUpgrade := 10;
                        when CABIN =>
                            Add(Win => InfoWindow, Str => "quality");
                            MaxUpgrade := 100;
                        when GUN | BATTERING_RAM =>
                            Add(Win => InfoWindow, Str => "damage");
                            MaxUpgrade := 100;
                        when HULL =>
                            Add(Win => InfoWindow, Str => "enlarge");
                            MaxUpgrade := 500;
                        when others =>
                            null;
                    end case;
                when others =>
                    null;
            end case;
            Move_Cursor(Win => InfoWindow, Line => 6, Column => 0);
            Add(Win => InfoWindow, Str => "Upgrade progress: ");
            UpgradePercent :=  100 - Natural((Float(PlayerShip.Modules.Element(ModuleIndex).UpgradeProgress) /
                Float(MaxUpgrade)) * 100.0);
            if UpgradePercent < 11 then
                Add(Win => InfoWindow, Str => "started");
            elsif UpgradePercent < 31 then
                Add(Win => InfoWindow, Str => "designing");
            elsif UpgradePercent < 51 then
                Add(Win => InfoWindow, Str => "base upgrades");
            elsif UpgradePercent < 80 then
                Add(Win => InfoWindow, Str => "advanced upgrades");
            else
                Add(Win => InfoWindow, Str => "final upgrades");
            end if;
            CurrentLine := 8;
        end if;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Upgrade module");
        Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 0, 
            Count => 1, Color => 1);
        Move_Cursor(Win => InfoWindow, Line => (CurrentLine + 1), Column => 0);
        Add(Win => InfoWindow, Str => "Rename module");
        Change_Attributes(Win => InfoWindow, Line => (CurrentLine + 1), Column => 2, 
            Count => 1, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowModuleInfo;

    procedure ShowShipInfo is
        Weight : Integer;
        Modules_Items: constant Item_Array_Access := new Item_Array(1..(PlayerShip.Modules.Last_Index + 1));
        MenuHeight, CurrentLine : Line_Position;
        MenuLength : Column_Position;
        UpgradePercent, MaxUpgrade : Natural;
    begin
        Weight := CountShipWeight(PlayerShip);
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Name: " & To_String(PlayerShip.Name));
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Manufacturing: ");
        if PlayerShip.Craft = 0 then
            Add(Str => "Nothing");
        else
            Add(Str => To_String(Items_List.Element(Recipes_List.Element(PlayerShip.Craft).ResultIndex).Name));
        end if;
        Move_Cursor(Line => 4, Column => 2);
        Add(Str => "Upgrading: ");
        if PlayerShip.UpgradeModule = 0 then
            Add(Str => "Nothing");
            CurrentLine := 5;
        else
            Add(Str => To_String(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name) & " " );
            case PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeAction is
                when DURABILITY => 
                    Add(Str => "(durability)");
                    MaxUpgrade := 10;
                when MAX_VALUE =>
                    case Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MType is
                        when ENGINE =>
                            Add(Str => "(power)");
                            MaxUpgrade := 10;
                        when CABIN =>
                            Add(Str => "(quality)");
                            MaxUpgrade := 100;
                        when GUN | BATTERING_RAM =>
                            Add(Str => "(damage)");
                            MaxUpgrade := 100;
                        when HULL =>
                            Add(Str => "(enlarge)");
                            MaxUpgrade := 500;
                        when others =>
                            null;
                    end case;
                when others =>
                    null;
            end case;
            Move_Cursor(Line => 5, Column => 2);
            Add(Str => "Upgrade progress: ");
            UpgradePercent :=  100 - Natural((Float(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress) /
                Float(MaxUpgrade)) * 100.0);
            if UpgradePercent < 11 then
                Add(Str => "started");
            elsif UpgradePercent < 31 then
                Add(Str => "designing");
            elsif UpgradePercent < 51 then
                Add(Str => "base upgrades");
            elsif UpgradePercent < 80 then
                Add(Str => "advanced upgrades");
            else
                Add(Str => "final upgrades");
            end if;
            CurrentLine := 6;
        end if;
        Move_Cursor(Line => CurrentLine, Column => 2);
        Add(Str => "Weight:" & Integer'Image(Weight) & "kg");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Modules_Items.all(I) := New_Item(To_String(PlayerShip.Modules.Element(I).Name));
        end loop;
        Modules_Items.all(Modules_Items'Last) := Null_Item;
        ModulesMenu := New_Menu(Modules_Items);
        Set_Format(ModulesMenu, Lines - 10, 1);
        Set_Mark(ModulesMenu, "");
        Scale(ModulesMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, (CurrentLine + 2), 2);
        Set_Window(ModulesMenu, MenuWindow);
        Set_Sub_Window(ModulesMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(ModulesMenu);
        ShowModuleInfo;
        Refresh(MenuWindow);
    end ShowShipInfo;

    procedure ShowItemInfo is
        InfoWindow : Window;
        ItemIndex : constant Positive := Get_Index(Current(ModulesMenu));
        ItemWeight : constant Positive := PlayerShip.Cargo.Element(ItemIndex).Amount * 
            Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Weight;
    begin
        InfoWindow := Create(5, (Columns / 2), 3, (Columns / 2));
        Add(Win => InfoWindow, Str => "Amount:" & Positive'Image(PlayerShip.Cargo.Element(ItemIndex).Amount));
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => "Weight:" &
            Positive'Image(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Weight) & " kg");
        Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
        Add(Win => InfoWindow, Str => "Total weight:" & Positive'Image(ItemWeight) & " kg");
        Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
        Add(Win => InfoWindow, Str => "Drop cargo");
        Change_Attributes(Win => InfoWindow, Line => 4, Column => 0, 
            Count => 1, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowItemInfo;

    procedure ShowCargoInfo is
        Cargo_Items: constant Item_Array_Access := new Item_Array(1..(PlayerShip.Cargo.Last_Index + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        FreeSpace : Integer;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            Cargo_Items.all(I) := New_Item(To_String(Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).Name));
        end loop;
        Cargo_Items.all(Cargo_Items'Last) := Null_Item;
        ModulesMenu := New_Menu(Cargo_Items);
        Set_Format(ModulesMenu, Lines - 10, 1);
        Set_Mark(ModulesMenu, "");
        Scale(ModulesMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(ModulesMenu, MenuWindow);
        Set_Sub_Window(ModulesMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(ModulesMenu);
        ShowItemInfo;
        Refresh(MenuWindow);
        Move_Cursor(Line => MenuHeight + 4, Column => 2);
        FreeSpace := FreeCargo(0);
        if FreeSpace < 0 then
            FreeSpace := 0;
        end if;
        Add(Str => "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
    end ShowCargoInfo;

    procedure ShowModuleForm is
        FormWindow : Window;
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        Visibility : Cursor_Visibility := Normal;
        ModuleName : String(1..20);
        NewName : Unbounded_String;
        SemicolonIndex : Natural;
    begin
        FormWindow := Create(3, 34, ((Lines / 2) - 1), ((Columns / 2) - 17));
        Box(FormWindow);
        Set_Echo_Mode(True);
        Set_Cursor_Visibility(Visibility);
        Move_Cursor(Win => FormWindow, Line => 1, Column => 2);
        Add(Win => FormWindow, Str => "New name: ");
        Get(Win => FormWindow, Str => ModuleName, Len => 20);
        NewName := Trim(To_Unbounded_String(ModuleName), Ada.Strings.Both);
        if Length(NewName) > 0 then
            SemicolonIndex := Index(NewName, ";");
            while SemicolonIndex > 0 loop
                Delete(NewName, SemicolonIndex, SemicolonIndex);
                SemicolonIndex := Index(NewName, ";");
            end loop;
            UpdateModule(PlayerShip, ModuleIndex, "Name", To_String(NewName));
        end if;
        Delete(FormWindow);
        Visibility := Invisible;
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);
        DrawGame(Ship_Info);
    end ShowModuleForm;

    procedure ShowCargoForm is
        FormWindow : Window;
        ItemIndex : constant Positive := Get_Index(Current(ModulesMenu));
        Visibility : Cursor_Visibility := Normal;
        ItemName : constant String := To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name);
        Amount : String(1..6);
        DropAmount : Natural;
        FormText : constant String := "Amount of " & ItemName & " to drop: ";
    begin
        FormWindow := Create(3, Column_Position(FormText'Length + 10), ((Lines / 2) - 1), 
            ((Columns / 2) - (Column_Position(FormText'Length + 10) / 2)));
        Box(FormWindow);
        Set_Echo_Mode(True);
        Set_Cursor_Visibility(Visibility);
        Move_Cursor(Win => FormWindow, Line => 1, Column => 2);
        Add(Win => FormWindow, Str => FormText);
        Get(Win => FormWindow, Str => Amount, Len => 6);
        if Amount = "      " then
            Amount := "0     ";
        end if;
        DropAmount := Natural'Value(Amount);
        if DropAmount > 0 and DropAmount <= PlayerShip.Cargo.Element(ItemIndex).Amount then
            UpdateCargo(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex, (0 - DropAmount));
            AddMessage("You dropped" & Positive'Image(DropAmount) & " " & ItemName, OtherMessage);
        elsif DropAmount > PlayerShip.Cargo.Element(ItemIndex).Amount then
            ShowDialog("You can't drop more " & ItemName & " than you have.");
        end if;
        Delete(FormWindow);
        Visibility := Invisible;
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);
        DrawGame(Cargo_Info);
    exception
        when CONSTRAINT_ERROR =>
            Visibility := Invisible;
            Set_Echo_Mode(False);
            Set_Cursor_Visibility(Visibility);
            ShowDialog("You must enter number as an amount to drop.");
            DrawGame(Cargo_Info);
    end ShowCargoForm;

    procedure ShowUpgradeMenu is
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        UpgradeWindow : Window;
        MaxValue : Natural;
        WindowHeight : Line_Position := 3;
        UpgradeDurability, UpgradeMaxValue, UpgradeContinue : Unbounded_String := Null_Unbounded_String;
        CurrentLine : Line_Position := 1;
    begin
        MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Durability) * 1.5);
        if PlayerShip.Modules.Element(ModuleIndex).MaxDurability < MaxValue then
            UpgradeDurability := To_Unbounded_String("1 Upgrade durability");
            WindowHeight := WindowHeight + 1;
        end if;
        MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
        case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
            when ENGINE =>
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    UpgradeMaxValue := To_Unbounded_String("2 Upgrade engine power");
                    WindowHeight := WindowHeight + 1;
                end if;
            when CABIN =>
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    UpgradeMaxValue := To_Unbounded_String("2 Upgrade quality");
                    WindowHeight := WindowHeight + 1;
                end if;
            when GUN | BATTERING_RAM =>
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    UpgradeMaxValue := To_Unbounded_String("2 Upgrade damage");
                    WindowHeight := WindowHeight + 1;
                end if;
            when HULL =>
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    UpgradeMaxValue := To_Unbounded_String("2 Enlarge hull");
                    WindowHeight := WindowHeight + 1;
                end if;
            when others =>
                null;
        end case;
        if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /= NONE then
            UpgradeContinue := To_Unbounded_String("3 Continue upgrading");
            WindowHeight := WindowHeight + 1;
        end if;
        if UpgradeDurability = Null_Unbounded_String and UpgradeMaxValue = Null_Unbounded_String and 
            UpgradeContinue = Null_Unbounded_String then
            ShowDialog("This module don't have available upgrades.");
            return;
        end if;
        UpgradeWindow := Create(WindowHeight, 24, ((Lines / 2) - 3), ((Columns / 2) - 12));
        Box(UpgradeWindow);
        if UpgradeDurability /= Null_Unbounded_String then
            Move_Cursor(Win => UpgradeWindow, Line => CurrentLine, Column => 1);
            Add(Win => UpgradeWindow, Str => To_String(UpgradeDurability));
            Change_Attributes(Win => UpgradeWindow, Line => CurrentLine, Column => 1, 
                Count => 1, Color => 1);
            CurrentLine := CurrentLine + 1;
        end if;
        if UpgradeMaxValue /= Null_Unbounded_String then
            Move_Cursor(Win => UpgradeWindow, Line => CurrentLine, Column => 1);
            Add(Win => UpgradeWindow, Str => To_String(UpgradeMaxValue));
            Change_Attributes(Win => UpgradeWindow, Line => CurrentLine, Column => 1, 
                Count => 1, Color => 1);
            CurrentLine := CurrentLine + 1;
        end if;
        if UpgradeContinue /= Null_Unbounded_String then
            Move_Cursor(Win => UpgradeWindow, Line => CurrentLine, Column => 1);
            Add(Win => UpgradeWindow, Str => To_String(UpgradeContinue));
            Change_Attributes(Win => UpgradeWindow, Line => CurrentLine, Column => 1, 
                Count => 1, Color => 1);
            CurrentLine := CurrentLine + 1;
        end if;
        Move_Cursor(Win => UpgradeWindow, Line => CurrentLine, Column => 1);
        Add(Win => UpgradeWindow, Str => "Quit");
        Change_Attributes(Win => UpgradeWindow, Line => CurrentLine, Column => 1, 
            Count => 1, Color => 1);
        Refresh;
        Refresh(UpgradeWindow);
        Delete(UpgradeWindow);
    end ShowUpgradeMenu;

    function ShipInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map or combat screen
                DrawGame(OldState);
                return OldState;
            when 56 | KEY_UP => -- Select previous module
                Result := Driver(ModulesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next module
                Result := Driver(ModulesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                end if;
            when Character'Pos('n') | Character'Pos('N') => -- Rename selected module
                ShowModuleForm;
            when Character'Pos('u') | Character'Pos('U') => -- Start upgrading selected module
                ShowUpgradeMenu;
                return Upgrade_Module;
            when others =>
                null;
        end case;
        return Ship_Info;
    end ShipInfoKeys;

    function CargoInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back sky map or combat screen
                DrawGame(OldState);
                return OldState;
            when 56 | KEY_UP => -- Select previous item
                Result := Driver(ModulesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next item
                Result := Driver(ModulesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when Character'Pos('d') | Character'Pos('D') => -- Drop selected cargo
                ShowCargoForm;
            when others =>
                null;
        end case;
        return Cargo_Info;
    end CargoInfoKeys;

    function ShipUpgradeKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Close upgrade menu
                null;
            when Character'Pos('1') | Character'Pos('2') | Character'Pos('3') => -- Give upgrade orders
                StartUpgrading(Get_Index(Current(ModulesMenu)), Positive(Key - 48));
            when others =>
                return Upgrade_Module;
        end case;
        DrawGame(Ship_Info);
        return Ship_Info;
    end ShipUpgradeKeys;
end Ships.UI;
