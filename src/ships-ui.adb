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
    begin
        InfoWindow := Create(5, (Columns / 2), 8, (Columns / 2));
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
                Add(Win => InfoWindow, Str => "Owner: " &
                    To_String(PlayerShip.Crew.Element(PlayerShip.Modules.Element(ModuleIndex).Owner).Name));
            when GUN =>
                Add(Win => InfoWindow, Str => "Ammunition: " &  
                    To_String(Items_List.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).Name));
            when TURRET =>
                Add(Win => InfoWindow, Str => "Weapon: " &
                    To_String(PlayerShip.Modules.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).Name));
            when others =>
                null;
        end case;
        Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
        Add(Win => InfoWindow, Str => "Rename module");
        Change_Attributes(Win => InfoWindow, Line => 4, Column => 2, 
            Count => 1, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowModuleInfo;

    procedure ShowShipInfo is
        Weight : Integer;
        Modules_Items: constant Item_Array_Access := new Item_Array(1..(PlayerShip.Modules.Last_Index + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
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
        Add(Str => "Weight:" & Integer'Image(Weight) & "kg");
        Move_Cursor(Line => 6, Column => 2);
        Add(Str => "Modules:");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            Modules_Items.all(I) := New_Item(To_String(PlayerShip.Modules.Element(I).Name));
        end loop;
        Modules_Items.all(Modules_Items'Last) := Null_Item;
        ModulesMenu := New_Menu(Modules_Items);
        Set_Format(ModulesMenu, Lines - 10, 1);
        Set_Mark(ModulesMenu, "");
        Scale(ModulesMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 8, 2);
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
        Add(Str => "Free cargo space:" & Integer'Image(FreeCargo(0)) & " kg");
    end ShowCargoInfo;

    procedure ShowModuleForm is
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        Visibility : Cursor_Visibility := Normal;
        ModuleName : String(1..20);
        NewName : Unbounded_String;
        SemicolonIndex : Natural;
    begin
        Move_Cursor(Line => 15, Column => (Columns / 2));
        Add(Str => "New name: ");
        Set_Echo_Mode(True);
        Set_Cursor_Visibility(Visibility);
        Get(Str => ModuleName, Len => 20);
        NewName := Trim(To_Unbounded_String(ModuleName), Ada.Strings.Both);
        if Length(NewName) > 0 then
            SemicolonIndex := Index(NewName, ";");
            while SemicolonIndex > 0 loop
                Delete(NewName, SemicolonIndex, SemicolonIndex);
                SemicolonIndex := Index(NewName, ";");
            end loop;
            UpdateModule(PlayerShip, ModuleIndex, "Name", To_String(NewName));
        end if;
        Visibility := Invisible;
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);
        DrawGame(Ship_Info);
    end ShowModuleForm;

    procedure ShowCargoForm is
        ItemIndex : constant Positive := Get_Index(Current(ModulesMenu));
        Visibility : Cursor_Visibility := Normal;
        ItemName : constant String := To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name);
        Amount : String(1..6);
        DropAmount : Natural;
    begin
        Move_Cursor(Line => 12, Column => (Columns / 2));
        Add(Str => "Amount of " & ItemName & " to drop: ");
        Set_Echo_Mode(True);
        Set_Cursor_Visibility(Visibility);
        Get(Str => Amount, Len => 6);
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

    function ShipInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Driver_Result;
        NewKey : Key_Code;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map or combat screen
                DrawGame(OldState);
                return OldState;
            when 56 => -- Select previous module
                Result := Driver(ModulesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 => -- Select next module
                Result := Driver(ModulesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                end if;
            when 27 => 
                NewKey := Get_KeyStroke;
                if NewKey = 91 then
                    NewKey := Get_KeyStroke;
                    if NewKey = 65 then -- Select previous module
                        Result := Driver(ModulesMenu, M_Up_Item);
                        if Result = Request_Denied then
                            Result := Driver(ModulesMenu, M_Last_Item);
                        end if;
                        if Result = Menu_Ok then
                            ShowModuleInfo;
                            Refresh(MenuWindow);
                        end if;
                    elsif NewKey = 66 then -- Select next module
                        Result := Driver(ModulesMenu, M_Down_Item);
                        if Result = Request_Denied then
                            Result := Driver(ModulesMenu, M_First_Item);
                        end if;
                        if Result = Menu_Ok then
                            ShowModuleInfo;
                            Refresh(MenuWindow);
                        end if;
                    end if;
                end if;
            when Character'Pos('n') | Character'Pos('N') => -- Rename selected module
                ShowModuleForm;
            when others =>
                null;
        end case;
        return Ship_Info;
    end ShipInfoKeys;

    function CargoInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Driver_Result;
        NewKey : Key_Code;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back sky map or combat screen
                DrawGame(OldState);
                return OldState;
            when 56 => -- Select previous item
                Result := Driver(ModulesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 => -- Select next item
                Result := Driver(ModulesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(ModulesMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 27 => 
                NewKey := Get_KeyStroke;
                if NewKey = 91 then
                    NewKey := Get_KeyStroke;
                    if NewKey = 65 then -- Select previous item
                        Result := Driver(ModulesMenu, M_Up_Item);
                        if Result = Request_Denied then
                            Result := Driver(ModulesMenu, M_Last_Item);
                        end if;
                        if Result = Menu_Ok then
                            ShowItemInfo;
                            Refresh(MenuWindow);
                        end if;
                    elsif NewKey = 66 then -- Select next item
                        Result := Driver(ModulesMenu, M_Down_Item);
                        if Result = Request_Denied then
                            Result := Driver(ModulesMenu, M_First_Item);
                        end if;
                        if Result = Menu_Ok then
                            ShowItemInfo;
                            Refresh(MenuWindow);
                        end if;
                    end if;
                end if;
            when Character'Pos('d') | Character'Pos('D') => -- Drop selected cargo
                ShowCargoForm;
            when others =>
                null;
        end case;
        return Cargo_Info;
    end CargoInfoKeys;

end Ships.UI;
