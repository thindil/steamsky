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
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Items; use Items;
with ShipModules; use ShipModules;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Crafts; use Crafts;
with Maps; use Maps;
with Bases; use Bases;

package body Ships.UI is

    ModulesMenu, OptionsMenu : Menu;
    MenuWindow, MenuWindow2 : Window;
    RenameForm : Form;
    FormWindow : Window;
    CurrentMenuIndex : Positive := 1;

    procedure ShowModuleInfo is
        InfoWindow : Window;
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        DamagePercent : Natural;
        MAmount : Natural := 0;
        CurrentLine : Line_Position := 5;
        MaxUpgrade, UpgradePercent : Natural;
        MaxValue : Positive;
    begin
        InfoWindow := Create(20, (Columns / 2), 8, (Columns / 2));
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
        MaxValue := Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Durability) * 1.5);
        if PlayerShip.Modules.Element(ModuleIndex).MaxDurability = MaxValue then
            Add(Win => InfoWindow, Str => " (max upgrade)");
        end if;
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => "Weight:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Weight) &
            " kg");
        Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
        Add(Win => InfoWindow, Str => "Repair/Upgrade material: ");
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
        Add(Win => InfoWindow, Str => "Repair/Upgrade skill: " &
            To_String(Skills_Names.Element(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).RepairSkill)));
        Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
        case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
            when ENGINE =>
                Add(Win => InfoWindow, Str => "Max power:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value));
                MaxValue := Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                    Add(Win => InfoWindow, Str => " (max upgrade)");
                end if;
                Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
                Add(Win => InfoWindow, Str => "Fuel usage:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Current_Value));
                MaxValue := Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Value) / 2.0);
                if PlayerShip.Modules.Element(ModuleIndex).Current_Value = MaxValue then
                    Add(Win => InfoWindow, Str => " (max upgrade)");
                end if;
                CurrentLine := CurrentLine + 1;
            when CARGO =>
                Add(Win => InfoWindow, Str => "Max cargo:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value) &
                    " kg");
            when HULL =>
                Add(Win => InfoWindow, Str => "Modules space:" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Current_Value) &
                    " /" & Integer'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value));
                MaxValue := Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                    Add(Win => InfoWindow, Str => " (max upgrade)");
                end if;
            when CABIN =>
                if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                    Add(Win => InfoWindow, Str => "Owner: " &
                        To_String(PlayerShip.Crew.Element(PlayerShip.Modules.Element(ModuleIndex).Owner).Name));
                else
                    Add(Win => InfoWindow, Str => "Owner: none");
                end if;
                Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
                Add(Win => InfoWindow, Str => "Quality: ");
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < 30 then
                    Add(Win => InfoWindow, Str => "minimal");
                elsif PlayerShip.Modules.Element(ModuleIndex).Max_Value > 29 and PlayerShip.Modules.Element(ModuleIndex).Max_Value < 60
                then
                    Add(Win => InfoWindow, Str => "basic");
                elsif PlayerShip.Modules.Element(ModuleIndex).Max_Value > 59 and PlayerShip.Modules.Element(ModuleIndex).Max_Value < 80
                then
                    Add(Win => InfoWindow, Str => "extended");
                else
                    Add(Win => InfoWindow, Str => "luxury");
                end if;
                MaxValue := Positive(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value = MaxValue then
                    Add(Win => InfoWindow, Str => " (max upgrade)");
                end if;
                CurrentLine := CurrentLine + 1;
            when GUN =>
                Add(Win => InfoWindow, Str => "Ammunition: " &  
                    To_String(Items_List.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).Name));
                Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
                if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                    Add(Win => InfoWindow, Str => "Gunner: " &
                        To_String(PlayerShip.Crew.Element(PlayerShip.Modules.Element(ModuleIndex).Owner).Name));
                else
                    Add(Win => InfoWindow, Str => "Gunner: none");
                end if;
                CurrentLine := CurrentLine + 1;
            when TURRET =>
                if PlayerShip.Modules.Element(ModuleIndex).Current_Value > 0 then
                    Add(Win => InfoWindow, Str => "Weapon: " &
                        To_String(PlayerShip.Modules.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).Name));
                else
                    Add(Win => InfoWindow, Str => "Weapon: none");
                end if;
            when ALCHEMY_LAB | FURNACE =>
                if PlayerShip.Modules.Element(ModuleIndex).Owner > 0 then
                    Add(Win => InfoWindow, Str => "Worker: " &
                        To_String(PlayerShip.Crew.Element(PlayerShip.Modules.Element(ModuleIndex).Owner).Name));
                else
                    Add(Win => InfoWindow, Str => "Worker: none");
                end if;
                Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
                if PlayerShip.Modules.Element(ModuleIndex).Current_Value > 0 then
                    Add(Win => InfoWIndow, Str => "Manufacturing: " &
                        To_String(Items_List.Element(Recipes_List.Element(PlayerShip.Modules.Element(ModuleIndex).Current_Value).ResultIndex).Name));
                else
                    Add(Win => InfoWindow, Str => "Manufacturing: nothing");
                end if;
                CurrentLine := CurrentLine + 1;
            when others =>
                CurrentLine := CurrentLine - 1;
        end case;
        if Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size > 0 then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => "Size:" & 
                Natural'Image(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size));
            CurrentLine := CurrentLine + 1;
        end if;
        if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /= NONE then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
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
                when VALUE =>
                    case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                        when ENGINE =>
                            Add(Win => InfoWindow, Str => "fuel usage");
                            MaxUpgrade := 100;
                        when others =>
                            null;
                    end case;
                when others =>
                    null;
            end case;
            CurrentLine := CurrentLine + 1;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
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
            CurrentLine := CurrentLine + 1;
        end if;
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Press Enter to see selected module options");
        Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 6, Count => 5, Color => 1);
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
        Change_Attributes(Line => 2, Column => 2, Count => 1, Color => 1);
        Move_Cursor(Line => 3, Column => 2);
        Add(Str => "Upgrading: ");
        if PlayerShip.UpgradeModule = 0 then
            Add(Str => "Nothing");
            CurrentLine := 4;
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
                when VALUE =>
                    case Modules_List.Element(PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex).MType is
                        when ENGINE =>
                            Add(Str => "(fuel usage)");
                            MaxUpgrade := 100;
                        when others =>
                            null;
                    end case;
                when others =>
                    null;
            end case;
            Move_Cursor(Line => 4, Column => 2);
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
            CurrentLine := 5;
        end if;
        Move_Cursor(Line => CurrentLine, Column => 2);
        Add(Str => "Destination: ");
        if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
            Add(Str => "None");
        else
            if SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY).BaseIndex > 0 then
                Add(Str => To_String(SkyBases(SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY).BaseIndex).Name));
            else
                Add(Str => "X:" & Positive'Image(PlayerShip.DestinationX) & " Y:" & Positive'Image(PlayerShip.DestinationY));
            end if;
        end if;
        CurrentLine := CurrentLine + 1;
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
        CurrentLine := CurrentLine + 2;
        MenuWindow := Create(MenuHeight, MenuLength, CurrentLine, 2);
        Set_Window(ModulesMenu, MenuWindow);
        Set_Sub_Window(ModulesMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(ModulesMenu);
        Set_Current(ModulesMenu, Modules_Items.all(CurrentMenuIndex));
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
        if Cargo_Items.all(CurrentMenuIndex) = Null_Item then
            CurrentMenuIndex := 1;
        end if;
        Set_Current(ModulesMenu, Cargo_Items.all(CurrentMenuIndex));
        ShowItemInfo;
        Refresh(MenuWindow);
        Move_Cursor(Line => MenuHeight + 4, Column => 2);
        FreeSpace := FreeCargo(0);
        if FreeSpace < 0 then
            FreeSpace := 0;
        end if;
        Add(Str => "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
    end ShowCargoInfo;

    procedure ShowShipForm(OptionText : String; MaxRange : Natural := 0) is
        Rename_Fields : constant Field_Array_Access := new Field_Array(1..5);
        FieldOptions : Field_Option_Set;
        FormHeight : Line_Position;
        FormLength : Column_Position;
        Visibility : Cursor_Visibility := Normal;
    begin
        if RenameForm = Null_Form then
            Set_Cursor_Visibility(Visibility);
            Rename_Fields.all(1) := New_Field(1, OptionText'Length, 0, 0, 0, 0);
            FieldOptions := Get_Options(Rename_Fields.all(1));
            Set_Buffer(Rename_Fields.all(1), 0, OptionText);
            FieldOptions.Active := False;
            Set_Options(Rename_Fields.all(1), FieldOptions);
            Rename_Fields.all(2) := New_Field(1, 20, 0, OptionText'Length, 0, 0);
            FieldOptions := Get_Options(Rename_Fields.all(2));
            FieldOptions.Auto_Skip := False;
            Set_Options(Rename_Fields.all(2), FieldOptions);
            Set_Background(Rename_Fields.all(2), (Reverse_Video => True, others => False));
            if MaxRange > 0 then
                Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type(Rename_Fields.all(2), (0, 0, MaxRange));
            end if;
            Rename_Fields.all(3) := New_Field(1, 8, 2, (OptionText'Length / 2), 0, 0);
            Set_Buffer(Rename_Fields.all(3), 0, "[Cancel]");
            FieldOptions := Get_Options(Rename_Fields.all(3));
            FieldOptions.Edit := False;
            Set_Options(Rename_Fields.all(3), FieldOptions);
            Rename_Fields.all(4) := New_Field(1, 4, 2, (OptionText'Length / 2) + 10, 0, 0);
            FieldOptions := Get_Options(Rename_Fields.all(4));
            FieldOptions.Edit := False;
            Set_Options(Rename_Fields.all(4), FieldOptions);
            Set_Buffer(Rename_Fields.all(4), 0, "[Ok]");
            Rename_Fields.all(5) := Null_Field;
            RenameForm := New_Form(Rename_Fields);
            Scale(RenameForm, FormHeight, FormLength);
            FormWindow := Create(FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)), ((Columns / 2) - (FormLength / 2)));
            Box(FormWindow);
            Set_Window(RenameForm, FormWindow);
            Set_Sub_Window(RenameForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
            Post(RenameForm);
        end if;
        Refresh;
        Refresh(FormWindow);
    end ShowShipForm;

    function RenameResult(CurrentState : GameStates) return GameStates is
        Visibility : Cursor_Visibility := Invisible;
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        FieldIndex : constant Positive := Get_Index(Current(RenameForm));
        NewName : Unbounded_String;
        SemicolonIndex : Natural;
    begin
        if FieldIndex < 3 then
            return CurrentState;
        elsif FieldIndex = 4 then
            NewName := Trim(To_Unbounded_String(Get_Buffer(Fields(RenameForm, 2))), Ada.Strings.Both);
            if Length(NewName) > 0 then
                SemicolonIndex := Index(NewName, ";");
                while SemicolonIndex > 0 loop
                    Delete(NewName, SemicolonIndex, SemicolonIndex);
                    SemicolonIndex := Index(NewName, ";");
                end loop;
                if CurrentState = Rename_Module then
                    UpdateModule(PlayerShip, ModuleIndex, "Name", To_String(NewName));
                else
                    PlayerShip.Name := NewName;
                end if;
            end if;
        end if;
        Set_Cursor_Visibility(Visibility);
        Post(RenameForm, False);
        Delete(RenameForm);
        DrawGame(Ship_Info);
        return Ship_Info;
    end RenameResult;

    function DropCargoResult return GameStates is
        ItemIndex : constant Positive := Get_Index(Current(ModulesMenu));
        ItemName : constant String := To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name);
        DropAmount : Natural;
        Visibility : Cursor_Visibility := Invisible;
        FieldIndex : constant Positive := Get_Index(Current(RenameForm));
    begin
        if FieldIndex < 3 then
            return Drop_Cargo;
        elsif FieldIndex = 4 then
            DropAmount := Natural'Value(Get_Buffer(Fields(RenameForm, 2)));
            UpdateCargo(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex, (0 - DropAmount));
            AddMessage("You dropped" & Positive'Image(DropAmount) & " " & ItemName, OtherMessage);
        end if;
        Set_Cursor_Visibility(Visibility);
        Post(RenameForm, False);
        Delete(RenameForm);
        DrawGame(Cargo_Info);
        return Cargo_Info;
    exception
        when CONSTRAINT_ERROR =>
            Set_Cursor_Visibility(Visibility);
            Post(RenameForm, False);
            Delete(RenameForm);
            DrawGame(Cargo_Info);
            return Cargo_Info;
    end DropCargoResult;

    procedure ShowModuleOptions is
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        Options_Items : constant Item_Array_Access := new Item_Array(1..9);
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MaxValue : Positive;
        MenuIndex : Positive := 1;
    begin
        MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Durability) * 1.5);
        if PlayerShip.Modules.Element(ModuleIndex).MaxDurability < MaxValue then
            Options_Items.all(MenuIndex) := New_Item("Upgrade durability", "1");
            MenuIndex := MenuIndex + 1;
        end if;
        case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
            when ENGINE =>
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    Options_Items.all(MenuIndex) := New_Item("Upgrade engine power", "2");
                    MenuIndex := MenuIndex + 1;
                end if;
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Value) / 2.0);
                if PlayerShip.Modules.Element(ModuleIndex).Current_Value > MaxValue then
                    Options_Items.all(MenuIndex) := New_Item("Reduce fuel usage", "3");
                    MenuIndex := MenuIndex + 1;
                end if;
            when CABIN =>
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    Options_Items.all(MenuIndex) := New_Item("Upgrade quality", "2");
                    MenuIndex := MenuIndex + 1;
                end if;
                Options_Items.all(MenuIndex) := New_Item("Assign owner", "7");
                MenuIndex := MenuIndex + 1;
            when GUN =>
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    Options_Items.all(MenuIndex) := New_Item("Upgrade damage", "2");
                    MenuIndex := MenuIndex + 1;
                end if;
                Options_Items.all(MenuIndex) := New_Item("Assign gunner", "7");
                MenuIndex := MenuIndex + 1;
            when BATTERING_RAM =>
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    Options_Items.all(MenuIndex) := New_Item("Upgrade damage", "2");
                    MenuIndex := MenuIndex + 1;
                end if;
            when HULL =>
                MaxValue := Natural(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MaxValue) * 1.5);
                if PlayerShip.Modules.Element(ModuleIndex).Max_Value < MaxValue then
                    Options_Items.all(MenuIndex) := New_Item("Enlarge hull", "2");
                    MenuIndex := MenuIndex + 1;
                end if;
            when ALCHEMY_LAB | FURNACE =>
                if PlayerShip.Modules.Element(ModuleIndex).Current_Value > 0 then
                    Options_Items.all(MenuIndex) := New_Item("Assign worker", "7");
                    MenuIndex := MenuIndex + 1;
                end if;
            when others =>
                null;
        end case;
        if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /= NONE then
            Options_Items.all(MenuIndex) := New_Item("Continue upgrade", "4");
            MenuIndex := MenuIndex + 1;
        end if;
        if PlayerShip.UpgradeModule > 0 then
            Options_Items.all(MenuIndex) := New_Item("Stop upgrading", "8");
            MenuIndex := MenuIndex + 1;
        end if;
        Options_Items.all(MenuIndex) := New_Item("Rename", "5");
        MenuIndex := MenuIndex + 1;
        Options_Items.all(MenuIndex) := New_Item("Quit", "6");
        MenuIndex := MenuIndex + 1;
        for I in MenuIndex..Options_Items'Last loop
            Options_Items.all(I) := Null_Item;
        end loop;
        OptionsMenu := New_Menu(Options_Items);
        Set_Mark(OptionsMenu, "");
        Set_Options(OptionsMenu, (Show_Descriptions => False, others => True));
        Scale(OptionsMenu, MenuHeight, MenuLength);
        MenuWindow2 := Create(MenuHeight + 2, MenuLength + 2, ((Lines / 3) - (MenuHeight / 2)), ((Columns / 2) - (MenuLength / 2)));
        Box(MenuWindow2);
        Set_Window(OptionsMenu, MenuWindow2);
        Set_Sub_Window(OptionsMenu, Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
        Post(OptionsMenu);
        Refresh;
        Refresh(MenuWindow2);
    end ShowModuleOptions;

    procedure ShowAssignMenu is
        ModuleIndex : constant Positive := Get_Index(Current(ModulesMenu));
        Assign_Items : constant Item_Array_Access := new Item_Array(1..(PlayerShip.Crew.Last_Index + 2));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuIndex : Positive := 1;
    begin   
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Modules.Element(ModuleIndex).Owner /= I then
                Assign_Items.all(MenuIndex) := New_Item("Assign " & To_String(PlayerShip.Crew.Element(I).Name), Positive'Image(I));
                MenuIndex := MenuIndex + 1;
            end if;
        end loop;
        Assign_Items.all(MenuIndex) := New_Item("Quit", "0");
        MenuIndex := MenuIndex + 1;
        for I in MenuIndex..Assign_Items'Last loop
            Assign_Items.all(I) := Null_Item;
        end loop;
        OptionsMenu := New_Menu(Assign_Items);
        Set_Options(OptionsMenu, (Show_Descriptions => False, others => True));
        Set_Mark(OptionsMenu, "");
        Scale(OptionsMenu, MenuHeight, MenuLength);
        MenuWindow2 := Create(MenuHeight + 2, MenuLength + 2, ((Lines / 3) - (MenuHeight / 2)), ((Columns / 2) - (MenuLength / 2)));
        Box(MenuWindow2);
        Set_Window(OptionsMenu, MenuWindow2);
        Set_Sub_Window(OptionsMenu, Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
        Post(OptionsMenu);
        Refresh;
        Refresh(MenuWindow2);
    end ShowAssignMenu;

    function ShipInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Menus.Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map or combat screen
                CurrentMenuIndex := 1;
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
            when 10 => -- Show module options menu
                ShowModuleOptions;
                return Module_Options;
            when Character'Pos('n') | Character'Pos('N') => -- Rename ship
                ShowShipForm("New name for ship:");
                return Rename_Ship;
            when others =>
                null;
        end case;
        CurrentMenuIndex := Get_Index(Current(ModulesMenu));
        return Ship_Info;
    end ShipInfoKeys;

    function CargoInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Menus.Driver_Result;
        ItemName : constant String := To_String(Items_List.Element(PlayerShip.Cargo.Element(CurrentMenuIndex).ProtoIndex).Name);
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back sky map or combat screen
                CurrentMenuIndex := 1;
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
                ShowShipForm("Amount of " & ItemName & " to drop:", PlayerShip.Cargo.Element(CurrentMenuIndex).Amount);
                return Drop_Cargo;
            when others =>
                null;
        end case;
        CurrentMenuIndex := Get_Index(Current(ModulesMenu));
        return Cargo_Info;
    end CargoInfoKeys;

    function ModuleOptionsKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
        OptionIndex : constant Positive := Positive'Value(Description(Current(OptionsMenu)));
        ModuleName : constant String := To_String(PlayerShip.Modules.Element(CurrentMenuIndex).Name);
    begin
        case Key is
            when 56 | KEY_UP => -- Select previous item
                Result := Driver(OptionsMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(OptionsMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 50 | KEY_DOWN => -- Select next item
                Result := Driver(OptionsMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(OptionsMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 10 => -- Select option from menu
                Post(OptionsMenu, False);
                if OptionIndex /= 5 and OptionIndex /= 7 then
                    if OptionIndex < 5 then
                        StartUpgrading(CurrentMenuIndex, OptionIndex);
                    elsif OptionIndex = 8 then
                        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
                            if PlayerShip.Crew.Element(I).Order = Upgrading then
                                GiveOrders(I, Rest);
                                exit;
                            end if;
                        end loop;
                        PlayerShip.UpgradeModule := 0;
                        AddMessage("You stopped current upgrade.", OrderMessage);
                    end if;
                    DrawGame(Ship_Info);
                    return Ship_Info;
                elsif OptionIndex = 5 then
                    DrawGame(Ship_Info);
                    ShowShipForm("New name for " & ModuleName & ":");
                    return Rename_Module;
                elsif OptionIndex = 7 then
                    DrawGame(Ship_Info);
                    ShowAssignMenu;
                    return Assign_Owner;
                end if;
            when others =>
                null;
        end case;
        return Module_Options;
    end ModuleOptionsKeys;

    function ShipFormKeys(Key : Key_Code; CurrentState : GameStates) return GameStates is
        Result : Forms.Driver_Result;
        FieldIndex : Positive := Get_Index(Current(RenameForm));
    begin
        case Key is
            when KEY_UP => -- Select previous field
                Result := Driver(RenameForm, F_Previous_Field);
                FieldIndex := Get_Index(Current(RenameForm));
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_End_Line);
                end if;
            when KEY_DOWN => -- Select next field
                Result := Driver(RenameForm, F_Next_Field);
                FieldIndex := Get_Index(Current(RenameForm));
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_End_Line);
                end if;
            when 10 => -- quit/rename module/drop cargo
                if CurrentState = Drop_Cargo then
                    return DropCargoResult;
                else
                    return RenameResult(CurrentState);
                end if;
            when KEY_BACKSPACE => -- delete last character
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Delete_Previous);
                    if Result = Form_Ok then
                        FieldIndex := Get_Index(Current(RenameForm));
                        if FieldIndex /= 2 then
                            Set_Current(RenameForm, Fields(RenameForm, 2));
                        end if;
                    end if;
                end if;
            when KEY_DC => -- delete character at cursor
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Delete_Char);
                end if;
            when KEY_RIGHT => -- Move cursor right
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Right_Char);
                end if;
            when KEY_LEFT => -- Move cursor left
                if FieldIndex = 2 then
                    Result := Driver(RenameForm, F_Left_Char);
                end if;
            when others =>
                Result := Driver(RenameForm, Key);
        end case;
        if Result = Form_Ok then
            if FieldIndex = 2 then
                Set_Background(Current(RenameForm), (Reverse_Video => True, others => False));
            else
                Set_Background(Fields(RenameForm, 2), (others => False));
            end if;
            Refresh(FormWindow);
        end if;
        return CurrentState;
    end ShipFormKeys;

    function AssignOwnerKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
        OptionIndex : constant Natural := Positive'Value(Description(Current(OptionsMenu)));
        ModuleName : constant String := To_String(PlayerShip.Modules.Element(CurrentMenuIndex).Name);
    begin
        case Key is
            when 56 | KEY_UP => -- Select previous item
                Result := Driver(OptionsMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(OptionsMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 50 | KEY_DOWN => -- Select next item
                Result := Driver(OptionsMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(OptionsMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 10 => -- Select new module owner
                Post(OptionsMenu, False);
                if OptionIndex /= 0 then
                    case Modules_List.Element(PlayerShip.Modules.Element(CurrentMenuIndex).ProtoIndex).MType is
                        when CABIN =>
                            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                                if PlayerShip.Modules.Element(I).Owner = OptionIndex and
                                    Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = CABIN
                                then
                                    UpdateModule(PlayerShip, I, "Owner", "0");
                                end if;
                            end loop;
                            UpdateModule(PlayerShip, CurrentMenuIndex, "Owner", Positive'Image(OptionIndex));
                            AddMessage("You assigned " & ModuleName & " to " & To_String(PlayerShip.Crew.Element(OptionIndex).Name)
                                & ".", OrderMessage);
                        when GUN =>
                            GiveOrders(OptionIndex, Gunner, CurrentMenuIndex);
                        when ALCHEMY_LAB | FURNACE =>
                            GiveOrders(OptionIndex, Craft, CurrentMenuIndex);
                        when others =>
                            null;
                    end case;
                end if;
                DrawGame(Ship_Info);
                return Ship_Info;
            when others =>
                null;
        end case;
        return Assign_Owner;
    end AssignOwnerKeys;

end Ships.UI;
