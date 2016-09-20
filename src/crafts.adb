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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Ships; use Ships;

package body Crafts is

    RecipesMenu : Menu;
    MenuWindow : Window;

    function LoadRecipes return Boolean is
        RecipesFile : File_Type;
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex, StartIndex, EndIndex, Amount : Natural;
        TempRecord : Craft_Data;
        TempMaterials : MaterialTypes_Container.Vector;
        TempAmount : MaterialAmounts_Container.Vector;
    begin
        if Recipes_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/recipes.dat") then
            return False;
        end if;
        TempRecord := (MaterialTypes => TempMaterials, MaterialAmounts => TempAmount,
            ResultIndex => 1, ResultAmount => 10000, Workplace => ALCHEMY_LAB,
            Skill => 1);
        Open(RecipesFile, In_File, "data/recipes.dat");
        while not End_Of_File(RecipesFile) loop
            RawData := To_Unbounded_String(Get_Line(RecipesFile));
            if Element(RawData, 1) /= '[' then
                EqualIndex := Index(RawData, "=");
                FieldName := Head(RawData, EqualIndex - 2);
                Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
                if FieldName = To_Unbounded_String("Material") then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        TempRecord.MaterialTypes.Append(New_Item => Items_Types'Value(Slice(Value, StartIndex, EndIndex - 1)));
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("Amount") then
                    StartIndex := 1;
                    Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                    for I in 1..Amount loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := Length(Value) + 1;
                        end if;
                        TempRecord.MaterialAmounts.Append(New_Item => Integer'Value(Slice(Value, StartIndex, EndIndex - 1)));
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("Result") then
                    TempRecord.ResultIndex := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Crafted") then
                    TempRecord.ResultAmount := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Workplace") then
                    TempRecord.Workplace := ModuleType'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Skill") then
                    if Value = "Alchemy" then
                        TempRecord.Skill := 5;
                    elsif Value = "Cooking" then
                        TempRecord.Skill := 6;
                    elsif Value = "Gunsmith" then
                        TempRecord.Skill := 7;
                    end if;
                end if;
            elsif TempRecord.ResultAmount < 10000 then
                Recipes_List.Append(New_Item => TempRecord);
                TempRecord := (MaterialTypes => TempMaterials, MaterialAmounts => TempAmount,
                    ResultIndex => 1, ResultAmount => 10000, Workplace => ALCHEMY_LAB, Skill => 1);
            end if;
        end loop;
        Close(RecipesFile);
        return True;
    end LoadRecipes;

    procedure SetRecipe is
        ModuleIndex : Natural := 0;
        RecipeIndex : constant Positive := Get_Index(Current(RecipesMenu));
        Recipe : constant Craft_Data := Recipes_List.Element(RecipeIndex);
        SpaceNeeded : Integer := 0;
        MaterialIndexes : array (Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index) of
            Natural := (others => 0);
    begin
        -- Check for materials
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            for J in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = Recipe.MaterialTypes(J) and
                    PlayerShip.Cargo.Element(I).Amount >= Recipe.MaterialAmounts(J) then
                    MaterialIndexes(J) := I;
                end if;
            end loop;
        end loop;
        for I in MaterialIndexes'Range loop
            if MaterialIndexes(I) = 0 then
                ShowDialog("You don't have enough materials to start manufacturing " & 
                To_String(Items_List.Element(Recipe.ResultIndex).Name) & ".");
                return;
            end if;
        end loop;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).Mtype = Recipe.Workplace and
                PlayerShip.Modules.Element(I).Durability > 0 then
                ModuleIndex := I;
                exit;
            end if;
        end loop;
        if ModuleIndex = 0 then
            ShowDialog("You don't have workplace for manufacture " & 
                To_String(Items_List.Element(Recipe.ResultIndex).Name) & ".");
            return;
        end if;
        for I in MaterialIndexes'Range loop
            SpaceNeeded := SpaceNeeded + Items_List.Element(MaterialIndexes(I)).Weight * Recipe.MaterialAmounts.Element(I);
        end loop;
        if FreeCargo(SpaceNeeded - (Items_List.Element(Recipe.ResultIndex).Weight * Recipe.ResultAmount)) < 0 then
            ShowDialog("You don't have that much free space in your ship cargo.");
            return;
        end if;
        PlayerShip.Craft := RecipeIndex;
        AddMessage(To_String(Items_List.Element(Recipe.ResultIndex).Name) & " was set as manufacturing order.", CraftMessage);
    end SetRecipe;

    procedure ShowRecipeInfo is
        InfoWindow : Window;
        Recipe : constant Craft_Data := Recipes_List.Element(Get_Index(Current(RecipesMenu)));
        CurrentLine : Line_Position := 3;
        MAmount : Natural := 0;
        HaveMaterial, HaveWorkplace : Boolean := False;
        CursorLine : Line_Position;
        CursorColumn : Column_Position;
        WorkplaceName : Unbounded_String := Null_Unbounded_String;
    begin
        InfoWindow := Create((Lines - 5), (Columns / 2), 3, (Columns / 2));
        Add(Win => InfoWindow, Str => "Name: " & To_String(Items_List.Element(Recipe.ResultIndex).Name));
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => "Amount:" & Integer'Image(Recipe.ResultAmount));
        Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
        Add(Win => InfoWindow, Str => "Materials needed: ");
        for I in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
            Add(Win => InfoWindow, Str => "-");
            MAmount := 0;
            for J in Items_List.First_Index..Items_List.Last_Index loop
                if Items_List.Element(J).IType = Recipe.MaterialTypes(I) then
                    if MAmount > 0 then
                        Add(Win => InfoWindow, Str => " or");
                    end if;
                    Add(Win => InfoWindow, Str => Integer'Image(Recipe.MaterialAmounts(I)) & "x" & To_String(Items_List.Element(J).Name));
                    MAmount := MAmount + 1;
                end if;
            end loop;
            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if Items_List.Element(PlayerShip.Cargo.Element(J).ProtoIndex).IType = Recipe.MaterialTypes(I) then
                    HaveMaterial := True;
                    exit;
                end if;
            end loop;
            if not HaveMaterial then
                Get_Cursor_Position(Win => InfoWindow, Line => CursorLine,
                    Column => CursorColumn);
                Change_Attributes(Win => InfoWindow, Line => CurrentLine,
                    Column => 3, Count => Integer(CursorColumn - 3), Color => 3);
            end if;
            HaveMaterial := False;
            CurrentLine := CurrentLine + 1;
        end loop;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Workplace: ");
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = Recipe.Workplace then
                if PlayerShip.Modules.Element(I).Durability > 0 then
                    HaveWorkplace := True;
                end if;
                WorkplaceName := PlayerShip.Modules.Element(I).Name;
                exit;
            end if;
        end loop;
        if WorkplaceName = Null_Unbounded_String then
            for I in Modules_List.First_Index..Modules_List.Last_Index loop
                if Modules_List.Element(I).MType = Recipe.Workplace then
                    WorkplaceName := Modules_List.Element(I).Name;
                    exit;
                end if;
            end loop;
        end if;
        Add(Win => InfoWindow, Str => To_String(WorkplaceName));
        if not HaveWorkplace then
            Change_Attributes(Win => InfoWindow, Line => CurrentLine,
                Column => 11, Count => Length(WorkplaceName), Color => 3);
        end if;
        Move_Cursor(Win => InfoWindow, Line => (CurrentLine + 2), Column => 0);
        Add(Win => InfoWindow, Str => "SPACE for set manufacturing order");
        Change_Attributes(Win => InfoWindow, Line => (CurrentLine + 2), Column => 0, Count => 5, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowRecipeInfo;

    procedure ShowRecipes is
        Recipes_Items: constant Item_Array_Access := new Item_Array(1..(Recipes_List.Last_Index + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
    begin
        Move_Cursor(Line => 3, Column => 2);
        for I in Recipes_List.First_Index..Recipes_List.Last_Index loop
            Recipes_Items.all(I) := New_Item(To_String(Items_List.Element(Recipes_List.Element(I).ResultIndex).Name));
        end loop;
        Recipes_Items.all(Recipes_Items'Last) := Null_Item;
        RecipesMenu := New_Menu(Recipes_Items);
        Set_Format(RecipesMenu, Lines - 10, 1);
        Set_Mark(RecipesMenu, "");
        Scale(RecipesMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(RecipesMenu, MenuWindow);
        Set_Sub_Window(RecipesMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(RecipesMenu);
        ShowRecipeInfo;
        Refresh(MenuWindow);
    end ShowRecipes;
    
    function CraftKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos(' ') => -- Set selected manufacturing order
                SetRecipe;
                DrawGame(Craft_View);
            when 56 | KEY_UP => -- Select previous recipe
                Result := Driver(RecipesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(RecipesMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowRecipeInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next recipe
                Result := Driver(RecipesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(RecipesMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowRecipeInfo;
                    Refresh(MenuWindow);
                end if;
            when others =>
                null;
        end case;
        return Craft_View;
    end CraftKeys;

end Crafts;
