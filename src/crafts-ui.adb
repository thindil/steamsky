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
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;
with Ships; use Ships;

package body Crafts.UI is

    RecipesMenu : Menu;
    MenuWindow : Window;

    procedure ShowRecipeInfo is
        InfoWindow : Window;
        Recipe : constant Craft_Data := Recipes_List.Element(Get_Index(Current(RecipesMenu)));
        CurrentLine : Line_Position := 3;
        MAmount, TextLength : Natural := 0;
        HaveMaterial, HaveWorkplace : Boolean := False;
        StartLine : Line_Position;
        StartColumn, EndColumn : Column_Position;
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
                    Get_Cursor_Position(Win => InfoWindow, Line => StartLine, Column => StartColumn);
                    Add(Win => InfoWindow, Str => Integer'Image(Recipe.MaterialAmounts(I)) & "x" & To_String(Items_List.Element(J).Name));
                    Get_Cursor_Position(Win => InfoWindow, Line => CurrentLine, Column => EndColumn);
                    for K in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                        if Items_List.Element(PlayerShip.Cargo.Element(K).ProtoIndex).IType = Recipe.MaterialTypes(I) then
                            HaveMaterial := True;
                            exit;
                        end if;
                    end loop;
                    if not HaveMaterial then
                        if StartLine = CurrentLine then
                            TextLength := Natural(EndColumn - StartColumn);
                            Change_Attributes(Win => InfoWindow, Line => StartLine,
                                Column => StartColumn, Count => Integer(StartColumn) + TextLength, Color => 3);
                        else
                            TextLength := Natural((Columns / 2) - StartColumn);
                            Change_Attributes(Win => InfoWindow, Line => StartLine,
                                Column => StartColumn, Count => Integer(StartColumn) + TextLength, Color => 3);
                            Change_Attributes(Win => InfoWindow, Line => CurrentLine,
                                Column => 0, Count => Integer(EndColumn), Color => 3);
                        end if;
                        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => EndColumn);
                    end if;
                    HaveMaterial := False;
                    MAmount := MAmount + 1;
                end if;
            end loop;
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
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Skill: " & To_String(Skills_Names.Element(Recipe.Skill)));
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
                SetRecipe(Get_Index(Current(RecipesMenu)));
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

end Crafts.UI;
