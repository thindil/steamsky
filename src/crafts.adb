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
with UserInterface; use UserInterface;
with Messages; use Messages;
with Ships; use Ships;

package body Crafts is

    RecipeIndex : Natural;


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
            ResultIndex => 1, ResultAmount => 10000, Workplace => ALCHEMY_LAB);
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
                end if;
            elsif TempRecord.ResultAmount < 10000 then
                Recipes_List.Append(New_Item => TempRecord);
                TempRecord := (MaterialTypes => TempMaterials, MaterialAmounts => TempAmount,
                    ResultIndex => 1, ResultAmount => 10000, Workplace => ALCHEMY_LAB);
            end if;
        end loop;
        Close(RecipesFile);
        return True;
    end LoadRecipes;

    procedure SetRecipe is
        ModuleIndex : Natural := 0;
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
        if FreeCargo(SpaceNeeded - 
            (Items_List.Element(Recipes_List.Element(RecipeIndex).ResultIndex).Weight * 
            Recipes_List.Element(RecipeIndex).ResultAmount)) < 0 then
            ShowDialog("You don't have that much free space in your ship cargo.");
            return;
        end if;
        PlayerShip.Craft := RecipeIndex;
        AddMessage(To_String(Items_List.Element(Recipe.ResultIndex).Name) & " was set as manufacturing order.", CraftMessage);
        RecipeIndex := 0;
    end SetRecipe;

    procedure ShowCraft(Key : Key_Code) is
        MAmount : Natural := 0;
        Recipe : Craft_Data;
        CurrentLine : Line_Position := 6;
    begin
        if Key /= KEY_NONE then
            Erase;
            Refresh;
            ShowGameHeader(Craft_View);
        end if;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Recipes");
        for I in Recipes_List.First_Index..Recipes_List.Last_Index loop
            Move_Cursor(Line => Line_Position(2 + I), Column => 2);
            Add(Str => Character'Val(96 + I) & " " & To_String(Items_List.Element(Recipes_List.Element(I).ResultIndex).Name));
            Change_Attributes(Line => Line_Position(2 + I), Column => 2, Count => 1, Color => 1);
        end loop;
        if Key /= KEY_NONE then -- Show info about selected recipe
            if (Key >= Key_Code(96 + Recipes_List.First_Index)) and (Key <= Key_Code(96 + Recipes_List.Last_Index)) then
                RecipeIndex := Integer(Key) - 96;
                Recipe := Recipes_List.Element(RecipeIndex);
                Move_Cursor(Line => 3, Column => (Columns / 2));
                Add(Str => "Name: " & To_String(Items_List.Element(Recipe.ResultIndex).Name));
                Move_Cursor(Line => 4, Column => (Columns / 2));
                Add(Str => "Amount:" & Integer'Image(Recipe.ResultAmount));
                Move_Cursor(Line => 5, Column => (Columns / 2));
                Add(Str => "Materials needed: ");
                for I in Recipe.MaterialTypes.First_Index..Recipe.MaterialTypes.Last_Index loop
                    Move_Cursor(Line => CurrentLine, Column => (Columns / 2) + 2);
                    Add(Str => "-");
                    for J in Items_List.First_Index..Items_List.Last_Index loop
                        if Items_List.Element(J).IType = Recipe.MaterialTypes(I) then
                            if MAmount > 0 then
                                Add(Str => " or");
                            end if;
                            Add(Str => Integer'Image(Recipe.MaterialAmounts(I)) & "x" & To_String(Items_List.Element(J).Name));
                            MAmount := MAmount + 1;
                        end if;
                    end loop;
                    CurrentLine := CurrentLine + 1;
                end loop;
                Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
                Add(Str => "Workplace: ");
                case Recipes_List.Element(RecipeIndex).Workplace is
                    when ALCHEMY_LAB =>
                        Add(Str => "Alchemy lab");
                    when others =>
                        null;
                end case;
                Move_Cursor(Line => (CurrentLine + 2), Column => (Columns / 2));
                Add(Str => "SPACE for set manufacturing order");
                Change_Attributes(Line => (CurrentLine + 2), Column => (Columns / 2), Count => 5, Color => 1);
            end if;
        end if;
    end ShowCraft;

    function CraftKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when Character'Pos(' ') => -- Set selected manufacturing order
                if RecipeIndex > 0 then
                    SetRecipe;
                    DrawGame(Craft_View);
                end if;
                return Craft_View;
            when others =>
                ShowCraft(Key);
                return Craft_View;
        end case;
    end CraftKeys;

end Crafts;
