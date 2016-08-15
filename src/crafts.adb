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
with UserInterface; use UserInterface;
with Messages; use Messages;
with Ships; use Ships;

package body Crafts is

    RecipeIndex : Natural;

    procedure SetRecipe is
        MaterialIndex, ModuleIndex : Natural := 0;
    begin
        -- Check for materials
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex).IType = Recipes(RecipeIndex).MaterialType and
                PlayerShip.Cargo.Element(I).Amount >= Recipes(RecipeIndex).MaterialAmount then
                MaterialIndex := I;
                exit;
            end if;
        end loop;
        if MaterialIndex = 0 then
            ShowDialog("You don't have enough materials to start manufacturing " & 
                To_String(Items_List.Element(Recipes(RecipeIndex).ResultIndex).Name) & ".");
            return;
        end if;
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).Mtype = Recipes(RecipeIndex).Workplace and
                PlayerShip.Modules.Element(I).Durability > 0 then
                ModuleIndex := I;
                exit;
            end if;
        end loop;
        if ModuleIndex = 0 then
            ShowDialog("You don't have workplace for manufacture " & 
                To_String(Items_List.Element(Recipes(RecipeIndex).ResultIndex).Name) & ".");
            return;
        end if;
        if FreeCargo((Items_List.Element(MaterialIndex).Weight * Recipes(RecipeIndex).MaterialAmount) - 
            (Items_List.Element(Recipes(RecipeIndex).ResultIndex).Weight * Recipes(RecipeIndex).ResultAmount)) < 0 then
            ShowDialog("You don't have that much free space in your ship cargo.");
            return;
        end if;
        PlayerShip.Craft := RecipeIndex;
        AddMessage(To_String(Items_List.Element(Recipes(RecipeIndex).ResultIndex).Name) & " was set as manufacturing order.");
        RecipeIndex := 0;
    end SetRecipe;

    procedure ShowCraft(Key : Key_Code) is
        MAmount : Natural := 0;
    begin
        if Key /= KEY_NONE then
            Erase;
            Refresh;
            ShowGameMenu(Craft_View);
        end if;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "Recipes");
        for I in Recipes'Range loop
            Move_Cursor(Line => Line_Position(2 + I), Column => 2);
            Add(Str => Character'Val(96 + I) & " " & To_String(Items_List.Element(Recipes(I).ResultIndex).Name));
            Change_Attributes(Line => Line_Position(2 + I), Column => 2, Count => 1, Color => 1);
        end loop;
        if Key /= KEY_NONE then -- Show info about selected recipe
            if (Key >= Key_Code(96 + Recipes'First)) and (Key <= Key_Code(96 + Recipes'Last)) then
                RecipeIndex := Integer(Key) - 96;
                Move_Cursor(Line => 3, Column => (Columns / 2));
                Add(Str => "Name: " & To_String(Items_List.Element(Recipes(RecipeIndex).ResultIndex).Name));
                Move_Cursor(Line => 4, Column => (Columns / 2));
                Add(Str => "Amount:" & Integer'Image(Recipes(RecipeIndex).ResultAmount));
                Move_Cursor(Line => 5, Column => (Columns / 2));
                Add(Str => "Material needed: ");
                for I in Items_List.First_Index..Items_List.Last_Index loop
                    if Items_List.Element(I).IType = Recipes(RecipeIndex).MaterialType then
                        if MAmount > 0 then
                            Add(Str => " or ");
                        end if;
                        Add(Str => To_String(Items_List.Element(I).Name));
                        MAmount := MAmount + 1;
                    end if;
                end loop;
                Move_Cursor(Line => 6, Column => (Columns / 2));
                Add(Str => "Material amount:" & Integer'Image(Recipes(RecipeIndex).MaterialAmount));
                Move_Cursor(Line => 7, Column => (Columns / 2));
                Add(Str => "Workplace: ");
                case Recipes(RecipeIndex).Workplace is
                    when ALCHEMY_LAB =>
                        Add(Str => "Alchemy lab");
                    when others =>
                        null;
                end case;
                Move_Cursor(Line => 9, Column => (Columns / 2));
                Add(Str => "SPACE for set manufacturing order");
                Change_Attributes(Line => 9, Column => (Columns / 2), Count => 5, Color => 1);
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
