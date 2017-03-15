--    Copyright 2016-2017 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew; use Crew;
with Items; use Items;
with Statistics; use Statistics;

package body Crafts is

   function LoadRecipes return Boolean is
      RecipesFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex, StartIndex, EndIndex, Amount: Natural;
      TempRecord: Craft_Data;
      TempMaterials: UnboundedString_Container.Vector;
      TempAmount: Positive_Container.Vector;
   begin
      if Recipes_List.Length > 0 then
         return True;
      end if;
      if not Exists("data/recipes.dat") then
         return False;
      end if;
      TempRecord :=
        (MaterialTypes => TempMaterials,
         MaterialAmounts => TempAmount,
         ResultIndex => 1,
         ResultAmount => 10000,
         Workplace => ALCHEMY_LAB,
         Skill => 1,
         Time => 15,
         Difficulty => 0,
         BaseType => 0,
         Tool => To_Unbounded_String("None"));
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
               for I in 1 .. Amount loop
                  EndIndex := Index(Value, ", ", StartIndex);
                  if EndIndex = 0 then
                     EndIndex := Length(Value) + 1;
                  end if;
                  TempRecord.MaterialTypes.Append
                  (New_Item =>
                     To_Unbounded_String
                       (Slice(Value, StartIndex, EndIndex - 1)));
                  StartIndex := EndIndex + 2;
               end loop;
            elsif FieldName = To_Unbounded_String("Amount") then
               StartIndex := 1;
               Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
               for I in 1 .. Amount loop
                  EndIndex := Index(Value, ", ", StartIndex);
                  if EndIndex = 0 then
                     EndIndex := Length(Value) + 1;
                  end if;
                  TempRecord.MaterialAmounts.Append
                  (New_Item =>
                     Integer'Value(Slice(Value, StartIndex, EndIndex - 1)));
                  StartIndex := EndIndex + 2;
               end loop;
            elsif FieldName = To_Unbounded_String("Result") then
               TempRecord.ResultIndex := Integer'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("Crafted") then
               TempRecord.ResultAmount := Integer'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("Workplace") then
               TempRecord.Workplace := ModuleType'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("Skill") then
               for I in
                 Skills_Names.First_Index .. Skills_Names.Last_Index loop
                  if Value = To_String(Skills_Names.Element(I)) then
                     TempRecord.Skill := I;
                     exit;
                  end if;
               end loop;
            elsif FieldName = To_Unbounded_String("Time") then
               TempRecord.Time := Integer'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("Difficulty") then
               TempRecord.Difficulty := Integer'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("BaseType") then
               TempRecord.BaseType := Integer'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("Tool") then
               TempRecord.Tool := Value;
            end if;
         elsif TempRecord.ResultAmount < 10000 then
            Recipes_List.Append(New_Item => TempRecord);
            TempRecord :=
              (MaterialTypes => TempMaterials,
               MaterialAmounts => TempAmount,
               ResultIndex => 1,
               ResultAmount => 10000,
               Workplace => ALCHEMY_LAB,
               Skill => 1,
               Time => 15,
               Difficulty => 0,
               BaseType => 0,
               Tool => To_Unbounded_String("None"));
         end if;
      end loop;
      Close(RecipesFile);
      return True;
   end LoadRecipes;

   procedure SetRecipe(RecipeIndex: Integer; ModuleIndex: Positive) is
      Recipe: Craft_Data;
      SpaceNeeded: Integer := 0;
      MaterialIndexes: Positive_Container.Vector;
      RecipeName: Unbounded_String;
      HaveTool: Boolean := False;
   begin
      if RecipeIndex > 0 then
         Recipe := Recipes_List.Element(RecipeIndex);
         RecipeName := Items_List.Element(Recipe.ResultIndex).Name;
      else
         Recipe.MaterialTypes.Append
         (New_Item => Items_List.Element(abs (RecipeIndex)).IType);
         Recipe.MaterialAmounts.Append(New_Item => 1);
         Recipe.ResultIndex := abs (RecipeIndex);
         Recipe.ResultAmount := 1;
         Recipe.Workplace := ALCHEMY_LAB;
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.ResultIndex = Recipe.ResultIndex then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               exit;
            end if;
         end loop;
         Recipe.Difficulty := 0;
         Recipe.Tool := To_Unbounded_String("AlchemySet");
         RecipeName :=
           To_Unbounded_String("Deconstructing ") &
           Items_List.Element(Recipe.ResultIndex).Name;
      end if;
      -- Check for materials
      if RecipeIndex > 0 then
         for I in
           PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
            for J in
              Recipe.MaterialTypes.First_Index ..
                  Recipe.MaterialTypes.Last_Index loop
               if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex)
                   .IType =
                 Recipe.MaterialTypes(J) and
                 PlayerShip.Cargo.Element(I).Amount >=
                   Recipe.MaterialAmounts(J) then
                  MaterialIndexes.Append(New_Item => I);
               end if;
            end loop;
         end loop;
      else
         for I in
           PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
            if Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex)
                .Name =
              Items_List.Element(Recipe.ResultIndex).Name then
               MaterialIndexes.Append(New_Item => I);
               exit;
            end if;
         end loop;
      end if;
      if MaterialIndexes.Length < Recipe.MaterialTypes.Length then
         ShowDialog
           ("You don't have enough materials to start manufacturing " &
            To_String(RecipeName) &
            ".");
         return;
      end if;
      -- Check for tool
      if Recipe.Tool /= To_Unbounded_String("None") then
         for Item of PlayerShip.Cargo loop
            if Items_List.Element(Item.ProtoIndex).IType = Recipe.Tool then
               HaveTool := True;
               exit;
            end if;
         end loop;
      else
         HaveTool := True;
      end if;
      if not HaveTool then
         ShowDialog
           ("You don't have proper tool to start manufacturing " &
            To_String(RecipeName) &
            ".");
         return;
      end if;
      -- Check for free space
      for I in MaterialIndexes.First_Index .. MaterialIndexes.Last_Index loop
         SpaceNeeded :=
           SpaceNeeded +
           Items_List.Element(MaterialIndexes.Element(I)).Weight *
             Recipe.MaterialAmounts.Element(I);
      end loop;
      if FreeCargo
          (SpaceNeeded -
           (Items_List.Element(Recipe.ResultIndex).Weight *
            Recipe.ResultAmount)) <
        0 then
         ShowDialog("You don't have that much free space in your ship cargo.");
         return;
      end if;
      UpdateModule
        (PlayerShip,
         ModuleIndex,
         "Current_Value",
         Integer'Image(RecipeIndex));
      UpdateModule
        (PlayerShip,
         ModuleIndex,
         "Max_Value",
         Positive'Image(Recipe.Time));
      AddMessage
        (To_String(RecipeName) &
         " was set as manufacturing order in " &
         To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
         ".",
         CraftMessage);
   end SetRecipe;

   procedure Manufacturing(Minutes: Positive) is
      CrafterIndex,
      ResultAmount,
      CraftedAmount,
      GainedExp,
      ToolIndex: Natural :=
        0;
      Amount: Integer := 0;
      Recipe: Craft_Data;
      MaterialIndexes: Positive_Container.Vector;
      WorkTime, CurrentMinutes, RecipeTime: Integer;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      subtype Workplaces is ModuleType range ALCHEMY_LAB .. GREENHOUSE;
      RecipeName: Unbounded_String;
      procedure UpdateMember(Member: in out Member_Data) is
      begin
         Member.OrderTime := WorkTime;
      end UpdateMember;
   begin
      for L in
        PlayerShip.Modules.First_Index .. PlayerShip.Modules.Last_Index loop
         if PlayerShip.Modules.Element(L).Owner > 0 and
           (Modules_List.Element(PlayerShip.Modules.Element(L).ProtoIndex)
              .MType in
              Workplaces) and
           PlayerShip.Modules.Element(L).Current_Value /= 0 then
            CrafterIndex := PlayerShip.Modules.Element(L).Owner;
            if PlayerShip.Crew.Element(CrafterIndex).Order = Craft then
               CurrentMinutes := Minutes;
               RecipeTime := PlayerShip.Modules.Element(L).Max_Value;
               if PlayerShip.Modules.Element(L).Current_Value > 0 then
                  Recipe :=
                    Recipes_List.Element
                    (PlayerShip.Modules.Element(L).Current_Value);
                  RecipeName :=
                    To_Unbounded_String("manufacturing ") &
                    Items_List.Element(Recipe.ResultIndex).Name;
               else
                  Recipe.ResultIndex :=
                    abs (PlayerShip.Modules.Element(L).Current_Value);
                  Recipe.MaterialTypes.Append
                  (New_Item => Items_List.Element(Recipe.ResultIndex).IType);
                  Recipe.MaterialAmounts.Append(New_Item => 1);
                  Recipe.ResultAmount := 0;
                  Recipe.Workplace := ALCHEMY_LAB;
                  for ProtoRecipe of Recipes_List loop
                     if ProtoRecipe.ResultIndex = Recipe.ResultIndex then
                        Recipe.Skill := ProtoRecipe.Skill;
                        Recipe.Time := ProtoRecipe.Difficulty * 15;
                        exit;
                     end if;
                  end loop;
                  Recipe.Difficulty := 0;
                  Recipe.BaseType := 0;
                  Recipe.Tool := To_Unbounded_String("AlchemySet");
                  RecipeName :=
                    To_Unbounded_String("deconstructing ") &
                    Items_List.Element(Recipe.ResultIndex).Name;
               end if;
               WorkTime := PlayerShip.Crew.Element(CrafterIndex).OrderTime;
               CraftedAmount := 0;
               Craft_Loop:
               while CurrentMinutes > 0 loop
                  if CurrentMinutes >= RecipeTime then
                     CurrentMinutes := CurrentMinutes - RecipeTime;
                     WorkTime := WorkTime - RecipeTime;
                     RecipeTime := Recipe.Time;
                     MaterialIndexes.Clear;
                     if PlayerShip.Modules.Element(L).Current_Value > 0 then
                        for J in
                          PlayerShip.Cargo.First_Index ..
                              PlayerShip.Cargo.Last_Index loop
                           for K in
                             Recipe.MaterialTypes.First_Index ..
                                 Recipe.MaterialTypes.Last_Index loop
                              if Items_List.Element
                                (PlayerShip.Cargo.Element(J).ProtoIndex)
                                  .IType =
                                Recipe.MaterialTypes(K) then
                                 MaterialIndexes.Append(New_Item => J);
                              end if;
                           end loop;
                        end loop;
                     else
                        for J in
                          PlayerShip.Cargo.First_Index ..
                              PlayerShip.Cargo.Last_Index loop
                           if Items_List.Element
                             (PlayerShip.Cargo.Element(J).ProtoIndex)
                               .Name =
                             Items_List.Element(Recipe.ResultIndex).Name then
                              MaterialIndexes.Append(New_Item => J);
                              exit;
                           end if;
                        end loop;
                     end if;
                     if MaterialIndexes.Length <
                       Recipe.MaterialTypes.Length then
                        AddMessage
                          ("You don't have crafting materials for " &
                           To_String(RecipeName) &
                           ".",
                           CraftMessage);
                        GiveOrders(CrafterIndex, Rest);
                        UpdateModule(PlayerShip, L, "Current_Value", "0");
                        UpdateModule
                          (PlayerShip,
                           L,
                           "Max_Value",
                           Integer'
                             Image
                               (0 - PlayerShip.Modules.Element(L).Max_Value));
                        exit Craft_Loop;
                     end if;
                     if Recipe.Tool /= To_Unbounded_String("None") then
                        ToolIndex := 0;
                        for J in
                          PlayerShip.Cargo.First_Index ..
                              PlayerShip.Cargo.Last_Index loop
                           if Items_List.Element
                             (PlayerShip.Cargo.Element(J).ProtoIndex)
                               .IType =
                             Recipe.Tool then
                              ToolIndex := J;
                              exit;
                           end if;
                        end loop;
                     else
                        ToolIndex := 0;
                     end if;
                     if ToolIndex = 0 and
                       Recipe.Tool /= To_Unbounded_String("None") then
                        AddMessage
                          ("You don't have tool for " &
                           To_String(RecipeName) &
                           ".",
                           CraftMessage);
                        UpdateModule(PlayerShip, L, "Current_Value", "0");
                        UpdateModule
                          (PlayerShip,
                           L,
                           "Max_Value",
                           Integer'
                             Image
                               (0 - PlayerShip.Modules.Element(L).Max_Value));
                        GiveOrders(CrafterIndex, Rest);
                        exit Craft_Loop;
                     end if;
                     Amount := 0;
                     for J in
                       MaterialIndexes.First_Index ..
                           MaterialIndexes.Last_Index loop
                        Amount :=
                          Amount +
                          Items_List.Element
                            (PlayerShip.Cargo.Element
                             (MaterialIndexes.Element(J))
                               .ProtoIndex)
                              .Weight *
                            Recipe.MaterialAmounts.Element(J);
                     end loop;
                     ResultAmount :=
                       Recipe.ResultAmount +
                       Integer
                         (Float'Floor
                            (Float(Recipe.ResultAmount) *
                             (Float
                                (GetSkillLevel(CrafterIndex, Recipe.Skill)) /
                              100.0)));
                     Damage :=
                       1.0 -
                       DamageFactor
                         (Float(PlayerShip.Modules.Element(L).Durability) /
                          Float(PlayerShip.Modules.Element(L).MaxDurability));
                     ResultAmount :=
                       ResultAmount -
                       Natural(Float(ResultAmount) * Float(Damage));
                     if ResultAmount = 0 then
                        ResultAmount := 1;
                     end if;
                     for J in
                       MaterialIndexes.First_Index ..
                           MaterialIndexes.Last_Index loop
                        if PlayerShip.Cargo.Element(MaterialIndexes.Element(J))
                            .Amount <
                          Recipe.MaterialAmounts.Element(J) then
                           AddMessage
                             ("You don't have enough crafting materials for " &
                              To_String(RecipeName) &
                              ".",
                              CraftMessage);
                           UpdateModule(PlayerShip, L, "Current_Value", "0");
                           UpdateModule
                             (PlayerShip,
                              L,
                              "Max_Value",
                              Integer'
                                Image
                                  (0 -
                                   PlayerShip.Modules.Element(L).Max_Value));
                           GiveOrders(CrafterIndex, Rest);
                           exit Craft_Loop;
                        end if;
                     end loop;
                     CraftedAmount := CraftedAmount + ResultAmount;
                     for J in
                       MaterialIndexes.First_Index ..
                           MaterialIndexes.Last_Index loop
                        UpdateCargo
                          (PlayerShip,
                           PlayerShip.Cargo.Element(MaterialIndexes.Element(J))
                             .ProtoIndex,
                           (0 - Recipe.MaterialAmounts.Element(J)));
                     end loop;
                     if ToolIndex > 0 then
                        DamageCargo(ToolIndex, CrafterIndex, Recipe.Skill);
                     end if;
                     if PlayerShip.Modules.Element(L).Current_Value > 0 then
                        Amount :=
                          Amount -
                          (Items_List.Element(Recipe.ResultIndex).Weight *
                           ResultAmount);
                        if FreeCargo(Amount) < 0 then
                           AddMessage
                             ("You don't have free cargo space for " &
                              To_String(RecipeName) &
                              ".",
                              CraftMessage);
                           UpdateModule(PlayerShip, L, "Current_Value", "0");
                           UpdateModule
                             (PlayerShip,
                              L,
                              "Max_Value",
                              Integer'
                                Image
                                  (0 -
                                   PlayerShip.Modules.Element(L).Max_Value));
                           GiveOrders(CrafterIndex, Rest);
                           exit Craft_Loop;
                        end if;
                        UpdateCargo
                          (PlayerShip,
                           Recipes_List.Element
                           (PlayerShip.Modules.Element(L).Current_Value)
                             .ResultIndex,
                           ResultAmount);
                        GameStats.CraftingOrders :=
                          GameStats.CraftingOrders + 1;
                     else
                        for I in
                          Recipes_List.First_Index ..
                              Recipes_List.Last_Index loop
                           if Recipes_List.Element(I).ResultIndex =
                             Recipe.ResultIndex then
                              Known_Recipes.Append(New_Item => I);
                              exit;
                           end if;
                        end loop;
                        exit Craft_Loop;
                     end if;
                  else
                     RecipeTime := RecipeTime - CurrentMinutes;
                     WorkTime := WorkTime - CurrentMinutes;
                     CurrentMinutes := 0;
                  end if;
               end loop Craft_Loop;
               UpdateModule
                 (PlayerShip,
                  L,
                  "Max_Value",
                  Positive'
                    Image
                      ((0 - PlayerShip.Modules.Element(L).Max_Value) +
                       RecipeTime));
               if CraftedAmount > 0 then
                  if Recipe.ResultAmount > 0 then
                     AddMessage
                       (To_String(PlayerShip.Crew.Element(CrafterIndex).Name) &
                        " was manufactured" &
                        Integer'Image(CraftedAmount) &
                        " " &
                        To_String
                          (Items_List.Element(Recipe.ResultIndex).Name) &
                        ".",
                        CraftMessage);
                  else
                     AddMessage
                       (To_String(PlayerShip.Crew.Element(CrafterIndex).Name) &
                        " was discovered recipe for " &
                        To_String
                          (Items_List.Element(Recipe.ResultIndex).Name) &
                        ".",
                        CraftMessage);
                  end if;
               end if;
               if PlayerShip.Crew.Element(CrafterIndex).Order = Craft then
                  while WorkTime <= 0 loop
                     GainedExp := GainedExp + 1;
                     WorkTime := WorkTime + 15;
                  end loop;
                  if GainedExp > 0 then
                     GainExp(GainedExp, Recipe.Skill, CrafterIndex);
                  end if;
                  PlayerShip.Crew.Update_Element
                  (Index => CrafterIndex, Process => UpdateMember'Access);
                  if PlayerShip.Modules.Element(L).Current_Value < 0 and
                    CraftedAmount > 0 then
                     UpdateModule(PlayerShip, L, "Current_Value", "0");
                     UpdateModule
                       (PlayerShip,
                        L,
                        "Max_Value",
                        Integer'
                          Image
                            (0 - PlayerShip.Modules.Element(L).Max_Value));
                     GiveOrders(CrafterIndex, Rest);
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Manufacturing;

end Crafts;
