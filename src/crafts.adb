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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Messages; use Messages;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew; use Crew;
with Items; use Items;
with Statistics; use Statistics;
with Log; use Log;
with Goals; use Goals;
with Trades; use Trades;

package body Crafts is

   procedure LoadRecipes is
      RecipesFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex, StartIndex, EndIndex, Amount, ItemIndex: Natural;
      TempRecord: Craft_Data;
      TempMaterials: UnboundedString_Container.Vector;
      TempAmount: Positive_Container.Vector;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
   begin
      if Recipes_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "recipes" & Dir_Separator) then
         raise Recipes_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "recipes" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Recipes_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (MaterialTypes => TempMaterials,
            MaterialAmounts => TempAmount,
            ResultIndex => 1,
            ResultAmount => 10000,
            Workplace => ALCHEMY_LAB,
            Skill => 1,
            Time => 15,
            Difficulty => 1,
            BaseType => 0,
            Tool => To_Unbounded_String("None"),
            Index => Null_Unbounded_String);
         LogMessage
           ("Loading recipes file: " & Full_Name(FoundFile),
            Everything);
         Open(RecipesFile, In_File, Full_Name(FoundFile));
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
                  ItemIndex := FindProtoItem(Value);
                  if ItemIndex = 0 then
                     Close(RecipesFile);
                     End_Search(Files);
                     raise Recipes_Invalid_Data
                       with "Invalid result item index: |" &
                       To_String(Value) &
                       "|.";
                  end if;
                  TempRecord.ResultIndex := ItemIndex;
               elsif FieldName = To_Unbounded_String("Crafted") then
                  TempRecord.ResultAmount := Integer'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Workplace") then
                  TempRecord.Workplace := ModuleType'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Skill") then
                  for I in Skills_Names.Iterate loop
                     if Value = To_String(Skills_Names(I)) then
                        TempRecord.Skill :=
                          UnboundedString_Container.To_Index(I);
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
            else
               if TempRecord.ResultAmount < 10000 then
                  Recipes_List.Append(New_Item => TempRecord);
                  LogMessage
                    ("Recipe added: " &
                     To_String(Items_List(TempRecord.ResultIndex).Name),
                     Everything);
                  TempRecord :=
                    (MaterialTypes => TempMaterials,
                     MaterialAmounts => TempAmount,
                     ResultIndex => 1,
                     ResultAmount => 10000,
                     Workplace => ALCHEMY_LAB,
                     Skill => 1,
                     Time => 15,
                     Difficulty => 1,
                     BaseType => 0,
                     Tool => To_Unbounded_String("None"),
                     Index => Null_Unbounded_String);
               end if;
               if Length(RawData) > 2 then
                  TempRecord.Index :=
                    Unbounded_Slice(RawData, 2, (Length(RawData) - 1));
               end if;
            end if;
         end loop;
         Close(RecipesFile);
      end loop;
      End_Search(Files);
   end LoadRecipes;

   function CheckRecipe(RecipeIndex: Integer) return Positive is
      Recipe: Craft_Data;
      SpaceNeeded: Integer := 0;
      MaterialIndexes: Positive_Container.Vector;
      RecipeName: Unbounded_String;
      HaveTool, HaveWorkshop: Boolean := False;
      MaxAmount: Positive := Positive'Last;
      MType: ModuleType;
   begin
      if RecipeIndex > 0 then
         Recipe := Recipes_List(RecipeIndex);
         RecipeName := Items_List(Recipe.ResultIndex).Name;
         MType := Recipes_List(RecipeIndex).Workplace;
      else
         Recipe.MaterialTypes.Append
         (New_Item => Items_List(abs (RecipeIndex)).IType);
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
         Recipe.Difficulty := 1;
         Recipe.Tool := AlchemyTools;
         RecipeName :=
           To_Unbounded_String("Deconstructing ") &
           Items_List(Recipe.ResultIndex).Name;
         MType := ALCHEMY_LAB;
      end if;
      -- Check for workshop
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = MType and
           Module.Durability > 0 then
            HaveWorkshop := True;
            exit;
         end if;
      end loop;
      if not HaveWorkshop then
         raise Crafting_No_Workshop with To_String(RecipeName);
      end if;
      -- Check for materials
      if RecipeIndex > 0 then
         for I in PlayerShip.Cargo.Iterate loop
            for J in Recipe.MaterialTypes.Iterate loop
               if Items_List(PlayerShip.Cargo(I).ProtoIndex).IType =
                 Recipe.MaterialTypes(J) and
                 PlayerShip.Cargo(I).Amount >=
                   Recipe.MaterialAmounts
                     (UnboundedString_Container.To_Index(J)) then
                  MaterialIndexes.Append
                  (New_Item => Cargo_Container.To_Index(I));
                  if MaxAmount >
                    PlayerShip.Cargo(I).Amount /
                      Recipe.MaterialAmounts
                        (UnboundedString_Container.To_Index(J)) then
                     MaxAmount :=
                       PlayerShip.Cargo(I).Amount /
                       Recipe.MaterialAmounts
                         (UnboundedString_Container.To_Index(J));
                  end if;
               end if;
            end loop;
         end loop;
      else
         for I in PlayerShip.Cargo.Iterate loop
            if Items_List(PlayerShip.Cargo(I).ProtoIndex).Name =
              Items_List(Recipe.ResultIndex).Name then
               MaterialIndexes.Append(New_Item => Cargo_Container.To_Index(I));
               exit;
            end if;
         end loop;
         MaxAmount := 1;
      end if;
      if MaterialIndexes.Length < Recipe.MaterialTypes.Length then
         raise Crafting_No_Materials with To_String(RecipeName);
      end if;
      -- Check for tool
      if Recipe.Tool /= To_Unbounded_String("None") then
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = Recipe.Tool then
               HaveTool := True;
               exit;
            end if;
         end loop;
      else
         HaveTool := True;
      end if;
      if not HaveTool then
         raise Crafting_No_Tools with To_String(RecipeName);
      end if;
      -- Check for free space
      for I in MaterialIndexes.Iterate loop
         SpaceNeeded :=
           SpaceNeeded +
           Items_List(MaterialIndexes(I)).Weight *
             Recipe.MaterialAmounts(Positive_Container.To_Index(I));
      end loop;
      if FreeCargo
          (SpaceNeeded -
           (Items_List(Recipe.ResultIndex).Weight * Recipe.ResultAmount)) <
        0 then
         raise Trade_No_Free_Cargo;
      end if;
      return MaxAmount;
   end CheckRecipe;

   procedure Manufacturing(Minutes: Positive) is
      CrafterIndex,
      ResultAmount,
      CraftedAmount,
      GainedExp,
      ToolIndex,
      CargoIndex: Natural :=
        0;
      Amount, NewAmount: Integer := 0;
      Recipe: Craft_Data;
      MaterialIndexes: Positive_Container.Vector;
      WorkTime, CurrentMinutes, RecipeTime: Integer;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      subtype Workplaces is ModuleType range ALCHEMY_LAB .. GREENHOUSE;
      RecipeName: Unbounded_String;
      HaveMaterial: Boolean;
   begin
      for Module of PlayerShip.Modules loop
         if Module.Owner > 0 and
           (Modules_List(Module.ProtoIndex).MType in Workplaces) and
           Module.Data(1) /= 0 then
            CrafterIndex := Module.Owner;
            if PlayerShip.Crew(CrafterIndex).Order = Craft then
               CurrentMinutes := Minutes;
               RecipeTime := Module.Data(2);
               if Module.Data(1) > 0 then
                  Recipe := Recipes_List(Module.Data(1));
                  RecipeName :=
                    To_Unbounded_String("manufacturing ") &
                    Items_List(Recipe.ResultIndex).Name;
               else
                  Recipe.ResultIndex := abs (Module.Data(1));
                  Recipe.MaterialTypes.Append
                  (New_Item => Items_List(Recipe.ResultIndex).IType);
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
                  Recipe.Difficulty := 1;
                  Recipe.BaseType := 0;
                  Recipe.Tool := AlchemyTools;
                  RecipeName :=
                    To_Unbounded_String("deconstructing ") &
                    Items_List(Recipe.ResultIndex).Name;
               end if;
               if Module.Durability = 0 then
                  AddMessage
                    (To_String(Module.Name) &
                     " is destroyed, so " &
                     To_String(PlayerShip.Crew(CrafterIndex).Name) &
                     " can't work on " &
                     To_String(RecipeName) &
                     ".",
                     CraftMessage,
                     3);
                  Module.Data := (0, 0, 0);
                  GiveOrders(CrafterIndex, Rest);
                  CurrentMinutes := 0;
               end if;
               WorkTime := PlayerShip.Crew(CrafterIndex).OrderTime;
               CraftedAmount := 0;
               Craft_Loop:
               while CurrentMinutes > 0 loop
                  if CurrentMinutes >= RecipeTime then
                     CurrentMinutes := CurrentMinutes - RecipeTime;
                     WorkTime := WorkTime - RecipeTime;
                     RecipeTime := Recipe.Time;
                     MaterialIndexes.Clear;
                     if Module.Data(1) > 0 then
                        for K in Recipe.MaterialTypes.Iterate loop
                           for J in Items_List.Iterate loop
                              if Items_List(J).IType =
                                Recipe.MaterialTypes
                                  (UnboundedString_Container.To_Index(K)) then
                                 MaterialIndexes.Append
                                 (New_Item => Objects_Container.To_Index(J));
                                 exit;
                              end if;
                           end loop;
                        end loop;
                     else
                        for J in Items_List.Iterate loop
                           if Items_List(J).Name =
                             Items_List(Recipe.ResultIndex).Name then
                              MaterialIndexes.Append
                              (New_Item => Objects_Container.To_Index(J));
                              exit;
                           end if;
                        end loop;
                     end if;
                     for MaterialIndex of MaterialIndexes loop
                        if FindCargo(MaterialIndex) = 0 then
                           AddMessage
                             ("You don't have crafting materials for " &
                              To_String(RecipeName) &
                              ".",
                              CraftMessage,
                              3);
                           Module.Data := (0, 0, 0);
                           GiveOrders(CrafterIndex, Rest);
                           exit Craft_Loop;
                        end if;
                     end loop;
                     if Recipe.Tool /= To_Unbounded_String("None") then
                        ToolIndex := FindCargo(ItemType => Recipe.Tool);
                     else
                        ToolIndex := 0;
                     end if;
                     if ToolIndex = 0 and
                       Recipe.Tool /= To_Unbounded_String("None") then
                        AddMessage
                          ("You don't have tool for " &
                           To_String(RecipeName) &
                           ".",
                           CraftMessage,
                           3);
                        Module.Data := (0, 0, 0);
                        GiveOrders(CrafterIndex, Rest);
                        exit Craft_Loop;
                     end if;
                     Amount := 0;
                     for J in MaterialIndexes.Iterate loop
                        Amount :=
                          Amount +
                          Items_List(Positive_Container.To_Index(J)).Weight *
                            Recipe.MaterialAmounts
                              (Positive_Container.To_Index(J));
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
                         (Float(Module.Durability) /
                          Float(Module.MaxDurability));
                     ResultAmount :=
                       ResultAmount -
                       Natural(Float(ResultAmount) * Float(Damage));
                     if ResultAmount = 0 then
                        ResultAmount := 1;
                     end if;
                     for J in MaterialIndexes.Iterate loop
                        HaveMaterial := False;
                        for Item of PlayerShip.Cargo loop
                           if Item.ProtoIndex = MaterialIndexes(J) and
                             Item.Amount >=
                               Recipe.MaterialAmounts
                                 (Positive_Container.To_Index(J)) then
                              HaveMaterial := True;
                              exit;
                           end if;
                        end loop;
                        exit when not HaveMaterial;
                     end loop;
                     if not HaveMaterial then
                        AddMessage
                          ("You don't have enough crafting materials for " &
                           To_String(RecipeName) &
                           ".",
                           CraftMessage,
                           3);
                        Module.Data := (0, 0, 0);
                        GiveOrders(CrafterIndex, Rest);
                        exit Craft_Loop;
                     end if;
                     CraftedAmount := CraftedAmount + ResultAmount;
                     Module.Data(3) := Module.Data(3) - 1;
                     for J in MaterialIndexes.Iterate loop
                        CargoIndex := 1;
                        while CargoIndex <= PlayerShip.Cargo.Last_Index loop
                           if PlayerShip.Cargo(CargoIndex).ProtoIndex =
                             MaterialIndexes(J) then
                              if PlayerShip.Cargo(CargoIndex).Amount >
                                Recipe.MaterialAmounts
                                  (Positive_Container.To_Index(J)) then
                                 NewAmount :=
                                   PlayerShip.Cargo(CargoIndex).Amount -
                                   Recipe.MaterialAmounts
                                     (Positive_Container.To_Index(J));
                                 PlayerShip.Cargo(CargoIndex).Amount :=
                                   NewAmount;
                                 exit;
                              elsif PlayerShip.Cargo(CargoIndex).Amount =
                                Recipe.MaterialAmounts
                                  (Positive_Container.To_Index(J)) then
                                 PlayerShip.Cargo.Delete
                                 (Index => CargoIndex, Count => 1);
                                 if ToolIndex > CargoIndex then
                                    ToolIndex := ToolIndex - 1;
                                 end if;
                                 exit;
                              end if;
                           end if;
                           CargoIndex := CargoIndex + 1;
                        end loop;
                     end loop;
                     if ToolIndex > 0 then
                        DamageCargo(ToolIndex, CrafterIndex, Recipe.Skill);
                     end if;
                     if Module.Data(1) > 0 then
                        Amount :=
                          Amount -
                          (Items_List(Recipe.ResultIndex).Weight *
                           ResultAmount);
                        if FreeCargo(Amount) < 0 then
                           AddMessage
                             ("You don't have free cargo space for " &
                              To_String(RecipeName) &
                              ".",
                              CraftMessage,
                              3);
                           Module.Data := (0, 0, 0);
                           GiveOrders(CrafterIndex, Rest);
                           exit Craft_Loop;
                        end if;
                        UpdateCargo
                          (PlayerShip,
                           Recipes_List.Element(Module.Data(1)).ResultIndex,
                           ResultAmount);
                        UpdateCraftingOrders(Recipe.Index);
                     else
                        for I in Recipes_List.Iterate loop
                           if Recipes_List(I).ResultIndex =
                             Recipe.ResultIndex then
                              Known_Recipes.Append
                              (New_Item => Recipes_Container.To_Index(I));
                              exit;
                           end if;
                        end loop;
                        exit Craft_Loop;
                     end if;
                     exit Craft_Loop when Module.Data(3) = 0;
                  else
                     RecipeTime := RecipeTime - CurrentMinutes;
                     WorkTime := WorkTime - CurrentMinutes;
                     CurrentMinutes := 0;
                  end if;
               end loop Craft_Loop;
               Module.Data(2) := RecipeTime;
               if CraftedAmount > 0 then
                  if Recipe.ResultAmount > 0 then
                     AddMessage
                       (To_String(PlayerShip.Crew(CrafterIndex).Name) &
                        " was manufactured" &
                        Integer'Image(CraftedAmount) &
                        " " &
                        To_String(Items_List(Recipe.ResultIndex).Name) &
                        ".",
                        CraftMessage,
                        2);
                     UpdateGoal(CRAFT, Recipe.Index, CraftedAmount);
                     if CurrentGoal.TargetIndex /= Null_Unbounded_String then
                        UpdateGoal
                          (CRAFT,
                           Items_List(Recipe.ResultIndex).IType,
                           CraftedAmount);
                        if Items_List(Recipe.ResultIndex).ShowType /=
                          Null_Unbounded_String then
                           UpdateGoal
                             (CRAFT,
                              Items_List(Recipe.ResultIndex).ShowType,
                              CraftedAmount);
                        end if;
                     end if;
                  else
                     AddMessage
                       (To_String(PlayerShip.Crew(CrafterIndex).Name) &
                        " was discovered recipe for " &
                        To_String(Items_List(Recipe.ResultIndex).Name) &
                        ".",
                        CraftMessage,
                        2);
                     UpdateGoal(CRAFT, Null_Unbounded_String);
                  end if;
               end if;
               if PlayerShip.Crew(CrafterIndex).Order = Craft then
                  while WorkTime <= 0 loop
                     GainedExp := GainedExp + 1;
                     WorkTime := WorkTime + 15;
                  end loop;
                  if GainedExp > 0 then
                     GainExp(GainedExp, Recipe.Skill, CrafterIndex);
                  end if;
                  PlayerShip.Crew(CrafterIndex).OrderTime := WorkTime;
                  if Module.Data(3) = 0 then
                     Module.Data := (0, 0, 0);
                     GiveOrders(CrafterIndex, Rest);
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Manufacturing;

   function FindRecipe(Index: Unbounded_String) return Natural is
   begin
      for I in Recipes_List.Iterate loop
         if Recipes_List(I).Index = Index then
            return Recipes_Container.To_Index(I);
         end if;
      end loop;
      return 0;
   end FindRecipe;

   procedure SetRecipe(Workshop, Amount: Positive; RecipeIndex: Integer) is
      RecipeName: Unbounded_String;
   begin
      PlayerShip.Modules(Workshop).Data(1) := RecipeIndex;
      PlayerShip.Modules(Workshop).Data(3) := Amount;
      if RecipeIndex > 0 then
         PlayerShip.Modules(Workshop).Data(2) :=
           Recipes_List(RecipeIndex).Time;
         RecipeName := Items_List(Recipes_List(RecipeIndex).ResultIndex).Name;
      else
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.ResultIndex = abs (RecipeIndex) then
               PlayerShip.Modules(Workshop).Data(2) :=
                 ProtoRecipe.Difficulty * 15;
               exit;
            end if;
         end loop;
         RecipeName :=
           To_Unbounded_String("Deconstructing ") &
           Items_List(abs (RecipeIndex)).Name;
      end if;
      AddMessage
        (To_String(RecipeName) &
         " was set as manufacturing order in " &
         To_String(PlayerShip.Modules(Workshop).Name) &
         ".",
         CraftMessage);
      UpdateOrders;
   end SetRecipe;

end Crafts;
