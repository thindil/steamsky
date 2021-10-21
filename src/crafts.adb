--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew; use Crew;
with Crew.Inventory; use Crew.Inventory;
with Items; use Items;
with Statistics; use Statistics;
with Log; use Log;
with Goals; use Goals;
with Trades; use Trades;

package body Crafts is

   procedure Load_Recipes(Reader: Tree_Reader) is
      TempRecord: Craft_Data;
      TempMaterials: UnboundedString_Container.Vector;
      TempAmount: Positive_Container.Vector;
      RecipesData: Document;
      NodesList, ChildNodes: Node_List;
      Amount, DeleteIndex: Natural;
      Recipe_Index, ItemIndex, Value: Unbounded_String;
      RecipeNode, ChildNode: Node;
      MaterialAdded: Boolean;
      Action: Data_Action;
      SkillIndex: Skills_Container.Extended_Index;
   begin
      RecipesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(RecipesData, "recipe");
      Load_Recipes_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Material_Types => TempMaterials, Material_Amounts => TempAmount,
            Result_Index => Null_Unbounded_String, Result_Amount => 10_000,
            Workplace => ALCHEMY_LAB, Skill => 1, Time => 15, Difficulty => 1,
            Tool => To_Unbounded_String("None"), Reputation => -100,
            Tool_Quality => 100);
         RecipeNode := Item(NodesList, I);
         Recipe_Index :=
           To_Unbounded_String(Get_Attribute(RecipeNode, "index"));
         Action :=
           (if Get_Attribute(RecipeNode, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(RecipeNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Recipes_Container.Contains(Recipes_List, Recipe_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " recipe '" & To_String(Recipe_Index) &
                 "', there is no recipe with that index.";
            end if;
         elsif Recipes_Container.Contains(Recipes_List, Recipe_Index) then
            raise Data_Loading_Error
              with "Can't add recipe '" & To_String(Recipe_Index) &
              "', there is already a recipe with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Recipes_List(Recipe_Index);
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (RecipeNode, "material");
            Read_Materials_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               Amount := Natural'Value(Get_Attribute(ChildNode, "amount"));
               Value := To_Unbounded_String(Get_Attribute(ChildNode, "type"));
               if Amount > 0 then
                  MaterialAdded := False;
                  Check_Added_Materials_Loop :
                  for K in
                    TempRecord.Material_Types.First_Index ..
                      TempRecord.Material_Types.Last_Index loop
                     if TempRecord.Material_Types(K) = Value then
                        TempRecord.Material_Amounts(K) := Amount;
                        MaterialAdded := True;
                        exit Check_Added_Materials_Loop;
                     end if;
                  end loop Check_Added_Materials_Loop;
                  if not MaterialAdded then
                     TempRecord.Material_Types.Append(New_Item => Value);
                     TempRecord.Material_Amounts.Append(New_Item => Amount);
                  end if;
               else
                  DeleteIndex := TempRecord.Material_Types.First_Index;
                  Delete_Materials_Loop :
                  while DeleteIndex <= TempRecord.Material_Types.Last_Index loop
                     if TempRecord.Material_Types(DeleteIndex) = Value then
                        TempRecord.Material_Types.Delete(Index => DeleteIndex);
                        exit Delete_Materials_Loop;
                     end if;
                     DeleteIndex := DeleteIndex + 1;
                  end loop Delete_Materials_Loop;
               end if;
            end loop Read_Materials_Loop;
            Value := To_Unbounded_String(Get_Attribute(RecipeNode, "result"));
            if Value /= Null_Unbounded_String then
               ItemIndex := Value;
               if ItemIndex = Null_Unbounded_String then
                  raise Data_Loading_Error
                    with "Can't add recipe '" & To_String(Recipe_Index) &
                    "', result item index '" & To_String(Value) &
                    "' does't exist.";
               end if;
               TempRecord.Result_Index := ItemIndex;
            end if;
            Value := To_Unbounded_String(Get_Attribute(RecipeNode, "crafted"));
            if Value /= Null_Unbounded_String then
               TempRecord.Result_Amount := Positive'Value(To_String(Value));
            end if;
            Value :=
              To_Unbounded_String(Get_Attribute(RecipeNode, "workplace"));
            if Value /= Null_Unbounded_String then
               TempRecord.Workplace := ModuleType'Value(To_String(Value));
            end if;
            Value := To_Unbounded_String(Get_Attribute(RecipeNode, "skill"));
            if Value /= Null_Unbounded_String then
               SkillIndex := Find_Skill_Index(To_String(Value));
               if SkillIndex = 0 then
                  raise Data_Loading_Error
                    with "Can't add recipe '" & To_String(Recipe_Index) &
                    "', no skill named '" & To_String(Value) & "'";
               end if;
               TempRecord.Skill := SkillIndex;
            end if;
            if Get_Attribute(RecipeNode, "time") /= "" then
               TempRecord.Time :=
                 Positive'Value(Get_Attribute(RecipeNode, "time"));
            end if;
            if Get_Attribute(RecipeNode, "difficulty") /= "" then
               TempRecord.Difficulty :=
                 Positive'Value(Get_Attribute(RecipeNode, "difficulty"));
            end if;
            if Get_Attribute(RecipeNode, "tool") /= "" then
               TempRecord.Tool :=
                 To_Unbounded_String(Get_Attribute(RecipeNode, "tool"));
            end if;
            Value :=
              To_Unbounded_String(Get_Attribute(RecipeNode, "reputation"));
            if Value /= Null_Unbounded_String then
               TempRecord.Reputation := Integer'Value(To_String(Value));
            end if;
            Value :=
              To_Unbounded_String(Get_Attribute(RecipeNode, "Tool_Quality"));
            if Value /= Null_Unbounded_String then
               TempRecord.Tool_Quality := Positive'Value(To_String(Value));
            end if;
            if Action /= UPDATE then
               Recipes_Container.Include
                 (Recipes_List, Recipe_Index, TempRecord);
               Log_Message
                 ("Recipe added: " &
                  To_String(Items_List(TempRecord.Result_Index).Name),
                  EVERYTHING);
            else
               Recipes_List(Recipe_Index) := TempRecord;
               Log_Message
                 ("Recipe updated: " &
                  To_String(Items_List(TempRecord.Result_Index).Name),
                  EVERYTHING);
            end if;
         else
            Recipes_Container.Exclude(Recipes_List, Recipe_Index);
            Log_Message
              ("Recipe removed: " & To_String(Recipe_Index), EVERYTHING);
         end if;
      end loop Load_Recipes_Loop;
   end Load_Recipes;

   function Set_Recipe_Data(Recipe_Index: Unbounded_String) return Craft_Data is
      Recipe: Craft_Data;
      ItemIndex: Unbounded_String;
   begin
      if Length(Recipe_Index) > 6
        and then Slice(Recipe_Index, 1, 5) = "Study" then
         ItemIndex := Unbounded_Slice(Recipe_Index, 7, Length(Recipe_Index));
         Recipe.Material_Types.Append(New_Item => Items_List(ItemIndex).IType);
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Result_Index := ItemIndex;
         Recipe.Result_Amount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Recipe_Skill_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Recipe.Result_Index then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               exit Set_Recipe_Skill_Loop;
            end if;
         end loop Set_Recipe_Skill_Loop;
         Recipe.Difficulty := 1;
         Recipe.Tool := Alchemy_Tools;
         Recipe.Tool_Quality := 100;
         return Recipe;
      elsif Length(Recipe_Index) > 12
        and then Slice(Recipe_Index, 1, 11) = "Deconstruct" then
         ItemIndex := Unbounded_Slice(Recipe_Index, 13, Length(Recipe_Index));
         Recipe.Material_Types.Append(New_Item => Items_List(ItemIndex).IType);
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Recipe_Data_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = ItemIndex then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               Recipe.Difficulty := ProtoRecipe.Difficulty;
               Recipe.Result_Index :=
                 FindProtoItem(ProtoRecipe.Material_Types(1));
               Recipe.Result_Amount :=
                 Positive
                   (Float'Ceiling
                      (Float(ProtoRecipe.Material_Amounts.Element(1)) * 0.8));
               if Recipe.Result_Amount = ProtoRecipe.Material_Amounts(1) then
                  Recipe.Result_Amount := Recipe.Result_Amount - 1;
               end if;
               exit Set_Recipe_Data_Loop;
            end if;
         end loop Set_Recipe_Data_Loop;
         Recipe.Tool := Alchemy_Tools;
         Recipe.Tool_Quality := 100;
         return Recipe;
      end if;
      return Recipes_List(Recipe_Index);
   end Set_Recipe_Data;

   function Check_Recipe(Recipe_Index: Unbounded_String) return Positive is
      Recipe: Craft_Data;
      MaterialIndexes: Positive_Container.Vector;
      RecipeName: Unbounded_String;
      MaxAmount: Positive := Positive'Last;
      MType: ModuleType;
   begin
      Recipe := Set_Recipe_Data(Recipe_Index);
      if Length(Recipe_Index) > 6
        and then Slice(Recipe_Index, 1, 5) = "Study" then
         RecipeName :=
           To_Unbounded_String("studying ") &
           Items_List(Unbounded_Slice(Recipe_Index, 7, Length(Recipe_Index)))
             .Name;
         MType := ALCHEMY_LAB;
      elsif Length(Recipe_Index) > 12
        and then Slice(Recipe_Index, 1, 11) = "Deconstruct" then
         RecipeName :=
           To_Unbounded_String("deconstructing ") &
           Items_List(Unbounded_Slice(Recipe_Index, 13, Length(Recipe_Index)))
             .Name;
         MType := ALCHEMY_LAB;
      else
         RecipeName :=
           To_Unbounded_String("manufacturing ") &
           Items_List(Recipe.Result_Index).Name;
         MType := Recipes_List(Recipe_Index).Workplace;
      end if;
      -- Check for workshop
      declare
         HaveWorkshop: Boolean := False;
      begin
         Check_For_Workshop_Loop :
         for Module of Player_Ship.Modules loop
            if Modules_List(Module.Proto_Index).MType = MType and
              Module.Durability > 0 then
               HaveWorkshop := True;
               exit Check_For_Workshop_Loop;
            end if;
         end loop Check_For_Workshop_Loop;
         if not HaveWorkshop then
            raise Crafting_No_Workshop with To_String(RecipeName);
         end if;
      end;
      -- Check for materials
      if Length(Recipe_Index) > 6
        and then Slice(Recipe_Index, 1, 5) = "Study" then
         Study_Materials_Loop :
         for I in Player_Ship.Cargo.Iterate loop
            if Items_List(Player_Ship.Cargo(I).ProtoIndex).Name =
              Items_List(Recipe.Result_Index).Name then
               MaterialIndexes.Append
                 (New_Item => Inventory_Container.To_Index(I));
               exit Study_Materials_Loop;
            end if;
         end loop Study_Materials_Loop;
         MaxAmount := 1;
      elsif Length(Recipe_Index) > 12
        and then Slice(Recipe_Index, 1, 11) = "Deconstruct" then
         Deconstruct_Materials_Loop :
         for I in Player_Ship.Cargo.Iterate loop
            if Player_Ship.Cargo(I).ProtoIndex =
              Unbounded_Slice(Recipe_Index, 13, Length(Recipe_Index)) then
               MaterialIndexes.Append
                 (New_Item => Inventory_Container.To_Index(I));
               MaxAmount := Player_Ship.Cargo(I).Amount;
               exit Deconstruct_Materials_Loop;
            end if;
         end loop Deconstruct_Materials_Loop;
      else
         Find_Materials_Loop :
         for J in Recipe.Material_Types.Iterate loop
            Check_Player_Cargo_Loop :
            for I in Player_Ship.Cargo.Iterate loop
               if Items_List(Player_Ship.Cargo(I).ProtoIndex).IType =
                 Recipe.Material_Types(J) and
                 Player_Ship.Cargo(I).Amount >=
                   Recipe.Material_Amounts
                     (UnboundedString_Container.To_Index(J)) then
                  MaterialIndexes.Append
                    (New_Item => Inventory_Container.To_Index(I));
                  if MaxAmount >
                    Player_Ship.Cargo(I).Amount /
                      Recipe.Material_Amounts
                        (UnboundedString_Container.To_Index(J)) then
                     MaxAmount :=
                       Player_Ship.Cargo(I).Amount /
                       Recipe.Material_Amounts
                         (UnboundedString_Container.To_Index(J));
                  end if;
                  exit Check_Player_Cargo_Loop;
               end if;
            end loop Check_Player_Cargo_Loop;
         end loop Find_Materials_Loop;
      end if;
      if MaterialIndexes.Length < Recipe.Material_Types.Length then
         raise Crafting_No_Materials with To_String(RecipeName);
      end if;
      -- Check for tool
      declare
         HaveTool: Boolean := False;
      begin
         if Recipe.Tool /= To_Unbounded_String("None")
           and then
             FindItem
               (Inventory => Player_Ship.Cargo, ItemType => Recipe.Tool,
                Quality => Recipe.Tool_Quality) >
             0 then
            HaveTool := True;
         elsif Recipe.Tool = To_Unbounded_String("None") then
            HaveTool := True;
         end if;
         if not HaveTool then
            raise Crafting_No_Tools with To_String(RecipeName);
         end if;
      end;
      -- Check for free space
      declare
         SpaceNeeded: Integer := 0;
      begin
         Count_Needed_Space_Loop :
         for I in MaterialIndexes.Iterate loop
            SpaceNeeded :=
              SpaceNeeded +
              Items_List(Player_Ship.Cargo(MaterialIndexes(I)).ProtoIndex)
                  .Weight *
                Recipe.Material_Amounts(Positive_Container.To_Index(I));
         end loop Count_Needed_Space_Loop;
         if FreeCargo
             (SpaceNeeded -
              (Items_List(Recipe.Result_Index).Weight * Recipe.Result_Amount)) <
           0 then
            raise Trade_No_Free_Cargo;
         end if;
      end;
      return MaxAmount;
   end Check_Recipe;

   procedure Manufacturing(Minutes: Positive) is
      Result_Amount, CraftedAmount, GainedExp: Natural := 0;
      Amount, NewAmount: Integer := 0;
      Recipe: Craft_Data;
      MaterialIndexes: UnboundedString_Container.Vector;
      WorkTime, CurrentMinutes, RecipeTime: Integer;
      Damage: Damage_Factor := 0.0;
      RecipeName: Unbounded_String;
      HaveMaterial: Boolean;
      CraftingMaterial: Natural;
      CrafterIndex: Crew_Container.Extended_Index;
      CargoIndex, ToolIndex: Inventory_Container.Extended_Index;
      procedure ResetOrder(Module: in out Module_Data; ModuleOwner: Natural) is
         HaveWorker: Boolean := False;
      begin
         if ToolIndex in
             Player_Ship.Crew(CrafterIndex).Inventory.First_Index ..
                   Player_Ship.Crew(CrafterIndex).Inventory.Last_Index then
            UpdateCargo
              (Player_Ship,
               Player_Ship.Crew(CrafterIndex).Inventory(ToolIndex).ProtoIndex,
               1,
               Player_Ship.Crew(CrafterIndex).Inventory(ToolIndex).Durability);
            UpdateInventory
              (MemberIndex => CrafterIndex, Amount => -1,
               InventoryIndex => ToolIndex);
         end if;
         Check_Owner_Loop :
         for Owner of Module.Owner loop
            if Owner = ModuleOwner or ModuleOwner = 0 then
               if Owner in
                   Player_Ship.Crew.First_Index ..
                         Player_Ship.Crew.Last_Index then
                  GiveOrders(Player_Ship, Owner, Rest);
               end if;
               Owner := 0;
            end if;
            if Owner > 0 then
               HaveWorker := True;
            end if;
         end loop Check_Owner_Loop;
         if not HaveWorker then
            Module.Crafting_Index := Null_Unbounded_String;
            Module.Crafting_Time := 0;
            Module.Crafting_Amount := 0;
         end if;
      end ResetOrder;
   begin
      Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type /= WORKSHOP then
            goto End_Of_Loop;
         end if;
         if Module.Crafting_Index = Null_Unbounded_String then
            goto End_Of_Loop;
         end if;
         Owners_Loop :
         for Owner of Module.Owner loop
            if Owner = 0 then
               goto End_Of_Owners_Loop;
            end if;
            CrafterIndex := Owner;
            if Player_Ship.Crew(CrafterIndex).Order = Craft then
               CurrentMinutes := Minutes;
               RecipeTime := Module.Crafting_Time;
               Recipe := Set_Recipe_Data(Module.Crafting_Index);
               if Length(Module.Crafting_Index) > 6
                 and then Slice(Module.Crafting_Index, 1, 5) = "Study" then
                  RecipeName :=
                    To_Unbounded_String("studying ") &
                    Items_List(Recipe.Result_Index).Name;
               elsif Length(Module.Crafting_Index) > 12
                 and then Slice(Module.Crafting_Index, 1, 11) =
                   "Deconstruct" then
                  RecipeName :=
                    To_Unbounded_String("deconstructing ") &
                    Items_List
                      (Unbounded_Slice
                         (Module.Crafting_Index, 13,
                          Length(Module.Crafting_Index)))
                      .Name;
               else
                  RecipeName :=
                    To_Unbounded_String("manufacturing ") &
                    Items_List(Recipe.Result_Index).Name;
               end if;
               if Module.Durability = 0 then
                  AddMessage
                    (To_String(Module.Name) & " is destroyed, so " &
                     To_String(Player_Ship.Crew(CrafterIndex).Name) &
                     " can't work on " & To_String(RecipeName) & ".",
                     CraftMessage, RED);
                  ResetOrder(Module, Owner);
                  CurrentMinutes := 0;
               end if;
               WorkTime := Player_Ship.Crew(CrafterIndex).OrderTime;
               CraftedAmount := 0;
               Craft_Loop :
               while CurrentMinutes > 0 loop
                  if CurrentMinutes < RecipeTime then
                     RecipeTime := RecipeTime - CurrentMinutes;
                     WorkTime := WorkTime - CurrentMinutes;
                     CurrentMinutes := 0;
                     goto End_Of_Craft_Loop;
                  end if;
                  RecipeTime := RecipeTime - CurrentMinutes;
                  WorkTime := WorkTime - CurrentMinutes;
                  CurrentMinutes := 0;
                  CurrentMinutes := CurrentMinutes - RecipeTime;
                  WorkTime := WorkTime - RecipeTime;
                  RecipeTime := Recipe.Time;
                  MaterialIndexes.Clear;
                  if Length(Module.Crafting_Index) > 6
                    and then Slice(Module.Crafting_Index, 1, 5) = "Study" then
                     Study_Materials_Loop :
                     for J in Items_List.Iterate loop
                        if Items_List(J).Name =
                          Items_List(Recipe.Result_Index).Name then
                           MaterialIndexes.Append
                             (New_Item => Objects_Container.Key(J));
                           exit Study_Materials_Loop;
                        end if;
                     end loop Study_Materials_Loop;
                  elsif Length(Module.Crafting_Index) > 12
                    and then Slice(Module.Crafting_Index, 1, 11) =
                      "Deconstruct" then
                     MaterialIndexes.Append
                       (New_Item =>
                          Unbounded_Slice
                            (Module.Crafting_Index, 13,
                             Length(Module.Crafting_Index)));
                  else
                     Recipe_Loop :
                     for K in Recipe.Material_Types.Iterate loop
                        Materials_Loop :
                        for J in Items_List.Iterate loop
                           if Items_List(J).IType =
                             Recipe.Material_Types
                               (UnboundedString_Container.To_Index(K)) then
                              MaterialIndexes.Append
                                (New_Item => Objects_Container.Key(J));
                              exit Materials_Loop;
                           end if;
                        end loop Materials_Loop;
                     end loop Recipe_Loop;
                  end if;
                  CraftingMaterial := 0;
                  Check_Materials_Loop :
                  for MaterialIndex of MaterialIndexes loop
                     CraftingMaterial :=
                       FindItem
                         (Player_Ship.Cargo,
                          ItemType => Items_List(MaterialIndex).IType);
                     if CraftingMaterial = 0 then
                        AddMessage
                          ("You don't have the crafting materials for " &
                           To_String(RecipeName) & ".",
                           CraftMessage, RED);
                        ResetOrder(Module, Owner);
                        exit Craft_Loop;
                     elsif Player_Ship.Cargo(CraftingMaterial).ProtoIndex /=
                       MaterialIndex then
                        MaterialIndex :=
                          Player_Ship.Cargo(CraftingMaterial).ProtoIndex;
                     end if;
                  end loop Check_Materials_Loop;
                  if Recipe.Tool /= To_Unbounded_String("None") then
                     ToolIndex :=
                       FindTools
                         (CrafterIndex, Recipe.Tool, Craft,
                          Recipe.Tool_Quality);
                     if ToolIndex = 0 then
                        AddMessage
                          ("You don't have the tool for " &
                           To_String(RecipeName) & ".",
                           CraftMessage, RED);
                        ResetOrder(Module, Owner);
                        exit Craft_Loop;
                     end if;
                  else
                     ToolIndex := 0;
                  end if;
                  Amount := 0;
                  Count_Amount_Loop :
                  for J in MaterialIndexes.Iterate loop
                     Amount :=
                       Amount +
                       Items_List(MaterialIndexes(J)).Weight *
                         Recipe.Material_Amounts
                           (UnboundedString_Container.To_Index(J));
                  end loop Count_Amount_Loop;
                  Result_Amount :=
                    Recipe.Result_Amount +
                    Integer
                      (Float'Floor
                         (Float(Recipe.Result_Amount) *
                          (Float
                             (GetSkillLevel
                                (Player_Ship.Crew(CrafterIndex),
                                 Recipe.Skill)) /
                           100.0)));
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(Module.Durability) /
                       Float(Module.Max_Durability));
                  Result_Amount :=
                    Result_Amount -
                    Natural(Float(Result_Amount) * Float(Damage));
                  if Result_Amount = 0 then
                     Result_Amount := 1;
                  end if;
                  Check_Enough_Materials_Loop :
                  for J in MaterialIndexes.Iterate loop
                     HaveMaterial := False;
                     Check_Cargo_Materials_Loop :
                     for Item of Player_Ship.Cargo loop
                        if Items_List(Item.ProtoIndex).IType =
                          Items_List(MaterialIndexes(J)).IType and
                          Item.Amount >=
                            Recipe.Material_Amounts
                              (UnboundedString_Container.To_Index(J)) then
                           HaveMaterial := True;
                           exit Check_Cargo_Materials_Loop;
                        end if;
                     end loop Check_Cargo_Materials_Loop;
                     exit Check_Enough_Materials_Loop when not HaveMaterial;
                  end loop Check_Enough_Materials_Loop;
                  if not HaveMaterial then
                     AddMessage
                       ("You don't have enough crafting materials for " &
                        To_String(RecipeName) & ".",
                        CraftMessage, RED);
                     ResetOrder(Module, Owner);
                     exit Craft_Loop;
                  end if;
                  CraftedAmount := CraftedAmount + Result_Amount;
                  Module.Crafting_Amount := Module.Crafting_Amount - 1;
                  Remove_Materials_Loop :
                  for J in MaterialIndexes.Iterate loop
                     CargoIndex := 1;
                     Remove_Materials_From_Cargo_Loop :
                     while CargoIndex <= Player_Ship.Cargo.Last_Index loop
                        if Items_List(Player_Ship.Cargo(CargoIndex).ProtoIndex)
                            .IType =
                          Items_List(MaterialIndexes(J)).IType then
                           if Player_Ship.Cargo(CargoIndex).Amount >
                             Recipe.Material_Amounts
                               (UnboundedString_Container.To_Index(J)) then
                              NewAmount :=
                                Player_Ship.Cargo(CargoIndex).Amount -
                                Recipe.Material_Amounts
                                  (UnboundedString_Container.To_Index(J));
                              Player_Ship.Cargo(CargoIndex).Amount :=
                                NewAmount;
                              exit Remove_Materials_From_Cargo_Loop;
                           elsif Player_Ship.Cargo(CargoIndex).Amount =
                             Recipe.Material_Amounts
                               (UnboundedString_Container.To_Index(J)) then
                              Player_Ship.Cargo.Delete
                                (Index => CargoIndex, Count => 1);
                              if ToolIndex > CargoIndex then
                                 ToolIndex := ToolIndex - 1;
                              end if;
                              exit Remove_Materials_From_Cargo_Loop;
                           end if;
                        end if;
                        CargoIndex := CargoIndex + 1;
                     end loop Remove_Materials_From_Cargo_Loop;
                  end loop Remove_Materials_Loop;
                  if ToolIndex > 0 then
                     DamageItem
                       (Player_Ship.Crew(CrafterIndex).Inventory, ToolIndex,
                        GetSkillLevel
                          (Player_Ship.Crew(CrafterIndex), Recipe.Skill),
                        CrafterIndex);
                  end if;
                  if Length(Module.Crafting_Index) < 6
                    or else
                    (Length(Module.Crafting_Index) > 6
                     and then Slice(Module.Crafting_Index, 1, 5) /=
                       "Study") then
                     Amount :=
                       Amount -
                       (Items_List(Recipe.Result_Index).Weight * Result_Amount);
                     if FreeCargo(Amount) < 0 then
                        AddMessage
                          ("You don't have the free cargo space for " &
                           To_String(RecipeName) & ".",
                           CraftMessage, RED);
                        ResetOrder(Module, Owner);
                        exit Craft_Loop;
                     end if;
                     if Length(Module.Crafting_Index) > 11
                       and then Slice(Module.Crafting_Index, 1, 11) =
                         "Deconstruct" then
                        UpdateCargo
                          (Player_Ship, Recipe.Result_Index, Result_Amount);
                     else
                        UpdateCargo
                          (Player_Ship,
                           Recipes_List(Module.Crafting_Index).Result_Index,
                           Result_Amount);
                     end if;
                     Update_Crafting_Orders_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           UpdateCraftingOrders(Recipes_Container.Key(I));
                           exit Update_Crafting_Orders_Loop;
                        end if;
                     end loop Update_Crafting_Orders_Loop;
                  else
                     Learn_Recipe_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           Known_Recipes.Append
                             (New_Item => Recipes_Container.Key(I));
                           exit Learn_Recipe_Loop;
                        end if;
                     end loop Learn_Recipe_Loop;
                     exit Craft_Loop;
                  end if;
                  exit Craft_Loop when Module.Crafting_Amount = 0;
                  <<End_Of_Craft_Loop>>
               end loop Craft_Loop;
               Module.Crafting_Time := RecipeTime;
               if CraftedAmount > 0 then
                  if Recipe.Result_Amount > 0 then
                     if Length(Module.Crafting_Index) > 12
                       and then Slice(Module.Crafting_Index, 1, 11) =
                         "Deconstruct" then
                        AddMessage
                          (To_String(Player_Ship.Crew(CrafterIndex).Name) &
                           " has recovered" & Integer'Image(CraftedAmount) &
                           " " &
                           To_String(Items_List(Recipe.Result_Index).Name) &
                           ".",
                           CraftMessage, GREEN);
                     else
                        AddMessage
                          (To_String(Player_Ship.Crew(CrafterIndex).Name) &
                           " has manufactured" & Integer'Image(CraftedAmount) &
                           " " &
                           To_String(Items_List(Recipe.Result_Index).Name) &
                           ".",
                           CraftMessage, GREEN);
                     end if;
                     Update_Goal_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           UpdateGoal
                             (CRAFT, Recipes_Container.Key(I), CraftedAmount);
                           exit Update_Goal_Loop;
                        end if;
                     end loop Update_Goal_Loop;
                     if CurrentGoal.TargetIndex /= Null_Unbounded_String then
                        UpdateGoal
                          (CRAFT, Items_List(Recipe.Result_Index).IType,
                           CraftedAmount);
                        if Items_List(Recipe.Result_Index).ShowType /=
                          Null_Unbounded_String then
                           UpdateGoal
                             (CRAFT, Items_List(Recipe.Result_Index).ShowType,
                              CraftedAmount);
                        end if;
                     end if;
                  else
                     AddMessage
                       (To_String(Player_Ship.Crew(CrafterIndex).Name) &
                        " has discovered recipe for " &
                        To_String(Items_List(Recipe.Result_Index).Name) & ".",
                        CraftMessage, GREEN);
                     UpdateGoal(CRAFT, Null_Unbounded_String);
                  end if;
               end if;
               if Player_Ship.Crew(CrafterIndex).Order = Craft then
                  Update_Work_Time_Loop :
                  while WorkTime <= 0 loop
                     GainedExp := GainedExp + 1;
                     WorkTime := WorkTime + 15;
                  end loop Update_Work_Time_Loop;
                  if GainedExp > 0 then
                     GainExp(GainedExp, Recipe.Skill, CrafterIndex);
                  end if;
                  Player_Ship.Crew(CrafterIndex).OrderTime := WorkTime;
                  if Module.Crafting_Amount = 0 then
                     ResetOrder(Module, Owner);
                  end if;
               end if;
            end if;
            <<End_Of_Owners_Loop>>
         end loop Owners_Loop;
         <<End_Of_Loop>>
      end loop Modules_Loop;
   exception
      when An_Exception : Crew_No_Space_Error =>
         AddMessage(Exception_Message(An_Exception), OrderMessage, RED);
         GiveOrders(Player_Ship, CrafterIndex, Rest);
   end Manufacturing;

   procedure Set_Recipe
     (Workshop, Amount: Positive; Recipe_Index: Unbounded_String) is
      RecipeName, ItemIndex: Unbounded_String;
   begin
      Player_Ship.Modules(Workshop).Crafting_Amount := Amount;
      if Length(Recipe_Index) > 6
        and then Slice(Recipe_Index, 1, 5) = "Study" then
         ItemIndex := Unbounded_Slice(Recipe_Index, 7, Length(Recipe_Index));
         Set_Study_Difficulty_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = ItemIndex then
               Player_Ship.Modules(Workshop).Crafting_Time :=
                 ProtoRecipe.Difficulty * 15;
               exit Set_Study_Difficulty_Loop;
            end if;
         end loop Set_Study_Difficulty_Loop;
         RecipeName :=
           To_Unbounded_String("Studying ") & Items_List(ItemIndex).Name;
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
      elsif Length(Recipe_Index) > 12
        and then Slice(Recipe_Index, 1, 11) = "Deconstruct" then
         ItemIndex := Unbounded_Slice(Recipe_Index, 13, Length(Recipe_Index));
         Set_Deconstruct_Difficulty_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = ItemIndex then
               Player_Ship.Modules(Workshop).Crafting_Time :=
                 ProtoRecipe.Difficulty * 15;
               exit Set_Deconstruct_Difficulty_Loop;
            end if;
         end loop Set_Deconstruct_Difficulty_Loop;
         RecipeName :=
           To_Unbounded_String("Deconstructing ") & Items_List(ItemIndex).Name;
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
      else
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
         Player_Ship.Modules(Workshop).Crafting_Time :=
           Recipes_List(Recipe_Index).Time;
         RecipeName := Items_List(Recipes_List(Recipe_Index).Result_Index).Name;
      end if;
      AddMessage
        (To_String(RecipeName) & " was set as manufacturing order in " &
         To_String(Player_Ship.Modules(Workshop).Name) & ".",
         CraftMessage);
      UpdateOrders(Player_Ship);
   end Set_Recipe;

end Crafts;
