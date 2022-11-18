--    Copyright 2016-2022 Bartek thindil Jasicki
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew; use Crew;
with Crew.Inventory; use Crew.Inventory;
with Statistics; use Statistics;
with Goals; use Goals;
with Trades; use Trades;

package body Crafts is

   procedure Load_Recipes(File_Name: String) is
      use Tiny_String;

      --## rule off TYPE_INITIAL_VALUES
      type Craft_Nim_Data is record
         Result_Index: Integer;
         Result_Amount: Integer;
         Workplace: Integer;
         Skill: Integer;
         Time: Positive := 1;
         Difficulty: Positive := 1;
         Tool: chars_ptr;
         Reputation: Integer;
         Tool_Quality: Positive := 1;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Craft_Data;
      Temp_Materials: TinyString_Container.Vector;
      Temp_Amount: Positive_Container.Vector;
      Temp_Nim_Record: Craft_Nim_Data;
      --## rule on IMPROPER_INITIALIZATION
      Index: Positive := 1;
      Index2, Material_Amount: Natural := 0;
      Material_Type: Unbounded_String := Null_Unbounded_String;
      procedure Load_Ada_Recipes(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaRecipes";
      procedure Get_Ada_Craft
        (Index: chars_ptr; Ada_Craft: out Craft_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaCraftData";
      function Get_Ada_Recipe_Material_Type
        (R_Index: chars_ptr; Type_Index: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaRecipeMaterialType";
      function Get_Ada_Recipe_Material_Amount
        (R_Index: chars_ptr; Type_Index: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaRecipeMaterialAmount";
   begin
      Load_Ada_Recipes(Name => New_String(Str => File_Name));
      Load_Recipes_Loop :
      loop
         Get_Ada_Craft
           (Index => New_String(Str => Index'Img),
            Ada_Craft => Temp_Nim_Record);
         exit Load_Recipes_Loop when Temp_Nim_Record.Result_Index = 0;
         Temp_Record :=
           (Material_Types => Temp_Materials, Material_Amounts => Temp_Amount,
            Result_Index => Temp_Nim_Record.Result_Index,
            Result_Amount => Temp_Nim_Record.Result_Amount,
            Workplace => Module_Type'Val(Temp_Nim_Record.Workplace),
            Skill =>
              SkillsData_Container.Extended_Index(Temp_Nim_Record.Skill),
            Time => Temp_Nim_Record.Time,
            Difficulty => Temp_Nim_Record.Difficulty,
            Tool =>
              To_Bounded_String
                (Source =>
                   Interfaces.C.Strings.Value(Item => Temp_Nim_Record.Tool)),
            Reputation => Temp_Nim_Record.Reputation,
            Tool_Quality => Temp_Nim_Record.Tool_Quality);
         Index2 := 0;
         Temp_Record.Material_Types.Clear;
         Load_Material_Types_Loop :
         loop
            Material_Type :=
              To_Unbounded_String
                (Source =>
                   Interfaces.C.Strings.Value
                     (Item =>
                        Get_Ada_Recipe_Material_Type
                          (R_Index =>
                             New_String
                               (Str =>
                                  Trim(Source => Index'Img, Side => Left)),
                           Type_Index => Index2)));
            exit Load_Material_Types_Loop when Length
                (Source => Material_Type) =
              0;
            Temp_Record.Material_Types.Append
              (New_Item =>
                 To_Bounded_String
                   (Source => To_String(Source => Material_Type)));
            Index2 := Index2 + 1;
         end loop Load_Material_Types_Loop;
         Index2 := 0;
         Temp_Record.Material_Amounts.Clear;
         Load_Material_Amount_Loop :
         loop
            Material_Amount :=
              Get_Ada_Recipe_Material_Amount
                (R_Index =>
                   New_String(Str => Trim(Source => Index'Img, Side => Left)),
                 Type_Index => Index2);
            exit Load_Material_Amount_Loop when Material_Amount = 0;
            Temp_Record.Material_Amounts.Append(New_Item => Material_Amount);
            Index2 := Index2 + 1;
         end loop Load_Material_Amount_Loop;
         Recipes_List.Include
           (Key =>
              To_Bounded_String
                (Source => Trim(Source => Index'Img, Side => Left)),
            New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Recipes_Loop;
   end Load_Recipes;

   function Set_Recipe_Data
     (Recipe_Index: Tiny_String.Bounded_String) return Craft_Data is
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Recipe: Craft_Data;
      --## rule on IMPROPER_INITIALIZATION
      Item_Index: Objects_Container.Extended_Index := 0;
   begin
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Item_Index :=
           Positive'Value
             (Slice
                (Source => Recipe_Index, Low => 7,
                 High => Length(Source => Recipe_Index)));
         Recipe.Material_Types.Append
           (New_Item =>
              Objects_Container.Element
                (Container => Items_List, Index => Item_Index)
                .I_Type);
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Result_Index := Item_Index;
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
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Item_Index :=
           Positive'Value
             (Slice
                (Source => Recipe_Index, Low => 13,
                 High => Length(Source => Recipe_Index)));
         Recipe.Material_Types.Append
           (New_Item =>
              Objects_Container.Element
                (Container => Items_List, Index => Item_Index)
                .I_Type);
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Recipe_Data_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Item_Index then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               Recipe.Difficulty := ProtoRecipe.Difficulty;
               Recipe.Result_Index :=
                 Find_Proto_Item(Item_Type => ProtoRecipe.Material_Types(1));
               Recipe.Result_Amount :=
                 Positive
                   (Float'Ceiling
                      (Float
                         (ProtoRecipe.Material_Amounts.Element(Index => 1)) *
                       0.8));
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
      return
        Recipes_List
          (To_Bounded_String(Source => To_String(Source => Recipe_Index)));
   end Set_Recipe_Data;

   function Check_Recipe
     (Recipe_Index: Tiny_String.Bounded_String) return Positive is
      use Tiny_String;

      Recipe: Craft_Data;
      --## rule off IMPROPER_INITIALIZATION
      Material_Indexes: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      Recipe_Name: Unbounded_String;
      Max_Amount: Positive := Positive'Last;
      M_Type: Module_Type;
   begin
      Recipe := Set_Recipe_Data(Recipe_Index => Recipe_Index);
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Recipe_Name :=
           To_Unbounded_String(Source => "studying ") &
           To_String
             (Source =>
                Objects_Container.Element
                  (Container => Items_List,
                   Index =>
                     Positive'Value
                       (Slice
                          (Source => Recipe_Index, Low => 7,
                           High => Length(Source => Recipe_Index))))
                  .Name);
         M_Type := ALCHEMY_LAB;
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Recipe_Name :=
           To_Unbounded_String(Source => "deconstructing ") &
           To_String
             (Source =>
                Objects_Container.Element
                  (Container => Items_List,
                   Index =>
                     Positive'Value
                       (Slice
                          (Source => Recipe_Index, Low => 13,
                           High => Length(Source => Recipe_Index))))
                  .Name);
         M_Type := ALCHEMY_LAB;
      else
         Recipe_Name :=
           To_Unbounded_String(Source => "manufacturing ") &
           To_String
             (Source =>
                Objects_Container.Element
                  (Container => Items_List, Index => Recipe.Result_Index)
                  .Name);
         M_Type := Recipes_List(Recipe_Index).Workplace;
      end if;
      -- Check for workshop
      Check_For_Workshop_Block :
      declare
         Have_Workshop: Boolean := False;
      begin
         Check_For_Workshop_Loop :
         for Module of Player_Ship.Modules loop
            if BaseModules_Container.Element
                (Container => Modules_List, Index => Module.Proto_Index)
                .M_Type =
              M_Type and
              Module.Durability > 0 then
               Have_Workshop := True;
               exit Check_For_Workshop_Loop;
            end if;
         end loop Check_For_Workshop_Loop;
         if not Have_Workshop then
            raise Crafting_No_Workshop with To_String(Source => Recipe_Name);
         end if;
      end Check_For_Workshop_Block;
      -- Check for materials
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Study_Materials_Loop :
         for I in
           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
             Inventory_Container.Last_Index
               (Container => Player_Ship.Cargo) loop
            if Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => I)
                     .Proto_Index)
                .Name =
              Objects_Container.Element
                (Container => Items_List, Index => Recipe.Result_Index)
                .Name then
               Material_Indexes.Append(New_Item => I);
               exit Study_Materials_Loop;
            end if;
         end loop Study_Materials_Loop;
         Max_Amount := 1;
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Deconstruct_Materials_Loop :
         for I in
           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
             Inventory_Container.Last_Index
               (Container => Player_Ship.Cargo) loop
            if Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Proto_Index =
              Positive'Value
                (Slice
                   (Source => Recipe_Index, Low => 13,
                    High => Length(Source => Recipe_Index))) then
               Material_Indexes.Append(New_Item => I);
               Max_Amount :=
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Amount;
               exit Deconstruct_Materials_Loop;
            end if;
         end loop Deconstruct_Materials_Loop;
      else
         Find_Materials_Loop :
         for J in Recipe.Material_Types.Iterate loop
            Check_Player_Cargo_Loop :
            for I in
              Inventory_Container.First_Index
                (Container => Player_Ship.Cargo) ..
                Inventory_Container.Last_Index
                  (Container => Player_Ship.Cargo) loop
               if Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Proto_Index)
                   .I_Type =
                 Recipe.Material_Types(J) and
                 Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => I)
                     .Amount >=
                   Recipe.Material_Amounts
                     (TinyString_Container.To_Index(Position => J)) then
                  Material_Indexes.Append(New_Item => I);
                  if Max_Amount >
                    Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Amount /
                      Recipe.Material_Amounts
                        (TinyString_Container.To_Index(Position => J)) then
                     Max_Amount :=
                       Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => I)
                         .Amount /
                       Recipe.Material_Amounts
                         (TinyString_Container.To_Index(Position => J));
                  end if;
                  exit Check_Player_Cargo_Loop;
               end if;
            end loop Check_Player_Cargo_Loop;
         end loop Find_Materials_Loop;
      end if;
      if Material_Indexes.Length < Recipe.Material_Types.Length then
         raise Crafting_No_Materials with To_String(Source => Recipe_Name);
      end if;
      -- Check for tool
      Check_For_Tool_Block :
      declare
         Have_Tool: Boolean := False;
      begin
         if Recipe.Tool /= To_Bounded_String(Source => "None")
           and then
             Find_Item
               (Inventory => Player_Ship.Cargo, Item_Type => Recipe.Tool,
                Quality => Recipe.Tool_Quality) >
             0 then
            Have_Tool := True;
         elsif Recipe.Tool = To_Bounded_String(Source => "None") then
            Have_Tool := True;
         end if;
         if not Have_Tool then
            raise Crafting_No_Tools with To_String(Source => Recipe_Name);
         end if;
      end Check_For_Tool_Block;
      -- Check for free space
      Check_For_Free_Space_Block :
      declare
         Space_Needed: Integer := 0;
      begin
         Count_Needed_Space_Loop :
         for I in Material_Indexes.Iterate loop
            Space_Needed :=
              Space_Needed +
              Objects_Container.Element
                  (Container => Items_List,
                   Index =>
                     Inventory_Container.Element
                       (Container => Player_Ship.Cargo,
                        Index => Material_Indexes(I))
                       .Proto_Index)
                  .Weight *
                Recipe.Material_Amounts
                  (Positive_Container.To_Index(Position => I));
         end loop Count_Needed_Space_Loop;
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         if Free_Cargo
             (Amount =>
                Space_Needed -
                (Objects_Container.Element
                   (Container => Items_List, Index => Recipe.Result_Index)
                   .Weight *
                 Recipe.Result_Amount)) <
           0 then
            raise Trade_No_Free_Cargo;
         end if;
         --## rule on SIMPLIFIABLE_EXPRESSIONS
      end Check_For_Free_Space_Block;
      return Max_Amount;
   end Check_Recipe;

   procedure Manufacturing(Minutes: Positive) is
      use Tiny_String;

      Result_Amount, Crafted_Amount, Gained_Exp: Natural := 0;
      Amount, New_Amount: Integer := 0;
      --## rule off IMPROPER_INITIALIZATION
      Recipe: Craft_Data;
      Material_Indexes: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      Work_Time, Current_Minutes, Recipe_Time: Integer := 0;
      Damage: Damage_Factor := 0.0;
      Recipe_Name: Unbounded_String := Null_Unbounded_String;
      Have_Material: Boolean := False;
      Crafting_Material: Natural := 0;
      Crafter_Index: Crew_Container.Extended_Index := 0;
      Cargo_Index, Tool_Index: Inventory_Container.Extended_Index := 0;
      procedure Reset_Order
        (Module: in out Module_Data; Module_Owner: Natural) is
         Have_Worker: Boolean := False;
      begin
         if Tool_Index in
             Inventory_Container.First_Index
                   (Container => Player_Ship.Crew(Crafter_Index).Inventory) ..
                   Inventory_Container.Last_Index
                     (Container =>
                        Player_Ship.Crew(Crafter_Index).Inventory) then
            Update_Cargo
              (Ship => Player_Ship,
               Proto_Index =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Crew(Crafter_Index).Inventory,
                    Index => Tool_Index)
                   .Proto_Index,
               Amount => 1,
               Durability =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Crew(Crafter_Index).Inventory,
                    Index => Tool_Index)
                   .Durability);
            Update_Inventory
              (Member_Index => Crafter_Index, Amount => -1,
               Inventory_Index => Tool_Index, Ship => Player_Ship);
         end if;
         Check_Owner_Loop :
         for Owner of Module.Owner loop
            if Owner = Module_Owner or Module_Owner = 0 then
               if Owner in
                   Player_Ship.Crew.First_Index ..
                         Player_Ship.Crew.Last_Index then
                  Give_Orders
                    (Ship => Player_Ship, Member_Index => Owner,
                     Given_Order => REST);
               end if;
               Owner := 0;
            end if;
            if Owner > 0 then
               Have_Worker := True;
            end if;
         end loop Check_Owner_Loop;
         if not Have_Worker then
            Module.Crafting_Index := Null_Bounded_String;
            Module.Crafting_Time := 0;
            Module.Crafting_Amount := 0;
         end if;
      end Reset_Order;
   begin
      Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type /= WORKSHOP then
            goto End_Of_Loop;
         end if;
         if Module.Crafting_Index = Null_Bounded_String then
            goto End_Of_Loop;
         end if;
         Owners_Loop :
         for Owner of Module.Owner loop
            if Owner = 0 then
               goto End_Of_Owners_Loop;
            end if;
            Crafter_Index := Owner;
            if Player_Ship.Crew(Crafter_Index).Order = CRAFT then
               Current_Minutes := Minutes;
               Recipe_Time := Module.Crafting_Time;
               Recipe :=
                 Set_Recipe_Data
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source =>
                           To_String(Source => Module.Crafting_Index)));
               if Length(Source => Module.Crafting_Index) > 6
                 and then
                   Slice
                     (Source => Module.Crafting_Index, Low => 1, High => 5) =
                   "Study" then
                  Recipe_Name :=
                    To_Unbounded_String(Source => "studying ") &
                    To_String
                      (Source =>
                         Objects_Container.Element
                           (Container => Items_List,
                            Index => Recipe.Result_Index)
                           .Name);
               elsif Length(Source => Module.Crafting_Index) > 12
                 and then
                   Slice
                     (Source => Module.Crafting_Index, Low => 1, High => 11) =
                   "Deconstruct" then
                  Recipe_Name :=
                    To_Unbounded_String(Source => "deconstructing ") &
                    To_String
                      (Source =>
                         Objects_Container.Element
                           (Container => Items_List,
                            Index =>
                              Positive'Value
                                (Slice
                                   (Source => Module.Crafting_Index, Low => 13,
                                    High =>
                                      Length
                                        (Source => Module.Crafting_Index))))
                           .Name);
               else
                  Recipe_Name :=
                    To_Unbounded_String(Source => "manufacturing ") &
                    To_String
                      (Source =>
                         Objects_Container.Element
                           (Container => Items_List,
                            Index => Recipe.Result_Index)
                           .Name);
               end if;
               if Module.Durability = 0 then
                  Add_Message
                    (Message =>
                       To_String(Source => Module.Name) &
                       " is destroyed, so " &
                       To_String
                         (Source => Player_Ship.Crew(Crafter_Index).Name) &
                       " can't work on " & To_String(Source => Recipe_Name) &
                       ".",
                     M_Type => CRAFTMESSAGE, Color => RED);
                  Reset_Order(Module => Module, Module_Owner => Owner);
                  Current_Minutes := 0;
               end if;
               Work_Time := Player_Ship.Crew(Crafter_Index).Order_Time;
               Crafted_Amount := 0;
               Craft_Loop :
               while Current_Minutes > 0 loop
                  if Current_Minutes < Recipe_Time then
                     Recipe_Time := Recipe_Time - Current_Minutes;
                     Work_Time := Work_Time - Current_Minutes;
                     Current_Minutes := 0;
                     goto End_Of_Craft_Loop;
                  end if;
                  Recipe_Time := Recipe_Time - Current_Minutes;
                  Work_Time := Work_Time - Current_Minutes - Recipe_Time;
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Current_Minutes := -(Recipe_Time);
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
                  Recipe_Time := Recipe.Time; --## rule line off ASSIGNMENTS
                  Material_Indexes.Clear;
                  if Length(Source => Module.Crafting_Index) > 6
                    and then
                      Slice
                        (Source => Module.Crafting_Index, Low => 1,
                         High => 5) =
                      "Study" then
                     Study_Materials_Loop :
                     for J in
                       Objects_Container.First_Index
                         (Container => Items_List) ..
                         Objects_Container.Last_Index
                           (Container => Items_List) loop
                        if Objects_Container.Element
                            (Container => Items_List, Index => J)
                            .Name =
                          Objects_Container.Element
                            (Container => Items_List,
                             Index => Recipe.Result_Index)
                            .Name then
                           Material_Indexes.Append(New_Item => J);
                           exit Study_Materials_Loop;
                        end if;
                     end loop Study_Materials_Loop;
                  elsif Length(Source => Module.Crafting_Index) > 12
                    and then
                      Slice
                        (Source => Module.Crafting_Index, Low => 1,
                         High => 11) =
                      "Deconstruct" then
                     Material_Indexes.Append
                       (New_Item =>
                          Positive'Value
                            (Slice
                               (Source => Module.Crafting_Index, Low => 13,
                                High =>
                                  Length(Source => Module.Crafting_Index))));
                  else
                     Recipe_Loop :
                     for K in Recipe.Material_Types.Iterate loop
                        Materials_Loop :
                        for J in
                          Objects_Container.First_Index
                            (Container => Items_List) ..
                            Objects_Container.Last_Index
                              (Container => Items_List) loop
                           if Objects_Container.Element
                               (Container => Items_List, Index => J)
                               .I_Type =
                             Recipe.Material_Types
                               (TinyString_Container.To_Index
                                  (Position => K)) then
                              Material_Indexes.Append(New_Item => J);
                              exit Materials_Loop;
                           end if;
                        end loop Materials_Loop;
                     end loop Recipe_Loop;
                  end if;
                  Crafting_Material := 0;
                  Check_Materials_Loop :
                  for MaterialIndex of Material_Indexes loop
                     Crafting_Material :=
                       Find_Item
                         (Inventory => Player_Ship.Cargo,
                          Item_Type =>
                            Objects_Container.Element
                              (Container => Items_List, Index => MaterialIndex)
                              .I_Type);
                     if Crafting_Material = 0 then
                        Add_Message
                          (Message =>
                             "You don't have the crafting materials for " &
                             To_String(Source => Recipe_Name) & ".",
                           M_Type => CRAFTMESSAGE, Color => RED);
                        Reset_Order(Module => Module, Module_Owner => Owner);
                        exit Craft_Loop;
                     elsif Inventory_Container.Element
                         (Container => Player_Ship.Cargo,
                          Index => Crafting_Material)
                         .Proto_Index /=
                       MaterialIndex then
                        MaterialIndex :=
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Crafting_Material)
                            .Proto_Index;
                     end if;
                  end loop Check_Materials_Loop;
                  if Recipe.Tool = To_Bounded_String(Source => "None") then
                     Tool_Index := 0;
                  else
                     Tool_Index :=
                       Find_Tools
                         (Member_Index => Crafter_Index,
                          Item_Type => Recipe.Tool, Order => CRAFT,
                          Tool_Quality => Recipe.Tool_Quality);
                     if Tool_Index = 0 then
                        Add_Message
                          (Message =>
                             "You don't have the tool for " &
                             To_String(Source => Recipe_Name) & ".",
                           M_Type => CRAFTMESSAGE, Color => RED);
                        Reset_Order(Module => Module, Module_Owner => Owner);
                        exit Craft_Loop;
                     end if;
                  end if;
                  Amount := 0;
                  Count_Amount_Loop :
                  for J in Material_Indexes.Iterate loop
                     Amount :=
                       Amount +
                       Objects_Container.Element
                           (Container => Items_List,
                            Index => Material_Indexes(J))
                           .Weight *
                         Recipe.Material_Amounts
                           (Positive_Container.To_Index(Position => J));
                  end loop Count_Amount_Loop;
                  Result_Amount :=
                    Recipe.Result_Amount +
                    Integer
                      (Float'Floor
                         (Float(Recipe.Result_Amount) *
                          (Float
                             (Get_Skill_Level
                                (Member => Player_Ship.Crew(Crafter_Index),
                                 Skill_Index => Recipe.Skill)) /
                           100.0)));
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(Module.Durability) /
                       Float(Module.Max_Durability));
                  --## rule off ASSIGNMENTS
                  Result_Amount :=
                    Result_Amount -
                    Natural(Float(Result_Amount) * Float(Damage));
                  --## rule on ASSIGNMENTS
                  if Result_Amount = 0 then
                     Result_Amount := 1;
                  end if;
                  Check_Enough_Materials_Loop :
                  for J in Material_Indexes.Iterate loop
                     Have_Material := False;
                     Check_Cargo_Materials_Loop :
                     for Item of Player_Ship.Cargo loop
                        if Objects_Container.Element
                            (Container => Items_List,
                             Index => Item.Proto_Index)
                            .I_Type =
                          Objects_Container.Element
                            (Container => Items_List,
                             Index => Material_Indexes(J))
                            .I_Type and
                          Item.Amount >=
                            Recipe.Material_Amounts
                              (Positive_Container.To_Index(Position => J)) then
                           Have_Material := True;
                           exit Check_Cargo_Materials_Loop;
                        end if;
                     end loop Check_Cargo_Materials_Loop;
                     exit Check_Enough_Materials_Loop when not Have_Material;
                  end loop Check_Enough_Materials_Loop;
                  if not Have_Material then
                     Add_Message
                       (Message =>
                          "You don't have enough crafting materials for " &
                          To_String(Source => Recipe_Name) & ".",
                        M_Type => CRAFTMESSAGE, Color => RED);
                     Reset_Order(Module => Module, Module_Owner => Owner);
                     exit Craft_Loop;
                  end if;
                  Crafted_Amount := Crafted_Amount + Result_Amount;
                  Module.Crafting_Amount := Module.Crafting_Amount - 1;
                  Remove_Materials_Loop :
                  for J in Material_Indexes.Iterate loop
                     Cargo_Index := 1;
                     Remove_Materials_From_Cargo_Loop :
                     while Cargo_Index <=
                       Inventory_Container.Last_Index
                         (Container => Player_Ship.Cargo) loop
                        Remove_Materials_From_Cargo_Block :
                        declare
                           Material: Inventory_Data :=
                             Inventory_Container.Element
                               (Container => Player_Ship.Cargo,
                                Index => Cargo_Index);
                        begin
                           if Objects_Container.Element
                               (Container => Items_List,
                                Index => Material.Proto_Index)
                               .I_Type =
                             Objects_Container.Element
                               (Container => Items_List,
                                Index => Material_Indexes(J))
                               .I_Type then
                              if Material.Amount >
                                Recipe.Material_Amounts
                                  (Positive_Container.To_Index
                                     (Position => J)) then
                                 New_Amount :=
                                   Material.Amount -
                                   Recipe.Material_Amounts
                                     (Positive_Container.To_Index
                                        (Position => J));
                                 Material.Amount := New_Amount;
                                 Inventory_Container.Replace_Element
                                   (Container => Player_Ship.Cargo,
                                    Index => Cargo_Index,
                                    New_Item => Material);
                                 exit Remove_Materials_From_Cargo_Loop;
                              elsif Material.Amount =
                                Recipe.Material_Amounts
                                  (Positive_Container.To_Index
                                     (Position => J)) then
                                 Inventory_Container.Delete
                                   (Container => Player_Ship.Cargo,
                                    Index => Cargo_Index, Count => 1);
                                 if Tool_Index > Cargo_Index then
                                    Tool_Index := Tool_Index - 1;
                                 end if;
                                 exit Remove_Materials_From_Cargo_Loop;
                              end if;
                           end if;
                        end Remove_Materials_From_Cargo_Block;
                        Cargo_Index := Cargo_Index + 1;
                     end loop Remove_Materials_From_Cargo_Loop;
                  end loop Remove_Materials_Loop;
                  if Tool_Index > 0 then
                     Damage_Item
                       (Inventory => Player_Ship.Crew(Crafter_Index).Inventory,
                        Item_Index => Tool_Index,
                        Skill_Level =>
                          Get_Skill_Level
                            (Member => Player_Ship.Crew(Crafter_Index),
                             Skill_Index => Recipe.Skill),
                        Member_Index => Crafter_Index, Ship => Player_Ship);
                  end if;
                  if Length(Source => Module.Crafting_Index) < 6
                    or else
                    (Length(Source => Module.Crafting_Index) > 6
                     and then
                       Slice
                         (Source => Module.Crafting_Index, Low => 1,
                          High => 5) /=
                       "Study") then
                     Amount :=
                       Amount -
                       (Objects_Container.Element
                          (Container => Items_List,
                           Index => Recipe.Result_Index)
                          .Weight *
                        Result_Amount);
                     if Free_Cargo(Amount => Amount) < 0 then
                        Add_Message
                          (Message =>
                             "You don't have the free cargo space for " &
                             To_String(Source => Recipe_Name) & ".",
                           M_Type => CRAFTMESSAGE, Color => RED);
                        Reset_Order(Module => Module, Module_Owner => Owner);
                        exit Craft_Loop;
                     end if;
                     if Length(Source => Module.Crafting_Index) > 11
                       and then
                         Slice
                           (Source => Module.Crafting_Index, Low => 1,
                            High => 11) =
                         "Deconstruct" then
                        Update_Cargo
                          (Ship => Player_Ship,
                           Proto_Index => Recipe.Result_Index,
                           Amount => Result_Amount);
                     else
                        Update_Cargo
                          (Ship => Player_Ship,
                           Proto_Index =>
                             Recipes_List(Module.Crafting_Index).Result_Index,
                           Amount => Result_Amount);
                     end if;
                     Update_Crafting_Orders_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           Update_Crafting_Orders
                             (Index => Recipes_Container.Key(Position => I));
                           exit Update_Crafting_Orders_Loop;
                        end if;
                     end loop Update_Crafting_Orders_Loop;
                  else
                     Learn_Recipe_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           Known_Recipes.Append
                             (New_Item =>
                                Recipes_Container.Key(Position => I));
                           exit Learn_Recipe_Loop;
                        end if;
                     end loop Learn_Recipe_Loop;
                     exit Craft_Loop;
                  end if;
                  exit Craft_Loop when Module.Crafting_Amount = 0;
                  <<End_Of_Craft_Loop>>
               end loop Craft_Loop;
               Module.Crafting_Time := Recipe_Time;
               if Crafted_Amount > 0 then
                  if Recipe.Result_Amount > 0 then
                     if Length(Source => Module.Crafting_Index) > 12
                       and then
                         Slice
                           (Source => Module.Crafting_Index, Low => 1,
                            High => 11) =
                         "Deconstruct" then
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Crafter_Index).Name) &
                             " has recovered" & Integer'Image(Crafted_Amount) &
                             " " &
                             To_String
                               (Source =>
                                  Objects_Container.Element
                                    (Container => Items_List,
                                     Index => Recipe.Result_Index)
                                    .Name) &
                             ".",
                           M_Type => CRAFTMESSAGE, Color => GREEN);
                     else
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Crafter_Index).Name) &
                             " has manufactured" &
                             Integer'Image(Crafted_Amount) & " " &
                             To_String
                               (Source =>
                                  Objects_Container.Element
                                    (Container => Items_List,
                                     Index => Recipe.Result_Index)
                                    .Name) &
                             ".",
                           M_Type => CRAFTMESSAGE, Color => GREEN);
                     end if;
                     Update_Goal_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           Update_Goal
                             (G_Type => CRAFT,
                              Target_Index =>
                                To_Unbounded_String
                                  (Source =>
                                     To_String
                                       (Source =>
                                          Recipes_Container.Key
                                            (Position => I))),
                              Amount => Crafted_Amount);
                           exit Update_Goal_Loop;
                        end if;
                     end loop Update_Goal_Loop;
                     if Current_Goal.Target_Index /= Null_Unbounded_String then
                        Update_Goal
                          (G_Type => CRAFT,
                           Target_Index =>
                             To_Unbounded_String
                               (Source =>
                                  To_String
                                    (Source =>
                                       Objects_Container.Element
                                         (Container => Items_List,
                                          Index => Recipe.Result_Index)
                                         .I_Type)),
                           Amount => Crafted_Amount);
                        if Objects_Container.Element
                            (Container => Items_List,
                             Index => Recipe.Result_Index)
                            .Show_Type /=
                          Null_Bounded_String then
                           Update_Goal
                             (G_Type => CRAFT,
                              Target_Index =>
                                To_Unbounded_String
                                  (Source =>
                                     To_String
                                       (Source =>
                                          Objects_Container.Element
                                            (Container => Items_List,
                                             Index => Recipe.Result_Index)
                                            .Show_Type)),
                              Amount => Crafted_Amount);
                        end if;
                     end if;
                  else
                     Add_Message
                       (Message =>
                          To_String
                            (Source => Player_Ship.Crew(Crafter_Index).Name) &
                          " has discovered recipe for " &
                          To_String
                            (Source =>
                               Objects_Container.Element
                                 (Container => Items_List,
                                  Index => Recipe.Result_Index)
                                 .Name) &
                          ".",
                        M_Type => CRAFTMESSAGE, Color => GREEN);
                     Update_Goal
                       (G_Type => CRAFT,
                        Target_Index => Null_Unbounded_String);
                  end if;
               end if;
               if Player_Ship.Crew(Crafter_Index).Order = CRAFT then
                  Update_Work_Time_Loop :
                  while Work_Time <= 0 loop
                     Gained_Exp := Gained_Exp + 1;
                     Work_Time := Work_Time + 15;
                  end loop Update_Work_Time_Loop;
                  if Gained_Exp > 0 then
                     Gain_Exp
                       (Amount => Gained_Exp, Skill_Number => Recipe.Skill,
                        Crew_Index => Crafter_Index);
                  end if;
                  Player_Ship.Crew(Crafter_Index).Order_Time := Work_Time;
                  if Module.Crafting_Amount = 0 then
                     Reset_Order(Module => Module, Module_Owner => Owner);
                  end if;
               end if;
            end if;
            <<End_Of_Owners_Loop>>
         end loop Owners_Loop;
         <<End_Of_Loop>>
      end loop Modules_Loop;
   exception
      when An_Exception : Crew_No_Space_Error =>
         Add_Message
           (Message => Exception_Message(X => An_Exception),
            M_Type => ORDERMESSAGE, Color => RED);
         Give_Orders
           (Ship => Player_Ship, Member_Index => Crafter_Index,
            Given_Order => REST);
   end Manufacturing;

   procedure Set_Recipe
     (Workshop, Amount: Positive; Recipe_Index: Tiny_String.Bounded_String) is
      use Tiny_String;

      Item_Index: Objects_Container.Extended_Index;
      Recipe_Name: Unbounded_String;
   begin
      Player_Ship.Modules(Workshop).Crafting_Amount := Amount;
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Item_Index :=
           Positive'Value
             (Slice
                (Source => Recipe_Index, Low => 7,
                 High => Length(Source => Recipe_Index)));
         Set_Study_Difficulty_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Item_Index then
               Player_Ship.Modules(Workshop).Crafting_Time :=
                 ProtoRecipe.Difficulty * 15;
               exit Set_Study_Difficulty_Loop;
            end if;
         end loop Set_Study_Difficulty_Loop;
         Recipe_Name :=
           To_Unbounded_String(Source => "Studying ") &
           To_String
             (Source =>
                Objects_Container.Element
                  (Container => Items_List, Index => Item_Index)
                  .Name);
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Item_Index :=
           Positive'Value
             (Slice
                (Source => Recipe_Index, Low => 13,
                 High => Length(Source => Recipe_Index)));
         Set_Deconstruct_Difficulty_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Item_Index then
               Player_Ship.Modules(Workshop).Crafting_Time :=
                 ProtoRecipe.Difficulty * 15;
               exit Set_Deconstruct_Difficulty_Loop;
            end if;
         end loop Set_Deconstruct_Difficulty_Loop;
         Recipe_Name :=
           To_Unbounded_String(Source => "Deconstructing ") &
           To_String
             (Source =>
                Objects_Container.Element
                  (Container => Items_List, Index => Item_Index)
                  .Name);
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
      else
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
         Player_Ship.Modules(Workshop).Crafting_Time :=
           Recipes_List(Recipe_Index).Time;
         Recipe_Name :=
           To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     Objects_Container.Element
                       (Container => Items_List,
                        Index => Recipes_List(Recipe_Index).Result_Index)
                       .Name));
      end if;
      Add_Message
        (Message =>
           To_String(Source => Recipe_Name) &
           " was set as manufacturing order in " &
           To_String(Source => Player_Ship.Modules(Workshop).Name) & ".",
         M_Type => CRAFTMESSAGE);
      Update_Orders(Ship => Player_Ship);
   end Set_Recipe;

   function Get_Workshop_Recipe_Name(Workshop: Positive) return String is
      use Tiny_String;
      Module: constant Module_Data := Player_Ship.Modules(Workshop);
   begin
      if Module.Crafting_Index /= Tiny_String.Null_Bounded_String then
         if Length(Source => Module.Crafting_Index) > 6
           and then
             Slice(Source => Module.Crafting_Index, Low => 1, High => 5) =
             "Study" then
            return
              "Studying " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Positive'Value
                          (Slice
                             (Source => Module.Crafting_Index, Low => 7,
                              High =>
                                Length(Source => Module.Crafting_Index))))
                     .Name);
         elsif Length(Source => Module.Crafting_Index) > 12
           and then
             Slice(Source => Module.Crafting_Index, Low => 1, High => 11) =
             "Deconstruct" then
            return
              "Deconstructing " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Positive'Value
                          (Slice
                             (Source => Module.Crafting_Index, Low => 13,
                              High =>
                                Length(Source => Module.Crafting_Index))))
                     .Name);
         else
            return
              "Manufacturing" & Positive'Image(Module.Crafting_Amount) & "x " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Recipes_List
                          (To_Bounded_String
                             (Source =>
                                To_String(Source => Module.Crafting_Index)))
                          .Result_Index)
                     .Name);
         end if;
      end if;
      return "";
   end Get_Workshop_Recipe_Name;

end Crafts;
