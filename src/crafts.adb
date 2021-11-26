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
      use Tiny_String;

      Temp_Record: Craft_Data;
      Temp_Materials: UnboundedString_Container.Vector;
      Temp_Amount: Positive_Container.Vector;
      Recipes_Data: Document;
      Nodes_List, Child_Nodes: Node_List;
      Amount, Delete_Index: Natural;
      Item_Index: Tiny_String.Bounded_String;
      Recipe_Index, Value: Unbounded_String;
      Recipe_Node, Child_Node: Node;
      Material_Added: Boolean;
      Action: Data_Action;
      Skill_Index: Skills_Container.Extended_Index;
   begin
      Recipes_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Recipes_Data, Tag_Name => "recipe");
      Load_Recipes_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Material_Types => Temp_Materials, Material_Amounts => Temp_Amount,
            Result_Index => Tiny_String.Null_Bounded_String, Result_Amount => 10_000,
            Workplace => ALCHEMY_LAB, Skill => 1, Time => 15, Difficulty => 1,
            Tool => To_Unbounded_String(Source => "None"), Reputation => -100,
            Tool_Quality => 100);
         Recipe_Node := Item(List => Nodes_List, Index => I);
         Recipe_Index :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Recipe_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Recipe_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Recipe_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Recipes_Container.Contains
                (Container => Recipes_List, Key => Recipe_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " recipe '" & To_String(Source => Recipe_Index) &
                 "', there is no recipe with that index.";
            end if;
         elsif Recipes_Container.Contains
             (Container => Recipes_List, Key => Recipe_Index) then
            raise Data_Loading_Error
              with "Can't add recipe '" & To_String(Source => Recipe_Index) &
              "', there is already a recipe with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Recipes_List(Recipe_Index);
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Recipe_Node, Name => "material");
            Read_Materials_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Amount :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "amount"));
               Value :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "type"));
               if Amount > 0 then
                  Material_Added := False;
                  Check_Added_Materials_Loop :
                  for K in
                    Temp_Record.Material_Types.First_Index ..
                      Temp_Record.Material_Types.Last_Index loop
                     if Temp_Record.Material_Types(K) = Value then
                        Temp_Record.Material_Amounts(K) := Amount;
                        Material_Added := True;
                        exit Check_Added_Materials_Loop;
                     end if;
                  end loop Check_Added_Materials_Loop;
                  if not Material_Added then
                     Temp_Record.Material_Types.Append(New_Item => Value);
                     Temp_Record.Material_Amounts.Append(New_Item => Amount);
                  end if;
               else
                  Delete_Index := Temp_Record.Material_Types.First_Index;
                  Delete_Materials_Loop :
                  while Delete_Index <=
                    Temp_Record.Material_Types.Last_Index loop
                     if Temp_Record.Material_Types(Delete_Index) = Value then
                        Temp_Record.Material_Types.Delete
                          (Index => Delete_Index);
                        exit Delete_Materials_Loop;
                     end if;
                     Delete_Index := Delete_Index + 1;
                  end loop Delete_Materials_Loop;
               end if;
            end loop Read_Materials_Loop;
            Value :=
              To_Unbounded_String
                (Source =>
                   Get_Attribute(Elem => Recipe_Node, Name => "result"));
            if Value /= Null_Unbounded_String then
               Item_Index := To_Bounded_String(Source => To_String(Source => Value));
               if Item_Index = Null_Bounded_String then
                  raise Data_Loading_Error
                    with "Can't add recipe '" &
                    To_String(Source => Recipe_Index) &
                    "', result item index '" & To_String(Source => Value) &
                    "' does't exist.";
               end if;
               Temp_Record.Result_Index := Item_Index;
            end if;
            Value :=
              To_Unbounded_String
                (Source =>
                   Get_Attribute(Elem => Recipe_Node, Name => "crafted"));
            if Value /= Null_Unbounded_String then
               Temp_Record.Result_Amount :=
                 Positive'Value(To_String(Source => Value));
            end if;
            Value :=
              To_Unbounded_String
                (Source =>
                   Get_Attribute(Elem => Recipe_Node, Name => "workplace"));
            if Value /= Null_Unbounded_String then
               Temp_Record.Workplace :=
                 ModuleType'Value(To_String(Source => Value));
            end if;
            Value :=
              To_Unbounded_String
                (Source =>
                   Get_Attribute(Elem => Recipe_Node, Name => "skill"));
            if Value /= Null_Unbounded_String then
               Skill_Index :=
                 Find_Skill_Index(Skill_Name => To_String(Source => Value));
               if Skill_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't add recipe '" &
                    To_String(Source => Recipe_Index) & "', no skill named '" &
                    To_String(Source => Value) & "'";
               end if;
               Temp_Record.Skill := Skill_Index;
            end if;
            if Get_Attribute(Elem => Recipe_Node, Name => "time") /= "" then
               Temp_Record.Time :=
                 Positive'Value
                   (Get_Attribute(Elem => Recipe_Node, Name => "time"));
            end if;
            if Get_Attribute(Elem => Recipe_Node, Name => "difficulty") /=
              "" then
               Temp_Record.Difficulty :=
                 Positive'Value
                   (Get_Attribute(Elem => Recipe_Node, Name => "difficulty"));
            end if;
            if Get_Attribute(Elem => Recipe_Node, Name => "tool") /= "" then
               Temp_Record.Tool :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Recipe_Node, Name => "tool"));
            end if;
            Value :=
              To_Unbounded_String
                (Source =>
                   Get_Attribute(Elem => Recipe_Node, Name => "reputation"));
            if Value /= Null_Unbounded_String then
               Temp_Record.Reputation :=
                 Integer'Value(To_String(Source => Value));
            end if;
            Value :=
              To_Unbounded_String
                (Source =>
                   Get_Attribute(Elem => Recipe_Node, Name => "Tool_Quality"));
            if Value /= Null_Unbounded_String then
               Temp_Record.Tool_Quality :=
                 Positive'Value(To_String(Source => Value));
            end if;
            if Action /= UPDATE then
               Recipes_Container.Include
                 (Container => Recipes_List, Key => Recipe_Index,
                  New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Recipe added: " &
                    To_String
                      (Source => Items_List(Temp_Record.Result_Index).Name),
                  Message_Type => EVERYTHING);
            else
               Recipes_List(Recipe_Index) := Temp_Record;
               Log_Message
                 (Message =>
                    "Recipe updated: " &
                    To_String
                      (Source => Items_List(Temp_Record.Result_Index).Name),
                  Message_Type => EVERYTHING);
            end if;
         else
            Recipes_Container.Exclude
              (Container => Recipes_List, Key => Recipe_Index);
            Log_Message
              (Message =>
                 "Recipe removed: " & To_String(Source => Recipe_Index),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Recipes_Loop;
   end Load_Recipes;

   function Set_Recipe_Data
     (Recipe_Index: Unbounded_String) return Craft_Data is
      use Tiny_String;

      Recipe: Craft_Data;
      Item_Index: Bounded_String;
   begin
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Item_Index :=
           To_Bounded_String(Source => Slice
             (Source => Recipe_Index, Low => 7,
              High => Length(Source => Recipe_Index)));
         Recipe.Material_Types.Append
           (New_Item => Items_List(Item_Index).IType);
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
           To_Bounded_String(Source => Slice
             (Source => Recipe_Index, Low => 13,
              High => Length(Source => Recipe_Index)));
         Recipe.Material_Types.Append
           (New_Item => Items_List(Item_Index).IType);
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Recipe_Data_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Item_Index then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               Recipe.Difficulty := ProtoRecipe.Difficulty;
               Recipe.Result_Index :=
                 FindProtoItem(ItemType => ProtoRecipe.Material_Types(1));
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
      return Recipes_List(Recipe_Index);
   end Set_Recipe_Data;

   function Check_Recipe(Recipe_Index: Unbounded_String) return Positive is
      use Tiny_String;

      Recipe: Craft_Data;
      Material_Indexes: Positive_Container.Vector;
      Recipe_Name: Unbounded_String;
      Max_Amount: Positive := Positive'Last;
      M_Type: ModuleType;
   begin
      Recipe := Set_Recipe_Data(Recipe_Index => Recipe_Index);
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Recipe_Name :=
           To_Unbounded_String(Source => "studying ") &
           Items_List
             (To_Bounded_String(Source => Slice
                (Source => Recipe_Index, Low => 7,
                 High => Length(Source => Recipe_Index))))
             .Name;
         M_Type := ALCHEMY_LAB;
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Recipe_Name :=
           To_Unbounded_String(Source => "deconstructing ") &
           Items_List
             (To_Bounded_String(Source => Slice
                (Source => Recipe_Index, Low => 13,
                 High => Length(Source => Recipe_Index))))
             .Name;
         M_Type := ALCHEMY_LAB;
      else
         Recipe_Name :=
           To_Unbounded_String(Source => "manufacturing ") &
           Items_List(Recipe.Result_Index).Name;
         M_Type := Recipes_List(Recipe_Index).Workplace;
      end if;
      -- Check for workshop
      Check_For_Workshop_Block :
      declare
         Have_Workshop: Boolean := False;
      begin
         Check_For_Workshop_Loop :
         for Module of Player_Ship.Modules loop
            if Modules_List(Module.Proto_Index).MType = M_Type and
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
         for I in Player_Ship.Cargo.Iterate loop
            if Items_List(Player_Ship.Cargo(I).ProtoIndex).Name =
              Items_List(Recipe.Result_Index).Name then
               Material_Indexes.Append
                 (New_Item => Inventory_Container.To_Index(Position => I));
               exit Study_Materials_Loop;
            end if;
         end loop Study_Materials_Loop;
         Max_Amount := 1;
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Deconstruct_Materials_Loop :
         for I in Player_Ship.Cargo.Iterate loop
            if Player_Ship.Cargo(I).ProtoIndex =
              To_Bounded_String(Source => Slice
                (Source => Recipe_Index, Low => 13,
                 High => Length(Source => Recipe_Index))) then
               Material_Indexes.Append
                 (New_Item => Inventory_Container.To_Index(Position => I));
               Max_Amount := Player_Ship.Cargo(I).Amount;
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
                     (UnboundedString_Container.To_Index(Position => J)) then
                  Material_Indexes.Append
                    (New_Item => Inventory_Container.To_Index(Position => I));
                  if Max_Amount >
                    Player_Ship.Cargo(I).Amount /
                      Recipe.Material_Amounts
                        (UnboundedString_Container.To_Index
                           (Position => J)) then
                     Max_Amount :=
                       Player_Ship.Cargo(I).Amount /
                       Recipe.Material_Amounts
                         (UnboundedString_Container.To_Index(Position => J));
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
         if Recipe.Tool /= To_Unbounded_String(Source => "None")
           and then
             FindItem
               (Inventory => Player_Ship.Cargo, ItemType => Recipe.Tool,
                Quality => Recipe.Tool_Quality) >
             0 then
            Have_Tool := True;
         elsif Recipe.Tool = To_Unbounded_String(Source => "None") then
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
              Items_List(Player_Ship.Cargo(Material_Indexes(I)).ProtoIndex)
                  .Weight *
                Recipe.Material_Amounts
                  (Positive_Container.To_Index(Position => I));
         end loop Count_Needed_Space_Loop;
         if FreeCargo
             (Amount =>
                Space_Needed -
                (Items_List(Recipe.Result_Index).Weight *
                 Recipe.Result_Amount)) <
           0 then
            raise Trade_No_Free_Cargo;
         end if;
      end Check_For_Free_Space_Block;
      return Max_Amount;
   end Check_Recipe;

   procedure Manufacturing(Minutes: Positive) is
      Result_Amount, Crafted_Amount, Gained_Exp: Natural := 0;
      Amount, New_Amount: Integer := 0;
      Recipe: Craft_Data;
      Material_Indexes: UnboundedString_Container.Vector;
      Work_Time, Current_Minutes, Recipe_Time: Integer;
      Damage: Damage_Factor := 0.0;
      Recipe_Name: Unbounded_String;
      Have_Material: Boolean;
      Crafting_Material: Natural;
      Crafter_Index: Crew_Container.Extended_Index;
      Cargo_Index, Tool_Index: Inventory_Container.Extended_Index;
      procedure Reset_Order
        (Module: in out Module_Data; Module_Owner: Natural) is
         Have_Worker: Boolean := False;
      begin
         if Tool_Index in
             Player_Ship.Crew(Crafter_Index).Inventory.First_Index ..
                   Player_Ship.Crew(Crafter_Index).Inventory.Last_Index then
            UpdateCargo
              (Ship => Player_Ship,
               ProtoIndex =>
                 Player_Ship.Crew(Crafter_Index).Inventory(Tool_Index)
                   .ProtoIndex,
               Amount => 1,
               Durability =>
                 Player_Ship.Crew(Crafter_Index).Inventory(Tool_Index)
                   .Durability);
            UpdateInventory
              (MemberIndex => Crafter_Index, Amount => -1,
               InventoryIndex => Tool_Index);
         end if;
         Check_Owner_Loop :
         for Owner of Module.Owner loop
            if Owner = Module_Owner or Module_Owner = 0 then
               if Owner in
                   Player_Ship.Crew.First_Index ..
                         Player_Ship.Crew.Last_Index then
                  GiveOrders
                    (Ship => Player_Ship, MemberIndex => Owner,
                     GivenOrder => REST);
               end if;
               Owner := 0;
            end if;
            if Owner > 0 then
               Have_Worker := True;
            end if;
         end loop Check_Owner_Loop;
         if not Have_Worker then
            Module.Crafting_Index := Null_Unbounded_String;
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
         if Module.Crafting_Index = Null_Unbounded_String then
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
                 Set_Recipe_Data(Recipe_Index => Module.Crafting_Index);
               if Length(Source => Module.Crafting_Index) > 6
                 and then
                   Slice
                     (Source => Module.Crafting_Index, Low => 1, High => 5) =
                   "Study" then
                  Recipe_Name :=
                    To_Unbounded_String(Source => "studying ") &
                    Items_List(Recipe.Result_Index).Name;
               elsif Length(Source => Module.Crafting_Index) > 12
                 and then
                   Slice
                     (Source => Module.Crafting_Index, Low => 1, High => 11) =
                   "Deconstruct" then
                  Recipe_Name :=
                    To_Unbounded_String(Source => "deconstructing ") &
                    Items_List
                      (Unbounded_Slice
                         (Source => Module.Crafting_Index, Low => 13,
                          High => Length(Source => Module.Crafting_Index)))
                      .Name;
               else
                  Recipe_Name :=
                    To_Unbounded_String(Source => "manufacturing ") &
                    Items_List(Recipe.Result_Index).Name;
               end if;
               if Module.Durability = 0 then
                  AddMessage
                    (Message =>
                       To_String(Source => Module.Name) &
                       " is destroyed, so " &
                       To_String
                         (Source => Player_Ship.Crew(Crafter_Index).Name) &
                       " can't work on " & To_String(Source => Recipe_Name) &
                       ".",
                     MType => CraftMessage, Color => RED);
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
                  Work_Time := Work_Time - Current_Minutes;
                  Current_Minutes := 0;
                  Current_Minutes := Current_Minutes - Recipe_Time;
                  Work_Time := Work_Time - Recipe_Time;
                  Recipe_Time := Recipe.Time;
                  Material_Indexes.Clear;
                  if Length(Source => Module.Crafting_Index) > 6
                    and then
                      Slice
                        (Source => Module.Crafting_Index, Low => 1,
                         High => 5) =
                      "Study" then
                     Study_Materials_Loop :
                     for J in Items_List.Iterate loop
                        if Items_List(J).Name =
                          Items_List(Recipe.Result_Index).Name then
                           Material_Indexes.Append
                             (New_Item =>
                                Objects_Container.Key(Position => J));
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
                          Unbounded_Slice
                            (Source => Module.Crafting_Index, Low => 13,
                             High => Length(Source => Module.Crafting_Index)));
                  else
                     Recipe_Loop :
                     for K in Recipe.Material_Types.Iterate loop
                        Materials_Loop :
                        for J in Items_List.Iterate loop
                           if Items_List(J).IType =
                             Recipe.Material_Types
                               (UnboundedString_Container.To_Index
                                  (Position => K)) then
                              Material_Indexes.Append
                                (New_Item =>
                                   Objects_Container.Key(Position => J));
                              exit Materials_Loop;
                           end if;
                        end loop Materials_Loop;
                     end loop Recipe_Loop;
                  end if;
                  Crafting_Material := 0;
                  Check_Materials_Loop :
                  for MaterialIndex of Material_Indexes loop
                     Crafting_Material :=
                       FindItem
                         (Inventory => Player_Ship.Cargo,
                          ItemType => Items_List(MaterialIndex).IType);
                     if Crafting_Material = 0 then
                        AddMessage
                          (Message =>
                             "You don't have the crafting materials for " &
                             To_String(Source => Recipe_Name) & ".",
                           MType => CraftMessage, Color => RED);
                        Reset_Order(Module => Module, Module_Owner => Owner);
                        exit Craft_Loop;
                     elsif Player_Ship.Cargo(Crafting_Material).ProtoIndex /=
                       MaterialIndex then
                        MaterialIndex :=
                          Player_Ship.Cargo(Crafting_Material).ProtoIndex;
                     end if;
                  end loop Check_Materials_Loop;
                  if Recipe.Tool /= To_Unbounded_String(Source => "None") then
                     Tool_Index :=
                       FindTools
                         (MemberIndex => Crafter_Index,
                          ItemType => Recipe.Tool, Order => CRAFT,
                          ToolQuality => Recipe.Tool_Quality);
                     if Tool_Index = 0 then
                        AddMessage
                          (Message =>
                             "You don't have the tool for " &
                             To_String(Source => Recipe_Name) & ".",
                           MType => CraftMessage, Color => RED);
                        Reset_Order(Module => Module, Module_Owner => Owner);
                        exit Craft_Loop;
                     end if;
                  else
                     Tool_Index := 0;
                  end if;
                  Amount := 0;
                  Count_Amount_Loop :
                  for J in Material_Indexes.Iterate loop
                     Amount :=
                       Amount +
                       Items_List(Material_Indexes(J)).Weight *
                         Recipe.Material_Amounts
                           (UnboundedString_Container.To_Index(Position => J));
                  end loop Count_Amount_Loop;
                  Result_Amount :=
                    Recipe.Result_Amount +
                    Integer
                      (Float'Floor
                         (Float(Recipe.Result_Amount) *
                          (Float
                             (GetSkillLevel
                                (Member => Player_Ship.Crew(Crafter_Index),
                                 SkillIndex => Recipe.Skill)) /
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
                  for J in Material_Indexes.Iterate loop
                     Have_Material := False;
                     Check_Cargo_Materials_Loop :
                     for Item of Player_Ship.Cargo loop
                        if Items_List(Item.ProtoIndex).IType =
                          Items_List(Material_Indexes(J)).IType and
                          Item.Amount >=
                            Recipe.Material_Amounts
                              (UnboundedString_Container.To_Index
                                 (Position => J)) then
                           Have_Material := True;
                           exit Check_Cargo_Materials_Loop;
                        end if;
                     end loop Check_Cargo_Materials_Loop;
                     exit Check_Enough_Materials_Loop when not Have_Material;
                  end loop Check_Enough_Materials_Loop;
                  if not Have_Material then
                     AddMessage
                       (Message =>
                          "You don't have enough crafting materials for " &
                          To_String(Source => Recipe_Name) & ".",
                        MType => CraftMessage, Color => RED);
                     Reset_Order(Module => Module, Module_Owner => Owner);
                     exit Craft_Loop;
                  end if;
                  Crafted_Amount := Crafted_Amount + Result_Amount;
                  Module.Crafting_Amount := Module.Crafting_Amount - 1;
                  Remove_Materials_Loop :
                  for J in Material_Indexes.Iterate loop
                     Cargo_Index := 1;
                     Remove_Materials_From_Cargo_Loop :
                     while Cargo_Index <= Player_Ship.Cargo.Last_Index loop
                        if Items_List
                            (Player_Ship.Cargo(Cargo_Index).ProtoIndex)
                            .IType =
                          Items_List(Material_Indexes(J)).IType then
                           if Player_Ship.Cargo(Cargo_Index).Amount >
                             Recipe.Material_Amounts
                               (UnboundedString_Container.To_Index
                                  (Position => J)) then
                              New_Amount :=
                                Player_Ship.Cargo(Cargo_Index).Amount -
                                Recipe.Material_Amounts
                                  (UnboundedString_Container.To_Index
                                     (Position => J));
                              Player_Ship.Cargo(Cargo_Index).Amount :=
                                New_Amount;
                              exit Remove_Materials_From_Cargo_Loop;
                           elsif Player_Ship.Cargo(Cargo_Index).Amount =
                             Recipe.Material_Amounts
                               (UnboundedString_Container.To_Index
                                  (Position => J)) then
                              Player_Ship.Cargo.Delete
                                (Index => Cargo_Index, Count => 1);
                              if Tool_Index > Cargo_Index then
                                 Tool_Index := Tool_Index - 1;
                              end if;
                              exit Remove_Materials_From_Cargo_Loop;
                           end if;
                        end if;
                        Cargo_Index := Cargo_Index + 1;
                     end loop Remove_Materials_From_Cargo_Loop;
                  end loop Remove_Materials_Loop;
                  if Tool_Index > 0 then
                     DamageItem
                       (Inventory => Player_Ship.Crew(Crafter_Index).Inventory,
                        ItemIndex => Tool_Index,
                        SkillLevel =>
                          GetSkillLevel
                            (Member => Player_Ship.Crew(Crafter_Index),
                             SkillIndex => Recipe.Skill),
                        MemberIndex => Crafter_Index);
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
                       (Items_List(Recipe.Result_Index).Weight *
                        Result_Amount);
                     if FreeCargo(Amount => Amount) < 0 then
                        AddMessage
                          (Message =>
                             "You don't have the free cargo space for " &
                             To_String(Source => Recipe_Name) & ".",
                           MType => CraftMessage, Color => RED);
                        Reset_Order(Module => Module, Module_Owner => Owner);
                        exit Craft_Loop;
                     end if;
                     if Length(Source => Module.Crafting_Index) > 11
                       and then
                         Slice
                           (Source => Module.Crafting_Index, Low => 1,
                            High => 11) =
                         "Deconstruct" then
                        UpdateCargo
                          (Ship => Player_Ship,
                           ProtoIndex => Recipe.Result_Index,
                           Amount => Result_Amount);
                     else
                        UpdateCargo
                          (Ship => Player_Ship,
                           ProtoIndex =>
                             Recipes_List(Module.Crafting_Index).Result_Index,
                           Amount => Result_Amount);
                     end if;
                     Update_Crafting_Orders_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           UpdateCraftingOrders
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
                        AddMessage
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Crafter_Index).Name) &
                             " has recovered" & Integer'Image(Crafted_Amount) &
                             " " &
                             To_String
                               (Source =>
                                  Items_List(Recipe.Result_Index).Name) &
                             ".",
                           MType => CraftMessage, Color => GREEN);
                     else
                        AddMessage
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Crafter_Index).Name) &
                             " has manufactured" &
                             Integer'Image(Crafted_Amount) & " " &
                             To_String
                               (Source =>
                                  Items_List(Recipe.Result_Index).Name) &
                             ".",
                           MType => CraftMessage, Color => GREEN);
                     end if;
                     Update_Goal_Loop :
                     for I in Recipes_List.Iterate loop
                        if Recipes_List(I).Result_Index =
                          Recipe.Result_Index then
                           Update_Goal
                             (G_Type => CRAFT,
                              Target_Index =>
                                Recipes_Container.Key(Position => I),
                              Amount => Crafted_Amount);
                           exit Update_Goal_Loop;
                        end if;
                     end loop Update_Goal_Loop;
                     if Current_Goal.Target_Index /= Null_Unbounded_String then
                        Update_Goal
                          (G_Type => CRAFT,
                           Target_Index =>
                             Items_List(Recipe.Result_Index).IType,
                           Amount => Crafted_Amount);
                        if Items_List(Recipe.Result_Index).ShowType /=
                          Null_Unbounded_String then
                           Update_Goal
                             (G_Type => CRAFT,
                              Target_Index =>
                                Items_List(Recipe.Result_Index).ShowType,
                              Amount => Crafted_Amount);
                        end if;
                     end if;
                  else
                     AddMessage
                       (Message =>
                          To_String
                            (Source => Player_Ship.Crew(Crafter_Index).Name) &
                          " has discovered recipe for " &
                          To_String
                            (Source => Items_List(Recipe.Result_Index).Name) &
                          ".",
                        MType => CraftMessage, Color => GREEN);
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
         AddMessage
           (Message => Exception_Message(X => An_Exception),
            MType => OrderMessage, Color => RED);
         GiveOrders
           (Ship => Player_Ship, MemberIndex => Crafter_Index,
            GivenOrder => REST);
   end Manufacturing;

   procedure Set_Recipe
     (Workshop, Amount: Positive; Recipe_Index: Unbounded_String) is
      Recipe_Name, Item_Index: Unbounded_String;
   begin
      Player_Ship.Modules(Workshop).Crafting_Amount := Amount;
      if Length(Source => Recipe_Index) > 6
        and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
          "Study" then
         Item_Index :=
           Unbounded_Slice
             (Source => Recipe_Index, Low => 7,
              High => Length(Source => Recipe_Index));
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
           Items_List(Item_Index).Name;
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
      elsif Length(Source => Recipe_Index) > 12
        and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
          "Deconstruct" then
         Item_Index :=
           Unbounded_Slice
             (Source => Recipe_Index, Low => 13,
              High => Length(Source => Recipe_Index));
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
           Items_List(Item_Index).Name;
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
      else
         Player_Ship.Modules(Workshop).Crafting_Index := Recipe_Index;
         Player_Ship.Modules(Workshop).Crafting_Time :=
           Recipes_List(Recipe_Index).Time;
         Recipe_Name :=
           Items_List(Recipes_List(Recipe_Index).Result_Index).Name;
      end if;
      AddMessage
        (Message =>
           To_String(Source => Recipe_Name) &
           " was set as manufacturing order in " &
           To_String(Source => Player_Ship.Modules(Workshop).Name) & ".",
         MType => CraftMessage);
      UpdateOrders(Ship => Player_Ship);
   end Set_Recipe;

end Crafts;
