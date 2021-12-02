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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Utils; use Utils;
with Crew; use Crew;
with Crew.Inventory; use Crew.Inventory;
with Crafts; use Crafts;
with Config; use Config;

package body Items is

   procedure Load_Items(Reader: Tree_Reader) is
      use Tiny_String;

      TempRecord: Object_Data;
      NodesList, ChildNodes: Node_List;
      ItemsData: Document;
      TempValue: Integer_Container.Vector;
      ItemNode, ChildNode: Node;
      ItemIndex: Bounded_String;
      Action: Data_Action;
   begin
      ItemsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ItemsData, "item");
      Load_Items_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Name => Null_Unbounded_String, Weight => 1,
            I_Type => Null_Unbounded_String, Price => 0, Value => TempValue,
            Show_Type => Null_Unbounded_String,
            Description => Null_Unbounded_String, Reputation => -100);
         ItemNode := Item(NodesList, I);
         ItemIndex := To_Bounded_String(Get_Attribute(ItemNode, "index"));
         Action :=
           (if Get_Attribute(ItemNode, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(ItemNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Objects_Container.Contains(Items_List, ItemIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " item '" & To_String(ItemIndex) &
                 "', there is no item with that index.";
            end if;
         elsif Objects_Container.Contains(Items_List, ItemIndex) then
            raise Data_Loading_Error
              with "Can't add item '" & To_String(ItemIndex) &
              "', there is an item with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Items_List(ItemIndex);
            end if;
            if Get_Attribute(ItemNode, "name")'Length > 0 then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(ItemNode, "name"));
            end if;
            if Get_Attribute(ItemNode, "weight")'Length > 0 then
               TempRecord.Weight :=
                 Natural'Value(Get_Attribute(ItemNode, "weight"));
            end if;
            if Get_Attribute(ItemNode, "type")'Length > 0 then
               TempRecord.I_Type :=
                 To_Unbounded_String(Get_Attribute(ItemNode, "type"));
            end if;
            if Get_Attribute(ItemNode, "showtype") /= "" then
               TempRecord.Show_Type :=
                 To_Unbounded_String(Get_Attribute(ItemNode, "showtype"));
            end if;
            if Get_Attribute(ItemNode, "reputation")'Length > 0 then
               TempRecord.Reputation :=
                 Integer'Value(Get_Attribute(ItemNode, "reputation"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ItemNode, "trade");
            Set_Buyable_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               if Get_Attribute(ChildNode, "buyable") = "N" then
                  TempRecord.Price :=
                    Natural'Value(Get_Attribute(ChildNode, "price"));
                  exit Set_Buyable_Loop;
               end if;
            end loop Set_Buyable_Loop;
            if Get_Attribute(ItemNode, "price")'Length > 0 then
               TempRecord.Price :=
                 Natural'Value(Get_Attribute(ItemNode, "price"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ItemNode, "data");
            if Length(ChildNodes) > 0 then
               TempRecord.Value.Clear;
            end if;
            Set_Value_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.Value.Append
                 (New_Item =>
                    Integer'Value
                      (Get_Attribute(Item(ChildNodes, J), "value")));
            end loop Set_Value_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (ItemNode, "description");
            if Length(ChildNodes) > 0 then
               TempRecord.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(ChildNodes, 0))));
            end if;
            if ItemIndex = Money_Index then
               Money_Name := TempRecord.Name;
            end if;
            -- Backward compatibility, all ammunitions are normal by default
            if Length(TempRecord.I_Type) > 4
              and then Slice(TempRecord.I_Type, 1, 4) = "Ammo"
              and then TempRecord.Value.Length = 1 then
               TempRecord.Value.Append(New_Item => 1);
            end if;
            if Action /= UPDATE then
               Objects_Container.Include(Items_List, ItemIndex, TempRecord);
               Log_Message
                 ("Item added: " & To_String(TempRecord.Name), EVERYTHING);
            else
               Items_List(ItemIndex) := TempRecord;
               Log_Message
                 ("Item updated: " & To_String(TempRecord.Name), EVERYTHING);
            end if;
         else
            Objects_Container.Exclude(Items_List, ItemIndex);
            Log_Message("Item removed: " & To_String(ItemIndex), EVERYTHING);
         end if;
      end loop Load_Items_Loop;
      Set_Items_Lists_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).I_Type = Weapon_Type then
            Weapons_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).I_Type = Shield_Type then
            Shields_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).I_Type = Head_Armor then
            Head_Armors_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).I_Type = Chest_Armor then
            Chest_Armors_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).I_Type = Arms_Armor then
            Arms_Armors_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).I_Type = Legs_Armor then
            Legs_Armors_List.Append(New_Item => Objects_Container.Key(I));
         end if;
      end loop Set_Items_Lists_Loop;
   end Load_Items;

   function Find_Proto_Item
     (Item_Type: Unbounded_String) return Tiny_String.Bounded_String is
   begin
      Find_Proto_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).I_Type = Item_Type then
            return Objects_Container.Key(I);
         end if;
      end loop Find_Proto_Loop;
      return Tiny_String.Null_Bounded_String;
   end Find_Proto_Item;

   function Get_Item_Damage
     (Item_Durability: Items_Durability; To_Lower: Boolean := False)
      return String is
      DamagePercent: Float range 0.0 .. 1.0;
      DamageText: Unbounded_String;
   begin
      DamagePercent := 1.0 - (Float(Item_Durability) / 100.0);
      DamageText :=
        (if DamagePercent < 0.2 then To_Unbounded_String("Slightly used")
         elsif DamagePercent < 0.5 then To_Unbounded_String("Damaged")
         elsif DamagePercent < 0.8 then To_Unbounded_String("Heavily damaged")
         else To_Unbounded_String("Almost destroyed"));
      if To_Lower then
         DamageText := To_Unbounded_String(Ada.Characters.Handling.To_Lower(To_String(DamageText)));
      end if;
      return To_String(DamageText);
   end Get_Item_Damage;

   function Get_Item_Name
     (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
      return String is
      ItemName: Unbounded_String;
   begin
      ItemName :=
        (if Item.Name /= Null_Unbounded_String then Item.Name
         else Items_List(Item.Proto_Index).Name);
      if Damage_Info and then Item.Durability < 100 then
         Append
           (ItemName, " (" & Get_Item_Damage(Item.Durability, To_Lower) & ")");
      end if;
      return To_String(ItemName);
   end Get_Item_Name;

   procedure Damage_Item
     (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
      Skill_Level, Member_Index: Natural := 0) is
      use Tiny_String;

      DamageChance: Integer :=
        Items_List(Inventory(Item_Index).Proto_Index).Value(1);
      I: Inventory_Container.Extended_Index := Inventory.First_Index;
   begin
      if Skill_Level > 0 then
         DamageChance := DamageChance - (Skill_Level / 5);
         if DamageChance < 1 then
            DamageChance := 1;
         end if;
      end if;
      if Get_Random(1, 100) > DamageChance then -- Item not damaged
         return;
      end if;
      if Inventory(Item_Index).Amount > 1 then
         declare
            Item: constant Inventory_Data := Inventory(Item_Index);
         begin
            Inventory.Append
              (New_Item =>
                 (Proto_Index => Item.Proto_Index, Amount => (Item.Amount - 1),
                  Name => Item.Name, Durability => Item.Durability,
                  Price => Item.Price));
         end;
         Inventory(Item_Index).Amount := 1;
      end if;
      Inventory(Item_Index).Durability := Inventory(Item_Index).Durability - 1;
      if Inventory(Item_Index).Durability = 0 then -- Item destroyed
         if Member_Index = 0 then
            UpdateCargo
              (Ship => Player_Ship, CargoIndex => Item_Index, Amount => -1);
         else
            UpdateInventory
              (MemberIndex => Member_Index, Amount => -1,
               InventoryIndex => Item_Index);
         end if;
         return;
      end if;
      Update_Inventory_Loop :
      while I <= Inventory.Last_Index loop
         Find_Item_Loop :
         for J in Inventory.First_Index .. Inventory.Last_Index loop
            if Inventory(I).Proto_Index = Inventory(J).Proto_Index and
              Inventory(I).Durability = Inventory(J).Durability and I /= J then
               if Member_Index = 0 then
                  UpdateCargo
                    (Ship => Player_Ship, CargoIndex => J,
                     Amount => (0 - Inventory.Element(J).Amount));
                  UpdateCargo
                    (Ship => Player_Ship, CargoIndex => I,
                     Amount => Inventory.Element(J).Amount);
               else
                  UpdateInventory
                    (MemberIndex => Member_Index,
                     Amount => (0 - Inventory.Element(J).Amount),
                     InventoryIndex => J);
                  UpdateInventory
                    (MemberIndex => Member_Index,
                     Amount => Inventory.Element(J).Amount,
                     InventoryIndex => I);
               end if;
               I := I - 1;
               exit Find_Item_Loop;
            end if;
         end loop Find_Item_Loop;
         I := I + 1;
      end loop Update_Inventory_Loop;
   end Damage_Item;

   function Find_Item
     (Inventory: Inventory_Container.Vector;
      Proto_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Item_Type: Unbounded_String := Null_Unbounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural is
      use Tiny_String;
   begin
      if Proto_Index /= Null_Bounded_String then
         Find_Item_With_Proto_Loop :
         for I in Inventory.Iterate loop
            if Inventory(I).Proto_Index = Proto_Index
              and then
              ((Items_List(Inventory(I).Proto_Index).Value.Length > 0
                and then Items_List(Inventory(I).Proto_Index).Value(1) <=
                  Quality) or
               Items_List(Inventory(I).Proto_Index).Value.Length = 0) then
               if Durability < Items_Durability'Last
                 and then Inventory(I).Durability = Durability then
                  return Inventory_Container.To_Index(I);
               else
                  return Inventory_Container.To_Index(I);
               end if;
            end if;
         end loop Find_Item_With_Proto_Loop;
      elsif Item_Type /= Null_Unbounded_String then
         Find_Item_Loop :
         for I in Inventory.Iterate loop
            if Items_List(Inventory(I).Proto_Index).I_Type = Item_Type
              and then
              ((Items_List(Inventory(I).Proto_Index).Value.Length > 0
                and then Items_List(Inventory(I).Proto_Index).Value(1) <=
                  Quality) or
               Items_List(Inventory(I).Proto_Index).Value.Length = 0) then
               if Durability < Items_Durability'Last
                 and then Inventory(I).Durability = Durability then
                  return Inventory_Container.To_Index(I);
               else
                  return Inventory_Container.To_Index(I);
               end if;
            end if;
         end loop Find_Item_Loop;
      end if;
      return 0;
   end Find_Item;

   procedure Set_Tools_List is
      use Tiny_String;

   begin
      if Tools_List.Length > 0 then
         return;
      end if;
      Tools_List.Append(New_Item => Repair_Tools);
      Tools_List.Append(New_Item => Cleaning_Tools);
      Tools_List.Append(New_Item => Alchemy_Tools);
      Recipes_Loop :
      for Recipe of Recipes_List loop
         if Tools_List.Find_Index(Item => Recipe.Tool) =
           UnboundedString_Container.No_Index then
            Tools_List.Append(New_Item => Recipe.Tool);
         end if;
      end loop Recipes_Loop;
      Skills_Loop :
      for I in 1 .. Skills_Amount loop
         if Tools_List.Find_Index
             (Item =>
                To_Unbounded_String
                  (To_String
                     (SkillsData_Container.Element(Skills_List, I).Tool))) =
           UnboundedString_Container.No_Index then
            Tools_List.Append
              (New_Item =>
                 To_Unbounded_String
                   (To_String
                      (SkillsData_Container.Element(Skills_List, I).Tool)));
         end if;
      end loop Skills_Loop;
   end Set_Tools_List;

   function Get_Item_Chance_To_Damage(Item_Data: Natural) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Natural'Image(Item_Data) & "%";
      end if;
      case Item_Data is
         when 1 =>
            return "Almost never";
         when 2 =>
            return "Very small";
         when 3 =>
            return "Small";
         when 4 .. 9 =>
            return "Below average";
         when 10 .. 14 =>
            return "Average";
         when 15 .. 19 =>
            return "High";
         when others =>
            return "Very high";
      end case;
   end Get_Item_Chance_To_Damage;

end Items;
