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

   procedure LoadItems(Reader: Tree_Reader) is
      TempRecord: Object_Data;
      NodesList, ChildNodes: Node_List;
      ItemsData: Document;
      TempValue: Integer_Container.Vector;
      ItemNode, ChildNode: Node;
      ItemIndex: Unbounded_String;
      Action: Data_Action;
   begin
      ItemsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ItemsData, "item");
      Load_Items_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Name => Null_Unbounded_String, Weight => 1,
            IType => Null_Unbounded_String, Price => 0, Value => TempValue,
            ShowType => Null_Unbounded_String,
            Description => Null_Unbounded_String, Reputation => -100);
         ItemNode := Item(NodesList, I);
         ItemIndex := To_Unbounded_String(Get_Attribute(ItemNode, "index"));
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
               TempRecord.IType :=
                 To_Unbounded_String(Get_Attribute(ItemNode, "type"));
            end if;
            if Get_Attribute(ItemNode, "showtype") /= "" then
               TempRecord.ShowType :=
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
            if Length(TempRecord.IType) > 4
              and then Slice(TempRecord.IType, 1, 4) = "Ammo"
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
         if Items_List(I).IType = Weapon_Type then
            Weapons_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).IType = Shield_Type then
            Shields_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).IType = Head_Armor then
            HeadArmors_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).IType = Chest_Armor then
            ChestArmors_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).IType = Arms_Armor then
            ArmsArmors_List.Append(New_Item => Objects_Container.Key(I));
         elsif Items_List(I).IType = Legs_Armor then
            LegsArmors_List.Append(New_Item => Objects_Container.Key(I));
         end if;
      end loop Set_Items_Lists_Loop;
   end LoadItems;

   function FindProtoItem
     (ItemType: Unbounded_String) return Unbounded_String is
   begin
      Find_Proto_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).IType = ItemType then
            return Objects_Container.Key(I);
         end if;
      end loop Find_Proto_Loop;
      return Null_Unbounded_String;
   end FindProtoItem;

   function GetItemDamage
     (ItemDurability: Items_Durability; ToLower: Boolean := False)
      return String is
      DamagePercent: Float range 0.0 .. 1.0;
      DamageText: Unbounded_String;
   begin
      DamagePercent := 1.0 - (Float(ItemDurability) / 100.0);
      DamageText :=
        (if DamagePercent < 0.2 then To_Unbounded_String("Slightly used")
         elsif DamagePercent < 0.5 then To_Unbounded_String("Damaged")
         elsif DamagePercent < 0.8 then To_Unbounded_String("Heavily damaged")
         else To_Unbounded_String("Almost destroyed"));
      if ToLower then
         DamageText := To_Unbounded_String(To_Lower(To_String(DamageText)));
      end if;
      return To_String(DamageText);
   end GetItemDamage;

   function GetItemName
     (Item: InventoryData; DamageInfo, ToLower: Boolean := True)
      return String is
      ItemName: Unbounded_String;
   begin
      ItemName :=
        (if Item.Name /= Null_Unbounded_String then Item.Name
         else Items_List(Item.ProtoIndex).Name);
      if DamageInfo and then Item.Durability < 100 then
         Append
           (ItemName, " (" & GetItemDamage(Item.Durability, ToLower) & ")");
      end if;
      return To_String(ItemName);
   end GetItemName;

   procedure DamageItem
     (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
      SkillLevel, MemberIndex: Natural := 0) is
      DamageChance: Integer :=
        Items_List(Inventory(ItemIndex).ProtoIndex).Value(1);
      I: Inventory_Container.Extended_Index := Inventory.First_Index;
   begin
      if SkillLevel > 0 then
         DamageChance := DamageChance - (SkillLevel / 5);
         if DamageChance < 1 then
            DamageChance := 1;
         end if;
      end if;
      if GetRandom(1, 100) > DamageChance then -- Item not damaged
         return;
      end if;
      if Inventory(ItemIndex).Amount > 1 then
         declare
            Item: constant InventoryData := Inventory(ItemIndex);
         begin
            Inventory.Append
              (New_Item =>
                 (ProtoIndex => Item.ProtoIndex, Amount => (Item.Amount - 1),
                  Name => Item.Name, Durability => Item.Durability,
                  Price => Item.Price));
         end;
         Inventory(ItemIndex).Amount := 1;
      end if;
      Inventory(ItemIndex).Durability := Inventory(ItemIndex).Durability - 1;
      if Inventory(ItemIndex).Durability = 0 then -- Item destroyed
         if MemberIndex = 0 then
            UpdateCargo
              (Ship => Player_Ship, CargoIndex => ItemIndex, Amount => -1);
         else
            UpdateInventory
              (MemberIndex => MemberIndex, Amount => -1,
               InventoryIndex => ItemIndex);
         end if;
         return;
      end if;
      Update_Inventory_Loop :
      while I <= Inventory.Last_Index loop
         Find_Item_Loop :
         for J in Inventory.First_Index .. Inventory.Last_Index loop
            if Inventory(I).ProtoIndex = Inventory(J).ProtoIndex and
              Inventory(I).Durability = Inventory(J).Durability and I /= J then
               if MemberIndex = 0 then
                  UpdateCargo
                    (Ship => Player_Ship, CargoIndex => J,
                     Amount => (0 - Inventory.Element(J).Amount));
                  UpdateCargo
                    (Ship => Player_Ship, CargoIndex => I,
                     Amount => Inventory.Element(J).Amount);
               else
                  UpdateInventory
                    (MemberIndex => MemberIndex,
                     Amount => (0 - Inventory.Element(J).Amount),
                     InventoryIndex => J);
                  UpdateInventory
                    (MemberIndex => MemberIndex,
                     Amount => Inventory.Element(J).Amount,
                     InventoryIndex => I);
               end if;
               I := I - 1;
               exit Find_Item_Loop;
            end if;
         end loop Find_Item_Loop;
         I := I + 1;
      end loop Update_Inventory_Loop;
   end DamageItem;

   function FindItem
     (Inventory: Inventory_Container.Vector;
      ProtoIndex, ItemType: Unbounded_String := Null_Unbounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural is
   begin
      if ProtoIndex /= Null_Unbounded_String then
         Find_Item_With_Proto_Loop :
         for I in Inventory.Iterate loop
            if Inventory(I).ProtoIndex = ProtoIndex
              and then
              ((Items_List(Inventory(I).ProtoIndex).Value.Length > 0
                and then Items_List(Inventory(I).ProtoIndex).Value(1) <=
                  Quality) or
               Items_List(Inventory(I).ProtoIndex).Value.Length = 0) then
               if Durability < Items_Durability'Last
                 and then Inventory(I).Durability = Durability then
                  return Inventory_Container.To_Index(I);
               else
                  return Inventory_Container.To_Index(I);
               end if;
            end if;
         end loop Find_Item_With_Proto_Loop;
      elsif ItemType /= Null_Unbounded_String then
         Find_Item_Loop :
         for I in Inventory.Iterate loop
            if Items_List(Inventory(I).ProtoIndex).IType = ItemType
              and then
              ((Items_List(Inventory(I).ProtoIndex).Value.Length > 0
                and then Items_List(Inventory(I).ProtoIndex).Value(1) <=
                  Quality) or
               Items_List(Inventory(I).ProtoIndex).Value.Length = 0) then
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
   end FindItem;

   procedure SetToolsList is
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
      for Skill of Skills_List loop
         if Tools_List.Find_Index(Item => Skill.Tool) =
           UnboundedString_Container.No_Index then
            Tools_List.Append(New_Item => Skill.Tool);
         end if;
      end loop Skills_Loop;
   end SetToolsList;

   function GetItemChanceToDamage(ItemData: Natural) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Natural'Image(ItemData) & "%";
      end if;
      case ItemData is
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
   end GetItemChanceToDamage;

end Items;
