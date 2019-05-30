--    Copyright 2016-2018 Bartek thindil Jasicki
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

package body Items is

   procedure LoadItems(Reader: Tree_Reader) is
      TempRecord: Object_Data;
      NodesList, ChildNodes: Node_List;
      ItemsData: Document;
      TempValue: Natural_Container.Vector;
   begin
      TempRecord :=
        (Name => Null_Unbounded_String, Weight => 1,
         IType => Null_Unbounded_String, Prices => (0, 0, 0, 0, 0),
         Buyable => (False, False, False, False, False), Value => TempValue,
         ShowType => Null_Unbounded_String,
         Description => Null_Unbounded_String, Index => Null_Unbounded_String);
      ItemsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ItemsData, "item");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
         TempRecord.Name :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name"));
         TempRecord.Weight :=
           Natural'Value(Get_Attribute(Item(NodesList, I), "weight"));
         TempRecord.IType :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "type"));
         if Get_Attribute(Item(NodesList, I), "showtype") /= "" then
            TempRecord.ShowType :=
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "showtype"));
         end if;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "trade");
         for J in 0 .. Length(ChildNodes) - 1 loop
            TempRecord.Prices(J + 1) :=
              Natural'Value(Get_Attribute(Item(ChildNodes, J), "price"));
            if Get_Attribute(Item(ChildNodes, J), "buyable") = "Y" then
               TempRecord.Buyable(J + 1) := True;
            end if;
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "data");
         for J in 0 .. Length(ChildNodes) - 1 loop
            TempRecord.Value.Append
              (New_Item =>
                 Natural'Value(Get_Attribute(Item(ChildNodes, J), "value")));
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "description");
         TempRecord.Description :=
           To_Unbounded_String(Node_Value(First_Child(Item(ChildNodes, 0))));
         if TempRecord.Index = MoneyIndex then
            MoneyName := TempRecord.Name;
         end if;
         if Get_Attribute(Item(NodesList, I), "remove") = "" then
            Items_List.Append(New_Item => TempRecord);
            if TempRecord.IType = WeaponType then
               Weapons_List.Append(New_Item => Items_List.Last_Index);
            elsif TempRecord.IType = ShieldType then
               Shields_List.Append(New_Item => Items_List.Last_Index);
            elsif TempRecord.IType = HeadArmor then
               HeadArmors_List.Append(New_Item => Items_List.Last_Index);
            elsif TempRecord.IType = ChestArmor then
               ChestArmors_List.Append(New_Item => Items_List.Last_Index);
            elsif TempRecord.IType = ArmsArmor then
               ArmsArmors_List.Append(New_Item => Items_List.Last_Index);
            elsif TempRecord.IType = LegsArmor then
               LegsArmors_List.Append(New_Item => Items_List.Last_Index);
            end if;
            LogMessage
              ("Item added: " & To_String(TempRecord.Name), Everything);
         else
            Items_List.Delete
              (Index =>
                 FindProtoItem
                   (To_Unbounded_String
                      (Get_Attribute(Item(NodesList, I), "remove"))));
            LogMessage
              ("Item removed: " & Get_Attribute(Item(NodesList, I), "remove"),
               Everything);
         end if;
         TempRecord :=
           (Name => Null_Unbounded_String, Weight => 1,
            IType => Null_Unbounded_String, Prices => (0, 0, 0, 0, 0),
            Buyable => (False, False, False, False, False), Value => TempValue,
            ShowType => Null_Unbounded_String,
            Description => Null_Unbounded_String,
            Index => Null_Unbounded_String);
      end loop;
   end LoadItems;

   function FindProtoItem
     (Index, ItemType: Unbounded_String := Null_Unbounded_String)
      return Natural is
   begin
      if Index /= Null_Unbounded_String then
         for I in Items_List.Iterate loop
            if Items_List(I).Index = Index then
               return Objects_Container.To_Index(I);
            end if;
         end loop;
      elsif ItemType /= Null_Unbounded_String then
         for I in Items_List.Iterate loop
            if Items_List(I).IType = ItemType then
               return Objects_Container.To_Index(I);
            end if;
         end loop;
      end if;
      return 0;
   end FindProtoItem;

   function GetItemName(Item: InventoryData) return String is
      ItemName: Unbounded_String;
      DamagePercent: Float;
   begin
      if Item.Name /= Null_Unbounded_String then
         ItemName := Item.Name;
      else
         ItemName := Items_List(Item.ProtoIndex).Name;
      end if;
      if Item.Durability < 100 then
         DamagePercent := 1.0 - (Float(Item.Durability) / 100.0);
         if DamagePercent < 0.2 then
            Append(ItemName, " (slightly used)");
         elsif DamagePercent < 0.5 then
            Append(ItemName, " (damaged)");
         elsif DamagePercent < 0.8 then
            Append(ItemName, " (heavily damaged)");
         else
            Append(ItemName, " (almost destroyed)");
         end if;
      end if;
      return To_String(ItemName);
   end GetItemName;

   procedure DamageItem
     (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
      SkillLevel, MemberIndex: Natural := 0) is
      DamageChance: Integer :=
        Items_List(Inventory(ItemIndex).ProtoIndex).Value(1);
      I: Natural := Inventory.First_Index;
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
                  Name => Item.Name, Durability => Item.Durability));
         end;
         Inventory(ItemIndex).Amount := 1;
      end if;
      Inventory(ItemIndex).Durability := Inventory(ItemIndex).Durability - 1;
      if Inventory(ItemIndex).Durability = 0 then -- Item destroyed
         if MemberIndex = 0 then
            UpdateCargo
              (Ship => PlayerShip, CargoIndex => ItemIndex, Amount => -1);
         else
            UpdateInventory
              (MemberIndex => MemberIndex, Amount => -1,
               InventoryIndex => ItemIndex);
         end if;
         return;
      end if;
      while I <= Inventory.Last_Index loop
         for J in Inventory.First_Index .. Inventory.Last_Index loop
            if Inventory(I).ProtoIndex = Inventory(J).ProtoIndex and
              Inventory(I).Durability = Inventory(J).Durability and I /= J then
               if MemberIndex = 0 then
                  UpdateCargo
                    (Ship => PlayerShip, CargoIndex => J,
                     Amount => (0 - Inventory.Element(J).Amount));
                  UpdateCargo
                    (Ship => PlayerShip, CargoIndex => I,
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
               exit;
            end if;
         end loop;
         I := I + 1;
      end loop;
   end DamageItem;

   function FindItem
     (Inventory: Inventory_Container.Vector; ProtoIndex: Natural := 0;
      ItemType: Unbounded_String := Null_Unbounded_String;
      Durability: Natural := 101) return Natural is
   begin
      if ProtoIndex > 0 then
         for I in Inventory.Iterate loop
            if Durability < 101 then
               if Inventory(I).ProtoIndex = ProtoIndex and
                 Inventory(I).Durability = Durability then
                  return Inventory_Container.To_Index(I);
               end if;
            else
               if Inventory(I).ProtoIndex = ProtoIndex then
                  return Inventory_Container.To_Index(I);
               end if;
            end if;
         end loop;
      elsif ItemType /= Null_Unbounded_String then
         for I in Inventory.Iterate loop
            if Durability < 101 then
               if Items_List(Inventory(I).ProtoIndex).IType = ItemType and
                 Inventory(I).Durability = Durability then
                  return Inventory_Container.To_Index(I);
               end if;
            else
               if Items_List(Inventory(I).ProtoIndex).IType = ItemType then
                  return Inventory_Container.To_Index(I);
               end if;
            end if;
         end loop;
      end if;
      return 0;
   end FindItem;

   procedure SetToolsList is
   begin
      if Tools_List.Length > 0 then
         return;
      end if;
      Tools_List.Append(New_Item => RepairTools);
      Tools_List.Append(New_Item => CleaningTools);
      Tools_List.Append(New_Item => AlchemyTools);
      for Recipe of Recipes_List loop
         if Tools_List.Find_Index(Item => Recipe.Tool) =
           UnboundedString_Container.No_Index then
            Tools_List.Append(New_Item => Recipe.Tool);
         end if;
      end loop;
   end SetToolsList;

end Items;
