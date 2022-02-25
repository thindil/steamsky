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

      Temp_Record: Object_Data;
      Nodes_List, Child_Nodes: Node_List;
      Items_Data: Document;
      Temp_Value: Integer_Container.Vector;
      Item_Node, Child_Node: Node;
      Item_Index: Bounded_String;
      Action: Data_Action;
   begin
      Items_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Items_Data, Tag_Name => "item");
      Load_Items_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Null_Unbounded_String, Weight => 1,
            I_Type => Null_Unbounded_String, Price => 0, Value => Temp_Value,
            Show_Type => Null_Unbounded_String,
            Description => Null_Unbounded_String, Reputation => -100);
         Item_Node := Item(List => Nodes_List, Index => I);
         Item_Index :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Item_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Item_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Item_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Objects_Container.Contains
                (Container => Items_List, Key => Item_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " item '" & To_String(Source => Item_Index) &
                 "', there is no item with that index.";
            end if;
         elsif Objects_Container.Contains
             (Container => Items_List, Key => Item_Index) then
            raise Data_Loading_Error
              with "Can't add item '" & To_String(Source => Item_Index) &
              "', there is an item with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Items_List(Item_Index);
            end if;
            if Get_Attribute(Elem => Item_Node, Name => "name")'Length > 0 then
               Temp_Record.Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Item_Node, Name => "name"));
            end if;
            if Get_Attribute(Elem => Item_Node, Name => "weight")'Length >
              0 then
               Temp_Record.Weight :=
                 Natural'Value
                   (Get_Attribute(Elem => Item_Node, Name => "weight"));
            end if;
            if Get_Attribute(Elem => Item_Node, Name => "type")'Length > 0 then
               Temp_Record.I_Type :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Item_Node, Name => "type"));
            end if;
            if Get_Attribute(Elem => Item_Node, Name => "showtype") /= "" then
               Temp_Record.Show_Type :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Item_Node, Name => "showtype"));
            end if;
            if Get_Attribute(Elem => Item_Node, Name => "reputation")'Length >
              0 then
               Temp_Record.Reputation :=
                 Integer'Value
                   (Get_Attribute(Elem => Item_Node, Name => "reputation"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Item_Node, Name => "trade");
            Set_Buyable_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               if Get_Attribute(Elem => Child_Node, Name => "buyable") =
                 "N" then
                  Temp_Record.Price :=
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "price"));
                  exit Set_Buyable_Loop;
               end if;
            end loop Set_Buyable_Loop;
            if Get_Attribute(Elem => Item_Node, Name => "price")'Length >
              0 then
               Temp_Record.Price :=
                 Natural'Value
                   (Get_Attribute(Elem => Item_Node, Name => "price"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Item_Node, Name => "data");
            if Length(List => Child_Nodes) > 0 then
               Temp_Record.Value.Clear;
            end if;
            Set_Value_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Temp_Record.Value.Append
                 (New_Item =>
                    Integer'Value
                      (Get_Attribute
                         (Elem => Item(List => Child_Nodes, Index => J),
                          Name => "value")));
            end loop Set_Value_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Item_Node, Name => "description");
            if Length(List => Child_Nodes) > 0 then
               Temp_Record.Description :=
                 To_Unbounded_String
                   (Source =>
                      Node_Value
                        (N =>
                           First_Child
                             (N => Item(List => Child_Nodes, Index => 0))));
            end if;
            if Item_Index = Money_Index then
               Money_Name := Temp_Record.Name;
            end if;
            -- Backward compatibility, all ammunitions are normal by default
            if Length(Source => Temp_Record.I_Type) > 4
              and then
                Slice(Source => Temp_Record.I_Type, Low => 1, High => 4) =
                "Ammo"
              and then Temp_Record.Value.Length = 1 then
               Temp_Record.Value.Append(New_Item => 1);
            end if;
            if Action /= UPDATE then
               Objects_Container.Include
                 (Container => Items_List, Key => Item_Index,
                  New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Item added: " & To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            else
               Items_List(Item_Index) := Temp_Record;
               Log_Message
                 (Message =>
                    "Item updated: " & To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            end if;
         else
            Objects_Container.Exclude
              (Container => Items_List, Key => Item_Index);
            Log_Message
              (Message => "Item removed: " & To_String(Source => Item_Index),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Items_Loop;
      Set_Items_Lists_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).I_Type = Weapon_Type then
            Weapons_List.Append
              (New_Item => Objects_Container.Key(Position => I));
         elsif Items_List(I).I_Type = Shield_Type then
            Shields_List.Append
              (New_Item => Objects_Container.Key(Position => I));
         elsif Items_List(I).I_Type = Head_Armor then
            Head_Armors_List.Append
              (New_Item => Objects_Container.Key(Position => I));
         elsif Items_List(I).I_Type = Chest_Armor then
            Chest_Armors_List.Append
              (New_Item => Objects_Container.Key(Position => I));
         elsif Items_List(I).I_Type = Arms_Armor then
            Arms_Armors_List.Append
              (New_Item => Objects_Container.Key(Position => I));
         elsif Items_List(I).I_Type = Legs_Armor then
            Legs_Armors_List.Append
              (New_Item => Objects_Container.Key(Position => I));
         end if;
      end loop Set_Items_Lists_Loop;
   end Load_Items;

   function Find_Proto_Item
     (Item_Type: Unbounded_String) return Tiny_String.Bounded_String is
   begin
      Find_Proto_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).I_Type = Item_Type then
            return Objects_Container.Key(Position => I);
         end if;
      end loop Find_Proto_Loop;
      return Tiny_String.Null_Bounded_String;
   end Find_Proto_Item;

   function Get_Item_Damage
     (Item_Durability: Items_Durability; To_Lower: Boolean := False)
      return String is
      Damage_Percent: Float range 0.0 .. 1.0;
      Damage_Text: Unbounded_String;
   begin
      Damage_Percent := 1.0 - (Float(Item_Durability) / 100.0);
      Damage_Text :=
        (if Damage_Percent < 0.2 then
           To_Unbounded_String(Source => "Slightly used")
         elsif Damage_Percent < 0.5 then
           To_Unbounded_String(Source => "Damaged")
         elsif Damage_Percent < 0.8 then
           To_Unbounded_String(Source => "Heavily damaged")
         else To_Unbounded_String(Source => "Almost destroyed"));
      if To_Lower then
         Damage_Text :=
           To_Unbounded_String
             (Source =>
                Ada.Characters.Handling.To_Lower
                  (Item => To_String(Source => Damage_Text)));
      end if;
      return To_String(Source => Damage_Text);
   end Get_Item_Damage;

   function Get_Item_Name
     (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
      return String is
      use Tiny_String;

      Item_Name: Unbounded_String;
   begin
      Item_Name :=
        (if Item.Name /= Null_Bounded_String then
           To_Unbounded_String(Source => To_String(Source => Item.Name))
         else Items_List(Item.Proto_Index).Name);
      if Damage_Info and then Item.Durability < 100 then
         Append
           (Source => Item_Name,
            New_Item =>
              " (" &
              Get_Item_Damage
                (Item_Durability => Item.Durability, To_Lower => To_Lower) &
              ")");
      end if;
      return To_String(Source => Item_Name);
   end Get_Item_Name;

   procedure Damage_Item
     (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
      Skill_Level, Member_Index: Natural := 0; Ship: in out Ship_Record) is
      use Tiny_String;

      Damage_Chance: Integer :=
        Items_List(Inventory(Item_Index).Proto_Index).Value(1);
      I: Inventory_Container.Extended_Index := Inventory.First_Index;
   begin
      if Skill_Level > 0 then
         Damage_Chance := Damage_Chance - (Skill_Level / 5);
         if Damage_Chance < 1 then
            Damage_Chance := 1;
         end if;
      end if;
      if Get_Random(Min => 1, Max => 100) >
        Damage_Chance then -- Item not damaged
         return;
      end if;
      if Inventory(Item_Index).Amount > 1 then
         Add_Damaged_Item_Block :
         declare
            Item: constant Inventory_Data := Inventory(Item_Index);
         begin
            Inventory.Append
              (New_Item =>
                 (Proto_Index => Item.Proto_Index, Amount => (Item.Amount - 1),
                  Name => Item.Name, Durability => Item.Durability,
                  Price => Item.Price));
         end Add_Damaged_Item_Block;
         Inventory(Item_Index).Amount := 1;
      end if;
      Inventory(Item_Index).Durability := Inventory(Item_Index).Durability - 1;
      if Inventory(Item_Index).Durability = 0 then -- Item destroyed
         if Member_Index = 0 then
            UpdateCargo(Ship => Ship, CargoIndex => Item_Index, Amount => -1);
         else
            UpdateInventory
              (MemberIndex => Member_Index, Amount => -1,
               InventoryIndex => Item_Index, Ship => Ship);
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
                    (Ship => Ship, CargoIndex => J,
                     Amount => -(Inventory.Element(Index => J).Amount));
                  UpdateCargo
                    (Ship => Ship, CargoIndex => I,
                     Amount => Inventory.Element(Index => J).Amount);
               else
                  UpdateInventory
                    (MemberIndex => Member_Index,
                     Amount => -(Inventory.Element(Index => J).Amount),
                     InventoryIndex => J, Ship => Ship);
                  UpdateInventory
                    (MemberIndex => Member_Index,
                     Amount => Inventory.Element(Index => J).Amount,
                     InventoryIndex => I, Ship => Ship);
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
                  return Inventory_Container.To_Index(Position => I);
               else
                  return Inventory_Container.To_Index(Position => I);
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
                  return Inventory_Container.To_Index(Position => I);
               else
                  return Inventory_Container.To_Index(Position => I);
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
                  (Source =>
                     To_String
                       (Source =>
                          SkillsData_Container.Element
                            (Container => Skills_List, Index => I)
                            .Tool))) =
           UnboundedString_Container.No_Index then
            Tools_List.Append
              (New_Item =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           SkillsData_Container.Element
                             (Container => Skills_List, Index => I)
                             .Tool)));
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
