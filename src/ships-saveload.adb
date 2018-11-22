--    Copyright 2017-2018 Bartek thindil Jasicki
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

with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with ShipModules; use ShipModules;
with Factions; use Factions;
with Bases; use Bases;

package body Ships.SaveLoad is

   procedure SavePlayerShip(SaveData: Document; MainNode: DOM.Core.Element) is
      RawValue: Unbounded_String;
      CategoryNode, DataNode: DOM.Core.Element;
   begin
      CategoryNode := Create_Element(SaveData, "playership");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "name", To_String(PlayerShip.Name));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
      Set_Attribute
        (CategoryNode, "x", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
      Set_Attribute
        (CategoryNode, "y", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue :=
        To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
      Set_Attribute
        (CategoryNode, "speed", To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.UpgradeModule));
      Set_Attribute
        (CategoryNode, "upgrademodule",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationX));
      Set_Attribute
        (CategoryNode, "destinationx",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationY));
      Set_Attribute
        (CategoryNode, "destinationy",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.RepairModule));
      Set_Attribute
        (CategoryNode, "repairpriority",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.HomeBase));
      Set_Attribute
        (CategoryNode, "homebase",
         To_String(Trim(RawValue, Ada.Strings.Left)));
      declare
         ModuleDataNode: DOM.Core.Element;
      begin
         for Module of PlayerShip.Modules loop
            DataNode := Create_Element(SaveData, "module");
            DataNode := Append_Child(CategoryNode, DataNode);
            Set_Attribute(DataNode, "name", To_String(Module.Name));
            Set_Attribute
              (DataNode, "index",
               To_String(Modules_List(Module.ProtoIndex).Index));
            RawValue := To_Unbounded_String(Integer'Image(Module.Weight));
            Set_Attribute
              (DataNode, "weight",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Module.Durability));
            Set_Attribute
              (DataNode, "durability",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(Module.MaxDurability));
            Set_Attribute
              (DataNode, "maxdurability",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue := To_Unbounded_String(Integer'Image(Module.Owner));
            Set_Attribute
              (DataNode, "owner", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(Module.UpgradeProgress));
            Set_Attribute
              (DataNode, "upgradeprogress",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String
                (Integer'Image(ShipUpgrade'Pos(Module.UpgradeAction)));
            Set_Attribute
              (DataNode, "upgradeaction",
               To_String(Trim(RawValue, Ada.Strings.Left)));
            for I in Module.Data'Range loop
               ModuleDataNode := Create_Element(SaveData, "data");
               ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
               RawValue := To_Unbounded_String(Integer'Image(Module.Data(I)));
               Set_Attribute
                 (ModuleDataNode, "value",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
         end loop;
      end;
      for Item of PlayerShip.Cargo loop
         DataNode := Create_Element(SaveData, "cargo");
         DataNode := Append_Child(CategoryNode, DataNode);
         Set_Attribute
           (DataNode, "index", To_String(Items_List(Item.ProtoIndex).Index));
         RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
         Set_Attribute
           (DataNode, "amount", To_String(Trim(RawValue, Ada.Strings.Left)));
         if Item.Name /= Null_Unbounded_String then
            Set_Attribute(DataNode, "name", To_String(Item.Name));
         end if;
         RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
         Set_Attribute
           (DataNode, "durability",
            To_String(Trim(RawValue, Ada.Strings.Left)));
      end loop;
      declare
         StatNode: DOM.Core.Element;
         AttributesNames: constant array
           (Positive range <>) of Unbounded_String :=
           (To_Unbounded_String("health"), To_Unbounded_String("tired"),
            To_Unbounded_String("hunger"), To_Unbounded_String("thirst"),
            To_Unbounded_String("order"), To_Unbounded_String("previousorder"),
            To_Unbounded_String("ordertime"), To_Unbounded_String("dailypay"),
            To_Unbounded_String("tradepay"),
            To_Unbounded_String("contractlength"),
            To_Unbounded_String("moralelevel"),
            To_Unbounded_String("moralepoints"),
            To_Unbounded_String("loyalty"), To_Unbounded_String("homebase"));
         AttributesValues: array(AttributesNames'Range) of Integer;
      begin
         for Member of PlayerShip.Crew loop
            DataNode := Create_Element(SaveData, "member");
            DataNode := Append_Child(CategoryNode, DataNode);
            Set_Attribute(DataNode, "name", To_String(Member.Name));
            Set_Attribute(DataNode, "gender", Member.Gender & "");
            Set_Attribute
              (DataNode, "faction",
               To_String(Factions_List(Member.Faction).Index));
            AttributesValues :=
              (Member.Health, Member.Tired, Member.Hunger, Member.Thirst,
               Crew_Orders'Pos(Member.Order),
               Crew_Orders'Pos(Member.PreviousOrder), Member.OrderTime,
               Member.Payment(1), Member.Payment(2), Member.ContractLength,
               Member.Morale(1), Member.Morale(2), Member.Loyalty,
               Member.HomeBase);
            for I in AttributesNames'Range loop
               RawValue :=
                 To_Unbounded_String(Integer'Image(AttributesValues(I)));
               Set_Attribute
                 (DataNode, To_String(AttributesNames(I)),
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
            for Skill of Member.Skills loop
               StatNode := Create_Element(SaveData, "skill");
               StatNode := Append_Child(DataNode, StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(1)));
               Set_Attribute
                 (StatNode, "index",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               RawValue := To_Unbounded_String(Integer'Image(Skill(2)));
               Set_Attribute
                 (StatNode, "level",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               RawValue := To_Unbounded_String(Integer'Image(Skill(3)));
               Set_Attribute
                 (StatNode, "experience",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
            for J in Member.Orders'Range loop
               StatNode := Create_Element(SaveData, "priority");
               StatNode := Append_Child(DataNode, StatNode);
               RawValue :=
                 To_Unbounded_String(Integer'Image(Member.Orders(J)));
               Set_Attribute
                 (StatNode, "value",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
            for Attribute of Member.Attributes loop
               StatNode := Create_Element(SaveData, "attribute");
               StatNode := Append_Child(DataNode, StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Attribute(1)));
               Set_Attribute
                 (StatNode, "level",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               RawValue := To_Unbounded_String(Integer'Image(Attribute(2)));
               Set_Attribute
                 (StatNode, "experience",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
            for Item of Member.Inventory loop
               StatNode := Create_Element(SaveData, "item");
               StatNode := Append_Child(DataNode, StatNode);
               Set_Attribute
                 (StatNode, "index",
                  To_String(Items_List(Item.ProtoIndex).Index));
               RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
               Set_Attribute
                 (StatNode, "amount",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               if Item.Name /= Null_Unbounded_String then
                  Set_Attribute(StatNode, "name", To_String(Item.Name));
               end if;
               RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
               Set_Attribute
                 (StatNode, "durability",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
            for I in Member.Equipment'Range loop
               StatNode := Create_Element(SaveData, "equipment");
               StatNode := Append_Child(DataNode, StatNode);
               RawValue :=
                 To_Unbounded_String(Integer'Image(Member.Equipment(I)));
               Set_Attribute
                 (StatNode, "index",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end loop;
         end loop;
      end;
   end SavePlayerShip;

   procedure LoadPlayerShip(SaveData: Document) is
      --BaseMissions: Mission_Container.Vector;
      ShipNode, ChildNodes: Node_List;
   begin
      ShipNode :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "playership");
      PlayerShip.Name :=
        To_Unbounded_String(Get_Attribute(Item(ShipNode, 0), "name"));
      PlayerShip.SkyX := Integer'Value(Get_Attribute(Item(ShipNode, 0), "x"));
      PlayerShip.SkyY := Integer'Value(Get_Attribute(Item(ShipNode, 0), "y"));
      PlayerShip.Speed :=
        ShipSpeed'Val
          (Integer'Value(Get_Attribute(Item(ShipNode, 0), "speed")));
      PlayerShip.UpgradeModule :=
        Integer'Value(Get_Attribute(Item(ShipNode, 0), "upgrademodule"));
      PlayerShip.DestinationX :=
        Integer'Value(Get_Attribute(Item(ShipNode, 0), "destinationx"));
      PlayerShip.DestinationY :=
        Integer'Value(Get_Attribute(Item(ShipNode, 0), "destinationy"));
      PlayerShip.RepairModule :=
        Integer'Value(Get_Attribute(Item(ShipNode, 0), "repairpriority"));
      PlayerShip.HomeBase :=
        Integer'Value(Get_Attribute(Item(ShipNode, 0), "homebase"));
      PlayerShip.Modules.Clear;
      PlayerShip.Cargo.Clear;
      PlayerShip.Crew.Clear;
      ChildNodes := Child_Nodes(Item(ShipNode, 0));
      for I in 0 .. Length(ChildNodes) - 1 loop
         if Node_Name(Item(ChildNodes, I)) = "module" then
            declare
               ModuleData: Node_List;
               Name: Unbounded_String;
               ProtoIndex, DataIndex: Positive;
               Weight, Owner: Natural;
               Durability, MaxDurability, UpgradeProgress: Integer;
               UpgradeAction: ShipUpgrade;
               Data: Data_Array;
            begin
               Name :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, I), "name"));
               ProtoIndex :=
                 FindProtoModule
                   (To_Unbounded_String
                      (Get_Attribute(Item(ChildNodes, I), "index")));
               Weight :=
                 Natural'Value(Get_Attribute(Item(ChildNodes, I), "weight"));
               Owner :=
                 Natural'Value(Get_Attribute(Item(ChildNodes, I), "owner"));
               Durability :=
                 Integer'Value
                   (Get_Attribute(Item(ChildNodes, I), "durability"));
               MaxDurability :=
                 Integer'Value
                   (Get_Attribute(Item(ChildNodes, I), "maxdurability"));
               UpgradeAction :=
                 ShipUpgrade'Val
                   (Integer'Value
                      (Get_Attribute(Item(ChildNodes, I), "upgradeaction")));
               UpgradeProgress :=
                 Integer'Value
                   (Get_Attribute(Item(ChildNodes, I), "upgradeprogress"));
               Data := (others => 0);
               ModuleData := Child_Nodes(Item(ChildNodes, I));
               DataIndex := 1;
               for K in 0 .. Length(ModuleData) - 1 loop
                  if Node_Name(Item(ModuleData, K)) = "data" then
                     Data(DataIndex) :=
                       Integer'Value
                         (Get_Attribute(Item(ModuleData, K), "value"));
                     DataIndex := DataIndex + 1;
                  end if;
               end loop;
               PlayerShip.Modules.Append
                 (New_Item =>
                    (Name => Name, ProtoIndex => ProtoIndex, Weight => Weight,
                     Durability => Durability, MaxDurability => MaxDurability,
                     Owner => Owner, UpgradeProgress => UpgradeProgress,
                     UpgradeAction => UpgradeAction, Data => Data));
            end;
         elsif Node_Name(Item(ChildNodes, I)) = "cargo" then
            declare
               ProtoIndex, Amount: Positive;
               Name: Unbounded_String;
               Durability: Natural;
            begin
               ProtoIndex :=
                 FindProtoItem
                   (To_Unbounded_String
                      (Get_Attribute(Item(ChildNodes, I), "index")));
               Amount :=
                 Positive'Value(Get_Attribute(Item(ChildNodes, I), "amount"));
               Name :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, I), "name"));
               Durability :=
                 Natural'Value
                   (Get_Attribute(Item(ChildNodes, I), "durability"));
               PlayerShip.Cargo.Append
                 (New_Item =>
                    (ProtoIndex => ProtoIndex, Amount => Amount, Name => Name,
                     Durability => Durability));
            end;
         elsif Node_Name(Item(ChildNodes, I)) = "member" then
            declare
               MemberData: Node_List;
               Name, ItemName: Unbounded_String;
               Gender: String(1 .. 1);
               Health, Tired, Hunger, Thirst, Index, Level, Experience,
               Loyalty: Natural;
               Skills: Skills_Container.Vector;
               Attributes: Attributes_Container.Vector;
               Order, PreviousOrder: Crew_Orders;
               Orders: Orders_Array;
               Inventory: Inventory_Container.Vector;
               Equipment: Equipment_Array;
               OrderTime, ContractLength: Integer;
               Amount, Durability, EquipmentIndex, PriorityIndex, HomeBase,
               FactionIndex: Positive;
               Payment, Morale: Attributes_Array;
            begin
               Skills.Clear;
               Attributes.Clear;
               Inventory.Clear;
               Name :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, I), "name"));
               Gender := Get_Attribute(Item(ChildNodes, I), "gender");
               Health :=
                 Integer'Value(Get_Attribute(Item(ChildNodes, I), "health"));
               Tired :=
                 Integer'Value(Get_Attribute(Item(ChildNodes, I), "tired"));
               Hunger :=
                 Integer'Value(Get_Attribute(Item(ChildNodes, I), "hunger"));
               Thirst :=
                 Integer'Value(Get_Attribute(Item(ChildNodes, I), "thirst"));
               Order :=
                 Crew_Orders'Val
                   (Integer'Value
                      (Get_Attribute(Item(ChildNodes, I), "order")));
               PreviousOrder :=
                 Crew_Orders'Val
                   (Integer'Value
                      (Get_Attribute(Item(ChildNodes, I), "previousorder")));
               Orders := (others => 0);
               Equipment := (others => 0);
               OrderTime :=
                 Integer'Value
                   (Get_Attribute(Item(ChildNodes, I), "ordertime"));
               EquipmentIndex := 1;
               MemberData := Child_Nodes(Item(ChildNodes, I));
               PriorityIndex := 1;
               Payment(1) :=
                 Natural'Value(Get_Attribute(Item(ChildNodes, I), "dailypay"));
               Payment(2) :=
                 Natural'Value(Get_Attribute(Item(ChildNodes, I), "tradepay"));
               ContractLength :=
                 Integer'Value
                   (Get_Attribute(Item(ChildNodes, I), "contractlength"));
               if Get_Attribute(Item(ChildNodes, I), "morale") /= "" then
                  Morale :=
                    (Natural'Value
                       (Get_Attribute(Item(ChildNodes, I), "morale")),
                     0);
               else
                  Morale := (50, 0);
               end if;
               if Get_Attribute(Item(ChildNodes, I), "moralelevel") /= "" then
                  Morale(1) :=
                    Natural'Value
                      (Get_Attribute(Item(ChildNodes, I), "moralelevel"));
               end if;
               if Get_Attribute(Item(ChildNodes, I), "moralepoints") /= "" then
                  Morale(2) :=
                    Natural'Value
                      (Get_Attribute(Item(ChildNodes, I), "moralepoints"));
               end if;
               if Get_Attribute(Item(ChildNodes, I), "loyalty") /= "" then
                  Loyalty :=
                    Natural'Value
                      (Get_Attribute(Item(ChildNodes, I), "loyalty"));
               else
                  Loyalty := 100;
               end if;
               for K in 0 .. Length(MemberData) - 1 loop
                  if Node_Name(Item(MemberData, K)) = "skill" then
                     Index :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "index"));
                     Level :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "level"));
                     Experience :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "experience"));
                     Skills.Append(New_Item => (Index, Level, Experience));
                  elsif Node_Name(Item(MemberData, K)) = "priority" then
                     Orders(PriorityIndex) :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "value"));
                     PriorityIndex := PriorityIndex + 1;
                  elsif Node_Name(Item(MemberData, K)) = "attribute" then
                     Level :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "level"));
                     Experience :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "experience"));
                     Attributes.Append(New_Item => (Level, Experience));
                  elsif Node_Name(Item(MemberData, K)) = "item" then
                     Index :=
                       FindProtoItem
                         (To_Unbounded_String
                            (Get_Attribute(Item(MemberData, K), "index")));
                     Amount :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "amount"));
                     ItemName :=
                       To_Unbounded_String
                         (Get_Attribute(Item(MemberData, K), "name"));
                     Durability :=
                       Integer'Value
                         (Get_Attribute(Item(MemberData, K), "durability"));
                     Inventory.Append
                       (New_Item =>
                          (ProtoIndex => Index, Amount => Amount,
                           Name => ItemName, Durability => Durability));
                  elsif Node_Name(Item(MemberData, K)) = "equipment" then
                     Equipment(EquipmentIndex) :=
                       Natural'Value
                         (Get_Attribute(Item(MemberData, K), "index"));
                     EquipmentIndex := EquipmentIndex + 1;
                  end if;
               end loop;
               if Get_Attribute(Item(ChildNodes, I), "homebase") /= "" then
                  HomeBase :=
                    Natural'Value
                      (Get_Attribute(Item(ChildNodes, I), "homebase"));
               else
                  HomeBase := PlayerShip.HomeBase;
               end if;
               if Get_Attribute(Item(ChildNodes, I), "faction") /= "" then
                  for J in Factions_List.Iterate loop
                     if Factions_List(J).Index =
                       To_Unbounded_String
                         (Get_Attribute(Item(ChildNodes, I), "faction")) then
                        FactionIndex := Factions_Container.To_Index(J);
                        exit;
                     end if;
                  end loop;
               else
                  FactionIndex := SkyBases(HomeBase).Owner;
               end if;
               PlayerShip.Crew.Append
                 (New_Item =>
                    (Name => Name, Gender => Gender(1), Health => Health,
                     Tired => Tired, Skills => Skills, Hunger => Hunger,
                     Thirst => Thirst, Order => Order,
                     PreviousOrder => PreviousOrder, OrderTime => OrderTime,
                     Orders => Orders, Attributes => Attributes,
                     Inventory => Inventory, Equipment => Equipment,
                     Payment => Payment, ContractLength => ContractLength,
                     Morale => Morale, Loyalty => Loyalty,
                     HomeBase => HomeBase, Faction => FactionIndex));
            end;
         end if;
      end loop;
   end LoadPlayerShip;

end Ships.SaveLoad;
