--    Copyright 2017-2021 Bartek thindil Jasicki
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with ShipModules; use ShipModules;
with Bases; use Bases;

package body Ships.SaveLoad is

   procedure SavePlayerShip(SaveData: Document; MainNode: DOM.Core.Element) is
      CategoryNode, DataNode: DOM.Core.Element;
      procedure SaveNumber
        (Value: Integer; Name: String;
         Node: DOM.Core.Element := CategoryNode) is
         RawValue: constant String :=
           Trim(Integer'Image(Value), Ada.Strings.Left);
      begin
         Set_Attribute(Node, Name, RawValue);
      end SaveNumber;
   begin
      CategoryNode := Create_Element(SaveData, "Player_Ship");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      Set_Attribute(CategoryNode, "name", To_String(Player_Ship.Name));
      SaveNumber(Player_Ship.Sky_X, "x");
      SaveNumber(Player_Ship.Sky_Y, "y");
      SaveNumber(Ship_Speed'Pos(Player_Ship.Speed), "speed");
      SaveNumber(Player_Ship.Upgrade_Module, "upgrademodule");
      SaveNumber(Player_Ship.Destination_X, "destinationx");
      SaveNumber(Player_Ship.Destination_Y, "destinationy");
      SaveNumber(Player_Ship.Repair_Module, "repairpriority");
      SaveNumber(Player_Ship.Home_Base, "homebase");
      declare
         ModuleDataNode: DOM.Core.Element;
      begin
         Save_Modules_Loop :
         for Module of Player_Ship.Modules loop
            DataNode := Create_Element(SaveData, "module");
            DataNode := Append_Child(CategoryNode, DataNode);
            Set_Attribute(DataNode, "name", To_String(Module.Name));
            Set_Attribute(DataNode, "index", To_String(Module.Proto_Index));
            SaveNumber(Module.Weight, "weight", DataNode);
            SaveNumber(Module.Durability, "durability", DataNode);
            SaveNumber(Module.Max_Durability, "maxdurability", DataNode);
            Save_Module_Owners_Loop :
            for Owner of Module.Owner loop
               ModuleDataNode := Create_Element(SaveData, "owner");
               ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
               SaveNumber(Owner, "value", ModuleDataNode);
            end loop Save_Module_Owners_Loop;
            if Module.Upgrade_Progress > 0 then
               SaveNumber
                 (Module.Upgrade_Progress, "upgradeprogress", DataNode);
            end if;
            if Module.Upgrade_Action /= NONE then
               SaveNumber
                 (Ship_Upgrade'Pos(Module.Upgrade_Action), "upgradeaction",
                  DataNode);
            end if;
            case Module.M_Type is
               when WORKSHOP =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Module.Crafting_Index));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Crafting_Time, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Crafting_Amount, "value", ModuleDataNode);
               when TRAINING_ROOM =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Trained_Skill, "value", ModuleDataNode);
               when MEDICAL_ROOM | COCKPIT | ARMOR | ANY | CARGO_ROOM =>
                  null;
               when ENGINE =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Fuel_Usage, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Power, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  if Module.Disabled then
                     Set_Attribute(ModuleDataNode, "value", "1");
                  else
                     Set_Attribute(ModuleDataNode, "value", "0");
                  end if;
               when CABIN =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Cleanliness, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Quality, "value", ModuleDataNode);
               when TURRET =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Gun_Index, "value", ModuleDataNode);
               when GUN =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Ammo_Index, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Damage, "value", ModuleDataNode);
               when HULL =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber
                    (Module.Installed_Modules, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Max_Modules, "value", ModuleDataNode);
               when BATTERING_RAM =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Damage2, "value", ModuleDataNode);
               when HARPOON_GUN =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Harpoon_Index, "value", ModuleDataNode);
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  SaveNumber(Module.Duration, "value", ModuleDataNode);
            end case;
         end loop Save_Modules_Loop;
      end;
      Save_Cargo_Loop :
      for Item of Player_Ship.Cargo loop
         DataNode := Create_Element(SaveData, "cargo");
         DataNode := Append_Child(CategoryNode, DataNode);
         Set_Attribute(DataNode, "index", To_String(Item.ProtoIndex));
         SaveNumber(Item.Amount, "amount", DataNode);
         if Item.Name /= Null_Unbounded_String then
            Set_Attribute(DataNode, "name", To_String(Item.Name));
         end if;
         SaveNumber(Item.Durability, "durability", DataNode);
         if Item.Price > 0 then
            SaveNumber(Item.Price, "price", DataNode);
         end if;
      end loop Save_Cargo_Loop;
      declare
         StatNode: DOM.Core.Element;
         AttributesNames: constant array(1 .. 14) of Unbounded_String :=
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
         Save_Crew_Loop :
         for Member of Player_Ship.Crew loop
            DataNode := Create_Element(SaveData, "member");
            DataNode := Append_Child(CategoryNode, DataNode);
            Set_Attribute(DataNode, "name", To_String(Member.Name));
            Set_Attribute(DataNode, "gender", Member.Gender & "");
            Set_Attribute(DataNode, "faction", To_String(Member.Faction));
            AttributesValues :=
              (Member.Health, Member.Tired, Member.Hunger, Member.Thirst,
               Crew_Orders'Pos(Member.Order),
               Crew_Orders'Pos(Member.PreviousOrder), Member.OrderTime,
               Member.Payment(1), Member.Payment(2), Member.ContractLength,
               Member.Morale(1), Member.Morale(2), Member.Loyalty,
               Member.HomeBase);
            Save_Characteristics_Loop :
            for I in AttributesNames'Range loop
               SaveNumber
                 (AttributesValues(I), To_String(AttributesNames(I)),
                  DataNode);
            end loop Save_Characteristics_Loop;
            Save_Skills_Loop :
            for Skill of Member.Skills loop
               StatNode := Create_Element(SaveData, "skill");
               StatNode := Append_Child(DataNode, StatNode);
               SaveNumber(Skill(1), "index", StatNode);
               SaveNumber(Skill(2), "level", StatNode);
               if Skill(3) > 0 then
                  SaveNumber(Skill(3), "experience", StatNode);
               end if;
            end loop Save_Skills_Loop;
            Save_Priorities_Loop :
            for J in Member.Orders'Range loop
               StatNode := Create_Element(SaveData, "priority");
               StatNode := Append_Child(DataNode, StatNode);
               SaveNumber(Member.Orders(J), "value", StatNode);
            end loop Save_Priorities_Loop;
            Save_Attributes_Loop :
            for Attribute of Member.Attributes loop
               StatNode := Create_Element(SaveData, "attribute");
               StatNode := Append_Child(DataNode, StatNode);
               SaveNumber(Attribute(1), "level", StatNode);
               if Attribute(2) > 0 then
                  SaveNumber(Attribute(2), "experience", StatNode);
               end if;
            end loop Save_Attributes_Loop;
            Save_Inventory_Loop :
            for Item of Member.Inventory loop
               StatNode := Create_Element(SaveData, "item");
               StatNode := Append_Child(DataNode, StatNode);
               Set_Attribute(StatNode, "index", To_String(Item.ProtoIndex));
               SaveNumber(Item.Amount, "amount", StatNode);
               if Item.Name /= Null_Unbounded_String then
                  Set_Attribute(StatNode, "name", To_String(Item.Name));
               end if;
               SaveNumber(Item.Durability, "durability", StatNode);
               if Item.Price > 0 then
                  SaveNumber(Item.Price, "price", StatNode);
               end if;
            end loop Save_Inventory_Loop;
            Save_Equipment_Loop :
            for I in Member.Equipment'Range loop
               StatNode := Create_Element(SaveData, "equipment");
               StatNode := Append_Child(DataNode, StatNode);
               SaveNumber(Member.Equipment(I), "index", StatNode);
            end loop Save_Equipment_Loop;
         end loop Save_Crew_Loop;
      end;
   end SavePlayerShip;

   procedure LoadPlayerShip(SaveData: Document) is
      ShipNode, ChildNodes: Node_List;
      LoadNode, ChildNode: Node;
   begin
      ShipNode :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "Player_Ship");
      LoadNode := Item(ShipNode, 0);
      Player_Ship.Name := To_Unbounded_String(Get_Attribute(LoadNode, "name"));
      Player_Ship.Sky_X := Integer'Value(Get_Attribute(LoadNode, "x"));
      Player_Ship.Sky_Y := Integer'Value(Get_Attribute(LoadNode, "y"));
      Player_Ship.Speed :=
        Ship_Speed'Val(Integer'Value(Get_Attribute(LoadNode, "speed")));
      Player_Ship.Upgrade_Module :=
        Integer'Value(Get_Attribute(LoadNode, "upgrademodule"));
      Player_Ship.Destination_X :=
        Integer'Value(Get_Attribute(LoadNode, "destinationx"));
      Player_Ship.Destination_Y :=
        Integer'Value(Get_Attribute(LoadNode, "destinationy"));
      Player_Ship.Repair_Module :=
        Integer'Value(Get_Attribute(LoadNode, "repairpriority"));
      Player_Ship.Home_Base :=
        Integer'Value(Get_Attribute(LoadNode, "homebase"));
      Player_Ship.Modules.Clear;
      Player_Ship.Cargo.Clear;
      Player_Ship.Crew.Clear;
      ChildNodes := Child_Nodes(LoadNode);
      Load_Ship_Loop :
      for I in 0 .. Length(ChildNodes) - 1 loop
         ChildNode := Item(ChildNodes, I);
         if Node_Name(ChildNode) = "module" then
            declare
               ModuleData: Node_List;
               Name, ProtoIndex: Unbounded_String;
               DataIndex: Positive;
               Weight: Natural := 0;
               Durability, MaxDurability, UpgradeProgress: Integer := 0;
               UpgradeAction: Ship_Upgrade := NONE;
               Data: Data_Array;
               ModuleNode: Node;
               MType: Module_Type_2;
               Owners: Natural_Container.Vector;
            begin
               Name := To_Unbounded_String(Get_Attribute(ChildNode, "name"));
               ProtoIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               Weight := Natural'Value(Get_Attribute(ChildNode, "weight"));
               if Get_Attribute(ChildNode, "owner") /= "" then
                  Owners.Append
                    (Natural'Value(Get_Attribute(ChildNode, "owner")));
               else
                  ModuleData := Child_Nodes(ChildNode);
                  Load_Owners_Loop :
                  for K in 0 .. Length(ModuleData) - 1 loop
                     ModuleNode := Item(ModuleData, K);
                     if Node_Name(ModuleNode) = "owner" then
                        Owners.Append
                          (Integer'Value(Get_Attribute(ModuleNode, "value")));
                     end if;
                  end loop Load_Owners_Loop;
               end if;
               Durability :=
                 Integer'Value(Get_Attribute(ChildNode, "durability"));
               MaxDurability :=
                 Integer'Value(Get_Attribute(ChildNode, "maxdurability"));
               if Get_Attribute(ChildNode, "upgradeaction") /= "" then
                  UpgradeAction :=
                    Ship_Upgrade'Val
                      (Integer'Value
                         (Get_Attribute(ChildNode, "upgradeaction")));
               end if;
               if Get_Attribute(ChildNode, "upgradeprogress") /= "" then
                  UpgradeProgress :=
                    Integer'Value(Get_Attribute(ChildNode, "upgradeprogress"));
               end if;
               if Get_Attribute(ChildNode, "mtype") /= "" then
                  case Modules_List(ProtoIndex)
                    .MType is -- backward compatybility
                     when MEDICAL_ROOM =>
                        MType := MEDICAL_ROOM;
                     when TRAINING_ROOM =>
                        MType := TRAINING_ROOM;
                     when ENGINE =>
                        MType := ENGINE;
                     when CABIN =>
                        MType := CABIN;
                     when COCKPIT =>
                        MType := COCKPIT;
                     when TURRET =>
                        MType := TURRET;
                     when GUN =>
                        MType := GUN;
                     when CARGO =>
                        MType := CARGO_ROOM;
                     when HULL =>
                        MType := HULL;
                     when ARMOR =>
                        MType := ARMOR;
                     when BATTERING_RAM =>
                        MType := BATTERING_RAM;
                     when HARPOON_GUN =>
                        MType := HARPOON_GUN;
                     when others =>
                        MType :=
                          Module_Type_2'Value
                            (Get_Attribute(ChildNode, "mtype"));
                  end case;
               else
                  case Modules_List(ProtoIndex).MType is
                     when ALCHEMY_LAB .. GREENHOUSE =>
                        MType := WORKSHOP;
                     when MEDICAL_ROOM =>
                        MType := MEDICAL_ROOM;
                     when TRAINING_ROOM =>
                        MType := TRAINING_ROOM;
                     when ENGINE =>
                        MType := ENGINE;
                     when CABIN =>
                        MType := CABIN;
                     when COCKPIT =>
                        MType := COCKPIT;
                     when TURRET =>
                        MType := TURRET;
                     when GUN =>
                        MType := GUN;
                     when CARGO =>
                        MType := CARGO_ROOM;
                     when HULL =>
                        MType := HULL;
                     when ARMOR =>
                        MType := ARMOR;
                     when BATTERING_RAM =>
                        MType := BATTERING_RAM;
                     when HARPOON_GUN =>
                        MType := HARPOON_GUN;
                     when others =>
                        MType := ANY;
                  end case;
               end if;
               case MType is
                  when ANY =>
                     Data := (others => 0);
                     ModuleData := Child_Nodes(ChildNode);
                     DataIndex := 1;
                     Load_Module_Data_Loop :
                     for K in 0 .. Length(ModuleData) - 1 loop
                        ModuleNode := Item(ModuleData, K);
                        if Node_Name(ModuleNode) = "data" then
                           Data(DataIndex) :=
                             Integer'Value(Get_Attribute(ModuleNode, "value"));
                           DataIndex := DataIndex + 1;
                        end if;
                     end loop Load_Module_Data_Loop;
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => ANY, Name => Name,
                           Proto_Index => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           Max_Durability => MaxDurability, Owner => Owners,
                           Upgrade_Progress => UpgradeProgress,
                           Upgrade_Action => UpgradeAction, Data => Data));
                  when ENGINE =>
                     declare
                        FuelUsage, Power: Positive;
                        Disabled: Boolean;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Engine_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" then
                              case DataIndex is
                                 when 1 =>
                                    FuelUsage :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 2 =>
                                    Power :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 3 =>
                                    if Get_Attribute(ModuleNode, "value") =
                                      "0" then
                                       Disabled := False;
                                    else
                                       Disabled := True;
                                    end if;
                                 when others =>
                                    null;
                              end case;
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Engine_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => ENGINE, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Fuel_Usage => FuelUsage, Power => Power,
                              Disabled => Disabled));
                     end;
                  when CABIN =>
                     declare
                        Cleanliness, Quality: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Cabin_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" then
                              case DataIndex is
                                 when 1 =>
                                    Cleanliness :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 2 =>
                                    Quality :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when others =>
                                    null;
                              end case;
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Cabin_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => CABIN, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Cleanliness => Cleanliness, Quality => Quality));
                     end;
                  when COCKPIT =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => COCKPIT, Name => Name,
                           Proto_Index => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           Max_Durability => MaxDurability, Owner => Owners,
                           Upgrade_Progress => UpgradeProgress,
                           Upgrade_Action => UpgradeAction));
                  when WORKSHOP =>
                     declare
                        CraftingIndex: Unbounded_String;
                        CraftingTime, CraftingAmount: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Workshop_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" then
                              case DataIndex is
                                 when 1 =>
                                    CraftingIndex :=
                                      To_Unbounded_String
                                        (Get_Attribute(ModuleNode, "value"));
                                    if CraftingIndex =
                                      To_Unbounded_String("0") then
                                       CraftingIndex := Null_Unbounded_String;
                                    end if;
                                 when 2 =>
                                    CraftingTime :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 3 =>
                                    CraftingAmount :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when others =>
                                    null;
                              end case;
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Workshop_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => WORKSHOP, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Crafting_Index => CraftingIndex,
                              Crafting_Time => CraftingTime,
                              Crafting_Amount => CraftingAmount));
                     end;
                  when MEDICAL_ROOM =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => MEDICAL_ROOM, Name => Name,
                           Proto_Index => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           Max_Durability => MaxDurability, Owner => Owners,
                           Upgrade_Progress => UpgradeProgress,
                           Upgrade_Action => UpgradeAction));
                  when TRAINING_ROOM =>
                     declare
                        TrainedSkill: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Training_Room_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" and
                             DataIndex = 1 then
                              TrainedSkill :=
                                Integer'Value
                                  (Get_Attribute(ModuleNode, "value"));
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Training_Room_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => TRAINING_ROOM, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Trained_Skill => TrainedSkill));
                     end;
                  when TURRET =>
                     declare
                        GunIndex: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Turret_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" and
                             DataIndex = 1 then
                              GunIndex :=
                                Integer'Value
                                  (Get_Attribute(ModuleNode, "value"));
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Turret_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => TURRET, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Gun_Index => GunIndex));
                     end;
                  when GUN =>
                     declare
                        Damage, AmmoIndex: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Gun_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" then
                              case DataIndex is
                                 when 1 =>
                                    AmmoIndex :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 2 =>
                                    Damage :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when others =>
                                    null;
                              end case;
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Gun_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => GUN, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Damage => Damage, Ammo_Index => AmmoIndex));
                     end;
                  when CARGO_ROOM =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => CARGO_ROOM, Name => Name,
                           Proto_Index => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           Max_Durability => MaxDurability, Owner => Owners,
                           Upgrade_Progress => UpgradeProgress,
                           Upgrade_Action => UpgradeAction));
                  when HULL =>
                     declare
                        InstalledModules, MaxModules: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Hull_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" then
                              case DataIndex is
                                 when 1 =>
                                    InstalledModules :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 2 =>
                                    MaxModules :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when others =>
                                    null;
                              end case;
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Hull_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => HULL, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Installed_Modules => InstalledModules,
                              Max_Modules => MaxModules));
                     end;
                  when ARMOR =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => ARMOR, Name => Name,
                           Proto_Index => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           Max_Durability => MaxDurability, Owner => Owners,
                           Upgrade_Progress => UpgradeProgress,
                           Upgrade_Action => UpgradeAction));
                  when BATTERING_RAM =>
                     declare
                        Damage: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Battering_Ram_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" and
                             DataIndex = 1 then
                              Damage :=
                                Integer'Value
                                  (Get_Attribute(ModuleNode, "value"));
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Battering_Ram_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => BATTERING_RAM, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Damage2 => Damage, Cooling_Down => False));
                     end;
                  when HARPOON_GUN =>
                     declare
                        Duration, HarpoonIndex: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        Load_Harpoon_Gun_Data_Loop :
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" then
                              case DataIndex is
                                 when 1 =>
                                    HarpoonIndex :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when 2 =>
                                    Duration :=
                                      Integer'Value
                                        (Get_Attribute(ModuleNode, "value"));
                                 when others =>
                                    null;
                              end case;
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop Load_Harpoon_Gun_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => HARPOON_GUN, Name => Name,
                              Proto_Index => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              Max_Durability => MaxDurability, Owner => Owners,
                              Upgrade_Progress => UpgradeProgress,
                              Upgrade_Action => UpgradeAction,
                              Duration => Duration,
                              Harpoon_Index => HarpoonIndex));
                     end;
               end case;
            end;
         elsif Node_Name(ChildNode) = "cargo" then
            declare
               Amount: Positive;
               Name, ProtoIndex: Unbounded_String;
               Durability, Price: Natural;
            begin
               ProtoIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               Amount := Positive'Value(Get_Attribute(ChildNode, "amount"));
               Name := To_Unbounded_String(Get_Attribute(ChildNode, "name"));
               Durability :=
                 Natural'Value(Get_Attribute(ChildNode, "durability"));
               Price :=
                 (if Get_Attribute(ChildNode, "price")'Length > 0 then
                    Natural'Value(Get_Attribute(ChildNode, "price"))
                  else 0);
               Player_Ship.Cargo.Append
                 (New_Item =>
                    (ProtoIndex => ProtoIndex, Amount => Amount, Name => Name,
                     Durability => Durability, Price => Price));
            end;
         elsif Node_Name(ChildNode) = "member" then
            declare
               MemberData: Node_List;
               Name, ItemName, FactionIndex, ItemIndex: Unbounded_String;
               Gender: String(1 .. 1);
               Health, Tired, Hunger, Thirst, Index, Level, Experience,
               Loyalty, Price: Natural;
               Skills: Skills_Container.Vector;
               Attributes: Attributes_Container.Vector;
               Order, PreviousOrder: Crew_Orders;
               Orders: Natural_Array(1 .. 12);
               Inventory: Inventory_Container.Vector;
               Equipment: Equipment_Array;
               OrderTime, ContractLength: Integer;
               Amount, Durability, EquipmentIndex, PriorityIndex,
               HomeBase: Positive;
               Payment, Morale: Attributes_Array;
               MemberNode: Node;
            begin
               Skills.Clear;
               Attributes.Clear;
               Inventory.Clear;
               Name := To_Unbounded_String(Get_Attribute(ChildNode, "name"));
               Gender := Get_Attribute(ChildNode, "gender");
               Health := Integer'Value(Get_Attribute(ChildNode, "health"));
               Tired := Integer'Value(Get_Attribute(ChildNode, "tired"));
               Hunger := Integer'Value(Get_Attribute(ChildNode, "hunger"));
               Thirst := Integer'Value(Get_Attribute(ChildNode, "thirst"));
               Order :=
                 Crew_Orders'Val
                   (Integer'Value(Get_Attribute(ChildNode, "order")));
               PreviousOrder :=
                 Crew_Orders'Val
                   (Integer'Value(Get_Attribute(ChildNode, "previousorder")));
               Orders := (others => 0);
               Equipment := (others => 0);
               OrderTime :=
                 Integer'Value(Get_Attribute(ChildNode, "ordertime"));
               EquipmentIndex := 1;
               MemberData := Child_Nodes(ChildNode);
               PriorityIndex := 1;
               Payment(1) :=
                 Natural'Value(Get_Attribute(ChildNode, "dailypay"));
               Payment(2) :=
                 Natural'Value(Get_Attribute(ChildNode, "tradepay"));
               ContractLength :=
                 Integer'Value(Get_Attribute(ChildNode, "contractlength"));
               Morale(1) :=
                 Natural'Value(Get_Attribute(ChildNode, "moralelevel"));
               Morale(2) :=
                 Natural'Value(Get_Attribute(ChildNode, "moralepoints"));
               Loyalty := Natural'Value(Get_Attribute(ChildNode, "loyalty"));
               Load_Crew_Loop :
               for K in 0 .. Length(MemberData) - 1 loop
                  MemberNode := Item(MemberData, K);
                  if Node_Name(MemberNode) = "skill" then
                     Index :=
                       Integer'Value(Get_Attribute(MemberNode, "index"));
                     Level :=
                       Integer'Value(Get_Attribute(MemberNode, "level"));
                     Experience :=
                       (if Get_Attribute(MemberNode, "experience") /= "" then
                          Integer'Value
                            (Get_Attribute(MemberNode, "experience"))
                        else 0);
                     Skills.Append(New_Item => (Index, Level, Experience));
                  elsif Node_Name(MemberNode) = "priority" then
                     Orders(PriorityIndex) :=
                       Integer'Value(Get_Attribute(MemberNode, "value"));
                     PriorityIndex := PriorityIndex + 1;
                  elsif Node_Name(MemberNode) = "attribute" then
                     Level :=
                       Integer'Value(Get_Attribute(MemberNode, "level"));
                     Experience :=
                       (if Get_Attribute(MemberNode, "experience") /= "" then
                          Integer'Value
                            (Get_Attribute(MemberNode, "experience"))
                        else 0);
                     Attributes.Append(New_Item => (Level, Experience));
                  elsif Node_Name(MemberNode) = "item" then
                     ItemIndex :=
                       To_Unbounded_String(Get_Attribute(MemberNode, "index"));
                     Amount :=
                       Integer'Value(Get_Attribute(MemberNode, "amount"));
                     ItemName :=
                       To_Unbounded_String(Get_Attribute(MemberNode, "name"));
                     Durability :=
                       Integer'Value(Get_Attribute(MemberNode, "durability"));
                     Price :=
                       (if Get_Attribute(MemberNode, "price")'Length > 0 then
                          Integer'Value(Get_Attribute(MemberNode, "price"))
                        else 0);
                     Inventory.Append
                       (New_Item =>
                          (ProtoIndex => ItemIndex, Amount => Amount,
                           Name => ItemName, Durability => Durability,
                           Price => Price));
                  elsif Node_Name(MemberNode) = "equipment" then
                     Equipment(EquipmentIndex) :=
                       Natural'Value(Get_Attribute(MemberNode, "index"));
                     EquipmentIndex := EquipmentIndex + 1;
                  end if;
               end loop Load_Crew_Loop;
               HomeBase :=
                 (if Get_Attribute(ChildNode, "homebase") /= "" then
                    Natural'Value(Get_Attribute(ChildNode, "homebase"))
                  else Player_Ship.Home_Base);
               FactionIndex :=
                 (if Get_Attribute(ChildNode, "faction") /= "" then
                    To_Unbounded_String(Get_Attribute(ChildNode, "faction"))
                  else SkyBases(HomeBase).Owner);
               Player_Ship.Crew.Append
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
      end loop Load_Ship_Loop;
   end LoadPlayerShip;

end Ships.SaveLoad;
