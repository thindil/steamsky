--    Copyright 2017-2022 Bartek thindil Jasicki
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

   procedure Save_Player_Ship
     (Save_Data: Document; Main_Node: DOM.Core.Element) is
      use Tiny_String;

      Category_Node, Data_Node: DOM.Core.Element;
      procedure Save_Number
        (Value: Integer; Name: String;
         Node: DOM.Core.Element := Category_Node) is
         Raw_Value: constant String :=
           Trim(Source => Integer'Image(Value), Side => Ada.Strings.Left);
      begin
         Set_Attribute(Elem => Node, Name => Name, Value => Raw_Value);
      end Save_Number;
   begin
      Category_Node :=
        Create_Element(Doc => Save_Data, Tag_Name => "playership");
      Category_Node :=
        Append_Child(N => Main_Node, New_Child => Category_Node);
      Set_Attribute
        (Elem => Category_Node, Name => "name",
         Value => To_String(Source => Player_Ship.Name));
      Save_Number(Value => Player_Ship.Sky_X, Name => "x");
      Save_Number(Value => Player_Ship.Sky_Y, Name => "y");
      Save_Number(Value => Ship_Speed'Pos(Player_Ship.Speed), Name => "speed");
      Save_Number
        (Value => Player_Ship.Upgrade_Module, Name => "upgrademodule");
      Save_Number(Value => Player_Ship.Destination_X, Name => "destinationx");
      Save_Number(Value => Player_Ship.Destination_Y, Name => "destinationy");
      Save_Number
        (Value => Player_Ship.Repair_Module, Name => "repairpriority");
      Save_Number(Value => Player_Ship.Home_Base, Name => "homebase");
      Save_Modules_Block :
      declare
         Module_Data_Node: DOM.Core.Element;
      begin
         Save_Modules_Loop :
         for Module of Player_Ship.Modules loop
            Data_Node :=
              Create_Element(Doc => Save_Data, Tag_Name => "module");
            Data_Node :=
              Append_Child(N => Category_Node, New_Child => Data_Node);
            Set_Attribute(Elem => Data_Node, Name => "name", Value => To_String(Source => Module.Name));
            Set_Attribute(Elem => Data_Node, Name => "index", Value => To_String(Source => Module.Proto_Index));
            Save_Number(Value => Module.Weight, Name => "weight", Node => Data_Node);
            Save_Number(Value => Module.Durability, Name => "durability", Node => Data_Node);
            Save_Number(Value => Module.Max_Durability, Name => "maxdurability", Node => Data_Node);
            Save_Module_Owners_Loop :
            for Owner of Module.Owner loop
               Module_Data_Node := Create_Element(Doc => Save_Data, Tag_Name => "owner");
               Module_Data_Node := Append_Child(Data_Node, Module_Data_Node);
               Save_Number(Owner, "value", Module_Data_Node);
            end loop Save_Module_Owners_Loop;
            if Module.Upgrade_Progress > 0 then
               Save_Number
                 (Module.Upgrade_Progress, "upgradeprogress", Data_Node);
            end if;
            if Module.Upgrade_Action /= NONE then
               Save_Number
                 (Ship_Upgrade'Pos(Module.Upgrade_Action), "upgradeaction",
                  Data_Node);
            end if;
            case Module.M_Type is
               when WORKSHOP =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Set_Attribute
                    (Module_Data_Node, "value",
                     To_String(Module.Crafting_Index));
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Crafting_Time, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number
                    (Module.Crafting_Amount, "value", Module_Data_Node);
               when TRAINING_ROOM =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number
                    (Natural(Module.Trained_Skill), "value", Module_Data_Node);
               when MEDICAL_ROOM | COCKPIT | ARMOR | ANY | CARGO_ROOM =>
                  null;
               when ENGINE =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Fuel_Usage, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Power, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  if Module.Disabled then
                     Set_Attribute(Module_Data_Node, "value", "1");
                  else
                     Set_Attribute(Module_Data_Node, "value", "0");
                  end if;
               when CABIN =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Cleanliness, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Quality, "value", Module_Data_Node);
               when TURRET =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Gun_Index, "value", Module_Data_Node);
               when GUN =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Ammo_Index, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Damage, "value", Module_Data_Node);
               when HULL =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number
                    (Module.Installed_Modules, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Max_Modules, "value", Module_Data_Node);
               when BATTERING_RAM =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Damage2, "value", Module_Data_Node);
               when HARPOON_GUN =>
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Harpoon_Index, "value", Module_Data_Node);
                  Module_Data_Node := Create_Element(Save_Data, "data");
                  Module_Data_Node :=
                    Append_Child(Data_Node, Module_Data_Node);
                  Save_Number(Module.Duration, "value", Module_Data_Node);
            end case;
         end loop Save_Modules_Loop;
      end Save_Modules_Block;
      Save_Cargo_Loop :
      for Item of Player_Ship.Cargo loop
         Data_Node := Create_Element(Save_Data, "cargo");
         Data_Node := Append_Child(Category_Node, Data_Node);
         Set_Attribute(Data_Node, "index", To_String(Item.Proto_Index));
         Save_Number(Item.Amount, "amount", Data_Node);
         if Item.Name /= Null_Bounded_String then
            Set_Attribute(Data_Node, "name", To_String(Item.Name));
         end if;
         Save_Number(Item.Durability, "durability", Data_Node);
         if Item.Price > 0 then
            Save_Number(Item.Price, "price", Data_Node);
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
            Data_Node := Create_Element(Save_Data, "member");
            Data_Node := Append_Child(Category_Node, Data_Node);
            Set_Attribute(Data_Node, "name", To_String(Member.Name));
            Set_Attribute(Data_Node, "gender", Member.Gender & "");
            Set_Attribute(Data_Node, "faction", To_String(Member.Faction));
            AttributesValues :=
              (Member.Health, Member.Tired, Member.Hunger, Member.Thirst,
               Crew_Orders'Pos(Member.Order),
               Crew_Orders'Pos(Member.Previous_Order), Member.Order_Time,
               Member.Payment(1), Member.Payment(2), Member.Contract_Length,
               Member.Morale(1), Member.Morale(2), Member.Loyalty,
               Member.Home_Base);
            Save_Characteristics_Loop :
            for I in AttributesNames'Range loop
               Save_Number
                 (AttributesValues(I), To_String(AttributesNames(I)),
                  Data_Node);
            end loop Save_Characteristics_Loop;
            Save_Skills_Loop :
            for Skill of Member.Skills loop
               StatNode := Create_Element(Save_Data, "skill");
               StatNode := Append_Child(Data_Node, StatNode);
               Save_Number(Natural(Skill.Index), "index", StatNode);
               Save_Number(Skill.Level, "level", StatNode);
               if Skill.Experience > 0 then
                  Save_Number(Skill.Experience, "experience", StatNode);
               end if;
            end loop Save_Skills_Loop;
            Save_Priorities_Loop :
            for J in Member.Orders'Range loop
               StatNode := Create_Element(Save_Data, "priority");
               StatNode := Append_Child(Data_Node, StatNode);
               Save_Number(Member.Orders(J), "value", StatNode);
            end loop Save_Priorities_Loop;
            Save_Attributes_Loop :
            for Attribute of Member.Attributes loop
               StatNode := Create_Element(Save_Data, "attribute");
               StatNode := Append_Child(Data_Node, StatNode);
               Save_Number(Attribute.Level, "level", StatNode);
               if Attribute.Experience > 0 then
                  Save_Number(Attribute.Experience, "experience", StatNode);
               end if;
            end loop Save_Attributes_Loop;
            Save_Inventory_Loop :
            for Item of Member.Inventory loop
               StatNode := Create_Element(Save_Data, "item");
               StatNode := Append_Child(Data_Node, StatNode);
               Set_Attribute(StatNode, "index", To_String(Item.Proto_Index));
               Save_Number(Item.Amount, "amount", StatNode);
               if Item.Name /= Null_Bounded_String then
                  Set_Attribute(StatNode, "name", To_String(Item.Name));
               end if;
               Save_Number(Item.Durability, "durability", StatNode);
               if Item.Price > 0 then
                  Save_Number(Item.Price, "price", StatNode);
               end if;
            end loop Save_Inventory_Loop;
            Save_Equipment_Loop :
            for I in Member.Equipment'Range loop
               StatNode := Create_Element(Save_Data, "equipment");
               StatNode := Append_Child(Data_Node, StatNode);
               Save_Number(Member.Equipment(I), "index", StatNode);
            end loop Save_Equipment_Loop;
         end loop Save_Crew_Loop;
      end;
   end Save_Player_Ship;

   procedure Load_Player_Ship(Save_Data: Document) is
      use Tiny_String;

      ShipNode, ChildNodes: Node_List;
      LoadNode, ChildNode: Node;
   begin
      ShipNode :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "playership");
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
      Inventory_Container.Clear(Container => Player_Ship.Cargo);
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
                    .M_Type is -- backward compatybility
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
                  case Modules_List(ProtoIndex).M_Type is
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
                              Trained_Skill =>
                                Skills_Amount_Range(TrainedSkill)));
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
               Name: Bounded_String;
               Durability, Price: Natural;
               ProtoIndex: Bounded_String;
            begin
               ProtoIndex :=
                 To_Bounded_String(Get_Attribute(ChildNode, "index"));
               Amount := Positive'Value(Get_Attribute(ChildNode, "amount"));
               Name := To_Bounded_String(Get_Attribute(ChildNode, "name"));
               Durability :=
                 Natural'Value(Get_Attribute(ChildNode, "durability"));
               Price :=
                 (if Get_Attribute(ChildNode, "price")'Length > 0 then
                    Natural'Value(Get_Attribute(ChildNode, "price"))
                  else 0);
               Inventory_Container.Append
                 (Container => Player_Ship.Cargo,
                  New_Item =>
                    (Proto_Index => ProtoIndex, Amount => Amount, Name => Name,
                     Durability => Durability, Price => Price));
            end;
         elsif Node_Name(ChildNode) = "member" then
            declare
               MemberData: Node_List;
               ItemName: Bounded_String;
               Name, FactionIndex, ItemIndex: Tiny_String.Bounded_String;
               Gender: String(1 .. 1);
               Health, Tired, Hunger, Thirst, Index, Level, Experience,
               Loyalty, Price: Natural;
               Skills: Skills_Container.Vector (Capacity => Skills_Amount);
               Attributes: Mob_Attributes
                 (1 ..
                      Positive
                        (AttributesData_Container.Length
                           (Container => Attributes_List)));
               Order, PreviousOrder: Crew_Orders;
               Orders: Natural_Array(1 .. 12);
               Inventory: Inventory_Container.Vector (Capacity => 32);
               Equipment: Equipment_Array;
               OrderTime, ContractLength: Integer;
               Amount, Durability, EquipmentIndex, PriorityIndex,
               HomeBase: Positive;
               Payment, Morale: Attributes_Array;
               MemberNode: Node;
               Attribute_Index: Positive := 1;
            begin
               Skills_Container.Clear(Container => Skills);
               Attributes := (others => <>);
               Inventory_Container.Clear(Container => Inventory);
               Name := To_Bounded_String(Get_Attribute(ChildNode, "name"));
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
                     Skills_Container.Append
                       (Container => Skills,
                        New_Item =>
                          (Skills_Amount_Range(Index), Level, Experience));
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
                     Attributes(Attribute_Index) := (Level, Experience);
                     Attribute_Index := Attribute_Index + 1;
                  elsif Node_Name(MemberNode) = "item" then
                     ItemIndex :=
                       To_Bounded_String(Get_Attribute(MemberNode, "index"));
                     Amount :=
                       Integer'Value(Get_Attribute(MemberNode, "amount"));
                     ItemName :=
                       To_Bounded_String(Get_Attribute(MemberNode, "name"));
                     Durability :=
                       Integer'Value(Get_Attribute(MemberNode, "durability"));
                     Price :=
                       (if Get_Attribute(MemberNode, "price")'Length > 0 then
                          Integer'Value(Get_Attribute(MemberNode, "price"))
                        else 0);
                     Inventory_Container.Append
                       (Container => Inventory,
                        New_Item =>
                          (Proto_Index => ItemIndex, Amount => Amount,
                           Name => ItemName, Durability => Durability,
                           Price => Price));
                  elsif Node_Name(MemberNode) = "equipment" then
                     Equipment(Equipment_Locations'Val(EquipmentIndex - 1)) :=
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
                    To_Bounded_String(Get_Attribute(ChildNode, "faction"))
                  else Sky_Bases(HomeBase).Owner);
               Player_Ship.Crew.Append
                 (New_Item =>
                    (Amount_Of_Attributes => Attributes_Amount,
                     Amount_Of_Skills => Skills_Amount, Name => Name,
                     Gender => Gender(1), Health => Health, Tired => Tired,
                     Skills => Skills, Hunger => Hunger, Thirst => Thirst,
                     Order => Order, Previous_Order => PreviousOrder,
                     Order_Time => OrderTime, Orders => Orders,
                     Attributes => Attributes, Inventory => Inventory,
                     Equipment => Equipment, Payment => Payment,
                     Contract_Length => ContractLength, Morale => Morale,
                     Loyalty => Loyalty, Home_Base => HomeBase,
                     Faction => FactionIndex));
            end;
         end if;
      end loop Load_Ship_Loop;
   end Load_Player_Ship;

end Ships.SaveLoad;
