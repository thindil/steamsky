--    Copyright 2017-2019 Bartek thindil Jasicki
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
            Set_Attribute(DataNode, "index", To_String(Module.ProtoIndex));
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
            case Module.MType is
               when WORKSHOP =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  Set_Attribute
                    (ModuleDataNode, "value", To_String(Module.CraftingIndex));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.CraftingTime));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.CraftingAmount));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when TRAINING_ROOM =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.TrainedSkill));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when MEDICAL_ROOM | COCKPIT | ARMOR | ANY | CARGO_ROOM =>
                  null;
               when ENGINE =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.FuelUsage));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue := To_Unbounded_String(Integer'Image(Module.Power));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
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
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.Cleanliness));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.Quality));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when TURRET =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.GunIndex));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when GUN =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.AmmoIndex));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.Damage));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when HULL =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String
                      (Integer'Image(Module.InstalledModules));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.MaxModules));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when BATTERING_RAM =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.Damage2));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
               when HARPOON_GUN =>
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.HarpoonIndex));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
                  ModuleDataNode := Create_Element(SaveData, "data");
                  ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
                  RawValue :=
                    To_Unbounded_String(Integer'Image(Module.Duration));
                  Set_Attribute
                    (ModuleDataNode, "value",
                     To_String(Trim(RawValue, Ada.Strings.Left)));
            end case;
         end loop;
      end;
      for Item of PlayerShip.Cargo loop
         DataNode := Create_Element(SaveData, "cargo");
         DataNode := Append_Child(CategoryNode, DataNode);
         Set_Attribute(DataNode, "index", To_String(Item.ProtoIndex));
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
         RawValue := To_Unbounded_String(Integer'Image(Item.Price));
         Set_Attribute
           (DataNode, "price", To_String(Trim(RawValue, Ada.Strings.Left)));
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
            Set_Attribute(DataNode, "faction", To_String(Member.Faction));
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
               Set_Attribute(StatNode, "index", To_String(Item.ProtoIndex));
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
               RawValue := To_Unbounded_String(Integer'Image(Item.Price));
               Set_Attribute
                 (StatNode, "price",
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
      ShipNode, ChildNodes: Node_List;
      LoadNode, ChildNode: Node;
   begin
      ShipNode :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "playership");
      LoadNode := Item(ShipNode, 0);
      PlayerShip.Name := To_Unbounded_String(Get_Attribute(LoadNode, "name"));
      PlayerShip.SkyX := Integer'Value(Get_Attribute(LoadNode, "x"));
      PlayerShip.SkyY := Integer'Value(Get_Attribute(LoadNode, "y"));
      PlayerShip.Speed :=
        ShipSpeed'Val(Integer'Value(Get_Attribute(LoadNode, "speed")));
      PlayerShip.UpgradeModule :=
        Integer'Value(Get_Attribute(LoadNode, "upgrademodule"));
      PlayerShip.DestinationX :=
        Integer'Value(Get_Attribute(LoadNode, "destinationx"));
      PlayerShip.DestinationY :=
        Integer'Value(Get_Attribute(LoadNode, "destinationy"));
      PlayerShip.RepairModule :=
        Integer'Value(Get_Attribute(LoadNode, "repairpriority"));
      PlayerShip.HomeBase :=
        Integer'Value(Get_Attribute(LoadNode, "homebase"));
      PlayerShip.Modules.Clear;
      PlayerShip.Cargo.Clear;
      PlayerShip.Crew.Clear;
      ChildNodes := Child_Nodes(LoadNode);
      for I in 0 .. Length(ChildNodes) - 1 loop
         ChildNode := Item(ChildNodes, I);
         if Node_Name(ChildNode) = "module" then
            declare
               ModuleData: Node_List;
               Name, ProtoIndex: Unbounded_String;
               DataIndex: Positive;
               Weight, Owner: Natural;
               Durability, MaxDurability, UpgradeProgress: Integer;
               UpgradeAction: ShipUpgrade;
               Data: Data_Array;
               ModuleNode: Node;
               MType: ModuleType2;
            begin
               Name := To_Unbounded_String(Get_Attribute(ChildNode, "name"));
               ProtoIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               Weight := Natural'Value(Get_Attribute(ChildNode, "weight"));
               Owner := Natural'Value(Get_Attribute(ChildNode, "owner"));
               Durability :=
                 Integer'Value(Get_Attribute(ChildNode, "durability"));
               MaxDurability :=
                 Integer'Value(Get_Attribute(ChildNode, "maxdurability"));
               UpgradeAction :=
                 ShipUpgrade'Val
                   (Integer'Value(Get_Attribute(ChildNode, "upgradeaction")));
               UpgradeProgress :=
                 Integer'Value(Get_Attribute(ChildNode, "upgradeprogress"));
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
                          ModuleType2'Value(Get_Attribute(ChildNode, "mtype"));
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
                     for K in 0 .. Length(ModuleData) - 1 loop
                        ModuleNode := Item(ModuleData, K);
                        if Node_Name(ModuleNode) = "data" then
                           Data(DataIndex) :=
                             Integer'Value(Get_Attribute(ModuleNode, "value"));
                           DataIndex := DataIndex + 1;
                        end if;
                     end loop;
                     PlayerShip.Modules.Append
                       (New_Item =>
                          (MType => ANY, Name => Name,
                           ProtoIndex => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           MaxDurability => MaxDurability, Owner => Owner,
                           UpgradeProgress => UpgradeProgress,
                           UpgradeAction => UpgradeAction, Data => Data));
                  when ENGINE =>
                     declare
                        FuelUsage, Power: Positive;
                        Disabled: Boolean;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
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
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => ENGINE, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              FuelUsage => FuelUsage, Power => Power,
                              Disabled => Disabled));
                     end;
                  when CABIN =>
                     declare
                        Cleanliness, Quality: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
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
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => CABIN, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              Cleanliness => Cleanliness, Quality => Quality));
                     end;
                  when COCKPIT =>
                     PlayerShip.Modules.Append
                       (New_Item =>
                          (MType => COCKPIT, Name => Name,
                           ProtoIndex => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           MaxDurability => MaxDurability, Owner => Owner,
                           UpgradeProgress => UpgradeProgress,
                           UpgradeAction => UpgradeAction));
                  when WORKSHOP =>
                     declare
                        CraftingIndex: Unbounded_String;
                        CraftingTime, CraftingAmount: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
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
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => WORKSHOP, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              CraftingIndex => CraftingIndex,
                              CraftingTime => CraftingTime,
                              CraftingAmount => CraftingAmount));
                     end;
                  when MEDICAL_ROOM =>
                     PlayerShip.Modules.Append
                       (New_Item =>
                          (MType => MEDICAL_ROOM, Name => Name,
                           ProtoIndex => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           MaxDurability => MaxDurability, Owner => Owner,
                           UpgradeProgress => UpgradeProgress,
                           UpgradeAction => UpgradeAction));
                  when TRAINING_ROOM =>
                     declare
                        TrainedSkill: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" and
                             DataIndex = 1 then
                              TrainedSkill :=
                                Integer'Value
                                  (Get_Attribute(ModuleNode, "value"));
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => TRAINING_ROOM, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              TrainedSkill => TrainedSkill));
                     end;
                  when TURRET =>
                     declare
                        GunIndex: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" and
                             DataIndex = 1 then
                              GunIndex :=
                                Integer'Value
                                  (Get_Attribute(ModuleNode, "value"));
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => TURRET, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              GunIndex => GunIndex));
                     end;
                  when GUN =>
                     declare
                        Damage, AmmoIndex: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
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
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => GUN, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction, Damage => Damage,
                              AmmoIndex => AmmoIndex));
                     end;
                  when CARGO_ROOM =>
                     PlayerShip.Modules.Append
                       (New_Item =>
                          (MType => CARGO_ROOM, Name => Name,
                           ProtoIndex => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           MaxDurability => MaxDurability, Owner => Owner,
                           UpgradeProgress => UpgradeProgress,
                           UpgradeAction => UpgradeAction));
                  when HULL =>
                     declare
                        InstalledModules, MaxModules: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
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
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => HULL, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              InstalledModules => InstalledModules,
                              MaxModules => MaxModules));
                     end;
                  when ARMOR =>
                     PlayerShip.Modules.Append
                       (New_Item =>
                          (MType => ARMOR, Name => Name,
                           ProtoIndex => ProtoIndex, Weight => Weight,
                           Durability => Durability,
                           MaxDurability => MaxDurability, Owner => Owner,
                           UpgradeProgress => UpgradeProgress,
                           UpgradeAction => UpgradeAction));
                  when BATTERING_RAM =>
                     declare
                        Damage: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
                        for K in 0 .. Length(ModuleData) - 1 loop
                           ModuleNode := Item(ModuleData, K);
                           if Node_Name(ModuleNode) = "data" and
                             DataIndex = 1 then
                              Damage :=
                                Integer'Value
                                  (Get_Attribute(ModuleNode, "value"));
                              DataIndex := DataIndex + 1;
                           end if;
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => BATTERING_RAM, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              Damage2 => Damage, CoolingDown => False));
                     end;
                  when HARPOON_GUN =>
                     declare
                        Duration, HarpoonIndex: Natural;
                     begin
                        ModuleData := Child_Nodes(ChildNode);
                        DataIndex := 1;
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
                        end loop;
                        PlayerShip.Modules.Append
                          (New_Item =>
                             (MType => HARPOON_GUN, Name => Name,
                              ProtoIndex => ProtoIndex, Weight => Weight,
                              Durability => Durability,
                              MaxDurability => MaxDurability, Owner => Owner,
                              UpgradeProgress => UpgradeProgress,
                              UpgradeAction => UpgradeAction,
                              Duration => Duration,
                              HarpoonIndex => HarpoonIndex));
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
               if Get_Attribute(ChildNode, "price")'Length > 0 then
                  Price := Natural'Value(Get_Attribute(ChildNode, "price"));
               else
                  Price := 0;
               end if;
               PlayerShip.Cargo.Append
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
               Orders: Orders_Array;
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
               for K in 0 .. Length(MemberData) - 1 loop
                  MemberNode := Item(MemberData, K);
                  if Node_Name(MemberNode) = "skill" then
                     Index :=
                       Integer'Value(Get_Attribute(MemberNode, "index"));
                     Level :=
                       Integer'Value(Get_Attribute(MemberNode, "level"));
                     Experience :=
                       Integer'Value(Get_Attribute(MemberNode, "experience"));
                     Skills.Append(New_Item => (Index, Level, Experience));
                  elsif Node_Name(MemberNode) = "priority" then
                     Orders(PriorityIndex) :=
                       Integer'Value(Get_Attribute(MemberNode, "value"));
                     PriorityIndex := PriorityIndex + 1;
                  elsif Node_Name(MemberNode) = "attribute" then
                     Level :=
                       Integer'Value(Get_Attribute(MemberNode, "level"));
                     Experience :=
                       Integer'Value(Get_Attribute(MemberNode, "experience"));
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
                     if Get_Attribute(MemberNode, "price")'Length > 0 then
                        Price :=
                          Integer'Value(Get_Attribute(MemberNode, "price"));
                     else
                        Price := 0;
                     end if;
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
               end loop;
               if Get_Attribute(ChildNode, "homebase") /= "" then
                  HomeBase :=
                    Natural'Value(Get_Attribute(ChildNode, "homebase"));
               else
                  HomeBase := PlayerShip.HomeBase;
               end if;
               if Get_Attribute(ChildNode, "faction") /= "" then
                  FactionIndex :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "faction"));
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
