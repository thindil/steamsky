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
with ShipModules; use ShipModules;
--with Maps; use Maps;
with Game.SaveLoad; use Game.SaveLoad;

package body Ships.SaveLoad is

   procedure SavePlayerShip(SaveData: Document; MainNode: DOM.Core.Element) is
      RawValue: Unbounded_String;
      CategoryNode, SubNode, DataNode: DOM.Core.Element;
   begin
      CategoryNode := Create_Element(SaveData, "playership");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      AddData("name", To_String(PlayerShip.Name), CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
      AddData("x", To_String(Trim(RawValue, Ada.Strings.Left)), CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
      AddData("y", To_String(Trim(RawValue, Ada.Strings.Left)), CategoryNode);
      RawValue :=
        To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
      AddData
        ("speed",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.UpgradeModule));
      AddData
        ("upgrademodule",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationX));
      AddData
        ("destinationx",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationY));
      AddData
        ("destinationy",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.RepairModule));
      AddData
        ("repairpriority",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      SubNode := Create_Element(SaveData, "modules");
      SubNode := Append_Child(CategoryNode, SubNode);
      declare
         ModuleDataNode: DOM.Core.Element;
      begin
         for Module of PlayerShip.Modules loop
            DataNode := Create_Element(SaveData, "module");
            DataNode := Append_Child(SubNode, DataNode);
            AddData("name", To_String(Module.Name), DataNode);
            AddData
              ("index",
               To_String(Modules_List(Module.ProtoIndex).Index),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Module.Weight));
            AddData
              ("weight",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Module.Durability));
            AddData
              ("durability",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(Module.MaxDurability));
            AddData
              ("maxdurability",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Module.Owner));
            AddData
              ("owner",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(Module.UpgradeProgress));
            AddData
              ("upgradeprogress",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(ShipUpgrade'Pos(Module.UpgradeAction)));
            AddData
              ("upgradeaction",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            ModuleDataNode := Create_Element(SaveData, "moduledata");
            ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
            for I in Module.Data'Range loop
               RawValue := To_Unbounded_String(Integer'Image(Module.Data(I)));
               AddData
                 ("data",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  ModuleDataNode);
            end loop;
         end loop;
      end;
      SubNode := Create_Element(SaveData, "cargo");
      SubNode := Append_Child(CategoryNode, SubNode);
      for Item of PlayerShip.Cargo loop
         DataNode := Create_Element(SaveData, "item");
         DataNode := Append_Child(SubNode, DataNode);
         AddData
           ("index",
            To_String(Items_List(Item.ProtoIndex).Index),
            DataNode);
         RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
         AddData
           ("amount",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            DataNode);
         if Item.Name /= Null_Unbounded_String then
            AddData("name", To_String(Item.Name), DataNode);
         end if;
         RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
         AddData
           ("durability",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            DataNode);
      end loop;
      SubNode := Create_Element(SaveData, "crew");
      SubNode := Append_Child(CategoryNode, SubNode);
      declare
         CrewDataNode, StatNode: DOM.Core.Element;
      begin
         for Member of PlayerShip.Crew loop
            DataNode := Create_Element(SaveData, "member");
            DataNode := Append_Child(SubNode, DataNode);
            AddData("name", To_String(Member.Name), DataNode);
            AddData("gender", Member.Gender & "", DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Health));
            AddData
              ("health",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Tired));
            AddData
              ("tired",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Hunger));
            AddData
              ("hunger",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Thirst));
            AddData
              ("thirst",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Crew_Orders'Pos(Member.Order)));
            AddData
              ("order",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Crew_Orders'Pos(Member.PreviousOrder)));
            AddData
              ("previousorder",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.OrderTime));
            AddData
              ("ordertime",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            CrewDataNode := Create_Element(SaveData, "skills");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for Skill of Member.Skills loop
               StatNode := Create_Element(SaveData, "skill");
               StatNode := Append_Child(CrewDataNode, StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(1)));
               AddData
                 ("index",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(2)));
               AddData
                 ("level",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(3)));
               AddData
                 ("experience",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "orderspriorities");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for J in Member.Orders'Range loop
               RawValue :=
                 To_Unbounded_String(Integer'Image(Member.Orders(J)));
               AddData
                 ("priority",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  CrewDataNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "attributes");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for Attribute of Member.Attributes loop
               StatNode := Create_Element(SaveData, "attribute");
               StatNode := Append_Child(CrewDataNode, StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Attribute(1)));
               AddData
                 ("level",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Attribute(2)));
               AddData
                 ("experience",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "inventory");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for Item of Member.Inventory loop
               StatNode := Create_Element(SaveData, "item");
               StatNode := Append_Child(CrewDataNode, StatNode);
               AddData
                 ("index",
                  To_String(Items_List(Item.ProtoIndex).Index),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
               AddData
                 ("amount",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               if Item.Name /= Null_Unbounded_String then
                  AddData("name", To_String(Item.Name), StatNode);
               end if;
               RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
               AddData
                 ("durability",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "equipment");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for I in Member.Equipment'Range loop
               RawValue :=
                 To_Unbounded_String(Integer'Image(Member.Equipment(I)));
               AddData
                 ("index",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  CrewDataNode);
            end loop;
         end loop;
      end;
      SubNode := Create_Element(SaveData, "missions");
      SubNode := Append_Child(CategoryNode, SubNode);
      if PlayerShip.Missions.Length > 0 then
         for Mission of PlayerShip.Missions loop
            DataNode := Create_Element(SaveData, "mission");
            DataNode := Append_Child(SubNode, DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Missions_Types'Pos(Mission.MType)));
            AddData
              ("type",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.Target));
            AddData
              ("target",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.Time));
            AddData
              ("time",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetX));
            AddData
              ("targetx",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetY));
            AddData
              ("targety",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.Reward));
            AddData
              ("reward",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.StartBase));
            AddData
              ("startbase",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            if Mission.Finished then
               AddData("finished", "Y", DataNode);
            else
               AddData("finished", "N", DataNode);
            end if;
         end loop;
      end if;
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.HomeBase));
      AddData
        ("homebase",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
   end SavePlayerShip;

   procedure LoadPlayerShip(SaveData: Document) is
      --BaseMissions: Mission_Container.Vector;
      ShipNode, ChildNodes: Node_List;
   begin
      ShipNode := Get_Elements_By_Tag_Name(SaveData, "playership");
      ChildNodes := Child_Nodes(Item(ShipNode, 0));
      for I in 0 .. Length(ChildNodes) - 1 loop
         if Node_Name(Item(ChildNodes, I)) = "name" then
            PlayerShip.Name :=
              To_Unbounded_String
                (Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "x" then
            PlayerShip.SkyX :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "y" then
            PlayerShip.SkyY :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "speed" then
            PlayerShip.Speed :=
              ShipSpeed'Val
                (Integer'Value(Node_Value(First_Child(Item(ChildNodes, I)))));
         elsif Node_Name(Item(ChildNodes, I)) = "upgrademodule" then
            PlayerShip.UpgradeModule :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "destinationx" then
            PlayerShip.DestinationX :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "destinationy" then
            PlayerShip.DestinationY :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "repairpriority" then
            PlayerShip.RepairModule :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         elsif Node_Name(Item(ChildNodes, I)) = "modules" then
            declare
               ModulesNodes, ModuleData, DataNode: Node_List;
               Name: Unbounded_String;
               ProtoIndex, DataIndex: Positive;
               Weight, Owner: Natural;
               Durability, MaxDurability, UpgradeProgress: Integer;
               UpgradeAction: ShipUpgrade;
               Data: Data_Array;
            begin
               ModulesNodes := Child_Nodes(Item(ChildNodes, I));
               for J in 0 .. Length(ModulesNodes) - 1 loop
                  if Node_Name(Item(ModulesNodes, J)) = "module" then
                     ModuleData := Child_Nodes(Item(ModulesNodes, J));
                     Name := Null_Unbounded_String;
                     ProtoIndex := 1;
                     Weight := 0;
                     Owner := 0;
                     Durability := 0;
                     MaxDurability := 0;
                     UpgradeAction := NONE;
                     UpgradeProgress := 0;
                     Data := (others => 0);
                     for K in 0 .. Length(ModuleData) - 1 loop
                        if Node_Name(Item(ModuleData, K)) = "name" then
                           Name :=
                             To_Unbounded_String
                               (Node_Value(First_Child(Item(ModuleData, K))));
                        elsif Node_Name(Item(ModuleData, K)) = "index" then
                           ProtoIndex :=
                             FindProtoModule
                               (To_Unbounded_String
                                  (Node_Value
                                     (First_Child(Item(ModuleData, K)))));
                        elsif Node_Name(Item(ModuleData, K)) = "weight" then
                           Weight :=
                             Natural'Value
                               (Node_Value(First_Child(Item(ModuleData, K))));
                        elsif Node_Name(Item(ModuleData, K)) =
                          "durability" then
                           Durability :=
                             Integer'Value
                               (Node_Value(First_Child(Item(ModuleData, K))));
                        elsif Node_Name(Item(ModuleData, K)) =
                          "maxdurability" then
                           MaxDurability :=
                             Integer'Value
                               (Node_Value(First_Child(Item(ModuleData, K))));
                        elsif Node_Name(Item(ModuleData, K)) = "owner" then
                           Owner :=
                             Natural'Value
                               (Node_Value(First_Child(Item(ModuleData, K))));
                        elsif Node_Name(Item(ModuleData, K)) =
                          "upgradeprogress" then
                           UpgradeProgress :=
                             Integer'Value
                               (Node_Value(First_Child(Item(ModuleData, K))));
                        elsif Node_Name(Item(ModuleData, K)) =
                          "upgradeaction" then
                           UpgradeAction :=
                             ShipUpgrade'Val
                               (Integer'Value
                                  (Node_Value
                                     (First_Child(Item(ModuleData, K)))));
                        elsif Node_Name(Item(ModuleData, K)) =
                          "moduledata" then
                           DataNode := Child_Nodes(Item(ModuleData, K));
                           DataIndex := 1;
                           for L in 0 .. Length(DataNode) - 1 loop
                              if Node_Name(Item(DataNode, L)) = "data" then
                                 Data(DataIndex) :=
                                   Integer'Value
                                     (Node_Value
                                        (First_Child(Item(DataNode, L))));
                                 DataIndex := DataIndex + 1;
                              end if;
                           end loop;
                        end if;
                     end loop;
                     PlayerShip.Modules.Append
                     (New_Item =>
                        (Name => Name,
                         ProtoIndex => ProtoIndex,
                         Weight => Weight,
                         Durability => Durability,
                         MaxDurability => MaxDurability,
                         Owner => Owner,
                         UpgradeProgress => UpgradeProgress,
                         UpgradeAction => UpgradeAction,
                         Data => Data));
                  end if;
               end loop;
            end;
         elsif Node_Name(Item(ChildNodes, I)) = "cargo" then
            declare
               CargoNodes, ItemData: Node_List;
               ProtoIndex, Amount: Positive;
               Name: Unbounded_String;
               Durability: Natural;
            begin
               CargoNodes := Child_Nodes(Item(ChildNodes, I));
               for J in 0 .. Length(CargoNodes) - 1 loop
                  if Node_Name(Item(CargoNodes, J)) = "item" then
                     ItemData := Child_Nodes(Item(CargoNodes, J));
                     ProtoIndex := 1;
                     Amount := 1;
                     Name := Null_Unbounded_String;
                     Durability := 0;
                     for K in 0 .. Length(ItemData) - 1 loop
                        if Node_Name(Item(ItemData, K)) = "index" then
                           ProtoIndex :=
                             FindProtoItem
                               (To_Unbounded_String
                                  (Node_Value
                                     (First_Child(Item(ItemData, K)))));
                        elsif Node_Name(Item(ItemData, K)) = "amount" then
                           Amount :=
                             Positive'Value
                               (Node_Value(First_Child(Item(ItemData, K))));
                        elsif Node_Name(Item(ItemData, K)) = "name" then
                           Name :=
                             To_Unbounded_String
                               (Node_Value(First_Child(Item(ItemData, K))));
                        elsif Node_Name(Item(ItemData, K)) = "durability" then
                           Durability :=
                             Natural'Value
                               (Node_Value(First_Child(Item(ItemData, K))));
                        end if;
                     end loop;
                     PlayerShip.Cargo.Append
                     (New_Item =>
                        (ProtoIndex => ProtoIndex,
                         Amount => Amount,
                         Name => Name,
                         Durability => Durability));
                  end if;
               end loop;
            end;
         elsif Node_Name(Item(ChildNodes, I)) = "crew" then
            declare
               CrewNodes, MemberData, ChildData, StatData: Node_List;
               Name, ItemName: Unbounded_String;
               Gender: Character;
               Health,
               Tired,
               Hunger,
               Thirst,
               Index,
               Level,
               Experience: Natural;
               Skills: Skills_Container.Vector;
               Attributes: Attributes_Container.Vector;
               Order, PreviousOrder: Crew_Orders;
               Orders: Orders_Array;
               Inventory: Inventory_Container.Vector;
               Equipment: Equipment_Array;
               OrderTime: Integer;
               Amount, Durability: Positive;
            begin
               CrewNodes := Child_Nodes(Item(ChildNodes, I));
               for J in 0 .. Length(CrewNodes) - 1 loop
                  if Node_Name(Item(CrewNodes, J)) = "member" then
                     MemberData := Child_Nodes(Item(CrewNodes, J));
                     Skills.Clear;
                     Attributes.Clear;
                     Inventory.Clear;
                     Name := Null_Unbounded_String;
                     Gender := 'M';
                     Health := 0;
                     Tired := 0;
                     Hunger := 0;
                     Thirst := 0;
                     Order := Rest;
                     PreviousOrder := Rest;
                     Orders := (others => 0);
                     Equipment := (others => 0);
                     OrderTime := 0;
                     for K in 0 .. Length(MemberData) - 1 loop
                        if Node_Name(Item(MemberData, K)) = "name" then
                           Name :=
                             To_Unbounded_String
                               (Node_Value(First_Child(Item(MemberData, K))));
                        elsif Node_Name(Item(MemberData, K)) = "gender" then
                           Gender :=
                             Node_Value(First_Child(Item(MemberData, K)))(1);
                        elsif Node_Name(Item(MemberData, K)) = "health" then
                           Health :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MemberData, K))));
                        elsif Node_Name(Item(MemberData, K)) = "tired" then
                           Tired :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MemberData, K))));
                        elsif Node_Name(Item(MemberData, K)) = "hunger" then
                           Hunger :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MemberData, K))));
                        elsif Node_Name(Item(MemberData, K)) = "thirst" then
                           Thirst :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MemberData, K))));
                        elsif Node_Name(Item(MemberData, K)) = "order" then
                           Order :=
                             Crew_Orders'Val
                               (Integer'Value
                                  (Node_Value
                                     (First_Child(Item(MemberData, K)))));
                        elsif Node_Name(Item(MemberData, K)) =
                          "previousorder" then
                           PreviousOrder :=
                             Crew_Orders'Val
                               (Integer'Value
                                  (Node_Value
                                     (First_Child(Item(MemberData, K)))));
                        elsif Node_Name(Item(MemberData, K)) = "ordertime" then
                           OrderTime :=
                             Integer'Value
                               (Node_Value(First_Child(Item(MemberData, K))));
                        elsif Node_Name(Item(MemberData, K)) = "skills" then
                           ChildData := Child_Nodes(Item(MemberData, K));
                           for L in 0 .. Length(ChildData) - 1 loop
                              if Node_Name(Item(ChildData, L)) = "skill" then
                                 Index := 0;
                                 Level := 0;
                                 Experience := 0;
                                 StatData := Child_Nodes(Item(ChildData, L));
                                 for M in 0 .. Length(StatData) - 1 loop
                                    if Node_Name(Item(StatData, M)) =
                                      "index" then
                                       Index :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    elsif Node_Name(Item(StatData, M)) =
                                      "level" then
                                       Level :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    elsif Node_Name(Item(StatData, M)) =
                                      "experience" then
                                       Experience :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    end if;
                                 end loop;
                                 Skills.Append
                                 (New_Item => (Index, Level, Experience));
                              end if;
                           end loop;
                        elsif Node_Name(Item(MemberData, K)) =
                          "orderspriorities" then
                           ChildData := Child_Nodes(Item(MemberData, K));
                           Index := 1;
                           for L in 0 .. Length(ChildData) - 1 loop
                              if Node_Name(Item(ChildData, L)) =
                                "priority" then
                                 Orders(Index) :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(ChildData, L))));
                                 Index := Index + 1;
                              end if;
                           end loop;
                        elsif Node_Name(Item(MemberData, K)) =
                          "attributes" then
                           ChildData := Child_Nodes(Item(MemberData, K));
                           for L in 0 .. Length(ChildData) - 1 loop
                              if Node_Name(Item(ChildData, L)) =
                                "attribute" then
                                 Level := 0;
                                 Experience := 0;
                                 StatData := Child_Nodes(Item(ChildData, L));
                                 for M in 0 .. Length(StatData) - 1 loop
                                    if Node_Name(Item(StatData, M)) =
                                      "level" then
                                       Level :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    elsif Node_Name(Item(StatData, M)) =
                                      "experience" then
                                       Experience :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    end if;
                                 end loop;
                                 Attributes.Append
                                 (New_Item => (Level, Experience));
                              end if;
                           end loop;
                        elsif Node_Name(Item(MemberData, K)) = "inventory" then
                           ChildData := Child_Nodes(Item(MemberData, K));
                           for L in 0 .. Length(ChildData) - 1 loop
                              if Node_Name(Item(ChildData, L)) = "item" then
                                 StatData := Child_Nodes(Item(ChildData, L));
                                 Index := 1;
                                 Amount := 1;
                                 ItemName := Null_Unbounded_String;
                                 Durability := 1;
                                 for M in 0 .. Length(StatData) - 1 loop
                                    if Node_Name(Item(StatData, M)) =
                                      "index" then
                                       Index :=
                                         FindProtoItem
                                           (To_Unbounded_String
                                              ((Node_Value
                                                  (First_Child
                                                     (Item(StatData, M))))));
                                    elsif Node_Name(Item(StatData, M)) =
                                      "amount" then
                                       Amount :=
                                         Positive'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    elsif Node_Name(Item(StatData, M)) =
                                      "name" then
                                       ItemName :=
                                         To_Unbounded_String
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    elsif Node_Name(Item(StatData, M)) =
                                      "durability" then
                                       Durability :=
                                         Positive'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatData, M))));
                                    end if;
                                 end loop;
                                 Inventory.Append
                                 (New_Item =>
                                    (ProtoIndex => Index,
                                     Amount => Amount,
                                     Name => ItemName,
                                     Durability => Durability));
                              end if;
                           end loop;
                        elsif Node_Name(Item(MemberData, K)) = "equipment" then
                           ChildData := Child_Nodes(Item(MemberData, K));
                           Index := 1;
                           for L in 0 .. Length(ChildData) - 1 loop
                              if Node_Name(Item(ChildData, L)) = "index" then
                                 Equipment(Index) :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(ChildData, L))));
                                 Index := Index + 1;
                              end if;
                           end loop;
                        end if;
                     end loop;
                     PlayerShip.Crew.Append
                     (New_Item =>
                        (Name => Name,
                         Gender => Gender,
                         Health => Health,
                         Tired => Tired,
                         Skills => Skills,
                         Hunger => Hunger,
                         Thirst => Thirst,
                         Order => Order,
                         PreviousOrder => PreviousOrder,
                         OrderTime => OrderTime,
                         Orders => Orders,
                         Attributes => Attributes,
                         Inventory => Inventory,
                         Equipment => Equipment));
                  end if;
               end loop;
            end;
         elsif Node_Name(Item(ChildNodes, I)) = "missions" then
            declare
               MissionsNodes, MissionData: Node_List;
               MType: Missions_Types;
               Target, TargetX, TargetY, StartBase: Natural;
               Time, Reward: Positive;
               Finished: Boolean;
            begin
               MissionsNodes := Child_Nodes(Item(ChildNodes, I));
               for K in 0 .. Length(MissionsNodes) - 1 loop
                  if Node_Name(Item(MissionsNodes, K)) = "mission" then
                     MissionData := Child_Nodes(Item(MissionsNodes, K));
                     MType := Deliver;
                     Target := 0;
                     Time := 1;
                     TargetX := 0;
                     TargetY := 0;
                     Reward := 1;
                     StartBase := 0;
                     Finished := False;
                     for L in 0 .. Length(MissionData) - 1 loop
                        if Node_Name(Item(MissionData, L)) = "type" then
                           MType :=
                             Missions_Types'Val
                               (Integer'Value
                                  (Node_Value
                                     (First_Child(Item(MissionData, L)))));
                        elsif Node_Name(Item(MissionData, L)) = "target" then
                           Target :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MissionData, L))));
                        elsif Node_Name(Item(MissionData, L)) = "time" then
                           Time :=
                             Positive'Value
                               (Node_Value(First_Child(Item(MissionData, L))));
                        elsif Node_Name(Item(MissionData, L)) = "targetx" then
                           TargetX :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MissionData, L))));
                        elsif Node_Name(Item(MissionData, L)) = "targety" then
                           TargetY :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MissionData, L))));
                        elsif Node_Name(Item(MissionData, L)) = "reward" then
                           Reward :=
                             Positive'Value
                               (Node_Value(First_Child(Item(MissionData, L))));
                        elsif Node_Name(Item(MissionData, L)) =
                          "startbase" then
                           StartBase :=
                             Natural'Value
                               (Node_Value(First_Child(Item(MissionData, L))));
                        elsif Node_Name(Item(MissionData, L)) = "finished" then
                           if Node_Value(First_Child(Item(MissionData, L))) =
                             "Y" then
                              Finished := True;
                           end if;
                        end if;
                     end loop;
                     PlayerShip.Missions.Append
                     (New_Item =>
                        (MType => MType,
                         Target => Target,
                         Time => Time,
                         TargetX => TargetX,
                         TargetY => TargetY,
                         Reward => Reward,
                         StartBase => StartBase,
                         Finished => Finished));
                  end if;
               end loop;
            end;
         elsif Node_Name(Item(ChildNodes, I)) = "homebase" then
            PlayerShip.HomeBase :=
              Integer'Value(Node_Value(First_Child(Item(ChildNodes, I))));
         end if;
      end loop;
   end LoadPlayerShip;

end Ships.SaveLoad;
