--    Copyright 2017-2023 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Bases; use Bases;
with ShipModules; use ShipModules;

package body Ships.SaveLoad is

   procedure Load_Player_Ship(Save_Data: Document) is
      use Tiny_String;

      Ship_Node, Ship_Child_Nodes: Node_List;
      Load_Node, Child_Node: Node;
   begin
      Ship_Node :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "playership");
      Load_Node := Item(List => Ship_Node, Index => 0);
      Player_Ship.Name :=
        To_Bounded_String
          (Source => Get_Attribute(Elem => Load_Node, Name => "name"));
      Player_Ship.Sky_X :=
        Integer'Value(Get_Attribute(Elem => Load_Node, Name => "x"));
      Player_Ship.Sky_Y :=
        Integer'Value(Get_Attribute(Elem => Load_Node, Name => "y"));
      Player_Ship.Speed :=
        Ship_Speed'Val
          (Integer'Value(Get_Attribute(Elem => Load_Node, Name => "speed")));
      Player_Ship.Upgrade_Module :=
        Integer'Value
          (Get_Attribute(Elem => Load_Node, Name => "upgrademodule"));
      Player_Ship.Destination_X :=
        Integer'Value
          (Get_Attribute(Elem => Load_Node, Name => "destinationx"));
      Player_Ship.Destination_Y :=
        Integer'Value
          (Get_Attribute(Elem => Load_Node, Name => "destinationy"));
      Player_Ship.Repair_Module :=
        Integer'Value
          (Get_Attribute(Elem => Load_Node, Name => "repairpriority"));
      Player_Ship.Home_Base :=
        Integer'Value(Get_Attribute(Elem => Load_Node, Name => "homebase"));
      Player_Ship.Modules.Clear;
      Inventory_Container.Clear(Container => Player_Ship.Cargo);
      Player_Ship.Crew.Clear;
      Ship_Child_Nodes := Child_Nodes(N => Load_Node);
      Load_Ship_Loop :
      for I in 0 .. Length(List => Ship_Child_Nodes) - 1 loop
         Child_Node := Item(List => Ship_Child_Nodes, Index => I);
         if Node_Name(N => Child_Node) = "module" then
            Load_Modules_Block :
            declare
               --## rule off IMPROPER_INITIALIZATION
               Data: Data_Array;
               Ship_Module_Data: Node_List;
               Owners: Natural_Container.Vector;
               --## rule on IMPROPER_INITIALIZATION
               Name: Bounded_String;
               Data_Index: Positive := 1;
               Proto_Index: Positive;
               Weight: Natural;
               Ship_Durability, Max_Durability, Upgrade_Progress: Integer := 0;
               Upgrade_Action: Ship_Upgrade := NONE;
               Module_Node: Node;
               M_Type: Module_Type_2 := ANY;
            begin
               Name :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "name"));
               Proto_Index :=
                 Positive'Value
                   (Get_Attribute(Elem => Child_Node, Name => "index"));
               Weight :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "weight"));
               if Get_Attribute(Elem => Child_Node, Name => "owner") /= "" then
                  --## rule off IMPROPER_INITIALIZATION
                  Owners.Append
                    (New_Item =>
                       Natural'Value
                         (Get_Attribute(Elem => Child_Node, Name => "owner")));
                  --## rule on IMPROPER_INITIALIZATION
               else
                  Ship_Module_Data := Child_Nodes(N => Child_Node);
                  Load_Owners_Loop :
                  for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                     Module_Node := Item(List => Ship_Module_Data, Index => K);
                     if Node_Name(N => Module_Node) = "owner" then
                        Owners.Append
                          (New_Item =>
                             Integer'Value
                               (Get_Attribute
                                  (Elem => Module_Node, Name => "value")));
                     end if;
                  end loop Load_Owners_Loop;
               end if;
               Ship_Durability :=
                 Integer'Value
                   (Get_Attribute(Elem => Child_Node, Name => "durability"));
               Max_Durability :=
                 Integer'Value
                   (Get_Attribute
                      (Elem => Child_Node, Name => "maxdurability"));
               if Get_Attribute(Elem => Child_Node, Name => "upgradeaction") /=
                 "" then
                  Upgrade_Action :=
                    Ship_Upgrade'Val
                      (Integer'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "upgradeaction")));
               end if;
               if Get_Attribute
                   (Elem => Child_Node, Name => "upgradeprogress") /=
                 "" then
                  Upgrade_Progress :=
                    Integer'Value
                      (Get_Attribute
                         (Elem => Child_Node, Name => "upgradeprogress"));
               end if;
               if Get_Attribute(Elem => Child_Node, Name => "mtype") /= "" then
                  case Get_Module(Index => Proto_Index)
                    .M_Type is -- backward compatybility
                     when MEDICAL_ROOM =>
                        M_Type := MEDICAL_ROOM;
                     when TRAINING_ROOM =>
                        M_Type := TRAINING_ROOM;
                     when ENGINE =>
                        M_Type := ENGINE;
                     when CABIN =>
                        M_Type := CABIN;
                     when COCKPIT =>
                        M_Type := COCKPIT;
                     when TURRET =>
                        M_Type := TURRET;
                     when GUN =>
                        M_Type := GUN;
                     when CARGO =>
                        M_Type := CARGO_ROOM;
                     when HULL =>
                        M_Type := HULL;
                     when ARMOR =>
                        M_Type := ARMOR;
                     when BATTERING_RAM =>
                        M_Type := BATTERING_RAM;
                     when HARPOON_GUN =>
                        M_Type := HARPOON_GUN;
                     when others =>
                        M_Type :=
                          Module_Type_2'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "mtype"));
                  end case;
               else
                  case Get_Module(Index => Proto_Index).M_Type is
                     when ALCHEMY_LAB .. GREENHOUSE =>
                        M_Type := WORKSHOP;
                     when MEDICAL_ROOM =>
                        M_Type := MEDICAL_ROOM;
                     when TRAINING_ROOM =>
                        M_Type := TRAINING_ROOM;
                     when ENGINE =>
                        M_Type := ENGINE;
                     when CABIN =>
                        M_Type := CABIN;
                     when COCKPIT =>
                        M_Type := COCKPIT;
                     when TURRET =>
                        M_Type := TURRET;
                     when GUN =>
                        M_Type := GUN;
                     when CARGO =>
                        M_Type := CARGO_ROOM;
                     when HULL =>
                        M_Type := HULL;
                     when ARMOR =>
                        M_Type := ARMOR;
                     when BATTERING_RAM =>
                        M_Type := BATTERING_RAM;
                     when HARPOON_GUN =>
                        M_Type := HARPOON_GUN;
                     when others =>
                        M_Type := ANY;
                  end case;
               end if;
               case M_Type is
                  when ANY =>
                     Data := (others => 0);
                     Ship_Module_Data := Child_Nodes(N => Child_Node);
                     Data_Index := 1;
                     Load_Module_Data_Loop :
                     for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                        Module_Node :=
                          Item(List => Ship_Module_Data, Index => K);
                        if Node_Name(N => Module_Node) = "data" then
                           Data(Data_Index) :=
                             Integer'Value
                               (Get_Attribute
                                  (Elem => Module_Node, Name => "value"));
                           Data_Index := Data_Index + 1;
                        end if;
                     end loop Load_Module_Data_Loop;
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => ANY, Name => Name,
                           Proto_Index => Proto_Index, Weight => Weight,
                           Durability => Ship_Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action, Data => Data));
                  when ENGINE =>
                     Load_Engine_Block :
                     declare
                        Fuel_Usage, Power: Positive;
                        Disabled: Boolean;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Engine_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Fuel_Usage :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 2 =>
                                    Power :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 3 =>
                                    if Get_Attribute
                                        (Elem => Module_Node,
                                         Name => "value") =
                                      "0" then
                                       Disabled := False;
                                    else
                                       Disabled := True;
                                    end if;
                                 when others =>
                                    null;
                              end case;
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Engine_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => ENGINE, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Fuel_Usage => Fuel_Usage, Power => Power,
                              Disabled => Disabled));
                     end Load_Engine_Block;
                  when CABIN =>
                     Load_Cabin_Block :
                     declare
                        Cleanliness, Quality: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Cabin_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Cleanliness :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 2 =>
                                    Quality :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when others =>
                                    null;
                              end case;
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Cabin_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => CABIN, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Cleanliness => Cleanliness, Quality => Quality));
                     end Load_Cabin_Block;
                  when COCKPIT =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => COCKPIT, Name => Name,
                           Proto_Index => Proto_Index, Weight => Weight,
                           Durability => Ship_Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when WORKSHOP =>
                     Load_Workshop_Block :
                     declare
                        Crafting_Index: Bounded_String;
                        Crafting_Time, Crafting_Amount: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Workshop_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Crafting_Index :=
                                      To_Bounded_String
                                        (Source =>
                                           Get_Attribute
                                             (Elem => Module_Node,
                                              Name => "value"));
                                    if Crafting_Index =
                                      To_Bounded_String(Source => "0") then
                                       Crafting_Index := Null_Bounded_String;
                                    end if;
                                 when 2 =>
                                    Crafting_Time :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 3 =>
                                    Crafting_Amount :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when others =>
                                    null;
                              end case;
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Workshop_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => WORKSHOP, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Crafting_Index => Crafting_Index,
                              Crafting_Time => Crafting_Time,
                              Crafting_Amount => Crafting_Amount));
                     end Load_Workshop_Block;
                  when MEDICAL_ROOM =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => MEDICAL_ROOM, Name => Name,
                           Proto_Index => Proto_Index, Weight => Weight,
                           Durability => Ship_Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when TRAINING_ROOM =>
                     Load_Training_Room_Block :
                     declare
                        Trained_Skill: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Training_Room_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" and
                             Data_Index = 1 then
                              Trained_Skill :=
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Module_Node, Name => "value"));
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Training_Room_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => TRAINING_ROOM, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Trained_Skill => Count_Type(Trained_Skill)));
                     end Load_Training_Room_Block;
                  when TURRET =>
                     Load_Turret_Block :
                     declare
                        Gun_Index: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Turret_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" and
                             Data_Index = 1 then
                              Gun_Index :=
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Module_Node, Name => "value"));
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Turret_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => TURRET, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Gun_Index => Gun_Index));
                     end Load_Turret_Block;
                  when GUN =>
                     Load_Gun_Block :
                     declare
                        Damage, Ammo_Index: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Gun_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Ammo_Index :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 2 =>
                                    Damage :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when others =>
                                    null;
                              end case;
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Gun_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => GUN, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Damage => Damage, Ammo_Index => Ammo_Index));
                     end Load_Gun_Block;
                  when CARGO_ROOM =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => CARGO_ROOM, Name => Name,
                           Proto_Index => Proto_Index, Weight => Weight,
                           Durability => Ship_Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when HULL =>
                     Load_Hull_Block :
                     declare
                        Installed_Modules, Max_Modules: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Hull_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Installed_Modules :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 2 =>
                                    Max_Modules :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when others =>
                                    null;
                              end case;
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Hull_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => HULL, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Installed_Modules => Installed_Modules,
                              Max_Modules => Max_Modules));
                     end Load_Hull_Block;
                  when ARMOR =>
                     Player_Ship.Modules.Append
                       (New_Item =>
                          (M_Type => ARMOR, Name => Name,
                           Proto_Index => Proto_Index, Weight => Weight,
                           Durability => Ship_Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when BATTERING_RAM =>
                     Load_Battering_Ram_Block :
                     declare
                        Damage: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Battering_Ram_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" and
                             Data_Index = 1 then
                              Damage :=
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Module_Node, Name => "value"));
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Battering_Ram_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => BATTERING_RAM, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Damage2 => Damage, Cooling_Down => False));
                     end Load_Battering_Ram_Block;
                  when HARPOON_GUN =>
                     Load_Harpoon_Gun_Block :
                     declare
                        Duration, Harpoon_Index: Natural;
                     begin
                        Ship_Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Harpoon_Gun_Data_Loop :
                        for K in 0 .. Length(List => Ship_Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Ship_Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Harpoon_Index :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when 2 =>
                                    Duration :=
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Module_Node,
                                            Name => "value"));
                                 when others =>
                                    null;
                              end case;
                              Data_Index := Data_Index + 1;
                           end if;
                        end loop Load_Harpoon_Gun_Data_Loop;
                        Player_Ship.Modules.Append
                          (New_Item =>
                             (M_Type => HARPOON_GUN, Name => Name,
                              Proto_Index => Proto_Index, Weight => Weight,
                              Durability => Ship_Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Duration => Duration,
                              Harpoon_Index => Harpoon_Index));
                     end Load_Harpoon_Gun_Block;
               end case;
            end Load_Modules_Block;
         elsif Node_Name(N => Child_Node) = "cargo" then
            Load_Cargo_Block :
            declare
               Amount: Positive;
               Name: Bounded_String;
               Durability, Price: Natural;
               Proto_Index: Natural;
            begin
               Proto_Index :=
                 Positive'Value
                   (Get_Attribute(Elem => Child_Node, Name => "index"));
               Amount :=
                 Positive'Value
                   (Get_Attribute(Elem => Child_Node, Name => "amount"));
               Name :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "name"));
               Durability :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "durability"));
               Price :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "price")'Length >
                    0
                  then
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "price"))
                  else 0);
               Inventory_Container.Append
                 (Container => Player_Ship.Cargo,
                  New_Item =>
                    (Proto_Index => Proto_Index, Amount => Amount,
                     Name => Name, Durability => Durability, Price => Price));
            end Load_Cargo_Block;
         elsif Node_Name(N => Child_Node) = "member" then
            Load_Crew_Block :
            declare
               Member_Data: Node_List;
               Item_Name: Bounded_String;
               Name, Faction_Index: Tiny_String.Bounded_String;
               Item_Index: Natural;
               Gender: String(1 .. 1);
               Health, Tired, Hunger, Thirst, Index, Level, Experience,
               Loyalty, Price: Natural;
               Skills: Skills_Container.Vector (Capacity => Skills_Amount);
               Attributes: Mob_Attributes
                 (1 ..
                      Positive
                        (AttributesData_Container.Length
                           (Container => Attributes_List)));
               Order, Previous_Order: Crew_Orders;
               Orders: Natural_Array(1 .. 12);
               Inventory: Inventory_Container.Vector (Capacity => 32);
               Equipment: Equipment_Array;
               Order_Time, Contract_Length: Integer;
               Amount, Durability, Equipment_Index, Priority_Index,
               Home_Base: Positive;
               Payment, Morale: Attributes_Array;
               Member_Node: Node;
               Attribute_Index: Positive := 1;
            begin
               Skills_Container.Clear(Container => Skills);
               Attributes := (others => <>);
               Inventory_Container.Clear(Container => Inventory);
               Name :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "name"));
               Gender := Get_Attribute(Elem => Child_Node, Name => "gender");
               Health :=
                 Integer'Value
                   (Get_Attribute(Elem => Child_Node, Name => "health"));
               Tired :=
                 Integer'Value
                   (Get_Attribute(Elem => Child_Node, Name => "tired"));
               Hunger :=
                 Integer'Value
                   (Get_Attribute(Elem => Child_Node, Name => "hunger"));
               Thirst :=
                 Integer'Value
                   (Get_Attribute(Elem => Child_Node, Name => "thirst"));
               Order :=
                 Crew_Orders'Val
                   (Integer'Value
                      (Get_Attribute(Elem => Child_Node, Name => "order")));
               Previous_Order :=
                 Crew_Orders'Val
                   (Integer'Value
                      (Get_Attribute
                         (Elem => Child_Node, Name => "previousorder")));
               Orders := (others => 0);
               Equipment := (others => 0);
               Order_Time :=
                 Integer'Value
                   (Get_Attribute(Elem => Child_Node, Name => "ordertime"));
               Equipment_Index := 1;
               Member_Data := Child_Nodes(N => Child_Node);
               Priority_Index := 1;
               Payment(1) :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "dailypay"));
               Payment(2) :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "tradepay"));
               Contract_Length :=
                 Integer'Value
                   (Get_Attribute
                      (Elem => Child_Node, Name => "contractlength"));
               Morale(1) :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "moralelevel"));
               Morale(2) :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "moralepoints"));
               Loyalty :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "loyalty"));
               Load_Crew_Loop :
               for K in 0 .. Length(List => Member_Data) - 1 loop
                  Member_Node := Item(List => Member_Data, Index => K);
                  if Node_Name(N => Member_Node) = "skill" then
                     Index :=
                       Integer'Value
                         (Get_Attribute(Elem => Member_Node, Name => "index"));
                     Level :=
                       Integer'Value
                         (Get_Attribute(Elem => Member_Node, Name => "level"));
                     Experience :=
                       (if
                          Get_Attribute
                            (Elem => Member_Node, Name => "experience") /=
                          ""
                        then
                          Integer'Value
                            (Get_Attribute
                               (Elem => Member_Node, Name => "experience"))
                        else 0);
                     Skills_Container.Append
                       (Container => Skills,
                        New_Item =>
                          (Index => Skills_Amount_Range(Index), Level => Level,
                           Experience => Experience));
                  elsif Node_Name(N => Member_Node) = "priority" then
                     Orders(Priority_Index) :=
                       Integer'Value
                         (Get_Attribute(Elem => Member_Node, Name => "value"));
                     Priority_Index := Priority_Index + 1;
                  elsif Node_Name(N => Member_Node) = "attribute" then
                     Level :=
                       Integer'Value
                         (Get_Attribute(Elem => Member_Node, Name => "level"));
                     Experience :=
                       (if
                          Get_Attribute
                            (Elem => Member_Node, Name => "experience") /=
                          ""
                        then
                          Integer'Value
                            (Get_Attribute
                               (Elem => Member_Node, Name => "experience"))
                        else 0);
                     Attributes(Attribute_Index) :=
                       (Level => Level, Experience => Experience);
                     Attribute_Index := Attribute_Index + 1;
                  elsif Node_Name(N => Member_Node) = "item" then
                     Item_Index :=
                       Positive'Value
                         (Get_Attribute(Elem => Member_Node, Name => "index"));
                     Amount :=
                       Integer'Value
                         (Get_Attribute
                            (Elem => Member_Node, Name => "amount"));
                     Item_Name :=
                       To_Bounded_String
                         (Source =>
                            Get_Attribute
                              (Elem => Member_Node, Name => "name"));
                     Durability :=
                       Integer'Value
                         (Get_Attribute
                            (Elem => Member_Node, Name => "durability"));
                     Price :=
                       (if
                          Get_Attribute(Elem => Member_Node, Name => "price")'
                            Length >
                          0
                        then
                          Integer'Value
                            (Get_Attribute
                               (Elem => Member_Node, Name => "price"))
                        else 0);
                     Inventory_Container.Append
                       (Container => Inventory,
                        New_Item =>
                          (Proto_Index => Item_Index, Amount => Amount,
                           Name => Item_Name, Durability => Durability,
                           Price => Price));
                  elsif Node_Name(N => Member_Node) = "equipment" then
                     Equipment(Equipment_Locations'Val(Equipment_Index - 1)) :=
                       Natural'Value
                         (Get_Attribute(Elem => Member_Node, Name => "index"));
                     Equipment_Index := Equipment_Index + 1;
                  end if;
               end loop Load_Crew_Loop;
               Home_Base :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "homebase") /= ""
                  then
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "homebase"))
                  else Player_Ship.Home_Base);
               Faction_Index :=
                 (if Get_Attribute(Elem => Child_Node, Name => "faction") /= ""
                  then
                    To_Bounded_String
                      (Source =>
                         Get_Attribute(Elem => Child_Node, Name => "faction"))
                  else Sky_Bases(Home_Base).Owner);
               Player_Ship.Crew.Append
                 (New_Item =>
                    (Amount_Of_Attributes => Attributes_Amount,
                     Amount_Of_Skills => Skills_Amount, Name => Name,
                     Gender => Gender(1), Health => Health, Tired => Tired,
                     Skills => Skills, Hunger => Hunger, Thirst => Thirst,
                     Order => Order, Previous_Order => Previous_Order,
                     Order_Time => Order_Time, Orders => Orders,
                     Attributes => Attributes, Inventory => Inventory,
                     Equipment => Equipment, Payment => Payment,
                     Contract_Length => Contract_Length, Morale => Morale,
                     Loyalty => Loyalty, Home_Base => Home_Base,
                     Faction => Faction_Index));
            end Load_Crew_Block;
         end if;
      end loop Load_Ship_Loop;
   end Load_Player_Ship;

end Ships.SaveLoad;
