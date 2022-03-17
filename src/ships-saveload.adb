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
            Set_Attribute
              (Elem => Data_Node, Name => "name",
               Value => To_String(Source => Module.Name));
            Set_Attribute
              (Elem => Data_Node, Name => "index",
               Value => To_String(Source => Module.Proto_Index));
            Save_Number
              (Value => Module.Weight, Name => "weight", Node => Data_Node);
            Save_Number
              (Value => Module.Durability, Name => "durability",
               Node => Data_Node);
            Save_Number
              (Value => Module.Max_Durability, Name => "maxdurability",
               Node => Data_Node);
            Save_Module_Owners_Loop :
            for Owner of Module.Owner loop
               Module_Data_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "owner");
               Module_Data_Node :=
                 Append_Child(N => Data_Node, New_Child => Module_Data_Node);
               Save_Number
                 (Value => Owner, Name => "value", Node => Module_Data_Node);
            end loop Save_Module_Owners_Loop;
            if Module.Upgrade_Progress > 0 then
               Save_Number
                 (Value => Module.Upgrade_Progress, Name => "upgradeprogress",
                  Node => Data_Node);
            end if;
            if Module.Upgrade_Action /= NONE then
               Save_Number
                 (Value => Ship_Upgrade'Pos(Module.Upgrade_Action),
                  Name => "upgradeaction", Node => Data_Node);
            end if;
            case Module.M_Type is
               when WORKSHOP =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Set_Attribute
                    (Elem => Module_Data_Node, Name => "value",
                     Value => To_String(Source => Module.Crafting_Index));
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Crafting_Time, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Crafting_Amount, Name => "value",
                     Node => Module_Data_Node);
               when TRAINING_ROOM =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Natural(Module.Trained_Skill), Name => "value",
                     Node => Module_Data_Node);
               when MEDICAL_ROOM | COCKPIT | ARMOR | ANY | CARGO_ROOM =>
                  null;
               when ENGINE =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Fuel_Usage, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Power, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  if Module.Disabled then
                     Set_Attribute
                       (Elem => Module_Data_Node, Name => "value",
                        Value => "1");
                  else
                     Set_Attribute
                       (Elem => Module_Data_Node, Name => "value",
                        Value => "0");
                  end if;
               when CABIN =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Cleanliness, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Quality, Name => "value",
                     Node => Module_Data_Node);
               when TURRET =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Gun_Index, Name => "value",
                     Node => Module_Data_Node);
               when GUN =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Ammo_Index, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Damage, Name => "value",
                     Node => Module_Data_Node);
               when HULL =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Installed_Modules, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Max_Modules, Name => "value",
                     Node => Module_Data_Node);
               when BATTERING_RAM =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Damage2, Name => "value",
                     Node => Module_Data_Node);
               when HARPOON_GUN =>
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Harpoon_Index, Name => "value",
                     Node => Module_Data_Node);
                  Module_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "data");
                  Module_Data_Node :=
                    Append_Child
                      (N => Data_Node, New_Child => Module_Data_Node);
                  Save_Number
                    (Value => Module.Duration, Name => "value",
                     Node => Module_Data_Node);
            end case;
         end loop Save_Modules_Loop;
      end Save_Modules_Block;
      Save_Cargo_Loop :
      for Item of Player_Ship.Cargo loop
         Data_Node := Create_Element(Doc => Save_Data, Tag_Name => "cargo");
         Data_Node := Append_Child(N => Category_Node, New_Child => Data_Node);
         Set_Attribute
           (Elem => Data_Node, Name => "index",
            Value => To_String(Source => Item.Proto_Index));
         Save_Number
           (Value => Item.Amount, Name => "amount", Node => Data_Node);
         if Item.Name /= Null_Bounded_String then
            Set_Attribute
              (Elem => Data_Node, Name => "name",
               Value => To_String(Source => Item.Name));
         end if;
         Save_Number
           (Value => Item.Durability, Name => "durability", Node => Data_Node);
         if Item.Price > 0 then
            Save_Number
              (Value => Item.Price, Name => "price", Node => Data_Node);
         end if;
      end loop Save_Cargo_Loop;
      Save_Crew_Block :
      declare
         Stat_Node: DOM.Core.Element;
         Attributes_Names: constant array(1 .. 14) of Unbounded_String :=
           (1 => To_Unbounded_String(Source => "health"),
            2 => To_Unbounded_String(Source => "tired"),
            3 => To_Unbounded_String(Source => "hunger"),
            4 => To_Unbounded_String(Source => "thirst"),
            5 => To_Unbounded_String(Source => "order"),
            6 => To_Unbounded_String(Source => "previousorder"),
            7 => To_Unbounded_String(Source => "ordertime"),
            8 => To_Unbounded_String(Source => "dailypay"),
            9 => To_Unbounded_String(Source => "tradepay"),
            10 => To_Unbounded_String(Source => "contractlength"),
            11 => To_Unbounded_String(Source => "moralelevel"),
            12 => To_Unbounded_String(Source => "moralepoints"),
            13 => To_Unbounded_String(Source => "loyalty"),
            14 => To_Unbounded_String(Source => "homebase"));
         Attributes_Values: array(Attributes_Names'Range) of Integer;
      begin
         Save_Crew_Loop :
         for Member of Player_Ship.Crew loop
            Data_Node :=
              Create_Element(Doc => Save_Data, Tag_Name => "member");
            Data_Node :=
              Append_Child(N => Category_Node, New_Child => Data_Node);
            Set_Attribute
              (Elem => Data_Node, Name => "name",
               Value => To_String(Source => Member.Name));
            Set_Attribute
              (Elem => Data_Node, Name => "gender",
               Value => Member.Gender & "");
            Set_Attribute
              (Elem => Data_Node, Name => "faction",
               Value => To_String(Source => Member.Faction));
            Attributes_Values :=
              (1 => Member.Health, 2 => Member.Tired, 3 => Member.Hunger,
               4 => Member.Thirst, 5 => Crew_Orders'Pos(Member.Order),
               6 => Crew_Orders'Pos(Member.Previous_Order),
               7 => Member.Order_Time, 8 => Member.Payment(1),
               9 => Member.Payment(2), 10 => Member.Contract_Length,
               11 => Member.Morale(1), 12 => Member.Morale(2),
               13 => Member.Loyalty, 14 => Member.Home_Base);
            Save_Characteristics_Loop :
            for I in Attributes_Names'Range loop
               Save_Number
                 (Value => Attributes_Values(I),
                  Name => To_String(Source => Attributes_Names(I)),
                  Node => Data_Node);
            end loop Save_Characteristics_Loop;
            Save_Skills_Loop :
            for Skill of Member.Skills loop
               Stat_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "skill");
               Stat_Node :=
                 Append_Child(N => Data_Node, New_Child => Stat_Node);
               Save_Number
                 (Value => Natural(Skill.Index), Name => "index",
                  Node => Stat_Node);
               Save_Number
                 (Value => Skill.Level, Name => "level", Node => Stat_Node);
               if Skill.Experience > 0 then
                  Save_Number
                    (Value => Skill.Experience, Name => "experience",
                     Node => Stat_Node);
               end if;
            end loop Save_Skills_Loop;
            Save_Priorities_Loop :
            for J in Member.Orders'Range loop
               Stat_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "priority");
               Stat_Node :=
                 Append_Child(N => Data_Node, New_Child => Stat_Node);
               Save_Number
                 (Value => Member.Orders(J), Name => "value",
                  Node => Stat_Node);
            end loop Save_Priorities_Loop;
            Save_Attributes_Loop :
            for Attribute of Member.Attributes loop
               Stat_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "attribute");
               Stat_Node :=
                 Append_Child(N => Data_Node, New_Child => Stat_Node);
               Save_Number
                 (Value => Attribute.Level, Name => "level",
                  Node => Stat_Node);
               if Attribute.Experience > 0 then
                  Save_Number
                    (Value => Attribute.Experience, Name => "experience",
                     Node => Stat_Node);
               end if;
            end loop Save_Attributes_Loop;
            Save_Inventory_Loop :
            for Item of Member.Inventory loop
               Stat_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "item");
               Stat_Node :=
                 Append_Child(N => Data_Node, New_Child => Stat_Node);
               Set_Attribute
                 (Elem => Stat_Node, Name => "index",
                  Value => To_String(Source => Item.Proto_Index));
               Save_Number
                 (Value => Item.Amount, Name => "amount", Node => Stat_Node);
               if Item.Name /= Null_Bounded_String then
                  Set_Attribute
                    (Elem => Stat_Node, Name => "name",
                     Value => To_String(Source => Item.Name));
               end if;
               Save_Number
                 (Value => Item.Durability, Name => "durability",
                  Node => Stat_Node);
               if Item.Price > 0 then
                  Save_Number
                    (Value => Item.Price, Name => "price", Node => Stat_Node);
               end if;
            end loop Save_Inventory_Loop;
            Save_Equipment_Loop :
            for I in Member.Equipment'Range loop
               Stat_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "equipment");
               Stat_Node :=
                 Append_Child(N => Data_Node, New_Child => Stat_Node);
               Save_Number
                 (Value => Member.Equipment(I), Name => "index",
                  Node => Stat_Node);
            end loop Save_Equipment_Loop;
         end loop Save_Crew_Loop;
      end Save_Crew_Block;
   end Save_Player_Ship;

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
        To_Unbounded_String
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
               Module_Data: Node_List;
               Name, Proto_Index: Unbounded_String;
               Data_Index: Positive;
               Weight: Natural := 0;
               Durability, Max_Durability, Upgrade_Progress: Integer := 0;
               Upgrade_Action: Ship_Upgrade := NONE;
               Data: Data_Array;
               Module_Node: Node;
               M_Type: Module_Type_2;
               Owners: Natural_Container.Vector;
            begin
               Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "name"));
               Proto_Index :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "index"));
               Weight :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "weight"));
               if Get_Attribute(Elem => Child_Node, Name => "owner") /= "" then
                  Owners.Append
                    (New_Item =>
                       Natural'Value
                         (Get_Attribute(Elem => Child_Node, Name => "owner")));
               else
                  Module_Data := Child_Nodes(N => Child_Node);
                  Load_Owners_Loop :
                  for K in 0 .. Length(List => Module_Data) - 1 loop
                     Module_Node := Item(List => Module_Data, Index => K);
                     if Node_Name(N => Module_Node) = "owner" then
                        Owners.Append
                          (New_Item =>
                             Integer'Value
                               (Get_Attribute
                                  (Elem => Module_Node, Name => "value")));
                     end if;
                  end loop Load_Owners_Loop;
               end if;
               Durability :=
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
                  case Modules_List(Proto_Index)
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
                  case Modules_List(Proto_Index).M_Type is
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
                     Module_Data := Child_Nodes(N => Child_Node);
                     Data_Index := 1;
                     Load_Module_Data_Loop :
                     for K in 0 .. Length(List => Module_Data) - 1 loop
                        Module_Node := Item(List => Module_Data, Index => K);
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
                           Durability => Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action, Data => Data));
                  when ENGINE =>
                     Load_Engine_Block :
                     declare
                        Fuel_Usage, Power: Positive;
                        Disabled: Boolean;
                     begin
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Engine_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Cabin_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
                           Durability => Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when WORKSHOP =>
                     Load_Workshop_Block :
                     declare
                        Crafting_Index: Unbounded_String;
                        Crafting_Time, Crafting_Amount: Natural;
                     begin
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Workshop_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
                           if Node_Name(N => Module_Node) = "data" then
                              case Data_Index is
                                 when 1 =>
                                    Crafting_Index :=
                                      To_Unbounded_String
                                        (Source =>
                                           Get_Attribute
                                             (Elem => Module_Node,
                                              Name => "value"));
                                    if Crafting_Index =
                                      To_Unbounded_String(Source => "0") then
                                       Crafting_Index := Null_Unbounded_String;
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
                              Durability => Durability,
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
                           Durability => Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when TRAINING_ROOM =>
                     Load_Training_Room_Block :
                     declare
                        Trained_Skill: Natural;
                     begin
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Training_Room_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
                              Max_Durability => Max_Durability,
                              Owner => Owners,
                              Upgrade_Progress => Upgrade_Progress,
                              Upgrade_Action => Upgrade_Action,
                              Trained_Skill =>
                                Skills_Amount_Range(Trained_Skill)));
                     end Load_Training_Room_Block;
                  when TURRET =>
                     Load_Turret_Block :
                     declare
                        Gun_Index: Natural;
                     begin
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Turret_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Gun_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
                           Durability => Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when HULL =>
                     Load_Hull_Block :
                     declare
                        Installed_Modules, Max_Modules: Natural;
                     begin
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Hull_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
                           Durability => Durability,
                           Max_Durability => Max_Durability, Owner => Owners,
                           Upgrade_Progress => Upgrade_Progress,
                           Upgrade_Action => Upgrade_Action));
                  when BATTERING_RAM =>
                     Load_Battering_Ram_Block :
                     declare
                        Damage: Natural;
                     begin
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Battering_Ram_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
                        Module_Data := Child_Nodes(N => Child_Node);
                        Data_Index := 1;
                        Load_Harpoon_Gun_Data_Loop :
                        for K in 0 .. Length(List => Module_Data) - 1 loop
                           Module_Node :=
                             Item(List => Module_Data, Index => K);
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
                              Durability => Durability,
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
               Proto_Index: Bounded_String;
            begin
               Proto_Index :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "index"));
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
               Name, Faction_Index, Item_Index: Tiny_String.Bounded_String;
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
                       To_Bounded_String
                         (Source =>
                            Get_Attribute
                              (Elem => Member_Node, Name => "index"));
                     Amount :=
                       Integer'Value
                         (Get_Attribute
                            (Elem => Member_Node, Name => "amount"));
                     Item_Name :=
                       To_Bounded_String(Get_Attribute(Member_Node, "name"));
                     Durability :=
                       Integer'Value(Get_Attribute(Member_Node, "durability"));
                     Price :=
                       (if Get_Attribute(Member_Node, "price")'Length > 0 then
                          Integer'Value(Get_Attribute(Member_Node, "price"))
                        else 0);
                     Inventory_Container.Append
                       (Container => Inventory,
                        New_Item =>
                          (Proto_Index => Item_Index, Amount => Amount,
                           Name => Item_Name, Durability => Durability,
                           Price => Price));
                  elsif Node_Name(Member_Node) = "equipment" then
                     Equipment(Equipment_Locations'Val(Equipment_Index - 1)) :=
                       Natural'Value(Get_Attribute(Member_Node, "index"));
                     Equipment_Index := Equipment_Index + 1;
                  end if;
               end loop Load_Crew_Loop;
               Home_Base :=
                 (if Get_Attribute(Child_Node, "homebase") /= "" then
                    Natural'Value(Get_Attribute(Child_Node, "homebase"))
                  else Player_Ship.Home_Base);
               Faction_Index :=
                 (if Get_Attribute(Child_Node, "faction") /= "" then
                    To_Bounded_String(Get_Attribute(Child_Node, "faction"))
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
