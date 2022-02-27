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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Items; use Items;
with Log; use Log;
with Utils; use Utils;

package body Mobs is

   procedure Load_Mobs(Reader: Tree_Reader) is
      use Tiny_String;

      Mobs_Data: Document;
      Nodes_List, Child_Nodes: Node_List;
      Temp_Record: Proto_Mob_Record
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Temp_Skills: Skills_Container.Vector (Capacity => Skills_Amount);
      Temp_Inventory: MobInventory_Container.Vector (Capacity => 32);
      Temp_Priorities: constant Natural_Array(1 .. 12) := (others => 0);
      Temp_Equipment: constant Equipment_Array := (others => 0);
      Orders_Names: constant array(1 .. 11) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "Piloting"),
         2 => To_Unbounded_String(Source => "Engineering"),
         3 => To_Unbounded_String(Source => "Operating guns"),
         4 => To_Unbounded_String(Source => "Repair ship"),
         5 => To_Unbounded_String(Source => "Manufacturing"),
         6 => To_Unbounded_String(Source => "Upgrading ship"),
         7 => To_Unbounded_String(Source => "Talking in bases"),
         8 => To_Unbounded_String(Source => "Healing wounded"),
         9 => To_Unbounded_String(Source => "Cleaning ship"),
         10 => To_Unbounded_String(Source => "Defend ship"),
         11 => To_Unbounded_String(Source => "Board enemy ship"));
      Equipment_Names: constant array(1 .. 7) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "Weapon"),
         2 => To_Unbounded_String(Source => "Shield"),
         3 => To_Unbounded_String(Source => "Head"),
         4 => To_Unbounded_String(Source => "Torso"),
         5 => To_Unbounded_String(Source => "Arms"),
         6 => To_Unbounded_String(Source => "Legs"),
         7 => To_Unbounded_String(Source => "Tool"));
      Action, Sub_Action: Data_Action;
      Mob_Node, Child_Node: Node;
      Child_Index: SkillsData_Container.Extended_Index;
      Delete_Index: Skills_Amount_Range;
      Mob_Index: Positive;
      Item_Index: Bounded_String;
   begin
      Mobs_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Mobs_Data, Tag_Name => "mobile");
      Load_Mobs_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Amount_Of_Attributes => Attributes_Amount,
            Amount_Of_Skills => Skills_Amount, Skills => Temp_Skills,
            Attributes => (others => <>), Order => REST,
            Priorities => Temp_Priorities, Inventory => Temp_Inventory,
            Equipment => Temp_Equipment);
         Mob_Node := Item(List => Nodes_List, Index => I);
         Mob_Index :=
           Positive'Value(Get_Attribute(Elem => Mob_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Mob_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Mob_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if Mob_Index not in
                ProtoMobs_Container.First_Index
                      (Container => Proto_Mobs_List) ..
                      ProtoMobs_Container.Last_Index
                        (Container => Proto_Mobs_List) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " mob '" & Positive'Image(Mob_Index) &
                 "', there is no mob with that index.";
            end if;
         elsif Mob_Index in
             ProtoMobs_Container.First_Index(Container => Proto_Mobs_List) ..
                   ProtoMobs_Container.Last_Index
                     (Container => Proto_Mobs_List) then
            raise Data_Loading_Error
              with "Can't add mob '" & Positive'Image(Mob_Index) &
              "', there is already a mob with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record :=
                 ProtoMobs_Container.Element
                   (Container => Proto_Mobs_List, Index => Mob_Index);
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Mob_Node, Name => "skill");
            Load_Skills_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Child_Index :=
                 Find_Skill_Index
                   (Skill_Name =>
                      Get_Attribute(Elem => Child_Node, Name => "name"));
               if Get_Attribute(Elem => Child_Node, Name => "name") =
                 "WeaponSkill" then
                  Child_Index :=
                    SkillsData_Container.Length(Container => Skills_List) + 1;
               end if;
               if Child_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) & " mob '" &
                    Positive'Image(Mob_Index) & "', there no skill named '" &
                    Get_Attribute(Elem => Child_Node, Name => "name") & "'.";
               end if;
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               case Sub_Action is
                  when ADD =>
                     if Get_Attribute(Elem => Child_Node, Name => "level")'
                         Length /=
                       0 then
                        Skills_Container.Append
                          (Container => Temp_Record.Skills,
                           New_Item =>
                             (Index => Child_Index,
                              Level =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node, Name => "level")),
                              Experience => 0));
                     else
                        if Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "minlevel")) >
                          Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "maxlevel")) then
                           raise Data_Loading_Error
                             with "Can't " &
                             To_Lower(Item => Data_Action'Image(Action)) &
                             " mob '" & Positive'Image(Mob_Index) &
                             " invalid range for skill '" &
                             Get_Attribute
                               (Elem => Child_Node, Name => "name") &
                             "'";
                        end if;
                        Skills_Container.Append
                          (Container => Temp_Record.Skills,
                           New_Item =>
                             (Index => Child_Index,
                              Level =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node, Name => "minlevel")),
                              Experience =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "maxlevel"))));
                     end if;
                  when UPDATE =>
                     Update_Skill_Loop :
                     for K in
                       Skills_Container.First_Index
                         (Container => Temp_Record.Skills) ..
                         Skills_Container.Last_Index
                           (Container => Temp_Record.Skills) loop
                        Update_Skill_Block :
                        declare
                           New_Skill: Skill_Info;
                        begin
                           if Skills_Container.Element
                               (Container => Temp_Record.Skills, Index => K)
                               .Index =
                             Child_Index then
                              if Get_Attribute
                                  (Elem => Child_Node, Name => "level")'
                                  Length /=
                                0 then
                                 New_Skill :=
                                   (Index => Child_Index,
                                    Level =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "level")),
                                    Experience => 0);
                              else
                                 if Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node,
                                         Name => "minlevel")) >
                                   Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node,
                                         Name => "maxlevel")) then
                                    raise Data_Loading_Error
                                      with "Can't " &
                                      To_Lower
                                        (Item => Data_Action'Image(Action)) &
                                      " mob '" & Positive'Image(Mob_Index) &
                                      " invalid range for skill '" &
                                      Get_Attribute
                                        (Elem => Child_Node, Name => "name") &
                                      "'";
                                 end if;
                                 New_Skill :=
                                   (Index => Child_Index,
                                    Level =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "minlevel")),
                                    Experience =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "maxlevel")));
                              end if;
                              Skills_Container.Replace_Element
                                (Container => Temp_Record.Skills, Index => K,
                                 New_Item => New_Skill);
                              exit Update_Skill_Loop;
                           end if;
                        end Update_Skill_Block;
                     end loop Update_Skill_Loop;
                  when REMOVE =>
                     Remove_Skill_Loop :
                     for K in
                       Skills_Container.First_Index
                         (Container => Temp_Record.Skills) ..
                         Skills_Container.Last_Index
                           (Container => Temp_Record.Skills) loop
                        if Skills_Container.Element
                            (Container => Temp_Record.Skills, Index => K)
                            .Index =
                          Child_Index then
                           Delete_Index := K;
                           exit Remove_Skill_Loop;
                        end if;
                     end loop Remove_Skill_Loop;
                     Skills_Container.Delete
                       (Container => Temp_Record.Skills,
                        Index => Delete_Index);
               end case;
            end loop Load_Skills_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Mob_Node, Name => "attribute");
            if Length(List => Child_Nodes) > 0 and Action = UPDATE then
               Temp_Record.Attributes := (others => <>);
            end if;
            Load_Attributes_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               if Get_Attribute(Elem => Child_Node, Name => "level") /= "" then
                  Temp_Record.Attributes(J + 1) :=
                    (Level =>
                       Integer'Value
                         (Get_Attribute(Elem => Child_Node, Name => "level")),
                     Experience => 0);
               else
                  if Integer'Value
                      (Get_Attribute(Elem => Child_Node, Name => "minlevel")) >
                    Integer'Value
                      (Get_Attribute
                         (Elem => Child_Node, Name => "maxlevel")) then
                     raise Data_Loading_Error
                       with "Can't " &
                       To_Lower(Item => Data_Action'Image(Action)) & " mob '" &
                       Positive'Image(Mob_Index) &
                       " invalid range for attribute.";
                  end if;
                  Temp_Record.Attributes(J + 1) :=
                    (Level =>
                       Integer'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "minlevel")),
                     Experience =>
                       Integer'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "maxlevel")));
               end if;
               exit Load_Attributes_Loop when J + 1 =
                 Positive
                   (AttributesData_Container.Length
                      (Container => Attributes_List));
            end loop Load_Attributes_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Mob_Node, Name => "priority");
            Load_Orders_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Set_Priorities_Loop :
               for K in Orders_Names'Range loop
                  if Orders_Names(K) =
                    To_Unbounded_String
                      (Source =>
                         Get_Attribute
                           (Elem => Child_Node, Name => "name")) then
                     Temp_Record.Priorities(K) :=
                       (if
                          Get_Attribute(Elem => Child_Node, Name => "value") =
                          "Normal"
                        then 1
                        else 2);
                     exit Set_Priorities_Loop;
                  end if;
               end loop Set_Priorities_Loop;
            end loop Load_Orders_Loop;
            if Get_Attribute(Elem => Mob_Node, Name => "order")'Length > 0 then
               Temp_Record.Order :=
                 Crew_Orders'Value
                   (Get_Attribute(Elem => Mob_Node, Name => "order"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Mob_Node, Name => "item");
            Load_Items_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Item_Index :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "index"));
               if not Objects_Container.Contains
                   (Container => Items_List, Key => Item_Index) then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) & " mob '" &
                    Positive'Image(Mob_Index) &
                    "', there is no item with index '" &
                    Get_Attribute(Elem => Child_Node, Name => "index") & "'.";
               end if;
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               case Sub_Action is
                  when ADD =>
                     if Get_Attribute(Elem => Child_Node, Name => "amount")'
                         Length /=
                       0 then
                        MobInventory_Container.Append
                          (Container => Temp_Record.Inventory,
                           New_Item =>
                             (Proto_Index => Item_Index,
                              Min_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node, Name => "amount")),
                              Max_Amount => 0));
                     else
                        if Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "minamount")) >
                          Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "maxamount")) then
                           raise Data_Loading_Error
                             with "Can't " &
                             To_Lower(Item => Data_Action'Image(Action)) &
                             " mob '" & Positive'Image(Mob_Index) &
                             " invalid range for amount of '" &
                             Get_Attribute
                               (Elem => Child_Node, Name => "index") &
                             "'.";
                        end if;
                        MobInventory_Container.Append
                          (Container => Temp_Record.Inventory,
                           New_Item =>
                             (Proto_Index => Item_Index,
                              Min_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "minamount")),
                              Max_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "maxamount"))));
                     end if;
                  when UPDATE =>
                     Update_Items_Loop :
                     for I in
                       MobInventory_Container.First_Index
                         (Container => Temp_Record.Inventory) ..
                         MobInventory_Container.Last_Index
                           (Container => Temp_Record.Inventory) loop
                        Update_Item_Block :
                        declare
                           Item: Mob_Inventory_Record :=
                             MobInventory_Container.Element
                               (Container => Temp_Record.Inventory,
                                Index => I);
                        begin
                           if Item.Proto_Index = Item_Index then
                              if Get_Attribute
                                  (Elem => Child_Node, Name => "amount")'
                                  Length /=
                                0 then
                                 Item :=
                                   (Proto_Index => Item_Index,
                                    Min_Amount =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "amount")),
                                    Max_Amount => 0);
                              else
                                 if Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node,
                                         Name => "minamount")) >
                                   Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node,
                                         Name => "maxamount")) then
                                    raise Data_Loading_Error
                                      with "Can't " &
                                      To_Lower
                                        (Item => Data_Action'Image(Action)) &
                                      " mob '" & Positive'Image(Mob_Index) &
                                      " invalid range for amount of '" &
                                      Get_Attribute
                                        (Elem => Child_Node, Name => "index") &
                                      "'.";
                                 end if;
                                 Item :=
                                   (Proto_Index => Item_Index,
                                    Min_Amount =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "minamount")),
                                    Max_Amount =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "maxamount")));
                              end if;
                              MobInventory_Container.Replace_Element
                                (Container => Temp_Record.Inventory,
                                 Index => I, New_Item => Item);
                              exit Update_Items_Loop;
                           end if;
                        end Update_Item_Block;
                     end loop Update_Items_Loop;
                  when REMOVE =>
                     Remove_Items_Block :
                     declare
                        Inventory_Index: Inventory_Amount_Range := 1;
                     begin
                        Remove_Items_Loop :
                        while Inventory_Index <=
                          MobInventory_Container.Last_Index
                            (Container => Temp_Record.Inventory) loop
                           if MobInventory_Container.Element
                               (Container => Temp_Record.Inventory,
                                Index => Inventory_Index)
                               .Proto_Index =
                             Item_Index then
                              MobInventory_Container.Delete
                                (Container => Temp_Record.Inventory,
                                 Index => Inventory_Index);
                              exit Remove_Items_Loop;
                           end if;
                           Inventory_Index := Inventory_Index + 1;
                        end loop Remove_Items_Loop;
                     end Remove_Items_Block;
               end case;
            end loop Load_Items_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Mob_Node, Name => "equipment");
            Equipment_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Update_Equipment_Loop :
               for K in Equipment_Names'Range loop
                  if Equipment_Names(K) =
                    To_Unbounded_String
                      (Source =>
                         Get_Attribute
                           (Elem => Child_Node, Name => "slot")) then
                     Temp_Record.Equipment(Equipment_Locations'Val(K - 1)) :=
                       Positive'Value
                         (Get_Attribute(Elem => Child_Node, Name => "index"));
                     exit Update_Equipment_Loop;
                  end if;
               end loop Update_Equipment_Loop;
            end loop Equipment_Loop;
            if Action /= UPDATE then
               ProtoMobs_Container.Append
                 (Container => Proto_Mobs_List, New_Item => Temp_Record);
               Log_Message
                 (Message => "Mob added: " & Positive'Image(Mob_Index),
                  Message_Type => EVERYTHING);
            else
               ProtoMobs_Container.Replace_Element
                 (Container => Proto_Mobs_List, Index => Mob_Index,
                  New_Item => Temp_Record);
               Log_Message
                 (Message => "Mob updated: " & Positive'Image(Mob_Index),
                  Message_Type => EVERYTHING);
            end if;
         else
            ProtoMobs_Container.Delete
              (Container => Proto_Mobs_List, Index => Mob_Index);
            Log_Message
              (Message => "Mob removed: " & Positive'Image(Mob_Index),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Mobs_Loop;
   end Load_Mobs;

   function Generate_Mob
     (Mob_Index: ProtoMobs_Container.Extended_Index;
      Faction_Index: Tiny_String.Bounded_String) return Member_Data is
      use Tiny_String;

      Mob: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Proto_Mob: constant Proto_Mob_Record :=
        ProtoMobs_Container.Element
          (Container => Proto_Mobs_List, Index => Mob_Index);
      Amount: Natural;
      Highest_Skill_Level, Weapon_Skill_Level: Skill_Range := 1;
      Skill_Index: Skills_Container.Extended_Index;
   begin
      Mob.Faction :=
        (if Get_Random(Min => 1, Max => 100) < 99 then Faction_Index
         else Get_Random_Faction);
      Mob.Gender := 'M';
      if not Factions_List(Mob.Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "nogender"))
        and then Get_Random(Min => 1, Max => 100) > 50 then
         Mob.Gender := 'F';
      end if;
      Mob.Name :=
        Generate_Member_Name
          (Gender => Mob.Gender, Faction_Index => Mob.Faction);
      Skills_Loop :
      for Skill of Proto_Mob.Skills loop
         Skill_Index :=
           (if Skill.Index = Skills_Amount + 1 then
              Factions_List(Mob.Faction).Weapon_Skill
            else Skill.Index);
         if Skill.Experience = 0 then
            Skills_Container.Append
              (Container => Mob.Skills,
               New_Item =>
                 (Index => Skill_Index, Level => Skill.Level,
                  Experience => 0));
         else
            Skills_Container.Append
              (Container => Mob.Skills,
               New_Item =>
                 (Index => Skill_Index,
                  Level =>
                    Get_Random(Min => Skill.Level, Max => Skill.Experience),
                  Experience => 0));
         end if;
         if Skill_Index = Factions_List(Mob.Faction).Weapon_Skill then
            Weapon_Skill_Level :=
              Skills_Container.Element
                (Container => Mob.Skills,
                 Index => Skills_Container.Last_Index(Container => Mob.Skills))
                .Level;
         end if;
         if Skills_Container.Element
             (Container => Mob.Skills,
              Index => Skills_Container.Last_Index(Container => Mob.Skills))
             .Level >
           Highest_Skill_Level then
            Highest_Skill_Level :=
              Skills_Container.Element
                (Container => Mob.Skills,
                 Index => Skills_Container.Last_Index(Container => Mob.Skills))
                .Level;
         end if;
      end loop Skills_Loop;
      Attributes_Loop :
      for Attribute in Proto_Mob.Attributes'Range loop
         if Proto_Mob.Attributes(Attribute).Experience = 0 then
            Mob.Attributes(Attribute) := Proto_Mob.Attributes(Attribute);
         else
            Mob.Attributes(Attribute) :=
              (Level =>
                 Get_Random
                   (Min => Proto_Mob.Attributes(Attribute).Level,
                    Max => Proto_Mob.Attributes(Attribute).Experience),
               Experience => 0);
         end if;
      end loop Attributes_Loop;
      Inventory_Loop :
      for I in
        MobInventory_Container.First_Index(Container => Proto_Mob.Inventory) ..
          MobInventory_Container.Last_Index
            (Container => Proto_Mob.Inventory) loop
         Fill_Inventory_Block :
         declare
            Proto_Item: constant Mob_Inventory_Record :=
              MobInventory_Container.Element
                (Container => Proto_Mob.Inventory, Index => I);
         begin
            Amount :=
              (if Proto_Item.Max_Amount > 0 then
                 Get_Random
                   (Min => Proto_Item.Min_Amount, Max => Proto_Item.Max_Amount)
               else Proto_Item.Min_Amount);
            Inventory_Container.Append(Container => Mob.Inventory
              ,New_Item =>
                 (Proto_Index => Proto_Item.Proto_Index, Amount => Amount,
                  Name => Null_Bounded_String, Durability => 100, Price => 0));
         end Fill_Inventory_Block;
      end loop Inventory_Loop;
      Mob.Equipment := Proto_Mob.Equipment;
      Set_Equipment_Block :
      declare
         Equipment_Items_List: TinyString_Container.Vector;
         Equipment_Item_Index: Bounded_String;
      begin
         Equipment_Loop :
         for I in WEAPON .. LEGS loop
            Equipment_Items_List :=
              (case I is when WEAPON => Weapons_List,
                 when SHIELD => Shields_List, when HELMET => Head_Armors_List,
                 when TORSO => Chest_Armors_List,
                 when ARMS => Arms_Armors_List, when LEGS => Legs_Armors_List);
            if Mob.Equipment(I) = 0 then
               Equipment_Item_Index := Null_Bounded_String;
               if Get_Random(Min => 1, Max => 100) < 95 then
                  Equipment_Item_Index :=
                    Get_Random_Item
                      (Items_Indexes => Equipment_Items_List, Equip_Index => I,
                       Highest_Level => Highest_Skill_Level,
                       Weapon_Skill_Level => Weapon_Skill_Level,
                       Faction_Index => Mob.Faction);
               end if;
               if Equipment_Item_Index /= Null_Bounded_String then
                  Inventory_Container.Append(Container => Mob.Inventory,
                    New_Item =>
                       (Proto_Index => Equipment_Item_Index, Amount => 1,
                        Name => Null_Bounded_String, Durability => 100,
                        Price => 0));
                  Mob.Equipment(I) := Inventory_Container.Last_Index(Container => Mob.Inventory);
               end if;
            end if;
         end loop Equipment_Loop;
      end Set_Equipment_Block;
      Mob.Orders := Proto_Mob.Priorities;
      Mob.Order := Proto_Mob.Order;
      Mob.Order_Time := 15;
      Mob.Previous_Order := REST;
      Mob.Health := 100;
      Mob.Tired := 0;
      Mob.Hunger := 0;
      Mob.Thirst := 0;
      Mob.Payment := (1 => 20, 2 => 0);
      Mob.Contract_Length := -1;
      Mob.Morale := (1 => 50, 2 => 0);
      Mob.Loyalty := 100;
      Mob.Home_Base := 1;
      return Mob;
   end Generate_Mob;

   function Get_Random_Item
     (Items_Indexes: TinyString_Container.Vector;
      Equip_Index: Equipment_Locations;
      Highest_Level, Weapon_Skill_Level: Positive;
      Faction_Index: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String is
      use Tiny_String;

      Item_Index, Max_Index: Positive;
      New_Indexes: TinyString_Container.Vector;
      Added: Boolean;
   begin
      if Equip_Index > WEAPON then
         Equipment_Item_Loop :
         for I in Items_Indexes.First_Index .. Items_Indexes.Last_Index loop
            Added := False;
            Add_Equipment_Item_Loop :
            for J in New_Indexes.First_Index .. New_Indexes.Last_Index loop
               if Items_List(Items_Indexes(I)).Price <
                 Items_List(New_Indexes(J)).Price then
                  New_Indexes.Insert
                    (Before => J, New_Item => Items_Indexes(I));
                  Added := True;
                  exit Add_Equipment_Item_Loop;
               end if;
            end loop Add_Equipment_Item_Loop;
            if not Added then
               New_Indexes.Append(New_Item => Items_Indexes(I));
            end if;
         end loop Equipment_Item_Loop;
         Max_Index :=
           Positive
             ((Float(New_Indexes.Last_Index) *
               (Float(Highest_Level) / 100.0)) +
              1.0);
         if Max_Index > New_Indexes.Last_Index then
            Max_Index := New_Indexes.Last_Index;
         end if;
         Item_Index :=
           Get_Random(Min => New_Indexes.First_Index, Max => Max_Index);
      else
         Proto_Items_Loop :
         for I in Items_Indexes.First_Index .. Items_Indexes.Last_Index loop
            Added := False;
            Add_Proto_Item_Loop :
            for J in New_Indexes.First_Index .. New_Indexes.Last_Index loop
               if Items_List(Items_Indexes(I)).Price <
                 Items_List(New_Indexes(J)).Price and
                 Skills_Amount_Range
                     (Items_List(Items_Indexes(I)).Value.Element(Index => 3)) =
                   Factions_List(Faction_Index).Weapon_Skill then
                  New_Indexes.Insert
                    (Before => J, New_Item => Items_Indexes(I));
                  Added := True;
                  exit Add_Proto_Item_Loop;
               end if;
            end loop Add_Proto_Item_Loop;
            if not Added and
              Skills_Amount_Range
                  (Items_List(Items_Indexes(I)).Value.Element(Index => 3)) =
                Factions_List(Faction_Index).Weapon_Skill then
               New_Indexes.Append(New_Item => Items_Indexes(I));
            end if;
         end loop Proto_Items_Loop;
         if New_Indexes.Length = 0 then
            return Null_Bounded_String;
         end if;
         Max_Index :=
           Positive
             ((Float(New_Indexes.Last_Index) *
               (Float(Weapon_Skill_Level) / 100.0)) +
              1.0);
         if Max_Index > New_Indexes.Last_Index then
            Max_Index := New_Indexes.Last_Index;
         end if;
         Get_Weapon_Loop :
         loop
            Item_Index :=
              Get_Random(Min => New_Indexes.First_Index, Max => Max_Index);
            exit Get_Weapon_Loop when Skills_Amount_Range
                (Items_List(New_Indexes(Item_Index)).Value.Element
                   (Index => 3)) =
              Factions_List(Faction_Index).Weapon_Skill;
         end loop Get_Weapon_Loop;
      end if;
      Get_Item_Index_Loop :
      for Index of Items_Indexes loop
         if Index = New_Indexes(Item_Index) then
            return Index;
         end if;
      end loop Get_Item_Index_Loop;
      return Null_Bounded_String;
   end Get_Random_Item;

end Mobs;
