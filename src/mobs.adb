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
      Temp_Skills: Skills_Container.Vector;
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
      Child_Index: Natural;
      Delete_Index: Positive;
      Mob_Index: Unbounded_String;
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
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Mob_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Mob_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Mob_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not ProtoMobs_Container.Contains
                (Container => Proto_Mobs_List, Key => Mob_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " mob '" & To_String(Source => Mob_Index) &
                 "', there is no mob with that index.";
            end if;
         elsif ProtoMobs_Container.Contains
             (Container => Proto_Mobs_List, Key => Mob_Index) then
            raise Data_Loading_Error
              with "Can't add mob '" & To_String(Source => Mob_Index) &
              "', there is already a mob with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Proto_Mobs_List(Mob_Index);
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
                    Natural
                      (SkillsData_Container.Length(Container => Skills_List)) +
                    1;
               end if;
               if Child_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) & " mob '" &
                    To_String(Source => Mob_Index) &
                    "', there no skill named '" &
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
                        Temp_Record.Skills.Append
                          (New_Item =>
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
                             " mob '" & To_String(Source => Mob_Index) &
                             " invalid range for skill '" &
                             Get_Attribute
                               (Elem => Child_Node, Name => "name") &
                             "'";
                        end if;
                        Temp_Record.Skills.Append
                          (New_Item =>
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
                     for Skill of Temp_Record.Skills loop
                        if Skill.Index = Child_Index then
                           if Get_Attribute
                               (Elem => Child_Node, Name => "level")'
                               Length /=
                             0 then
                              Skill :=
                                (Index => Child_Index,
                                 Level =>
                                   Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node, Name => "level")),
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
                                   " mob '" & To_String(Source => Mob_Index) &
                                   " invalid range for skill '" &
                                   Get_Attribute
                                     (Elem => Child_Node, Name => "name") &
                                   "'";
                              end if;
                              Skill :=
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
                           exit Update_Skill_Loop;
                        end if;
                     end loop Update_Skill_Loop;
                  when REMOVE =>
                     Remove_Skill_Loop :
                     for K in Temp_Record.Skills.Iterate loop
                        if Temp_Record.Skills(K).Index = Child_Index then
                           Delete_Index :=
                             Skills_Container.To_Index(Position => K);
                           exit Remove_Skill_Loop;
                        end if;
                     end loop Remove_Skill_Loop;
                     Temp_Record.Skills.Delete(Index => Delete_Index);
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
                       To_String(Source => Mob_Index) &
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
                    To_String(Source => Mob_Index) &
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
                             " mob '" & To_String(Source => Mob_Index) &
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
                                      " mob '" &
                                      To_String(Source => Mob_Index) &
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
                        Inventory_Index: Positive := 1;
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
                    To_Unbounded_String(Source => Get_Attribute(Elem => Child_Node, Name => "slot")) then
                     Temp_Record.Equipment(Equipment_Locations'Val(K - 1)) :=
                       Positive'Value(Get_Attribute(Elem => Child_Node, Name => "index"));
                     exit Update_Equipment_Loop;
                  end if;
               end loop Update_Equipment_Loop;
            end loop Equipment_Loop;
            if Action /= UPDATE then
               ProtoMobs_Container.Include
                 (Container => Proto_Mobs_List, Key => Mob_Index, New_Item => Temp_Record);
               Log_Message(Message => "Mob added: " & To_String(Source => Mob_Index), Message_Type => EVERYTHING);
            else
               Proto_Mobs_List(Mob_Index) := Temp_Record;
               Log_Message(Message => "Mob updated: " & To_String(Source => Mob_Index), Message_Type => EVERYTHING);
            end if;
         else
            ProtoMobs_Container.Exclude(Container => Proto_Mobs_List, Key => Mob_Index);
            Log_Message(Message => "Mob removed: " & To_String(Source => Mob_Index), Message_Type => EVERYTHING);
         end if;
      end loop Load_Mobs_Loop;
   end Load_Mobs;

   function Generate_Mob
     (Mob_Index, Faction_Index: Unbounded_String) return Member_Data is
      Mob: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      ProtoMob: constant Proto_Mob_Record := Proto_Mobs_List(Mob_Index);
      Amount: Natural;
      HighestSkillLevel, WeaponSkillLevel: Skill_Range := 1;
      SkillIndex: Skills_Container.Extended_Index;
   begin
      Mob.Faction :=
        (if Get_Random(1, 100) < 99 then Faction_Index
         else Get_Random_Faction);
      Mob.Gender := 'M';
      if not Factions_List(Mob.Faction).Flags.Contains
          (To_Unbounded_String("nogender"))
        and then Get_Random(1, 100) > 50 then
         Mob.Gender := 'F';
      end if;
      Mob.Name := Generate_Member_Name(Mob.Gender, Mob.Faction);
      Skills_Loop :
      for Skill of ProtoMob.Skills loop
         SkillIndex :=
           (if Skill.Index = Skills_Amount + 1 then
              Factions_List(Mob.Faction).Weapon_Skill
            else Skill.Index);
         if Skill.Experience = 0 then
            Mob.Skills.Append(New_Item => (SkillIndex, Skill.Level, 0));
         else
            Mob.Skills.Append
              (New_Item =>
                 (SkillIndex, Get_Random(Skill.Level, Skill.Experience), 0));
         end if;
         if SkillIndex = Factions_List(Mob.Faction).Weapon_Skill then
            WeaponSkillLevel := Mob.Skills(Mob.Skills.Last_Index).Level;
         end if;
         if Mob.Skills(Mob.Skills.Last_Index).Level > HighestSkillLevel then
            HighestSkillLevel := Mob.Skills(Mob.Skills.Last_Index).Level;
         end if;
      end loop Skills_Loop;
      Attributes_Loop :
      for Attribute in ProtoMob.Attributes'Range loop
         if ProtoMob.Attributes(Attribute).Experience = 0 then
            Mob.Attributes(Attribute) := ProtoMob.Attributes(Attribute);
         else
            Mob.Attributes(Attribute) :=
              (Get_Random
                 (ProtoMob.Attributes(Attribute).Level,
                  ProtoMob.Attributes(Attribute).Experience),
               0);
         end if;
      end loop Attributes_Loop;
      Inventory_Loop :
      for I in
        MobInventory_Container.First_Index(Container => ProtoMob.Inventory) ..
          MobInventory_Container.Last_Index
            (Container => ProtoMob.Inventory) loop
         declare
            Proto_Item: constant Mob_Inventory_Record :=
              MobInventory_Container.Element
                (Container => ProtoMob.Inventory, Index => I);
         begin
            Amount :=
              (if Proto_Item.Max_Amount > 0 then
                 Get_Random(Proto_Item.Min_Amount, Proto_Item.Max_Amount)
               else Proto_Item.Min_Amount);
            Mob.Inventory.Append
              (New_Item =>
                 (Proto_Index => Proto_Item.Proto_Index, Amount => Amount,
                  Name => Null_Unbounded_String, Durability => 100,
                  Price => 0));
         end;
      end loop Inventory_Loop;
      Mob.Equipment := ProtoMob.Equipment;
      declare
         use Tiny_String;

         ItemsList: TinyString_Container.Vector;
         ItemIndex: Bounded_String;
      begin
         Equipment_Loop :
         for I in WEAPON .. LEGS loop
            ItemsList :=
              (case I is when WEAPON => Weapons_List,
                 when SHIELD => Shields_List, when HELMET => Head_Armors_List,
                 when TORSO => Chest_Armors_List,
                 when ARMS => Arms_Armors_List, when LEGS => Legs_Armors_List);
            if Mob.Equipment(I) = 0 then
               ItemIndex := Null_Bounded_String;
               if Get_Random(1, 100) < 95 then
                  ItemIndex :=
                    Get_Random_Item
                      (ItemsList, I, HighestSkillLevel, WeaponSkillLevel,
                       Mob.Faction);
               end if;
               if ItemIndex /= Null_Bounded_String then
                  Mob.Inventory.Append
                    (New_Item =>
                       (Proto_Index => ItemIndex, Amount => 1,
                        Name => Null_Unbounded_String, Durability => 100,
                        Price => 0));
                  Mob.Equipment(I) := Mob.Inventory.Last_Index;
               end if;
            end if;
         end loop Equipment_Loop;
      end;
      Mob.Orders := ProtoMob.Priorities;
      Mob.Order := ProtoMob.Order;
      Mob.Order_Time := 15;
      Mob.Previous_Order := REST;
      Mob.Health := 100;
      Mob.Tired := 0;
      Mob.Hunger := 0;
      Mob.Thirst := 0;
      Mob.Payment := (20, 0);
      Mob.Contract_Length := -1;
      Mob.Morale := (50, 0);
      Mob.Loyalty := 100;
      Mob.Home_Base := 1;
      return Mob;
   end Generate_Mob;

   function Get_Random_Item
     (Items_Indexes: TinyString_Container.Vector;
      Equip_Index: Equipment_Locations;
      Highest_Level, Weapon_Skill_Level: Positive;
      Faction_Index: Unbounded_String) return Tiny_String.Bounded_String is
      use Tiny_String;

      ItemIndex, MaxIndex: Positive;
      NewIndexes: TinyString_Container.Vector;
      Added: Boolean;
   begin
      if Equip_Index > WEAPON then
         Equipment_Item_Loop :
         for I in Items_Indexes.First_Index .. Items_Indexes.Last_Index loop
            Added := False;
            Add_Equipment_Item_Loop :
            for J in NewIndexes.First_Index .. NewIndexes.Last_Index loop
               if Items_List(Items_Indexes(I)).Price <
                 Items_List(NewIndexes(J)).Price then
                  NewIndexes.Insert(J, Items_Indexes(I));
                  Added := True;
                  exit Add_Equipment_Item_Loop;
               end if;
            end loop Add_Equipment_Item_Loop;
            if not Added then
               NewIndexes.Append(Items_Indexes(I));
            end if;
         end loop Equipment_Item_Loop;
         MaxIndex :=
           Positive
             ((Float(NewIndexes.Last_Index) * (Float(Highest_Level) / 100.0)) +
              1.0);
         if MaxIndex > NewIndexes.Last_Index then
            MaxIndex := NewIndexes.Last_Index;
         end if;
         ItemIndex := Get_Random(NewIndexes.First_Index, MaxIndex);
      else
         Proto_Items_Loop :
         for I in Items_Indexes.First_Index .. Items_Indexes.Last_Index loop
            Added := False;
            Add_Proto_Item_Loop :
            for J in NewIndexes.First_Index .. NewIndexes.Last_Index loop
               if Items_List(Items_Indexes(I)).Price <
                 Items_List(NewIndexes(J)).Price and
                 Items_List(Items_Indexes(I)).Value(3) =
                   Factions_List(Faction_Index).Weapon_Skill then
                  NewIndexes.Insert(J, Items_Indexes(I));
                  Added := True;
                  exit Add_Proto_Item_Loop;
               end if;
            end loop Add_Proto_Item_Loop;
            if not Added and
              Items_List(Items_Indexes(I)).Value(3) =
                Factions_List(Faction_Index).Weapon_Skill then
               NewIndexes.Append(Items_Indexes(I));
            end if;
         end loop Proto_Items_Loop;
         if NewIndexes.Length = 0 then
            return Null_Bounded_String;
         end if;
         MaxIndex :=
           Positive
             ((Float(NewIndexes.Last_Index) *
               (Float(Weapon_Skill_Level) / 100.0)) +
              1.0);
         if MaxIndex > NewIndexes.Last_Index then
            MaxIndex := NewIndexes.Last_Index;
         end if;
         Get_Weapon_Loop :
         loop
            ItemIndex := Get_Random(NewIndexes.First_Index, MaxIndex);
            exit Get_Weapon_Loop when Items_List(NewIndexes(ItemIndex)).Value
                (3) =
              Factions_List(Faction_Index).Weapon_Skill;
         end loop Get_Weapon_Loop;
      end if;
      Get_Item_Index_Loop :
      for Index of Items_Indexes loop
         if Index = NewIndexes(ItemIndex) then
            return Index;
         end if;
      end loop Get_Item_Index_Loop;
      return Null_Bounded_String;
   end Get_Random_Item;

end Mobs;
