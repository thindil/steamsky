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

   procedure LoadMobs(Reader: Tree_Reader) is
      MobsData: Document;
      NodesList, ChildNodes: Node_List;
      TempRecord: ProtoMobRecord;
      TempSkills: Skills_Container.Vector;
      TempInventory: MobInventory_Container.Vector;
      TempAttributes: Attributes_Container.Vector;
      TempPriorities: constant Natural_Array(1 .. 12) := (others => 0);
      TempEquipment: constant Equipment_Array := (others => 0);
      OrdersNames: constant array(1 .. 11) of Unbounded_String :=
        (To_Unbounded_String("Piloting"), To_Unbounded_String("Engineering"),
         To_Unbounded_String("Operating guns"),
         To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading ship"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleaning ship"),
         To_Unbounded_String("Defend ship"),
         To_Unbounded_String("Board enemy ship"));
      EquipmentNames: constant array(1 .. 7) of Unbounded_String :=
        (To_Unbounded_String("Weapon"), To_Unbounded_String("Shield"),
         To_Unbounded_String("Head"), To_Unbounded_String("Torso"),
         To_Unbounded_String("Arms"), To_Unbounded_String("Legs"),
         To_Unbounded_String("Tool"));
      Action, SubAction: DataAction;
      MobNode, ChildNode: Node;
      ChildIndex: Natural;
      DeleteIndex: Positive;
      MobIndex, ItemIndex: Unbounded_String;
   begin
      MobsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(MobsData, "mobile");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Skills => TempSkills, Attributes => TempAttributes, Order => Rest,
            Priorities => TempPriorities, Inventory => TempInventory,
            Equipment => TempEquipment);
         MobNode := Item(NodesList, I);
         MobIndex := To_Unbounded_String(Get_Attribute(MobNode, "index"));
         Action :=
           (if Get_Attribute(MobNode, "action")'Length > 0 then
              DataAction'Value(Get_Attribute(MobNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not ProtoMobs_Container.Contains(ProtoMobs_List, MobIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " mob '" & To_String(MobIndex) &
                 "', there is no mob with that index.";
            end if;
         elsif ProtoMobs_Container.Contains(ProtoMobs_List, MobIndex) then
            raise Data_Loading_Error
              with "Can't add mob '" & To_String(MobIndex) &
              "', there is already a mob with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := ProtoMobs_List(MobIndex);
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(MobNode, "skill");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               ChildIndex :=
                 FindSkillIndex
                   (To_Unbounded_String(Get_Attribute(ChildNode, "name")));
               if Get_Attribute(ChildNode, "name") = "WeaponSkill" then
                  ChildIndex := Natural(Skills_List.Length) + 1;
               end if;
               if ChildIndex = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " mob '" & To_String(MobIndex) &
                    "', there no skill named '" &
                    Get_Attribute(ChildNode, "name") & "'.";
               end if;
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    DataAction'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               case SubAction is
                  when ADD =>
                     if Get_Attribute(ChildNode, "level")'Length /= 0 then
                        TempRecord.Skills.Append
                          (New_Item =>
                             (ChildIndex,
                              Integer'Value(Get_Attribute(ChildNode, "level")),
                              0));
                     else
                        if Integer'Value
                            (Get_Attribute(ChildNode, "minlevel")) >
                          Integer'Value
                            (Get_Attribute(ChildNode, "maxlevel")) then
                           raise Data_Loading_Error
                             with "Can't " &
                             To_Lower(DataAction'Image(Action)) & " mob '" &
                             To_String(MobIndex) &
                             " invalid range for skill '" &
                             Get_Attribute(ChildNode, "name") & "'";
                        end if;
                        TempRecord.Skills.Append
                          (New_Item =>
                             (ChildIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "minlevel")),
                              Integer'Value
                                (Get_Attribute(ChildNode, "maxlevel"))));
                     end if;
                  when UPDATE =>
                     for Skill of TempRecord.Skills loop
                        if Skill(1) = ChildIndex then
                           if Get_Attribute(ChildNode, "level")'Length /=
                             0 then
                              Skill :=
                                (ChildIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "level")),
                                 0);
                           else
                              if Integer'Value
                                  (Get_Attribute(ChildNode, "minlevel")) >
                                Integer'Value
                                  (Get_Attribute(ChildNode, "maxlevel")) then
                                 raise Data_Loading_Error
                                   with "Can't " &
                                   To_Lower(DataAction'Image(Action)) &
                                   " mob '" & To_String(MobIndex) &
                                   " invalid range for skill '" &
                                   Get_Attribute(ChildNode, "name") & "'";
                              end if;
                              Skill :=
                                (ChildIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "minlevel")),
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "maxlevel")));
                           end if;
                           exit;
                        end if;
                     end loop;
                  when REMOVE =>
                     for K in TempRecord.Skills.Iterate loop
                        if TempRecord.Skills(K)(1) = ChildIndex then
                           DeleteIndex := Skills_Container.To_Index(K);
                           exit;
                        end if;
                     end loop;
                     TempRecord.Skills.Delete(Index => DeleteIndex);
               end case;
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(MobNode, "attribute");
            if Length(ChildNodes) > 0 and Action = UPDATE then
               TempRecord.Attributes.Clear;
            end if;
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               if Get_Attribute(ChildNode, "level") /= "" then
                  TempRecord.Attributes.Append
                    (New_Item =>
                       (Integer'Value(Get_Attribute(ChildNode, "level")), 0));
               else
                  if Integer'Value(Get_Attribute(ChildNode, "minlevel")) >
                    Integer'Value(Get_Attribute(ChildNode, "maxlevel")) then
                     raise Data_Loading_Error
                       with "Can't " & To_Lower(DataAction'Image(Action)) &
                       " mob '" & To_String(MobIndex) &
                       " invalid range for attribute.";
                  end if;
                  TempRecord.Attributes.Append
                    (New_Item =>
                       (Integer'Value(Get_Attribute(ChildNode, "minlevel")),
                        Integer'Value(Get_Attribute(ChildNode, "maxlevel"))));
               end if;
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(MobNode, "priority");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               for K in OrdersNames'Range loop
                  if OrdersNames(K) =
                    To_Unbounded_String(Get_Attribute(ChildNode, "name")) then
                     if Get_Attribute(ChildNode, "value") = "Normal" then
                        TempRecord.Priorities(K) := 1;
                     else
                        TempRecord.Priorities(K) := 2;
                     end if;
                     exit;
                  end if;
               end loop;
            end loop;
            if Get_Attribute(MobNode, "order")'Length > 0 then
               TempRecord.Order :=
                 Crew_Orders'Value(Get_Attribute(MobNode, "order"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(MobNode, "item");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               ItemIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               if not Objects_Container.Contains(Items_List, ItemIndex) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " mob '" & To_String(MobIndex) &
                    "', there is no item with index '" &
                    Get_Attribute(ChildNode, "index") & "'.";
               end if;
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    DataAction'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               case SubAction is
                  when ADD =>
                     if Get_Attribute(ChildNode, "amount")'Length /= 0 then
                        TempRecord.Inventory.Append
                          (New_Item =>
                             (ItemIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "amount")),
                              0));
                     else
                        if Integer'Value
                            (Get_Attribute(ChildNode, "minamount")) >
                          Integer'Value
                            (Get_Attribute(ChildNode, "maxamount")) then
                           raise Data_Loading_Error
                             with "Can't " &
                             To_Lower(DataAction'Image(Action)) & " mob '" &
                             To_String(MobIndex) &
                             " invalid range for amount of '" &
                             Get_Attribute(ChildNode, "index") & "'.";
                        end if;
                        TempRecord.Inventory.Append
                          (New_Item =>
                             (ItemIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "minamount")),
                              Integer'Value
                                (Get_Attribute(ChildNode, "maxamount"))));
                     end if;
                  when UPDATE =>
                     for Item of TempRecord.Inventory loop
                        if Item.ProtoIndex = ItemIndex then
                           if Get_Attribute(ChildNode, "amount")'Length /=
                             0 then
                              Item :=
                                (ItemIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "amount")),
                                 0);
                           else
                              if Integer'Value
                                  (Get_Attribute(ChildNode, "minamount")) >
                                Integer'Value
                                  (Get_Attribute(ChildNode, "maxamount")) then
                                 raise Data_Loading_Error
                                   with "Can't " &
                                   To_Lower(DataAction'Image(Action)) &
                                   " mob '" & To_String(MobIndex) &
                                   " invalid range for amount of '" &
                                   Get_Attribute(ChildNode, "index") & "'.";
                              end if;
                              Item :=
                                (ItemIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "minamount")),
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "maxamount")));
                           end if;
                           exit;
                        end if;
                     end loop;
                  when REMOVE =>
                     declare
                        DeleteIndex: Natural := 0;
                     begin
                        for K in
                          TempRecord.Inventory.First_Index ..
                            TempRecord.Inventory.Last_Index loop
                           if TempRecord.Inventory(K).ProtoIndex =
                             ItemIndex then
                              DeleteIndex := K;
                              exit;
                           end if;
                        end loop;
                        if DeleteIndex > 0 then
                           TempRecord.Inventory.Delete(DeleteIndex);
                        end if;
                     end;
               end case;
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(MobNode, "equipment");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               for K in EquipmentNames'Range loop
                  if EquipmentNames(K) =
                    To_Unbounded_String(Get_Attribute(ChildNode, "slot")) then
                     TempRecord.Equipment(K) :=
                       Positive'Value(Get_Attribute(ChildNode, "index"));
                     exit;
                  end if;
               end loop;
            end loop;
            if Action /= UPDATE then
               ProtoMobs_Container.Include
                 (ProtoMobs_List, MobIndex, TempRecord);
               LogMessage("Mob added: " & To_String(MobIndex), Everything);
            else
               ProtoMobs_List(MobIndex) := TempRecord;
               LogMessage("Mob updated: " & To_String(MobIndex), Everything);
            end if;
         else
            ProtoMobs_Container.Exclude(ProtoMobs_List, MobIndex);
            LogMessage("Mob removed: " & To_String(MobIndex), Everything);
         end if;
      end loop;
   end LoadMobs;

   function GenerateMob
     (MobIndex, FactionIndex: Unbounded_String) return Member_Data is
      Mob: Member_Data;
      ProtoMob: constant ProtoMobRecord := ProtoMobs_List(MobIndex);
      Amount: Natural;
      HighestSkillLevel, WeaponSkillLevel: Skill_Range := 1;
      SkillIndex: Skills_Container.Extended_Index;
   begin
      Mob.Faction :=
        (if GetRandom(1, 100) < 99 then FactionIndex else GetRandomFaction);
      Mob.Gender := 'M';
      if not Factions_List(Mob.Faction).Flags.Contains
          (To_Unbounded_String("nogender"))
        and then GetRandom(1, 100) > 50 then
         Mob.Gender := 'F';
      end if;
      Mob.Name := GenerateMemberName(Mob.Gender, Mob.Faction);
      for Skill of ProtoMob.Skills loop
         SkillIndex :=
           (if Skill(1) = Positive(Skills_List.Length) + 1 then
              Factions_List(Mob.Faction).WeaponSkill
            else Skill(1));
         if Skill(3) = 0 then
            Mob.Skills.Append(New_Item => (SkillIndex, Skill(2), 0));
         else
            Mob.Skills.Append
              (New_Item => (SkillIndex, GetRandom(Skill(2), Skill(3)), 0));
         end if;
         if SkillIndex = Factions_List(Mob.Faction).WeaponSkill then
            WeaponSkillLevel := Mob.Skills(Mob.Skills.Last_Index)(2);
         end if;
         if Mob.Skills(Mob.Skills.Last_Index)(2) > HighestSkillLevel then
            HighestSkillLevel := Mob.Skills(Mob.Skills.Last_Index)(2);
         end if;
      end loop;
      for Attribute of ProtoMob.Attributes loop
         if Attribute(2) = 0 then
            Mob.Attributes.Append(New_Item => Attribute);
         else
            Mob.Attributes.Append
              (New_Item => (GetRandom(Attribute(1), Attribute(2)), 0));
         end if;
      end loop;
      for I in ProtoMob.Inventory.Iterate loop
         Amount :=
           (if ProtoMob.Inventory(I).MaxAmount > 0 then
              GetRandom
                (ProtoMob.Inventory(I).MinAmount,
                 ProtoMob.Inventory(I).MaxAmount)
            else ProtoMob.Inventory(I).MinAmount);
         Mob.Inventory.Append
           (New_Item =>
              (ProtoIndex => ProtoMob.Inventory(I).ProtoIndex,
               Amount => Amount, Name => Null_Unbounded_String,
               Durability => 100, Price => 0));
      end loop;
      Mob.Equipment := ProtoMob.Equipment;
      declare
         ItemsList: UnboundedString_Container.Vector;
         ItemIndex: Unbounded_String;
      begin
         for I in 1 .. 6 loop
            case I is
               when 1 =>
                  ItemsList := Weapons_List;
               when 2 =>
                  ItemsList := Shields_List;
               when 3 =>
                  ItemsList := HeadArmors_List;
               when 4 =>
                  ItemsList := ChestArmors_List;
               when 5 =>
                  ItemsList := ArmsArmors_List;
               when 6 =>
                  ItemsList := LegsArmors_List;
            end case;
            if Mob.Equipment(I) = 0 then
               ItemIndex := Null_Unbounded_String;
               if GetRandom(1, 100) < 95 then
                  ItemIndex :=
                    GetRandomItem
                      (ItemsList, I, HighestSkillLevel, WeaponSkillLevel,
                       Mob.Faction);
               end if;
               if ItemIndex /= Null_Unbounded_String then
                  Mob.Inventory.Append
                    (New_Item =>
                       (ProtoIndex => ItemIndex, Amount => 1,
                        Name => Null_Unbounded_String, Durability => 100,
                        Price => 0));
                  Mob.Equipment(I) := Mob.Inventory.Last_Index;
               end if;
            end if;
         end loop;
      end;
      Mob.Orders := ProtoMob.Priorities;
      Mob.Order := ProtoMob.Order;
      Mob.OrderTime := 15;
      Mob.PreviousOrder := Rest;
      Mob.Health := 100;
      Mob.Tired := 0;
      Mob.Hunger := 0;
      Mob.Thirst := 0;
      Mob.Payment := (20, 0);
      Mob.ContractLength := -1;
      Mob.Morale := (50, 0);
      Mob.Loyalty := 100;
      Mob.HomeBase := 1;
      return Mob;
   end GenerateMob;

   function GetRandomItem
     (ItemsIndexes: UnboundedString_Container.Vector;
      EquipIndex, HighestLevel, WeaponSkillLevel: Positive;
      FactionIndex: Unbounded_String) return Unbounded_String is
      ItemIndex, MaxIndex: Positive;
      NewIndexes: UnboundedString_Container.Vector;
      Added: Boolean;
   begin
      if EquipIndex > 1 then
         for I in ItemsIndexes.First_Index .. ItemsIndexes.Last_Index loop
            Added := False;
            for J in NewIndexes.First_Index .. NewIndexes.Last_Index loop
               if Items_List(ItemsIndexes(I)).Price <
                 Items_List(NewIndexes(J)).Price then
                  NewIndexes.Insert(J, ItemsIndexes(I));
                  Added := True;
                  exit;
               end if;
            end loop;
            if not Added then
               NewIndexes.Append(ItemsIndexes(I));
            end if;
         end loop;
         MaxIndex :=
           Positive
             ((Float(NewIndexes.Last_Index) * (Float(HighestLevel) / 100.0)) +
              1.0);
         if MaxIndex > NewIndexes.Last_Index then
            MaxIndex := NewIndexes.Last_Index;
         end if;
         ItemIndex := GetRandom(NewIndexes.First_Index, MaxIndex);
      else
         for I in ItemsIndexes.First_Index .. ItemsIndexes.Last_Index loop
            Added := False;
            for J in NewIndexes.First_Index .. NewIndexes.Last_Index loop
               if Items_List(ItemsIndexes(I)).Price <
                 Items_List(NewIndexes(J)).Price and
                 Items_List(ItemsIndexes(I)).Value(3) =
                   Factions_List(FactionIndex).WeaponSkill then
                  NewIndexes.Insert(J, ItemsIndexes(I));
                  Added := True;
                  exit;
               end if;
            end loop;
            if not Added and
              Items_List(ItemsIndexes(I)).Value(3) =
                Factions_List(FactionIndex).WeaponSkill then
               NewIndexes.Append(ItemsIndexes(I));
            end if;
         end loop;
         if NewIndexes.Length = 0 then
            return Null_Unbounded_String;
         end if;
         MaxIndex :=
           Positive
             ((Float(NewIndexes.Last_Index) *
               (Float(WeaponSkillLevel) / 100.0)) +
              1.0);
         if MaxIndex > NewIndexes.Last_Index then
            MaxIndex := NewIndexes.Last_Index;
         end if;
         loop
            ItemIndex := GetRandom(NewIndexes.First_Index, MaxIndex);
            exit when Items_List(NewIndexes(ItemIndex)).Value(3) =
              Factions_List(FactionIndex).WeaponSkill;
         end loop;
      end if;
      for Index of ItemsIndexes loop
         if Index = NewIndexes(ItemIndex) then
            return Index;
         end if;
      end loop;
      return Null_Unbounded_String;
   end GetRandomItem;

end Mobs;
