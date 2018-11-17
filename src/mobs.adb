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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Game; use Game;
with Items; use Items;

package body Mobs is

   procedure LoadMobs(Reader: Tree_Reader) is
      MobsData: Document;
      NodesList, ChildNodes: Node_List;
      TempRecord: ProtoMobRecord;
      TempSkills, TempInventory: Skills_Container.Vector;
      TempAttributes: Attributes_Container.Vector;
      TempPriorities: constant Orders_Array := (others => 0);
      TempEquipment: constant Equipment_Array := (others => 0);
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
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
      EquipmentNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Weapon"), To_Unbounded_String("Shield"),
         To_Unbounded_String("Head"), To_Unbounded_String("Torso"),
         To_Unbounded_String("Arms"), To_Unbounded_String("Legs"),
         To_Unbounded_String("Tool"));
      Action, SubAction: DataAction;
      MobNode, ChildNode: Node;
      MobIndex, ChildIndex: Natural;
      DeleteIndex: Positive;
   begin
      TempRecord :=
        (Index => Null_Unbounded_String, Skills => TempSkills,
         Attributes => TempAttributes, Order => Rest,
         Priorities => TempPriorities, Inventory => TempInventory,
         Equipment => TempEquipment);
      MobsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(MobsData, "mobile");
      for I in 0 .. Length(NodesList) - 1 loop
         MobNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(MobNode, "index"));
         if Get_Attribute(MobNode, "action")'Length > 0 then
            Action := DataAction'Value(Get_Attribute(MobNode, "action"));
         else
            Action := ADD;
         end if;
         MobIndex := FindProtoMob(TempRecord.Index);
         if (Action = UPDATE or Action = REMOVE) then
            if MobIndex = 0 then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " mob '" & To_String(TempRecord.Index) &
                 "', there no mob with that index.";
            end if;
         elsif MobIndex > 0 then
            raise Data_Loading_Error
              with "Can't add mob '" & To_String(TempRecord.Index) &
              "', there is one with that index.";
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
               if ChildIndex = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " mob '" & To_String(TempRecord.Index) &
                    "', there no skill named '" &
                    Get_Attribute(ChildNode, "name") & "'.";
               end if;
               if Get_Attribute(ChildNode, "action")'Length > 0 then
                  SubAction :=
                    DataAction'Value(Get_Attribute(ChildNode, "action"));
               else
                  SubAction := ADD;
               end if;
               case SubAction is
                  when ADD =>
                     if Get_Attribute(ChildNode, "level")'Length /= 0 then
                        TempRecord.Skills.Append
                          (New_Item =>
                             (ChildIndex,
                              Integer'Value(Get_Attribute(ChildNode, "level")),
                              0));
                     else
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
            if Length(ChildNodes) > 0 and ACTION = UPDATE then
               TempRecord.Attributes.Clear;
            end if;
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               if Get_Attribute(ChildNode, "level") /= "" then
                  TempRecord.Attributes.Append
                    (New_Item =>
                       (Integer'Value(Get_Attribute(ChildNode, "level")), 0));
               else
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
               ChildIndex :=
                 FindProtoItem
                   (To_Unbounded_String(Get_Attribute(ChildNode, "index")));
               if ChildIndex = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " mob '" & To_String(TempRecord.Index) &
                    "', there no item with index '" &
                    Get_Attribute(ChildNode, "index") & "'.";
               end if;
               if Get_Attribute(ChildNode, "action")'Length > 0 then
                  SubAction :=
                    DataAction'Value(Get_Attribute(ChildNode, "action"));
               else
                  SubAction := ADD;
               end if;
               case SubAction is
                  when ADD =>
                     if Get_Attribute(ChildNode, "amount")'Length /= 0 then
                        TempRecord.Inventory.Append
                          (New_Item =>
                             (ChildIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "amount")),
                              0));
                     else
                        TempRecord.Inventory.Append
                          (New_Item =>
                             (ChildIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "minamount")),
                              Integer'Value
                                (Get_Attribute(ChildNode, "maxamount"))));
                     end if;
                  when UPDATE =>
                     for Item of TempRecord.Inventory loop
                        if Item(1) = ChildIndex then
                           if Get_Attribute(ChildNode, "amount")'Length /=
                             0 then
                              Item :=
                                (ChildIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "amount")),
                                 0);
                           else
                              Item :=
                                (ChildIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "minamount")),
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "maxamount")));
                           end if;
                           exit;
                        end if;
                     end loop;
                  when REMOVE =>
                     for K in TempRecord.Inventory.Iterate loop
                        if TempRecord.Inventory(K)(1) = ChildIndex then
                           DeleteIndex := Skills_Container.To_Index(K);
                           exit;
                        end if;
                     end loop;
                     TempRecord.Inventory.Delete(Index => DeleteIndex);
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
               ProtoMobs_List.Append(New_Item => TempRecord);
               LogMessage
                 ("Mob added: " & To_String(TempRecord.Index), Everything);
            else
               ProtoMobs_List(MobIndex) := TempRecord;
               LogMessage
                 ("Mob updated: " & To_String(TempRecord.Index), Everything);
            end if;
         else
            Items_List.Delete(Index => MobIndex);
            LogMessage
              ("Mob removed: " & To_String(TempRecord.Index), Everything);
         end if;
         TempRecord :=
           (Index => Null_Unbounded_String, Skills => TempSkills,
            Attributes => TempAttributes, Order => Rest,
            Priorities => TempPriorities, Inventory => TempInventory,
            Equipment => TempEquipment);
      end loop;
   end LoadMobs;

   function FindProtoMob(Index: Unbounded_String) return Natural is
   begin
      for I in ProtoMobs_List.Iterate loop
         if ProtoMobs_List(I).Index = Index then
            return ProtoMobs_Container.To_Index(I);
         end if;
      end loop;
      return 0;
   end FindProtoMob;

end Mobs;
