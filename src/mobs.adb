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
      RemoveIndex: Unbounded_String;
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
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "skill");
         for J in 0 .. Length(ChildNodes) - 1 loop
            if Get_Attribute(Item(ChildNodes, J), "level") /= "" then
               TempRecord.Skills.Append
                 (New_Item =>
                    (FindSkillIndex
                       (To_Unbounded_String
                          (Get_Attribute(Item(ChildNodes, J), "name"))),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "level")),
                     0));
            else
               TempRecord.Skills.Append
                 (New_Item =>
                    (FindSkillIndex
                       (To_Unbounded_String
                          (Get_Attribute(Item(ChildNodes, J), "name"))),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "minlevel")),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "maxlevel"))));
            end if;
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "attribute");
         for J in 0 .. Length(ChildNodes) - 1 loop
            if Get_Attribute(Item(ChildNodes, J), "level") /= "" then
               TempRecord.Attributes.Append
                 (New_Item =>
                    (Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "level")),
                     0));
            else
               TempRecord.Attributes.Append
                 (New_Item =>
                    (Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "minlevel")),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "maxlevel"))));
            end if;
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "priority");
         for J in 0 .. Length(ChildNodes) - 1 loop
            for K in OrdersNames'Range loop
               if OrdersNames(K) =
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "name")) then
                  if Get_Attribute(Item(ChildNodes, J), "value") =
                    "Normal" then
                     TempRecord.Priorities(K) := 1;
                  else
                     TempRecord.Priorities(K) := 2;
                  end if;
                  exit;
               end if;
            end loop;
         end loop;
         TempRecord.Order :=
           Crew_Orders'Value(Get_Attribute(Item(NodesList, I), "order"));
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "item");
         for J in 0 .. Length(ChildNodes) - 1 loop
            if Get_Attribute(Item(ChildNodes, J), "amount") /= "" then
               TempRecord.Inventory.Append
                 (New_Item =>
                    (FindProtoItem
                       (To_Unbounded_String
                          (Get_Attribute(Item(ChildNodes, J), "index"))),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "amount")),
                     0));
            else
               TempRecord.Inventory.Append
                 (New_Item =>
                    (FindProtoItem
                       (To_Unbounded_String
                          (Get_Attribute(Item(ChildNodes, J), "index"))),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "minamount")),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "maxamount"))));
            end if;
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "equipment");
         for J in 0 .. Length(ChildNodes) - 1 loop
            for K in EquipmentNames'Range loop
               if EquipmentNames(K) =
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "slot")) then
                  TempRecord.Equipment(K) :=
                    Positive'Value
                      (Get_Attribute(Item(ChildNodes, J), "index"));
                  exit;
               end if;
            end loop;
         end loop;
         if Get_Attribute(Item(NodesList, I), "remove") = "" then
            ProtoMobs_List.Append(New_Item => TempRecord);
            LogMessage
              ("Mob added: " & To_String(TempRecord.Index), Everything);
         else
            RemoveIndex :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "remove"));
            ProtoMobs_List.Delete(Index => FindProtoMob(RemoveIndex));
            LogMessage("Mob removed: " & To_String(RemoveIndex), Everything);
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
