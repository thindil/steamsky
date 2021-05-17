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

with Ada.Exceptions; use Ada.Exceptions;
with Messages; use Messages;
with HallOfFame; use HallOfFame;
with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Maps; use Maps;
with Events; use Events;
with Crew.Inventory; use Crew.Inventory;
with Utils; use Utils;
with Missions; use Missions;
with Factions; use Factions;

package body Ships.Crew is

   function GetSkillLevel
     (Member: Member_Data; SkillIndex: Skills_Container.Extended_Index)
      return Skill_Range is
      SkillLevel: Integer := 0;
      Damage: Damage_Factor := 0.0;
      BaseSkillLevel: Natural range 0 .. 151;
   begin
      Get_Skill_Loop :
      for Skill of Member.Skills loop
         if Skill(1) = SkillIndex then
            BaseSkillLevel :=
              Skill(2) + Member.Attributes(Skills_List(Skill(1)).Attribute)(1);
            Damage := 1.0 - Damage_Factor(Float(Member.Health) / 100.0);
            SkillLevel :=
              SkillLevel +
              (BaseSkillLevel -
               Integer(Float(BaseSkillLevel) * Float(Damage)));
            if Member.Thirst > 40 then
               Damage := 1.0 - Damage_Factor(Float(Member.Thirst) / 100.0);
               SkillLevel :=
                 SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
            end if;
            if Member.Hunger > 80 then
               Damage := 1.0 - Damage_Factor(Float(Member.Hunger) / 100.0);
               SkillLevel :=
                 SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
            end if;
            if Member.Morale(1) < 25 then
               Damage := Damage_Factor(Float(Member.Morale(1)) / 100.0);
               SkillLevel :=
                 SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
            end if;
            if SkillLevel < 1 then
               SkillLevel := 1;
            end if;
            if SkillLevel > 100 then
               SkillLevel := 100;
            end if;
            if Member.Morale(1) > 90 then
               Damage := Damage_Factor(Float(SkillLevel) / 100.0);
               SkillLevel :=
                 SkillLevel + (Integer(Float(BaseSkillLevel) * Float(Damage)));
               if SkillLevel > 100 then
                  SkillLevel := 100;
               end if;
            end if;
            return SkillLevel;
         end if;
      end loop Get_Skill_Loop;
      return SkillLevel;
   end GetSkillLevel;

   procedure Death
     (MemberIndex: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out ShipRecord; CreateBody: Boolean := True) is
   begin
      if Ship = PlayerShip then
         if MemberIndex > 1 then
            AddMessage
              (To_String(Ship.Crew(MemberIndex).Name) & " died from " &
               To_String(Reason) & ".",
               CombatMessage, RED);
         else
            AddMessage
              ("You died from " & To_String(Reason) & ".", CombatMessage, RED);
            PlayerShip.Crew(MemberIndex).Order := Rest;
            PlayerShip.Crew(MemberIndex).Health := 0;
            Update_Hall_Of_Fame(PlayerShip.Crew(MemberIndex).Name, Reason);
            return;
         end if;
      end if;
      if CreateBody then
         Ship.Cargo.Append
           (New_Item =>
              (ProtoIndex => Corpse_Index, Amount => 1,
               Name =>
                 Ship.Crew(MemberIndex).Name &
                 To_Unbounded_String("'s corpse"),
               Durability => 100, Price => 0));
      end if;
      DeleteMember(MemberIndex, Ship);
      for I in Ship.Crew.Iterate loop
         UpdateMorale(Ship, Crew_Container.To_Index(I), GetRandom(-25, -10));
      end loop;
   end Death;

   procedure DeleteMember
     (MemberIndex: Crew_Container.Extended_Index; Ship: in out ShipRecord) is
      TempValue: Integer;
   begin
      Ship.Crew.Delete(Index => MemberIndex);
      Module_Loop :
      for Module of Ship.Modules loop
         Owners_Loop :
         for Owner of Module.Owner loop
            if Owner = MemberIndex then
               Owner := 0;
            elsif Owner > MemberIndex then
               Owner := Owner - 1;
            end if;
         end loop Owners_Loop;
      end loop Module_Loop;
      if Ship = PlayerShip then
         Delete_Missions_Loop :
         for I in
           AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
            if AcceptedMissions(I).MType = Passenger
              and then AcceptedMissions(I).Data = MemberIndex then
               DeleteMission(I);
               exit Delete_Missions_Loop;
            end if;
         end loop Delete_Missions_Loop;
         Update_Missions_Loop :
         for Mission of AcceptedMissions loop
            if Mission.MType = Passenger
              and then Mission.Data > MemberIndex then
               TempValue := Mission.Data;
               TempValue := TempValue - 1;
               Mission.Data := TempValue;
            end if;
         end loop Update_Missions_Loop;
      end if;
   end DeleteMember;

   function FindMember
     (Order: Crew_Orders; Crew: Crew_Container.Vector := PlayerShip.Crew)
      return Natural is
   begin
      Find_Member_Loop :
      for I in Crew.Iterate loop
         if Crew(I).Order = Order then
            return Crew_Container.To_Index(I);
         end if;
      end loop Find_Member_Loop;
      return 0;
   end FindMember;

   procedure GiveOrders
     (Ship: in out ShipRecord; MemberIndex: Crew_Container.Extended_Index;
      GivenOrder: Crew_Orders;
      ModuleIndex: Modules_Container.Extended_Index := 0;
      CheckPriorities: Boolean := True) is
      MemberName: constant String := To_String(Ship.Crew(MemberIndex).Name);
      ToolsIndex: Inventory_Container.Extended_Index := 0;
      RequiredTool: Unbounded_String;
      ToolQuality: Items_Durability := Default_Item_Durability;
      ModuleIndex2: Modules_Container.Extended_Index := 0;
   begin
      if GivenOrder = Ship.Crew(MemberIndex).Order then
         if GivenOrder in Craft | Gunner then
            Give_Orders_Modules_Loop :
            for I in Ship.Modules.Iterate loop
               if Modules_Container.To_Index(I) = ModuleIndex then
                  Owners_Loop :
                  for Owner of Ship.Modules(I).Owner loop
                     if Owner = MemberIndex then
                        return;
                     end if;
                  end loop Owners_Loop;
               end if;
            end loop Give_Orders_Modules_Loop;
         else
            return;
         end if;
      end if;
      if GivenOrder /= Rest and
        ((Ship.Crew(MemberIndex).Morale(1) < 11 and GetRandom(1, 100) < 50) or
         Ship.Crew(MemberIndex).Loyalty < 20) then
         if Ship = PlayerShip then
            raise Crew_Order_Error
              with MemberName & " refuses to execute order.";
         else
            return;
         end if;
      end if;
      if GivenOrder = Train
        and then Ship.Modules(ModuleIndex).TrainedSkill = 0 then
         raise Crew_Order_Error
           with MemberName & " can't start training because " &
           To_String(Ship.Modules(ModuleIndex).Name) & " isn't prepared.";
      end if;
      if GivenOrder in Pilot | Engineer | Upgrading | Talk then
         Give_Crew_Orders_Loop :
         for I in Ship.Crew.First_Index .. Ship.Crew.Last_Index loop
            if Ship.Crew(I).Order = GivenOrder then
               GiveOrders(PlayerShip, I, Rest, 0, False);
               exit Give_Crew_Orders_Loop;
            end if;
         end loop Give_Crew_Orders_Loop;
      elsif (GivenOrder in Gunner | Craft | Train) or
        (GivenOrder = Heal and ModuleIndex > 0) then
         declare
            FreePosition: Boolean := False;
         begin
            Free_Position_Loop :
            for Owner of Ship.Modules(ModuleIndex).Owner loop
               if Owner = 0 then
                  FreePosition := True;
                  exit Free_Position_Loop;
               end if;
            end loop Free_Position_Loop;
            if not FreePosition then
               GiveOrders
                 (PlayerShip, Ship.Modules(ModuleIndex).Owner(1), Rest, 0,
                  False);
            end if;
         end;
      end if;
      if ModuleIndex = 0 and (GivenOrder in Pilot | Engineer | Rest) then
         declare
            MType: constant ModuleType :=
              (case GivenOrder is when Pilot => COCKPIT,
                 when Engineer => ENGINE, when Rest => CABIN,
                 when others => ENGINE);
         begin
            Modules_Loop :
            for I in Ship.Modules.Iterate loop
               if MType /= CABIN then
                  if Modules_List(Ship.Modules(I).ProtoIndex).MType = MType and
                    Ship.Modules(I).Durability > 0 then
                     if Ship.Modules(I).Owner(1) /= 0 then
                        GiveOrders
                          (PlayerShip, Ship.Modules(I).Owner(1), Rest, 0,
                           False);
                     end if;
                     ModuleIndex2 := Modules_Container.To_Index(I);
                     exit Modules_Loop;
                  end if;
               else
                  if Ship.Modules(I).MType = CABIN and
                    Ship.Modules(I).Durability > 0 then
                     Cabin_Owners_Loop :
                     for Owner of Ship.Modules(I).Owner loop
                        if MemberIndex = Owner then
                           ModuleIndex2 := Modules_Container.To_Index(I);
                           exit Modules_Loop;
                        end if;
                     end loop Cabin_Owners_Loop;
                  end if;
               end if;
            end loop Modules_Loop;
         end;
      else
         ModuleIndex2 := ModuleIndex;
      end if;
      if ModuleIndex2 = 0 and Ship = PlayerShip then
         case GivenOrder is
            when Pilot =>
               raise Crew_Order_Error
                 with MemberName &
                 " can't start piloting because the cockpit is destroyed or you don't have cockpit.";
            when Engineer =>
               raise Crew_Order_Error
                 with MemberName &
                 " can't start engineer's duty because all of the engines are destroyed or you don't have engine.";
            when Gunner =>
               raise Crew_Order_Error
                 with MemberName &
                 " can't start operating gun because all of the guns are destroyed or you don't have any installed.";
            when Rest =>
               Modules_Loop2 :
               for Module of Ship.Modules loop
                  if Module.MType = CABIN and Module.Durability > 0 then
                     Owners_Loop2 :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           Owner := MemberIndex;
                           AddMessage
                             (MemberName & " takes " & To_String(Module.Name) &
                              " as their own cabin.",
                              OtherMessage);
                           exit Modules_Loop2;
                        end if;
                     end loop Owners_Loop2;
                  end if;
               end loop Modules_Loop2;
            when others =>
               null;
         end case;
      end if;
      Modules_Loop3 :
      for Module of Ship.Modules loop
         if Module.MType /= CABIN then
            Owners_Loop3 :
            for Owner of Module.Owner loop
               if Owner = MemberIndex then
                  Owner := 0;
                  exit Modules_Loop3;
               end if;
            end loop Owners_Loop3;
         end if;
      end loop Modules_Loop3;
      if ToolsIndex > 0 and
        Ship.Crew(MemberIndex).Equipment(7) /= ToolsIndex then
         UpdateInventory
           (MemberIndex, 1, Ship.Cargo(ToolsIndex).ProtoIndex,
            Ship.Cargo(ToolsIndex).Durability);
         UpdateCargo(Ship => Ship, Amount => -1, CargoIndex => ToolsIndex);
         Ship.Crew(MemberIndex).Equipment(7) :=
           FindItem
             (Inventory => Ship.Crew(MemberIndex).Inventory,
              ItemType => RequiredTool);
      end if;
      ToolsIndex := Ship.Crew(MemberIndex).Equipment(7);
      if ToolsIndex > 0
        and then
          Items_List(Ship.Crew(MemberIndex).Inventory(ToolsIndex).ProtoIndex)
            .IType /=
          RequiredTool then
         UpdateCargo
           (Ship, Ship.Crew(MemberIndex).Inventory(ToolsIndex).ProtoIndex, 1,
            Ship.Crew(MemberIndex).Inventory(ToolsIndex).Durability);
         UpdateInventory
           (MemberIndex => MemberIndex, Amount => -1,
            InventoryIndex => ToolsIndex);
         ToolsIndex := 0;
      end if;
      if GivenOrder in Upgrading | Repair | Clean |
            Train then -- Check for tools
         if GivenOrder = Clean then
            RequiredTool := Cleaning_Tools;
         elsif GivenOrder = Train then
            RequiredTool :=
              Skills_List(Ship.Modules(ModuleIndex).TrainedSkill).Tool;
            ToolQuality :=
              GetTrainingToolQuality
                (MemberIndex, Ship.Modules(ModuleIndex).TrainedSkill);
         else
            RequiredTool := Repair_Tools;
         end if;
         if RequiredTool /= Null_Unbounded_String then
            if ToolsIndex = 0 then
               ToolsIndex :=
                 FindItem
                   (Inventory => Ship.Cargo, ItemType => RequiredTool,
                    Quality => ToolQuality);
               if ToolsIndex = 0 then
                  ToolsIndex :=
                    FindItem
                      (Inventory => Ship.Crew(MemberIndex).Inventory,
                       ItemType => RequiredTool, Quality => ToolQuality);
                  if ToolsIndex > 0 then
                     Ship.Crew(MemberIndex).Equipment(7) := ToolsIndex;
                  end if;
               else
                  Ship.Crew(MemberIndex).Equipment(7) := 0;
               end if;
            end if;
            if ToolsIndex = 0 then
               case GivenOrder is
                  when Repair =>
                     raise Crew_Order_Error
                       with MemberName &
                       " can't start repairing ship because you don't have the proper tools.";
                  when Clean =>
                     raise Crew_Order_Error
                       with MemberName &
                       " can't start cleaning ship because you don't have any cleaning tools.";
                  when Upgrading =>
                     raise Crew_Order_Error
                       with MemberName &
                       " can't start upgrading module because you don't have the proper tools.";
                  when Train =>
                     raise Crew_Order_Error
                       with MemberName &
                       " can't start training because you don't have the proper tools.";
                  when others =>
                     return;
               end case;
            end if;
         end if;
      end if;
      if GivenOrder = Rest then
         Ship.Crew(MemberIndex).PreviousOrder := Rest;
         if Ship.Crew(MemberIndex).Order in Repair | Clean | Upgrading |
               Train then
            ToolsIndex := Ship.Crew(MemberIndex).Equipment(7);
            if ToolsIndex > 0 then
               UpdateCargo
                 (Ship,
                  Ship.Crew(MemberIndex).Inventory(ToolsIndex).ProtoIndex, 1,
                  Ship.Crew(MemberIndex).Inventory(ToolsIndex).Durability);
               UpdateInventory
                 (MemberIndex => MemberIndex, Amount => -1,
                  InventoryIndex => ToolsIndex);
            end if;
         end if;
      end if;
      if Ship = PlayerShip then
         case GivenOrder is
            when Pilot =>
               AddMessage(MemberName & " starts piloting.", OrderMessage);
               Ship.Modules(ModuleIndex2).Owner(1) := MemberIndex;
            when Engineer =>
               AddMessage
                 (MemberName & " starts engineer's duty.", OrderMessage);
            when Gunner =>
               AddMessage(MemberName & " starts operating gun.", OrderMessage);
               Ship.Modules(ModuleIndex2).Owner(1) := MemberIndex;
            when Rest =>
               AddMessage(MemberName & " is going on a break.", OrderMessage);
            when Repair =>
               AddMessage
                 (MemberName & " starts repairing ship.", OrderMessage);
            when Craft =>
               AddMessage(MemberName & " starts manufacturing.", OrderMessage);
               for Owner of Ship.Modules(ModuleIndex2).Owner loop
                  if Owner = 0 then
                     Owner := MemberIndex;
                     exit;
                  end if;
               end loop;
            when Upgrading =>
               AddMessage
                 (MemberName & " starts upgrading " &
                  To_String(Ship.Modules(Ship.UpgradeModule).Name) & ".",
                  OrderMessage);
            when Talk =>
               AddMessage
                 (MemberName & " is now assigned to talking in bases.",
                  OrderMessage);
            when Heal =>
               AddMessage
                 (MemberName & " starts healing wounded crew members.",
                  OrderMessage);
               if ModuleIndex > 0 then
                  for Owner of Ship.Modules(ModuleIndex).Owner loop
                     if Owner = 0 then
                        Owner := MemberIndex;
                        exit;
                     end if;
                  end loop;
               end if;
            when Clean =>
               AddMessage(MemberName & " starts cleaning ship.", OrderMessage);
            when Boarding =>
               AddMessage
                 (MemberName & " starts boarding the enemy ship.",
                  OrderMessage);
            when Defend =>
               AddMessage
                 (MemberName & " starts defending the ship.", OrderMessage);
            when Train =>
               AddMessage
                 (MemberName & " starts personal training.", OrderMessage);
               for Owner of Ship.Modules(ModuleIndex2).Owner loop
                  if Owner = 0 then
                     Owner := MemberIndex;
                     exit;
                  end if;
               end loop;
         end case;
      end if;
      Ship.Crew(MemberIndex).Order := GivenOrder;
      Ship.Crew(MemberIndex).OrderTime := 15;
      if GivenOrder /= Rest then
         UpdateMorale(Ship, MemberIndex, -1);
      end if;
      if CheckPriorities then
         UpdateOrders(Ship);
      end if;
   exception
      when An_Exception : Crew_No_Space_Error =>
         if Ship = PlayerShip then
            raise Crew_Order_Error with Exception_Message(An_Exception);
         else
            return;
         end if;
   end GiveOrders;

   procedure UpdateOrders(Ship: in out ShipRecord; Combat: Boolean := False) is
      HavePilot, HaveEngineer, HaveUpgrade, HaveTrader, NeedClean, NeedRepairs,
      NeedGunners, NeedCrafters, CanHeal, NeedTrader: Boolean := False;
      EventIndex: constant Events_Container.Extended_Index :=
        SkyMap(Ship.SkyX, Ship.SkyY).EventIndex;
      function UpdatePosition
        (Order: Crew_Orders; MaxPriority: Boolean := True) return Boolean is
         OrderIndex: Natural := 0;
         MemberIndex: Crew_Container.Extended_Index := 0;
         ModuleIndex: Modules_Container.Extended_Index := 0;
      begin
         OrderIndex :=
           (if Crew_Orders'Pos(Order) < Crew_Orders'Pos(Defend) then
              Crew_Orders'Pos(Order) + 1
            else Crew_Orders'Pos(Order));
         if MaxPriority then
            Find_Member_Max_Priority_Loop :
            for I in Ship.Crew.Iterate loop
               if Ship.Crew(I).Orders(OrderIndex) = 2 and
                 Ship.Crew(I).Order /= Order and
                 Ship.Crew(I).PreviousOrder /= Order then
                  MemberIndex := Crew_Container.To_Index(I);
                  exit Find_Member_Max_Priority_Loop;
               end if;
            end loop Find_Member_Max_Priority_Loop;
         else
            Find_Member_Priority_Loop :
            for I in Ship.Crew.Iterate loop
               if Ship.Crew(I).Orders(OrderIndex) = 1 and
                 Ship.Crew(I).Order = Rest and
                 Ship.Crew(I).PreviousOrder = Rest then
                  MemberIndex := Crew_Container.To_Index(I);
                  exit Find_Member_Priority_Loop;
               end if;
            end loop Find_Member_Priority_Loop;
         end if;
         if MemberIndex = 0 then
            return False;
         end if;
         if Order in Gunner | Craft | Heal | Pilot | Engineer | Train then
            Find_Module_Index_Loop :
            for I in Ship.Modules.Iterate loop
               if Ship.Modules(I).Durability > 0 then
                  case Ship.Modules(I).MType is
                     when GUN =>
                        if Order = Gunner and Ship.Modules(I).Owner(1) = 0 then
                           ModuleIndex := Modules_Container.To_Index(I);
                           exit Find_Module_Index_Loop;
                        end if;
                     when WORKSHOP =>
                        if Order = Craft and
                          Ship.Modules(I).CraftingIndex /=
                            Null_Unbounded_String then
                           Find_Empty_Workplace_Loop :
                           for Owner of Ship.Modules(I).Owner loop
                              if Owner = 0 then
                                 ModuleIndex := Modules_Container.To_Index(I);
                                 exit Find_Empty_Workplace_Loop;
                              end if;
                           end loop Find_Empty_Workplace_Loop;
                           exit Find_Module_Index_Loop when ModuleIndex > 0;
                        end if;
                     when MEDICAL_ROOM =>
                        if Order = Heal then
                           Find_Empty_Medical_Loop :
                           for Owner of Ship.Modules(I).Owner loop
                              if Owner = 0 then
                                 ModuleIndex := Modules_Container.To_Index(I);
                                 exit Find_Empty_Medical_Loop;
                              end if;
                           end loop Find_Empty_Medical_Loop;
                           exit Find_Module_Index_Loop when ModuleIndex > 0;
                        end if;
                     when COCKPIT =>
                        if Order = Pilot then
                           ModuleIndex := Modules_Container.To_Index(I);
                           exit Find_Module_Index_Loop;
                        end if;
                     when ENGINE =>
                        if Order = Engineer then
                           ModuleIndex := Modules_Container.To_Index(I);
                           exit Find_Module_Index_Loop;
                        end if;
                     when TRAINING_ROOM =>
                        if Order = Train and
                          Ship.Modules(I).TrainedSkill > 0 then
                           Find_Empty_Training_Loop :
                           for Owner of Ship.Modules(I).Owner loop
                              if Owner = 0 then
                                 ModuleIndex := Modules_Container.To_Index(I);
                                 exit Find_Empty_Training_Loop;
                              end if;
                           end loop Find_Empty_Training_Loop;
                           exit Find_Module_Index_Loop when ModuleIndex > 0;
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
            end loop Find_Module_Index_Loop;
            if ModuleIndex = 0 then
               return False;
            end if;
         end if;
         if Ship.Crew(MemberIndex).Order /= Rest then
            GiveOrders(Ship, MemberIndex, Rest, 0, False);
         end if;
         GiveOrders(Ship, MemberIndex, Order, ModuleIndex);
         return True;
      exception
         when An_Exception : Crew_Order_Error | Crew_No_Space_Error =>
            if Ship = PlayerShip then
               AddMessage(Exception_Message(An_Exception), OrderMessage, RED);
            end if;
            return False;
      end UpdatePosition;
   begin
      Crew_Members_Loop :
      for Member of Ship.Crew loop
         case Member.Order is
            when Pilot =>
               HavePilot := True;
            when Engineer =>
               HaveEngineer := True;
            when Upgrading =>
               HaveUpgrade := True;
            when Talk =>
               HaveTrader := True;
            when others =>
               null;
         end case;
         if Member.Health < 100 then
            if FindItem
                (Inventory => Ship.Cargo,
                 ItemType => Factions_List(Member.Faction).HealingTools) >
              0 then
               CanHeal := True;
            end if;
         end if;
      end loop Crew_Members_Loop;
      Modules_Need_Loop :
      for Module of Ship.Modules loop
         if Module.Durability > 0 then
            case Module.MType is
               when GUN =>
                  if Module.Owner(1) = 0 and not NeedGunners then
                     NeedGunners := True;
                  end if;
               when WORKSHOP =>
                  if Module.CraftingIndex /= Null_Unbounded_String and
                    not NeedCrafters then
                     Find_Empty_Crafting_Loop :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           NeedCrafters := True;
                           exit Find_Empty_Crafting_Loop;
                        end if;
                     end loop Find_Empty_Crafting_Loop;
                  end if;
               when CABIN =>
                  if Module.Cleanliness < Module.Quality then
                     NeedClean := True;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         if Module.Durability < Module.MaxDurability and not NeedRepairs then
            Find_Need_Repairs_Loop :
            for Item of Ship.Cargo loop
               if Items_List(Item.ProtoIndex).IType =
                 Modules_List(Module.ProtoIndex).RepairMaterial then
                  NeedRepairs := True;
                  exit Find_Need_Repairs_Loop;
               end if;
            end loop Find_Need_Repairs_Loop;
         end if;
      end loop Modules_Need_Loop;
      if SkyMap(Ship.SkyX, Ship.SkyY).BaseIndex > 0 then
         NeedTrader := True;
      end if;
      if (not NeedTrader and EventIndex > 0)
        and then
        (Events_List(EventIndex).EType = Trader or
         Events_List(EventIndex).EType = FriendlyShip) then
         NeedTrader := True;
      end if;
      if not HavePilot and then UpdatePosition(Pilot) then
         UpdateOrders(Ship);
      end if;
      if not HaveEngineer and then UpdatePosition(Engineer) then
         UpdateOrders(Ship);
      end if;
      if NeedGunners and then UpdatePosition(Gunner) then
         UpdateOrders(Ship);
      end if;
      if NeedCrafters and then UpdatePosition(Craft) then
         UpdateOrders(Ship);
      end if;
      if not HaveUpgrade and Ship.UpgradeModule > 0 and
        FindItem(Inventory => Ship.Cargo, ItemType => Repair_Tools) > 0 then
         if FindItem
             (Inventory => Ship.Cargo,
              ItemType =>
                Modules_List(Ship.Modules(Ship.UpgradeModule).ProtoIndex)
                  .RepairMaterial) >
           0
           and then UpdatePosition(Upgrading) then
            UpdateOrders(Ship);
         end if;
      end if;
      if (not HaveTrader and NeedTrader) and then UpdatePosition(Talk) then
         UpdateOrders(Ship);
      end if;
      if
        (NeedClean and
         FindItem(Inventory => Ship.Cargo, ItemType => Cleaning_Tools) > 0)
        and then UpdatePosition(Clean) then
         UpdateOrders(Ship);
      end if;
      if CanHeal and then UpdatePosition(Heal) then
         UpdateOrders(Ship);
      end if;
      if
        (NeedRepairs and
         FindItem(Inventory => Ship.Cargo, ItemType => Repair_Tools) > 0)
        and then UpdatePosition(Repair) then
         UpdateOrders(Ship);
      end if;
      if Combat then
         if UpdatePosition(Defend) then
            UpdateOrders(Ship);
         end if;
         if UpdatePosition(Boarding) then
            UpdateOrders(Ship);
         end if;
      end if;
      if UpdatePosition(Train) then
         UpdateOrders(Ship);
      end if;
      if not HavePilot and then UpdatePosition(Pilot, False) then
         UpdateOrders(Ship);
      end if;
      if not HaveEngineer and then UpdatePosition(Engineer, False) then
         UpdateOrders(Ship);
      end if;
      if NeedGunners and then UpdatePosition(Gunner, False) then
         UpdateOrders(Ship);
      end if;
      if NeedCrafters and then UpdatePosition(Craft, False) then
         UpdateOrders(Ship);
      end if;
      if not HaveUpgrade and Ship.UpgradeModule > 0 and
        FindItem(Inventory => Ship.Cargo, ItemType => Repair_Tools) > 0 then
         if FindItem
             (Inventory => Ship.Cargo,
              ItemType =>
                Modules_List(Ship.Modules(Ship.UpgradeModule).ProtoIndex)
                  .RepairMaterial) >
           0
           and then UpdatePosition(Upgrading, False) then
            UpdateOrders(Ship);
         end if;
      end if;
      if (not HaveTrader and SkyMap(Ship.SkyX, Ship.SkyY).BaseIndex > 0)
        and then UpdatePosition(Talk, False) then
         UpdateOrders(Ship);
      end if;
      if
        (NeedClean and
         FindItem(Inventory => Ship.Cargo, ItemType => Cleaning_Tools) > 0)
        and then UpdatePosition(Clean, False) then
         UpdateOrders(Ship);
      end if;
      if CanHeal and then UpdatePosition(Heal, False) then
         UpdateOrders(Ship);
      end if;
      if
        (NeedRepairs and
         FindItem(Inventory => Ship.Cargo, ItemType => Repair_Tools) > 0)
        and then UpdatePosition(Repair, False) then
         UpdateOrders(Ship);
      end if;
      if Combat then
         if UpdatePosition(Defend, False) then
            UpdateOrders(Ship);
         end if;
         if UpdatePosition(Boarding, False) then
            UpdateOrders(Ship);
         end if;
      end if;
      if UpdatePosition(Train, False) then
         UpdateOrders(Ship, False);
      end if;
   end UpdateOrders;

   procedure UpdateMorale
     (Ship: in out ShipRecord; MemberIndex: Crew_Container.Extended_Index;
      Value: Integer) is
      NewMorale, NewLoyalty, NewValue: Integer;
      FactionIndex: constant Unbounded_String :=
        Ship.Crew(MemberIndex).Faction;
   begin
      if Factions_List(FactionIndex).Flags.Contains
          (To_Unbounded_String("nomorale")) then
         return;
      end if;
      NewValue := Value;
      if Factions_List(FactionIndex).Flags.Contains
          (To_Unbounded_String("fanaticism")) then
         if Value > 0 then
            NewValue := Value * 5;
         else
            NewValue := Value / 10;
            if NewValue = 0 and then GetRandom(1, 10) <= abs (Value) then
               NewValue := -1;
            end if;
            if NewValue = 0 then
               return;
            end if;
         end if;
      end if;
      NewValue := Ship.Crew(MemberIndex).Morale(2) + NewValue;
      NewMorale := Ship.Crew(MemberIndex).Morale(1);
      Raise_Morale_Loop :
      while NewValue >= 5 loop
         NewValue := NewValue - 5;
         NewMorale := NewMorale + 1;
      end loop Raise_Morale_Loop;
      Lower_Morale_Loop :
      while NewValue < 0 loop
         NewValue := NewValue + 5;
         NewMorale := NewMorale - 1;
      end loop Lower_Morale_Loop;
      if NewMorale > 100 then
         NewMorale := 100;
      elsif NewMorale < 0 then
         NewMorale := 0;
      end if;
      Ship.Crew(MemberIndex).Morale := (NewMorale, NewValue);
      if Ship = PlayerShip and MemberIndex = 1 then
         return;
      end if;
      NewLoyalty := Ship.Crew(MemberIndex).Loyalty;
      if NewMorale > 75 and NewLoyalty < 100 then
         NewLoyalty := NewLoyalty + 1;
      end if;
      if NewMorale < 25 and NewLoyalty > 0 then
         NewLoyalty := NewLoyalty - GetRandom(5, 10);
      end if;
      if NewLoyalty > 100 then
         NewLoyalty := 100;
      elsif NewLoyalty < 0 then
         NewLoyalty := 0;
      end if;
      Ship.Crew(MemberIndex).Loyalty := NewLoyalty;
   end UpdateMorale;

end Ships.Crew;
