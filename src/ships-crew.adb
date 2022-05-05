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

with Ada.Exceptions; use Ada.Exceptions;
with Messages; use Messages;
with HallOfFame; use HallOfFame;
with Ships.Cargo; use Ships.Cargo;
with Maps; use Maps;
with Events; use Events;
with Crew.Inventory; use Crew.Inventory;
with Utils; use Utils;
with Missions; use Missions;
with Factions; use Factions;

package body Ships.Crew is

   function Get_Skill_Level
     (Member: Member_Data; Skill_Index: Skills_Amount_Range)
      return Skill_Range is
      Skill_Level: Integer := 0;
      Damage: Damage_Factor := 0.0;
      Base_Skill_Level: Natural range 0 .. 151;
   begin
      Get_Skill_Loop :
      for Skill of Member.Skills loop
         if Skill.Index = Skill_Index then
            Base_Skill_Level :=
              Skill.Level +
              Member.Attributes
                (Positive
                   (SkillsData_Container.Element
                      (Container => Skills_List, Index => Skill.Index)
                      .Attribute))
                .Level;
            Damage := 1.0 - Damage_Factor(Float(Member.Health) / 100.0);
            Skill_Level :=
              Skill_Level +
              (Base_Skill_Level -
               Integer(Float(Base_Skill_Level) * Float(Damage)));
            if Member.Thirst > 40 then
               Damage := 1.0 - Damage_Factor(Float(Member.Thirst) / 100.0);
               Skill_Level :=
                 Skill_Level -
                 (Integer(Float(Base_Skill_Level) * Float(Damage)));
            end if;
            if Member.Hunger > 80 then
               Damage := 1.0 - Damage_Factor(Float(Member.Hunger) / 100.0);
               Skill_Level :=
                 Skill_Level -
                 (Integer(Float(Base_Skill_Level) * Float(Damage)));
            end if;
            if Member.Morale(1) < 25 then
               Damage := Damage_Factor(Float(Member.Morale(1)) / 100.0);
               Skill_Level :=
                 Skill_Level -
                 (Integer(Float(Base_Skill_Level) * Float(Damage)));
            end if;
            if Skill_Level < 1 then
               Skill_Level := 1;
            end if;
            if Skill_Level > 100 then
               Skill_Level := 100;
            end if;
            if Member.Morale(1) > 90 then
               Damage := Damage_Factor(Float(Skill_Level) / 100.0);
               Skill_Level :=
                 Skill_Level +
                 (Integer(Float(Base_Skill_Level) * Float(Damage)));
               if Skill_Level > 100 then
                  Skill_Level := 100;
               end if;
            end if;
            return Skill_Level;
         end if;
      end loop Get_Skill_Loop;
      return Skill_Level;
   end Get_Skill_Level;

   procedure Death
     (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; Create_Body: Boolean := True) is
      use Tiny_String;

      Member_Name: Bounded_String := Ship.Crew(Member_Index).Name;
   begin
      if Ship = Player_Ship then
         if Member_Index > 1 then
            Add_Message
              (Message =>
                 To_String(Source => Member_Name) & " died from " &
                 To_String(Source => Reason) & ".",
               M_Type => COMBATMESSAGE, Color => RED);
         else
            Add_Message
              (Message => "You died from " & To_String(Source => Reason) & ".",
               M_Type => COMBATMESSAGE, Color => RED);
            Player_Ship.Crew(Member_Index).Order := REST;
            Player_Ship.Crew(Member_Index).Health := 0;
            Update_Hall_Of_Fame
              (Player_Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source => Player_Ship.Crew(Member_Index).Name)),
               Death_Reason => Reason);
            return;
         end if;
      end if;
      if Create_Body then
         if Length(Source => Member_Name) > 54 then
            Delete
              (Source => Member_Name, From => 55,
               Through => Length(Source => Member_Name));
         end if;
         Inventory_Container.Append
           (Container => Ship.Cargo,
            New_Item =>
              (Proto_Index => Corpse_Index, Amount => 1,
               Name => Member_Name & "'s corpse", Durability => 100,
               Price => 0));
      end if;
      Delete_Member(Member_Index => Member_Index, Ship => Ship);
      Reduce_Morale_Loop :
      for I in Ship.Crew.Iterate loop
         Update_Morale
           (Ship => Ship,
            Member_Index => Crew_Container.To_Index(Position => I),
            Value => Get_Random(Min => -25, Max => -10));
      end loop Reduce_Morale_Loop;
   end Death;

   procedure Delete_Member
     (Member_Index: Crew_Container.Extended_Index; Ship: in out Ship_Record) is
      Temp_Value: Integer;
   begin
      Ship.Crew.Delete(Index => Member_Index);
      Module_Loop :
      for Module of Ship.Modules loop
         Owners_Loop :
         for Owner of Module.Owner loop
            if Owner = Member_Index then
               Owner := 0;
            elsif Owner > Member_Index then
               Owner := Owner - 1;
            end if;
         end loop Owners_Loop;
      end loop Module_Loop;
      if Ship = Player_Ship then
         Delete_Missions_Loop :
         for I in
           Accepted_Missions.First_Index .. Accepted_Missions.Last_Index loop
            if Accepted_Missions(I).M_Type = PASSENGER
              and then Accepted_Missions(I).Data = Member_Index then
               Delete_Mission(Mission_Index => I);
               exit Delete_Missions_Loop;
            end if;
         end loop Delete_Missions_Loop;
         Update_Missions_Loop :
         for Mission of Accepted_Missions loop
            if Mission.M_Type = PASSENGER
              and then Mission.Data > Member_Index then
               Temp_Value := Mission.Data;
               Temp_Value := Temp_Value - 1;
               Mission.Data := Temp_Value;
            end if;
         end loop Update_Missions_Loop;
      end if;
   end Delete_Member;

   function Find_Member
     (Order: Crew_Orders; Crew: Crew_Container.Vector := Player_Ship.Crew)
      return Natural is
   begin
      Find_Member_Loop :
      for I in Crew.Iterate loop
         if Crew(I).Order = Order then
            return Crew_Container.To_Index(Position => I);
         end if;
      end loop Find_Member_Loop;
      return 0;
   end Find_Member;

   procedure Give_Orders
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Given_Order: Crew_Orders;
      Module_Index: Modules_Container.Extended_Index := 0;
      Check_Priorities: Boolean := True) is
      use Tiny_String;

      Member_Name: constant String :=
        To_String(Source => Ship.Crew(Member_Index).Name);
      Tools_Index: Inventory_Container.Extended_Index := 0;
      Required_Tool: Bounded_String;
      Tool_Quality: Items_Durability := Default_Item_Durability;
      Module_Index_2: Modules_Container.Extended_Index := 0;
   begin
      if Given_Order = Ship.Crew(Member_Index).Order then
         if Given_Order in CRAFT | GUNNER then
            Give_Orders_Modules_Loop :
            for I in Ship.Modules.Iterate loop
               if Modules_Container.To_Index(Position => I) = Module_Index then
                  Owners_Loop :
                  for Owner of Ship.Modules(I).Owner loop
                     if Owner = Member_Index then
                        return;
                     end if;
                  end loop Owners_Loop;
               end if;
            end loop Give_Orders_Modules_Loop;
         else
            return;
         end if;
      end if;
      if Given_Order /= REST and
        ((Ship.Crew(Member_Index).Morale(1) < 11 and
          Get_Random(Min => 1, Max => 100) < 50) or
         Ship.Crew(Member_Index).Loyalty < 20) then
         if Ship = Player_Ship then
            raise Crew_Order_Error
              with Member_Name & " refuses to execute order.";
         else
            return;
         end if;
      end if;
      if Given_Order = TRAIN
        and then Ship.Modules(Module_Index).Trained_Skill = 0 then
         raise Crew_Order_Error
           with Member_Name & " can't start training because " &
           To_String(Source => Ship.Modules(Module_Index).Name) &
           " isn't prepared.";
      end if;
      if Given_Order in PILOT | ENGINEER | UPGRADING | TALK then
         Give_Crew_Orders_Loop :
         for I in Ship.Crew.First_Index .. Ship.Crew.Last_Index loop
            if Ship.Crew(I).Order = Given_Order then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => I, Given_Order => REST,
                  Module_Index => 0, Check_Priorities => False);
               exit Give_Crew_Orders_Loop;
            end if;
         end loop Give_Crew_Orders_Loop;
      elsif (Given_Order in GUNNER | CRAFT | TRAIN) or
        (Given_Order = HEAL and Module_Index > 0) then
         Find_Free_Position_Block :
         declare
            Free_Position: Boolean := False;
         begin
            Free_Position_Loop :
            for Owner of Ship.Modules(Module_Index).Owner loop
               if Owner = 0 then
                  Free_Position := True;
                  exit Free_Position_Loop;
               end if;
            end loop Free_Position_Loop;
            if not Free_Position then
               Give_Orders
                 (Ship => Player_Ship,
                  Member_Index => Ship.Modules(Module_Index).Owner(1),
                  Given_Order => REST, Module_Index => 0,
                  Check_Priorities => False);
            end if;
         end Find_Free_Position_Block;
      end if;
      if Module_Index = 0 and (Given_Order in PILOT | ENGINEER | REST) then
         Find_Order_Module_Block :
         declare
            M_Type: constant Module_Type :=
              (case Given_Order is when PILOT => COCKPIT,
                 when ENGINEER => ENGINE, when REST => CABIN,
                 when others => ENGINE);
         begin
            Modules_Loop :
            for I in Ship.Modules.Iterate loop
               if M_Type /= CABIN then
                  if BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Ship.Modules(I).Proto_Index)
                      .M_Type =
                    M_Type and
                    Ship.Modules(I).Durability > 0 then
                     if Ship.Modules(I).Owner(1) /= 0 then
                        Give_Orders
                          (Ship => Player_Ship,
                           Member_Index => Ship.Modules(I).Owner(1),
                           Given_Order => REST, Module_Index => 0,
                           Check_Priorities => False);
                     end if;
                     Module_Index_2 :=
                       Modules_Container.To_Index(Position => I);
                     exit Modules_Loop;
                  end if;
               else
                  if Ship.Modules(I).M_Type = CABIN and
                    Ship.Modules(I).Durability > 0 then
                     Cabin_Owners_Loop :
                     for Owner of Ship.Modules(I).Owner loop
                        if Member_Index = Owner then
                           Module_Index_2 :=
                             Modules_Container.To_Index(Position => I);
                           exit Modules_Loop;
                        end if;
                     end loop Cabin_Owners_Loop;
                  end if;
               end if;
            end loop Modules_Loop;
         end Find_Order_Module_Block;
      else
         Module_Index_2 := Module_Index;
      end if;
      if Module_Index_2 = 0 and Ship = Player_Ship then
         case Given_Order is
            when PILOT =>
               raise Crew_Order_Error
                 with Member_Name &
                 " can't start piloting because the cockpit is destroyed or you don't have cockpit.";
            when ENGINEER =>
               raise Crew_Order_Error
                 with Member_Name &
                 " can't start engineer's duty because all of the engines are destroyed or you don't have engine.";
            when GUNNER =>
               raise Crew_Order_Error
                 with Member_Name &
                 " can't start operating gun because all of the guns are destroyed or you don't have any installed.";
            when REST =>
               Modules_2_Loop :
               for Module of Ship.Modules loop
                  if Module.M_Type = CABIN and Module.Durability > 0 then
                     Owners_2_Loop :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           Owner := Member_Index;
                           Add_Message
                             (Message =>
                                Member_Name & " takes " &
                                To_String(Source => Module.Name) &
                                " as their own cabin.",
                              M_Type => OTHERMESSAGE);
                           exit Modules_2_Loop;
                        end if;
                     end loop Owners_2_Loop;
                  end if;
               end loop Modules_2_Loop;
            when others =>
               null;
         end case;
      end if;
      Modules_3_Loop :
      for Module of Ship.Modules loop
         if Module.M_Type /= CABIN then
            Owners_3_Loop :
            for Owner of Module.Owner loop
               if Owner = Member_Index then
                  Owner := 0;
                  exit Modules_3_Loop;
               end if;
            end loop Owners_3_Loop;
         end if;
      end loop Modules_3_Loop;
      if Tools_Index > 0 and
        Ship.Crew(Member_Index).Equipment(TOOL) /= Tools_Index then
         UpdateInventory
           (MemberIndex => Member_Index, Amount => 1,
            ProtoIndex =>
              Inventory_Container.Element
                (Container => Ship.Cargo, Index => Tools_Index)
                .Proto_Index,
            Durability =>
              Inventory_Container.Element
                (Container => Ship.Cargo, Index => Tools_Index)
                .Durability,
            Ship => Ship);
         Update_Cargo(Ship => Ship, Amount => -1, Cargo_Index => Tools_Index);
         Ship.Crew(Member_Index).Equipment(TOOL) :=
           Find_Item
             (Inventory => Ship.Crew(Member_Index).Inventory,
              Item_Type => Required_Tool);
      end if;
      Tools_Index := Ship.Crew(Member_Index).Equipment(TOOL);
      if Tools_Index > 0
        and then
          Items_List
            (Inventory_Container.Element
               (Container => Ship.Crew(Member_Index).Inventory,
                Index => Tools_Index)
               .Proto_Index)
            .I_Type /=
          Required_Tool then
         Update_Cargo
           (Ship => Ship,
            Proto_Index =>
              Inventory_Container.Element
                (Container => Ship.Crew(Member_Index).Inventory,
                 Index => Tools_Index)
                .Proto_Index,
            Amount => 1,
            Durability =>
              Inventory_Container.Element
                (Container => Ship.Crew(Member_Index).Inventory,
                 Index => Tools_Index)
                .Durability);
         UpdateInventory
           (MemberIndex => Member_Index, Amount => -1,
            InventoryIndex => Tools_Index, Ship => Ship);
         Tools_Index := 0;
      end if;
      if Given_Order in UPGRADING | REPAIR | CLEAN |
            TRAIN then -- Check for tools
         if Given_Order = CLEAN then
            Required_Tool := Cleaning_Tools;
         elsif Given_Order = TRAIN then
            Required_Tool :=
                        SkillsData_Container.Element
                          (Container => Skills_List,
                           Index => Ship.Modules(Module_Index).Trained_Skill)
                          .Tool;
            Tool_Quality :=
              Get_Training_Tool_Quality
                (Member_Index => Member_Index,
                 Skill_Index =>
                   Natural(Ship.Modules(Module_Index).Trained_Skill));
         else
            Required_Tool := Repair_Tools;
         end if;
         if Required_Tool /= Null_Bounded_String then
            if Tools_Index = 0 then
               Tools_Index :=
                 Find_Item
                   (Inventory => Ship.Cargo, Item_Type => Required_Tool,
                    Quality => Tool_Quality);
               if Tools_Index = 0 then
                  Tools_Index :=
                    Find_Item
                      (Inventory => Ship.Crew(Member_Index).Inventory,
                       Item_Type => Required_Tool, Quality => Tool_Quality);
                  if Tools_Index > 0 then
                     Ship.Crew(Member_Index).Equipment(TOOL) := Tools_Index;
                  end if;
               else
                  Ship.Crew(Member_Index).Equipment(TOOL) := 0;
               end if;
            end if;
            if Tools_Index = 0 then
               case Given_Order is
                  when REPAIR =>
                     raise Crew_Order_Error
                       with Member_Name &
                       " can't start repairing ship because you don't have the proper tools.";
                  when CLEAN =>
                     raise Crew_Order_Error
                       with Member_Name &
                       " can't start cleaning ship because you don't have any cleaning tools.";
                  when UPGRADING =>
                     raise Crew_Order_Error
                       with Member_Name &
                       " can't start upgrading module because you don't have the proper tools.";
                  when TRAIN =>
                     raise Crew_Order_Error
                       with Member_Name &
                       " can't start training because you don't have the proper tools.";
                  when others =>
                     return;
               end case;
            end if;
         end if;
      end if;
      if Given_Order = REST then
         Ship.Crew(Member_Index).Previous_Order := REST;
         if Ship.Crew(Member_Index).Order in REPAIR | CLEAN | UPGRADING |
               TRAIN then
            Tools_Index := Ship.Crew(Member_Index).Equipment(TOOL);
            if Tools_Index > 0 then
               Update_Cargo
                 (Ship => Ship,
                  Proto_Index =>
                    Inventory_Container.Element
                      (Container => Ship.Crew(Member_Index).Inventory,
                       Index => Tools_Index)
                      .Proto_Index,
                  Amount => 1,
                  Durability =>
                    Inventory_Container.Element
                      (Container => Ship.Crew(Member_Index).Inventory,
                       Index => Tools_Index)
                      .Durability);
               UpdateInventory
                 (MemberIndex => Member_Index, Amount => -1,
                  InventoryIndex => Tools_Index, Ship => Ship);
            end if;
         end if;
      end if;
      if Ship = Player_Ship then
         case Given_Order is
            when PILOT =>
               Add_Message
                 (Message => Member_Name & " starts piloting.",
                  M_Type => ORDERMESSAGE);
               Ship.Modules(Module_Index_2).Owner(1) := Member_Index;
            when ENGINEER =>
               Add_Message
                 (Message => Member_Name & " starts engineer's duty.",
                  M_Type => ORDERMESSAGE);
            when GUNNER =>
               Add_Message
                 (Message => Member_Name & " starts operating gun.",
                  M_Type => ORDERMESSAGE);
               Ship.Modules(Module_Index_2).Owner(1) := Member_Index;
            when REST =>
               Add_Message
                 (Message => Member_Name & " is going on a break.",
                  M_Type => ORDERMESSAGE);
            when REPAIR =>
               Add_Message
                 (Message => Member_Name & " starts repairing ship.",
                  M_Type => ORDERMESSAGE);
            when CRAFT =>
               Add_Message
                 (Message => Member_Name & " starts manufacturing.",
                  M_Type => ORDERMESSAGE);
               Find_Owner_Loop :
               for Owner of Ship.Modules(Module_Index_2).Owner loop
                  if Owner = 0 then
                     Owner := Member_Index;
                     exit Find_Owner_Loop;
                  end if;
               end loop Find_Owner_Loop;
            when UPGRADING =>
               Add_Message
                 (Message =>
                    Member_Name & " starts upgrading " &
                    To_String
                      (Source => Ship.Modules(Ship.Upgrade_Module).Name) &
                    ".",
                  M_Type => ORDERMESSAGE);
            when TALK =>
               Add_Message
                 (Message =>
                    Member_Name & " is now assigned to talking in bases.",
                  M_Type => ORDERMESSAGE);
            when HEAL =>
               Add_Message
                 (Message =>
                    Member_Name & " starts healing wounded crew members.",
                  M_Type => ORDERMESSAGE);
               if Module_Index > 0 then
                  Find_Owner_2_Loop :
                  for Owner of Ship.Modules(Module_Index).Owner loop
                     if Owner = 0 then
                        Owner := Member_Index;
                        exit Find_Owner_2_Loop;
                     end if;
                  end loop Find_Owner_2_Loop;
               end if;
            when CLEAN =>
               Add_Message
                 (Message => Member_Name & " starts cleaning ship.",
                  M_Type => ORDERMESSAGE);
            when BOARDING =>
               Add_Message
                 (Message => Member_Name & " starts boarding the enemy ship.",
                  M_Type => ORDERMESSAGE);
            when DEFEND =>
               Add_Message
                 (Message => Member_Name & " starts defending the ship.",
                  M_Type => ORDERMESSAGE);
            when TRAIN =>
               Add_Message
                 (Message => Member_Name & " starts personal training.",
                  M_Type => ORDERMESSAGE);
               Find_Owner_3_Loop :
               for Owner of Ship.Modules(Module_Index_2).Owner loop
                  if Owner = 0 then
                     Owner := Member_Index;
                     exit Find_Owner_3_Loop;
                  end if;
               end loop Find_Owner_3_Loop;
         end case;
      end if;
      Ship.Crew(Member_Index).Order := Given_Order;
      Ship.Crew(Member_Index).Order_Time := 15;
      if Given_Order /= REST then
         Update_Morale
           (Ship => Ship, Member_Index => Member_Index, Value => -1);
      end if;
      if Check_Priorities then
         Update_Orders(Ship => Ship);
      end if;
   exception
      when An_Exception : Crew_No_Space_Error =>
         if Ship = Player_Ship then
            raise Crew_Order_Error with Exception_Message(X => An_Exception);
         else
            return;
         end if;
   end Give_Orders;

   procedure Update_Orders
     (Ship: in out Ship_Record; Combat: Boolean := False) is
      use Tiny_String;

      Have_Pilot, Have_Engineer, Have_Upgrade, Have_Trader, Need_Clean,
      Need_Repairs, Need_Gunners, Need_Crafters, Can_Heal,
      Need_Trader: Boolean := False;
      Event_Index: constant Events_Container.Extended_Index :=
        Sky_Map(Ship.Sky_X, Ship.Sky_Y).Event_Index;
      function Update_Position
        (Order: Crew_Orders; Max_Priority: Boolean := True) return Boolean is
         Order_Index: Natural := 0;
         Member_Index: Crew_Container.Extended_Index := 0;
         Module_Index: Modules_Container.Extended_Index := 0;
      begin
         Order_Index :=
           (if Crew_Orders'Pos(Order) < Crew_Orders'Pos(DEFEND) then
              Crew_Orders'Pos(Order) + 1
            else Crew_Orders'Pos(Order));
         if Max_Priority then
            Find_Member_Max_Priority_Loop :
            for I in Ship.Crew.Iterate loop
               if Ship.Crew(I).Orders(Order_Index) = 2 and
                 Ship.Crew(I).Order /= Order and
                 Ship.Crew(I).Previous_Order /= Order then
                  Member_Index := Crew_Container.To_Index(Position => I);
                  exit Find_Member_Max_Priority_Loop;
               end if;
            end loop Find_Member_Max_Priority_Loop;
         else
            Find_Member_Priority_Loop :
            for I in Ship.Crew.Iterate loop
               if Ship.Crew(I).Orders(Order_Index) = 1 and
                 Ship.Crew(I).Order = REST and
                 Ship.Crew(I).Previous_Order = REST then
                  Member_Index := Crew_Container.To_Index(Position => I);
                  exit Find_Member_Priority_Loop;
               end if;
            end loop Find_Member_Priority_Loop;
         end if;
         if Member_Index = 0 then
            return False;
         end if;
         if Order in GUNNER | CRAFT | HEAL | PILOT | ENGINEER | TRAIN then
            Find_Module_Index_Loop :
            for I in Ship.Modules.Iterate loop
               if Ship.Modules(I).Durability > 0 then
                  case Ship.Modules(I).M_Type is
                     when GUN =>
                        if Order = GUNNER and Ship.Modules(I).Owner(1) = 0 then
                           Module_Index :=
                             Modules_Container.To_Index(Position => I);
                           exit Find_Module_Index_Loop;
                        end if;
                     when WORKSHOP =>
                        if Order = CRAFT and
                          Ship.Modules(I).Crafting_Index /=
                            Null_Bounded_String then
                           Find_Empty_Workplace_Loop :
                           for Owner of Ship.Modules(I).Owner loop
                              if Owner = 0 then
                                 Module_Index :=
                                   Modules_Container.To_Index(Position => I);
                                 exit Find_Empty_Workplace_Loop;
                              end if;
                           end loop Find_Empty_Workplace_Loop;
                           exit Find_Module_Index_Loop when Module_Index > 0;
                        end if;
                     when MEDICAL_ROOM =>
                        if Order = HEAL then
                           Find_Empty_Medical_Loop :
                           for Owner of Ship.Modules(I).Owner loop
                              if Owner = 0 then
                                 Module_Index :=
                                   Modules_Container.To_Index(Position => I);
                                 exit Find_Empty_Medical_Loop;
                              end if;
                           end loop Find_Empty_Medical_Loop;
                           exit Find_Module_Index_Loop when Module_Index > 0;
                        end if;
                     when COCKPIT =>
                        if Order = PILOT then
                           Module_Index :=
                             Modules_Container.To_Index(Position => I);
                           exit Find_Module_Index_Loop;
                        end if;
                     when ENGINE =>
                        if Order = ENGINEER then
                           Module_Index :=
                             Modules_Container.To_Index(Position => I);
                           exit Find_Module_Index_Loop;
                        end if;
                     when TRAINING_ROOM =>
                        if Order = TRAIN and
                          Ship.Modules(I).Trained_Skill > 0 then
                           Find_Empty_Training_Loop :
                           for Owner of Ship.Modules(I).Owner loop
                              if Owner = 0 then
                                 Module_Index :=
                                   Modules_Container.To_Index(Position => I);
                                 exit Find_Empty_Training_Loop;
                              end if;
                           end loop Find_Empty_Training_Loop;
                           exit Find_Module_Index_Loop when Module_Index > 0;
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
            end loop Find_Module_Index_Loop;
            if Module_Index = 0 then
               return False;
            end if;
         end if;
         if Ship.Crew(Member_Index).Order /= REST then
            Give_Orders
              (Ship => Ship, Member_Index => Member_Index, Given_Order => REST,
               Module_Index => 0, Check_Priorities => False);
         end if;
         Give_Orders
           (Ship => Ship, Member_Index => Member_Index, Given_Order => Order,
            Module_Index => Module_Index);
         return True;
      exception
         when An_Exception : Crew_Order_Error | Crew_No_Space_Error =>
            if Ship = Player_Ship then
               Add_Message
                 (Message => Exception_Message(X => An_Exception),
                  M_Type => ORDERMESSAGE, Color => RED);
            end if;
            return False;
      end Update_Position;
   begin
      Crew_Members_Loop :
      for Member of Ship.Crew loop
         case Member.Order is
            when PILOT =>
               Have_Pilot := True;
            when ENGINEER =>
               Have_Engineer := True;
            when UPGRADING =>
               Have_Upgrade := True;
            when TALK =>
               Have_Trader := True;
            when others =>
               null;
         end case;
         if Member.Health < 100 then
            if Find_Item
                (Inventory => Ship.Cargo,
                 Item_Type => Factions_List(Member.Faction).Healing_Tools) >
              0 then
               Can_Heal := True;
            end if;
         end if;
      end loop Crew_Members_Loop;
      Modules_Need_Loop :
      for Module of Ship.Modules loop
         if Module.Durability > 0 then
            case Module.M_Type is
               when GUN =>
                  if Module.Owner(1) = 0 and not Need_Gunners then
                     Need_Gunners := True;
                  end if;
               when WORKSHOP =>
                  if Module.Crafting_Index /= Null_Bounded_String and
                    not Need_Crafters then
                     Find_Empty_Crafting_Loop :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           Need_Crafters := True;
                           exit Find_Empty_Crafting_Loop;
                        end if;
                     end loop Find_Empty_Crafting_Loop;
                  end if;
               when CABIN =>
                  if Module.Cleanliness < Module.Quality then
                     Need_Clean := True;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         if Module.Durability < Module.Max_Durability and not Need_Repairs then
            Find_Need_Repairs_Loop :
            for Item of Ship.Cargo loop
               if To_String(Source => Items_List(Item.Proto_Index).I_Type) =
                 To_String
                   (Source =>
                      BaseModules_Container.Element
                        (Container => Modules_List,
                         Index => Module.Proto_Index)
                        .Repair_Material) then
                  Need_Repairs := True;
                  exit Find_Need_Repairs_Loop;
               end if;
            end loop Find_Need_Repairs_Loop;
         end if;
      end loop Modules_Need_Loop;
      if Sky_Map(Ship.Sky_X, Ship.Sky_Y).Base_Index > 0 then
         Need_Trader := True;
      end if;
      if (not Need_Trader and Event_Index > 0)
        and then
        (Events_List(Event_Index).E_Type in TRADER | FRIENDLYSHIP) then
         Need_Trader := True;
      end if;
      if not Have_Pilot and then Update_Position(Order => PILOT) then
         Update_Orders(Ship => Ship);
      end if;
      if not Have_Engineer and then Update_Position(Order => ENGINEER) then
         Update_Orders(Ship => Ship);
      end if;
      if Need_Gunners and then Update_Position(Order => GUNNER) then
         Update_Orders(Ship => Ship);
      end if;
      if Need_Crafters and then Update_Position(Order => CRAFT) then
         Update_Orders(Ship => Ship);
      end if;
      if not Have_Upgrade and Ship.Upgrade_Module > 0 and
        Find_Item(Inventory => Ship.Cargo, Item_Type => Repair_Tools) > 0 then
         if Find_Item
             (Inventory => Ship.Cargo,
              Item_Type =>
                          BaseModules_Container.Element
                            (Container => Modules_List,
                             Index =>
                               Ship.Modules(Ship.Upgrade_Module).Proto_Index)
                            .Repair_Material) >
           0
           and then Update_Position(Order => UPGRADING) then
            Update_Orders(Ship => Ship);
         end if;
      end if;
      if (not Have_Trader and Need_Trader)
        and then Update_Position(Order => TALK) then
         Update_Orders(Ship => Ship);
      end if;
      if
        (Need_Clean and
         Find_Item(Inventory => Ship.Cargo, Item_Type => Cleaning_Tools) > 0)
        and then Update_Position(Order => CLEAN) then
         Update_Orders(Ship => Ship);
      end if;
      if Can_Heal and then Update_Position(Order => HEAL) then
         Update_Orders(Ship => Ship);
      end if;
      if
        (Need_Repairs and
         Find_Item(Inventory => Ship.Cargo, Item_Type => Repair_Tools) > 0)
        and then Update_Position(Order => REPAIR) then
         Update_Orders(Ship => Ship);
      end if;
      if Combat then
         if Update_Position(Order => DEFEND) then
            Update_Orders(Ship => Ship);
         end if;
         if Update_Position(Order => BOARDING) then
            Update_Orders(Ship => Ship);
         end if;
      end if;
      if Update_Position(Order => TRAIN) then
         Update_Orders(Ship => Ship);
      end if;
      if not Have_Pilot
        and then Update_Position(Order => PILOT, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if not Have_Engineer
        and then Update_Position(Order => ENGINEER, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if Need_Gunners
        and then Update_Position(Order => GUNNER, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if Need_Crafters
        and then Update_Position(Order => CRAFT, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if not Have_Upgrade and Ship.Upgrade_Module > 0 and
        Find_Item(Inventory => Ship.Cargo, Item_Type => Repair_Tools) > 0 then
         if Find_Item
             (Inventory => Ship.Cargo,
              Item_Type =>
                          BaseModules_Container.Element
                            (Container => Modules_List,
                             Index =>
                               Ship.Modules(Ship.Upgrade_Module).Proto_Index)
                            .Repair_Material) >
           0
           and then Update_Position
             (Order => UPGRADING, Max_Priority => False) then
            Update_Orders(Ship => Ship);
         end if;
      end if;
      if (not Have_Trader and Sky_Map(Ship.Sky_X, Ship.Sky_Y).Base_Index > 0)
        and then Update_Position(Order => TALK, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if
        (Need_Clean and
         Find_Item(Inventory => Ship.Cargo, Item_Type => Cleaning_Tools) > 0)
        and then Update_Position(Order => CLEAN, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if Can_Heal
        and then Update_Position(Order => HEAL, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if
        (Need_Repairs and
         Find_Item(Inventory => Ship.Cargo, Item_Type => Repair_Tools) > 0)
        and then Update_Position(Order => REPAIR, Max_Priority => False) then
         Update_Orders(Ship => Ship);
      end if;
      if Combat then
         if Update_Position(Order => DEFEND, Max_Priority => False) then
            Update_Orders(Ship => Ship);
         end if;
         if Update_Position(Order => BOARDING, Max_Priority => False) then
            Update_Orders(Ship => Ship);
         end if;
      end if;
      if Update_Position(Order => TRAIN, Max_Priority => False) then
         Update_Orders(Ship => Ship, Combat => False);
      end if;
   end Update_Orders;

   procedure Update_Morale
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Value: Integer) is
      New_Morale, New_Loyalty, New_Value: Integer;
      Faction_Index: constant Tiny_String.Bounded_String :=
        Ship.Crew(Member_Index).Faction;
   begin
      if Factions_List(Faction_Index).Flags.Contains
          (Item => To_Unbounded_String(Source => "nomorale")) then
         return;
      end if;
      New_Value := Value;
      if Factions_List(Faction_Index).Flags.Contains
          (Item => To_Unbounded_String(Source => "fanaticism")) then
         if Value > 0 then
            New_Value := Value * 5;
         else
            New_Value := Value / 10;
            if New_Value = 0
              and then Get_Random(Min => 1, Max => 10) <= abs (Value) then
               New_Value := -1;
            end if;
            if New_Value = 0 then
               return;
            end if;
         end if;
      end if;
      New_Value := Ship.Crew(Member_Index).Morale(2) + New_Value;
      New_Morale := Ship.Crew(Member_Index).Morale(1);
      Raise_Morale_Loop :
      while New_Value >= 5 loop
         New_Value := New_Value - 5;
         New_Morale := New_Morale + 1;
      end loop Raise_Morale_Loop;
      Lower_Morale_Loop :
      while New_Value < 0 loop
         New_Value := New_Value + 5;
         New_Morale := New_Morale - 1;
      end loop Lower_Morale_Loop;
      if New_Morale > 100 then
         New_Morale := 100;
      elsif New_Morale < 0 then
         New_Morale := 0;
      end if;
      Ship.Crew(Member_Index).Morale := (1 => New_Morale, 2 => New_Value);
      if Ship = Player_Ship and Member_Index = 1 then
         return;
      end if;
      New_Loyalty := Ship.Crew(Member_Index).Loyalty;
      if New_Morale > 75 and New_Loyalty < 100 then
         New_Loyalty := New_Loyalty + 1;
      end if;
      if New_Morale < 25 and New_Loyalty > 0 then
         New_Loyalty := New_Loyalty - Get_Random(Min => 5, Max => 10);
      end if;
      if New_Loyalty > 100 then
         New_Loyalty := 100;
      elsif New_Loyalty < 0 then
         New_Loyalty := 0;
      end if;
      Ship.Crew(Member_Index).Loyalty := New_Loyalty;
   end Update_Morale;

end Ships.Crew;
