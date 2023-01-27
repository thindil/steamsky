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

with Interfaces.C.Strings;
with Crafts;
with Messages; use Messages;
with HallOfFame; use HallOfFame;
with Events; use Events;
with Utils; use Utils;
with Missions; use Missions;

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
      use Interfaces.C;
      use Interfaces.C.Strings;

      Message: chars_ptr;
      function Give_Ada_Orders
        (Get_Player_Ship, M_Index, G_Order, Mod_Index, Priorities: Natural)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "giveAdaOrders";
   begin
      Set_Ship_In_Nim(Ship => Ship);
      Message :=
        Give_Ada_Orders
          (Get_Player_Ship => (if Ship = Player_Ship then 1 else 0),
           M_Index => Member_Index, G_Order => Crew_Orders'Pos(Given_Order),
           Mod_Index => Module_Index,
           Priorities => (if Check_Priorities then 1 else 0));
      if Strlen(Item => Message) > 0 then
         if Ship = Player_Ship then
            raise Crew_Order_Error with Value(Item => Message);
         else
            return;
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Ship);
   end Give_Orders;

   procedure Update_Orders
     (Ship: in out Ship_Record; Combat: Boolean := False) is
      procedure Update_Ada_Orders(Get_Player_Ship, Comb: Natural) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaOrders";
   begin
      Set_Ship_In_Nim(Ship => Ship);
      Update_Ada_Orders
        (Get_Player_Ship => (if Ship = Player_Ship then 1 else 0),
         Comb => (if Combat then 1 else 0));
      Get_Ship_From_Nim(Ship => Ship);
   end Update_Orders;

   procedure Update_Morale
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Value: Integer) is
      procedure Update_Ada_Morale
        (Is_Player_Ship, M_Index, N_Value: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMorale";
   begin
      Get_Ada_Crew(Ship => Ship);
      Update_Ada_Morale
        (Is_Player_Ship => (if Ship = Player_Ship then 1 else 0),
         M_Index => Member_Index, N_Value => Value);
      Set_Ada_Crew(Ship => Ship);
   end Update_Morale;

   function Get_Current_Order
     (Member_Index: Positive) return Unbounded_String is
      use Tiny_String;
      use Crafts;

      Member: constant Member_Data := Player_Ship.Crew(Member_Index);
      Member_Info: Unbounded_String := Null_Unbounded_String;
      function Get_Module_Name(M_Type: Module_Type_2) return String is
      begin
         Modules_Loop :
         for Module of Player_Ship.Modules loop
            if Module.M_Type = M_Type then
               Owners_Loop :
               for Owner of Module.Owner loop
                  if Owner = Member_Index then
                     return To_String(Source => Module.Name);
                  end if;
               end loop Owners_Loop;
            end if;
         end loop Modules_Loop;
         return "";
      end Get_Module_Name;
   begin
      case Member.Order is
         when PILOT =>
            Append(Source => Member_Info, New_Item => "Piloting the ship");
         when ENGINEER =>
            Append(Source => Member_Info, New_Item => "Engineering the ship");
         when GUNNER =>
            Append
              (Source => Member_Info,
               New_Item => "Operating " & Get_Module_Name(M_Type => GUN));
         when REPAIR =>
            Append(Source => Member_Info, New_Item => "Repairing the ship");
         when CRAFT =>
            Find_Workshop_Loop :
            for I in Player_Ship.Modules.Iterate loop
               if Player_Ship.Modules(I).M_Type = WORKSHOP then
                  Find_Owner_Loop :
                  for Owner of Player_Ship.Modules(I).Owner loop
                     if Owner = Member_Index then
                        Append
                          (Source => Member_Info,
                           New_Item =>
                             Get_Workshop_Recipe_Name
                               (Workshop =>
                                  Modules_Container.To_Index(Position => I)) &
                             " in " &
                             To_String(Source => Player_Ship.Modules(I).Name));
                        exit Find_Workshop_Loop;
                     end if;
                  end loop Find_Owner_Loop;
               end if;
            end loop Find_Workshop_Loop;
         when UPGRADING =>
            Append
              (Source => Member_Info,
               New_Item =>
                 "Upgrading " &
                 To_String
                   (Source =>
                      Player_Ship.Modules(Player_Ship.Upgrade_Module).Name));
         when TALK =>
            Append(Source => Member_Info, New_Item => "Talking with others");
         when HEAL =>
            Append
              (Source => Member_Info,
               New_Item =>
                 "Healing the wounded in " &
                 Get_Module_Name(M_Type => MEDICAL_ROOM));
         when CLEAN =>
            Append(Source => Member_Info, New_Item => "Cleaning the ship");
         when REST =>
            Append
              (Source => Member_Info,
               New_Item =>
                 "Resting in " & Get_Module_Name(M_Type => CABIN) &
                 ", no order");
         when DEFEND =>
            Append(Source => Member_Info, New_Item => "Defending the ship");
         when BOARDING =>
            Append
              (Source => Member_Info, New_Item => "Boarding the enemy's ship");
         when TRAIN =>
            Find_Training_Room_Loop :
            for I in Player_Ship.Modules.Iterate loop
               if Player_Ship.Modules(I).M_Type = TRAINING_ROOM then
                  Find_Traineer_Loop :
                  for Owner of Player_Ship.Modules(I).Owner loop
                     if Owner = Member_Index then
                        Append
                          (Source => Member_Info,
                           New_Item =>
                             "Training " &
                             To_String
                               (Source =>
                                  SkillsData_Container.Element
                                    (Container => Skills_List,
                                     Index =>
                                       Player_Ship.Modules(I).Trained_Skill)
                                    .Name) &
                             " in " &
                             To_String(Source => Player_Ship.Modules(I).Name));
                        exit Find_Training_Room_Loop;
                     end if;
                  end loop Find_Traineer_Loop;
               end if;
            end loop Find_Training_Room_Loop;
      end case;
      return Member_Info;
   end Get_Current_Order;

end Ships.Crew;
