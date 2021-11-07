--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ships; use Ships;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Utils; use Utils;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Maps; use Maps;
with Crew.Inventory; use Crew.Inventory;
with Combat; use Combat;
with Factions; use Factions;
with Bases; use Bases;
with Careers; use Careers;
with Config; use Config;

package body Crew is

   procedure Gain_Exp(Amount: Natural; Skill_Number, Crew_Index: Positive) is
      use Tiny_String;

      Skill_Exp, Attribute_Exp, Attribute_Level, New_Amount: Natural := 0;
      Attribute_Index: constant Skills_Container.Extended_Index :=
        Natural
          (SkillsData_Container.Element
             (Container => Skills_List, Index => Skill_Number)
             .Attribute);
      Skill_Index: Skills_Container.Extended_Index := 0;
      Skill_Level: Skill_Range := 0;
      procedure Gain_Exp_In_Attribute(Attribute: Positive) is
         Attribute_To_Check: Mob_Attribute_Record :=
           Player_Ship.Crew(Crew_Index).Attributes(Attribute);
      begin
         if Attribute_To_Check.Level = 50 then
            return;
         end if;
         Attribute_Exp := Attribute_To_Check.Experience + New_Amount;
         Attribute_Level := Attribute_To_Check.Level;
         if Attribute_Exp >= (Attribute_Level * 250) then
            Attribute_Exp := Attribute_Exp - (Attribute_Level * 250);
            Attribute_Level := Attribute_Level + 1;
         end if;
         Attribute_To_Check.Level := Attribute_Level;
         Attribute_To_Check.Experience := Attribute_Exp;
         Player_Ship.Crew(Crew_Index).Attributes(Attribute) :=
           Attribute_To_Check;
      end Gain_Exp_In_Attribute;
   begin
      New_Amount :=
        (if
           Careers_List(Player_Career).Skills.Contains
             (Item =>
                To_Unbounded_String
                  (Source =>
                     To_String
                       (Source =>
                          SkillsData_Container.Element
                            (Container => Skills_List, Index => Skill_Number)
                            .Name)))
         then Amount + (Amount / 2)
         else Amount);
      New_Amount :=
        Natural(Float(New_Amount) * Float(New_Game_Settings.Experience_Bonus));
      if New_Amount = 0 then
         return;
      end if;
      -- Gain experience in condition assigned attribute
      Gain_Exp_In_Attribute(Attribute => Positive(Condition_Index));
      -- Gain experience in associated attribute
      Gain_Exp_In_Attribute(Attribute => Attribute_Index);
      -- Gain experience in skill
      Experience_In_Skill_Loop :
      for I in Player_Ship.Crew(Crew_Index).Skills.Iterate loop
         if Player_Ship.Crew(Crew_Index).Skills(I).Index = Skill_Number then
            Skill_Index := Skills_Container.To_Index(Position => I);
            exit Experience_In_Skill_Loop;
         end if;
      end loop Experience_In_Skill_Loop;
      if Skill_Index > 0 then
         if Player_Ship.Crew(Crew_Index).Skills(Skill_Index).Level =
           Skill_Range'Last then
            return;
         end if;
         Skill_Level := Player_Ship.Crew(Crew_Index).Skills(Skill_Index).Level;
         Skill_Exp :=
           Player_Ship.Crew(Crew_Index).Skills(Skill_Index).Experience +
           New_Amount;
      end if;
      if Skill_Exp >= (Skill_Level * 25) then
         Skill_Exp := Skill_Exp - (Skill_Level * 25);
         Skill_Level := Skill_Level + 1;
      end if;
      if Skill_Index > 0 then
         Player_Ship.Crew(Crew_Index).Skills(Skill_Index).Level := Skill_Level;
         Player_Ship.Crew(Crew_Index).Skills(Skill_Index).Experience :=
           Skill_Exp;
      else
         Player_Ship.Crew(Crew_Index).Skills.Append
           (New_Item =>
              (Index => Skill_Number, Level => Skill_Level,
               Experience => Skill_Exp));
      end if;
   end Gain_Exp;

   function Generate_Member_Name
     (Gender: Character; Faction_Index: Unbounded_String)
      return Unbounded_String is
      New_Name: Unbounded_String := Null_Unbounded_String;
      Name_Type: constant NamesTypes := Factions_List(Faction_Index).NamesType;
   begin
      if Name_Type = Factions.ROBOTIC then
         return Generate_Robotic_Name;
      end if;
      if Gender = 'M' then
         New_Name :=
           Male_Syllables_Start
             (Get_Random
                (Min => Male_Syllables_Start.First_Index,
                 Max => Male_Syllables_Start.Last_Index)) &
           Male_Vocals
             (Get_Random
                (Min => Male_Vocals.First_Index,
                 Max => Male_Vocals.Last_Index));
         if Get_Random(Min => 1, Max => 100) < 36 then
            Append
              (Source => New_Name,
               New_Item =>
                 Male_Syllables_Middle
                   (Get_Random
                      (Min => Male_Syllables_Middle.First_Index,
                       Max => Male_Syllables_Middle.Last_Index)));
         end if;
         if Get_Random(Min => 1, Max => 100) < 11 then
            Append
              (Source => New_Name,
               New_Item =>
                 Male_Consonants
                   (Get_Random
                      (Min => Male_Consonants.First_Index,
                       Max => Male_Consonants.Last_Index)));
         end if;
         Append
           (Source => New_Name,
            New_Item =>
              Male_Syllables_End
                (Get_Random
                   (Min => Male_Syllables_End.First_Index,
                    Max => Male_Syllables_End.Last_Index)));
         return New_Name;
      end if;
      New_Name :=
        Female_Syllables_Start
          (Get_Random
             (Min => Female_Syllables_Start.First_Index,
              Max => Female_Syllables_Start.Last_Index)) &
        Female_Vocals
          (Get_Random
             (Min => Female_Vocals.First_Index,
              Max => Female_Vocals.Last_Index));
      if Get_Random(Min => 1, Max => 100) < 36 then
         Append
           (Source => New_Name,
            New_Item =>
              Female_Syllables_Middle
                (Get_Random
                   (Min => Female_Syllables_Middle.First_Index,
                    Max => Female_Syllables_Middle.Last_Index)));
      end if;
      if Get_Random(Min => 1, Max => 100) < 11 then
         Append
           (Source => New_Name,
            New_Item =>
              Female_Syllables_Middle
                (Get_Random
                   (Min => Female_Syllables_Middle.First_Index,
                    Max => Female_Syllables_Middle.Last_Index)));
      end if;
      Append
        (Source => New_Name,
         New_Item =>
           Female_Syllables_End
             (Get_Random
                (Min => Female_Syllables_End.First_Index,
                 Max => Female_Syllables_End.Last_Index)));
      return New_Name;
   end Generate_Member_Name;

   function Find_Cabin(Member_Index: Positive) return Natural is
   begin
      Find_Cabin_Loop :
      for I in Player_Ship.Modules.Iterate loop
         if Player_Ship.Modules(I).M_Type = CABIN then
            Check_Owner_Loop :
            for Owner of Player_Ship.Modules(I).Owner loop
               if Owner = Member_Index then
                  return Modules_Container.To_Index(Position => I);
               end if;
            end loop Check_Owner_Loop;
         end if;
      end loop Find_Cabin_Loop;
      return 0;
   end Find_Cabin;

   procedure Update_Crew
     (Minutes: Positive; Tired_Points: Natural; In_Combat: Boolean := False) is
      use Tiny_String;

      Tired_Level, Hunger_Level, Thirst_Level: Integer := 0;
      Health_Level: Integer := 100;
      Death_Reason: Unbounded_String;
      Tool_Index: Inventory_Container.Extended_Index;
      Cabin_Index: Modules_Container.Extended_Index;
      Times, Rest_Amount, I: Natural;
      Order_Time, Current_Minutes, Heal_Amount: Integer;
      Damage: Damage_Factor := 0.0;
      Need_Cleaning, Have_Medical_Room: Boolean := False;
      Skill_Index: Skills_Container.Extended_Index;
      function Consume(Item_Type: Unbounded_String) return Natural is
         Consume_Value: Natural;
         Item_Index: Inventory_Container.Extended_Index :=
           FindItem(Inventory => Player_Ship.Cargo, ItemType => Item_Type);
      begin
         if Item_Index > 0 then
            Consume_Value :=
              Items_List(Player_Ship.Cargo(Item_Index).ProtoIndex).Value(1);
            if Items_List(Player_Ship.Cargo(Item_Index).ProtoIndex).Value
                .Length >
              1
              and then
                Items_List(Player_Ship.Cargo(Item_Index).ProtoIndex).Value
                  (2) /=
                0 then
               UpdateMorale
                 (Ship => Player_Ship, MemberIndex => I,
                  Value =>
                    Items_List(Player_Ship.Cargo(Item_Index).ProtoIndex).Value
                      (2));
            end if;
            UpdateCargo
              (Ship => Player_Ship,
               ProtoIndex =>
                 Player_Ship.Cargo.Element(Index => Item_Index).ProtoIndex,
               Amount => -1);
            return Consume_Value;
         end if;
         Item_Index :=
           FindItem
             (Inventory => Player_Ship.Crew(I).Inventory,
              ItemType => Item_Type);
         if Item_Index > 0 then
            Consume_Value :=
              Items_List(Player_Ship.Crew(I).Inventory(Item_Index).ProtoIndex)
                .Value
                (1);
            if Items_List(Player_Ship.Cargo(Item_Index).ProtoIndex).Value(2) /=
              0 then
               UpdateMorale
                 (Ship => Player_Ship, MemberIndex => I,
                  Value =>
                    Items_List(Player_Ship.Cargo(Item_Index).ProtoIndex).Value
                      (2));
            end if;
            UpdateInventory
              (MemberIndex => I, Amount => -1, InventoryIndex => Item_Index);
            return Consume_Value;
         end if;
         return 0;
      end Consume;
      procedure Update_Member(Member: in out Member_Data) is
         Back_To_Work: Boolean := True;
         Consume_Result: Natural := 0;
         procedure Normalize_Stat
           (Stat: in out Integer; Max_Value: Positive := 100) is
         begin
            if Stat > Max_Value then
               Stat := Max_Value;
            elsif Stat < 0 then
               Stat := 0;
            end if;
         end Normalize_Stat;
      begin
         if Factions_List(Member.Faction).Flags.Contains
             (Item => To_Unbounded_String(Source => "nofatigue")) then
            Tired_Level := 0;
         end if;
         if Tired_Level = 0 and Member.Order = REST and
           Member.Previous_Order /= REST then
            if Member.Previous_Order not in REPAIR | CLEAN
              and then FindMember(Order => Member.Previous_Order) > 0 then
               Back_To_Work := False;
            end if;
            if Member.Previous_Order in GUNNER | CRAFT then
               Module_Loop :
               for Module of Player_Ship.Modules loop
                  if (Member.Previous_Order = GUNNER and Module.M_Type = GUN)
                    and then (Module.Owner(1) in I | 0) then
                     Back_To_Work := True;
                     Module.Owner(1) := I;
                     exit Module_Loop;
                  elsif
                    (Member.Previous_Order = CRAFT and
                     Module.M_Type = WORKSHOP)
                    and then Module.Crafting_Index /=
                      Null_Unbounded_String then
                     Module_Is_Owner_Loop :
                     for Owner of Module.Owner loop
                        if Owner = I then
                           Back_To_Work := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop Module_Is_Owner_Loop;
                     Module_Empty_Owner_Loop :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           Back_To_Work := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop Module_Empty_Owner_Loop;
                  end if;
               end loop Module_Loop;
            end if;
            if Back_To_Work then
               Member.Order := Member.Previous_Order;
               Member.Order_Time := 15;
               AddMessage
                 (Message =>
                    To_String(Source => Member.Name) &
                    " returns to work fully rested.",
                  MType => OrderMessage, Color => YELLOW);
               UpdateMorale(Ship => Player_Ship, MemberIndex => I, Value => 1);
            end if;
            Member.Previous_Order := REST;
         end if;
         if Tired_Level >
           (80 + Member.Attributes(Positive(Condition_Index)).Level) and
           Member.Order /= REST and not In_Combat then
            Member_Rest_Block :
            declare
               Can_Rest: Boolean := True;
            begin
               if Member.Order = BOARDING and HarpoonDuration = 0 and
                 Combat.Enemy.HarpoonDuration = 0 then
                  Can_Rest := False;
               end if;
               if Can_Rest then
                  Member.Previous_Order := Member.Order;
                  Member.Order := REST;
                  Member.Order_Time := 15;
                  if Member.Equipment(7) > 0 then
                     UpdateCargo
                       (Ship => Player_Ship,
                        ProtoIndex =>
                          Member.Inventory(Member.Equipment(7)).ProtoIndex,
                        Amount => 1,
                        Durability =>
                          Member.Inventory(Member.Equipment(7)).Durability);
                     UpdateInventory
                       (MemberIndex => I, Amount => -1,
                        InventoryIndex => Member.Equipment(7));
                     Member.Equipment(7) := 0;
                  end if;
                  AddMessage
                    (Message =>
                       To_String(Source => Member.Name) &
                       " is too tired to work, they're going to rest.",
                     MType => OrderMessage, Color => YELLOW);
                  if Find_Cabin(Member_Index => I) = 0 then
                     Modules_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN and Module.Durability > 0 then
                           Find_Cabin_Owner_Loop :
                           for Owner of Module.Owner loop
                              if Owner = 0 then
                                 Owner := I;
                                 AddMessage
                                   (Message =>
                                      To_String(Source => Member.Name) &
                                      " take " &
                                      To_String(Source => Module.Name) &
                                      " as own cabin.",
                                    MType => OtherMessage);
                                 exit Modules_Loop;
                              end if;
                           end loop Find_Cabin_Owner_Loop;
                        end if;
                     end loop Modules_Loop;
                  end if;
               else
                  AddMessage
                    (Message =>
                       To_String(Source => Member.Name) &
                       " is very tired but they can't go to rest.",
                     MType => OrderMessage, Color => RED);
                  UpdateMorale
                    (Ship => Player_Ship, MemberIndex => I,
                     Value => Get_Random(Min => -5, Max => -1));
               end if;
            end Member_Rest_Block;
         end if;
         Normalize_Stat(Stat => Tired_Level, Max_Value => 150);
         Member.Tired := Tired_Level;
         if Hunger_Level > 80 then
            Find_Food_Loop :
            for Food_Type of Factions_List(Member.Faction).FoodTypes loop
               Consume_Result := Consume(Item_Type => Food_Type);
               exit Find_Food_Loop when Consume_Result > 0;
            end loop Find_Food_Loop;
            Hunger_Level :=
              (if Hunger_Level - Consume_Result < Skill_Range'First then
                 Skill_Range'First
               else Hunger_Level - Consume_Result);
            if Consume_Result = 0 then
               AddMessage
                 (Message =>
                    To_String(Source => Member.Name) &
                    " is hungry, but they can't find anything to eat.",
                  MType => OtherMessage, Color => RED);
               UpdateMorale
                 (Ship => Player_Ship, MemberIndex => I,
                  Value => Get_Random(Min => -10, Max => -5));
            end if;
         end if;
         Normalize_Stat(Stat => Hunger_Level);
         Member.Hunger := Hunger_Level;
         if Thirst_Level > 40 then
            Find_Drink_Loop :
            for Drinks_Type of Factions_List(Member.Faction).DrinksTypes loop
               Consume_Result := Consume(Item_Type => Drinks_Type);
               exit Find_Drink_Loop when Consume_Result > 0;
            end loop Find_Drink_Loop;
            Thirst_Level :=
              (if Thirst_Level - Consume_Result < Skill_Range'First then
                 Skill_Range'First
               else Thirst_Level - Consume_Result);
            if Consume_Result = 0 then
               AddMessage
                 (Message =>
                    To_String(Source => Member.Name) &
                    " is thirsty, but they can't find anything to drink.",
                  MType => OtherMessage, Color => RED);
               UpdateMorale
                 (Ship => Player_Ship, MemberIndex => I,
                  Value => Get_Random(Min => -20, Max => -10));
            end if;
         end if;
         Normalize_Stat(Stat => Thirst_Level);
         Member.Thirst := Thirst_Level;
         Normalize_Stat(Stat => Health_Level);
         Member.Health := Health_Level;
         if Member.Order not in REPAIR | CRAFT | UPGRADING then
            Member.Order_Time := Order_Time;
         end if;
         if Member.Skills.Length = 0 then
            Member.Contract_Length := Member.Contract_Length - Minutes;
            if Member.Contract_Length < 0 then
               Member.Contract_Length := 0;
            end if;
         end if;
      end Update_Member;
   begin
      I := Player_Ship.Crew.First_Index;
      Update_Crew_Loop :
      while I <= Player_Ship.Crew.Last_Index loop
         Current_Minutes := Minutes;
         Order_Time := Player_Ship.Crew(I).Order_Time;
         Times := 0;
         Update_Current_Minutes_Loop :
         while Current_Minutes > 0 loop
            if Current_Minutes >= Order_Time then
               Current_Minutes := Current_Minutes - Order_Time;
               Times := Times + 1;
               Order_Time := 15;
            else
               Order_Time := Order_Time - Current_Minutes;
               Current_Minutes := 0;
            end if;
         end loop Update_Current_Minutes_Loop;
         Health_Level := Player_Ship.Crew(I).Health;
         Hunger_Level := Player_Ship.Crew(I).Hunger;
         Thirst_Level := Player_Ship.Crew(I).Thirst;
         Tired_Level := Player_Ship.Crew(I).Tired;
         if Times = 0 then
            goto End_Of_Loop;
         end if;
         if Player_Ship.Crew(I).Order = REST then
            Cabin_Index := Find_Cabin(Member_Index => I);
            Rest_Amount := 0;
            if Player_Ship.Crew(I).Tired > 0 then
               if Cabin_Index > 0 then
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(Player_Ship.Modules(Cabin_Index).Durability) /
                       Float(Player_Ship.Modules(Cabin_Index).Max_Durability));
                  Rest_Amount :=
                    Player_Ship.Modules(Cabin_Index).Cleanliness -
                    Natural
                      (Float(Player_Ship.Modules(Cabin_Index).Cleanliness) *
                       Float(Damage));
                  if Rest_Amount = 0 then
                     Rest_Amount := 1;
                  end if;
                  Tired_Level := Tired_Level - (Times * Rest_Amount);
               else
                  Tired_Level := Tired_Level - Times;
               end if;
               if Tired_Level < 0 then
                  Tired_Level := 0;
               end if;
            end if;
            if not Factions_List(Player_Ship.Crew(I).Faction).Flags.Contains
                (Item => To_Unbounded_String(Source => "nofatigue")) and
              (Health_Level in 1 .. 99) and Cabin_Index > 0 then
               Health_Level := Health_Level + Times;
               if Health_Level > 100 then
                  Health_Level := 100;
               end if;
            end if;
            if Player_Ship.Crew(I).Morale(1) < 50 then
               UpdateMorale
                 (Ship => Player_Ship, MemberIndex => I,
                  Value => (Times + Rest_Amount));
               if Player_Ship.Crew(I).Morale(1) > 50 then
                  Player_Ship.Crew(I).Morale := (1 => 50, 2 => 0);
               end if;
            end if;
         else
            if Player_Ship.Crew(I).Order /= TALK then
               Tired_Level := Tired_Level + Times;
            end if;
            if Tired_Level >
              (100 +
               Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                 .Level) then
               Tired_Level :=
                 (100 +
                  Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                    .Level);
            end if;
            if Tired_Level >=
              (50 +
               Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                 .Level) then
               UpdateMorale
                 (Ship => Player_Ship, MemberIndex => I,
                  Value => ((Times / 5) * (-1)));
            end if;
            case Player_Ship.Crew(I).Order is
               when PILOT =>
                  if Player_Ship.Speed /= DOCKED then
                     Gain_Exp
                       (Amount => Times, Skill_Number => Piloting_Skill,
                        Crew_Index => I);
                  else
                     Tired_Level := Player_Ship.Crew(I).Tired;
                  end if;
               when ENGINEER =>
                  if Player_Ship.Speed /= DOCKED then
                     Gain_Exp
                       (Amount => Times, Skill_Number => Engineering_Skill,
                        Crew_Index => I);
                  else
                     Tired_Level := Player_Ship.Crew(I).Tired;
                  end if;
               when GUNNER =>
                  if Player_Ship.Speed = DOCKED then
                     Tired_Level := Player_Ship.Crew(I).Tired;
                  end if;
               when HEAL =>
                  Have_Medical_Room := False;
                  Heal_Module_Loop :
                  for Module of Player_Ship.Modules loop
                     if Modules_List(Module.Proto_Index).MType =
                       MEDICAL_ROOM and
                       Module.Durability > 0 and
                       Module.Owner.Contains(Item => I) then
                        Have_Medical_Room := True;
                        exit Heal_Module_Loop;
                     end if;
                  end loop Heal_Module_Loop;
                  Heal_Loop :
                  for Member of Player_Ship.Crew loop
                     if Member.Name /= Player_Ship.Crew(I).Name and
                       Member.Health < 100 then
                        Heal_Amount :=
                          Times *
                          (GetSkillLevel
                             (Member => Player_Ship.Crew(I),
                              SkillIndex =>
                                Factions_List(Member.Faction).HealingSkill) /
                           20);
                        if Heal_Amount < Times then
                           Heal_Amount := Times;
                        end if;
                        if not Have_Medical_Room then
                           Heal_Amount := Heal_Amount / 2;
                        end if;
                        if Heal_Amount > 0 then
                           Heal_Amount := Heal_Amount * (-1);
                           Tool_Index :=
                             FindItem
                               (Inventory => Player_Ship.Cargo,
                                ItemType =>
                                  Factions_List(Member.Faction).HealingTools);
                           if Tool_Index > 0 then
                              Heal_Amount :=
                                (if
                                   Player_Ship.Cargo(Tool_Index).Amount <
                                   abs (Heal_Amount)
                                 then Player_Ship.Cargo(Tool_Index).Amount
                                 else abs (Heal_Amount));
                              UpdateCargo
                                (Ship => Player_Ship, Amount => -(Heal_Amount),
                                 CargoIndex => Tool_Index);
                           else
                              Tool_Index :=
                                FindItem
                                  (Inventory => Player_Ship.Crew(I).Inventory,
                                   ItemType =>
                                     Factions_List(Member.Faction)
                                       .HealingTools);
                              if Tool_Index > 0 then
                                 Heal_Amount :=
                                   (if
                                      Player_Ship.Crew(I).Inventory(Tool_Index)
                                        .Amount <
                                      abs (Heal_Amount)
                                    then
                                      Player_Ship.Crew(I).Inventory(Tool_Index)
                                        .Amount
                                    else abs (Heal_Amount));
                                 UpdateInventory
                                   (MemberIndex => I, Amount => -(Heal_Amount),
                                    InventoryIndex => Tool_Index);
                              end if;
                           end if;
                        end if;
                        if Heal_Amount > 0 then
                           Heal_Crew_Loop :
                           for J in Player_Ship.Crew.Iterate loop
                              if Player_Ship.Crew(J).Health < 100 and
                                Crew_Container.To_Index(Position => J) /=
                                  I then
                                 Player_Ship.Crew(J).Health :=
                                   (if
                                      Player_Ship.Crew(J).Health +
                                      Heal_Amount >
                                      Skill_Range'Last
                                    then Skill_Range'Last
                                    else Player_Ship.Crew(J).Health +
                                      Heal_Amount);
                                 AddMessage
                                   (Message =>
                                      To_String
                                        (Source => Player_Ship.Crew(I).Name) &
                                      " healed " &
                                      To_String
                                        (Source => Player_Ship.Crew(J).Name) &
                                      " a bit.",
                                    MType => OrderMessage);
                                 Gain_Exp
                                   (Amount => Times,
                                    Skill_Number =>
                                      Factions_List(Member.Faction)
                                        .HealingSkill,
                                    Crew_Index => I);
                                 exit Heal_Crew_Loop;
                              end if;
                           end loop Heal_Crew_Loop;
                        else
                           if Tool_Index = 0 then
                              AddMessage
                                (Message => "You don't have any " &
                                 To_String
                                   (Source => Factions_List(Member.Faction)
                                      .HealingTools) &
                                 " to continue healing the wounded " &
                                 To_String(Source => Member.Name) & ".",
                                 MType => OrderMessage, Color => RED);
                           else
                              AddMessage
                                (Message => To_String(Source => Player_Ship.Crew(I).Name) &
                                 " is not enough experienced to heal " &
                                 To_String(Source => Member.Name) &
                                 " in that amount of time.",
                                 MType => OrderMessage, Color => RED);
                           end if;
                        end if;
                     end if;
                  end loop Heal_Loop;
                  Heal_Amount := 1;
                  Update_Heal_Amount_Loop :
                  for J in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(J).Health < 100 and
                       Crew_Container.To_Index(Position => J) /= I then
                        Heal_Amount := 0;
                        Tool_Index :=
                          FindItem
                            (Inventory => Player_Ship.Cargo,
                             ItemType =>
                               Factions_List(Player_Ship.Crew(J).Faction)
                                 .HealingTools);
                        if Tool_Index = 0 then
                           Tool_Index :=
                             FindItem
                               (Inventory => Player_Ship.Crew(I).Inventory,
                                ItemType =>
                                  Factions_List(Player_Ship.Crew(J).Faction)
                                    .HealingTools);
                           if Tool_Index = 0 then
                              Heal_Amount := -1;
                           end if;
                        end if;
                        exit Update_Heal_Amount_Loop;
                     end if;
                  end loop Update_Heal_Amount_Loop;
                  if Heal_Amount > 0 then
                     AddMessage
                       (Message => To_String(Source => Player_Ship.Crew(I).Name) &
                        " finished healing the wounded.",
                        MType => OrderMessage, Color => GREEN);
                  end if;
                  if Heal_Amount /= 0 then
                     GiveOrders(Ship => Player_Ship, MemberIndex => I, GivenOrder => REST);
                  end if;
               when CLEAN =>
                  Tool_Index := FindTools(MemberIndex => I, ItemType => Cleaning_Tools, Order => CLEAN);
                  Need_Cleaning := False;
                  if Tool_Index > 0 then
                     Update_Clean_Tools_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           Module.Cleanliness :=
                             (if Module.Cleanliness + Times > Module.Quality
                              then Module.Quality
                              else Module.Cleanliness + Times);
                           DamageItem
                             (Inventory => Player_Ship.Crew(I).Inventory,
                              ItemIndex => Tool_Index, MemberIndex => I);
                           exit Update_Clean_Tools_Loop;
                        end if;
                     end loop Update_Clean_Tools_Loop;
                     Check_Dirty_Modules_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           Need_Cleaning := True;
                           exit Check_Dirty_Modules_Loop;
                        end if;
                     end loop Check_Dirty_Modules_Loop;
                  end if;
                  if not Need_Cleaning then
                     if Tool_Index = 0 then
                        AddMessage
                          ("You can't continue cleaning the ship because you don't have any cleaning tools.",
                           OrderMessage, RED);
                     end if;
                     Remove_Clean_Order_Loop :
                     for J in Player_Ship.Crew.Iterate loop
                        if Player_Ship.Crew(J).Order = CLEAN then
                           GiveOrders
                             (Player_Ship, Crew_Container.To_Index(J), REST);
                        end if;
                     end loop Remove_Clean_Order_Loop;
                  end if;
               when TALK =>
                  if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex =
                    0 then
                     GiveOrders(Player_Ship, I, REST);
                  end if;
               when TRAIN =>
                  Modules_Loop :
                  for Module of Player_Ship.Modules loop
                     if Module.M_Type = TRAINING_ROOM then
                        for Owner of Module.Owner loop
                           if Owner = I then
                              Skill_Index := Module.Trained_Skill;
                              exit Modules_Loop;
                           end if;
                        end loop;
                     end if;
                  end loop Modules_Loop;
                  if SkillsData_Container.Element(Skills_List, Skill_Index)
                      .Tool /=
                    Null_Bounded_String then
                     Tool_Index :=
                       FindTools
                         (I,
                          To_Unbounded_String
                            (To_String
                               (SkillsData_Container.Element
                                  (Skills_List, Skill_Index)
                                  .Tool)),
                          TRAIN, Get_Training_Tool_Quality(I, Skill_Index));
                     if Tool_Index > 0 then
                        Update_Train_Tool_Loop :
                        for J in 1 .. Times loop
                           Gain_Exp(Get_Random(1, 5), Skill_Index, I);
                           DamageItem
                             (Inventory => Player_Ship.Crew(I).Inventory,
                              ItemIndex => Tool_Index, MemberIndex => I);
                           Tool_Index :=
                             FindTools
                               (I,
                                To_Unbounded_String
                                  (To_String
                                     (SkillsData_Container.Element
                                        (Skills_List, Skill_Index)
                                        .Tool)),
                                TRAIN);
                           exit Update_Train_Tool_Loop when Tool_Index = 0;
                        end loop Update_Train_Tool_Loop;
                        AddMessage
                          (To_String(Player_Ship.Crew(I).Name) &
                           " trained a little " &
                           To_String
                             (SkillsData_Container.Element
                                (Skills_List, Skill_Index)
                                .Name) &
                           ".",
                           OrderMessage);
                     end if;
                     if Tool_Index = 0 then
                        AddMessage
                          (To_String(Player_Ship.Crew(I).Name) &
                           " can't continue training because they don't have the proper tools.",
                           OrderMessage, RED);
                        GiveOrders(Player_Ship, I, REST);
                     end if;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         <<End_Of_Loop>>
         if Tired_Points > 0 then
            if Factions_List(Player_Ship.Crew(I).Faction).FoodTypes.Length >
              0 then
               Hunger_Level :=
                 (if Hunger_Level + Tired_Points > Skill_Range'Last then
                    Skill_Range'Last
                  else Hunger_Level + Tired_Points);
               if Player_Ship.Crew(I).Hunger = Skill_Range'Last then
                  Health_Level := Health_Level - Tired_Points;
                  UpdateMorale(Player_Ship, I, -(Tired_Points));
                  if Health_Level < 1 then
                     Health_Level := Skill_Range'First;
                     Death_Reason := To_Unbounded_String("starvation");
                  end if;
               end if;
            end if;
            if Factions_List(Player_Ship.Crew(I).Faction).DrinksTypes.Length >
              0 then
               Thirst_Level :=
                 (if Thirst_Level + Tired_Points > Skill_Range'Last then
                    Skill_Range'Last
                  else Thirst_Level + Tired_Points);
               if Player_Ship.Crew(I).Thirst = Skill_Range'Last then
                  Health_Level := Health_Level - Tired_Points;
                  UpdateMorale(Player_Ship, I, -(Tired_Points));
                  if Health_Level < 1 then
                     Health_Level := Skill_Range'First;
                     Death_Reason := To_Unbounded_String("dehydration");
                  end if;
               end if;
            end if;
            if Health_Level = Skill_Range'First then
               if Death_Reason = Null_Unbounded_String then
                  Death_Reason := To_Unbounded_String("debugging");
               end if;
               Death(I, Death_Reason, Player_Ship);
               exit Update_Crew_Loop when I = 1;
            end if;
         end if;
         if Health_Level > Skill_Range'First then
            Player_Ship.Crew.Update_Element
              (Index => I, Process => Update_Member'Access);
            I := I + 1;
         end if;
      end loop Update_Crew_Loop;
   end Update_Crew;

   procedure Wait_For_Rest is
      CabinIndex: Modules_Container.Extended_Index := 0;
      TimeNeeded, TempTimeNeeded: Natural := 0;
      Damage: Damage_Factor := 0.0;
      CabinBonus: Natural;
   begin
      Wait_For_Rest_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Tired > 0 and
           Player_Ship.Crew(I).Order = REST then
            CabinIndex := 0;
            TempTimeNeeded := 0;
            CabinIndex := Find_Cabin(Crew_Container.To_Index(I));
            if CabinIndex > 0 then
               Damage :=
                 1.0 -
                 Damage_Factor
                   (Float(Player_Ship.Modules(CabinIndex).Durability) /
                    Float(Player_Ship.Modules(CabinIndex).Max_Durability));
               CabinBonus :=
                 Player_Ship.Modules(CabinIndex).Cleanliness -
                 Natural
                   (Float(Player_Ship.Modules(CabinIndex).Cleanliness) *
                    Float(Damage));
               if CabinBonus = 0 then
                  CabinBonus := 1;
               end if;
               TempTimeNeeded := (Player_Ship.Crew(I).Tired / CabinBonus) * 15;
               if TempTimeNeeded = 0 then
                  TempTimeNeeded := 15;
               end if;
            else
               TempTimeNeeded := Player_Ship.Crew(I).Tired * 15;
            end if;
            TempTimeNeeded := TempTimeNeeded + 15;
            if TempTimeNeeded > TimeNeeded then
               TimeNeeded := TempTimeNeeded;
            end if;
         end if;
      end loop Wait_For_Rest_Loop;
      if TimeNeeded > 0 then
         Update_Game(TimeNeeded);
         WaitInPlace(TimeNeeded);
      end if;
   end Wait_For_Rest;

   function Get_Skill_Level_Name(Skill_Level: Skill_Range) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Positive'Image(Skill_Level);
      end if;
      case Skill_Level is
         when 0 =>
            return "Untrained";
         when 1 .. 10 =>
            return "Beginner";
         when 11 .. 20 =>
            return "Novice";
         when 21 .. 30 =>
            return "Apprentice";
         when 31 .. 40 =>
            return "Practitioner";
         when 41 .. 50 =>
            return "Competent";
         when 51 .. 60 =>
            return "Respected";
         when 61 .. 70 =>
            return "Renowned";
         when 71 .. 80 =>
            return "Master";
         when 81 .. 90 =>
            return "Grand-Master";
         when 91 .. 99 =>
            return "Legendary";
         when others =>
            return "Ultimate";
      end case;
   end Get_Skill_Level_Name;

   function Get_Attribute_Level_Name
     (Attribute_Level: Positive) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Positive'Image(Attribute_Level);
      end if;
      case Attribute_Level is
         when 1 .. 5 =>
            return "Very low";
         when 6 .. 10 =>
            return "Low";
         when 11 .. 15 =>
            return "Below average";
         when 16 .. 30 =>
            return "Average";
         when 31 .. 35 =>
            return "Above average";
         when 36 .. 40 =>
            return "High";
         when 41 .. 49 =>
            return "Very high";
         when others =>
            return "Outstanding";
      end case;
   end Get_Attribute_Level_Name;

   procedure Daily_Payment is
      MoneyIndex2: constant Inventory_Container.Extended_Index :=
        FindItem(Player_Ship.Cargo, Money_Index);
      PayMessage: Unbounded_String;
      MemberIndex: Crew_Container.Extended_Index;
      HaveMoney: Boolean := True;
      MoneyNeeded: Natural;
   begin
      MemberIndex := 1;
      Daily_Payment_Loop :
      for Member of Player_Ship.Crew loop
         if Member.Payment(1) > 0 then
            if MoneyIndex2 = 0 and HaveMoney then
               AddMessage
                 ("You don't have any " & To_String(Money_Name) &
                  " to pay your crew members.",
                  TradeMessage, RED);
               HaveMoney := False;
            end if;
            if HaveMoney then
               if Player_Ship.Cargo(MoneyIndex2).Amount <
                 Member.Payment(1) then
                  MoneyNeeded := Player_Ship.Cargo(MoneyIndex2).Amount;
                  UpdateCargo
                    (Ship => Player_Ship, ProtoIndex => Money_Index,
                     Amount => (0 - MoneyNeeded));
                  AddMessage
                    ("You don't have enough " & To_String(Money_Name) &
                     " to pay your crew members.",
                     TradeMessage, RED);
                  HaveMoney := False;
               end if;
               if HaveMoney then
                  UpdateCargo
                    (Ship => Player_Ship, CargoIndex => MoneyIndex2,
                     Amount => (0 - Member.Payment(1)));
                  PayMessage := To_Unbounded_String("You pay ") & Member.Name;
                  if Member.Gender = 'M' then
                     Append(PayMessage, " his ");
                  else
                     Append(PayMessage, " her ");
                  end if;
                  Append(PayMessage, "daily payment.");
                  AddMessage(To_String(PayMessage), TradeMessage);
                  UpdateMorale(Player_Ship, MemberIndex, Get_Random(1, 5));
               end if;
            end if;
            if not HaveMoney then
               UpdateMorale(Player_Ship, MemberIndex, Get_Random(-50, -10));
            end if;
         end if;
         MemberIndex := MemberIndex + 1;
      end loop Daily_Payment_Loop;
      MemberIndex := 1;
      Update_Contracts_Loop :
      while MemberIndex <= Player_Ship.Crew.Last_Index loop
         if Player_Ship.Crew(MemberIndex).Contract_Length > 0 then
            Player_Ship.Crew(MemberIndex).Contract_Length :=
              Player_Ship.Crew(MemberIndex).Contract_Length - 1;
            if Player_Ship.Crew(MemberIndex).Contract_Length = 0 then
               AddMessage
                 ("Your contract with " &
                  To_String(Player_Ship.Crew(MemberIndex).Name) &
                  " has ended.",
                  TradeMessage, RED);
               if Player_Ship.Speed /= DOCKED then
                  Player_Ship.Crew(MemberIndex).Orders := (others => 0);
                  GiveOrders(Player_Ship, MemberIndex, REST);
               else
                  DeleteMember(MemberIndex, Player_Ship);
                  Sky_Bases
                    (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex)
                    .Population :=
                    Sky_Bases
                      (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex)
                      .Population +
                    1;
                  MemberIndex := MemberIndex - 1;
               end if;
            end if;
         end if;
         MemberIndex := MemberIndex + 1;
      end loop Update_Contracts_Loop;
   end Daily_Payment;

   function Get_Training_Tool_Quality
     (Member_Index, Skill_Index: Positive) return Positive is
      ToolQuality: Positive := 100;
   begin
      Skill_Loop :
      for Skill of Player_Ship.Crew(Member_Index).Skills loop
         if Skill.Index = Skill_Index then
            Tool_Quality_Loop :
            for Quality of SkillsData_Container.Element
              (Skills_List, Skill_Index)
              .Tools_Quality loop
               if Skill.Level <= Quality.Level then
                  ToolQuality := Quality.Quality;
                  exit Skill_Loop;
               end if;
            end loop Tool_Quality_Loop;
         end if;
      end loop Skill_Loop;
      return ToolQuality;
   end Get_Training_Tool_Quality;

end Crew;
