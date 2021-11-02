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

      SkillExp, AttributeExp, AttributeLevel, NewAmount: Natural := 0;
      AttributeIndex: constant Skills_Container.Extended_Index :=
        Natural
          (SkillsData_Container.Element(Skills_List, Skill_Number).Attribute);
      SkillIndex: Skills_Container.Extended_Index := 0;
      SkillLevel: Skill_Range := 0;
      procedure GainExpInAttribute(Attribute: Positive) is
         Attribute_To_Check: Mob_Attribute_Record :=
           Player_Ship.Crew(Crew_Index).Attributes(Attribute);
      begin
         if Attribute_To_Check.Level = 50 then
            return;
         end if;
         AttributeExp := Attribute_To_Check.Experience + NewAmount;
         AttributeLevel := Attribute_To_Check.Level;
         if AttributeExp >= (AttributeLevel * 250) then
            AttributeExp := AttributeExp - (AttributeLevel * 250);
            AttributeLevel := AttributeLevel + 1;
         end if;
         Attribute_To_Check.Level := AttributeLevel;
         Attribute_To_Check.Experience := AttributeExp;
         Player_Ship.Crew(Crew_Index).Attributes(Attribute) :=
           Attribute_To_Check;
      end GainExpInAttribute;
   begin
      NewAmount :=
        (if
           Careers_List(Player_Career).Skills.Contains
             (To_Unbounded_String
                (To_String
                   (SkillsData_Container.Element(Skills_List, Skill_Number)
                      .Name)))
         then Amount + (Amount / 2)
         else Amount);
      NewAmount :=
        Natural(Float(NewAmount) * Float(New_Game_Settings.Experience_Bonus));
      if NewAmount = 0 then
         return;
      end if;
      -- Gain experience in condition assigned attribute
      GainExpInAttribute(Positive(Condition_Index));
      -- Gain experience in associated attribute
      GainExpInAttribute(AttributeIndex);
      -- Gain experience in skill
      Experience_In_Skill_Loop :
      for I in Player_Ship.Crew(Crew_Index).Skills.Iterate loop
         if Player_Ship.Crew(Crew_Index).Skills(I).Index = Skill_Number then
            SkillIndex := Skills_Container.To_Index(I);
            exit Experience_In_Skill_Loop;
         end if;
      end loop Experience_In_Skill_Loop;
      if SkillIndex > 0 then
         if Player_Ship.Crew(Crew_Index).Skills(SkillIndex).Level =
           Skill_Range'Last then
            return;
         end if;
         SkillLevel := Player_Ship.Crew(Crew_Index).Skills(SkillIndex).Level;
         SkillExp :=
           Player_Ship.Crew(Crew_Index).Skills(SkillIndex).Experience +
           NewAmount;
      end if;
      if SkillExp >= (SkillLevel * 25) then
         SkillExp := SkillExp - (SkillLevel * 25);
         SkillLevel := SkillLevel + 1;
      end if;
      if SkillIndex > 0 then
         Player_Ship.Crew(Crew_Index).Skills(SkillIndex).Level := SkillLevel;
         Player_Ship.Crew(Crew_Index).Skills(SkillIndex).Experience :=
           SkillExp;
      else
         Player_Ship.Crew(Crew_Index).Skills.Append
           (New_Item => (Skill_Number, SkillLevel, SkillExp));
      end if;
   end Gain_Exp;

   function Generate_Member_Name
     (Gender: Character; Faction_Index: Unbounded_String)
      return Unbounded_String is
      NewName: Unbounded_String := Null_Unbounded_String;
      NameType: constant NamesTypes := Factions_List(Faction_Index).NamesType;
   begin
      if NameType = Factions.ROBOTIC then
         return Generate_Robotic_Name;
      end if;
      if Gender = 'M' then
         NewName :=
           Male_Syllables_Start
             (Get_Random
                (Male_Syllables_Start.First_Index,
                 Male_Syllables_Start.Last_Index)) &
           Male_Vocals
             (Get_Random(Male_Vocals.First_Index, Male_Vocals.Last_Index));
         if Get_Random(1, 100) < 36 then
            Append
              (NewName,
               Male_Syllables_Middle
                 (Get_Random
                    (Male_Syllables_Middle.First_Index,
                     Male_Syllables_Middle.Last_Index)));
         end if;
         if Get_Random(1, 100) < 11 then
            Append
              (NewName,
               Male_Consonants
                 (Get_Random
                    (Male_Consonants.First_Index,
                     Male_Consonants.Last_Index)));
         end if;
         Append
           (NewName,
            Male_Syllables_End
              (Get_Random
                 (Male_Syllables_End.First_Index,
                  Male_Syllables_End.Last_Index)));
         return NewName;
      end if;
      NewName :=
        Female_Syllables_Start
          (Get_Random
             (Female_Syllables_Start.First_Index,
              Female_Syllables_Start.Last_Index)) &
        Female_Vocals
          (Get_Random(Female_Vocals.First_Index, Female_Vocals.Last_Index));
      if Get_Random(1, 100) < 36 then
         Append
           (NewName,
            Female_Syllables_Middle
              (Get_Random
                 (Female_Syllables_Middle.First_Index,
                  Female_Syllables_Middle.Last_Index)));
      end if;
      if Get_Random(1, 100) < 11 then
         Append
           (NewName,
            Female_Syllables_Middle
              (Get_Random
                 (Female_Syllables_Middle.First_Index,
                  Female_Syllables_Middle.Last_Index)));
      end if;
      Append
        (NewName,
         Female_Syllables_End
           (Get_Random
              (Female_Syllables_End.First_Index,
               Female_Syllables_End.Last_Index)));
      return NewName;
   end Generate_Member_Name;

   function Find_Cabin(Member_Index: Positive) return Natural is
   begin
      Find_Cabin_Loop :
      for I in Player_Ship.Modules.Iterate loop
         if Player_Ship.Modules(I).M_Type = CABIN then
            Check_Owner_Loop :
            for Owner of Player_Ship.Modules(I).Owner loop
               if Owner = Member_Index then
                  return Modules_Container.To_Index(I);
               end if;
            end loop Check_Owner_Loop;
         end if;
      end loop Find_Cabin_Loop;
      return 0;
   end Find_Cabin;

   procedure Update_Crew
     (Minutes: Positive; Tired_Points: Natural; In_Combat: Boolean := False) is
      use Tiny_String;

      TiredLevel, HungerLevel, ThirstLevel: Integer := 0;
      HealthLevel: Integer := 100;
      DeathReason: Unbounded_String;
      ToolIndex: Inventory_Container.Extended_Index;
      CabinIndex: Modules_Container.Extended_Index;
      Times, RestAmount, I: Natural;
      Order_Time, CurrentMinutes, HealAmount: Integer;
      Damage: Damage_Factor := 0.0;
      NeedCleaning, HaveMedicalRoom: Boolean := False;
      SkillIndex: Skills_Container.Extended_Index;
      function Consume(ItemType: Unbounded_String) return Natural is
         ConsumeValue: Natural;
         ItemIndex: Inventory_Container.Extended_Index :=
           FindItem(Inventory => Player_Ship.Cargo, ItemType => ItemType);
      begin
         if ItemIndex > 0 then
            ConsumeValue :=
              Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Value(1);
            if Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Value
                .Length >
              1
              and then
                Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Value(2) /=
                0 then
               UpdateMorale
                 (Player_Ship, I,
                  Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Value
                    (2));
            end if;
            UpdateCargo
              (Player_Ship, Player_Ship.Cargo.Element(ItemIndex).ProtoIndex,
               -1);
            return ConsumeValue;
         end if;
         ItemIndex :=
           FindItem
             (Inventory => Player_Ship.Crew(I).Inventory,
              ItemType => ItemType);
         if ItemIndex > 0 then
            ConsumeValue :=
              Items_List(Player_Ship.Crew(I).Inventory(ItemIndex).ProtoIndex)
                .Value
                (1);
            if Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Value(2) /=
              0 then
               UpdateMorale
                 (Player_Ship, I,
                  Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Value
                    (2));
            end if;
            UpdateInventory
              (MemberIndex => I, Amount => -1, InventoryIndex => ItemIndex);
            return ConsumeValue;
         end if;
         return 0;
      end Consume;
      procedure UpdateMember(Member: in out Member_Data) is
         BackToWork: Boolean := True;
         ConsumeResult: Natural := 0;
         procedure NormalizeStat
           (Stat: in out Integer; MaxValue: Positive := 100) is
         begin
            if Stat > MaxValue then
               Stat := MaxValue;
            elsif Stat < 0 then
               Stat := 0;
            end if;
         end NormalizeStat;
      begin
         if Factions_List(Member.Faction).Flags.Contains
             (To_Unbounded_String("nofatigue")) then
            TiredLevel := 0;
         end if;
         if TiredLevel = 0 and Member.Order = REST and
           Member.Previous_Order /= REST then
            if Member.Previous_Order not in REPAIR | CLEAN
              and then FindMember(Member.Previous_Order) > 0 then
               BackToWork := False;
            end if;
            if Member.Previous_Order in GUNNER | CRAFT then
               Module_Loop :
               for Module of Player_Ship.Modules loop
                  if (Member.Previous_Order = GUNNER and Module.M_Type = GUN)
                    and then (Module.Owner(1) in I | 0) then
                     BackToWork := True;
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
                           BackToWork := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop Module_Is_Owner_Loop;
                     Module_Empty_Owner_Loop :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           BackToWork := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop Module_Empty_Owner_Loop;
                  end if;
               end loop Module_Loop;
            end if;
            if BackToWork then
               Member.Order := Member.Previous_Order;
               Member.Order_Time := 15;
               AddMessage
                 (To_String(Member.Name) & " returns to work fully rested.",
                  OrderMessage, YELLOW);
               UpdateMorale(Player_Ship, I, 1);
            end if;
            Member.Previous_Order := REST;
         end if;
         if TiredLevel >
           (80 + Member.Attributes(Positive(Condition_Index)).Level) and
           Member.Order /= REST and not In_Combat then
            declare
               CanRest: Boolean := True;
            begin
               if Member.Order = BOARDING and HarpoonDuration = 0 and
                 Combat.Enemy.HarpoonDuration = 0 then
                  CanRest := False;
               end if;
               if CanRest then
                  Member.Previous_Order := Member.Order;
                  Member.Order := REST;
                  Member.Order_Time := 15;
                  if Member.Equipment(7) > 0 then
                     UpdateCargo
                       (Player_Ship,
                        Member.Inventory(Member.Equipment(7)).ProtoIndex, 1,
                        Member.Inventory(Member.Equipment(7)).Durability);
                     UpdateInventory
                       (MemberIndex => I, Amount => -1,
                        InventoryIndex => Member.Equipment(7));
                     Member.Equipment(7) := 0;
                  end if;
                  AddMessage
                    (To_String(Member.Name) &
                     " is too tired to work, they're going to rest.",
                     OrderMessage, YELLOW);
                  if Find_Cabin(I) = 0 then
                     Modules_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN and Module.Durability > 0 then
                           Find_Cabin_Owner_Loop :
                           for Owner of Module.Owner loop
                              if Owner = 0 then
                                 Owner := I;
                                 AddMessage
                                   (To_String(Member.Name) & " take " &
                                    To_String(Module.Name) & " as own cabin.",
                                    OtherMessage);
                                 exit Modules_Loop;
                              end if;
                           end loop Find_Cabin_Owner_Loop;
                        end if;
                     end loop Modules_Loop;
                  end if;
               else
                  AddMessage
                    (To_String(Member.Name) &
                     " is very tired but they can't go to rest.",
                     OrderMessage, RED);
                  UpdateMorale(Player_Ship, I, Get_Random(-5, -1));
               end if;
            end;
         end if;
         NormalizeStat(TiredLevel, 150);
         Member.Tired := TiredLevel;
         if HungerLevel > 80 then
            Find_Food_Loop :
            for FoodType of Factions_List(Member.Faction).FoodTypes loop
               ConsumeResult := Consume(FoodType);
               exit Find_Food_Loop when ConsumeResult > 0;
            end loop Find_Food_Loop;
            HungerLevel :=
              (if HungerLevel - ConsumeResult < Skill_Range'First then
                 Skill_Range'First
               else HungerLevel - ConsumeResult);
            if ConsumeResult = 0 then
               AddMessage
                 (To_String(Member.Name) &
                  " is hungry, but they can't find anything to eat.",
                  OtherMessage, RED);
               UpdateMorale(Player_Ship, I, Get_Random(-10, -5));
            end if;
         end if;
         NormalizeStat(HungerLevel);
         Member.Hunger := HungerLevel;
         if ThirstLevel > 40 then
            Find_Drink_Loop :
            for DrinksType of Factions_List(Member.Faction).DrinksTypes loop
               ConsumeResult := Consume(DrinksType);
               exit Find_Drink_Loop when ConsumeResult > 0;
            end loop Find_Drink_Loop;
            ThirstLevel :=
              (if ThirstLevel - ConsumeResult < Skill_Range'First then
                 Skill_Range'First
               else ThirstLevel - ConsumeResult);
            if ConsumeResult = 0 then
               AddMessage
                 (To_String(Member.Name) &
                  " is thirsty, but they can't find anything to drink.",
                  OtherMessage, RED);
               UpdateMorale(Player_Ship, I, Get_Random(-20, -10));
            end if;
         end if;
         NormalizeStat(ThirstLevel);
         Member.Thirst := ThirstLevel;
         NormalizeStat(HealthLevel);
         Member.Health := HealthLevel;
         if Member.Order not in REPAIR | CRAFT | UPGRADING then
            Member.Order_Time := Order_Time;
         end if;
         if Member.Skills.Length = 0 then
            Member.Contract_Length := Member.Contract_Length - Minutes;
            if Member.Contract_Length < 0 then
               Member.Contract_Length := 0;
            end if;
         end if;
      end UpdateMember;
   begin
      I := Player_Ship.Crew.First_Index;
      Update_Crew_Loop :
      while I <= Player_Ship.Crew.Last_Index loop
         CurrentMinutes := Minutes;
         Order_Time := Player_Ship.Crew(I).Order_Time;
         Times := 0;
         Update_Current_Minutes_Loop :
         while CurrentMinutes > 0 loop
            if CurrentMinutes >= Order_Time then
               CurrentMinutes := CurrentMinutes - Order_Time;
               Times := Times + 1;
               Order_Time := 15;
            else
               Order_Time := Order_Time - CurrentMinutes;
               CurrentMinutes := 0;
            end if;
         end loop Update_Current_Minutes_Loop;
         HealthLevel := Player_Ship.Crew(I).Health;
         HungerLevel := Player_Ship.Crew(I).Hunger;
         ThirstLevel := Player_Ship.Crew(I).Thirst;
         TiredLevel := Player_Ship.Crew(I).Tired;
         if Times = 0 then
            goto End_Of_Loop;
         end if;
         if Player_Ship.Crew(I).Order = REST then
            CabinIndex := Find_Cabin(I);
            RestAmount := 0;
            if Player_Ship.Crew(I).Tired > 0 then
               if CabinIndex > 0 then
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(Player_Ship.Modules(CabinIndex).Durability) /
                       Float(Player_Ship.Modules(CabinIndex).Max_Durability));
                  RestAmount :=
                    Player_Ship.Modules(CabinIndex).Cleanliness -
                    Natural
                      (Float(Player_Ship.Modules(CabinIndex).Cleanliness) *
                       Float(Damage));
                  if RestAmount = 0 then
                     RestAmount := 1;
                  end if;
                  TiredLevel := TiredLevel - (Times * RestAmount);
               else
                  TiredLevel := TiredLevel - Times;
               end if;
               if TiredLevel < 0 then
                  TiredLevel := 0;
               end if;
            end if;
            if not Factions_List(Player_Ship.Crew(I).Faction).Flags.Contains
                (To_Unbounded_String("nofatigue")) and
              (HealthLevel in 1 .. 99) and CabinIndex > 0 then
               HealthLevel := HealthLevel + Times;
               if HealthLevel > 100 then
                  HealthLevel := 100;
               end if;
            end if;
            if Player_Ship.Crew(I).Morale(1) < 50 then
               UpdateMorale(Player_Ship, I, (Times + RestAmount));
               if Player_Ship.Crew(I).Morale(1) > 50 then
                  Player_Ship.Crew(I).Morale := (50, 0);
               end if;
            end if;
         else
            if Player_Ship.Crew(I).Order /= TALK then
               TiredLevel := TiredLevel + Times;
            end if;
            if TiredLevel >
              (100 +
               Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                 .Level) then
               TiredLevel :=
                 (100 +
                  Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                    .Level);
            end if;
            if TiredLevel >=
              (50 +
               Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                 .Level) then
               UpdateMorale(Player_Ship, I, ((Times / 5) * (-1)));
            end if;
            case Player_Ship.Crew(I).Order is
               when PILOT =>
                  if Player_Ship.Speed /= DOCKED then
                     Gain_Exp(Times, Piloting_Skill, I);
                  else
                     TiredLevel := Player_Ship.Crew(I).Tired;
                  end if;
               when ENGINEER =>
                  if Player_Ship.Speed /= DOCKED then
                     Gain_Exp(Times, Engineering_Skill, I);
                  else
                     TiredLevel := Player_Ship.Crew(I).Tired;
                  end if;
               when GUNNER =>
                  if Player_Ship.Speed = DOCKED then
                     TiredLevel := Player_Ship.Crew(I).Tired;
                  end if;
               when HEAL =>
                  HaveMedicalRoom := False;
                  Heal_Module_Loop :
                  for Module of Player_Ship.Modules loop
                     if Modules_List(Module.Proto_Index).MType =
                       MEDICAL_ROOM and
                       Module.Durability > 0 and Module.Owner.Contains(I) then
                        HaveMedicalRoom := True;
                        exit Heal_Module_Loop;
                     end if;
                  end loop Heal_Module_Loop;
                  Heal_Loop :
                  for Member of Player_Ship.Crew loop
                     if Member.Name /= Player_Ship.Crew(I).Name and
                       Member.Health < 100 then
                        HealAmount :=
                          Times *
                          (GetSkillLevel
                             (Player_Ship.Crew(I),
                              Factions_List(Member.Faction).HealingSkill) /
                           20);
                        if HealAmount < Times then
                           HealAmount := Times;
                        end if;
                        if not HaveMedicalRoom then
                           HealAmount := HealAmount / 2;
                        end if;
                        if HealAmount > 0 then
                           HealAmount := HealAmount * (-1);
                           ToolIndex :=
                             FindItem
                               (Inventory => Player_Ship.Cargo,
                                ItemType =>
                                  Factions_List(Member.Faction).HealingTools);
                           if ToolIndex > 0 then
                              HealAmount :=
                                (if
                                   Player_Ship.Cargo(ToolIndex).Amount <
                                   abs (HealAmount)
                                 then Player_Ship.Cargo(ToolIndex).Amount
                                 else abs (HealAmount));
                              UpdateCargo
                                (Ship => Player_Ship, Amount => -(HealAmount),
                                 CargoIndex => ToolIndex);
                           else
                              ToolIndex :=
                                FindItem
                                  (Inventory => Player_Ship.Crew(I).Inventory,
                                   ItemType =>
                                     Factions_List(Member.Faction)
                                       .HealingTools);
                              if ToolIndex > 0 then
                                 HealAmount :=
                                   (if
                                      Player_Ship.Crew(I).Inventory(ToolIndex)
                                        .Amount <
                                      abs (HealAmount)
                                    then
                                      Player_Ship.Crew(I).Inventory(ToolIndex)
                                        .Amount
                                    else abs (HealAmount));
                                 UpdateInventory
                                   (MemberIndex => I, Amount => -(HealAmount),
                                    InventoryIndex => ToolIndex);
                              end if;
                           end if;
                        end if;
                        if HealAmount > 0 then
                           Heal_Crew_Loop :
                           for J in Player_Ship.Crew.Iterate loop
                              if Player_Ship.Crew(J).Health < 100 and
                                Crew_Container.To_Index(J) /= I then
                                 Player_Ship.Crew(J).Health :=
                                   (if
                                      Player_Ship.Crew(J).Health + HealAmount >
                                      Skill_Range'Last
                                    then Skill_Range'Last
                                    else Player_Ship.Crew(J).Health +
                                      HealAmount);
                                 AddMessage
                                   (To_String(Player_Ship.Crew(I).Name) &
                                    " healed " &
                                    To_String(Player_Ship.Crew(J).Name) &
                                    " a bit.",
                                    OrderMessage);
                                 Gain_Exp
                                   (Times,
                                    Factions_List(Member.Faction).HealingSkill,
                                    I);
                                 exit Heal_Crew_Loop;
                              end if;
                           end loop Heal_Crew_Loop;
                        else
                           if ToolIndex = 0 then
                              AddMessage
                                ("You don't have any " &
                                 To_String
                                   (Factions_List(Member.Faction)
                                      .HealingTools) &
                                 " to continue healing the wounded " &
                                 To_String(Member.Name) & ".",
                                 OrderMessage, RED);
                           else
                              AddMessage
                                (To_String(Player_Ship.Crew(I).Name) &
                                 " is not enough experienced to heal " &
                                 To_String(Member.Name) &
                                 " in that amount of time.",
                                 OrderMessage, RED);
                           end if;
                        end if;
                     end if;
                  end loop Heal_Loop;
                  HealAmount := 1;
                  Update_Heal_Amount_Loop :
                  for J in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(J).Health < 100 and
                       Crew_Container.To_Index(J) /= I then
                        HealAmount := 0;
                        ToolIndex :=
                          FindItem
                            (Inventory => Player_Ship.Cargo,
                             ItemType =>
                               Factions_List(Player_Ship.Crew(J).Faction)
                                 .HealingTools);
                        if ToolIndex = 0 then
                           ToolIndex :=
                             FindItem
                               (Inventory => Player_Ship.Crew(I).Inventory,
                                ItemType =>
                                  Factions_List(Player_Ship.Crew(J).Faction)
                                    .HealingTools);
                           if ToolIndex = 0 then
                              HealAmount := -1;
                           end if;
                        end if;
                        exit Update_Heal_Amount_Loop;
                     end if;
                  end loop Update_Heal_Amount_Loop;
                  if HealAmount > 0 then
                     AddMessage
                       (To_String(Player_Ship.Crew(I).Name) &
                        " finished healing the wounded.",
                        OrderMessage, GREEN);
                  end if;
                  if HealAmount /= 0 then
                     GiveOrders(Player_Ship, I, REST);
                  end if;
               when CLEAN =>
                  ToolIndex := FindTools(I, Cleaning_Tools, CLEAN);
                  NeedCleaning := False;
                  if ToolIndex > 0 then
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
                              ItemIndex => ToolIndex, MemberIndex => I);
                           exit Update_Clean_Tools_Loop;
                        end if;
                     end loop Update_Clean_Tools_Loop;
                     Check_Dirty_Modules_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           NeedCleaning := True;
                           exit Check_Dirty_Modules_Loop;
                        end if;
                     end loop Check_Dirty_Modules_Loop;
                  end if;
                  if not NeedCleaning then
                     if ToolIndex = 0 then
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
                              SkillIndex := Module.Trained_Skill;
                              exit Modules_Loop;
                           end if;
                        end loop;
                     end if;
                  end loop Modules_Loop;
                  if SkillsData_Container.Element(Skills_List, SkillIndex)
                      .Tool /=
                    Null_Bounded_String then
                     ToolIndex :=
                       FindTools
                         (I,
                          To_Unbounded_String
                            (To_String
                               (SkillsData_Container.Element
                                  (Skills_List, SkillIndex)
                                  .Tool)),
                          TRAIN, Get_Training_Tool_Quality(I, SkillIndex));
                     if ToolIndex > 0 then
                        Update_Train_Tool_Loop :
                        for J in 1 .. Times loop
                           Gain_Exp(Get_Random(1, 5), SkillIndex, I);
                           DamageItem
                             (Inventory => Player_Ship.Crew(I).Inventory,
                              ItemIndex => ToolIndex, MemberIndex => I);
                           ToolIndex :=
                             FindTools
                               (I,
                                To_Unbounded_String
                                  (To_String
                                     (SkillsData_Container.Element
                                        (Skills_List, SkillIndex)
                                        .Tool)),
                                TRAIN);
                           exit Update_Train_Tool_Loop when ToolIndex = 0;
                        end loop Update_Train_Tool_Loop;
                        AddMessage
                          (To_String(Player_Ship.Crew(I).Name) &
                           " trained a little " &
                           To_String
                             (SkillsData_Container.Element
                                (Skills_List, SkillIndex)
                                .Name) &
                           ".",
                           OrderMessage);
                     end if;
                     if ToolIndex = 0 then
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
               HungerLevel :=
                 (if HungerLevel + Tired_Points > Skill_Range'Last then
                    Skill_Range'Last
                  else HungerLevel + Tired_Points);
               if Player_Ship.Crew(I).Hunger = Skill_Range'Last then
                  HealthLevel := HealthLevel - Tired_Points;
                  UpdateMorale(Player_Ship, I, -(Tired_Points));
                  if HealthLevel < 1 then
                     HealthLevel := Skill_Range'First;
                     DeathReason := To_Unbounded_String("starvation");
                  end if;
               end if;
            end if;
            if Factions_List(Player_Ship.Crew(I).Faction).DrinksTypes.Length >
              0 then
               ThirstLevel :=
                 (if ThirstLevel + Tired_Points > Skill_Range'Last then
                    Skill_Range'Last
                  else ThirstLevel + Tired_Points);
               if Player_Ship.Crew(I).Thirst = Skill_Range'Last then
                  HealthLevel := HealthLevel - Tired_Points;
                  UpdateMorale(Player_Ship, I, -(Tired_Points));
                  if HealthLevel < 1 then
                     HealthLevel := Skill_Range'First;
                     DeathReason := To_Unbounded_String("dehydration");
                  end if;
               end if;
            end if;
            if HealthLevel = Skill_Range'First then
               if DeathReason = Null_Unbounded_String then
                  DeathReason := To_Unbounded_String("debugging");
               end if;
               Death(I, DeathReason, Player_Ship);
               exit Update_Crew_Loop when I = 1;
            end if;
         end if;
         if HealthLevel > Skill_Range'First then
            Player_Ship.Crew.Update_Element
              (Index => I, Process => UpdateMember'Access);
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
