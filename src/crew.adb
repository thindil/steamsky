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

   procedure GainExp(Amount: Natural; SkillNumber, CrewIndex: Positive) is
      SkillExp, AttributeExp, AttributeLevel, NewAmount: Natural := 0;
      AttributeIndex: constant Skills_Container.Extended_Index :=
        Skills_List(SkillNumber).Attribute;
      SkillIndex: Skills_Container.Extended_Index := 0;
      SkillLevel: Skill_Range := 0;
      procedure GainExpInAttribute(Attribute: Positive) is
      begin
         if PlayerShip.Crew(CrewIndex).Attributes(Attribute)(1) = 50 then
            return;
         end if;
         AttributeExp :=
           PlayerShip.Crew(CrewIndex).Attributes(Attribute)(2) + NewAmount;
         AttributeLevel := PlayerShip.Crew(CrewIndex).Attributes(Attribute)(1);
         if AttributeExp >= (AttributeLevel * 250) then
            AttributeExp := AttributeExp - (AttributeLevel * 250);
            AttributeLevel := AttributeLevel + 1;
         end if;
         PlayerShip.Crew(CrewIndex).Attributes(Attribute)(1) := AttributeLevel;
         PlayerShip.Crew(CrewIndex).Attributes(Attribute)(2) := AttributeExp;
      end GainExpInAttribute;
   begin
      NewAmount :=
        (if
           Careers_List(Player_Career).Skills.Contains
             (Skills_List(SkillNumber).Name)
         then Amount + (Amount / 2)
         else Amount);
      NewAmount :=
        Natural(Float(NewAmount) * Float(New_Game_Settings.Experience_Bonus));
      if NewAmount = 0 then
         return;
      end if;
      -- Gain experience in condition assigned attribute
      GainExpInAttribute(Condition_Index);
      -- Gain experience in associated attribute
      GainExpInAttribute(AttributeIndex);
      -- Gain experience in skill
      Experience_In_Skill_Loop :
      for I in PlayerShip.Crew(CrewIndex).Skills.Iterate loop
         if PlayerShip.Crew(CrewIndex).Skills(I)(1) = SkillNumber then
            SkillIndex := Skills_Container.To_Index(I);
            exit Experience_In_Skill_Loop;
         end if;
      end loop Experience_In_Skill_Loop;
      if SkillIndex > 0 then
         if PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(2) =
           Skill_Range'Last then
            return;
         end if;
         SkillLevel := PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(2);
         SkillExp :=
           PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(3) + NewAmount;
      end if;
      if SkillExp >= (SkillLevel * 25) then
         SkillExp := SkillExp - (SkillLevel * 25);
         SkillLevel := SkillLevel + 1;
      end if;
      if SkillIndex > 0 then
         PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(2) := SkillLevel;
         PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(3) := SkillExp;
      else
         PlayerShip.Crew(CrewIndex).Skills.Append
           (New_Item => (SkillNumber, SkillLevel, SkillExp));
      end if;
   end GainExp;

   function GenerateMemberName
     (Gender: Character; FactionIndex: Unbounded_String)
      return Unbounded_String is
      NewName: Unbounded_String := Null_Unbounded_String;
      NameType: constant NamesTypes := Factions_List(FactionIndex).NamesType;
   begin
      if NameType = Factions.ROBOTIC then
         return GenerateRoboticName;
      end if;
      if Gender = 'M' then
         NewName :=
           MaleSyllablesStart
             (GetRandom
                (MaleSyllablesStart.First_Index,
                 MaleSyllablesStart.Last_Index)) &
           MaleVocals
             (GetRandom(MaleVocals.First_Index, MaleVocals.Last_Index));
         if GetRandom(1, 100) < 36 then
            Append
              (NewName,
               MaleSyllablesMiddle
                 (GetRandom
                    (MaleSyllablesMiddle.First_Index,
                     MaleSyllablesMiddle.Last_Index)));
         end if;
         if GetRandom(1, 100) < 11 then
            Append
              (NewName,
               MaleConsonants
                 (GetRandom
                    (MaleConsonants.First_Index, MaleConsonants.Last_Index)));
         end if;
         Append
           (NewName,
            MaleSyllablesEnd
              (GetRandom
                 (MaleSyllablesEnd.First_Index, MaleSyllablesEnd.Last_Index)));
         return NewName;
      end if;
      NewName :=
        FemaleSyllablesStart
          (GetRandom
             (FemaleSyllablesStart.First_Index,
              FemaleSyllablesStart.Last_Index)) &
        FemaleVocals
          (GetRandom(FemaleVocals.First_Index, FemaleVocals.Last_Index));
      if GetRandom(1, 100) < 36 then
         Append
           (NewName,
            FemaleSyllablesMiddle
              (GetRandom
                 (FemaleSyllablesMiddle.First_Index,
                  FemaleSyllablesMiddle.Last_Index)));
      end if;
      if GetRandom(1, 100) < 11 then
         Append
           (NewName,
            FemaleSyllablesMiddle
              (GetRandom
                 (FemaleSyllablesMiddle.First_Index,
                  FemaleSyllablesMiddle.Last_Index)));
      end if;
      Append
        (NewName,
         FemaleSyllablesEnd
           (GetRandom
              (FemaleSyllablesEnd.First_Index,
               FemaleSyllablesEnd.Last_Index)));
      return NewName;
   end GenerateMemberName;

   function FindCabin(MemberIndex: Positive) return Natural is
   begin
      Find_Cabin_Loop :
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).M_Type = CABIN then
            Check_Owner_Loop :
            for Owner of PlayerShip.Modules(I).Owner loop
               if Owner = MemberIndex then
                  return Modules_Container.To_Index(I);
               end if;
            end loop Check_Owner_Loop;
         end if;
      end loop Find_Cabin_Loop;
      return 0;
   end FindCabin;

   procedure UpdateCrew
     (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False) is
      TiredLevel, HungerLevel, ThirstLevel: Integer := 0;
      HealthLevel: Integer := 100;
      DeathReason: Unbounded_String;
      ToolIndex: Inventory_Container.Extended_Index;
      CabinIndex: Modules_Container.Extended_Index;
      Times, RestAmount, I: Natural;
      OrderTime, CurrentMinutes, HealAmount: Integer;
      Damage: Damage_Factor := 0.0;
      NeedCleaning, HaveMedicalRoom: Boolean := False;
      SkillIndex: Skills_Container.Extended_Index;
      function Consume(ItemType: Unbounded_String) return Natural is
         ConsumeValue: Natural;
         ItemIndex: Inventory_Container.Extended_Index :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => ItemType);
      begin
         if ItemIndex > 0 then
            ConsumeValue :=
              Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value(1);
            if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value
                .Length >
              1
              and then
                Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value(2) /=
                0 then
               UpdateMorale
                 (PlayerShip, I,
                  Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value(2));
            end if;
            UpdateCargo
              (PlayerShip, PlayerShip.Cargo.Element(ItemIndex).ProtoIndex, -1);
            return ConsumeValue;
         end if;
         ItemIndex :=
           FindItem
             (Inventory => PlayerShip.Crew(I).Inventory, ItemType => ItemType);
         if ItemIndex > 0 then
            ConsumeValue :=
              Items_List(PlayerShip.Crew(I).Inventory(ItemIndex).ProtoIndex)
                .Value
                (1);
            if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value(2) /=
              0 then
               UpdateMorale
                 (PlayerShip, I,
                  Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value(2));
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
         if TiredLevel = 0 and Member.Order = Rest and
           Member.PreviousOrder /= Rest then
            if Member.PreviousOrder not in Repair | Clean
              and then FindMember(Member.PreviousOrder) > 0 then
               BackToWork := False;
            end if;
            if Member.PreviousOrder in Gunner | Craft then
               Module_Loop :
               for Module of PlayerShip.Modules loop
                  if (Member.PreviousOrder = Gunner and Module.M_Type = GUN)
                    and then (Module.Owner(1) in I | 0) then
                     BackToWork := True;
                     Module.Owner(1) := I;
                     exit Module_Loop;
                  elsif
                    (Member.PreviousOrder = Craft and Module.M_Type = WORKSHOP)
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
               Member.Order := Member.PreviousOrder;
               Member.OrderTime := 15;
               AddMessage
                 (To_String(Member.Name) & " returns to work fully rested.",
                  OrderMessage, YELLOW);
               UpdateMorale(PlayerShip, I, 1);
            end if;
            Member.PreviousOrder := Rest;
         end if;
         if TiredLevel > (80 + Member.Attributes(Condition_Index)(1)) and
           Member.Order /= Rest and not InCombat then
            declare
               CanRest: Boolean := True;
            begin
               if Member.Order = Boarding and HarpoonDuration = 0 and
                 Combat.Enemy.HarpoonDuration = 0 then
                  CanRest := False;
               end if;
               if CanRest then
                  Member.PreviousOrder := Member.Order;
                  Member.Order := Rest;
                  Member.OrderTime := 15;
                  if Member.Equipment(7) > 0 then
                     UpdateCargo
                       (PlayerShip,
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
                  if FindCabin(I) = 0 then
                     Modules_Loop :
                     for Module of PlayerShip.Modules loop
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
                  UpdateMorale(PlayerShip, I, GetRandom(-5, -1));
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
               UpdateMorale(PlayerShip, I, GetRandom(-10, -5));
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
               UpdateMorale(PlayerShip, I, GetRandom(-20, -10));
            end if;
         end if;
         NormalizeStat(ThirstLevel);
         Member.Thirst := ThirstLevel;
         NormalizeStat(HealthLevel);
         Member.Health := HealthLevel;
         if Member.Order not in Repair | Craft | Upgrading then
            Member.OrderTime := OrderTime;
         end if;
         if Member.Skills.Length = 0 then
            Member.ContractLength := Member.ContractLength - Minutes;
            if Member.ContractLength < 0 then
               Member.ContractLength := 0;
            end if;
         end if;
      end UpdateMember;
   begin
      I := PlayerShip.Crew.First_Index;
      Update_Crew_Loop :
      while I <= PlayerShip.Crew.Last_Index loop
         CurrentMinutes := Minutes;
         OrderTime := PlayerShip.Crew(I).OrderTime;
         Times := 0;
         Update_Current_Minutes_Loop :
         while CurrentMinutes > 0 loop
            if CurrentMinutes >= OrderTime then
               CurrentMinutes := CurrentMinutes - OrderTime;
               Times := Times + 1;
               OrderTime := 15;
            else
               OrderTime := OrderTime - CurrentMinutes;
               CurrentMinutes := 0;
            end if;
         end loop Update_Current_Minutes_Loop;
         HealthLevel := PlayerShip.Crew(I).Health;
         HungerLevel := PlayerShip.Crew(I).Hunger;
         ThirstLevel := PlayerShip.Crew(I).Thirst;
         TiredLevel := PlayerShip.Crew(I).Tired;
         if Times = 0 then
            goto End_Of_Loop;
         end if;
         if PlayerShip.Crew(I).Order = Rest then
            CabinIndex := FindCabin(I);
            RestAmount := 0;
            if PlayerShip.Crew(I).Tired > 0 then
               if CabinIndex > 0 then
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(PlayerShip.Modules(CabinIndex).Durability) /
                       Float(PlayerShip.Modules(CabinIndex).Max_Durability));
                  RestAmount :=
                    PlayerShip.Modules(CabinIndex).Cleanliness -
                    Natural
                      (Float(PlayerShip.Modules(CabinIndex).Cleanliness) *
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
            if not Factions_List(PlayerShip.Crew(I).Faction).Flags.Contains
                (To_Unbounded_String("nofatigue")) and
              (HealthLevel in 1 .. 99) and CabinIndex > 0 then
               HealthLevel := HealthLevel + Times;
               if HealthLevel > 100 then
                  HealthLevel := 100;
               end if;
            end if;
            if PlayerShip.Crew(I).Morale(1) < 50 then
               UpdateMorale(PlayerShip, I, (Times + RestAmount));
               if PlayerShip.Crew(I).Morale(1) > 50 then
                  PlayerShip.Crew(I).Morale := (50, 0);
               end if;
            end if;
         else
            if PlayerShip.Crew(I).Order /= Talk then
               TiredLevel := TiredLevel + Times;
            end if;
            if TiredLevel >
              (100 + PlayerShip.Crew(I).Attributes(Condition_Index)(1)) then
               TiredLevel :=
                 (100 + PlayerShip.Crew(I).Attributes(Condition_Index)(1));
            end if;
            if TiredLevel >=
              (50 + PlayerShip.Crew(I).Attributes(Condition_Index)(1)) then
               UpdateMorale(PlayerShip, I, ((Times / 5) * (-1)));
            end if;
            case PlayerShip.Crew(I).Order is
               when Pilot =>
                  if PlayerShip.Speed /= DOCKED then
                     GainExp(Times, Piloting_Skill, I);
                  else
                     TiredLevel := PlayerShip.Crew(I).Tired;
                  end if;
               when Engineer =>
                  if PlayerShip.Speed /= DOCKED then
                     GainExp(Times, Engineering_Skill, I);
                  else
                     TiredLevel := PlayerShip.Crew(I).Tired;
                  end if;
               when Gunner =>
                  if PlayerShip.Speed = DOCKED then
                     TiredLevel := PlayerShip.Crew(I).Tired;
                  end if;
               when Heal =>
                  HaveMedicalRoom := False;
                  Heal_Module_Loop :
                  for Module of PlayerShip.Modules loop
                     if Modules_List(Module.Proto_Index).MType =
                       MEDICAL_ROOM and
                       Module.Durability > 0 and Module.Owner.Contains(I) then
                        HaveMedicalRoom := True;
                        exit Heal_Module_Loop;
                     end if;
                  end loop Heal_Module_Loop;
                  Heal_Loop :
                  for Member of PlayerShip.Crew loop
                     if Member.Name /= PlayerShip.Crew(I).Name and
                       Member.Health < 100 then
                        HealAmount :=
                          Times *
                          (GetSkillLevel
                             (PlayerShip.Crew(I),
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
                               (Inventory => PlayerShip.Cargo,
                                ItemType =>
                                  Factions_List(Member.Faction).HealingTools);
                           if ToolIndex > 0 then
                              HealAmount :=
                                (if
                                   PlayerShip.Cargo(ToolIndex).Amount <
                                   abs (HealAmount)
                                 then PlayerShip.Cargo(ToolIndex).Amount
                                 else abs (HealAmount));
                              UpdateCargo
                                (Ship => PlayerShip, Amount => -(HealAmount),
                                 CargoIndex => ToolIndex);
                           else
                              ToolIndex :=
                                FindItem
                                  (Inventory => PlayerShip.Crew(I).Inventory,
                                   ItemType =>
                                     Factions_List(Member.Faction)
                                       .HealingTools);
                              if ToolIndex > 0 then
                                 HealAmount :=
                                   (if
                                      PlayerShip.Crew(I).Inventory(ToolIndex)
                                        .Amount <
                                      abs (HealAmount)
                                    then
                                      PlayerShip.Crew(I).Inventory(ToolIndex)
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
                           for J in PlayerShip.Crew.Iterate loop
                              if PlayerShip.Crew(J).Health < 100 and
                                Crew_Container.To_Index(J) /= I then
                                 PlayerShip.Crew(J).Health :=
                                   (if
                                      PlayerShip.Crew(J).Health + HealAmount >
                                      Skill_Range'Last
                                    then Skill_Range'Last
                                    else PlayerShip.Crew(J).Health +
                                      HealAmount);
                                 AddMessage
                                   (To_String(PlayerShip.Crew(I).Name) &
                                    " healed " &
                                    To_String(PlayerShip.Crew(J).Name) &
                                    " a bit.",
                                    OrderMessage);
                                 GainExp
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
                                (To_String(PlayerShip.Crew(I).Name) &
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
                  for J in PlayerShip.Crew.Iterate loop
                     if PlayerShip.Crew(J).Health < 100 and
                       Crew_Container.To_Index(J) /= I then
                        HealAmount := 0;
                        ToolIndex :=
                          FindItem
                            (Inventory => PlayerShip.Cargo,
                             ItemType =>
                               Factions_List(PlayerShip.Crew(J).Faction)
                                 .HealingTools);
                        if ToolIndex = 0 then
                           ToolIndex :=
                             FindItem
                               (Inventory => PlayerShip.Crew(I).Inventory,
                                ItemType =>
                                  Factions_List(PlayerShip.Crew(J).Faction)
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
                       (To_String(PlayerShip.Crew(I).Name) &
                        " finished healing the wounded.",
                        OrderMessage, GREEN);
                  end if;
                  if HealAmount /= 0 then
                     GiveOrders(PlayerShip, I, Rest);
                  end if;
               when Clean =>
                  ToolIndex := FindTools(I, Cleaning_Tools, Clean);
                  NeedCleaning := False;
                  if ToolIndex > 0 then
                     Update_Clean_Tools_Loop :
                     for Module of PlayerShip.Modules loop
                        if Module.M_Type = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           Module.Cleanliness :=
                             (if Module.Cleanliness + Times > Module.Quality
                              then Module.Quality
                              else Module.Cleanliness + Times);
                           DamageItem
                             (Inventory => PlayerShip.Crew(I).Inventory,
                              ItemIndex => ToolIndex, MemberIndex => I);
                           exit Update_Clean_Tools_Loop;
                        end if;
                     end loop Update_Clean_Tools_Loop;
                     Check_Dirty_Modules_Loop :
                     for Module of PlayerShip.Modules loop
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
                     for J in PlayerShip.Crew.Iterate loop
                        if PlayerShip.Crew(J).Order = Clean then
                           GiveOrders
                             (PlayerShip, Crew_Container.To_Index(J), Rest);
                        end if;
                     end loop Remove_Clean_Order_Loop;
                  end if;
               when Talk =>
                  if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex =
                    0 then
                     GiveOrders(PlayerShip, I, Rest);
                  end if;
               when Train =>
                  Modules_Loop :
                  for Module of PlayerShip.Modules loop
                     if Module.M_Type = TRAINING_ROOM then
                        for Owner of Module.Owner loop
                           if Owner = I then
                              SkillIndex := Module.Trained_Skill;
                              exit Modules_Loop;
                           end if;
                        end loop;
                     end if;
                  end loop Modules_Loop;
                  if Skills_List(SkillIndex).Tool /= Null_Unbounded_String then
                     ToolIndex :=
                       FindTools
                         (I, Skills_List(SkillIndex).Tool, Train,
                          GetTrainingToolQuality(I, SkillIndex));
                     if ToolIndex > 0 then
                        Update_Train_Tool_Loop :
                        for J in 1 .. Times loop
                           GainExp(GetRandom(1, 5), SkillIndex, I);
                           DamageItem
                             (Inventory => PlayerShip.Crew(I).Inventory,
                              ItemIndex => ToolIndex, MemberIndex => I);
                           ToolIndex :=
                             FindTools(I, Skills_List(SkillIndex).Tool, Train);
                           exit Update_Train_Tool_Loop when ToolIndex = 0;
                        end loop Update_Train_Tool_Loop;
                        AddMessage
                          (To_String(PlayerShip.Crew(I).Name) &
                           " trained a little " &
                           To_String(Skills_List(SkillIndex).Name) & ".",
                           OrderMessage);
                     end if;
                     if ToolIndex = 0 then
                        AddMessage
                          (To_String(PlayerShip.Crew(I).Name) &
                           " can't continue training because they don't have the proper tools.",
                           OrderMessage, RED);
                        GiveOrders(PlayerShip, I, Rest);
                     end if;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         <<End_Of_Loop>>
         if TiredPoints > 0 then
            if Factions_List(PlayerShip.Crew(I).Faction).FoodTypes.Length >
              0 then
               HungerLevel :=
                 (if HungerLevel + TiredPoints > Skill_Range'Last then
                    Skill_Range'Last
                  else HungerLevel + TiredPoints);
               if PlayerShip.Crew(I).Hunger = Skill_Range'Last then
                  HealthLevel := HealthLevel - TiredPoints;
                  UpdateMorale(PlayerShip, I, -(TiredPoints));
                  if HealthLevel < 1 then
                     HealthLevel := Skill_Range'First;
                     DeathReason := To_Unbounded_String("starvation");
                  end if;
               end if;
            end if;
            if Factions_List(PlayerShip.Crew(I).Faction).DrinksTypes.Length >
              0 then
               ThirstLevel :=
                 (if ThirstLevel + TiredPoints > Skill_Range'Last then
                    Skill_Range'Last
                  else ThirstLevel + TiredPoints);
               if PlayerShip.Crew(I).Thirst = Skill_Range'Last then
                  HealthLevel := HealthLevel - TiredPoints;
                  UpdateMorale(PlayerShip, I, -(TiredPoints));
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
               Death(I, DeathReason, PlayerShip);
               exit Update_Crew_Loop when I = 1;
            end if;
         end if;
         if HealthLevel > Skill_Range'First then
            PlayerShip.Crew.Update_Element
              (Index => I, Process => UpdateMember'Access);
            I := I + 1;
         end if;
      end loop Update_Crew_Loop;
   end UpdateCrew;

   procedure WaitForRest is
      CabinIndex: Modules_Container.Extended_Index := 0;
      TimeNeeded, TempTimeNeeded: Natural := 0;
      Damage: Damage_Factor := 0.0;
      CabinBonus: Natural;
   begin
      Wait_For_Rest_Loop :
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            CabinIndex := 0;
            TempTimeNeeded := 0;
            CabinIndex := FindCabin(Crew_Container.To_Index(I));
            if CabinIndex > 0 then
               Damage :=
                 1.0 -
                 Damage_Factor
                   (Float(PlayerShip.Modules(CabinIndex).Durability) /
                    Float(PlayerShip.Modules(CabinIndex).Max_Durability));
               CabinBonus :=
                 PlayerShip.Modules(CabinIndex).Cleanliness -
                 Natural
                   (Float(PlayerShip.Modules(CabinIndex).Cleanliness) *
                    Float(Damage));
               if CabinBonus = 0 then
                  CabinBonus := 1;
               end if;
               TempTimeNeeded := (PlayerShip.Crew(I).Tired / CabinBonus) * 15;
               if TempTimeNeeded = 0 then
                  TempTimeNeeded := 15;
               end if;
            else
               TempTimeNeeded := PlayerShip.Crew(I).Tired * 15;
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
   end WaitForRest;

   function GetSkillLevelName(SkillLevel: Skill_Range) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Positive'Image(SkillLevel);
      end if;
      case SkillLevel is
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
   end GetSkillLevelName;

   function GetAttributeLevelName(AttributeLevel: Positive) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Positive'Image(AttributeLevel);
      end if;
      case AttributeLevel is
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
   end GetAttributeLevelName;

   procedure DailyPayment is
      MoneyIndex2: constant Inventory_Container.Extended_Index :=
        FindItem(PlayerShip.Cargo, Money_Index);
      PayMessage: Unbounded_String;
      MemberIndex: Crew_Container.Extended_Index;
      HaveMoney: Boolean := True;
      MoneyNeeded: Natural;
   begin
      MemberIndex := 1;
      Daily_Payment_Loop :
      for Member of PlayerShip.Crew loop
         if Member.Payment(1) > 0 then
            if MoneyIndex2 = 0 and HaveMoney then
               AddMessage
                 ("You don't have any " & To_String(Money_Name) &
                  " to pay your crew members.",
                  TradeMessage, RED);
               HaveMoney := False;
            end if;
            if HaveMoney then
               if PlayerShip.Cargo(MoneyIndex2).Amount < Member.Payment(1) then
                  MoneyNeeded := PlayerShip.Cargo(MoneyIndex2).Amount;
                  UpdateCargo
                    (Ship => PlayerShip, ProtoIndex => Money_Index,
                     Amount => (0 - MoneyNeeded));
                  AddMessage
                    ("You don't have enough " & To_String(Money_Name) &
                     " to pay your crew members.",
                     TradeMessage, RED);
                  HaveMoney := False;
               end if;
               if HaveMoney then
                  UpdateCargo
                    (Ship => PlayerShip, CargoIndex => MoneyIndex2,
                     Amount => (0 - Member.Payment(1)));
                  PayMessage := To_Unbounded_String("You pay ") & Member.Name;
                  if Member.Gender = 'M' then
                     Append(PayMessage, " his ");
                  else
                     Append(PayMessage, " her ");
                  end if;
                  Append(PayMessage, "daily payment.");
                  AddMessage(To_String(PayMessage), TradeMessage);
                  UpdateMorale(PlayerShip, MemberIndex, GetRandom(1, 5));
               end if;
            end if;
            if not HaveMoney then
               UpdateMorale(PlayerShip, MemberIndex, GetRandom(-50, -10));
            end if;
         end if;
         MemberIndex := MemberIndex + 1;
      end loop Daily_Payment_Loop;
      MemberIndex := 1;
      Update_Contracts_Loop :
      while MemberIndex <= PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(MemberIndex).ContractLength > 0 then
            PlayerShip.Crew(MemberIndex).ContractLength :=
              PlayerShip.Crew(MemberIndex).ContractLength - 1;
            if PlayerShip.Crew(MemberIndex).ContractLength = 0 then
               AddMessage
                 ("Your contract with " &
                  To_String(PlayerShip.Crew(MemberIndex).Name) & " has ended.",
                  TradeMessage, RED);
               if PlayerShip.Speed /= DOCKED then
                  PlayerShip.Crew(MemberIndex).Orders := (others => 0);
                  GiveOrders(PlayerShip, MemberIndex, Rest);
               else
                  DeleteMember(MemberIndex, PlayerShip);
                  SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
                    .Population :=
                    SkyBases
                      (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
                      .Population +
                    1;
                  MemberIndex := MemberIndex - 1;
               end if;
            end if;
         end if;
         MemberIndex := MemberIndex + 1;
      end loop Update_Contracts_Loop;
   end DailyPayment;

   function GetTrainingToolQuality
     (MemberIndex, SkillIndex: Positive) return Positive is
      ToolQuality: Positive := 100;
   begin
      Skill_Loop :
      for Skill of PlayerShip.Crew(MemberIndex).Skills loop
         if Skill(1) = SkillIndex then
            Tool_Quality_Loop :
            for Quality of Skills_List(SkillIndex).Tools_Quality loop
               if Skill(2) <= Quality(1) then
                  ToolQuality := Quality(2);
                  exit Skill_Loop;
               end if;
            end loop Tool_Quality_Loop;
         end if;
      end loop Skill_Loop;
      return ToolQuality;
   end GetTrainingToolQuality;

end Crew;
