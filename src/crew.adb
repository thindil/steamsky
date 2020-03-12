--    Copyright 2016-2019 Bartek thindil Jasicki
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
      SkillExp, SkillLevel, SkillIndex, AttributeExp, AttributeLevel,
      NewAmount: Natural := 0;
      AttributeIndex: constant Positive := Skills_List(SkillNumber).Attribute;
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
      if Careers_List(PlayerCareer).Skills.Contains
          (Skills_List(SkillNumber).Name) then
         NewAmount := Amount + (Amount / 2);
      else
         NewAmount := Amount;
      end if;
      NewAmount := Natural(Float(NewAmount) * NewGameSettings.ExperienceBonus);
      if NewAmount = 0 then
         return;
      end if;
      -- Gain experience in condition assigned attribute
      GainExpInAttribute(ConditionIndex);
      -- Gain experience in associated attribute
      GainExpInAttribute(AttributeIndex);
      -- Gain experience in skill
      for I in PlayerShip.Crew(CrewIndex).Skills.Iterate loop
         if PlayerShip.Crew(CrewIndex).Skills(I)(1) = SkillNumber then
            SkillIndex := Skills_Container.To_Index(I);
            exit;
         end if;
      end loop;
      if SkillIndex > 0 then
         if PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(2) = 100 then
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
      NewName: Unbounded_String;
      NameType: NamesTypes;
   begin
      NameType := Factions_List(FactionIndex).NamesType;
      NewName := Null_Unbounded_String;
      if NameType = Factions.STANDARD then
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
                       (MaleConsonants.First_Index,
                        MaleConsonants.Last_Index)));
            end if;
            Append
              (NewName,
               MaleSyllablesEnd
                 (GetRandom
                    (MaleSyllablesEnd.First_Index,
                     MaleSyllablesEnd.Last_Index)));
         else
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
         end if;
      else
         NewName := GenerateRoboticName;
      end if;
      return NewName;
   end GenerateMemberName;

   function FindCabin(MemberIndex: Positive) return Natural is
   begin
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).MType = CABIN then
            for Owner of PlayerShip.Modules(I).Owner loop
               if Owner = MemberIndex then
                  return Modules_Container.To_Index(I);
               end if;
            end loop;
         end if;
      end loop;
      return 0;
   end FindCabin;

   procedure UpdateCrew
     (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False) is
      TiredLevel, HungerLevel, ThirstLevel: Integer := 0;
      HealthLevel: Integer := 100;
      DeathReason: Unbounded_String;
      CabinIndex, Times, RestAmount, I, ToolIndex: Natural;
      OrderTime, CurrentMinutes, HealAmount: Integer;
      Damage: DamageFactor := 0.0;
      NeedCleaning, HaveMedicalRoom: Boolean := False;
      SkillIndex: Positive;
      function Consume(ItemType: Unbounded_String) return Natural is
         ConsumeValue, ItemIndex: Natural;
      begin
         ItemIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => ItemType);
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
         else
            ItemIndex :=
              FindItem
                (Inventory => PlayerShip.Crew(I).Inventory,
                 ItemType => ItemType);
            if ItemIndex > 0 then
               ConsumeValue :=
                 Items_List(PlayerShip.Crew(I).Inventory(ItemIndex).ProtoIndex)
                   .Value
                   (1);
               if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value
                   (2) /=
                 0 then
                  UpdateMorale
                    (PlayerShip, I,
                     Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value
                       (2));
               end if;
               UpdateInventory
                 (MemberIndex => I, Amount => -1, InventoryIndex => ItemIndex);
               return ConsumeValue;
            end if;
         end if;
         return 0;
      end Consume;
      procedure UpdateMember(Member: in out Member_Data) is
         BackToWork: Boolean := True;
         ConsumeResult: Natural := 0;
      begin
         if Factions_List(Member.Faction).Flags.Contains
             (To_Unbounded_String("nofatigue")) then
            TiredLevel := 0;
         end if;
         Member.Tired := TiredLevel;
         if TiredLevel = 0 and Member.Order = Rest and
           Member.PreviousOrder /= Rest then
            if Member.PreviousOrder /= Repair and
              Member.PreviousOrder /= Clean then
               if FindMember(Member.PreviousOrder) > 0 then
                  BackToWork := False;
               end if;
            end if;
            if
              (Member.PreviousOrder = Gunner or
               Member.PreviousOrder = Craft) then
               Module_Loop :
               for Module of PlayerShip.Modules loop
                  if (Member.PreviousOrder = Gunner and Module.MType = GUN)
                    and then (Module.Owner(1) = I or Module.Owner(1) = 0) then
                     BackToWork := True;
                     Module.Owner(1) := I;
                     exit;
                  elsif
                    (Member.PreviousOrder = Craft and Module.MType = WORKSHOP)
                    and then Module.CraftingIndex /= Null_Unbounded_String then
                     for Owner of Module.Owner loop
                        if Owner = I then
                           BackToWork := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop;
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           BackToWork := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop;
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
         if TiredLevel > (80 + Member.Attributes(ConditionIndex)(1)) and
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
                  AddMessage
                    (To_String(Member.Name) &
                     " is too tired to work, going to rest.",
                     OrderMessage, YELLOW);
                  if FindCabin(I) = 0 then
                     Modules_Loop :
                     for Module of PlayerShip.Modules loop
                        if Module.MType = CABIN and Module.Durability > 0 then
                           for Owner of Module.Owner loop
                              if Owner = 0 then
                                 Owner := I;
                                 AddMessage
                                   (To_String(Member.Name) & " take " &
                                    To_String(Module.Name) & " as own cabin.",
                                    OtherMessage);
                                 exit Modules_Loop;
                              end if;
                           end loop;
                        end if;
                     end loop Modules_Loop;
                  end if;
               else
                  AddMessage
                    (To_String(Member.Name) &
                     " is very tired but can't go to rest.",
                     OrderMessage, RED);
                  UpdateMorale(PlayerShip, I, GetRandom(-5, -1));
               end if;
            end;
         end if;
         if HungerLevel > 80 then
            for FoodType of Factions_List(Member.Faction).FoodTypes loop
               ConsumeResult := Consume(FoodType);
               exit when ConsumeResult > 0;
            end loop;
            HungerLevel := HungerLevel - ConsumeResult;
            if HungerLevel < 0 then
               HungerLevel := 0;
            end if;
            if ConsumeResult = 0 then
               AddMessage
                 (To_String(Member.Name) &
                  " is hungry, but can't find anything to eat.",
                  OtherMessage, RED);
               UpdateMorale(PlayerShip, I, GetRandom(-10, -5));
            end if;
         end if;
         Member.Hunger := HungerLevel;
         if ThirstLevel > 40 then
            for DrinksType of Factions_List(Member.Faction).DrinksTypes loop
               ConsumeResult := Consume(DrinksType);
               exit when ConsumeResult > 0;
            end loop;
            ThirstLevel := ThirstLevel - ConsumeResult;
            if ThirstLevel < 0 then
               ThirstLevel := 0;
            end if;
            if ConsumeResult = 0 then
               AddMessage
                 (To_String(Member.Name) &
                  " is thirsty, but can't find anything to drink.",
                  OtherMessage, RED);
               UpdateMorale(PlayerShip, I, GetRandom(-20, -10));
            end if;
         end if;
         Member.Thirst := ThirstLevel;
         Member.Health := HealthLevel;
         if Member.Order /= Repair and Member.Order /= Craft and
           Member.Order /= Upgrading then
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
      while I <= PlayerShip.Crew.Last_Index loop
         CurrentMinutes := Minutes;
         OrderTime := PlayerShip.Crew(I).OrderTime;
         Times := 0;
         while CurrentMinutes > 0 loop
            if CurrentMinutes >= OrderTime then
               CurrentMinutes := CurrentMinutes - OrderTime;
               Times := Times + 1;
               OrderTime := 15;
            else
               OrderTime := OrderTime - CurrentMinutes;
               CurrentMinutes := 0;
            end if;
         end loop;
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
                    DamageFactor
                      (Float(PlayerShip.Modules(CabinIndex).Durability) /
                       Float(PlayerShip.Modules(CabinIndex).MaxDurability));
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
              HealthLevel > 0 and HealthLevel < 100 and CabinIndex > 0 then
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
              (100 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) then
               TiredLevel :=
                 (100 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1));
            end if;
            if TiredLevel >=
              (50 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) then
               UpdateMorale(PlayerShip, I, ((Times / 5) * (-1)));
            end if;
            case PlayerShip.Crew(I).Order is
               when Pilot =>
                  if PlayerShip.Speed /= DOCKED then
                     GainExp(Times, PilotingSkill, I);
                  else
                     TiredLevel := PlayerShip.Crew(I).Tired;
                  end if;
               when Engineer =>
                  if PlayerShip.Speed /= DOCKED then
                     GainExp(Times, EngineeringSkill, I);
                  else
                     TiredLevel := PlayerShip.Crew(I).Tired;
                  end if;
               when Gunner =>
                  if PlayerShip.Speed = DOCKED then
                     TiredLevel := PlayerShip.Crew(I).Tired;
                  end if;
               when Heal =>
                  HaveMedicalRoom := False;
                  for Module of PlayerShip.Modules loop
                     if Modules_List(Module.ProtoIndex).MType =
                       MEDICAL_ROOM and
                       Module.Durability > 0 and Module.Owner.Contains(I) then
                        HaveMedicalRoom := True;
                        exit;
                     end if;
                  end loop;
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
                              if PlayerShip.Cargo(ToolIndex).Amount <
                                abs (HealAmount) then
                                 HealAmount :=
                                   PlayerShip.Cargo(ToolIndex).Amount;
                              else
                                 HealAmount := abs (HealAmount);
                              end if;
                              UpdateCargo
                                (Ship => PlayerShip,
                                 Amount => (0 - HealAmount),
                                 CargoIndex => ToolIndex);
                           else
                              ToolIndex :=
                                FindItem
                                  (Inventory => PlayerShip.Crew(I).Inventory,
                                   ItemType =>
                                     Factions_List(Member.Faction)
                                       .HealingTools);
                              if ToolIndex > 0 then
                                 if PlayerShip.Crew(I).Inventory(ToolIndex)
                                     .Amount <
                                   abs (HealAmount) then
                                    HealAmount :=
                                      PlayerShip.Crew(I).Inventory(ToolIndex)
                                        .Amount;
                                 else
                                    HealAmount := abs (HealAmount);
                                 end if;
                                 UpdateInventory
                                   (MemberIndex => I,
                                    Amount => (0 - HealAmount),
                                    InventoryIndex => ToolIndex);
                              end if;
                           end if;
                        end if;
                        if HealAmount > 0 then
                           for J in PlayerShip.Crew.Iterate loop
                              if PlayerShip.Crew(J).Health < 100 and
                                Crew_Container.To_Index(J) /= I then
                                 PlayerShip.Crew(J).Health :=
                                   PlayerShip.Crew(J).Health + HealAmount;
                                 if PlayerShip.Crew(J).Health > 100 then
                                    PlayerShip.Crew(J).Health := 100;
                                 end if;
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
                                 exit;
                              end if;
                           end loop;
                        else
                           AddMessage
                             ("You don't have any " &
                              To_String
                                (Factions_List(Member.Faction).HealingTools) &
                              " to continue healing wounded " &
                              To_String(Member.Name) & ".",
                              OrderMessage, RED);
                        end if;
                     end if;
                  end loop;
                  HealAmount := 1;
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
                        exit;
                     end if;
                  end loop;
                  if HealAmount > 0 then
                     AddMessage
                       (To_String(PlayerShip.Crew(I).Name) &
                        " finished healing wounded.",
                        OrderMessage, GREEN);
                  end if;
                  if HealAmount /= 0 then
                     GiveOrders(PlayerShip, I, Rest);
                  end if;
               when Clean =>
                  ToolIndex := FindTools(I, CleaningTools, Clean);
                  NeedCleaning := False;
                  if ToolIndex > 0 then
                     for Module of PlayerShip.Modules loop
                        if Module.MType = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           if Module.Cleanliness + Times > Module.Quality then
                              Module.Cleanliness := Module.Quality;
                           else
                              Module.Cleanliness := Module.Cleanliness + Times;
                           end if;
                           DamageItem
                             (Inventory => PlayerShip.Crew(I).Inventory,
                              ItemIndex => ToolIndex, MemberIndex => I);
                           exit;
                        end if;
                     end loop;
                     for Module of PlayerShip.Modules loop
                        if Module.MType = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           NeedCleaning := True;
                           exit;
                        end if;
                     end loop;
                  end if;
                  if not NeedCleaning then
                     if ToolIndex = 0 then
                        AddMessage
                          ("You can't continue cleaning ship because you don't have any cleaning tools.",
                           OrderMessage, RED);
                     end if;
                     for J in PlayerShip.Crew.Iterate loop
                        if PlayerShip.Crew(J).Order = Clean then
                           GiveOrders
                             (PlayerShip, Crew_Container.To_Index(J), Rest);
                        end if;
                     end loop;
                  end if;
               when Talk =>
                  if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex =
                    0 then
                     GiveOrders(PlayerShip, I, Rest);
                  end if;
               when Train =>
                  Modules_Loop :
                  for Module of PlayerShip.Modules loop
                     if Module.MType = TRAINING_ROOM then
                        for Owner of Module.Owner loop
                           if Owner = I then
                              SkillIndex := Module.TrainedSkill;
                              exit Modules_Loop;
                           end if;
                        end loop;
                     end if;
                  end loop Modules_Loop;
                  if Skills_List(SkillIndex).Tool /= Null_Unbounded_String then
                     ToolIndex :=
                       FindTools(I, Skills_List(SkillIndex).Tool, Train);
                     if ToolIndex > 0 then
                        for J in 1 .. Times loop
                           GainExp(GetRandom(1, 5), SkillIndex, I);
                           DamageItem
                             (Inventory => PlayerShip.Crew(I).Inventory,
                              ItemIndex => ToolIndex, MemberIndex => I);
                           ToolIndex :=
                             FindTools(I, Skills_List(SkillIndex).Tool, Train);
                           exit when ToolIndex = 0;
                        end loop;
                        AddMessage
                          (To_String(PlayerShip.Crew(I).Name) &
                           " trained a little " &
                           To_String(Skills_List(SkillIndex).Name) & ".",
                           OrderMessage);
                     end if;
                     if ToolIndex = 0 then
                        AddMessage
                          (To_String(PlayerShip.Crew(I).Name) &
                           " can't continue training because you don't have proper tools.",
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
               HungerLevel := HungerLevel + TiredPoints;
               if HungerLevel > 100 then
                  HungerLevel := 100;
               end if;
               if PlayerShip.Crew(I).Hunger = 100 then
                  HealthLevel := HealthLevel - TiredPoints;
                  UpdateMorale(PlayerShip, I, (0 - TiredPoints));
                  if HealthLevel < 1 then
                     HealthLevel := 0;
                     DeathReason := To_Unbounded_String("starvation");
                  end if;
               end if;
            end if;
            if Factions_List(PlayerShip.Crew(I).Faction).DrinksTypes.Length >
              0 then
               ThirstLevel := ThirstLevel + TiredPoints;
               if ThirstLevel > 100 then
                  ThirstLevel := 100;
               end if;
               if PlayerShip.Crew(I).Thirst = 100 then
                  HealthLevel := HealthLevel - TiredPoints;
                  UpdateMorale(PlayerShip, I, (0 - TiredPoints));
                  if HealthLevel < 1 then
                     HealthLevel := 0;
                     DeathReason := To_Unbounded_String("dehydration");
                  end if;
               end if;
            end if;
            if HealthLevel = 0 then
               Death(I, DeathReason, PlayerShip);
               exit when I = 1;
            end if;
         end if;
         if HealthLevel > 0 then
            PlayerShip.Crew.Update_Element
              (Index => I, Process => UpdateMember'Access);
            I := I + 1;
         end if;
      end loop;
   end UpdateCrew;

   procedure WaitForRest is
      TimeNeeded, CabinIndex, TempTimeNeeded: Natural := 0;
      Damage: DamageFactor := 0.0;
      CabinBonus: Natural;
   begin
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            CabinIndex := 0;
            TempTimeNeeded := 0;
            CabinIndex := FindCabin(Crew_Container.To_Index(I));
            if CabinIndex > 0 then
               Damage :=
                 1.0 -
                 DamageFactor
                   (Float(PlayerShip.Modules(CabinIndex).Durability) /
                    Float(PlayerShip.Modules(CabinIndex).MaxDurability));
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
      end loop;
      if TimeNeeded > 0 then
         UpdateGame(TimeNeeded);
         WaitInPlace(TimeNeeded);
      end if;
   end WaitForRest;

   function GetSkillLevelName(SkillLevel: Positive) return String is
   begin
      if GameSettings.ShowNumbers then
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
      if GameSettings.ShowNumbers then
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
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      PayMessage: Unbounded_String;
      MemberIndex: Positive;
      HaveMoney: Boolean := True;
   begin
      MemberIndex := 1;
      for Member of PlayerShip.Crew loop
         if Member.Payment(1) > 0 then
            if MoneyIndex2 = 0 and HaveMoney then
               AddMessage
                 ("You don't have " & To_String(MoneyName) &
                  " to pay your crew members.",
                  TradeMessage, RED);
               HaveMoney := False;
            end if;
            if HaveMoney then
               if PlayerShip.Cargo(MoneyIndex2).Amount < Member.Payment(1) then
                  UpdateCargo
                    (Ship => PlayerShip, ProtoIndex => MoneyIndex,
                     Amount => (0 - PlayerShip.Cargo(MoneyIndex2).Amount));
                  AddMessage
                    ("You don't have enough " & To_String(MoneyName) &
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
                  Append(PayMessage, " daily payment.");
                  AddMessage(To_String(PayMessage), TradeMessage);
                  UpdateMorale(PlayerShip, MemberIndex, GetRandom(1, 5));
               end if;
            end if;
            if not HaveMoney then
               UpdateMorale(PlayerShip, MemberIndex, GetRandom(-50, -10));
            end if;
         end if;
         MemberIndex := MemberIndex + 1;
      end loop;
      MemberIndex := 1;
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
      end loop;
   end DailyPayment;

end Crew;
