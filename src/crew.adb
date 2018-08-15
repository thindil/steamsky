--    Copyright 2016-2018 Bartek thindil Jasicki
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
with Maps; use Maps;
with Crew.Inventory; use Crew.Inventory;
with Combat; use Combat;

package body Crew is

   procedure GainExp(Amount: Natural; SkillNumber, CrewIndex: Positive) is
      SkillExp,
      SkillLevel,
      SkillIndex,
      AttributeExp,
      AttributeLevel: Natural :=
        0;
      AttributeIndex: constant Positive := Skills_List(SkillNumber).Attribute;
      procedure GainExpInAttribute(Attribute: Positive) is
      begin
         if PlayerShip.Crew(CrewIndex).Attributes(Attribute)(1) = 50 then
            return;
         end if;
         AttributeExp :=
           PlayerShip.Crew(CrewIndex).Attributes(Attribute)(2) + Amount;
         AttributeLevel := PlayerShip.Crew(CrewIndex).Attributes(Attribute)(1);
         if AttributeExp >= (AttributeLevel * 500) then
            AttributeExp := AttributeExp - (AttributeLevel * 500);
            AttributeLevel := AttributeLevel + 1;
         end if;
         PlayerShip.Crew(CrewIndex).Attributes(Attribute)(1) := AttributeLevel;
         PlayerShip.Crew(CrewIndex).Attributes(Attribute)(2) := AttributeExp;
      end GainExpInAttribute;
   begin
      -- Gain experience in condition assigned attribute
      GainExpInAttribute(ConditionIndex);
      -- Gain experience in associated attribute
      GainExpInAttribute(AttributeIndex);
      -- Gain experience in skill
      for I in
        PlayerShip.Crew(CrewIndex).Skills.First_Index ..
            PlayerShip.Crew(CrewIndex).Skills.Last_Index loop
         if PlayerShip.Crew(CrewIndex).Skills(I)(1) = SkillNumber then
            SkillIndex := I;
         end if;
      end loop;
      if SkillIndex > 0 then
         if PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(2) = 100 then
            return;
         end if;
         SkillLevel := PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(2);
         SkillExp := PlayerShip.Crew(CrewIndex).Skills(SkillIndex)(3) + Amount;
      end if;
      if SkillExp >= (SkillLevel * 50) then
         SkillExp := SkillExp - (SkillLevel * 50);
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
     (Gender: Character)
     return Unbounded_String is -- based on name generator from libtcod
      NewName: Unbounded_String;
   begin
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
      return NewName;
   end GenerateMemberName;

   procedure UpdateCrew(Minutes: Positive; TiredPoints: Natural) is
      TiredLevel, HungerLevel, ThirstLevel: Integer := 0;
      HealthLevel: Integer := 100;
      DeathReason: Unbounded_String;
      CabinIndex, Times, RestAmount, I, ToolIndex: Natural;
      OrderTime, CurrentMinutes, HealAmount: Integer;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      NeedCleaning, HaveMedicalRoom: Boolean := False;
      function Consume(ItemType: Unbounded_String) return Natural is
         ConsumeValue, ItemIndex: Natural;
      begin
         ItemIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => ItemType);
         if ItemIndex > 0 then
            ConsumeValue :=
              Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value(1);
            UpdateCargo
              (PlayerShip,
               PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
               -1);
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
               UpdateInventory
                 (MemberIndex => I,
                  Amount => -1,
                  InventoryIndex => ItemIndex);
               return ConsumeValue;
            end if;
         end if;
         return 0;
      end Consume;
      procedure UpdateMember(Member: in out Member_Data) is
         BackToWork: Boolean := True;
         ConsumeResult: Natural := 0;
      begin
         Member.Tired := TiredLevel;
         if TiredLevel = 0 and
           Member.Order = Rest and
           Member.PreviousOrder /= Rest then
            if Member.PreviousOrder /= Repair and
              Member.PreviousOrder /= Clean then
               if FindMember(Member.PreviousOrder) > 0 then
                  BackToWork := False;
               end if;
            end if;
            if Member.PreviousOrder = Gunner and not BackToWork then
               for Module of PlayerShip.Modules loop
                  if Modules_List(Module.ProtoIndex).MType = GUN and
                    Module.Owner = I then
                     BackToWork := True;
                     exit;
                  end if;
               end loop;
            end if;
            if BackToWork then
               Member.Order := Member.PreviousOrder;
               Member.OrderTime := 15;
               AddMessage
                 (To_String(Member.Name) & " back to work, fully rested.",
                  OrderMessage,
                  1);
            end if;
            Member.PreviousOrder := Rest;
         end if;
         if TiredLevel > (80 + Member.Attributes(ConditionIndex)(1)) and
           Member.Order /= Rest then
            declare
               HaveCabin: Boolean := False;
               CanRest: Boolean := True;
            begin
               if Member.Order = Boarding and
                 HarpoonDuration = 0 and
                 Enemy.HarpoonDuration = 0 then
                  CanRest := False;
               end if;
               if CanRest then
                  Member.PreviousOrder := Member.Order;
                  Member.Order := Rest;
                  Member.OrderTime := 15;
                  AddMessage
                    (To_String(Member.Name) &
                     " is too tired to work, going rest.",
                     OrderMessage,
                     1);
                  for Module of PlayerShip.Modules loop
                     if Module.Owner = I and
                       Modules_List(Module.ProtoIndex).MType = CABIN then
                        HaveCabin := True;
                        exit;
                     end if;
                  end loop;
                  if not HaveCabin then
                     for Module of PlayerShip.Modules loop
                        if Modules_List(Module.ProtoIndex).MType = CABIN and
                          Module.Durability > 0 and
                          Module.Owner = 0 then
                           Module.Owner := I;
                           AddMessage
                             (To_String(Member.Name) &
                              " take " &
                              To_String(Module.Name) &
                              " as own cabin.",
                              OtherMessage);
                           exit;
                        end if;
                     end loop;
                  end if;
               end if;
            end;
         end if;
         if HungerLevel > 80 then
            for FoodType of FoodTypes loop
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
                  OtherMessage,
                  3);
            end if;
         end if;
         Member.Hunger := HungerLevel;
         if ThirstLevel > 40 then
            ConsumeResult := Consume(DrinksType);
            ThirstLevel := ThirstLevel - ConsumeResult;
            if ThirstLevel < 0 then
               ThirstLevel := 0;
            end if;
            if ConsumeResult = 0 then
               AddMessage
                 (To_String(Member.Name) &
                  " is thirsty, but can't find anything to drink.",
                  OtherMessage,
                  3);
            end if;
         end if;
         Member.Thirst := ThirstLevel;
         Member.Health := HealthLevel;
         if Member.Order /= Repair and
           Member.Order /= Craft and
           Member.Order /= Upgrading then
            Member.OrderTime := OrderTime;
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
         if Times > 0 then
            if PlayerShip.Crew(I).Order = Rest then
               CabinIndex := 0;
               for J in PlayerShip.Modules.Iterate loop
                  if Modules_List(PlayerShip.Modules(J).ProtoIndex).MType =
                    CABIN and
                    PlayerShip.Modules(J).Owner = I then
                     CabinIndex := Modules_Container.To_Index(J);
                     exit;
                  end if;
               end loop;
               if PlayerShip.Crew(I).Tired > 0 then
                  if CabinIndex > 0 then
                     Damage :=
                       1.0 -
                       DamageFactor
                         (Float(PlayerShip.Modules(CabinIndex).Durability) /
                          Float(PlayerShip.Modules(CabinIndex).MaxDurability));
                     RestAmount :=
                       PlayerShip.Modules(CabinIndex).Data(1) -
                       Natural
                         (Float(PlayerShip.Modules(CabinIndex).Data(1)) *
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
               if HealthLevel > 0 and HealthLevel < 100 and CabinIndex > 0 then
                  HealthLevel := HealthLevel + Times;
                  if HealthLevel > 100 then
                     HealthLevel := 100;
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
               case PlayerShip.Crew(I).Order is
                  when Pilot =>
                     GainExp(Times, PilotingSkill, I);
                  when Engineer =>
                     GainExp(Times, EngineeringSkill, I);
                  when Heal =>
                     HealAmount :=
                       Times *
                       (GetSkillLevel(PlayerShip.Crew(I), HealingSkill) / 20);
                     if HealAmount < Times then
                        HealAmount := Times;
                     end if;
                     HaveMedicalRoom := False;
                     for Module of PlayerShip.Modules loop
                        if Modules_List(Module.ProtoIndex).MType =
                          MEDICAL_ROOM and
                          Module.Durability > 0 then
                           HaveMedicalRoom := True;
                           exit;
                        end if;
                     end loop;
                     if not HaveMedicalRoom then
                        HealAmount := HealAmount / 2;
                     end if;
                     if HealAmount > 0 then
                        HealAmount := HealAmount * (-1);
                        ToolIndex :=
                          FindItem
                            (Inventory => PlayerShip.Cargo,
                             ItemType => HealingTools);
                        if ToolIndex > 0 then
                           if PlayerShip.Cargo(ToolIndex).Amount <
                             abs (HealAmount) then
                              HealAmount := PlayerShip.Cargo(ToolIndex).Amount;
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
                                ItemType => HealingTools);
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
                              GainExp(Times, HealingSkill, I);
                              exit;
                           end if;
                        end loop;
                        for J in PlayerShip.Crew.Iterate loop
                           if PlayerShip.Crew(J).Health < 100 and
                             Crew_Container.To_Index(J) /= I then
                              HealAmount := 0;
                              exit;
                           end if;
                        end loop;
                        if HealAmount > 0 then
                           AddMessage
                             (To_String(PlayerShip.Crew(I).Name) &
                              " finished healing wounded.",
                              OrderMessage,
                              2);
                        end if;
                     else
                        AddMessage
                          ("You don't have any " &
                           To_String(HealingTools) &
                           " to continue healing wounded crew members.",
                           OrderMessage,
                           3);
                     end if;
                     if HealAmount /= 0 then
                        GiveOrders(PlayerShip, I, Rest);
                     end if;
                  when Clean =>
                     ToolIndex := FindTools(I, CleaningTools, Clean);
                     NeedCleaning := False;
                     if ToolIndex > 0 then
                        for Module of PlayerShip.Modules loop
                           if Modules_List(Module.ProtoIndex).MType = CABIN and
                             Module.Data(1) < Module.Data(2) then
                              if Module.Data(1) + Times > Module.Data(2) then
                                 Module.Data(1) := Module.Data(2);
                              else
                                 Module.Data(1) := Module.Data(1) + Times;
                              end if;
                              DamageItem
                                (Inventory => PlayerShip.Crew(I).Inventory,
                                 ItemIndex => ToolIndex,
                                 MemberIndex => I);
                              exit;
                           end if;
                        end loop;
                        for Module of PlayerShip.Modules loop
                           if Modules_List(Module.ProtoIndex).MType = CABIN and
                             Module.Data(1) < Module.Data(2) then
                              NeedCleaning := True;
                              exit;
                           end if;
                        end loop;
                     end if;
                     if not NeedCleaning then
                        if ToolIndex = 0 then
                           AddMessage
                             ("You can't continue cleaning ship because you don't have any cleaning tools.",
                              OrderMessage,
                              3);
                        end if;
                        for J in PlayerShip.Crew.Iterate loop
                           if PlayerShip.Crew(J).Order = Clean then
                              GiveOrders
                                (PlayerShip,
                                 Crew_Container.To_Index(J),
                                 Rest);
                           end if;
                        end loop;
                     end if;
                  when Talk =>
                     if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex =
                       0 then
                        GiveOrders(PlayerShip, I, Rest);
                     end if;
                  when others =>
                     null;
               end case;
            end if;
         end if;
         if TiredPoints > 0 then
            HungerLevel := HungerLevel + TiredPoints;
            if HungerLevel > 100 then
               HungerLevel := 100;
            end if;
            if PlayerShip.Crew(I).Hunger = 100 then
               HealthLevel := HealthLevel - TiredPoints;
               if HealthLevel < 1 then
                  HealthLevel := 0;
                  DeathReason := To_Unbounded_String("starvation");
               end if;
            end if;
            ThirstLevel := ThirstLevel + TiredPoints;
            if ThirstLevel > 100 then
               ThirstLevel := 100;
            end if;
            if PlayerShip.Crew(I).Thirst = 100 then
               HealthLevel := HealthLevel - TiredPoints;
               if HealthLevel < 1 then
                  HealthLevel := 0;
                  DeathReason := To_Unbounded_String("dehydration");
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
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      CabinBonus: Natural;
   begin
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            CabinIndex := 0;
            TempTimeNeeded := 0;
            for J in PlayerShip.Modules.Iterate loop
               if Modules_List(PlayerShip.Modules(J).ProtoIndex).MType =
                 CABIN and
                 PlayerShip.Modules(J).Owner = Crew_Container.To_Index(I) then
                  CabinIndex := Modules_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            if CabinIndex > 0 then
               Damage :=
                 1.0 -
                 DamageFactor
                   (Float(PlayerShip.Modules(CabinIndex).Durability) /
                    Float(PlayerShip.Modules(CabinIndex).MaxDurability));
               CabinBonus :=
                 PlayerShip.Modules(CabinIndex).Data(1) -
                 Natural
                   (Float(PlayerShip.Modules(CabinIndex).Data(1)) *
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
      end if;
   end WaitForRest;

   function GetSkillLevelName(SkillLevel: Positive) return String is
   begin
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

end Crew;
