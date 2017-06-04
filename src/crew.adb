--    Copyright 2016-2017 Bartek thindil Jasicki
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
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Game; use Game;
with Items; use Items;
with Utils; use Utils;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Maps; use Maps;

package body Crew is

   procedure GiveOrders
     (MemberIndex: Positive;
      GivenOrder: Crew_Orders;
      ModuleIndex: Natural := 0;
      CheckPriorities: Boolean := True) is
      MemberName: constant String :=
        To_String(PlayerShip.Crew(MemberIndex).Name);
      ModuleIndex2, ToolsIndex: Natural := 0;
      MType: ModuleType := ENGINE;
      RequiredTool: Unbounded_String;
   begin
      if GivenOrder = PlayerShip.Crew(MemberIndex).Order then
         if GivenOrder = Craft then
            for I in PlayerShip.Modules.Iterate loop
               if PlayerShip.Modules(I).Owner = MemberIndex and
                 Modules_Container.To_Index(I) = ModuleIndex then
                  return;
               end if;
            end loop;
         else
            return;
         end if;
      end if;
      if GivenOrder = Upgrading or
        GivenOrder = Repair or
        GivenOrder = Clean then -- Check for tools
         if GivenOrder = Clean then
            RequiredTool := CleaningTools;
         else
            RequiredTool := RepairTools;
         end if;
         ToolsIndex := FindCargo(ItemType => RequiredTool);
         if ToolsIndex = 0 then
            case GivenOrder is
               when Repair =>
                  ShowDialog
                    (MemberName &
                     " can't starts repairing ship because you don't have repair tools.");
               when Clean =>
                  ShowDialog
                    (MemberName &
                     " can't starts cleaning ship because you don't have any cleaning tools.");
               when Upgrading =>
                  ShowDialog
                    (MemberName &
                     " can't starts upgrading module because you don't have repair tools.");
               when others =>
                  null;
            end case;
            return;
         end if;
      end if;
      if GivenOrder = Pilot or
        GivenOrder = Engineer or
        GivenOrder = Upgrading or
        GivenOrder = Talk then
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew(I).Order = GivenOrder then
               GiveOrders(I, Rest, 0, False);
               exit;
            end if;
         end loop;
      elsif GivenOrder = Gunner or GivenOrder = Craft or GivenOrder = Heal then
         if PlayerShip.Modules(ModuleIndex).Owner > 0 then
            GiveOrders(PlayerShip.Modules(ModuleIndex).Owner, Rest, 0, False);
         end if;
      end if;
      if ModuleIndex = 0 and
        (GivenOrder = Pilot or GivenOrder = Engineer or GivenOrder = Rest) then
         case GivenOrder is
            when Pilot =>
               MType := COCKPIT;
            when Engineer =>
               MType := ENGINE;
            when Rest =>
               MType := CABIN;
            when others =>
               null;
         end case;
         for I in PlayerShip.Modules.Iterate loop
            if MType /= CABIN then
               if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType =
                 MType and
                 PlayerShip.Modules(I).Durability > 0 then
                  if PlayerShip.Modules(I).Owner /= 0 then
                     GiveOrders(PlayerShip.Modules(I).Owner, Rest, 0, False);
                  end if;
                  ModuleIndex2 := Modules_Container.To_Index(I);
                  exit;
               end if;
            else
               if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType =
                 CABIN and
                 PlayerShip.Modules(I).Durability > 0 and
                 PlayerShip.Modules(I).Owner = MemberIndex then
                  ModuleIndex2 := Modules_Container.To_Index(I);
                  exit;
               end if;
            end if;
         end loop;
      else
         ModuleIndex2 := ModuleIndex;
      end if;
      if ModuleIndex2 = 0 then
         case GivenOrder is
            when Pilot =>
               ShowDialog
                 (MemberName &
                  " can't starts piloting because cockpit is destroyed or you don't have cockpit.");
               return;
            when Engineer =>
               ShowDialog
                 (MemberName &
                  " can't starts engineers duty because all engines are destroyed or you don't have engine.");
               return;
            when Gunner =>
               ShowDialog
                 (MemberName &
                  " can't starts operating gun because all guns are destroyed or you don't have installed any.");
               return;
            when Rest =>
               for Module of PlayerShip.Modules loop
                  if Modules_List(Module.ProtoIndex).MType = CABIN and
                    Module.Durability > 0 and
                    Module.Owner = 0 then
                     Module.Owner := MemberIndex;
                     AddMessage
                       (MemberName &
                        " take " &
                        To_String(Module.Name) &
                        " as own cabin.",
                        OtherMessage);
                     exit;
                  end if;
               end loop;
            when others =>
               null;
         end case;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType /= CABIN and
           Module.Owner = MemberIndex then
            Module.Owner := 0;
            exit;
         end if;
      end loop;
      case GivenOrder is
         when Pilot =>
            AddMessage(MemberName & " starts piloting.", OrderMessage);
            PlayerShip.Modules(ModuleIndex2).Owner := MemberIndex;
         when Engineer =>
            AddMessage(MemberName & " starts engineers duty.", OrderMessage);
         when Gunner =>
            AddMessage(MemberName & " starts operating gun.", OrderMessage);
            PlayerShip.Modules(ModuleIndex2).Owner := MemberIndex;
         when Rest =>
            AddMessage(MemberName & " going on break.", OrderMessage);
         when Repair =>
            AddMessage(MemberName & " starts repair ship.", OrderMessage);
         when Craft =>
            AddMessage(MemberName & " starts manufacturing.", OrderMessage);
            PlayerShip.Modules(ModuleIndex2).Owner := MemberIndex;
         when Upgrading =>
            AddMessage
              (MemberName &
               " starts upgrading " &
               To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
               ".",
               OrderMessage);
         when Talk =>
            AddMessage
              (MemberName & " was assigned to talking in bases.",
               OrderMessage);
         when Heal =>
            AddMessage
              (MemberName & " starts healing wounded crew members.",
               OrderMessage);
            PlayerShip.Modules(ModuleIndex2).Owner := MemberIndex;
         when Clean =>
            AddMessage(MemberName & " starts cleaning ship.", OrderMessage);
      end case;
      PlayerShip.Crew(MemberIndex).Order := GivenOrder;
      if GivenOrder = Rest then
         PlayerShip.Crew(MemberIndex).PreviousOrder := Rest;
      end if;
      PlayerShip.Crew(MemberIndex).OrderTime := 15;
      if CheckPriorities then
         UpdateOrders;
      end if;
   end GiveOrders;

   procedure GainExp(Amount: Natural; SkillNumber, CrewIndex: Positive) is
      SkillExp, SkillLevel, SkillIndex: Natural := 0;
   begin
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
         ItemIndex := FindCargo(ItemType => ItemType);
         if ItemIndex > 0 then
            ConsumeValue :=
              Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value;
            UpdateCargo
              (PlayerShip,
               PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
               -1);
            return ConsumeValue;
         end if;
         if ItemType = FoodTypes(1) then
            ItemIndex := FindCargo(ItemType => FoodTypes(2));
            if ItemIndex > 0 then
               ConsumeValue :=
                 Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Value;
               UpdateCargo
                 (PlayerShip,
                  PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
                  -1);
               return ConsumeValue;
            end if;
         end if;
         return 0;
      end Consume;
      procedure UpdateMember(Member: in out Member_Data) is
         BackToWork: Boolean := True;
         ConsumeResult: Natural;
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
                  OrderMessage);
            end if;
            Member.PreviousOrder := Rest;
         end if;
         if TiredLevel > 80 and Member.Order /= Rest then
            Member.PreviousOrder := Member.Order;
            Member.Order := Rest;
            Member.OrderTime := 15;
            AddMessage
              (To_String(Member.Name) & " is too tired to work, going rest.",
               OrderMessage);
         end if;
         if HungerLevel > 80 then
            ConsumeResult := Consume(FoodTypes(1));
            HungerLevel := HungerLevel - ConsumeResult;
            if HungerLevel < 0 then
               HungerLevel := 0;
            end if;
            if ConsumeResult = 0 then
               AddMessage
                 (To_String(Member.Name) &
                  " is hungry, but can't find anything to eat.",
                  OtherMessage);
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
                  OtherMessage);
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
                       PlayerShip.Modules(CabinIndex).Current_Value -
                       Natural
                         (Float(PlayerShip.Modules(CabinIndex).Current_Value) *
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
               end if;
            else
               if PlayerShip.Crew(I).Order /= Talk then
                  TiredLevel := TiredLevel + Times;
               end if;
               if TiredLevel > 100 then
                  TiredLevel := 100;
               end if;
               case PlayerShip.Crew(I).Order is
                  when Pilot =>
                     GainExp(Times, 1, I);
                  when Engineer =>
                     GainExp(Times, 2, I);
                  when Heal =>
                     HealAmount := Times * (GetSkillLevel(I, 10) / 20);
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
                        HealAmount := 0;
                        AddMessage
                          ("You don't have medical room to continue healing wounded crew members.",
                           OrderMessage);
                     end if;
                     if HealAmount > 0 then
                        HealAmount := HealAmount * (-1);
                        for I in
                          PlayerShip.Cargo.First_Index ..
                              PlayerShip.Cargo.Last_Index loop
                           if Items_List
                               (PlayerShip.Cargo.Element(I).ProtoIndex)
                               .IType =
                             HealingTools and
                             PlayerShip.Cargo.Element(I).Amount >= Times then
                              HealAmount := abs (HealAmount);
                              UpdateCargo
                                (PlayerShip,
                                 PlayerShip.Cargo.Element(I).ProtoIndex,
                                 (0 - Times));
                              exit;
                           end if;
                        end loop;
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
                              GainExp(Times, 10, I);
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
                              OrderMessage);
                        end if;
                     else
                        AddMessage
                          ("You don't have any medical supplies to continue healing wounded crew members.",
                           OrderMessage);
                     end if;
                     if HealAmount /= 0 then
                        GiveOrders(I, Rest);
                     end if;
                  when Clean =>
                     ToolIndex := 0;
                     NeedCleaning := False;
                     for J in PlayerShip.Cargo.Iterate loop
                        if Items_List(PlayerShip.Cargo(J).ProtoIndex).IType =
                          CleaningTools then
                           ToolIndex := Cargo_Container.To_Index(J);
                           exit;
                        end if;
                     end loop;
                     if ToolIndex > 0 then
                        for Module of PlayerShip.Modules loop
                           if Modules_List(Module.ProtoIndex).MType = CABIN and
                             Module.Current_Value < Module.Max_Value then
                              if Module.Current_Value + Times >
                                Module.Max_Value then
                                 Module.Current_Value := Module.Max_Value;
                              else
                                 Module.Current_Value :=
                                   Module.Current_Value + Times;
                              end if;
                              DamageCargo(ToolIndex);
                              exit;
                           end if;
                        end loop;
                        for Module of PlayerShip.Modules loop
                           if Modules_List(Module.ProtoIndex).MType = CABIN and
                             Module.Current_Value < Module.Max_Value then
                              NeedCleaning := True;
                              exit;
                           end if;
                        end loop;
                     end if;
                     if not NeedCleaning then
                        if ToolIndex = 0 then
                           AddMessage
                             ("You can't continue cleaning ship because you don't have any cleaning tools.",
                              OrderMessage);
                        end if;
                        for J in PlayerShip.Crew.Iterate loop
                           if PlayerShip.Crew(J).Order = Clean then
                              GiveOrders(Crew_Container.To_Index(J), Rest);
                           end if;
                        end loop;
                     end if;
                  when Talk =>
                     if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex =
                       0 then
                        GiveOrders(I, Rest);
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

   procedure UpdateOrders is
      HavePilot,
      HaveEngineer,
      HaveUpgrade,
      HaveTrader,
      NeedClean,
      NeedRepairs,
      NeedGunners,
      NeedCrafters,
      NeedHealer,
      CanHeal: Boolean :=
        False;
      function UpdatePosition
        (Order: Crew_Orders;
         MaxPriority: Boolean := True) return Boolean is
         ModuleIndex, MemberIndex: Natural := 0;
      begin
         if MaxPriority then
            for I in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(I).Orders(Crew_Orders'Pos(Order) + 1) = 2 and
                 PlayerShip.Crew(I).Order /= Order and
                 PlayerShip.Crew(I).PreviousOrder /= Order then
                  MemberIndex := Crew_Container.To_Index(I);
                  exit;
               end if;
            end loop;
         else
            for I in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(I).Orders(Crew_Orders'Pos(Order) + 1) = 1 and
                 PlayerShip.Crew(I).Order = Rest and
                 PlayerShip.Crew(I).PreviousOrder = Rest then
                  MemberIndex := Crew_Container.To_Index(I);
                  exit;
               end if;
            end loop;
         end if;
         if MemberIndex = 0 then
            return False;
         end if;
         if Order = Gunner or Order = Craft or Order = Heal then
            for I in PlayerShip.Modules.Iterate loop
               case Modules_List(PlayerShip.Modules(I).ProtoIndex).MType is
                  when GUN =>
                     if Order = Gunner and
                       PlayerShip.Modules(I).Owner = 0 and
                       PlayerShip.Modules(I).Durability > 0 then
                        ModuleIndex := Modules_Container.To_Index(I);
                        exit;
                     end if;
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     if Order = Craft and
                       PlayerShip.Modules(I).Owner = 0 and
                       PlayerShip.Modules(I).Durability > 0 and
                       PlayerShip.Modules(I).Current_Value /= 0 then
                        ModuleIndex := Modules_Container.To_Index(I);
                        exit;
                     end if;
                  when MEDICAL_ROOM =>
                     if Order = Heal and
                       PlayerShip.Modules(I).Owner = 0 and
                       PlayerShip.Modules(I).Durability > 0 then
                        ModuleIndex := Modules_Container.To_Index(I);
                        exit;
                     end if;
                  when others =>
                     null;
               end case;
            end loop;
            if ModuleIndex = 0 then
               return False;
            end if;
         elsif Order = Pilot or Order = Engineer then
            for I in PlayerShip.Modules.Iterate loop
               case Modules_List(PlayerShip.Modules(I).ProtoIndex).MType is
                  when COCKPIT =>
                     if Order = Pilot and
                       PlayerShip.Modules(I).Durability > 0 then
                        ModuleIndex := Modules_Container.To_Index(I);
                        exit;
                     end if;
                  when ENGINE =>
                     if Order = Engineer and
                       PlayerShip.Modules(I).Durability > 0 then
                        ModuleIndex := Modules_Container.To_Index(I);
                        exit;
                     end if;
                  when others =>
                     null;
               end case;
            end loop;
            if ModuleIndex = 0 then
               return False;
            end if;
         end if;
         GiveOrders(MemberIndex, Order, ModuleIndex);
         return True;
      end UpdatePosition;
   begin
      for Member of PlayerShip.Crew loop
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
            NeedHealer := True;
         end if;
      end loop;
      for Module of PlayerShip.Modules loop
         case Modules_List(Module.ProtoIndex).MType is
            when GUN =>
               if Module.Owner = 0 and
                 Module.Durability > 0 and
                 not NeedGunners then
                  NeedGunners := True;
               end if;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.Current_Value /= 0 and
                 Module.Owner = 0 and
                 Module.Durability > 0 and
                 not NeedCrafters then
                  NeedCrafters := True;
               end if;
            when CABIN =>
               if Module.Current_Value < Module.Max_Value and
                 Module.Durability > 0 then
                  NeedClean := True;
               end if;
            when MEDICAL_ROOM =>
               if NeedHealer and
                 Module.Durability > 0 and
                 FindCargo(ItemType => HealingTools) > 0 then
                  CanHeal := True;
               end if;
            when others =>
               null;
         end case;
         if Module.Durability < Module.MaxDurability and not NeedRepairs then
            for Item of PlayerShip.Cargo loop
               if Items_List(Item.ProtoIndex).IType =
                 Modules_List(Module.ProtoIndex).RepairMaterial then
                  NeedRepairs := True;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      if not HavePilot then
         if UpdatePosition(Pilot) then
            UpdateOrders;
         end if;
      end if;
      if not HaveEngineer then
         if UpdatePosition(Engineer) then
            UpdateOrders;
         end if;
      end if;
      if NeedGunners then
         if UpdatePosition(Gunner) then
            UpdateOrders;
         end if;
      end if;
      if NeedCrafters then
         if UpdatePosition(Craft) then
            UpdateOrders;
         end if;
      end if;
      if not HaveUpgrade and
        PlayerShip.UpgradeModule > 0 and
        FindCargo(ItemType => RepairTools) > 0 then
         if FindCargo
             (ItemType =>
                Modules_List
                  (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                  .RepairMaterial) >
           0 then
            if UpdatePosition(Upgrading) then
               UpdateOrders;
            end if;
         end if;
      end if;
      if not HaveTrader and
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         if UpdatePosition(Talk) then
            UpdateOrders;
         end if;
      end if;
      if NeedClean and FindCargo(ItemType => CleaningTools) > 0 then
         if UpdatePosition(Clean) then
            UpdateOrders;
         end if;
      end if;
      if CanHeal then
         if UpdatePosition(Heal) then
            UpdateOrders;
         end if;
      end if;
      if NeedRepairs and FindCargo(ItemType => RepairTools) > 0 then
         if UpdatePosition(Repair) then
            UpdateOrders;
         end if;
      end if;
      if not HavePilot then
         if UpdatePosition(Pilot, False) then
            UpdateOrders;
         end if;
      end if;
      if not HaveEngineer then
         if UpdatePosition(Engineer, False) then
            UpdateOrders;
         end if;
      end if;
      if NeedGunners then
         if UpdatePosition(Gunner, False) then
            UpdateOrders;
         end if;
      end if;
      if NeedCrafters then
         if UpdatePosition(Craft, False) then
            UpdateOrders;
         end if;
      end if;
      if not HaveUpgrade and
        PlayerShip.UpgradeModule > 0 and
        FindCargo(ItemType => RepairTools) > 0 then
         if FindCargo
             (ItemType =>
                Modules_List
                  (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                  .RepairMaterial) >
           0 then
            if UpdatePosition(Upgrading, False) then
               UpdateOrders;
            end if;
         end if;
      end if;
      if not HaveTrader and
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         if UpdatePosition(Talk, False) then
            UpdateOrders;
         end if;
      end if;
      if NeedClean and FindCargo(ItemType => CleaningTools) > 0 then
         if UpdatePosition(Clean, False) then
            UpdateOrders;
         end if;
      end if;
      if CanHeal then
         if UpdatePosition(Heal, False) then
            UpdateOrders;
         end if;
      end if;
      if NeedRepairs and FindCargo(ItemType => RepairTools) > 0 then
         if UpdatePosition(Repair, False) then
            UpdateOrders;
         end if;
      end if;
   end UpdateOrders;

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
                 PlayerShip.Modules(CabinIndex).Current_Value -
                 Natural
                   (Float(PlayerShip.Modules(CabinIndex).Current_Value) *
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

end Crew;
