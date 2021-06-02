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

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Messages; use Messages;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Utils; use Utils;
with Goals; use Goals;
with Crafts; use Crafts;
with Config; use Config;
with BasesTypes; use BasesTypes;
with Maps; use Maps;
with Mobs; use Mobs;

package body Bases is

   procedure GainRep(BaseIndex: Bases_Range; Points: Integer) is
      NewPoints: Integer;
   begin
      if SkyBases(BaseIndex).Reputation(1) = -100 or
        SkyBases(BaseIndex).Reputation(1) = 100 then
         return;
      end if;
      NewPoints :=
        SkyBases(BaseIndex).Reputation(2) +
        Integer(Float(Points) * Float(New_Game_Settings.Reputation_Bonus));
      if BaseIndex = PlayerShip.Home_Base then
         NewPoints := NewPoints + Points;
      end if;
      Reduce_Reputation_Loop :
      while NewPoints < 0 loop
         SkyBases(BaseIndex).Reputation(1) :=
           SkyBases(BaseIndex).Reputation(1) - 1;
         NewPoints := NewPoints + abs (SkyBases(BaseIndex).Reputation(1) * 5);
         if NewPoints >= 0 then
            SkyBases(BaseIndex).Reputation(2) := NewPoints;
            return;
         end if;
      end loop Reduce_Reputation_Loop;
      Raise_Reputation_Loop :
      while NewPoints > abs (SkyBases(BaseIndex).Reputation(1) * 5) loop
         NewPoints := NewPoints - abs (SkyBases(BaseIndex).Reputation(1) * 5);
         SkyBases(BaseIndex).Reputation(1) :=
           SkyBases(BaseIndex).Reputation(1) + 1;
      end loop Raise_Reputation_Loop;
      SkyBases(BaseIndex).Reputation(2) := NewPoints;
      if SkyBases(BaseIndex).Reputation(1) = 100 then
         UpdateGoal(REPUTATION, SkyBases(BaseIndex).Owner);
      end if;
   end GainRep;

   procedure CountPrice
     (Price: in out Natural; TraderIndex: Crew_Container.Extended_Index;
      Reduce: Boolean := True) is
      Bonus: Integer := 0;
   begin
      if Price = 0 then
         return;
      end if;
      if TraderIndex /= Crew_Container.No_Index then
         Bonus :=
           Integer
             (Float'Floor
                (Float(Price) *
                 (Float
                    (GetSkillLevel
                       (PlayerShip.Crew(TraderIndex), Talking_Skill)) /
                  200.0)));
      end if;
      if SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex > 0 then
         case SkyBases(SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex)
           .Reputation
           (1) is
            when -24 .. -1 =>
               Bonus := Bonus - Integer(Float'Floor(Float(Price) * 0.05));
            when 26 .. 50 =>
               Bonus := Bonus + Integer(Float'Floor(Float(Price) * 0.05));
            when 51 .. 75 =>
               Bonus := Bonus + Integer(Float'Floor(Float(Price) * 0.1));
            when 76 .. 100 =>
               Bonus := Bonus + Integer(Float'Floor(Float(Price) * 0.15));
            when others =>
               null;
         end case;
      end if;
      if Bonus < 0 then
         Bonus := 0;
      end if;
      if Reduce then
         if Bonus >= Price then
            Bonus := Price - 1;
         end if;
         Price := Price - Bonus;
      else
         Price := Price + Bonus;
      end if;
   end CountPrice;

   function GenerateBaseName
     (FactionIndex: Unbounded_String) return Unbounded_String is
      NewName: Unbounded_String := Null_Unbounded_String;
   begin
      if Factions_List(FactionIndex).NamesType = ROBOTIC then
         return GenerateRoboticName;
      end if;
      if GetRandom(1, 100) < 16 then
         NewName :=
           BaseSyllablesPre
             (GetRandom
                (BaseSyllablesPre.First_Index, BaseSyllablesPre.Last_Index)) &
           " ";
      end if;
      NewName :=
        NewName &
        BaseSyllablesStart
          (GetRandom
             (BaseSyllablesStart.First_Index, BaseSyllablesStart.Last_Index)) &
        BaseSyllablesEnd
          (GetRandom
             (BaseSyllablesEnd.First_Index, BaseSyllablesEnd.Last_Index));
      if GetRandom(1, 100) < 16 then
         NewName :=
           NewName & " " &
           BaseSyllablesPost
             (GetRandom
                (BaseSyllablesPost.First_Index, BaseSyllablesPost.Last_Index));
      end if;
      return NewName;
   end GenerateBaseName;

   procedure GenerateRecruits is
      BaseIndex: constant Bases_Range :=
        SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex;
      RecruitBase: Bases_Range;
      BaseRecruits: Recruit_Container.Vector;
      Skills: Skills_Container.Vector;
      Gender: Character;
      Price, Payment: Natural;
      SkillIndex: Integer range -1 .. Integer'Last;
      Attributes: Attributes_Container.Vector;
      Inventory, TempTools: UnboundedString_Container.Vector;
      Equipment: Equipment_Array;
      MaxSkillLevel: Integer range -100 .. 100;
      SkillLevel, HighestLevel: Skill_Range;
      RecruitFaction: Unbounded_String;
      MaxRecruits, RecruitsAmount: Positive range 1 .. 30;
      SkillsAmount, SkillNumber, HighestSkill: Skills_Container.Extended_Index;
      MaxSkillAmount: Integer;
      procedure AddInventory
        (ItemsIndexes: UnboundedString_Container.Vector;
         EquipIndex: Positive) is
         ItemIndex: Unbounded_String;
      begin
         if GetRandom(1, 100) > 80 then
            return;
         end if;
         ItemIndex :=
           GetRandomItem
             (ItemsIndexes, EquipIndex, HighestLevel, Skills(1)(2),
              RecruitFaction);
         if ItemIndex = Null_Unbounded_String then
            return;
         end if;
         Inventory.Append(New_Item => ItemIndex);
         Equipment(EquipIndex) := Inventory.Last_Index;
         Price := Price + Get_Price(SkyBases(BaseIndex).BaseType, ItemIndex);
         Payment :=
           Payment + (Get_Price(SkyBases(BaseIndex).BaseType, ItemIndex) / 10);
      end AddInventory;
   begin
      if DaysDifference(SkyBases(BaseIndex).RecruitDate) < 30 or
        SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      MaxRecruits :=
        (if SkyBases(BaseIndex).Population < 150 then 5
         elsif SkyBases(BaseIndex).Population < 300 then 10 else 15);
      if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
          (To_Unbounded_String("barracks")) then
         MaxRecruits := MaxRecruits * 2;
      end if;
      if MaxRecruits > (SkyBases(BaseIndex).Population / 10) then
         MaxRecruits := (SkyBases(BaseIndex).Population / 10) + 1;
      end if;
      RecruitsAmount := GetRandom(1, MaxRecruits);
      MaxSkillAmount :=
        Integer
          (Float(Skills_List.Length) *
           (Float(SkyBases(BaseIndex).Reputation(1)) / 100.0));
      if MaxSkillAmount < 5 then
         MaxSkillAmount := 5;
      end if;
      Generate_Recruits_Loop :
      for I in 1 .. RecruitsAmount loop
         Skills.Clear;
         Attributes.Clear;
         Price := 0;
         Inventory.Clear;
         TempTools.Clear;
         Equipment := (others => 0);
         Payment := 0;
         RecruitFaction :=
           (if GetRandom(1, 100) < 99 then SkyBases(BaseIndex).Owner
            else GetRandomFaction);
         if not Factions_List(RecruitFaction).Flags.Contains
             (To_Unbounded_String("nogender")) then
            Gender := (if GetRandom(1, 2) = 1 then 'M' else 'F');
         else
            Gender := 'M';
         end if;
         SkillsAmount :=
           GetRandom(Skills_List.First_Index, Skills_List.Last_Index);
         if SkillsAmount > MaxSkillAmount then
            SkillsAmount := MaxSkillAmount;
         end if;
         HighestLevel := 1;
         HighestSkill := 1;
         MaxSkillLevel := SkyBases(BaseIndex).Reputation(1);
         if MaxSkillLevel < 20 then
            MaxSkillLevel := 20;
         end if;
         if GetRandom(1, 100) > 95 then
            MaxSkillLevel := GetRandom(MaxSkillLevel, 100);
         end if;
         Generate_Skills_Loop :
         for J in 1 .. SkillsAmount loop
            SkillNumber :=
              (if J > 1 then
                 GetRandom(Skills_List.First_Index, Skills_List.Last_Index)
               else Factions_List(RecruitFaction).WeaponSkill);
            SkillLevel := GetRandom(1, MaxSkillLevel);
            if SkillLevel > HighestLevel then
               HighestLevel := SkillLevel;
               HighestSkill := SkillNumber;
            end if;
            SkillIndex := 0;
            Get_Skill_Index_Loop :
            for C in Skills.Iterate loop
               if Skills(C)(1) = SkillNumber then
                  SkillIndex :=
                    (if Skills(C)(2) < SkillLevel then
                       Skills_Container.To_Index(C)
                     else -1);
                  exit Get_Skill_Index_Loop;
               end if;
            end loop Get_Skill_Index_Loop;
            if SkillIndex = 0 then
               Skills.Append(New_Item => (SkillNumber, SkillLevel, 0));
            elsif SkillIndex > 0 then
               Skills.Replace_Element
                 (Index => SkillIndex,
                  New_Item => (SkillNumber, SkillLevel, 0));
            end if;
         end loop Generate_Skills_Loop;
         Generate_Attributes_Loop :
         for J in Attributes_List.Iterate loop
            Attributes.Append
              (New_Item => (GetRandom(3, (MaxSkillLevel / 3)), 0));
         end loop Generate_Attributes_Loop;
         Update_Price_With_Skills_Loop :
         for Skill of Skills loop
            Price := Price + Skill(2);
            Payment := Payment + Skill(2);
         end loop Update_Price_With_Skills_Loop;
         Update_Price_With_Stats_Loop :
         for Stat of Attributes loop
            Price := Price + (Stat(1) * 2);
            Payment := Payment + (Stat(1) * 2);
         end loop Update_Price_With_Stats_Loop;
         AddInventory(Weapons_List, 1);
         AddInventory(Shields_List, 2);
         AddInventory(HeadArmors_List, 3);
         AddInventory(ChestArmors_List, 4);
         AddInventory(ArmsArmors_List, 5);
         AddInventory(LegsArmors_List, 6);
         Add_Tool_Loop :
         for Recipe of Recipes_List loop
            if HighestSkill = Recipe.Skill then
               Find_Tool_Loop :
               for J in Items_List.Iterate loop
                  if Items_List(J).IType = Recipe.Tool then
                     TempTools.Append(New_Item => Objects_Container.Key(J));
                  end if;
               end loop Find_Tool_Loop;
               AddInventory(TempTools, 7);
               exit Add_Tool_Loop;
            end if;
         end loop Add_Tool_Loop;
         if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
             (To_Unbounded_String("barracks")) then
            Price := Price / 2;
            Payment := Payment / 2;
         end if;
         Price :=
           Natural(Float(Price * 100) * Float(New_Game_Settings.Prices_Bonus));
         if Price = 0 then
            Price := 1;
         end if;
         RecruitBase :=
           (if GetRandom(1, 100) < 99 then BaseIndex
            else GetRandom(SkyBases'First, SkyBases'Last));
         BaseRecruits.Append
           (New_Item =>
              (Name => GenerateMemberName(Gender, RecruitFaction),
               Gender => Gender, Price => Price, Skills => Skills,
               Attributes => Attributes, Inventory => Inventory,
               Equipment => Equipment, Payment => Payment,
               HomeBase => RecruitBase, Faction => RecruitFaction));
      end loop Generate_Recruits_Loop;
      SkyBases(BaseIndex).RecruitDate := Game_Date;
      SkyBases(BaseIndex).Recruits := BaseRecruits;
   end GenerateRecruits;

   procedure AskForBases is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex;
      TmpBaseIndex: Extended_Base_Range;
      ShipIndex: Unbounded_String;
      UnknownBases: Extended_Base_Range := 0;
      TraderIndex: constant Natural := FindMember(Talk);
      Amount: Natural range 0 .. 40;
      Radius: Integer range -40 .. 40;
      TempX, TempY: Integer range -40 .. Bases_Range'Last + 40;
   begin
      if TraderIndex = 0 then
         return;
      end if;
      if BaseIndex > 0 then -- asking in base
         if SkyBases(BaseIndex).Population < 150 then
            Amount := 10;
            Radius := 10;
         elsif SkyBases(BaseIndex).Population < 300 then
            Amount := 20;
            Radius := 20;
         else
            Amount := 40;
            Radius := 40;
         end if;
         GainRep(BaseIndex, 1);
         SkyBases(BaseIndex).AskedForBases := True;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) &
            " asked for directions to other bases in base '" &
            To_String(SkyBases(BaseIndex).Name) & "'.",
            OrderMessage);
      else -- asking friendly ship
         Radius := 40;
         ShipIndex :=
           (Events_List(SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).EventIndex)
              .ShipIndex);
         Amount :=
           (if Proto_Ships_List(ShipIndex).Crew.Length < 5 then 3
            elsif Proto_Ships_List(ShipIndex).Crew.Length < 10 then 5 else 10);
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) & " asked ship '" &
            To_String(GenerateShipName(Proto_Ships_List(ShipIndex).Owner)) &
            "' for directions to other bases.",
            OrderMessage);
         DeleteEvent(SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).EventIndex);
         UpdateOrders(PlayerShip);
      end if;
      Bases_Loop :
      for X in -Radius .. Radius loop
         for Y in -Radius .. Radius loop
            TempX := PlayerShip.Sky_X + X;
            NormalizeCoord(TempX);
            TempY := PlayerShip.Sky_Y + Y;
            NormalizeCoord(TempY, False);
            TmpBaseIndex := SkyMap(TempX, TempY).BaseIndex;
            if TmpBaseIndex > 0 and then not SkyBases(TmpBaseIndex).Known then
               SkyBases(TmpBaseIndex).Known := True;
               Amount := Amount - 1;
               exit Bases_Loop when Amount = 0;
            end if;
         end loop;
      end loop Bases_Loop;
      if Amount > 0 then
         if BaseIndex > 0 then -- asking in base
            if SkyBases(BaseIndex).Population < 150 and then Amount > 1 then
               Amount := 1;
            elsif SkyBases(BaseIndex).Population < 300 and then Amount > 2 then
               Amount := 2;
            elsif Amount > 4 then
               Amount := 4;
            end if;
         else -- asking friendly ship
            Amount :=
              (if Proto_Ships_List(ShipIndex).Crew.Length < 5 then 1
               elsif Proto_Ships_List(ShipIndex).Crew.Length < 10 then 2
               else 4);
         end if;
         Count_Unknown_Bases :
         for I in SkyBases'Range loop
            if not SkyBases(I).Known then
               UnknownBases := UnknownBases + 1;
            end if;
            exit Count_Unknown_Bases when UnknownBases >= Amount;
         end loop Count_Unknown_Bases;
         if UnknownBases >= Amount then
            Reveal_Random_Bases_Loop :
            loop
               TmpBaseIndex := GetRandom(1, 1_024);
               if not SkyBases(TmpBaseIndex).Known then
                  SkyBases(TmpBaseIndex).Known := True;
                  Amount := Amount - 1;
               end if;
               exit Reveal_Random_Bases_Loop when Amount = 0;
            end loop Reveal_Random_Bases_Loop;
         else
            Reveal_Bases_Loop :
            for I in SkyBases'Range loop
               if not SkyBases(I).Known then
                  SkyBases(I).Known := True;
               end if;
            end loop Reveal_Bases_Loop;
         end if;
      end if;
      GainExp(1, Talking_Skill, TraderIndex);
      Update_Game(30);
   end AskForBases;

   procedure AskForEvents is
      BaseIndex: constant Extended_Base_Range :=
        SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex;
      EventTime, DiffX, DiffY: Positive;
      Event: Events_Types;
      MinX, MinY, MaxX, MaxY: Integer range -100 .. 1_124;
      Enemies: UnboundedString_Container.Vector;
      Attempts: Natural range 0 .. 10;
      NewItemIndex, ShipIndex: Unbounded_String;
      TraderIndex: constant Crew_Container.Extended_Index := FindMember(Talk);
      MaxEvents, EventsAmount: Positive range 1 .. 15;
      TmpBaseIndex: Bases_Range;
      EventX, EventY: Positive range 1 .. 1_024;
      ItemIndex: Integer;
   begin
      if TraderIndex = 0 then
         return;
      end if;
      if BaseIndex > 0 then -- asking in base
         MaxEvents :=
           (if SkyBases(BaseIndex).Population < 150 then 5
            elsif SkyBases(BaseIndex).Population < 300 then 10 else 15);
         SkyBases(BaseIndex).AskedForEvents := Game_Date;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) &
            " asked for recent events known at base '" &
            To_String(SkyBases(BaseIndex).Name) & "'.",
            OrderMessage);
         GainRep(BaseIndex, 1);
      else -- asking friendly ship
         ShipIndex :=
           Events_List(SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).EventIndex)
             .ShipIndex;
         MaxEvents :=
           (if Proto_Ships_List(ShipIndex).Crew.Length < 5 then 1
            elsif Proto_Ships_List(ShipIndex).Crew.Length < 10 then 3 else 5);
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) & " asked ship '" &
            To_String(GenerateShipName(Proto_Ships_List(ShipIndex).Owner)) &
            "' for recent events.",
            OrderMessage);
         DeleteEvent(SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).EventIndex);
         UpdateOrders(PlayerShip);
      end if;
      EventsAmount := GetRandom(1, MaxEvents);
      MinX := PlayerShip.Sky_X - 100;
      NormalizeCoord(MinX);
      MaxX := PlayerShip.Sky_X + 100;
      NormalizeCoord(MaxX);
      MinY := PlayerShip.Sky_Y - 100;
      NormalizeCoord(MinY, False);
      MaxY := PlayerShip.Sky_Y + 100;
      NormalizeCoord(MaxY, False);
      GenerateEnemies(Enemies);
      Generate_Events_Loop :
      for I in 1 .. EventsAmount loop
         Event := Events_Types'Val(GetRandom(1, 5));
         Attempts := 10;
         Generate_Event_Location_Loop :
         loop
            if Event = EnemyShip then
               EventX := GetRandom(MinX, MaxX);
               EventY := GetRandom(MinY, MaxY);
               exit Generate_Event_Location_Loop when SkyMap(EventX, EventY)
                   .BaseIndex =
                 0 and
                 EventX /= PlayerShip.Sky_X and EventY /= PlayerShip.Sky_Y and
                 SkyMap(EventX, EventY).EventIndex = 0;
            else
               TmpBaseIndex := GetRandom(1, 1_024);
               EventX := SkyBases(TmpBaseIndex).SkyX;
               EventY := SkyBases(TmpBaseIndex).SkyY;
               Attempts := Attempts - 1;
               if Attempts = 0 then
                  Event := EnemyShip;
                  Regenerate_Event_Location_Loop :
                  loop
                     EventX := GetRandom(MinX, MaxX);
                     EventY := GetRandom(MinY, MaxY);
                     exit Regenerate_Event_Location_Loop when SkyMap
                         (EventX, EventY)
                         .BaseIndex =
                       0 and
                       EventX /= PlayerShip.Sky_X and
                       EventY /= PlayerShip.Sky_Y and
                       SkyMap(EventX, EventY).EventIndex = 0;
                  end loop Regenerate_Event_Location_Loop;
                  exit Generate_Event_Location_Loop;
               end if;
               if EventX /= PlayerShip.Sky_X and EventY /= PlayerShip.Sky_Y and
                 SkyMap(EventX, EventY).EventIndex = 0 and
                 SkyBases(SkyMap(EventX, EventY).BaseIndex).Known then
                  if Event = AttackOnBase and
                    SkyBases(SkyMap(EventX, EventY).BaseIndex).Population /=
                      0 then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = DoublePrice and
                    IsFriendly
                      (PlayerShip.Crew(1).Faction,
                       SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner) then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = Disease and
                    not Factions_List
                      (SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner)
                      .Flags
                      .Contains
                      (To_Unbounded_String("diseaseimmune")) and
                    IsFriendly
                      (PlayerShip.Crew(1).Faction,
                       SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner) then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = BaseRecovery and
                    SkyBases(SkyMap(EventX, EventY).BaseIndex).Population =
                      0 then
                     exit Generate_Event_Location_Loop;
                  end if;
               end if;
            end if;
         end loop Generate_Event_Location_Loop;
         DiffX := abs (PlayerShip.Sky_X - EventX);
         DiffY := abs (PlayerShip.Sky_Y - EventY);
         EventTime := Positive(60.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
         case Event is
            when EnemyShip =>
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, EventX, EventY,
                     GetRandom(EventTime, EventTime + 60),
                     Enemies
                       (GetRandom(Enemies.First_Index, Enemies.Last_Index))));
            when AttackOnBase =>
               GenerateEnemies(Enemies, To_Unbounded_String("Any"), False);
               Events_List.Append
                 (New_Item =>
                    (AttackOnBase, EventX, EventY,
                     GetRandom(EventTime, EventTime + 120),
                     Enemies
                       (GetRandom(Enemies.First_Index, Enemies.Last_Index))));
               GenerateEnemies(Enemies);
            when Disease =>
               Events_List.Append
                 (New_Item =>
                    (Disease, EventX, EventY, GetRandom(10_080, 12_000), 1));
            when DoublePrice =>
               loop
                  ItemIndex := GetRandom(1, Positive(Items_List.Length));
                  for J in Items_List.Iterate loop
                     ItemIndex := ItemIndex - 1;
                     if ItemIndex <= 0
                       and then
                         Get_Price
                           (SkyBases(SkyMap(EventX, EventY).BaseIndex)
                              .BaseType,
                            Objects_Container.Key(J)) >
                         0 then
                        NewItemIndex := Objects_Container.Key(J);
                        exit;
                     end if;
                  end loop;
                  exit when NewItemIndex /= Null_Unbounded_String;
               end loop;
               Events_List.Append
                 (New_Item =>
                    (DoublePrice, EventX, EventY,
                     GetRandom((EventTime * 3), (EventTime * 4)),
                     NewItemIndex));
            when BaseRecovery =>
               RecoverBase(SkyMap(EventX, EventY).BaseIndex);
            when others =>
               null;
         end case;
         if Event /= BaseRecovery then
            SkyMap(EventX, EventY).EventIndex := Events_List.Last_Index;
         end if;
      end loop Generate_Events_Loop;
      GainExp(1, Talking_Skill, TraderIndex);
      Update_Game(30);
   end AskForEvents;

   procedure UpdatePopulation is
      BaseIndex: constant Bases_Range :=
        SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex;
      PopulationDiff: Integer;
   begin
      if DaysDifference(SkyBases(BaseIndex).RecruitDate) < 30 then
         return;
      end if;
      if SkyBases(BaseIndex).Population > 0 then
         if GetRandom(1, 100) > 30 then
            return;
         end if;
         PopulationDiff :=
           (if GetRandom(1, 100) < 20 then -(GetRandom(1, 10))
            else GetRandom(1, 10));
         if SkyBases(BaseIndex).Population + PopulationDiff < 0 then
            PopulationDiff := -(SkyBases(BaseIndex).Population);
         end if;
         SkyBases(BaseIndex).Population :=
           SkyBases(BaseIndex).Population + PopulationDiff;
         if SkyBases(BaseIndex).Population = 0 then
            SkyBases(BaseIndex).Reputation := (0, 0);
         end if;
      else
         if GetRandom(1, 100) > 5 then
            return;
         end if;
         SkyBases(BaseIndex).Population := GetRandom(5, 10);
         SkyBases(BaseIndex).Owner := GetRandomFaction;
      end if;
   end UpdatePopulation;

   procedure UpdatePrices is
      BaseIndex: constant Bases_Range :=
        SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex;
      Roll: Positive range 1 .. 100;
      Chance: Positive;
   begin
      if SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      Chance :=
        (if SkyBases(BaseIndex).Population < 150 then 1
         elsif SkyBases(BaseIndex).Population < 300 then 2 else 5);
      Chance := Chance + (DaysDifference(SkyBases(BaseIndex).Visited) / 10);
      if GetRandom(1, 100) > Chance then
         return;
      end if;
      for Item of SkyBases(BaseIndex).Cargo loop
         Roll := GetRandom(1, 100);
         if Roll < 30 and Item.Price > 1 then
            Item.Price := Item.Price - 1;
         elsif Roll < 60 and Item.Price > 0 then
            Item.Price := Item.Price + 1;
         end if;
      end loop;
   end UpdatePrices;

end Bases;
