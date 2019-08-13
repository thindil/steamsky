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

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Utils; use Utils;
with Goals; use Goals;
with Crafts; use Crafts;
with Config; use Config;

package body Bases is

   procedure GainRep(BaseIndex: BasesRange; Points: Integer) is
      NewPoints: Integer;
   begin
      if SkyBases(BaseIndex).Reputation(1) = -100 or
        SkyBases(BaseIndex).Reputation(1) = 100 then
         return;
      end if;
      NewPoints :=
        SkyBases(BaseIndex).Reputation(2) +
        Integer(Float(Points) * NewGameSettings.ReputationBonus);
      if BaseIndex = PlayerShip.HomeBase then
         NewPoints := NewPoints + Points;
      end if;
      while NewPoints < 0 loop
         SkyBases(BaseIndex).Reputation(1) :=
           SkyBases(BaseIndex).Reputation(1) - 1;
         NewPoints := NewPoints + abs (SkyBases(BaseIndex).Reputation(1) * 5);
         if NewPoints >= 0 then
            SkyBases(BaseIndex).Reputation(2) := NewPoints;
            return;
         end if;
      end loop;
      while NewPoints > abs (SkyBases(BaseIndex).Reputation(1) * 5) loop
         NewPoints := NewPoints - abs (SkyBases(BaseIndex).Reputation(1) * 5);
         SkyBases(BaseIndex).Reputation(1) :=
           SkyBases(BaseIndex).Reputation(1) + 1;
      end loop;
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
                       (PlayerShip.Crew(TraderIndex), TalkingSkill)) /
                  200.0)));
      end if;
      if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
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
      NewName: Unbounded_String;
   begin
      NewName := Null_Unbounded_String;
      if Factions_List(FactionIndex).NamesType = Factions.STANDARD then
         if GetRandom(1, 100) < 16 then
            NewName :=
              BaseSyllablesPre
                (GetRandom
                   (BaseSyllablesPre.First_Index,
                    BaseSyllablesPre.Last_Index)) &
              " ";
         end if;
         NewName :=
           NewName &
           BaseSyllablesStart
             (GetRandom
                (BaseSyllablesStart.First_Index,
                 BaseSyllablesStart.Last_Index)) &
           BaseSyllablesEnd
             (GetRandom
                (BaseSyllablesEnd.First_Index, BaseSyllablesEnd.Last_Index));
         if GetRandom(1, 100) < 16 then
            NewName :=
              NewName & " " &
              BaseSyllablesPost
                (GetRandom
                   (BaseSyllablesPost.First_Index,
                    BaseSyllablesPost.Last_Index));
         end if;
      else
         NewName := GenerateRoboticName;
      end if;
      return NewName;
   end GenerateBaseName;

   procedure GenerateRecruits is
      BaseIndex: constant BasesRange :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MaxRecruits, RecruitsAmount, SkillsAmount, SkillNumber, SkillLevel,
      HighestSkill, HighestLevel, RecruitBase: Positive;
      BaseRecruits: Recruit_Container.Vector;
      Skills: Skills_Container.Vector;
      Gender: Character;
      Price, Payment: Natural;
      SkillIndex: Integer;
      Attributes: Attributes_Container.Vector;
      Inventory, TempTools: UnboundedString_Container.Vector;
      Equipment: Natural_Array(1 .. 7);
      MaxSkillLevel: Integer;
      RecruitFaction: Unbounded_String;
      procedure AddInventory
        (ItemsIndexes: UnboundedString_Container.Vector;
         EquipIndex: Positive) is
         ItemIndex: Positive;
      begin
         if GetRandom(1, 100) > 80 then
            return;
         end if;
         ItemIndex :=
           GetRandom(ItemsIndexes.First_Index, ItemsIndexes.Last_Index);
         Inventory.Append(New_Item => ItemsIndexes(ItemIndex));
         Equipment(EquipIndex) := Inventory.Last_Index;
         Price := Price + (Items_List(ItemsIndexes(ItemIndex)).Prices(1) * 2);
         Payment :=
           Payment + (Items_List(ItemsIndexes(ItemIndex)).Prices(1) / 10);
      end AddInventory;
   begin
      if DaysDifference(SkyBases(BaseIndex).RecruitDate) < 30 or
        SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      if SkyBases(BaseIndex).Population < 150 then
         MaxRecruits := 5;
      elsif SkyBases(BaseIndex).Population < 300 then
         MaxRecruits := 10;
      else
         MaxRecruits := 15;
      end if;
      if MaxRecruits > (SkyBases(BaseIndex).Population / 10) then
         MaxRecruits := (SkyBases(BaseIndex).Population / 10) + 1;
      end if;
      RecruitsAmount := GetRandom(1, MaxRecruits);
      for I in 1 .. RecruitsAmount loop
         Skills.Clear;
         Attributes.Clear;
         Price := 0;
         Inventory.Clear;
         TempTools.Clear;
         Equipment := (others => 0);
         Payment := 0;
         if GetRandom(1, 100) < 99 then
            RecruitFaction := SkyBases(BaseIndex).Owner;
         else
            RecruitFaction := GetRandomFaction;
         end if;
         if not Factions_List(RecruitFaction).Flags.Contains
             (To_Unbounded_String("nogender")) then
            if GetRandom(1, 2) = 1 then
               Gender := 'M';
            else
               Gender := 'F';
            end if;
         else
            Gender := 'M';
         end if;
         SkillsAmount :=
           GetRandom(Skills_List.First_Index, Skills_List.Last_Index);
         HighestLevel := 1;
         HighestSkill := 1;
         MaxSkillLevel := SkyBases(BaseIndex).Reputation(1);
         if MaxSkillLevel < 20 then
            MaxSkillLevel := 20;
         end if;
         if GetRandom(1, 100) > 90 then
            MaxSkillLevel := GetRandom(MaxSkillLevel, 100);
         end if;
         for J in 1 .. SkillsAmount loop
            SkillNumber :=
              GetRandom(Skills_List.First_Index, Skills_List.Last_Index);
            SkillLevel := GetRandom(1, MaxSkillLevel);
            if SkillLevel > HighestLevel then
               HighestLevel := SkillLevel;
               HighestSkill := SkillNumber;
            end if;
            SkillIndex := 0;
            for C in Skills.Iterate loop
               if Skills(C)(1) = SkillNumber then
                  if Skills(C)(2) < SkillLevel then
                     SkillIndex := Skills_Container.To_Index(C);
                  else
                     SkillIndex := -1;
                  end if;
                  exit;
               end if;
            end loop;
            if SkillIndex = 0 then
               Skills.Append(New_Item => (SkillNumber, SkillLevel, 0));
            elsif SkillIndex > 0 then
               Skills.Replace_Element
                 (Index => SkillIndex,
                  New_Item => (SkillNumber, SkillLevel, 0));
            end if;
         end loop;
         for J in Attributes_List.Iterate loop
            Attributes.Append
              (New_Item => (GetRandom(3, (MaxSkillLevel / 2)), 0));
         end loop;
         for Skill of Skills loop
            Price := Price + Skill(2);
            Payment := Payment + Skill(2);
         end loop;
         for Stat of Attributes loop
            Price := Price + (Stat(1) * 2);
            Payment := Payment + (Stat(1) * 2);
         end loop;
         AddInventory(Weapons_List, 1);
         AddInventory(Shields_List, 2);
         AddInventory(HeadArmors_List, 3);
         AddInventory(ChestArmors_List, 4);
         AddInventory(ArmsArmors_List, 5);
         AddInventory(LegsArmors_List, 6);
         for Recipe of Recipes_List loop
            if HighestSkill = Recipe.Skill then
               for J in Items_List.Iterate loop
                  if Items_List(J).IType = Recipe.Tool then
                     TempTools.Append(New_Item => Objects_Container.Key(J));
                  end if;
               end loop;
               AddInventory(TempTools, 7);
               exit;
            end if;
         end loop;
         Price := Natural(Float(Price * 100) * NewGameSettings.PricesBonus);
         if Price = 0 then
            Price := 1;
         end if;
         if GetRandom(1, 100) < 99 then
            RecruitBase := BaseIndex;
         else
            RecruitBase := GetRandom(SkyBases'First, SkyBases'Last);
         end if;
         BaseRecruits.Append
           (New_Item =>
              (Name => GenerateMemberName(Gender, RecruitFaction),
               Gender => Gender, Price => Price, Skills => Skills,
               Attributes => Attributes, Inventory => Inventory,
               Equipment => Equipment, Payment => Payment,
               HomeBase => RecruitBase, Faction => RecruitFaction));
      end loop;
      SkyBases(BaseIndex).RecruitDate := GameDate;
      SkyBases(BaseIndex).Recruits := BaseRecruits;
   end GenerateRecruits;

   procedure AskForBases is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Radius, TempX, TempY: Integer;
      Amount, TmpBaseIndex, TraderIndex: Natural;
      ShipIndex: Unbounded_String;
      UnknownBases: Natural := 0;
   begin
      TraderIndex := FindMember(Talk);
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
           (Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
              .ShipIndex);
         if ProtoShips_List(ShipIndex).Crew.Length < 5 then
            Amount := 3;
         elsif ProtoShips_List(ShipIndex).Crew.Length < 10 then
            Amount := 5;
         else
            Amount := 10;
         end if;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) & " asked ship '" &
            To_String
              (GenerateShipName
                 (Factions_List(ProtoShips_List(ShipIndex).Owner).Name)) &
            "' for directions to other bases.",
            OrderMessage);
         DeleteEvent(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex);
         UpdateOrders(PlayerShip);
      end if;
      Bases_Loop :
      for X in -Radius .. Radius loop
         for Y in -Radius .. Radius loop
            TempX := PlayerShip.SkyX + X;
            NormalizeCoord(TempX);
            TempY := PlayerShip.SkyY + Y;
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
            if ProtoShips_List(ShipIndex).Crew.Length < 5 then
               Amount := 1;
            elsif ProtoShips_List(ShipIndex).Crew.Length < 10 then
               Amount := 2;
            else
               Amount := 4;
            end if;
         end if;
         for I in SkyBases'Range loop
            if not SkyBases(I).Known then
               UnknownBases := UnknownBases + 1;
            end if;
            exit when UnknownBases >= Amount;
         end loop;
         if UnknownBases >= Amount then
            loop
               TmpBaseIndex := GetRandom(1, 1024);
               if not SkyBases(TmpBaseIndex).Known then
                  SkyBases(TmpBaseIndex).Known := True;
                  Amount := Amount - 1;
               end if;
               exit when Amount = 0;
            end loop;
         else
            for I in SkyBases'Range loop
               if not SkyBases(I).Known then
                  SkyBases(I).Known := True;
               end if;
            end loop;
         end if;
      end if;
      GainExp(1, TalkingSkill, TraderIndex);
      UpdateGame(30);
   end AskForBases;

   procedure AskForEvents is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MaxEvents, EventsAmount, TmpBaseIndex, EventX, EventY, EventTime, DiffX,
      DiffY: Positive;
      Event: Events_Types;
      MinX, MinY, MaxX, MaxY, ItemIndex: Integer;
      Enemies: UnboundedString_Container.Vector;
      Attempts, TraderIndex: Natural;
      PlayerShips: UnboundedString_Container.Vector;
      NewItemIndex, ShipIndex: Unbounded_String;
   begin
      TraderIndex := FindMember(Talk);
      if TraderIndex = 0 then
         return;
      end if;
      if BaseIndex > 0 then -- asking in base
         if SkyBases(BaseIndex).Population < 150 then
            MaxEvents := 5;
         elsif SkyBases(BaseIndex).Population < 300 then
            MaxEvents := 10;
         else
            MaxEvents := 15;
         end if;
         SkyBases(BaseIndex).AskedForEvents := GameDate;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) &
            " asked for recent events known at base '" &
            To_String(SkyBases(BaseIndex).Name) & "'.",
            OrderMessage);
         GainRep(BaseIndex, 1);
      else -- asking friendly ship
         ShipIndex :=
           Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .ShipIndex;
         if ProtoShips_List(ShipIndex).Crew.Length < 5 then
            MaxEvents := 1;
         elsif ProtoShips_List(ShipIndex).Crew.Length < 10 then
            MaxEvents := 3;
         else
            MaxEvents := 5;
         end if;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) & " asked ship '" &
            To_String
              (GenerateShipName
                 (Factions_List(ProtoShips_List(ShipIndex).Owner).Name)) &
            "' for recent events.",
            OrderMessage);
         DeleteEvent(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex);
         UpdateOrders(PlayerShip);
      end if;
      EventsAmount := GetRandom(1, MaxEvents);
      MinX := PlayerShip.SkyX - 100;
      NormalizeCoord(MinX);
      MaxX := PlayerShip.SkyX + 100;
      NormalizeCoord(MaxX);
      MinY := PlayerShip.SkyY - 100;
      NormalizeCoord(MinY, False);
      MaxY := PlayerShip.SkyY + 100;
      NormalizeCoord(MaxY, False);
      for Faction of Factions_List loop
         for Career of Faction.Careers loop
            PlayerShips.Append(New_Item => Career.ShipIndex);
         end loop;
      end loop;
      GenerateEnemies(Enemies);
      for I in 1 .. EventsAmount loop
         Event := Events_Types'Val(GetRandom(1, 5));
         Attempts := 10;
         loop
            if Event = EnemyShip then
               EventX := GetRandom(MinX, MaxX);
               EventY := GetRandom(MinY, MaxY);
               exit when SkyMap(EventX, EventY).BaseIndex = 0 and
                 EventX /= PlayerShip.SkyX and EventY /= PlayerShip.SkyY and
                 SkyMap(EventX, EventY).EventIndex = 0;
            else
               TmpBaseIndex := GetRandom(1, 1024);
               EventX := SkyBases(TmpBaseIndex).SkyX;
               EventY := SkyBases(TmpBaseIndex).SkyY;
               Attempts := Attempts - 1;
               if Attempts = 0 then
                  Event := EnemyShip;
                  loop
                     EventX := GetRandom(MinX, MaxX);
                     EventY := GetRandom(MinY, MaxY);
                     exit when SkyMap(EventX, EventY).BaseIndex = 0 and
                       EventX /= PlayerShip.SkyX and
                       EventY /= PlayerShip.SkyY and
                       SkyMap(EventX, EventY).EventIndex = 0;
                  end loop;
                  exit;
               end if;
               if EventX /= PlayerShip.SkyX and EventY /= PlayerShip.SkyY and
                 SkyMap(EventX, EventY).EventIndex = 0 and
                 SkyBases(SkyMap(EventX, EventY).BaseIndex).Known then
                  if Event = AttackOnBase and
                    SkyBases(SkyMap(EventX, EventY).BaseIndex).Population /=
                      0 then
                     exit;
                  end if;
                  if Event = DoublePrice and
                    IsFriendly
                      (PlayerShip.Crew(1).Faction,
                       SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner) then
                     exit;
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
                     exit;
                  end if;
                  if Event = BaseRecovery and
                    SkyBases(SkyMap(EventX, EventY).BaseIndex).Population =
                      0 then
                     exit;
                  end if;
               end if;
            end if;
         end loop;
         DiffX := abs (PlayerShip.SkyX - EventX);
         DiffY := abs (PlayerShip.SkyY - EventY);
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
                    (Disease, EventX, EventY, GetRandom(10080, 12000), 1));
            when DoublePrice =>
               loop
                  ItemIndex := GetRandom(1, Positive(Items_List.Length));
                  for J in Items_List.Iterate loop
                     ItemIndex := ItemIndex - 1;
                     if ItemIndex <= 0
                       and then Items_List(J).Prices(1) > 0 then
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
      end loop;
      GainExp(1, TalkingSkill, TraderIndex);
      UpdateGame(30);
   end AskForEvents;

   procedure UpdatePopulation is
      BaseIndex: constant BasesRange :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      PopulationDiff: Integer;
   begin
      if DaysDifference(SkyBases(BaseIndex).RecruitDate) < 30 then
         return;
      end if;
      if SkyBases(BaseIndex).Population > 0 then
         if GetRandom(1, 100) > 30 then
            return;
         end if;
         if GetRandom(1, 100) < 20 then
            PopulationDiff := 0 - GetRandom(1, 10);
         else
            PopulationDiff := GetRandom(1, 10);
         end if;
         if SkyBases(BaseIndex).Population + PopulationDiff < 0 then
            PopulationDiff := 0 - SkyBases(BaseIndex).Population;
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
      BaseIndex: constant BasesRange :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Chance, Roll: Positive;
   begin
      if SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      if SkyBases(BaseIndex).Population < 150 then
         Chance := 1;
      elsif SkyBases(BaseIndex).Population < 300 then
         Chance := 2;
      else
         Chance := 5;
      end if;
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
