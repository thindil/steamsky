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

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Utils; use Utils;
with Goals; use Goals;

package body Bases is

   procedure GainRep(BaseIndex: Positive; Points: Integer) is
      NewPoints: Integer;
   begin
      if SkyBases(BaseIndex).Reputation(1) = -100 or
        SkyBases(BaseIndex).Reputation(1) = 100 then
         return;
      end if;
      NewPoints := SkyBases(BaseIndex).Reputation(2) + Points;
      if BaseIndex = PlayerShip.HomeBase then
         NewPoints := NewPoints + Points;
      end if;
      while NewPoints < 0 loop
         SkyBases(BaseIndex).Reputation(1) :=
           SkyBases(BaseIndex).Reputation(1) - 1;
         NewPoints := NewPoints + abs (SkyBases(BaseIndex).Reputation(1) * 10);
         if NewPoints >= 0 then
            SkyBases(BaseIndex).Reputation(2) := NewPoints;
            return;
         end if;
      end loop;
      while NewPoints > abs (SkyBases(BaseIndex).Reputation(1) * 10) loop
         NewPoints := NewPoints - abs (SkyBases(BaseIndex).Reputation(1) * 10);
         SkyBases(BaseIndex).Reputation(1) :=
           SkyBases(BaseIndex).Reputation(1) + 1;
      end loop;
      SkyBases(BaseIndex).Reputation(2) := NewPoints;
      if SkyBases(BaseIndex).Reputation(1) = 100 then
         UpdateGoal
           (REPUTATION,
            To_Unbounded_String
              (Bases_Owners'Image(SkyBases(BaseIndex).Owner)));
      end if;
   end GainRep;

   procedure CountPrice
     (Price: in out Positive;
      TraderIndex: Natural;
      Reduce: Boolean := True) is
      Bonus: Natural := 0;
   begin
      if TraderIndex > 0 then
         Bonus :=
           Integer
             (Float'Floor
                (Float(Price) *
                 (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
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
     return Unbounded_String is -- based on name generator from libtcod
      NewName: Unbounded_String;
   begin
      NewName := Null_Unbounded_String;
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
             (BaseSyllablesEnd.First_Index,
              BaseSyllablesEnd.Last_Index));
      if GetRandom(1, 100) < 16 then
         NewName :=
           NewName &
           " " &
           BaseSyllablesPost
             (GetRandom
                (BaseSyllablesPost.First_Index,
                 BaseSyllablesPost.Last_Index));
      end if;
      return NewName;
   end GenerateBaseName;

   procedure GenerateRecruits is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MaxRecruits,
      RecruitsAmount,
      SkillsAmount,
      SkillNumber,
      SkillLevel: Positive;
      BaseRecruits: Recruit_Container.Vector;
      Skills: Skills_Container.Vector;
      Gender: Character;
      Price: Natural;
      SkillIndex: Integer;
      Attributes: Attributes_Container.Vector;
   begin
      if DaysDifference(SkyBases(BaseIndex).RecruitDate) < 30 or
        SkyBases(BaseIndex).Owner = Abandoned then
         return;
      end if;
      if SkyBases(BaseIndex).Population < 150 then
         MaxRecruits := 5;
      elsif SkyBases(BaseIndex).Population > 149 and
        SkyBases(BaseIndex).Population < 300 then
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
         if GetRandom(1, 2) = 1 then
            Gender := 'M';
         else
            Gender := 'F';
         end if;
         BaseRecruits.Append
         (New_Item =>
            (Name => GenerateMemberName(Gender),
             Gender => Gender,
             Price => 1,
             Skills => Skills,
             Attributes => Attributes));
         SkillsAmount :=
           GetRandom(Skills_Names.First_Index, Skills_Names.Last_Index);
         for J in 1 .. SkillsAmount loop
            SkillNumber :=
              GetRandom(Skills_Names.First_Index, Skills_Names.Last_Index);
            SkillLevel := GetRandom(1, 100);
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
               (Index => SkillIndex, New_Item => (SkillNumber, SkillLevel, 0));
            end if;
         end loop;
         for J in Attributes_Names.Iterate loop
            Attributes.Append(New_Item => (GetRandom(3, 50), 0));
         end loop;
         for Skill of Skills loop
            Price := Price + Skill(2);
         end loop;
         for Stat of Attributes loop
            Price := Price + (Stat(2) * 5);
         end loop;
         Price := Price * 100;
         BaseRecruits(BaseRecruits.Last_Index).Skills := Skills;
         BaseRecruits(BaseRecruits.Last_Index).Attributes := Attributes;
         BaseRecruits(BaseRecruits.Last_Index).Price := Price;
      end loop;
      SkyBases(BaseIndex).RecruitDate := GameDate;
      SkyBases(BaseIndex).Recruits := BaseRecruits;
   end GenerateRecruits;

   procedure AskForBases is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Radius, TempX, TempY: Integer;
      Amount, TmpBaseIndex: Natural;
      TraderIndex, ShipIndex: Positive;
      UnknownBases: Natural := 0;
   begin
      TraderIndex := FindMember(Talk);
      if BaseIndex > 0 then -- asking in base
         if SkyBases(BaseIndex).Population < 150 then
            Amount := 10;
            Radius := 10;
         elsif SkyBases(BaseIndex).Population > 149 and
           SkyBases(BaseIndex).Population < 300 then
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
            To_String(SkyBases(BaseIndex).Name) &
            "'.",
            OrderMessage);
      else -- asking friendly ship
         Radius := 40;
         ShipIndex :=
           Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .Data;
         if ProtoShips_List(ShipIndex).Crew.Length < 5 then
            Amount := 3;
         elsif ProtoShips_List(ShipIndex).Crew.Length < 10 then
            Amount := 5;
         else
            Amount := 10;
         end if;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) &
            " asked ship '" &
            To_String(GenerateShipName) &
            "' for directions to other bases.",
            OrderMessage);
         DeleteEvent(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex);
         UpdateOrders;
      end if;
      Bases_Loop:
      for X in -Radius .. Radius loop
         for Y in -Radius .. Radius loop
            TempX := PlayerShip.SkyX + X;
            if TempX < 1 then
               TempX := 1;
            elsif TempX > 1024 then
               TempX := 1024;
            end if;
            TempY := PlayerShip.SkyY + Y;
            if TempY < 1 then
               TempY := 1;
            elsif TempY > 1024 then
               TempY := 1024;
            end if;
            TmpBaseIndex := SkyMap(TempX, TempY).BaseIndex;
            if TmpBaseIndex > 0 then
               if not SkyBases(TmpBaseIndex).Known then
                  SkyBases(TmpBaseIndex).Known := True;
                  Amount := Amount - 1;
                  exit Bases_Loop when Amount = 0;
               end if;
            end if;
         end loop;
      end loop Bases_Loop;
      if Amount > 0 then
         if BaseIndex > 0 then -- asking in base
            if SkyBases(BaseIndex).Population < 150 then
               if Amount > 1 then
                  Amount := 1;
               end if;
            elsif SkyBases(BaseIndex).Population > 149 and
              SkyBases(BaseIndex).Population < 300 then
               if Amount > 2 then
                  Amount := 2;
               end if;
            else
               if Amount > 4 then
                  Amount := 4;
               end if;
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
      GainExp(1, 4, TraderIndex);
      UpdateGame(30);
   end AskForBases;

   procedure AskForEvents is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MaxEvents,
      EventsAmount,
      TmpBaseIndex,
      ItemIndex,
      ShipIndex,
      TraderIndex: Positive;
      Event: Events_Types;
      EventX, EventY, EventTime, DiffX, DiffY: Positive;
      MinX, MinY, MaxX, MaxY: Integer;
      Enemies: Positive_Container.Vector;
      PlayerValue: Natural := 0;
      Attempts: Natural;
   begin
      TraderIndex := FindMember(Talk);
      if BaseIndex > 0 then -- asking in base
         if SkyBases(BaseIndex).Population < 150 then
            MaxEvents := 5;
         elsif SkyBases(BaseIndex).Population > 149 and
           SkyBases(BaseIndex).Population < 300 then
            MaxEvents := 10;
         else
            MaxEvents := 15;
         end if;
         SkyBases(BaseIndex).AskedForEvents := GameDate;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) &
            " asked for events in base '" &
            To_String(SkyBases(BaseIndex).Name) &
            "'.",
            OrderMessage);
         GainRep(BaseIndex, 1);
      else -- asking friendly ship
         ShipIndex :=
           Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .Data;
         if ProtoShips_List(ShipIndex).Crew.Length < 5 then
            MaxEvents := 1;
         elsif ProtoShips_List(ShipIndex).Crew.Length < 10 then
            MaxEvents := 3;
         else
            MaxEvents := 5;
         end if;
         AddMessage
           (To_String(PlayerShip.Crew(TraderIndex).Name) &
            " asked ship '" &
            To_String(GenerateShipName) &
            "' for events.",
            OrderMessage);
         DeleteEvent(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex);
         UpdateOrders;
      end if;
      EventsAmount := GetRandom(1, MaxEvents);
      MinX := PlayerShip.SkyX - 100;
      if MinX < 1 then
         MinX := 1;
      end if;
      MaxX := PlayerShip.SkyX + 100;
      if MaxX > 1024 then
         MaxX := 1024;
      end if;
      MinY := PlayerShip.SkyY - 100;
      if MinY < 1 then
         MinY := 1;
      end if;
      MaxY := PlayerShip.SkyY + 100;
      if MaxY > 1024 then
         MaxY := 1024;
      end if;
      if GetRandom(1, 100) < 99 then
         for Module of PlayerShip.Modules loop
            case Modules_List(Module.ProtoIndex).MType is
               when HULL | GUN | BATTERING_RAM =>
                  PlayerValue :=
                    PlayerValue + Module.MaxDurability + (Module.Data(2) * 10);
               when ARMOR =>
                  PlayerValue := PlayerValue + Module.MaxDurability;
               when others =>
                  null;
            end case;
         end loop;
         for Item of PlayerShip.Cargo loop
            if Length(Items_List(Item.ProtoIndex).IType) >= 4 then
               if Slice(Items_List(Item.ProtoIndex).IType, 1, 4) = "Ammo" then
                  PlayerValue :=
                    PlayerValue + (Items_List(Item.ProtoIndex).Value * 10);
               end if;
            end if;
         end loop;
         for C in ProtoShips_List.Iterate loop
            if ProtoShips_List(C).CombatValue <= PlayerValue and
              (ProtoShips_List(C).Owner /= Poleis and
               ProtoShips_List(C).Owner /= Independent) then
               Enemies.Append(New_Item => ProtoShips_Container.To_Index(C));
            end if;
         end loop;
      else
         for C in ProtoShips_List.Iterate loop
            if ProtoShips_List(C).Owner /= Poleis and
              ProtoShips_List(C).Owner /= Independent then
               Enemies.Append(New_Item => ProtoShips_Container.To_Index(C));
            end if;
         end loop;
      end if;
      for I in 1 .. EventsAmount loop
         Event := Events_Types'Val(GetRandom(1, 5));
         Attempts := 10;
         loop
            if Event = EnemyShip then
               EventX := GetRandom(MinX, MaxX);
               EventY := GetRandom(MinY, MaxY);
               exit when SkyMap(EventX, EventY).BaseIndex = 0 and
                 EventX /= PlayerShip.SkyX and
                 EventY /= PlayerShip.SkyY and
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
               if EventX /= PlayerShip.SkyX and
                 EventY /= PlayerShip.SkyY and
                 SkyMap(EventX, EventY).EventIndex = 0 and
                 SkyBases(SkyMap(EventX, EventY).BaseIndex).Known then
                  if Event = AttackOnBase and
                    SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner /=
                      Abandoned then
                     exit;
                  end if;
                  if (Event = Disease or Event = DoublePrice) and
                    (SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner /=
                     Abandoned or
                     SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner /=
                       Drones or
                     SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner /=
                       Undead) then
                     exit;
                  end if;
                  if Event = BaseRecovery and
                    SkyBases(SkyMap(EventX, EventY).BaseIndex).Owner =
                      Abandoned then
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
                  (EnemyShip,
                   EventX,
                   EventY,
                   GetRandom(EventTime, EventTime + 60),
                   Enemies
                     (GetRandom(Enemies.First_Index, Enemies.Last_Index))));
            when AttackOnBase =>
               Events_List.Append
               (New_Item =>
                  (AttackOnBase,
                   EventX,
                   EventY,
                   GetRandom(EventTime, EventTime + 120),
                   Enemies
                     (GetRandom(Enemies.First_Index, Enemies.Last_Index))));
            when Disease =>
               Events_List.Append
               (New_Item =>
                  (Disease, EventX, EventY, GetRandom(10080, 12000), 1));
            when DoublePrice =>
               loop
                  ItemIndex :=
                    GetRandom(Items_List.First_Index, Items_List.Last_Index);
                  exit when Items_List(ItemIndex).Prices(1) > 0;
               end loop;
               Events_List.Append
               (New_Item =>
                  (DoublePrice,
                   EventX,
                   EventY,
                   GetRandom((EventTime * 3), (EventTime * 4)),
                   ItemIndex));
            when BaseRecovery =>
               RecoverBase(SkyMap(EventX, EventY).BaseIndex);
            when others =>
               null;
         end case;
         if Event /= BaseRecovery then
            SkyMap(EventX, EventY).EventIndex := Events_List.Last_Index;
         end if;
      end loop;
      GainExp(1, 4, TraderIndex);
      UpdateGame(30);
   end AskForEvents;

   procedure UpdatePopulation is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      PopulationDiff: Integer;
   begin
      if DaysDifference(SkyBases(BaseIndex).RecruitDate) < 30 then
         return;
      end if;
      if SkyBases(BaseIndex).Owner /= Abandoned then
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
            SkyBases(BaseIndex).Owner := Abandoned;
            SkyBases(BaseIndex).Reputation := (0, 0);
         end if;
      else
         if GetRandom(1, 100) > 5 then
            return;
         end if;
         SkyBases(BaseIndex).Population := GetRandom(5, 10);
         loop
            SkyBases(BaseIndex).Owner :=
              Bases_Owners'Val
                (GetRandom
                   (Bases_Owners'Pos(Bases_Owners'First),
                    Bases_Owners'Pos(Bases_Owners'Last)));
            exit when SkyBases(BaseIndex).Owner /= Abandoned and
              SkyBases(BaseIndex).Owner /= Any;
         end loop;
      end if;
   end UpdatePopulation;

   procedure GenerateCargo is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      Chance, Roll: Positive;
   begin
      if SkyBases(BaseIndex).Population < 150 then
         Chance := 5;
      elsif SkyBases(BaseIndex).Population > 149 and
        SkyBases(BaseIndex).Population < 300 then
         Chance := 10;
      else
         Chance := 15;
      end if;
      Chance := Chance + DaysDifference(SkyBases(BaseIndex).Visited);
      if SkyBases(BaseIndex).Cargo.Length = 0 then
         Chance := 101;
      end if;
      if GetRandom(1, 100) > Chance then
         return;
      end if;
      if SkyBases(BaseIndex).Cargo.Length = 0 then
         SkyBases(BaseIndex).Cargo.Append
         (New_Item =>
            (ProtoIndex => FindProtoItem(MoneyIndex),
             Amount => (GetRandom(50, 200) * SkyBases(BaseIndex).Population),
             Durability => 100,
             Price => 0));
         for I in Items_List.Iterate loop
            if Items_List(I).Buyable(BaseType) then
               SkyBases(BaseIndex).Cargo.Append
               (New_Item =>
                  (ProtoIndex => Objects_Container.To_Index(I),
                   Amount =>
                     (GetRandom(0, 100) * SkyBases(BaseIndex).Population),
                   Durability => 100,
                   Price => Items_List(I).Prices(BaseType)));
            end if;
         end loop;
      else
         for Item of SkyBases(BaseIndex).Cargo loop
            Roll := GetRandom(1, 100);
            if Roll < 30 and Item.Amount > 0 then
               Item.Amount := Item.Amount - (GetRandom(1, (Item.Amount / 2)));
            elsif Roll < 60 and SkyBases(BaseIndex).Owner /= Abandoned then
               if Item.Amount = 0 then
                  Item.Amount :=
                    GetRandom(1, 10) * SkyBases(BaseIndex).Population;
               else
                  Item.Amount :=
                    Item.Amount + (GetRandom(1, (Item.Amount / 2)));
               end if;
            end if;
         end loop;
      end if;
   end GenerateCargo;

   procedure UpdateBaseCargo
     (ProtoIndex: Natural := 0;
      Amount: Integer;
      Durability: Natural := 100;
      CargoIndex: Natural := 0) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      ItemIndex: Natural;
   begin
      if ProtoIndex > 0 then
         ItemIndex := FindBaseCargo(ProtoIndex, Durability);
      else
         ItemIndex := CargoIndex;
      end if;
      if Amount > 0 then
         if ItemIndex = 0 then
            SkyBases(BaseIndex).Cargo.Append
            (New_Item =>
               (ProtoIndex => ProtoIndex,
                Amount => Amount,
                Durability => Durability,
                Price => Items_List(ProtoIndex).Prices(BaseType)));
         else
            SkyBases(BaseIndex).Cargo(ItemIndex).Amount :=
              SkyBases(BaseIndex).Cargo(ItemIndex).Amount + Amount;
         end if;
      else
         SkyBases(BaseIndex).Cargo(ItemIndex).Amount :=
           SkyBases(BaseIndex).Cargo(ItemIndex).Amount + Amount;
         if SkyBases(BaseIndex).Cargo(ItemIndex).Amount = 0 and
           not Items_List(SkyBases(BaseIndex).Cargo(ItemIndex).ProtoIndex)
             .Buyable
             (BaseType) and
           ItemIndex > 1 then
            SkyBases(BaseIndex).Cargo.Delete(Index => ItemIndex, Count => 1);
         end if;
      end if;
   end UpdateBaseCargo;

   function FindBaseCargo
     (ProtoIndex: Positive;
      Durability: Natural := 101) return Natural is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      for I in SkyBases(BaseIndex).Cargo.Iterate loop
         if Durability < 101 then
            if SkyBases(BaseIndex).Cargo(I).ProtoIndex = ProtoIndex and
              SkyBases(BaseIndex).Cargo(I).Durability = Durability then
               return BaseCargo_Container.To_Index(I);
            end if;
         else
            if SkyBases(BaseIndex).Cargo(I).ProtoIndex = ProtoIndex then
               return BaseCargo_Container.To_Index(I);
            end if;
         end if;
      end loop;
      return 0;
   end FindBaseCargo;

   procedure UpdatePrices is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Chance, Roll: Positive;
   begin
      if SkyBases(BaseIndex).Owner = Abandoned then
         return;
      end if;
      if SkyBases(BaseIndex).Population < 150 then
         Chance := 1;
      elsif SkyBases(BaseIndex).Population > 149 and
        SkyBases(BaseIndex).Population < 300 then
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
