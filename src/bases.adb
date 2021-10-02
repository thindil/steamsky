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

   procedure Gain_Rep(Base_Index: Bases_Range; Points: Integer) is
      New_Points: Integer;
   begin
      if Sky_Bases(Base_Index).Reputation(1) = -100 or
        Sky_Bases(Base_Index).Reputation(1) = 100 then
         return;
      end if;
      New_Points :=
        Sky_Bases(Base_Index).Reputation(2) +
        Integer(Float(Points) * Float(New_Game_Settings.Reputation_Bonus));
      if Base_Index = Player_Ship.Home_Base then
         New_Points := New_Points + Points;
      end if;
      Reduce_Reputation_Loop :
      while New_Points < 0 loop
         Sky_Bases(Base_Index).Reputation(1) :=
           Sky_Bases(Base_Index).Reputation(1) - 1;
         New_Points :=
           New_Points + abs (Sky_Bases(Base_Index).Reputation(1) * 5);
         if New_Points >= 0 then
            Sky_Bases(Base_Index).Reputation(2) := New_Points;
            return;
         end if;
      end loop Reduce_Reputation_Loop;
      Raise_Reputation_Loop :
      while New_Points > abs (Sky_Bases(Base_Index).Reputation(1) * 5) loop
         New_Points :=
           New_Points - abs (Sky_Bases(Base_Index).Reputation(1) * 5);
         Sky_Bases(Base_Index).Reputation(1) :=
           Sky_Bases(Base_Index).Reputation(1) + 1;
      end loop Raise_Reputation_Loop;
      Sky_Bases(Base_Index).Reputation(2) := New_Points;
      if Sky_Bases(Base_Index).Reputation(1) = 100 then
         UpdateGoal
           (GType => REPUTATION, TargetIndex => Sky_Bases(Base_Index).Owner);
      end if;
   end Gain_Rep;

   procedure Count_Price
     (Price: in out Natural; Trader_Index: Crew_Container.Extended_Index;
      Reduce: Boolean := True) is
      Bonus: Integer := 0;
   begin
      if Price = 0 then
         return;
      end if;
      if Trader_Index /= Crew_Container.No_Index then
         Bonus :=
           Integer
             (Float'Floor
                (Float(Price) *
                 (Float
                    (GetSkillLevel
                       (Member => Player_Ship.Crew(Trader_Index),
                        SkillIndex => Talking_Skill)) /
                  200.0)));
      end if;
      if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex > 0 then
         case Sky_Bases(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex)
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
   end Count_Price;

   function Generate_Base_Name
     (Faction_Index: Unbounded_String) return Unbounded_String is
      New_Name: Unbounded_String := Null_Unbounded_String;
   begin
      if Factions_List(Faction_Index).NamesType = ROBOTIC then
         return Generate_Robotic_Name;
      end if;
      if Get_Random(Min => 1, Max => 100) < 16 then
         New_Name :=
           Base_Syllables_Pre
             (Get_Random
                (Min => Base_Syllables_Pre.First_Index,
                 Max => Base_Syllables_Pre.Last_Index)) &
           " ";
      end if;
      New_Name :=
        New_Name &
        Base_Syllables_Start
          (Get_Random
             (Min => Base_Syllables_Start.First_Index,
              Max => Base_Syllables_Start.Last_Index)) &
        Base_Syllables_End
          (Get_Random
             (Min => Base_Syllables_End.First_Index,
              Max => Base_Syllables_End.Last_Index));
      if Get_Random(Min => 1, Max => 100) < 16 then
         New_Name :=
           New_Name & " " &
           Base_Syllables_Post
             (Get_Random
                (Min => Base_Syllables_Post.First_Index,
                 Max => Base_Syllables_Post.Last_Index));
      end if;
      return New_Name;
   end Generate_Base_Name;

   procedure Generate_Recruits is
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Recruit_Base: Bases_Range;
      Base_Recruits: Recruit_Container.Vector;
      Skills: Skills_Container.Vector;
      Gender: Character;
      Price, Payment: Natural;
      Skill_Index: Integer range -1 .. Integer'Last;
      Attributes: Mob_Attributes
        (1 ..
             Positive
               (AttributesData_Container.Length
                  (Container => Attributes_List)));
      Inventory, Temp_Tools: UnboundedString_Container.Vector;
      Equipment: Equipment_Array;
      Max_Skill_Level: Integer range -100 .. 100;
      Skill_Level, Highest_Level: Skill_Range;
      Recruit_Faction: Unbounded_String;
      Max_Recruits, Recruits_Amount: Positive range 1 .. 30;
      Skills_Amount, Skill_Number,
      Highest_Skill: Skills_Container.Extended_Index := 0;
      Max_Skill_Amount: Integer;
      procedure Add_Inventory
        (Items_Indexes: UnboundedString_Container.Vector;
         Equip_Index: Positive) is
         Item_Index: Unbounded_String;
      begin
         if Get_Random(1, 100) > 80 then
            return;
         end if;
         Item_Index :=
           GetRandomItem
             (Items_Indexes, Equip_Index, Highest_Level, Skills(1).Level,
              Recruit_Faction);
         if Item_Index = Null_Unbounded_String then
            return;
         end if;
         Inventory.Append(New_Item => Item_Index);
         Equipment(Equip_Index) := Inventory.Last_Index;
         Price :=
           Price + Get_Price(Sky_Bases(Base_Index).Base_Type, Item_Index);
         Payment :=
           Payment +
           (Get_Price(Sky_Bases(Base_Index).Base_Type, Item_Index) / 10);
      end Add_Inventory;
   begin
      if Days_Difference(Sky_Bases(Base_Index).Recruit_Date) < 30 or
        Sky_Bases(Base_Index).Population = 0 then
         return;
      end if;
      Max_Recruits :=
        (if Sky_Bases(Base_Index).Population < 150 then 5
         elsif Sky_Bases(Base_Index).Population < 300 then 10 else 15);
      if BasesTypes_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
          (To_Unbounded_String("barracks")) then
         Max_Recruits := Max_Recruits * 2;
      end if;
      if Max_Recruits > (Sky_Bases(Base_Index).Population / 10) then
         Max_Recruits := (Sky_Bases(Base_Index).Population / 10) + 1;
      end if;
      Recruits_Amount := Get_Random(1, Max_Recruits);
      Max_Skill_Amount :=
        Integer
          (Float(SkillsData_Container.Length(Skills_List)) *
           (Float(Sky_Bases(Base_Index).Reputation(1)) / 100.0));
      if Max_Skill_Amount < 5 then
         Max_Skill_Amount := 5;
      end if;
      Generate_Recruits_Loop :
      for I in 1 .. Recruits_Amount loop
         Skills.Clear;
         Attributes := (others => <>);
         Price := 0;
         Inventory.Clear;
         Temp_Tools.Clear;
         Equipment := (others => 0);
         Payment := 0;
         Recruit_Faction :=
           (if Get_Random(1, 100) < 99 then Sky_Bases(Base_Index).Owner
            else GetRandomFaction);
         if not Factions_List(Recruit_Faction).Flags.Contains
             (To_Unbounded_String("nogender")) then
            Gender := (if Get_Random(1, 2) = 1 then 'M' else 'F');
         else
            Gender := 'M';
         end if;
         Skills_Amount := Get_Random(1, Skills_Amount);
         if Skills_Amount > Max_Skill_Amount then
            Skills_Amount := Max_Skill_Amount;
         end if;
         Highest_Level := 1;
         Highest_Skill := 1;
         Max_Skill_Level := Sky_Bases(Base_Index).Reputation(1);
         if Max_Skill_Level < 20 then
            Max_Skill_Level := 20;
         end if;
         if Get_Random(1, 100) > 95 then
            Max_Skill_Level := Get_Random(Max_Skill_Level, 100);
         end if;
         Generate_Skills_Loop :
         for J in 1 .. Skills_Amount loop
            Skill_Number :=
              (if J > 1 then Get_Random(1, Skills_Amount)
               else Factions_List(Recruit_Faction).WeaponSkill);
            Skill_Level := Get_Random(1, Max_Skill_Level);
            if Skill_Level > Highest_Level then
               Highest_Level := Skill_Level;
               Highest_Skill := Skill_Number;
            end if;
            Skill_Index := 0;
            Get_Skill_Index_Loop :
            for C in Skills.Iterate loop
               if Skills(C).Index = Skill_Number then
                  Skill_Index :=
                    (if Skills(C).Level < Skill_Level then
                       Skills_Container.To_Index(C)
                     else -1);
                  exit Get_Skill_Index_Loop;
               end if;
            end loop Get_Skill_Index_Loop;
            if Skill_Index = 0 then
               Skills.Append(New_Item => (Skill_Number, Skill_Level, 0));
            elsif Skill_Index > 0 then
               Skills.Replace_Element
                 (Index => Skill_Index,
                  New_Item => (Skill_Number, Skill_Level, 0));
            end if;
         end loop Generate_Skills_Loop;
         Generate_Attributes_Loop :
         for J in Attributes'Range loop
            Attributes(J) := (Get_Random(3, (Max_Skill_Level / 3)), 0);
         end loop Generate_Attributes_Loop;
         Update_Price_With_Skills_Loop :
         for Skill of Skills loop
            Price := Price + Skill.Level;
            Payment := Payment + Skill.Level;
         end loop Update_Price_With_Skills_Loop;
         Update_Price_With_Stats_Loop :
         for Stat of Attributes loop
            Price := Price + (Stat.Level * 2);
            Payment := Payment + (Stat.Level * 2);
         end loop Update_Price_With_Stats_Loop;
         Add_Inventory(Weapons_List, 1);
         Add_Inventory(Shields_List, 2);
         Add_Inventory(HeadArmors_List, 3);
         Add_Inventory(ChestArmors_List, 4);
         Add_Inventory(ArmsArmors_List, 5);
         Add_Inventory(LegsArmors_List, 6);
         Add_Tool_Loop :
         for Recipe of Recipes_List loop
            if Highest_Skill = Recipe.Skill then
               Find_Tool_Loop :
               for J in Items_List.Iterate loop
                  if Items_List(J).IType = Recipe.Tool then
                     Temp_Tools.Append(New_Item => Objects_Container.Key(J));
                  end if;
               end loop Find_Tool_Loop;
               Add_Inventory(Temp_Tools, 7);
               exit Add_Tool_Loop;
            end if;
         end loop Add_Tool_Loop;
         if BasesTypes_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
             (To_Unbounded_String("barracks")) then
            Price := Price / 2;
            Payment := Payment / 2;
         end if;
         Price :=
           Natural(Float(Price * 100) * Float(New_Game_Settings.Prices_Bonus));
         if Price = 0 then
            Price := 1;
         end if;
         Recruit_Base :=
           (if Get_Random(1, 100) < 99 then Base_Index
            else Get_Random(Sky_Bases'First, Sky_Bases'Last));
         Base_Recruits.Append
           (New_Item =>
              (Amount_Of_Attributes => Attributes_Amount,
               Amount_Of_Skills => Skills_Amount,
               Name => GenerateMemberName(Gender, Recruit_Faction),
               Gender => Gender, Price => Price, Skills => Skills,
               Attributes => Attributes, Inventory => Inventory,
               Equipment => Equipment, Payment => Payment,
               Home_Base => Recruit_Base, Faction => Recruit_Faction));
      end loop Generate_Recruits_Loop;
      Sky_Bases(Base_Index).Recruit_Date := Game_Date;
      Sky_Bases(Base_Index).Recruits := Base_Recruits;
   end Generate_Recruits;

   procedure Ask_For_Bases is
      Base_Index: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      TmpBase_Index: Extended_Base_Range;
      ShipIndex: Unbounded_String;
      UnknownBases: Extended_Base_Range := 0;
      Trader_Index: constant Natural := FindMember(Talk);
      Amount: Natural range 0 .. 40;
      Radius: Integer range -40 .. 40;
      TempX, TempY: Integer range -40 .. Bases_Range'Last + 40;
   begin
      if Trader_Index = 0 then
         return;
      end if;
      if Base_Index > 0 then -- asking in base
         if Sky_Bases(Base_Index).Population < 150 then
            Amount := 10;
            Radius := 10;
         elsif Sky_Bases(Base_Index).Population < 300 then
            Amount := 20;
            Radius := 20;
         else
            Amount := 40;
            Radius := 40;
         end if;
         Gain_Rep(Base_Index, 1);
         Sky_Bases(Base_Index).Asked_For_Bases := True;
         AddMessage
           (To_String(Player_Ship.Crew(Trader_Index).Name) &
            " asked for directions to other bases in base '" &
            To_String(Sky_Bases(Base_Index).Name) & "'.",
            OrderMessage);
      else -- asking friendly ship
         Radius := 40;
         ShipIndex :=
           (Events_List
              (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
              .ShipIndex);
         Amount :=
           (if Proto_Ships_List(ShipIndex).Crew.Length < 5 then 3
            elsif Proto_Ships_List(ShipIndex).Crew.Length < 10 then 5 else 10);
         AddMessage
           (To_String(Player_Ship.Crew(Trader_Index).Name) & " asked ship '" &
            To_String(Generate_Ship_Name(Proto_Ships_List(ShipIndex).Owner)) &
            "' for directions to other bases.",
            OrderMessage);
         DeleteEvent(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex);
         UpdateOrders(Player_Ship);
      end if;
      Bases_Loop :
      for X in -Radius .. Radius loop
         for Y in -Radius .. Radius loop
            TempX := Player_Ship.Sky_X + X;
            NormalizeCoord(TempX);
            TempY := Player_Ship.Sky_Y + Y;
            NormalizeCoord(TempY, False);
            TmpBase_Index := SkyMap(TempX, TempY).BaseIndex;
            if TmpBase_Index > 0
              and then not Sky_Bases(TmpBase_Index).Known then
               Sky_Bases(TmpBase_Index).Known := True;
               Amount := Amount - 1;
               exit Bases_Loop when Amount = 0;
            end if;
         end loop;
      end loop Bases_Loop;
      if Amount > 0 then
         if Base_Index > 0 then -- asking in base
            if Sky_Bases(Base_Index).Population < 150 and then Amount > 1 then
               Amount := 1;
            elsif Sky_Bases(Base_Index).Population < 300
              and then Amount > 2 then
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
         for I in Sky_Bases'Range loop
            if not Sky_Bases(I).Known then
               UnknownBases := UnknownBases + 1;
            end if;
            exit Count_Unknown_Bases when UnknownBases >= Amount;
         end loop Count_Unknown_Bases;
         if UnknownBases >= Amount then
            Reveal_Random_Bases_Loop :
            loop
               TmpBase_Index := Get_Random(1, 1_024);
               if not Sky_Bases(TmpBase_Index).Known then
                  Sky_Bases(TmpBase_Index).Known := True;
                  Amount := Amount - 1;
               end if;
               exit Reveal_Random_Bases_Loop when Amount = 0;
            end loop Reveal_Random_Bases_Loop;
         else
            Reveal_Bases_Loop :
            for I in Sky_Bases'Range loop
               if not Sky_Bases(I).Known then
                  Sky_Bases(I).Known := True;
               end if;
            end loop Reveal_Bases_Loop;
         end if;
      end if;
      GainExp(1, Talking_Skill, Trader_Index);
      Update_Game(30);
   end Ask_For_Bases;

   procedure Ask_For_Events is
      Base_Index: constant Extended_Base_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      EventTime, DiffX, DiffY: Positive;
      Event: Events_Types;
      MinX, MinY, MaxX, MaxY: Integer range -100 .. 1_124;
      Enemies: UnboundedString_Container.Vector;
      Attempts: Natural range 0 .. 10;
      NewItemIndex, ShipIndex: Unbounded_String;
      Trader_Index: constant Crew_Container.Extended_Index := FindMember(Talk);
      MaxEvents, EventsAmount: Positive range 1 .. 15;
      TmpBase_Index: Bases_Range;
      EventX, EventY: Positive range 1 .. 1_024;
      ItemIndex: Integer;
   begin
      if Trader_Index = 0 then
         return;
      end if;
      if Base_Index > 0 then -- asking in base
         MaxEvents :=
           (if Sky_Bases(Base_Index).Population < 150 then 5
            elsif Sky_Bases(Base_Index).Population < 300 then 10 else 15);
         Sky_Bases(Base_Index).Asked_For_Events := Game_Date;
         AddMessage
           (To_String(Player_Ship.Crew(Trader_Index).Name) &
            " asked for recent events known at base '" &
            To_String(Sky_Bases(Base_Index).Name) & "'.",
            OrderMessage);
         Gain_Rep(Base_Index, 1);
      else -- asking friendly ship
         ShipIndex :=
           Events_List(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
             .ShipIndex;
         MaxEvents :=
           (if Proto_Ships_List(ShipIndex).Crew.Length < 5 then 1
            elsif Proto_Ships_List(ShipIndex).Crew.Length < 10 then 3 else 5);
         AddMessage
           (To_String(Player_Ship.Crew(Trader_Index).Name) & " asked ship '" &
            To_String(Generate_Ship_Name(Proto_Ships_List(ShipIndex).Owner)) &
            "' for recent events.",
            OrderMessage);
         DeleteEvent(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex);
         UpdateOrders(Player_Ship);
      end if;
      EventsAmount := Get_Random(1, MaxEvents);
      MinX := Player_Ship.Sky_X - 100;
      NormalizeCoord(MinX);
      MaxX := Player_Ship.Sky_X + 100;
      NormalizeCoord(MaxX);
      MinY := Player_Ship.Sky_Y - 100;
      NormalizeCoord(MinY, False);
      MaxY := Player_Ship.Sky_Y + 100;
      NormalizeCoord(MaxY, False);
      GenerateEnemies(Enemies);
      Generate_Events_Loop :
      for I in 1 .. EventsAmount loop
         Event := Events_Types'Val(Get_Random(1, 5));
         Attempts := 10;
         Generate_Event_Location_Loop :
         loop
            if Event = EnemyShip then
               EventX := Get_Random(MinX, MaxX);
               EventY := Get_Random(MinY, MaxY);
               exit Generate_Event_Location_Loop when SkyMap(EventX, EventY)
                   .BaseIndex =
                 0 and
                 EventX /= Player_Ship.Sky_X and
                 EventY /= Player_Ship.Sky_Y and
                 SkyMap(EventX, EventY).EventIndex = 0;
            else
               TmpBase_Index := Get_Random(1, 1_024);
               EventX := Sky_Bases(TmpBase_Index).Sky_X;
               EventY := Sky_Bases(TmpBase_Index).Sky_Y;
               Attempts := Attempts - 1;
               if Attempts = 0 then
                  Event := EnemyShip;
                  Regenerate_Event_Location_Loop :
                  loop
                     EventX := Get_Random(MinX, MaxX);
                     EventY := Get_Random(MinY, MaxY);
                     exit Regenerate_Event_Location_Loop when SkyMap
                         (EventX, EventY)
                         .BaseIndex =
                       0 and
                       EventX /= Player_Ship.Sky_X and
                       EventY /= Player_Ship.Sky_Y and
                       SkyMap(EventX, EventY).EventIndex = 0;
                  end loop Regenerate_Event_Location_Loop;
                  exit Generate_Event_Location_Loop;
               end if;
               if EventX /= Player_Ship.Sky_X and
                 EventY /= Player_Ship.Sky_Y and
                 SkyMap(EventX, EventY).EventIndex = 0 and
                 Sky_Bases(SkyMap(EventX, EventY).BaseIndex).Known then
                  if Event = AttackOnBase and
                    Sky_Bases(SkyMap(EventX, EventY).BaseIndex).Population /=
                      0 then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = DoublePrice and
                    IsFriendly
                      (Player_Ship.Crew(1).Faction,
                       Sky_Bases(SkyMap(EventX, EventY).BaseIndex).Owner) then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = Disease and
                    not Factions_List
                      (Sky_Bases(SkyMap(EventX, EventY).BaseIndex).Owner)
                      .Flags
                      .Contains
                      (To_Unbounded_String("diseaseimmune")) and
                    IsFriendly
                      (Player_Ship.Crew(1).Faction,
                       Sky_Bases(SkyMap(EventX, EventY).BaseIndex).Owner) then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = BaseRecovery and
                    Sky_Bases(SkyMap(EventX, EventY).BaseIndex).Population =
                      0 then
                     exit Generate_Event_Location_Loop;
                  end if;
               end if;
            end if;
         end loop Generate_Event_Location_Loop;
         DiffX := abs (Player_Ship.Sky_X - EventX);
         DiffY := abs (Player_Ship.Sky_Y - EventY);
         EventTime := Positive(60.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
         case Event is
            when EnemyShip =>
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, EventX, EventY,
                     Get_Random(EventTime, EventTime + 60),
                     Enemies
                       (Get_Random(Enemies.First_Index, Enemies.Last_Index))));
            when AttackOnBase =>
               GenerateEnemies(Enemies, To_Unbounded_String("Any"), False);
               Events_List.Append
                 (New_Item =>
                    (AttackOnBase, EventX, EventY,
                     Get_Random(EventTime, EventTime + 120),
                     Enemies
                       (Get_Random(Enemies.First_Index, Enemies.Last_Index))));
               GenerateEnemies(Enemies);
            when Disease =>
               Events_List.Append
                 (New_Item =>
                    (Disease, EventX, EventY, Get_Random(10_080, 12_000), 1));
            when DoublePrice =>
               loop
                  ItemIndex := Get_Random(1, Positive(Items_List.Length));
                  for J in Items_List.Iterate loop
                     ItemIndex := ItemIndex - 1;
                     if ItemIndex <= 0
                       and then
                         Get_Price
                           (Sky_Bases(SkyMap(EventX, EventY).BaseIndex)
                              .Base_Type,
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
                     Get_Random((EventTime * 3), (EventTime * 4)),
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
      GainExp(1, Talking_Skill, Trader_Index);
      Update_Game(30);
   end Ask_For_Events;

   procedure Update_Population is
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      PopulationDiff: Integer;
   begin
      if Days_Difference(Sky_Bases(Base_Index).Recruit_Date) < 30 then
         return;
      end if;
      if Sky_Bases(Base_Index).Population > 0 then
         if Get_Random(1, 100) > 30 then
            return;
         end if;
         PopulationDiff :=
           (if Get_Random(1, 100) < 20 then -(Get_Random(1, 10))
            else Get_Random(1, 10));
         if Sky_Bases(Base_Index).Population + PopulationDiff < 0 then
            PopulationDiff := -(Sky_Bases(Base_Index).Population);
         end if;
         Sky_Bases(Base_Index).Population :=
           Sky_Bases(Base_Index).Population + PopulationDiff;
         if Sky_Bases(Base_Index).Population = 0 then
            Sky_Bases(Base_Index).Reputation := (0, 0);
         end if;
      else
         if Get_Random(1, 100) > 5 then
            return;
         end if;
         Sky_Bases(Base_Index).Population := Get_Random(5, 10);
         Sky_Bases(Base_Index).Owner := GetRandomFaction;
      end if;
   end Update_Population;

   procedure Update_Prices is
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Roll: Positive range 1 .. 100;
      Chance: Positive;
   begin
      if Sky_Bases(Base_Index).Population = 0 then
         return;
      end if;
      Chance :=
        (if Sky_Bases(Base_Index).Population < 150 then 1
         elsif Sky_Bases(Base_Index).Population < 300 then 2 else 5);
      Chance := Chance + (Days_Difference(Sky_Bases(Base_Index).Visited) / 10);
      if Get_Random(1, 100) > Chance then
         return;
      end if;
      for Item of Sky_Bases(Base_Index).Cargo loop
         Roll := Get_Random(1, 100);
         if Roll < 30 and Item.Price > 1 then
            Item.Price := Item.Price - 1;
         elsif Roll < 60 and Item.Price > 0 then
            Item.Price := Item.Price + 1;
         end if;
      end loop;
   end Update_Prices;

end Bases;
