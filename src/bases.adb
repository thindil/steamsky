--    Copyright 2016-2022 Bartek thindil Jasicki
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
      use Tiny_String;

      New_Points: Integer;
   begin
      if Sky_Bases(Base_Index).Reputation.Level = -100 or
        Sky_Bases(Base_Index).Reputation.Level = 100 then
         return;
      end if;
      New_Points :=
        Sky_Bases(Base_Index).Reputation.Experience +
        Integer(Float(Points) * Float(New_Game_Settings.Reputation_Bonus));
      if Base_Index = Player_Ship.Home_Base then
         New_Points := New_Points + Points;
      end if;
      Reduce_Reputation_Loop :
      while New_Points < 0 loop
         Sky_Bases(Base_Index).Reputation.Level :=
           Sky_Bases(Base_Index).Reputation.Level - 1;
         New_Points :=
           New_Points + abs (Sky_Bases(Base_Index).Reputation.Level * 5);
         if New_Points >= 0 then
            Sky_Bases(Base_Index).Reputation.Experience := New_Points;
            return;
         end if;
      end loop Reduce_Reputation_Loop;
      Raise_Reputation_Loop :
      while New_Points > abs (Sky_Bases(Base_Index).Reputation.Level * 5) loop
         New_Points :=
           New_Points - abs (Sky_Bases(Base_Index).Reputation.Level * 5);
         Sky_Bases(Base_Index).Reputation.Level :=
           Sky_Bases(Base_Index).Reputation.Level + 1;
      end loop Raise_Reputation_Loop;
      Sky_Bases(Base_Index).Reputation.Experience := New_Points;
      if Sky_Bases(Base_Index).Reputation.Level = 100 then
         Update_Goal
           (G_Type => REPUTATION,
            Target_Index =>
              To_Unbounded_String
                (Source => To_String(Source => Sky_Bases(Base_Index).Owner)));
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
                    (Get_Skill_Level
                       (Member => Player_Ship.Crew(Trader_Index),
                        Skill_Index => Talking_Skill)) /
                  200.0)));
      end if;
      if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index > 0 then
         case Sky_Bases
           (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index)
           .Reputation
           .Level is
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
     (Faction_Index: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String is
      use Tiny_String;
      New_Name: Bounded_String := Null_Bounded_String;
   begin
      if Factions_List(Faction_Index).Names_Type = ROBOTIC then
         return Generate_Robotic_Name;
      end if;
      if Get_Random(Min => 1, Max => 100) < 16 then
         New_Name :=
           Syllable_String.To_String
             (Source =>
                SyllableString_Container.Element
                  (Container => Base_Syllables_Pre,
                   Index =>
                     (Get_Random
                        (Min =>
                           SyllableString_Container.First_Index
                             (Container => Base_Syllables_Pre),
                         Max =>
                           SyllableString_Container.Last_Index
                             (Container => Base_Syllables_Pre))))) &
           To_Bounded_String(Source => " ");
      end if;
      New_Name :=
        New_Name &
        Syllable_String.To_String
          (Source =>
             SyllableString_Container.Element
               (Container => Base_Syllables_Start,
                Index =>
                  (Get_Random
                     (Min =>
                        SyllableString_Container.First_Index
                          (Container => Base_Syllables_Start),
                      Max =>
                        SyllableString_Container.Last_Index
                          (Container => Base_Syllables_Start))))) &
        Syllable_String.To_String
          (Source =>
             SyllableString_Container.Element
               (Container => Base_Syllables_End,
                Index =>
                  (Get_Random
                     (Min =>
                        SyllableString_Container.First_Index
                          (Container => Base_Syllables_End),
                      Max =>
                        SyllableString_Container.Last_Index
                          (Container => Base_Syllables_End)))));
      if Get_Random(Min => 1, Max => 100) < 16 then
         New_Name :=
           New_Name & " " &
           Syllable_String.To_String
             (Source =>
                SyllableString_Container.Element
                  (Container => Base_Syllables_Post,
                   Index =>
                     (Get_Random
                        (Min =>
                           SyllableString_Container.First_Index
                             (Container => Base_Syllables_Post),
                         Max =>
                           SyllableString_Container.Last_Index
                             (Container => Base_Syllables_Post)))));
      end if;
      return New_Name;
   end Generate_Base_Name;

   procedure Generate_Recruits is
      use Tiny_String;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit_Base: Bases_Range;
      Base_Recruits: Recruit_Container.Vector;
      Skills: Skills_Container.Vector (Capacity => Skills_Amount);
      Gender: Character;
      Price, Payment: Natural;
      Skill_Index: Integer range -1 .. Integer'Last;
      Attributes: Mob_Attributes(1 .. Attributes_Amount);
      Inventory: Positive_Container.Vector;
      Temp_Tools: Positive_Container.Vector;
      Equipment: Equipment_Array;
      Max_Skill_Level: Integer range -100 .. 100;
      Skill_Level, Highest_Level: Skill_Range;
      Recruit_Faction: Bounded_String;
      Max_Recruits, Recruits_Amount: Positive range 1 .. 30;
      Local_Skills_Amount, Skill_Number, Highest_Skill: Skills_Amount_Range :=
        1;
      Max_Skill_Amount: Integer;
      procedure Add_Inventory
        (Items_Indexes: Positive_Container.Vector;
         Equip_Index: Equipment_Locations) is
         Item_Index: Objects_Container.Extended_Index;
      begin
         if Get_Random(Min => 1, Max => 100) > 80 then
            return;
         end if;
         Item_Index :=
           Get_Random_Item
             (Items_Indexes => Items_Indexes, Equip_Index => Equip_Index,
              Highest_Level => Highest_Level,
              Weapon_Skill_Level =>
                Skills_Container.Element(Container => Skills, Index => 1)
                  .Level,
              Faction_Index => Recruit_Faction);
         if Item_Index = 0 then
            return;
         end if;
         Positive_Container.Append
           (Container => Inventory, New_Item => Item_Index);
         Equipment(Equip_Index) :=
           Positive_Container.Last_Index(Container => Inventory);
         Price :=
           Price +
           Get_Price
             (Base_Type => Sky_Bases(Base_Index).Base_Type,
              Item_Index => Item_Index);
         Payment :=
           Payment +
           (Get_Price
              (Base_Type => Sky_Bases(Base_Index).Base_Type,
               Item_Index => Item_Index) /
            10);
      end Add_Inventory;
   begin
      if Days_Difference
          (Date_To_Compare => Sky_Bases(Base_Index).Recruit_Date) <
        30 or
        Sky_Bases(Base_Index).Population = 0 then
         return;
      end if;
      Max_Recruits :=
        (if Sky_Bases(Base_Index).Population < 150 then 5
         elsif Sky_Bases(Base_Index).Population < 300 then 10 else 15);
      if Bases_Types_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
          (Item => To_Unbounded_String(Source => "barracks")) then
         Max_Recruits := Max_Recruits * 2;
      end if;
      if Max_Recruits > (Sky_Bases(Base_Index).Population / 10) then
         Max_Recruits := (Sky_Bases(Base_Index).Population / 10) + 1;
      end if;
      Recruits_Amount := Get_Random(Min => 1, Max => Max_Recruits);
      Max_Skill_Amount :=
        Integer
          (Float(SkillsData_Container.Length(Container => Skills_List)) *
           (Float(Sky_Bases(Base_Index).Reputation.Level) / 100.0));
      if Max_Skill_Amount < 5 then
         Max_Skill_Amount := 5;
      end if;
      Generate_Recruits_Loop :
      for I in 1 .. Recruits_Amount loop
         Skills_Container.Clear(Container => Skills);
         Attributes := (others => <>);
         Price := 0;
         Positive_Container.Clear(Container => Inventory);
         Temp_Tools.Clear;
         Equipment := (others => 0);
         Payment := 0;
         Recruit_Faction :=
           (if Get_Random(Min => 1, Max => 100) < 99 then
              Sky_Bases(Base_Index).Owner
            else Get_Random_Faction);
         if not Factions_List(Recruit_Faction).Flags.Contains
             (Item => To_Unbounded_String(Source => "nogender")) then
            Gender :=
              (if Get_Random(Min => 1, Max => 2) = 1 then 'M' else 'F');
         else
            Gender := 'M';
         end if;
         Local_Skills_Amount :=
           Skills_Amount_Range
             (Get_Random(Min => 1, Max => Natural(Skills_Amount)));
         if Local_Skills_Amount > Skills_Amount_Range(Max_Skill_Amount) then
            Local_Skills_Amount := Skills_Amount_Range(Max_Skill_Amount);
         end if;
         Highest_Level := 1;
         Highest_Skill := 1;
         Max_Skill_Level := Sky_Bases(Base_Index).Reputation.Level;
         if Max_Skill_Level < 20 then
            Max_Skill_Level := 20;
         end if;
         if Get_Random(Min => 1, Max => 100) > 95 then
            Max_Skill_Level := Get_Random(Min => Max_Skill_Level, Max => 100);
         end if;
         Generate_Skills_Loop :
         for J in 1 .. Local_Skills_Amount loop
            Skill_Number :=
              (if J > 1 then
                 Skills_Amount_Range
                   (Get_Random(Min => 1, Max => Natural(Skills_Amount)))
               else Factions_List(Recruit_Faction).Weapon_Skill);
            Skill_Level := Get_Random(Min => 1, Max => Max_Skill_Level);
            if Skill_Level > Highest_Level then
               Highest_Level := Skill_Level;
               Highest_Skill := Skill_Number;
            end if;
            Skill_Index := 0;
            Get_Skill_Index_Loop :
            for C in
              Skills_Container.First_Index(Container => Skills) ..
                Skills_Container.Last_Index(Container => Skills) loop
               if Skills_Container.Element(Container => Skills, Index => C)
                   .Index =
                 Skill_Number then
                  Skill_Index :=
                    (if
                       Skills_Container.Element
                         (Container => Skills, Index => C)
                         .Level <
                       Skill_Level
                     then Integer(C)
                     else -1);
                  exit Get_Skill_Index_Loop;
               end if;
            end loop Get_Skill_Index_Loop;
            if Skill_Index = 0 then
               Skills_Container.Append
                 (Container => Skills,
                  New_Item =>
                    (Index => Skill_Number, Level => Skill_Level,
                     Experience => 0));
            elsif Skill_Index > 0 then
               Skills_Container.Replace_Element
                 (Container => Skills,
                  Index => Skills_Amount_Range(Skill_Index),
                  New_Item =>
                    (Index => Skill_Number, Level => Skill_Level,
                     Experience => 0));
            end if;
         end loop Generate_Skills_Loop;
         Generate_Attributes_Loop :
         for J in Attributes'Range loop
            Attributes(J) :=
              (Level => Get_Random(Min => 3, Max => (Max_Skill_Level / 3)),
               Experience => 0);
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
         Add_Inventory(Items_Indexes => Weapons_List, Equip_Index => WEAPON);
         Add_Inventory(Items_Indexes => Shields_List, Equip_Index => SHIELD);
         Add_Inventory
           (Items_Indexes => Head_Armors_List, Equip_Index => HELMET);
         Add_Inventory
           (Items_Indexes => Chest_Armors_List, Equip_Index => TORSO);
         Add_Inventory(Items_Indexes => Arms_Armors_List, Equip_Index => ARMS);
         Add_Inventory(Items_Indexes => Legs_Armors_List, Equip_Index => LEGS);
         Add_Tool_Loop :
         for Recipe of Recipes_List loop
            if Highest_Skill = Recipe.Skill then
               Find_Tool_Loop :
               for J in Items_List.Iterate loop
                  if Items_List(J).I_Type = Recipe.Tool then
                     Temp_Tools.Append
                       (New_Item => Objects_Container.To_Index(Position => J));
                  end if;
               end loop Find_Tool_Loop;
               Add_Inventory(Items_Indexes => Temp_Tools, Equip_Index => TOOL);
               exit Add_Tool_Loop;
            end if;
         end loop Add_Tool_Loop;
         if Bases_Types_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
             (Item => To_Unbounded_String(Source => "barracks")) then
            Price := Price / 2;
            Payment := Payment / 2;
         end if;
         Price :=
           Natural(Float(Price * 100) * Float(New_Game_Settings.Prices_Bonus));
         if Price = 0 then
            Price := 1;
         end if;
         Recruit_Base :=
           (if Get_Random(Min => 1, Max => 100) < 99 then Base_Index
            else Get_Random(Min => Sky_Bases'First, Max => Sky_Bases'Last));
         Recruit_Container.Append
           (Container => Base_Recruits,
            New_Item =>
              (Amount_Of_Attributes => Attributes_Amount,
               Amount_Of_Skills => Skills_Amount,
               Name =>
                 Tiny_String.To_Bounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Generate_Member_Name
                             (Gender => Gender,
                              Faction_Index => Recruit_Faction))),
               Gender => Gender, Price => Price, Skills => Skills,
               Attributes => Attributes, Inventory => Inventory,
               Equipment => Equipment, Payment => Payment,
               Home_Base => Recruit_Base, Faction => Recruit_Faction));
      end loop Generate_Recruits_Loop;
      Sky_Bases(Base_Index).Recruit_Date := Game_Date;
      Recruit_Container.Assign
        (Target => Sky_Bases(Base_Index).Recruits, Source => Base_Recruits);
   end Generate_Recruits;

   procedure Ask_For_Bases is
      use Tiny_String;

      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Tmp_Base_Index: Extended_Base_Range;
      Ship_Index: Proto_Ships_Container.Extended_Index;
      Unknown_Bases: Extended_Base_Range := 0;
      Trader_Index: constant Natural := Find_Member(Order => TALK);
      Amount: Natural range 0 .. 40;
      Radius: Integer range -40 .. 40;
      Temp_X, Temp_Y: Integer range -40 .. Bases_Range'Last + 40;
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
         Gain_Rep(Base_Index => Base_Index, Points => 1);
         Sky_Bases(Base_Index).Asked_For_Bases := True;
         Add_Message
           (Message =>
              To_String(Source => Player_Ship.Crew(Trader_Index).Name) &
              " asked for directions to other bases in base '" &
              To_String(Source => Sky_Bases(Base_Index).Name) & "'.",
            M_Type => ORDERMESSAGE);
      else -- asking friendly ship
         Radius := 40;
         Ship_Index :=
           Events_List
             (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
             .Ship_Index;
         Amount :=
           (if Proto_Ships_List(Ship_Index).Crew.Length < 5 then 3
            elsif Proto_Ships_List(Ship_Index).Crew.Length < 10 then 5
            else 10);
         Add_Message
           (Message =>
              To_String(Source => Player_Ship.Crew(Trader_Index).Name) &
              " asked ship '" &
              To_String
                (Source =>
                   Generate_Ship_Name
                     (Owner => Proto_Ships_List(Ship_Index).Owner)) &
              "' for directions to other bases.",
            M_Type => ORDERMESSAGE);
         Delete_Event
           (Event_Index =>
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index);
         Update_Orders(Ship => Player_Ship);
      end if;
      Bases_X_Loop :
      for X in -Radius .. Radius loop
         Bases_Y_Loop :
         for Y in -Radius .. Radius loop
            Temp_X := Player_Ship.Sky_X + X;
            Normalize_Coord(Coord => Temp_X);
            Temp_Y := Player_Ship.Sky_Y + Y;
            Normalize_Coord(Coord => Temp_Y, Is_X_Axis => False);
            Tmp_Base_Index := Sky_Map(Temp_X, Temp_Y).Base_Index;
            if Tmp_Base_Index > 0
              and then not Sky_Bases(Tmp_Base_Index).Known then
               Sky_Bases(Tmp_Base_Index).Known := True;
               Amount := Amount - 1;
               exit Bases_X_Loop when Amount = 0;
            end if;
         end loop Bases_Y_Loop;
      end loop Bases_X_Loop;
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
              (if Proto_Ships_List(Ship_Index).Crew.Length < 5 then 1
               elsif Proto_Ships_List(Ship_Index).Crew.Length < 10 then 2
               else 4);
         end if;
         Count_Unknown_Bases_Loop :
         for I in Sky_Bases'Range loop
            if not Sky_Bases(I).Known then
               Unknown_Bases := Unknown_Bases + 1;
            end if;
            exit Count_Unknown_Bases_Loop when Unknown_Bases >= Amount;
         end loop Count_Unknown_Bases_Loop;
         if Unknown_Bases >= Amount then
            Reveal_Random_Bases_Loop :
            loop
               Tmp_Base_Index := Get_Random(Min => 1, Max => 1_024);
               if not Sky_Bases(Tmp_Base_Index).Known then
                  Sky_Bases(Tmp_Base_Index).Known := True;
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
      Gain_Exp
        (Amount => 1, Skill_Number => Talking_Skill,
         Crew_Index => Trader_Index);
      Update_Game(Minutes => 30);
   end Ask_For_Bases;

   procedure Ask_For_Events is
      use Tiny_String;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Event_Time, Diff_X, Diff_Y: Positive;
      Event: Events_Types;
      Min_X, Min_Y, Max_X, Max_Y: Integer range -100 .. 1_124;
      Enemies: Positive_Container.Vector;
      Attempts: Natural range 0 .. 10;
      New_Item_Index: Objects_Container.Extended_Index;
      Ship_Index: Proto_Ships_Container.Extended_Index;
      Trader_Index: constant Crew_Container.Extended_Index :=
        Find_Member(Order => TALK);
      Max_Events, Events_Amount: Positive range 1 .. 15;
      Tmp_Base_Index: Bases_Range;
      Event_X, Event_Y: Positive range 1 .. 1_024;
      Item_Index: Integer;
   begin
      if Trader_Index = 0 then
         return;
      end if;
      if Base_Index > 0 then -- asking in base
         Max_Events :=
           (if Sky_Bases(Base_Index).Population < 150 then 5
            elsif Sky_Bases(Base_Index).Population < 300 then 10 else 15);
         Sky_Bases(Base_Index).Asked_For_Events := Game_Date;
         Add_Message
           (Message =>
              To_String(Source => Player_Ship.Crew(Trader_Index).Name) &
              " asked for recent events known at base '" &
              To_String(Source => Sky_Bases(Base_Index).Name) & "'.",
            M_Type => ORDERMESSAGE);
         Gain_Rep(Base_Index => Base_Index, Points => 1);
      else -- asking friendly ship
         Ship_Index :=
           Events_List
             (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
             .Ship_Index;
         Max_Events :=
           (if Proto_Ships_List(Ship_Index).Crew.Length < 5 then 1
            elsif Proto_Ships_List(Ship_Index).Crew.Length < 10 then 3 else 5);
         Add_Message
           (Message =>
              To_String(Source => Player_Ship.Crew(Trader_Index).Name) &
              " asked ship '" &
              To_String
                (Source =>
                   Generate_Ship_Name
                     (Owner => Proto_Ships_List(Ship_Index).Owner)) &
              "' for recent events.",
            M_Type => ORDERMESSAGE);
         Delete_Event
           (Event_Index =>
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index);
         Update_Orders(Ship => Player_Ship);
      end if;
      Events_Amount := Get_Random(Min => 1, Max => Max_Events);
      Min_X := Player_Ship.Sky_X - 100;
      Normalize_Coord(Coord => Min_X);
      Max_X := Player_Ship.Sky_X + 100;
      Normalize_Coord(Coord => Max_X);
      Min_Y := Player_Ship.Sky_Y - 100;
      Normalize_Coord(Coord => Min_Y, Is_X_Axis => False);
      Max_Y := Player_Ship.Sky_Y + 100;
      Normalize_Coord(Coord => Max_Y, Is_X_Axis => False);
      Generate_Enemies(Enemies => Enemies);
      Generate_Events_Loop :
      for I in 1 .. Events_Amount loop
         Event := Events_Types'Val(Get_Random(Min => 1, Max => 5));
         Attempts := 10;
         Generate_Event_Location_Loop :
         loop
            if Event = ENEMYSHIP then
               Event_X := Get_Random(Min => Min_X, Max => Max_X);
               Event_Y := Get_Random(Min => Min_Y, Max => Max_Y);
               exit Generate_Event_Location_Loop when Sky_Map(Event_X, Event_Y)
                   .Base_Index =
                 0 and
                 Event_X /= Player_Ship.Sky_X and
                 Event_Y /= Player_Ship.Sky_Y and
                 Sky_Map(Event_X, Event_Y).Event_Index = 0;
            else
               Tmp_Base_Index := Get_Random(Min => 1, Max => 1_024);
               Event_X := Sky_Bases(Tmp_Base_Index).Sky_X;
               Event_Y := Sky_Bases(Tmp_Base_Index).Sky_Y;
               Attempts := Attempts - 1;
               if Attempts = 0 then
                  Event := ENEMYSHIP;
                  Regenerate_Event_Location_Loop :
                  loop
                     Event_X := Get_Random(Min => Min_X, Max => Max_X);
                     Event_Y := Get_Random(Min => Min_Y, Max => Max_Y);
                     exit Regenerate_Event_Location_Loop when Sky_Map
                         (Event_X, Event_Y)
                         .Base_Index =
                       0 and
                       Event_X /= Player_Ship.Sky_X and
                       Event_Y /= Player_Ship.Sky_Y and
                       Sky_Map(Event_X, Event_Y).Event_Index = 0;
                  end loop Regenerate_Event_Location_Loop;
                  exit Generate_Event_Location_Loop;
               end if;
               if Event_X /= Player_Ship.Sky_X and
                 Event_Y /= Player_Ship.Sky_Y and
                 Sky_Map(Event_X, Event_Y).Event_Index = 0 and
                 Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index).Known then
                  if Event = ATTACKONBASE and
                    Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index)
                        .Population /=
                      0 then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = DOUBLEPRICE and
                    Is_Friendly
                      (Source_Faction => Player_Ship.Crew(1).Faction,
                       Target_Faction =>
                         Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index)
                           .Owner) then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = DISEASE and
                    not Factions_List
                      (Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index).Owner)
                      .Flags
                      .Contains
                      (Item =>
                         To_Unbounded_String(Source => "diseaseimmune")) and
                    Is_Friendly
                      (Source_Faction => Player_Ship.Crew(1).Faction,
                       Target_Faction =>
                         Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index)
                           .Owner) then
                     exit Generate_Event_Location_Loop;
                  end if;
                  if Event = BASERECOVERY and
                    Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index)
                        .Population =
                      0 then
                     exit Generate_Event_Location_Loop;
                  end if;
               end if;
            end if;
         end loop Generate_Event_Location_Loop;
         Diff_X := abs (Player_Ship.Sky_X - Event_X);
         Diff_Y := abs (Player_Ship.Sky_Y - Event_Y);
         Event_Time :=
           Positive(60.0 * Sqrt(X => Float((Diff_X**2) + (Diff_Y**2))));
         case Event is
            when ENEMYSHIP =>
               Events_List.Append
                 (New_Item =>
                    (E_Type => ENEMYSHIP, Sky_X => Event_X, Sky_Y => Event_Y,
                     Time =>
                       Get_Random(Min => Event_Time, Max => Event_Time + 60),
                     Ship_Index =>
                       Enemies
                         (Get_Random
                            (Min => Enemies.First_Index,
                             Max => Enemies.Last_Index))));
            when ATTACKONBASE =>
               Generate_Enemies
                 (Enemies => Enemies,
                  Owner => Tiny_String.To_Bounded_String(Source => "Any"),
                  With_Traders => False);
               Events_List.Append
                 (New_Item =>
                    (E_Type => ATTACKONBASE, Sky_X => Event_X,
                     Sky_Y => Event_Y,
                     Time =>
                       Get_Random(Min => Event_Time, Max => Event_Time + 120),
                     Ship_Index =>
                       Enemies
                         (Get_Random
                            (Min => Enemies.First_Index,
                             Max => Enemies.Last_Index))));
               Generate_Enemies(Enemies => Enemies);
            when DISEASE =>
               Events_List.Append
                 (New_Item =>
                    (E_Type => DISEASE, Sky_X => Event_X, Sky_Y => Event_Y,
                     Time => Get_Random(Min => 10_080, Max => 12_000),
                     Data => 1));
            when DOUBLEPRICE =>
               Set_Double_Price_Event_Loop :
               loop
                  Item_Index :=
                    Get_Random(Min => 1, Max => Positive(Items_List.Length));
                  Find_Item_Index_Loop :
                  for J in Items_List.Iterate loop
                     Item_Index := Item_Index - 1;
                     if Item_Index <= 0
                       and then
                         Get_Price
                           (Base_Type =>
                              Sky_Bases(Sky_Map(Event_X, Event_Y).Base_Index)
                                .Base_Type,
                            Item_Index =>
                              Objects_Container.To_Index(Position => J)) >
                         0 then
                        New_Item_Index :=
                          Objects_Container.To_Index(Position => J);
                        exit Set_Double_Price_Event_Loop;
                     end if;
                  end loop Find_Item_Index_Loop;
               end loop Set_Double_Price_Event_Loop;
               Events_List.Append
                 (New_Item =>
                    (E_Type => DOUBLEPRICE, Sky_X => Event_X, Sky_Y => Event_Y,
                     Time =>
                       Get_Random
                         (Min => (Event_Time * 3), Max => (Event_Time * 4)),
                     Item_Index => New_Item_Index));
            when BASERECOVERY =>
               Recover_Base
                 (Base_Index => Sky_Map(Event_X, Event_Y).Base_Index);
            when others =>
               null;
         end case;
         if Event /= BASERECOVERY then
            Sky_Map(Event_X, Event_Y).Event_Index := Events_List.Last_Index;
         end if;
      end loop Generate_Events_Loop;
      Gain_Exp
        (Amount => 1, Skill_Number => Talking_Skill,
         Crew_Index => Trader_Index);
      Update_Game(Minutes => 30);
   end Ask_For_Events;

   procedure Update_Population is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Population_Diff: Integer;
   begin
      if Days_Difference
          (Date_To_Compare => Sky_Bases(Base_Index).Recruit_Date) <
        30 then
         return;
      end if;
      if Sky_Bases(Base_Index).Population > 0 then
         if Get_Random(Min => 1, Max => 100) > 30 then
            return;
         end if;
         Population_Diff :=
           (if Get_Random(Min => 1, Max => 100) < 20 then
              -(Get_Random(Min => 1, Max => 10))
            else Get_Random(Min => 1, Max => 10));
         if Sky_Bases(Base_Index).Population + Population_Diff < 0 then
            Population_Diff := -(Sky_Bases(Base_Index).Population);
         end if;
         Sky_Bases(Base_Index).Population :=
           Sky_Bases(Base_Index).Population + Population_Diff;
         if Sky_Bases(Base_Index).Population = 0 then
            Sky_Bases(Base_Index).Reputation := Default_Reputation;
         end if;
      else
         if Get_Random(Min => 1, Max => 100) > 5 then
            return;
         end if;
         Sky_Bases(Base_Index).Population := Get_Random(Min => 5, Max => 10);
         Sky_Bases(Base_Index).Owner := Get_Random_Faction;
      end if;
   end Update_Population;

   procedure Update_Prices is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Roll: Positive range 1 .. 100;
      Chance: Positive;
      Item: Base_Cargo;
   begin
      if Sky_Bases(Base_Index).Population = 0 then
         return;
      end if;
      Chance :=
        (if Sky_Bases(Base_Index).Population < 150 then 1
         elsif Sky_Bases(Base_Index).Population < 300 then 2 else 5);
      Chance :=
        Chance +
        (Days_Difference(Date_To_Compare => Sky_Bases(Base_Index).Visited) /
         10);
      if Get_Random(Min => 1, Max => 100) > Chance then
         return;
      end if;
      Update_Prices_Loop :
      for I in
        BaseCargo_Container.First_Index
          (Container => Sky_Bases(Base_Index).Cargo) ..
          BaseCargo_Container.Last_Index
            (Container => Sky_Bases(Base_Index).Cargo) loop
         Item :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo, Index => I);
         Roll := Get_Random(Min => 1, Max => 100);
         if Roll < 30 and Item.Price > 1 then
            Item.Price := Item.Price - 1;
         elsif Roll < 60 and Item.Price > 0 then
            Item.Price := Item.Price + 1;
         end if;
         BaseCargo_Container.Replace_Element
           (Container => Sky_Bases(Base_Index).Cargo, Index => I,
            New_Item => Item);
      end loop Update_Prices_Loop;
   end Update_Prices;

end Bases;
