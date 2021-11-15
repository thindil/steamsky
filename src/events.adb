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

with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Combat; use Combat;
with Crew; use Crew;
with Factions; use Factions;
with Items; use Items;
with Maps; use Maps;
with Messages; use Messages;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Utils; use Utils;

package body Events is

   function Check_For_Event return Boolean is
      Time_Passed: Integer;
      Crew_Index: Crew_Container.Extended_Index := 0;
      Roll: Positive range 1 .. 100;
      Roll2: Integer range -20 .. 120;
      Engines: Positive_Container.Vector;
      Base_Index: constant Extended_Base_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Enemies: UnboundedString_Container.Vector;
      procedure Gain_Perception is
      begin
         Gain_Perception_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Order in PILOT | GUNNER then
               Gain_Exp
                 (Amount => 1, Skill_Number => Perception_Skill,
                  Crew_Index => Crew_Container.To_Index(Position => I));
            end if;
         end loop Gain_Perception_Loop;
      end Gain_Perception;
   begin
      if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex > 0 then
         case Events_List
           (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
           .E_Type is
            when ENEMYSHIP =>
               return
                 StartCombat
                   (EnemyIndex =>
                      Events_List
                        (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                           .EventIndex)
                        .Ship_Index);
            when others =>
               return False;
         end case;
      end if;
      -- No event
      if Get_Random(Min => 1, Max => 100) > 6 then
         return False;
      end if;
      Roll := Get_Random(Min => 1, Max => 100);
      if Base_Index = 0 then -- Outside bases
         case Roll is
            when 1 .. 5 => -- Engine damaged
               Crew_Index := FindMember(Order => ENGINEER);
               if Crew_Index > 0 and Player_Ship.Speed /= FULL_STOP then
                  Roll2 := Get_Random(Min => 1, Max => 100);
                  case Player_Ship.Speed is
                     when QUARTER_SPEED =>
                        Roll2 := (if Roll2 < 21 then 1 else Roll2 - 20);
                     when FULL_SPEED =>
                        Roll2 := Roll2 + 20;
                     when others =>
                        null;
                  end case;
                  if Roll2 >
                    GetSkillLevel
                      (Member => Player_Ship.Crew(Crew_Index),
                       SkillIndex => Engineering_Skill) then
                     AddMessage
                       (Message => "One of your engines is taking damage.",
                        MType => OtherMessage, Color => RED);
                     Count_Engines_Loop :
                     for I in Player_Ship.Modules.Iterate loop
                        if Player_Ship.Modules(I).M_Type = ENGINE
                          and then not Player_Ship.Modules(I).Disabled then
                           Engines.Append
                             (New_Item =>
                                Modules_Container.To_Index(Position => I));
                        end if;
                     end loop Count_Engines_Loop;
                     Reduce_Engine_Durability_Block :
                     declare
                        Engine_Index: constant Positive :=
                          Engines
                            (Get_Random
                               (Min => Engines.First_Index,
                                Max => Engines.Last_Index));
                     begin
                        Player_Ship.Modules(Engine_Index).Durability :=
                          Player_Ship.Modules(Engine_Index).Durability - 1;
                     end Reduce_Engine_Durability_Block;
                     UpdateOrders(Ship => Player_Ship);
                  else
                     AddMessage
                       (Message =>
                          To_String
                            (Source => Player_Ship.Crew(Crew_Index).Name) &
                          " has prevented engine damage.",
                        MType => OtherMessage, Color => GREEN);
                  end if;
                  Gain_Exp
                    (Amount => 1, Skill_Number => Engineering_Skill,
                     Crew_Index => Crew_Index);
               end if;
            when 6 .. 20 => -- Bad weather
               Crew_Index := FindMember(Order => PILOT);
               if Crew_Index > 0 then
                  AddMessage
                    (Message =>
                       "Sudden bad weather causes your travel to take longer.",
                     MType => OtherMessage, Color => RED);
                  Time_Passed :=
                    60 -
                    GetSkillLevel
                      (Member => Player_Ship.Crew(Crew_Index),
                       SkillIndex => Piloting_Skill);
                  if Time_Passed < 1 then
                     Time_Passed := 1;
                  end if;
                  Gain_Exp
                    (Amount => 1, Skill_Number => Piloting_Skill,
                     Crew_Index => Crew_Index);
                  UpdateCargo
                    (Ship => Player_Ship,
                     ProtoIndex => FindProtoItem(ItemType => Fuel_Type),
                     Amount => CountFuelNeeded);
                  Update_Game(Minutes => Time_Passed);
               end if;
            when 21 .. 23 => -- Friendly trader
               Events_List.Append
                 (New_Item =>
                    (E_Type => TRADER, Sky_X => Player_Ship.Sky_X,
                     Sky_Y => Player_Ship.Sky_Y,
                     Time => Get_Random(Min => 30, Max => 45),
                     Ship_Index =>
                       Traders
                         (Get_Random
                            (Min => Traders.First_Index,
                             Max => Traders.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               AddMessage
                 (Message => "You've meet a friendly trader.",
                  MType => OtherMessage);
               Gain_Perception;
               UpdateOrders(Ship => Player_Ship);
            when 24 .. 30 => -- Friendly ship
               Events_List.Append
                 (New_Item =>
                    (E_Type => FRIENDLYSHIP, Sky_X => Player_Ship.Sky_X,
                     Sky_Y => Player_Ship.Sky_Y,
                     Time => Get_Random(Min => 30, Max => 45),
                     Ship_Index =>
                       Friendly_Ships
                         (Get_Random
                            (Min => Friendly_Ships.First_Index,
                             Max => Friendly_Ships.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               AddMessage
                 (Message => "You've spotted a friendly ship.",
                  MType => OtherMessage);
               Gain_Perception;
               UpdateOrders(Ship => Player_Ship);
            when others => -- Combat
               Generate_Enemies(Enemies => Enemies);
               Events_List.Append
                 (New_Item =>
                    (E_Type => ENEMYSHIP, Sky_X => Player_Ship.Sky_X,
                     Sky_Y => Player_Ship.Sky_Y,
                     Time => Get_Random(Min => 30, Max => 45),
                     Ship_Index =>
                       Enemies
                         (Get_Random
                            (Min => Enemies.First_Index,
                             Max => Enemies.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               return
                 StartCombat
                   (EnemyIndex =>
                      Events_List(Events_List.Last_Index).Ship_Index);
         end case;
      else
         if Sky_Bases(Base_Index).Population = 0 then
            if Roll < 6 and
              Player_Ship.Speed /=
                DOCKED then -- Change owner of abandoned base
               Recover_Base(Base_Index => Base_Index);
            end if;
            return False;
         end if;
         if Player_Ship.Speed /= DOCKED then
            if Roll in 21 .. 30 and
              Sky_Bases(Base_Index).Reputation(1) = -100 then
               Roll := 31;
            end if;
            if Factions_List(Sky_Bases(Base_Index).Owner).Flags.Contains
                (Item => To_Unbounded_String(Source => "diseaseimmune")) and
              Roll = 21 then
               Roll := 20;
            end if;
            case Roll is
               when 1 .. 20 => -- Base is attacked
                  Generate_Enemies
                    (Enemies => Enemies,
                     Owner => To_Unbounded_String(Source => "Any"),
                     With_Traders => False);
                  Events_List.Append
                    (New_Item =>
                       (E_Type => ATTACKONBASE, Sky_X => Player_Ship.Sky_X,
                        Sky_Y => Player_Ship.Sky_Y,
                        Time => Get_Random(Min => 60, Max => 90),
                        Ship_Index =>
                          Enemies
                            (Get_Random
                               (Min => Enemies.First_Index,
                                Max => Enemies.Last_Index))));
                  AddMessage
                    (Message =>
                       "You can't dock to base now, because base is under attack. You can help defend it.",
                     MType => OtherMessage);
                  return
                    StartCombat
                      (EnemyIndex =>
                         Events_List(Events_List.Last_Index).Ship_Index);
               when 21 => -- Disease in base
                  Events_List.Append
                    (New_Item =>
                       (E_Type => DISEASE, Sky_X => Player_Ship.Sky_X,
                        Sky_Y => Player_Ship.Sky_Y,
                        Time => Get_Random(Min => 10_080, Max => 12_000),
                        Data => 1));
                  AddMessage
                    (Message =>
                       "You can't dock to base now, it is closed due to disease.",
                     MType => OtherMessage);
               when 22 .. 30 => -- Double price for item in base
                  Set_Double_Price_Event_Block :
                  declare
                     Item_Index: Natural;
                     New_Item_Index: Unbounded_String;
                  begin
                     Get_Price_Loop :
                     loop
                        Item_Index :=
                          Get_Random
                            (Min => 1, Max => Positive(Items_List.Length));
                        Find_Item_Index_Loop :
                        for J in Items_List.Iterate loop
                           Item_Index := Item_Index - 1;
                           if Item_Index = 0 then
                              if Get_Price
                                  (Base_Type =>
                                     Sky_Bases
                                       (SkyMap
                                          (Player_Ship.Sky_X,
                                           Player_Ship.Sky_Y)
                                          .BaseIndex)
                                       .Base_Type,
                                   Item_Index =>
                                     Objects_Container.Key(Position => J)) >
                                0 then
                                 New_Item_Index :=
                                   Objects_Container.Key(Position => J);
                              end if;
                              exit Find_Item_Index_Loop;
                           end if;
                        end loop Find_Item_Index_Loop;
                        exit Get_Price_Loop when Get_Price
                            (Base_Type =>
                               Sky_Bases
                                 (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                    .BaseIndex)
                                 .Base_Type,
                             Item_Index => New_Item_Index) >
                          0;
                     end loop Get_Price_Loop;
                     Events_List.Append
                       (New_Item =>
                          (E_Type => DOUBLEPRICE, Sky_X => Player_Ship.Sky_X,
                           Sky_Y => Player_Ship.Sky_Y,
                           Time => Get_Random(Min => 1_440, Max => 2_880),
                           Item_Index => New_Item_Index));
                  end Set_Double_Price_Event_Block;
               when others => -- Full docks or enemy patrol
                  if Roll in 20 .. 40 and
                    not IsFriendly
                      (SourceFaction => Player_Ship.Crew(1).Faction,
                       TargetFaction => Sky_Bases(Base_Index).Owner) then
                     Generate_Enemies
                       (Enemies => Enemies,
                        Owner => Sky_Bases(Base_Index).Owner,
                        With_Traders => False);
                     Events_List.Append
                       (New_Item =>
                          (E_Type => ENEMYPATROL, Sky_X => Player_Ship.Sky_X,
                           Sky_Y => Player_Ship.Sky_Y,
                           Time => Get_Random(Min => 30, Max => 45),
                           Ship_Index =>
                             Enemies
                               (Get_Random
                                  (Min => Enemies.First_Index,
                                   Max => Enemies.Last_Index))));
                     SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                       Events_List.Last_Index;
                     return
                       StartCombat
                         (EnemyIndex =>
                            Events_List(Events_List.Last_Index).Ship_Index);
                  end if;
                  Events_List.Append
                    (New_Item =>
                       (E_Type => FULLDOCKS, Sky_X => Player_Ship.Sky_X,
                        Sky_Y => Player_Ship.Sky_Y,
                        Time => Get_Random(Min => 15, Max => 30), Data => 1));
                  AddMessage
                    (Message =>
                       "You can't dock to base now, because it's docks are full.",
                     MType => OtherMessage, Color => RED);
            end case;
            SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
              Events_List.Last_Index;
         else
            if Roll < 5 and
              Player_Ship.Crew.Last_Index > 1 then -- Brawl in base
               Count_Injuries_Block :
               declare
                  Resting_Crew: Positive_Container.Vector;
                  Injuries: Positive;
               begin
                  Find_Resting_Crew_Loop :
                  for I in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(I).Order = REST then
                        Resting_Crew.Append
                          (New_Item => Crew_Container.To_Index(Position => I));
                     end if;
                  end loop Find_Resting_Crew_Loop;
                  if Resting_Crew.Length > 0 then
                     Roll2 :=
                       Get_Random
                         (Min => Resting_Crew.First_Index,
                          Max => Resting_Crew.Last_Index);
                     Injuries := Get_Random(Min => 1, Max => 10);
                     if Injuries >
                       Player_Ship.Crew(Resting_Crew(Roll2)).Health then
                        Injuries :=
                          Player_Ship.Crew(Resting_Crew(Roll2)).Health;
                     end if;
                     Player_Ship.Crew(Resting_Crew(Roll2)).Health :=
                       Player_Ship.Crew(Resting_Crew(Roll2)).Health - Injuries;
                     AddMessage
                       (Message =>
                          To_String
                            (Source =>
                               Player_Ship.Crew(Resting_Crew(Roll2)).Name) &
                          " was injured in a brawl inside the base.",
                        MType => OtherMessage, Color => RED);
                     if Player_Ship.Crew(Resting_Crew(Roll2)).Health = 0 then
                        Death
                          (MemberIndex => Resting_Crew(Roll2),
                           Reason =>
                             To_Unbounded_String
                               (Source => "injuries in brawl in base"),
                           Ship => Player_Ship);
                     end if;
                  end if;
               end Count_Injuries_Block;
            elsif Roll > 4 and Roll < 10 then -- Lost cargo in base
               Roll2 :=
                 Get_Random(Min => 1, Max => Player_Ship.Cargo.Last_Index);
               Count_Lost_Cargo_Block :
               declare
                  Lost_Cargo: Positive range 1 .. 10 :=
                    Get_Random(Min => 1, Max => 10);
               begin
                  if Lost_Cargo > Player_Ship.Cargo(Roll2).Amount then
                     Lost_Cargo := Player_Ship.Cargo(Roll2).Amount;
                  end if;
                  AddMessage
                    (Message =>
                       "During checking ship's cargo, you noticed that you lost" &
                       Positive'Image(Lost_Cargo) & " " &
                       GetItemName(Item => Player_Ship.Cargo(Roll2)) & ".",
                     MType => OtherMessage, Color => RED);
                  UpdateCargo
                    (Ship => Player_Ship, Amount => (0 - Lost_Cargo),
                     CargoIndex => Roll2);
               end Count_Lost_Cargo_Block;
            end if;
         end if;
      end if;
      return False;
   end Check_For_Event;

   procedure Update_Events(Minutes: Positive) is
      Current_Index: Events_Container.Extended_Index :=
        Events_List.First_Index;
      New_Time: Integer;
      Events_Amount: constant Natural := Natural(Events_List.Length);
      Population_Lost: Positive range 1 .. 10;
      Base_Index: Bases_Range;
   begin
      if Events_Amount = 0 then
         return;
      end if;
      Update_Events_Loop :
      while Current_Index <= Events_List.Last_Index loop
         New_Time := Events_List(Current_Index).Time - Minutes;
         if New_Time < 1 then
            if Events_List(Current_Index).E_Type in DISEASE | ATTACKONBASE and
              Get_Random(Min => 1, Max => 100) < 10 then
               Base_Index :=
                 SkyMap
                   (Events_List(Current_Index).Sky_X,
                    Events_List(Current_Index).Sky_Y)
                   .BaseIndex;
               Population_Lost := Get_Random(Min => 1, Max => 10);
               if Population_Lost > Sky_Bases(Base_Index).Population then
                  Population_Lost := Sky_Bases(Base_Index).Population;
                  Sky_Bases(Base_Index).Reputation := (1 => 0, 2 => 0);
               end if;
               Sky_Bases(Base_Index).Population :=
                 Sky_Bases(Base_Index).Population - Population_Lost;
            end if;
            SkyMap
              (Events_List(Current_Index).Sky_X,
               Events_List(Current_Index).Sky_Y)
              .EventIndex :=
              0;
            Events_List.Delete(Index => Current_Index);
         else
            Events_List(Current_Index).Time := New_Time;
            Current_Index := Current_Index + 1;
         end if;
      end loop Update_Events_Loop;
      if Events_Amount > Natural(Events_List.Length) then
         Update_Map_Loop :
         for I in Events_List.First_Index .. Events_List.Last_Index loop
            SkyMap(Events_List(I).Sky_X, Events_List(I).Sky_Y).EventIndex := I;
         end loop Update_Map_Loop;
      end if;
   end Update_Events;

   procedure Delete_Event(Event_Index: Positive) is
   begin
      SkyMap(Events_List(Event_Index).Sky_X, Events_List(Event_Index).Sky_Y)
        .EventIndex :=
        0;
      Events_List.Delete(Index => Event_Index);
      Delete_Events_Loop :
      for I in Events_List.First_Index .. Events_List.Last_Index loop
         SkyMap(Events_List(I).Sky_X, Events_List(I).Sky_Y).EventIndex := I;
      end loop Delete_Events_Loop;
   end Delete_Event;

   -- ****if* Events/Events.Get_Player_Ships
   -- FUNCTION
   -- Get the list of all prototypes ships which are only for the player
   -- PARAMETERS
   -- Player_Ships - The list with all available indexes of prototype player's
   --               ships
   -- RESULT
   -- Parameter Player_Ships
   -- SOURCE
   procedure Get_Player_Ships
     (Player_Ships: in out UnboundedString_Container.Vector) is
   -- ****
   begin
      Get_Faction_Loop :
      for Faction of Factions_List loop
         Get_Career_Loop :
         for Career of Faction.Careers loop
            Player_Ships.Append(New_Item => Career.ShipIndex);
         end loop Get_Career_Loop;
      end loop Get_Faction_Loop;
   end Get_Player_Ships;

   procedure Generate_Traders is
      Player_Ships: UnboundedString_Container.Vector;
   begin
      Count_Traders_Loop :
      for I in Proto_Ships_List.Iterate loop
         if Index
             (Source => Proto_Ships_List(I).Name,
              Pattern => To_String(Source => Traders_Name)) >
           0 then
            Traders.Append
              (New_Item => Proto_Ships_Container.Key(Position => I));
         end if;
      end loop Count_Traders_Loop;
      Get_Player_Ships(Player_Ships => Player_Ships);
      Count_Friendly_Loop :
      for I in Proto_Ships_List.Iterate loop
         if IsFriendly
             (SourceFaction => Player_Ship.Crew(1).Faction,
              TargetFaction => Proto_Ships_List(I).Owner) and
           not Player_Ships.Contains
             (Item => Proto_Ships_Container.Key(Position => I)) then
            Friendly_Ships.Append
              (New_Item => Proto_Ships_Container.Key(Position => I));
         end if;
      end loop Count_Friendly_Loop;
   end Generate_Traders;

   procedure Recover_Base(Base_Index: Bases_Range) is
      Max_Spawn_Chance: Natural := 0;
      Faction_Roll: Positive;
   begin
      Count_Spawn_Chance_Loop :
      for Faction of Factions_List loop
         Max_Spawn_Chance := Max_Spawn_Chance + Faction.SpawnChance;
      end loop Count_Spawn_Chance_Loop;
      Faction_Roll := Get_Random(Min => 1, Max => Max_Spawn_Chance);
      Choose_Faction_Loop :
      for I in Factions_List.Iterate loop
         if Faction_Roll > Factions_List(I).SpawnChance then
            Faction_Roll := Faction_Roll - Factions_List(I).SpawnChance;
         else
            Sky_Bases(Base_Index).Owner := Factions_Container.Key(I);
            Sky_Bases(Base_Index).Reputation(1) :=
              GetReputation
                (Player_Ship.Crew(1).Faction, Sky_Bases(Base_Index).Owner);
            exit Choose_Faction_Loop;
         end if;
      end loop Choose_Faction_Loop;
      Sky_Bases(Base_Index).Population := Get_Random(2, 50);
      Sky_Bases(Base_Index).Visited := (others => 0);
      Sky_Bases(Base_Index).Recruit_Date := (others => 0);
      Sky_Bases(Base_Index).Missions_Date := (others => 0);
      AddMessage
        ("Base " & To_String(Sky_Bases(Base_Index).Name) & " has a new owner.",
         OtherMessage, CYAN);
   end Recover_Base;

   procedure Generate_Enemies
     (Enemies: in out UnboundedString_Container.Vector;
      Owner: Unbounded_String := To_Unbounded_String(Source => "Any");
      With_Traders: Boolean := True) is
      PlayerValue: Natural := 0;
      Player_Ships: UnboundedString_Container.Vector;
   begin
      PlayerValue := Count_Combat_Value;
      if Get_Random(1, 100) > 98 then
         PlayerValue := PlayerValue * 2;
      end if;
      Get_Player_Ships(Player_Ships);
      Generate_Enemies_Loop :
      for I in Proto_Ships_List.Iterate loop
         if Proto_Ships_List(I).Combat_Value <= PlayerValue and
           (Owner = To_Unbounded_String("Any") or
            Proto_Ships_List(I).Owner = Owner) and
           not IsFriendly
             (Player_Ship.Crew(1).Faction, Proto_Ships_List(I).Owner) and
           not Player_Ships.Contains(Proto_Ships_Container.Key(I)) and
           (With_Traders or
            Index(Proto_Ships_List(I).Name, To_String(Traders_Name)) = 0) then
            Enemies.Append(New_Item => Proto_Ships_Container.Key(I));
         end if;
      end loop Generate_Enemies_Loop;
   end Generate_Enemies;

end Events;
