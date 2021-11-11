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
               Gain_Exp(Amount => 1, Skill_Number => Perception_Skill, Crew_Index => Crew_Container.To_Index(Position => I));
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
                   (EnemyIndex => Events_List
                      (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
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
                      (Member => Player_Ship.Crew(Crew_Index), SkillIndex => Engineering_Skill) then
                     AddMessage
                       (Message => "One of your engines is taking damage.", MType => OtherMessage,
                        Color => RED);
                     Count_Engines_Loop :
                     for I in Player_Ship.Modules.Iterate loop
                        if Player_Ship.Modules(I).M_Type = ENGINE
                          and then not Player_Ship.Modules(I).Disabled then
                           Engines.Append
                             (New_Item => Modules_Container.To_Index(I));
                        end if;
                     end loop Count_Engines_Loop;
                     declare
                        EngineIndex: constant Positive :=
                          Engines
                            (Get_Random
                               (Engines.First_Index, Engines.Last_Index));
                     begin
                        Player_Ship.Modules(EngineIndex).Durability :=
                          Player_Ship.Modules(EngineIndex).Durability - 1;
                     end;
                     UpdateOrders(Player_Ship);
                  else
                     AddMessage
                       (To_String(Player_Ship.Crew(Crew_Index).Name) &
                        " has prevented engine damage.",
                        OtherMessage, GREEN);
                  end if;
                  Gain_Exp(1, Engineering_Skill, Crew_Index);
               end if;
            when 6 .. 20 => -- Bad weather
               Crew_Index := FindMember(PILOT);
               if Crew_Index > 0 then
                  AddMessage
                    ("Sudden bad weather causes your travel to take longer.",
                     OtherMessage, RED);
                  Time_Passed :=
                    60 -
                    GetSkillLevel(Player_Ship.Crew(Crew_Index), Piloting_Skill);
                  if Time_Passed < 1 then
                     Time_Passed := 1;
                  end if;
                  Gain_Exp(1, Piloting_Skill, Crew_Index);
                  UpdateCargo
                    (Player_Ship, FindProtoItem(ItemType => Fuel_Type),
                     CountFuelNeeded);
                  Update_Game(Time_Passed);
               end if;
            when 21 .. 23 => -- Friendly trader
               Events_List.Append
                 (New_Item =>
                    (TRADER, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                     Get_Random(30, 45),
                     Traders
                       (Get_Random(Traders.First_Index, Traders.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               AddMessage("You've meet a friendly trader.", OtherMessage);
               Gain_Perception;
               UpdateOrders(Player_Ship);
            when 24 .. 30 => -- Friendly ship
               Events_List.Append
                 (New_Item =>
                    (FRIENDLYSHIP, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                     Get_Random(30, 45),
                     Friendly_Ships
                       (Get_Random
                          (Friendly_Ships.First_Index,
                           Friendly_Ships.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               AddMessage("You've spotted a friendly ship.", OtherMessage);
               Gain_Perception;
               UpdateOrders(Player_Ship);
            when others => -- Combat
               Generate_Enemies(Enemies);
               Events_List.Append
                 (New_Item =>
                    (ENEMYSHIP, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                     Get_Random(30, 45),
                     Enemies
                       (Get_Random(Enemies.First_Index, Enemies.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               return
                 StartCombat(Events_List(Events_List.Last_Index).Ship_Index);
         end case;
      else
         if Sky_Bases(Base_Index).Population = 0 then
            if Roll < 6 and
              Player_Ship.Speed /=
                DOCKED then -- Change owner of abandoned base
               Recover_Base(Base_Index);
            end if;
            return False;
         end if;
         if Player_Ship.Speed /= DOCKED then
            if Roll in 21 .. 30 and
              Sky_Bases(Base_Index).Reputation(1) = -100 then
               Roll := 31;
            end if;
            if Factions_List(Sky_Bases(Base_Index).Owner).Flags.Contains
                (To_Unbounded_String("diseaseimmune")) and
              Roll = 21 then
               Roll := 20;
            end if;
            case Roll is
               when 1 .. 20 => -- Base is attacked
                  Generate_Enemies(Enemies, To_Unbounded_String("Any"), False);
                  Events_List.Append
                    (New_Item =>
                       (ATTACKONBASE, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                        Get_Random(60, 90),
                        Enemies
                          (Get_Random
                             (Enemies.First_Index, Enemies.Last_Index))));
                  AddMessage
                    ("You can't dock to base now, because base is under attack. You can help defend it.",
                     OtherMessage);
                  return
                    StartCombat
                      (Events_List(Events_List.Last_Index).Ship_Index);
               when 21 => -- Disease in base
                  Events_List.Append
                    (New_Item =>
                       (DISEASE, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                        Get_Random(10_080, 12_000), 1));
                  AddMessage
                    ("You can't dock to base now, it is closed due to disease.",
                     OtherMessage);
               when 22 .. 30 => -- Double price for item in base
                  declare
                     ItemIndex: Natural;
                     NewItemIndex: Unbounded_String;
                  begin
                     Get_Price_Loop :
                     loop
                        ItemIndex :=
                          Get_Random(1, Positive(Items_List.Length));
                        Find_Item_Index_Loop :
                        for J in Items_List.Iterate loop
                           ItemIndex := ItemIndex - 1;
                           if ItemIndex = 0 then
                              if Get_Price
                                  (Sky_Bases
                                     (SkyMap
                                        (Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                        .BaseIndex)
                                     .Base_Type,
                                   Objects_Container.Key(J)) >
                                0 then
                                 NewItemIndex := Objects_Container.Key(J);
                              end if;
                              exit Find_Item_Index_Loop;
                           end if;
                        end loop Find_Item_Index_Loop;
                        exit Get_Price_Loop when Get_Price
                            (Sky_Bases
                               (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                  .BaseIndex)
                               .Base_Type,
                             NewItemIndex) >
                          0;
                     end loop Get_Price_Loop;
                     Events_List.Append
                       (New_Item =>
                          (DOUBLEPRICE, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                           Get_Random(1_440, 2_880), NewItemIndex));
                  end;
               when others => -- Full docks or enemy patrol
                  if Roll in 20 .. 40 and
                    not IsFriendly
                      (Player_Ship.Crew(1).Faction,
                       Sky_Bases(Base_Index).Owner) then
                     Generate_Enemies
                       (Enemies, Sky_Bases(Base_Index).Owner, False);
                     Events_List.Append
                       (New_Item =>
                          (ENEMYPATROL, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                           Get_Random(30, 45),
                           Enemies
                             (Get_Random
                                (Enemies.First_Index, Enemies.Last_Index))));
                     SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                       Events_List.Last_Index;
                     return
                       StartCombat
                         (Events_List(Events_List.Last_Index).Ship_Index);
                  end if;
                  Events_List.Append
                    (New_Item =>
                       (FULLDOCKS, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                        Get_Random(15, 30), 1));
                  AddMessage
                    ("You can't dock to base now, because it's docks are full.",
                     OtherMessage, RED);
            end case;
            SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
              Events_List.Last_Index;
         else
            if Roll < 5 and
              Player_Ship.Crew.Last_Index > 1 then -- Brawl in base
               declare
                  RestingCrew: Positive_Container.Vector;
                  Injuries: Positive;
               begin
                  Find_Resting_Crew_Loop :
                  for I in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(I).Order = REST then
                        RestingCrew.Append
                          (New_Item => Crew_Container.To_Index(I));
                     end if;
                  end loop Find_Resting_Crew_Loop;
                  if RestingCrew.Length > 0 then
                     Roll2 :=
                       Get_Random
                         (RestingCrew.First_Index, RestingCrew.Last_Index);
                     Injuries := Get_Random(1, 10);
                     if Injuries >
                       Player_Ship.Crew(RestingCrew(Roll2)).Health then
                        Injuries :=
                          Player_Ship.Crew(RestingCrew(Roll2)).Health;
                     end if;
                     Player_Ship.Crew(RestingCrew(Roll2)).Health :=
                       Player_Ship.Crew(RestingCrew(Roll2)).Health - Injuries;
                     AddMessage
                       (To_String(Player_Ship.Crew(RestingCrew(Roll2)).Name) &
                        " was injured in a brawl inside the base.",
                        OtherMessage, RED);
                     if Player_Ship.Crew(RestingCrew(Roll2)).Health = 0 then
                        Death
                          (RestingCrew(Roll2),
                           To_Unbounded_String("injuries in brawl in base"),
                           Player_Ship);
                     end if;
                  end if;
               end;
            elsif Roll > 4 and Roll < 10 then -- Lost cargo in base
               Roll2 := Get_Random(1, Player_Ship.Cargo.Last_Index);
               declare
                  LostCargo: Positive range 1 .. 10 := Get_Random(1, 10);
               begin
                  if LostCargo > Player_Ship.Cargo(Roll2).Amount then
                     LostCargo := Player_Ship.Cargo(Roll2).Amount;
                  end if;
                  AddMessage
                    ("During checking ship's cargo, you noticed that you lost" &
                     Positive'Image(LostCargo) & " " &
                     GetItemName(Player_Ship.Cargo(Roll2)) & ".",
                     OtherMessage, RED);
                  UpdateCargo
                    (Ship => Player_Ship, Amount => (0 - LostCargo),
                     CargoIndex => Roll2);
               end;
            end if;
         end if;
      end if;
      return False;
   end Check_For_Event;

   procedure Update_Events(Minutes: Positive) is
      CurrentIndex: Events_Container.Extended_Index := Events_List.First_Index;
      NewTime: Integer;
      EventsAmount: constant Natural := Natural(Events_List.Length);
      PopulationLost: Positive range 1 .. 10;
      BaseIndex: Bases_Range;
   begin
      if EventsAmount = 0 then
         return;
      end if;
      Update_Events_Loop :
      while CurrentIndex <= Events_List.Last_Index loop
         NewTime := Events_List(CurrentIndex).Time - Minutes;
         if NewTime < 1 then
            if Events_List(CurrentIndex).E_Type in DISEASE | ATTACKONBASE and
              Get_Random(1, 100) < 10 then
               BaseIndex :=
                 SkyMap
                   (Events_List(CurrentIndex).Sky_X,
                    Events_List(CurrentIndex).Sky_Y)
                   .BaseIndex;
               PopulationLost := Get_Random(1, 10);
               if PopulationLost > Sky_Bases(BaseIndex).Population then
                  PopulationLost := Sky_Bases(BaseIndex).Population;
                  Sky_Bases(BaseIndex).Reputation := (0, 0);
               end if;
               Sky_Bases(BaseIndex).Population :=
                 Sky_Bases(BaseIndex).Population - PopulationLost;
            end if;
            SkyMap
              (Events_List(CurrentIndex).Sky_X,
               Events_List(CurrentIndex).Sky_Y)
              .EventIndex :=
              0;
            Events_List.Delete(Index => CurrentIndex);
         else
            Events_List(CurrentIndex).Time := NewTime;
            CurrentIndex := CurrentIndex + 1;
         end if;
      end loop Update_Events_Loop;
      if EventsAmount > Natural(Events_List.Length) then
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

   -- ****if* Events/Events.GetPlayer_Ships
   -- FUNCTION
   -- Get the list of all prototypes ships which are only for the player
   -- PARAMETERS
   -- Player_Ships - The list with all available indexes of prototype player's
   --               ships
   -- RESULT
   -- Parameter Player_Ships
   -- SOURCE
   procedure GetPlayer_Ships
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
   end GetPlayer_Ships;

   procedure Generate_Traders is
      Player_Ships: UnboundedString_Container.Vector;
   begin
      Count_Traders_Loop :
      for I in Proto_Ships_List.Iterate loop
         if Index(Proto_Ships_List(I).Name, To_String(Traders_Name)) > 0 then
            Traders.Append(New_Item => Proto_Ships_Container.Key(I));
         end if;
      end loop Count_Traders_Loop;
      GetPlayer_Ships(Player_Ships);
      Count_Friendly_Loop :
      for I in Proto_Ships_List.Iterate loop
         if IsFriendly
             (Player_Ship.Crew(1).Faction, Proto_Ships_List(I).Owner) and
           not Player_Ships.Contains(Proto_Ships_Container.Key(I)) then
            Friendly_Ships.Append(New_Item => Proto_Ships_Container.Key(I));
         end if;
      end loop Count_Friendly_Loop;
   end Generate_Traders;

   procedure Recover_Base(Base_Index: Bases_Range) is
      MaxSpawnChance: Natural := 0;
      FactionRoll: Positive;
   begin
      Count_Spawn_Chance_Loop :
      for Faction of Factions_List loop
         MaxSpawnChance := MaxSpawnChance + Faction.SpawnChance;
      end loop Count_Spawn_Chance_Loop;
      FactionRoll := Get_Random(1, MaxSpawnChance);
      Choose_Faction_Loop :
      for I in Factions_List.Iterate loop
         if FactionRoll > Factions_List(I).SpawnChance then
            FactionRoll := FactionRoll - Factions_List(I).SpawnChance;
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
      GetPlayer_Ships(Player_Ships);
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
