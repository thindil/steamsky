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

   function CheckForEvent return Boolean is
      TimePassed: Integer;
      CrewIndex: Crew_Container.Extended_Index := 0;
      Roll: Positive range 1 .. 100;
      Roll2: Integer range -20 .. 120;
      Engines: Positive_Container.Vector;
      BaseIndex: constant Extended_Base_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Enemies: UnboundedString_Container.Vector;
      procedure GainPerception is
      begin
         Gain_Perception_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Order in Pilot | Gunner then
               GainExp(1, Perception_Skill, Crew_Container.To_Index(I));
            end if;
         end loop Gain_Perception_Loop;
      end GainPerception;
   begin
      if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex > 0 then
         case Events_List
           (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
           .EType is
            when EnemyShip =>
               return
                 StartCombat
                   (Events_List
                      (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
                      .ShipIndex);
            when others =>
               return False;
         end case;
      end if;
      -- No event
      if GetRandom(1, 100) > 6 then
         return False;
      end if;
      Roll := GetRandom(1, 100);
      if BaseIndex = 0 then -- Outside bases
         case Roll is
            when 1 .. 5 => -- Engine damaged
               CrewIndex := FindMember(Engineer);
               if CrewIndex > 0 and Player_Ship.Speed /= FULL_STOP then
                  Roll2 := GetRandom(1, 100);
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
                      (Player_Ship.Crew(CrewIndex), Engineering_Skill) then
                     AddMessage
                       ("One of your engines is taking damage.", OtherMessage,
                        RED);
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
                            (GetRandom
                               (Engines.First_Index, Engines.Last_Index));
                     begin
                        Player_Ship.Modules(EngineIndex).Durability :=
                          Player_Ship.Modules(EngineIndex).Durability - 1;
                     end;
                     UpdateOrders(Player_Ship);
                  else
                     AddMessage
                       (To_String(Player_Ship.Crew(CrewIndex).Name) &
                        " has prevented engine damage.",
                        OtherMessage, GREEN);
                  end if;
                  GainExp(1, Engineering_Skill, CrewIndex);
               end if;
            when 6 .. 20 => -- Bad weather
               CrewIndex := FindMember(Pilot);
               if CrewIndex > 0 then
                  AddMessage
                    ("Sudden bad weather causes your travel to take longer.",
                     OtherMessage, RED);
                  TimePassed :=
                    60 -
                    GetSkillLevel(Player_Ship.Crew(CrewIndex), Piloting_Skill);
                  if TimePassed < 1 then
                     TimePassed := 1;
                  end if;
                  GainExp(1, Piloting_Skill, CrewIndex);
                  UpdateCargo
                    (Player_Ship, FindProtoItem(ItemType => Fuel_Type),
                     CountFuelNeeded);
                  Update_Game(TimePassed);
               end if;
            when 21 .. 23 => -- Friendly trader
               Events_List.Append
                 (New_Item =>
                    (Trader, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                     GetRandom(30, 45),
                     Traders
                       (GetRandom(Traders.First_Index, Traders.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               AddMessage("You've meet a friendly trader.", OtherMessage);
               GainPerception;
               UpdateOrders(Player_Ship);
            when 24 .. 30 => -- Friendly ship
               Events_List.Append
                 (New_Item =>
                    (FriendlyShip, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                     GetRandom(30, 45),
                     FriendlyShips
                       (GetRandom
                          (FriendlyShips.First_Index,
                           FriendlyShips.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               AddMessage("You've spotted a friendly ship.", OtherMessage);
               GainPerception;
               UpdateOrders(Player_Ship);
            when others => -- Combat
               GenerateEnemies(Enemies);
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                     GetRandom(30, 45),
                     Enemies
                       (GetRandom(Enemies.First_Index, Enemies.Last_Index))));
               SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                 Events_List.Last_Index;
               return
                 StartCombat(Events_List(Events_List.Last_Index).ShipIndex);
         end case;
      else
         if SkyBases(BaseIndex).Population = 0 then
            if Roll < 6 and
              Player_Ship.Speed /= DOCKED then -- Change owner of abandoned base
               RecoverBase(BaseIndex);
            end if;
            return False;
         end if;
         if Player_Ship.Speed /= DOCKED then
            if Roll in 21 .. 30 and
              SkyBases(BaseIndex).Reputation(1) = -100 then
               Roll := 31;
            end if;
            if Factions_List(SkyBases(BaseIndex).Owner).Flags.Contains
                (To_Unbounded_String("diseaseimmune")) and
              Roll = 21 then
               Roll := 20;
            end if;
            case Roll is
               when 1 .. 20 => -- Base is attacked
                  GenerateEnemies(Enemies, To_Unbounded_String("Any"), False);
                  Events_List.Append
                    (New_Item =>
                       (AttackOnBase, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                        GetRandom(60, 90),
                        Enemies
                          (GetRandom
                             (Enemies.First_Index, Enemies.Last_Index))));
                  AddMessage
                    ("You can't dock to base now, because base is under attack. You can help defend it.",
                     OtherMessage);
                  return
                    StartCombat(Events_List(Events_List.Last_Index).ShipIndex);
               when 21 => -- Disease in base
                  Events_List.Append
                    (New_Item =>
                       (Disease, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                        GetRandom(10_080, 12_000), 1));
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
                        ItemIndex := GetRandom(1, Positive(Items_List.Length));
                        Find_Item_Index_Loop :
                        for J in Items_List.Iterate loop
                           ItemIndex := ItemIndex - 1;
                           if ItemIndex = 0 then
                              if Get_Price
                                  (SkyBases
                                     (SkyMap
                                        (Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                        .BaseIndex)
                                     .BaseType,
                                   Objects_Container.Key(J)) >
                                0 then
                                 NewItemIndex := Objects_Container.Key(J);
                              end if;
                              exit Find_Item_Index_Loop;
                           end if;
                        end loop Find_Item_Index_Loop;
                        exit Get_Price_Loop when Get_Price
                            (SkyBases
                               (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                  .BaseIndex)
                               .BaseType,
                             NewItemIndex) >
                          0;
                     end loop Get_Price_Loop;
                     Events_List.Append
                       (New_Item =>
                          (DoublePrice, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                           GetRandom(1_440, 2_880), NewItemIndex));
                  end;
               when others => -- Full docks or enemy patrol
                  if Roll in 20 .. 40 and
                    not IsFriendly
                      (Player_Ship.Crew(1).Faction,
                       SkyBases(BaseIndex).Owner) then
                     GenerateEnemies
                       (Enemies, SkyBases(BaseIndex).Owner, False);
                     Events_List.Append
                       (New_Item =>
                          (EnemyPatrol, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                           GetRandom(30, 45),
                           Enemies
                             (GetRandom
                                (Enemies.First_Index, Enemies.Last_Index))));
                     SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex :=
                       Events_List.Last_Index;
                     return
                       StartCombat
                         (Events_List(Events_List.Last_Index).ShipIndex);
                  end if;
                  Events_List.Append
                    (New_Item =>
                       (FullDocks, Player_Ship.Sky_X, Player_Ship.Sky_Y,
                        GetRandom(15, 30), 1));
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
                     if Player_Ship.Crew(I).Order = Rest then
                        RestingCrew.Append
                          (New_Item => Crew_Container.To_Index(I));
                     end if;
                  end loop Find_Resting_Crew_Loop;
                  if RestingCrew.Length > 0 then
                     Roll2 :=
                       GetRandom
                         (RestingCrew.First_Index, RestingCrew.Last_Index);
                     Injuries := GetRandom(1, 10);
                     if Injuries >
                       Player_Ship.Crew(RestingCrew(Roll2)).Health then
                        Injuries := Player_Ship.Crew(RestingCrew(Roll2)).Health;
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
               Roll2 := GetRandom(1, Player_Ship.Cargo.Last_Index);
               declare
                  LostCargo: Positive range 1 .. 10 := GetRandom(1, 10);
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
   end CheckForEvent;

   procedure UpdateEvents(Minutes: Positive) is
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
            if Events_List(CurrentIndex).EType in Disease | AttackOnBase and
              GetRandom(1, 100) < 10 then
               BaseIndex :=
                 SkyMap
                   (Events_List(CurrentIndex).SkyX,
                    Events_List(CurrentIndex).SkyY)
                   .BaseIndex;
               PopulationLost := GetRandom(1, 10);
               if PopulationLost > SkyBases(BaseIndex).Population then
                  PopulationLost := SkyBases(BaseIndex).Population;
                  SkyBases(BaseIndex).Reputation := (0, 0);
               end if;
               SkyBases(BaseIndex).Population :=
                 SkyBases(BaseIndex).Population - PopulationLost;
            end if;
            SkyMap
              (Events_List(CurrentIndex).SkyX, Events_List(CurrentIndex).SkyY)
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
            SkyMap(Events_List(I).SkyX, Events_List(I).SkyY).EventIndex := I;
         end loop Update_Map_Loop;
      end if;
   end UpdateEvents;

   procedure DeleteEvent(EventIndex: Positive) is
   begin
      SkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY)
        .EventIndex :=
        0;
      Events_List.Delete(Index => EventIndex);
      Delete_Events_Loop :
      for I in Events_List.First_Index .. Events_List.Last_Index loop
         SkyMap(Events_List(I).SkyX, Events_List(I).SkyY).EventIndex := I;
      end loop Delete_Events_Loop;
   end DeleteEvent;

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

   procedure GenerateTraders is
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
            FriendlyShips.Append(New_Item => Proto_Ships_Container.Key(I));
         end if;
      end loop Count_Friendly_Loop;
   end GenerateTraders;

   procedure RecoverBase(BaseIndex: Bases_Range) is
      MaxSpawnChance: Natural := 0;
      FactionRoll: Positive;
   begin
      Count_Spawn_Chance_Loop :
      for Faction of Factions_List loop
         MaxSpawnChance := MaxSpawnChance + Faction.SpawnChance;
      end loop Count_Spawn_Chance_Loop;
      FactionRoll := GetRandom(1, MaxSpawnChance);
      Choose_Faction_Loop :
      for I in Factions_List.Iterate loop
         if FactionRoll > Factions_List(I).SpawnChance then
            FactionRoll := FactionRoll - Factions_List(I).SpawnChance;
         else
            SkyBases(BaseIndex).Owner := Factions_Container.Key(I);
            SkyBases(BaseIndex).Reputation(1) :=
              GetReputation
                (Player_Ship.Crew(1).Faction, SkyBases(BaseIndex).Owner);
            exit Choose_Faction_Loop;
         end if;
      end loop Choose_Faction_Loop;
      SkyBases(BaseIndex).Population := GetRandom(2, 50);
      SkyBases(BaseIndex).Visited := (others => 0);
      SkyBases(BaseIndex).RecruitDate := (others => 0);
      SkyBases(BaseIndex).MissionsDate := (others => 0);
      AddMessage
        ("Base " & To_String(SkyBases(BaseIndex).Name) & " has a new owner.",
         OtherMessage, CYAN);
   end RecoverBase;

   procedure GenerateEnemies
     (Enemies: in out UnboundedString_Container.Vector;
      Owner: Unbounded_String := To_Unbounded_String("Any");
      WithTraders: Boolean := True) is
      PlayerValue: Natural := 0;
      Player_Ships: UnboundedString_Container.Vector;
   begin
      PlayerValue := CountCombatValue;
      if GetRandom(1, 100) > 98 then
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
           (WithTraders or
            Index(Proto_Ships_List(I).Name, To_String(Traders_Name)) = 0) then
            Enemies.Append(New_Item => Proto_Ships_Container.Key(I));
         end if;
      end loop Generate_Enemies_Loop;
   end GenerateEnemies;

end Events;
