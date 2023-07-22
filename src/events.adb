--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Bases; use Bases;
with BasesTypes;
with Combat;
with Crew;
with Factions;
with Items;
with Maps; use Maps;
with Messages;
with Ships.Cargo;
with Ships.Crew;
with Ships.Movement;
with Utils;

package body Events is

   function Check_For_Event return Boolean is
      use Ada.Strings.Unbounded;
      use Combat;
      use Crew;
      use Factions;
      use Items;
      use Messages;
      use Ships.Cargo;
      use Ships.Crew;
      use Ships.Movement;
      use Tiny_String;
      use Utils;

      Time_Passed: Integer := 0;
      Crew_Index: Crew_Container.Extended_Index := 0;
      Roll: Positive range 1 .. 100;
      Roll2: Integer range -20 .. 120 := 0;
      --## rule off IMPROPER_INITIALIZATION
      Engines: Positive_Container.Vector;
      Enemies: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
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
      if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
         case Events_List
           (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
           .E_Type is
            when ENEMYSHIP =>
               return
                 Start_Combat
                   (Enemy_Index =>
                      Events_List
                        (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                           .Event_Index)
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
               Crew_Index := Find_Member(Order => ENGINEER);
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
                    Get_Skill_Level
                      (Member => Player_Ship.Crew(Crew_Index),
                       Skill_Index => Engineering_Skill) then
                     Add_Message
                       (Message => "One of your engines is taking damage.",
                        M_Type => OTHERMESSAGE, Color => RED);
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
                     Update_Orders(Ship => Player_Ship);
                  else
                     Add_Message
                       (Message =>
                          To_String
                            (Source => Player_Ship.Crew(Crew_Index).Name) &
                          " has prevented engine damage.",
                        M_Type => OTHERMESSAGE, Color => GREEN);
                  end if;
                  Gain_Exp
                    (Amount => 1, Skill_Number => Engineering_Skill,
                     Crew_Index => Crew_Index);
               end if;
            when 6 .. 20 => -- Bad weather
               Crew_Index := Find_Member(Order => PILOT);
               if Crew_Index > 0 then
                  Add_Message
                    (Message =>
                       "Sudden bad weather causes your travel to take longer.",
                     M_Type => OTHERMESSAGE, Color => RED);
                  Time_Passed :=
                    60 -
                    Get_Skill_Level
                      (Member => Player_Ship.Crew(Crew_Index),
                       Skill_Index => Piloting_Skill);
                  if Time_Passed < 1 then
                     Time_Passed := 1;
                  end if;
                  Gain_Exp
                    (Amount => 1, Skill_Number => Piloting_Skill,
                     Crew_Index => Crew_Index);
                  Update_Cargo
                    (Ship => Player_Ship,
                     Proto_Index => Find_Proto_Item(Item_Type => Fuel_Type),
                     Amount => Count_Fuel_Needed);
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
               Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index :=
                 Events_List.Last_Index;
               Add_Message
                 (Message => "You've meet a friendly trader.",
                  M_Type => OTHERMESSAGE);
               Gain_Perception;
               Update_Orders(Ship => Player_Ship);
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
               Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index :=
                 Events_List.Last_Index;
               Add_Message
                 (Message => "You've spotted a friendly ship.",
                  M_Type => OTHERMESSAGE);
               Gain_Perception;
               Update_Orders(Ship => Player_Ship);
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
               Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index :=
                 Events_List.Last_Index;
               return
                 Start_Combat
                   (Enemy_Index =>
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
         if Player_Ship.Speed = DOCKED then
            if Roll < 5 and
              Player_Ship.Crew.Last_Index > 1 then -- Brawl in base
               Count_Injuries_Block :
               declare
                  --## rule off IMPROPER_INITIALIZATION
                  Resting_Crew: Positive_Container.Vector;
                  --## rule on IMPROPER_INITIALIZATION
                  Injuries: Positive := 1;
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
                     Add_Message
                       (Message =>
                          To_String
                            (Source =>
                               Player_Ship.Crew(Resting_Crew(Roll2)).Name) &
                          " was injured in a brawl inside the base.",
                        M_Type => OTHERMESSAGE, Color => RED);
                     if Player_Ship.Crew(Resting_Crew(Roll2)).Health = 0 then
                        Death
                          (Member_Index => Resting_Crew(Roll2),
                           Reason =>
                             To_Unbounded_String
                               (Source => "injuries in brawl in base"),
                           Ship => Player_Ship);
                     end if;
                  end if;
               end Count_Injuries_Block;
            elsif Roll > 4 and Roll < 10 then -- Lost cargo in base
               Roll2 :=
                 Get_Random
                   (Min => 1,
                    Max =>
                      Inventory_Container.Last_Index
                        (Container => Player_Ship.Cargo));
               Count_Lost_Cargo_Block :
               declare
                  Lost_Cargo: Positive range 1 .. 10 :=
                    Get_Random(Min => 1, Max => 10);
               begin
                  if Lost_Cargo >
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Roll2)
                      .Amount then
                     Lost_Cargo :=
                       Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => Roll2)
                         .Amount;
                  end if;
                  Add_Message
                    (Message =>
                       "During checking ship's cargo, you noticed that you lost" &
                       Positive'Image(Lost_Cargo) & " " &
                       Get_Item_Name
                         (Item =>
                            Inventory_Container.Element
                              (Container => Player_Ship.Cargo,
                               Index => Roll2)) &
                       ".",
                     M_Type => OTHERMESSAGE, Color => RED);
                  Update_Cargo
                    (Ship => Player_Ship, Amount => 0 - Lost_Cargo,
                     Cargo_Index => Roll2);
               end Count_Lost_Cargo_Block;
            end if;
         else
            if Roll in 21 .. 30 and
              Sky_Bases(Base_Index).Reputation.Level = -100 then
               Roll := 31;
            end if;
            if Get_Faction(Index => Sky_Bases(Base_Index).Owner).Flags.Contains
                (Item => To_Unbounded_String(Source => "diseaseimmune")) and
              Roll = 21 then
               Roll := 20;
            end if;
            case Roll is
               when 1 .. 20 => -- Base is attacked
                  Generate_Enemies
                    (Enemies => Enemies,
                     Owner => Tiny_String.To_Bounded_String(Source => "Any"),
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
                  Add_Message
                    (Message =>
                       "You can't dock to base now, because base is under attack. You can help defend it.",
                     M_Type => OTHERMESSAGE);
                  return
                    Start_Combat
                      (Enemy_Index =>
                         Events_List(Events_List.Last_Index).Ship_Index);
               when 21 => -- Disease in base
                  Events_List.Append
                    (New_Item =>
                       (E_Type => DISEASE, Sky_X => Player_Ship.Sky_X,
                        Sky_Y => Player_Ship.Sky_Y,
                        Time => Get_Random(Min => 10_080, Max => 12_000),
                        Data => 1));
                  Add_Message
                    (Message =>
                       "You can't dock to base now, it is closed due to disease.",
                     M_Type => OTHERMESSAGE);
               when 22 .. 30 => -- Double price for item in base
                  Set_Double_Price_Event_Block :
                  declare
                     use BasesTypes;

                     Item_Index: Natural := 0;
                     New_Item_Index: Natural := 0;
                  begin
                     Get_Price_Loop :
                     loop
                        Item_Index :=
                          Get_Random(Min => 1, Max => Get_Proto_Amount);
                        Find_Item_Index_Loop :
                        for J in 1 .. Get_Proto_Amount loop
                           Item_Index := Item_Index - 1;
                           if Item_Index = 0 then
                              if Get_Price
                                  (Base_Type =>
                                     Sky_Bases
                                       (Sky_Map
                                          (Player_Ship.Sky_X,
                                           Player_Ship.Sky_Y)
                                          .Base_Index)
                                       .Base_Type,
                                   Item_Index => J) >
                                0 then
                                 New_Item_Index := J;
                              end if;
                              exit Find_Item_Index_Loop;
                           end if;
                        end loop Find_Item_Index_Loop;
                        exit Get_Price_Loop when Get_Price
                            (Base_Type =>
                               Sky_Bases
                                 (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                    .Base_Index)
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
                    not Is_Friendly
                      (Source_Faction => Player_Ship.Crew(1).Faction,
                       Target_Faction => Sky_Bases(Base_Index).Owner) then
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
                     Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                       .Event_Index :=
                       Events_List.Last_Index;
                     return
                       Start_Combat
                         (Enemy_Index =>
                            Events_List(Events_List.Last_Index).Ship_Index);
                  end if;
                  Events_List.Append
                    (New_Item =>
                       (E_Type => FULLDOCKS, Sky_X => Player_Ship.Sky_X,
                        Sky_Y => Player_Ship.Sky_Y,
                        Time => Get_Random(Min => 15, Max => 30), Data => 1));
                  Add_Message
                    (Message =>
                       "You can't dock to base now, because it's docks are full.",
                     M_Type => OTHERMESSAGE, Color => RED);
            end case;
            Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index :=
              Events_List.Last_Index;
         end if;
      end if;
      return False;
   end Check_For_Event;

   procedure Set_Nim_Events is
      procedure Clear_Ada_Events with
         Import => True,
         Convention => C,
         External_Name => "clearAdaEvents";
   begin
      Clear_Ada_Events;
      Set_Events_In_Nim_Loop :
      for I in Events_List.Iterate loop
         Get_Ada_Event
           (Index => Events_Container.To_Index(Position => I),
            X => Events_List(I).Sky_X, Y => Events_List(I).Sky_Y,
            Time => Events_List(I).Time,
            E_Type => Events_Types'Pos(Events_List(I).E_Type),
            Data =>
              (case Events_List(I).E_Type is
                 when DOUBLEPRICE => Events_List(I).Item_Index,
                 when ATTACKONBASE | ENEMYSHIP | ENEMYPATROL | TRADER |
                   FRIENDLYSHIP => Events_List(I).Ship_Index,
                 when others => Events_List(I).Data));
         Sky_Map(Events_List(I).Sky_X, Events_List(I).Sky_Y).Event_Index := 0;
      end loop Set_Events_In_Nim_Loop;
   end Set_Nim_Events;

   procedure Update_Events(Minutes: Positive) is
      procedure Update_Ada_Events(M: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaEvents";
   begin
      if Events_List.Length = 0 then
         return;
      end if;
      Set_Nim_Events;
      Update_Ada_Events(M => Minutes);
      Events_List.Clear;
      Set_Events_In_Ada_Loop :
      for I in 1 .. 100 loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
   end Update_Events;

   procedure Delete_Event(Event_Index: Positive) is
      procedure Delete_Ada_Event(E_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "deleteAdaEvent";
   begin
      Set_Nim_Events;
      Delete_Ada_Event(E_Index => Event_Index);
      Sky_Map(Events_List(Event_Index).Sky_X, Events_List(Event_Index).Sky_Y)
        .Event_Index :=
        0;
      Events_List.Delete(Index => Event_Index);
      Delete_Events_Loop :
      for I in Events_List.First_Index .. Events_List.Last_Index loop
         Sky_Map(Events_List(I).Sky_X, Events_List(I).Sky_Y).Event_Index := I;
      end loop Delete_Events_Loop;
   end Delete_Event;

   procedure Generate_Traders is
      Ship_Index: Natural := 0;
      procedure Generate_Ada_Traders with
         Import => True,
         Convention => C,
         External_Name => "generateAdaTraders";
      function Get_Trader_Or_Friendly
        (Index, Get_Trader: Natural) return Natural with
         Import => True,
         Convention => C,
         External_Name => "getTraderOrFriendly";
   begin
      Generate_Ada_Traders;
      Count_Traders_Loop :
      for I in 1 .. Get_Proto_Ships_Amount loop
         Ship_Index := Get_Trader_Or_Friendly(Index => I, Get_Trader => 1);
         exit Count_Traders_Loop when Ship_Index = 0;
         Traders.Append(New_Item => I);
      end loop Count_Traders_Loop;
      Count_Friendly_Loop :
      for I in 1 .. Get_Proto_Ships_Amount loop
         Ship_Index := Get_Trader_Or_Friendly(Index => I, Get_Trader => 0);
         exit Count_Friendly_Loop when Ship_Index = 0;
         Friendly_Ships.Append(New_Item => I);
      end loop Count_Friendly_Loop;
   end Generate_Traders;

   procedure Recover_Base(Base_Index: Bases_Range) is
      procedure Recover_Ada_Base(B_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "recoverAdaBase";
   begin
      Set_Base_In_Nim(Base_Index => Base_Index);
      Recover_Ada_Base(B_Index => Base_Index);
      Get_Base_From_Nim(Base_Index => Base_Index);
   end Recover_Base;

   procedure Generate_Enemies
     (Enemies: in out Positive_Container.Vector;
      Owner: Tiny_String.Bounded_String :=
        Tiny_String.To_Bounded_String(Source => "Any");
      With_Traders: Boolean := True) is
      use Interfaces.C.Strings;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Ships_Array is array(0 .. 299) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      Nim_Ships: Nim_Ships_Array;
      procedure Generate_Ada_Enemies
        (E: out Nim_Ships_Array; O: chars_ptr; W_Traders: Integer) with
         Import => True,
         Convention => C,
         External_Name => "generateAdaEnemies";
   begin
      Generate_Ada_Enemies
        (E => Nim_Ships,
         O => New_String(Str => Tiny_String.To_String(Source => Owner)),
         W_Traders => (if With_Traders then 1 else 0));
      Convert_Ships_Loop :
      for Ship of Nim_Ships loop
         exit Convert_Ships_Loop when Ship = 0;
         Enemies.Append(New_Item => Ship);
      end loop Convert_Ships_Loop;
   end Generate_Enemies;

   procedure Set_Event(Index: Positive) is
      X, Y, Time, E_Type, Data: Integer;
      procedure Set_Ada_Event(I: Positive; X1, Y1, T, E, D: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaEvent";
   begin
      Set_Ada_Event
        (I => Index, X1 => X, Y1 => Y, T => Time, E => E_Type, D => Data);
      if X = -1 then
         return;
      end if;
      Sky_Map(X, Y).Event_Index := Index;
      case E_Type is
         when 1 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => ENEMYSHIP, Sky_X => X, Sky_Y => Y, Time => Time,
                  Ship_Index => Data));
         when 2 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => ATTACKONBASE, Sky_X => X, Sky_Y => Y, Time => Time,
                  Ship_Index => Data));
         when 3 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => DISEASE, Sky_X => X, Sky_Y => Y, Time => Time,
                  Data => Data));
         when 4 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => DOUBLEPRICE, Sky_X => X, Sky_Y => Y, Time => Time,
                  Item_Index => Data));
         when 6 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => FULLDOCKS, Sky_X => X, Sky_Y => Y, Time => Time,
                  Data => Data));
         when 7 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => ENEMYPATROL, Sky_X => X, Sky_Y => Y, Time => Time,
                  Ship_Index => Data));
         when 8 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => TRADER, Sky_X => X, Sky_Y => Y, Time => Time,
                  Ship_Index => Data));
         when 9 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => FRIENDLYSHIP, Sky_X => X, Sky_Y => Y, Time => Time,
                  Ship_Index => Data));
         when others =>
            null;
      end case;
   end Set_Event;

end Events;
