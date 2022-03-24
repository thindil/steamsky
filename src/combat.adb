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

with GNAT.String_Split; use GNAT.String_Split;
with Crew; use Crew;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Items; use Items;
with Statistics; use Statistics;
with Events; use Events;
with Maps; use Maps;
with Bases; use Bases;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Utils; use Utils;
with Log; use Log;
with Goals; use Goals;
with Factions; use Factions;
with Stories; use Stories;
with Config; use Config;
with Trades; use Trades;

package body Combat is

   -- ****iv* Combat/Combat.Faction_Name
   -- FUNCTION
   -- Name of enemy ship (and its crew) faction
   -- SOURCE
   Faction_Name: Tiny_String.Bounded_String;
   -- ****

   -- ****iv* Combat/Combat.Turn_Number
   -- FUNCTION
   -- Number of turn of combat
   -- SOURCE
   Turn_Number: Natural;
   -- ****

   function Start_Combat
     (Enemy_Index: Unbounded_String; New_Combat: Boolean := True)
      return Boolean is
      use Tiny_String;

      Enemy_Ship: Ship_Record;
      Enemy_Guns: Guns_Container.Vector;
      Shooting_Speed: Integer;
      function Count_Perception
        (Spotter, Spotted: Ship_Record) return Natural is
         Result: Natural := 0;
      begin
         Count_Spotter_Perception_Loop :
         for I in Spotter.Crew.Iterate loop
            case Spotter.Crew(I).Order is
               when PILOT =>
                  Result :=
                    Result +
                    Get_Skill_Level
                      (Member => Spotter.Crew(I),
                       Skill_Index => Perception_Skill);
                  if Spotter = Player_Ship then
                     Gain_Exp
                       (Amount => 1, Skill_Number => Perception_Skill,
                        Crew_Index => Crew_Container.To_Index(Position => I));
                  end if;
               when GUNNER =>
                  Result :=
                    Result +
                    Get_Skill_Level
                      (Member => Spotter.Crew(I),
                       Skill_Index => Perception_Skill);
                  if Spotter = Player_Ship then
                     Gain_Exp
                       (Amount => 1, Skill_Number => Perception_Skill,
                        Crew_Index => Crew_Container.To_Index(Position => I));
                  end if;
               when others =>
                  null;
            end case;
         end loop Count_Spotter_Perception_Loop;
         Count_Modules_Loop :
         for Module of Spotted.Modules loop
            if Module.M_Type = HULL then
               Result := Result + Module.Max_Modules;
               exit Count_Modules_Loop;
            end if;
         end loop Count_Modules_Loop;
         return Result;
      end Count_Perception;
   begin
      Enemy_Ship_Index := Enemy_Index;
      Faction_Name := Factions_List(Proto_Ships_List(Enemy_Index).Owner).Name;
      Harpoon_Duration := 0;
      Boarding_Orders.Clear;
      Enemy_Ship :=
        Create_Ship
          (Proto_Index => Enemy_Index, Name => Null_Unbounded_String,
           X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y,
           Speed => FULL_SPEED);
      -- Enemy ship is trader, generate cargo for it
      if Index
          (Source => Proto_Ships_List(Enemy_Index).Name,
           Pattern => To_String(Source => Traders_Name)) >
        0 then
         GenerateTraderCargo(ProtoIndex => Enemy_Index);
         Update_Cargo_Loop :
         for I in
           BaseCargo_Container.First_Index(Container => TraderCargo) ..
             BaseCargo_Container.Last_Index(Container => TraderCargo) loop
            UpdateCargo
              (Ship => Enemy_Ship,
               ProtoIndex =>
                 BaseCargo_Container.Element
                   (Container => TraderCargo, Index => I)
                   .Proto_Index,
               Amount =>
                 BaseCargo_Container.Element
                   (Container => TraderCargo, Index => I)
                   .Amount);
         end loop Update_Cargo_Loop;
         BaseCargo_Container.Clear(Container => TraderCargo);
      end if;
      Add_Enemy_Cargo_Block :
      declare
         Min_Free_Space, Item_Index, Cargo_Item_Index: Natural := 0;
         Item_Amount: Positive;
         New_Item_Index: Tiny_String.Bounded_String;
         Item: Inventory_Data;
      begin
         Count_Free_Space_Loop :
         for Module of Enemy_Ship.Modules loop
            if Module.M_Type = CARGO_ROOM and Module.Durability > 0 then
               Min_Free_Space :=
                 Min_Free_Space + Modules_List(Module.Proto_Index).Max_Value;
            end if;
         end loop Count_Free_Space_Loop;
         Min_Free_Space :=
           Natural
             (Float(Min_Free_Space) *
              (1.0 - (Float(Get_Random(Min => 20, Max => 70)) / 100.0)));
         Add_Enemy_Cargo_Loop :
         loop
            exit Add_Enemy_Cargo_Loop when FreeCargo
                (Amount => 0, Ship => Enemy_Ship) <=
              Min_Free_Space;
            Item_Index :=
              Get_Random(Min => 1, Max => Positive(Items_List.Length));
            Find_Item_Index_Loop :
            for I in Items_List.Iterate loop
               Item_Index := Item_Index - 1;
               if Item_Index = 0 then
                  New_Item_Index := Objects_Container.Key(Position => I);
                  exit Find_Item_Index_Loop;
               end if;
            end loop Find_Item_Index_Loop;
            Item_Amount :=
              (if Enemy_Ship.Crew.Length < 5 then
                 Get_Random(Min => 1, Max => 100)
               elsif Enemy_Ship.Crew.Length < 10 then
                 Get_Random(Min => 1, Max => 500)
               else Get_Random(Min => 1, Max => 1_000));
            Cargo_Item_Index :=
              Find_Item
                (Inventory => Enemy_Ship.Cargo, Proto_Index => New_Item_Index);
            if Cargo_Item_Index > 0 then
               Item :=
                 Inventory_Container.Element
                   (Container => Enemy_Ship.Cargo, Index => Cargo_Item_Index);
               Item.Amount := Item.Amount + Item_Amount;
               Inventory_Container.Replace_Element
                 (Container => Enemy_Ship.Cargo, Index => Cargo_Item_Index,
                  New_Item => Item);
            else
               if FreeCargo
                   (Amount =>
                      0 - (Items_List(New_Item_Index).Weight * Item_Amount)) >
                 -1 then
                  Inventory_Container.Append
                    (Container => Enemy_Ship.Cargo,
                     New_Item =>
                       (Proto_Index => New_Item_Index, Amount => Item_Amount,
                        Durability => 100, Name => Null_Bounded_String,
                        Price => 0));
               end if;
            end if;
         end loop Add_Enemy_Cargo_Loop;
      end Add_Enemy_Cargo_Block;
      Enemy_Guns.Clear;
      Count_Enemy_Shooting_Speed_Loop :
      for I in Enemy_Ship.Modules.Iterate loop
         if (Enemy_Ship.Modules(I).M_Type in GUN | HARPOON_GUN) and
           Enemy_Ship.Modules(I).Durability > 0 then
            if Modules_List(Enemy_Ship.Modules(I).Proto_Index).Speed > 0 then
               Shooting_Speed :=
                 (if Proto_Ships_List(Enemy_Index).Combat_Ai = DISARMER then
                    Natural
                      (Float'Ceiling
                         (Float
                            (Modules_List(Enemy_Ship.Modules(I).Proto_Index)
                               .Speed) /
                          2.0))
                  else Modules_List(Enemy_Ship.Modules(I).Proto_Index).Speed);
            else
               Shooting_Speed :=
                 (if Proto_Ships_List(Enemy_Index).Combat_Ai = DISARMER then
                    Modules_List(Enemy_Ship.Modules(I).Proto_Index).Speed - 1
                  else Modules_List(Enemy_Ship.Modules(I).Proto_Index).Speed);
            end if;
            Enemy_Guns.Append
              (New_Item =>
                 (1 => Modules_Container.To_Index(Position => I), 2 => 1,
                  3 => Shooting_Speed));
         end if;
      end loop Count_Enemy_Shooting_Speed_Loop;
      Enemy :=
        (Ship => Enemy_Ship, Accuracy => 0, Distance => 10_000,
         Combat_Ai => Proto_Ships_List(Enemy_Index).Combat_Ai, Evasion => 0,
         Loot => 0, Perception => 0, Harpoon_Duration => 0,
         Guns => Enemy_Guns);
      Enemy.Accuracy :=
        (if Proto_Ships_List(Enemy_Index).Accuracy(2) = 0 then
           Proto_Ships_List(Enemy_Index).Accuracy(1)
         else Get_Random
             (Min => Proto_Ships_List(Enemy_Index).Accuracy(1),
              Max => Proto_Ships_List(Enemy_Index).Accuracy(2)));
      Enemy.Evasion :=
        (if Proto_Ships_List(Enemy_Index).Evasion(2) = 0 then
           Proto_Ships_List(Enemy_Index).Evasion(1)
         else Get_Random
             (Min => Proto_Ships_List(Enemy_Index).Evasion(1),
              Max => Proto_Ships_List(Enemy_Index).Evasion(2)));
      Enemy.Perception :=
        (if Proto_Ships_List(Enemy_Index).Perception(2) = 0 then
           Proto_Ships_List(Enemy_Index).Perception(1)
         else Get_Random
             (Min => Proto_Ships_List(Enemy_Index).Perception(1),
              Max => Proto_Ships_List(Enemy_Index).Perception(2)));
      Enemy.Loot :=
        (if Proto_Ships_List(Enemy_Index).Loot(2) = 0 then
           Proto_Ships_List(Enemy_Index).Loot(1)
         else Get_Random
             (Min => Proto_Ships_List(Enemy_Index).Loot(1),
              Max => Proto_Ships_List(Enemy_Index).Loot(2)));
      if Pilot_Order = 0 then
         Pilot_Order := 2;
         Engineer_Order := 3;
      end if;
      End_Combat := False;
      Enemy_Name :=
        Generate_Ship_Name(Owner => Proto_Ships_List(Enemy_Index).Owner);
      Messages_Starts := Get_Last_Message_Index + 1;
      Set_Player_Guns_List_Block :
      declare
         Old_Guns_List: constant Guns_Container.Vector := Guns;
         Same_Lists: Boolean := True;
      begin
         Guns.Clear;
         Set_Player_Guns_Loop :
         for I in Player_Ship.Modules.Iterate loop
            if (Player_Ship.Modules(I).M_Type in GUN | HARPOON_GUN) and
              Player_Ship.Modules(I).Durability > 0 then
               Guns.Append
                 (New_Item =>
                    (1 => Modules_Container.To_Index(Position => I), 2 => 1,
                     3 =>
                       Modules_List(Player_Ship.Modules(I).Proto_Index)
                         .Speed));
            end if;
         end loop Set_Player_Guns_Loop;
         if Old_Guns_List.Length > 0 and
           Old_Guns_List.Length = Guns.Length then
            Compare_Lists_Loop :
            for I in Guns.First_Index .. Guns.Last_Index loop
               if Guns(I)(1) /= Old_Guns_List(I)(1) then
                  Same_Lists := False;
                  exit Compare_Lists_Loop;
               end if;
            end loop Compare_Lists_Loop;
            if Same_Lists then
               Guns := Old_Guns_List;
            end if;
         end if;
      end Set_Player_Guns_List_Block;
      if New_Combat then
         Start_Combat_Block :
         declare
            Player_Perception: constant Natural :=
              Count_Perception(Spotter => Player_Ship, Spotted => Enemy.Ship);
            Enemy_Perception: Natural := 0;
         begin
            Old_Speed := Player_Ship.Speed;
            Enemy_Perception :=
              (if Enemy.Perception > 0 then Enemy.Perception
               else Count_Perception
                   (Spotter => Enemy.Ship, Spotted => Player_Ship));
            if (Player_Perception + Get_Random(Min => 1, Max => 50)) >
              (Enemy_Perception + Get_Random(Min => 1, Max => 50)) then
               Add_Message
                 (Message =>
                    "You spotted " & To_String(Source => Enemy.Ship.Name) &
                    ".",
                  M_Type => OTHERMESSAGE);
            else
               if RealSpeed(Ship => Player_Ship) <
                 RealSpeed(Ship => Enemy.Ship) then
                  Log_Message
                    (Message =>
                       "You were attacked by " &
                       To_String(Source => Enemy.Ship.Name),
                     Message_Type => Log.COMBAT);
                  Add_Message
                    (Message =>
                       To_String(Source => Enemy.Ship.Name) &
                       " intercepted you.",
                     M_Type => COMBATMESSAGE);
                  return True;
               end if;
               Add_Message
                 (Message =>
                    "You spotted " & To_String(Source => Enemy.Ship.Name) &
                    ".",
                  M_Type => OTHERMESSAGE);
            end if;
         end Start_Combat_Block;
         return False;
      end if;
      Turn_Number := 0;
      Log_Message
        (Message =>
           "Started combat with " & To_String(Source => Enemy.Ship.Name),
         Message_Type => Log.COMBAT);
      return True;
   end Start_Combat;

   procedure Combat_Turn is
      use Tiny_String;

      Accuracy_Bonus, Evade_Bonus: Integer := 0;
      Pilot_Index, Engineer_Index, Enemy_Weapon_Index, Enemy_Ammo_Index,
      Enemy_Pilot_Index, Ammo_Index_2: Natural := 0;
      Distance_Traveled, Speed_Bonus: Integer;
      Shoot_Message, Message: Unbounded_String;
      Enemy_Pilot_Order: Positive := 2;
      Damage_Range: Positive := 10_000;
      Ship_Free_Space: Integer := 0;
      procedure Attack(Ship, Enemy_Ship: in out Ship_Record) is
         Gunner_Index: Crew_Container.Extended_Index;
         Ammo_Index: Inventory_Container.Extended_Index;
         Armor_Index, Weapon_Index: Modules_Container.Extended_Index;
         Shoots: Natural;
         Gunner_Order: Positive;
         Hit_Chance, Hit_Location, Current_Accuracy_Bonus: Integer;
         Damage: Damage_Factor := 0.0;
         Weapon_Damage: Integer;
         Enemy_Name_Owner: constant Unbounded_String :=
           Enemy_Name & To_Unbounded_String(Source => " (") &
           To_String(Source => Faction_Name) &
           To_Unbounded_String(Source => ")");
         procedure Remove_Gun(Module_Index: Positive) is
         begin
            if Enemy_Ship = Player_Ship then
               Remove_Gun_Loop :
               for J in Guns.First_Index .. Guns.Last_Index loop
                  if Guns(J)(1) = Module_Index then
                     Guns.Delete(Index => J);
                     exit Remove_Gun_Loop;
                  end if;
               end loop Remove_Gun_Loop;
            end if;
         end Remove_Gun;
         function Find_Enemy_Module(M_Type: Module_Type) return Natural is
         begin
            Find_Enemy_Module_Loop :
            for I in Enemy_Ship.Modules.Iterate loop
               if Modules_List(Enemy_Ship.Modules(I).Proto_Index).M_Type =
                 M_Type and
                 Enemy_Ship.Modules(I).Durability > 0 then
                  return Modules_Container.To_Index(Position => I);
               end if;
            end loop Find_Enemy_Module_Loop;
            return 0;
         end Find_Enemy_Module;
         procedure Find_Hit_Weapon is
         begin
            Find_Weapon_Location_Loop :
            for J in Enemy_Ship.Modules.Iterate loop
               if
                 ((Enemy_Ship.Modules(J).M_Type = TURRET
                   and then Enemy_Ship.Modules(J).Gun_Index > 0) or
                  Modules_List(Enemy_Ship.Modules(J).Proto_Index).M_Type =
                    BATTERING_RAM) and
                 Enemy_Ship.Modules(J).Durability > 0 then
                  Hit_Location := Modules_Container.To_Index(Position => J);
                  return;
               end if;
            end loop Find_Weapon_Location_Loop;
         end Find_Hit_Weapon;
      begin
         if Ship = Player_Ship then
            Log_Message
              (Message => "Player's round.", Message_Type => Log.COMBAT);
         else
            Log_Message
              (Message => "Enemy's round.", Message_Type => Log.COMBAT);
         end if;
         Attack_Loop :
         for K in Ship.Modules.Iterate loop
            if Ship.Modules(K).Durability = 0 or
              (Ship.Modules(K).M_Type not in GUN | BATTERING_RAM |
                   HARPOON_GUN) then
               goto End_Of_Attack_Loop;
            end if;
            Gunner_Index := 0;
            Ammo_Index := 0;
            if Ship.Modules(K).M_Type = HARPOON_GUN then
               Ammo_Index_2 := Ship.Modules(K).Harpoon_Index;
            elsif Ship.Modules(K).M_Type = GUN then
               Ammo_Index_2 := Ship.Modules(K).Ammo_Index;
            end if;
            if Ship.Modules(K).M_Type in GUN | HARPOON_GUN then
               Gunner_Index := Ship.Modules(K).Owner(1);
               Log_Message
                 (Message =>
                    "Gunner index:" & Natural'Image(Gunner_Index) & ".",
                  Message_Type => Log.COMBAT);
               if Ship = Player_Ship then
                  Shoots := 0;
                  if Gunner_Index > 0 then
                     Count_Player_Shoots_Loop :
                     for Gun of Guns loop
                        if Gun(1) = Modules_Container.To_Index(Position => K) then
                           Gunner_Order := Gun(2);
                           if Gun(3) > 0 then
                              Shoots := Gun(3);
                              if Gunner_Order /= 3 then
                                 Shoots :=
                                   Natural(Float'Ceiling(Float(Shoots) / 2.0));
                              end if;
                              Log_Message
                                (Message => "Player Shoots (no cooldown):" &
                                 Natural'Image(Shoots),
                                 Message_Type => Log.COMBAT);
                           elsif Gun(3) < 0 then
                              Shoots := 0;
                              Gun(3) := Gun(3) + 1;
                              if Gun(3) = 0 then
                                 Shoots := 1;
                                 Gun(3) :=
                                   (if Gunner_Order = 3 then
                                      Modules_List
                                        (Player_Ship.Modules(Gun(1))
                                           .Proto_Index)
                                        .Speed
                                    else Modules_List
                                        (Player_Ship.Modules(Gun(1))
                                           .Proto_Index)
                                        .Speed -
                                      1);
                              end if;
                              Log_Message
                                (Message => "Player Shoots (after cooldown):" &
                                 Natural'Image(Shoots),
                                 Message_Type => Log.COMBAT);
                           end if;
                           exit Count_Player_Shoots_Loop;
                        end if;
                     end loop Count_Player_Shoots_Loop;
                     Log_Message
                       (Message => "Shoots test3:" & Natural'Image(Shoots), Message_Type => Log.COMBAT);
                     if Ship.Crew(Gunner_Index).Order /= GUNNER then
                        Gunner_Order := 1;
                     end if;
                     case Gunner_Order is
                        when 1 =>
                           if Shoots > 0 then
                              Shoots := 0;
                           end if;
                        when 2 =>
                           Current_Accuracy_Bonus := Accuracy_Bonus + 20;
                        when 4 =>
                           Current_Accuracy_Bonus := Accuracy_Bonus - 10;
                        when 5 =>
                           Current_Accuracy_Bonus := Accuracy_Bonus - 20;
                        when others =>
                           null;
                     end case;
                  end if;
               else
                  Count_Enemy_Shoots_Loop :
                  for Gun of Enemy.Guns loop
                     if Gun(1) = Modules_Container.To_Index(Position => K) then
                        if Gun(3) > 0 then
                           Shoots := Gun(3);
                        elsif Gun(3) < 0 then
                           Shoots := 0;
                           Gun(3) := Gun(3) + 1;
                           if Gun(3) = 0 then
                              Shoots := 1;
                              Gun(3) :=
                                (if Enemy.Combat_Ai = DISARMER then
                                   Modules_List
                                     (Ship.Modules(Gun(1)).Proto_Index)
                                     .Speed -
                                   1
                                 else Modules_List
                                     (Ship.Modules(Gun(1)).Proto_Index)
                                     .Speed);
                           end if;
                        end if;
                        exit Count_Enemy_Shoots_Loop;
                     end if;
                  end loop Count_Enemy_Shoots_Loop;
                  if Ship.Crew.Length > 0 and Gunner_Index = 0 then
                     Shoots := 0;
                  end if;
               end if;
               if Ammo_Index_2 in
                   Inventory_Container.First_Index(Container => Ship.Cargo) ..
                         Inventory_Container.Last_Index
                           (Container => Ship.Cargo)
                 and then
                   Items_List
                     (Inventory_Container.Element
                        (Container => Ship.Cargo, Index => Ammo_Index_2)
                        .Proto_Index)
                     .I_Type =
                   Items_Types
                     (Modules_List(Ship.Modules(K).Proto_Index).Value) then
                  Ammo_Index := Ammo_Index_2;
               end if;
               if Ammo_Index = 0 then
                  Find_Ammo_Index_Loop :
                  for I in Items_List.Iterate loop
                     if Items_List(I).I_Type =
                       Items_Types
                         (Modules_List(Ship.Modules(K).Proto_Index).Value) then
                        Get_Ammo_Index_Loop :
                        for J in
                          Inventory_Container.First_Index
                            (Container => Ship.Cargo) ..
                            Inventory_Container.Last_Index
                              (Container => Ship.Cargo) loop
                           if Inventory_Container.Element
                               (Container => Ship.Cargo, Index => J)
                               .Proto_Index =
                             Objects_Container.Key(Position => I) then
                              Ammo_Index := J;
                              if Ship.Modules(K).M_Type = HARPOON_GUN then
                                 Ship.Modules(K).Harpoon_Index := Ammo_Index;
                              elsif Ship.Modules(K).M_Type = GUN then
                                 Ship.Modules(K).Ammo_Index := Ammo_Index;
                              end if;
                              exit Get_Ammo_Index_Loop;
                           end if;
                        end loop Get_Ammo_Index_Loop;
                        exit Find_Ammo_Index_Loop when Ammo_Index > 0;
                     end if;
                  end loop Find_Ammo_Index_Loop;
               end if;
               if Ammo_Index = 0 then
                  if Ship = Player_Ship then
                     Add_Message
                       (Message => "You don't have ammo to " &
                        To_String(Source => Ship.Modules(K).Name) & "!",
                        M_Type => COMBATMESSAGE, Color => RED);
                  end if;
                  Shoots := 0;
               elsif Inventory_Container.Element
                   (Container => Ship.Cargo, Index => Ammo_Index)
                   .Amount <
                 Shoots then
                  Shoots :=
                    Inventory_Container.Element
                      (Container => Ship.Cargo, Index => Ammo_Index)
                      .Amount;
               end if;
               if Enemy.Distance > 5_000 then
                  Shoots := 0;
               end if;
               if Ship.Modules(K).M_Type = HARPOON_GUN and Shoots > 0 then
                  Shoots := 1;
                  if Enemy.Distance > 2_000 then
                     Shoots := 0;
                  end if;
                  if Find_Enemy_Module(M_Type => ARMOR) > 0 then
                     Shoots := 0;
                  end if;
               end if;
               if Ship.Modules(K).M_Type = GUN and Shoots > 0 then
                  case Items_List
                    (Inventory_Container.Element
                       (Container => Ship.Cargo, Index => Ammo_Index)
                       .Proto_Index)
                    .Value
                    (2) is
                     when 2 =>
                        if Ship = Player_Ship then
                           Current_Accuracy_Bonus :=
                             Current_Accuracy_Bonus - 10;
                        else
                           Evade_Bonus := Evade_Bonus + 10;
                        end if;
                     when 3 =>
                        if Ship = Player_Ship then
                           Current_Accuracy_Bonus :=
                             Current_Accuracy_Bonus + 10;
                        else
                           Evade_Bonus := Evade_Bonus - 10;
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
            else
               if Enemy.Distance > 100 then
                  Shoots := 0;
               else
                  Shoots := (if Ship.Modules(K).Cooling_Down then 0 else 1);
               end if;
               Ship.Modules(K).Cooling_Down :=
                 not Ship.Modules(K).Cooling_Down;
            end if;
            Log_Message(Message => "Shoots:" & Integer'Image(Shoots), Message_Type => Log.COMBAT);
            if Shoots > 0 then
               Hit_Chance :=
                 (if Ship = Player_Ship then
                    Current_Accuracy_Bonus - Enemy.Evasion
                  else Enemy.Accuracy - Evade_Bonus);
               if Gunner_Index > 0 then
                  Hit_Chance :=
                    Hit_Chance +
                    Get_Skill_Level(Member => Ship.Crew(Gunner_Index), Skill_Index => Gunnery_Skill);
               end if;
               if Hit_Chance < -48 then
                  Hit_Chance := -48;
               end if;
               Log_Message
                 (Message => "Player Accuracy:" & Integer'Image(Current_Accuracy_Bonus) &
                  " Player Evasion:" & Integer'Image(Evade_Bonus),
                  Message_Type => Log.COMBAT);
               Log_Message
                 ("Enemy Evasion:" & Integer'Image(Enemy.Evasion) &
                  " Enemy Accuracy:" & Integer'Image(Enemy.Accuracy),
                  Log.COMBAT);
               Log_Message
                 ("Chance to hit:" & Integer'Image(Hit_Chance), Log.COMBAT);
               Shooting_Loop :
               for I in 1 .. Shoots loop
                  if Ship = Player_Ship then
                     Shoot_Message :=
                       (if Ship.Modules(K).M_Type in GUN | HARPOON_GUN then
                          To_String(Source => Ship.Crew(Gunner_Index).Name) &
                          To_Unbounded_String(" shoots at ") & Enemy_Name_Owner
                        else To_Unbounded_String("You ram ") &
                          Enemy_Name_Owner);
                  else
                     Shoot_Message :=
                       Enemy_Name_Owner & To_Unbounded_String(" attacks");
                  end if;
                  if Hit_Chance + Get_Random(1, 50) >
                    Get_Random(1, Hit_Chance + 50) then
                     Shoot_Message :=
                       Shoot_Message & To_Unbounded_String(" and hits ");
                     Armor_Index := Find_Enemy_Module(ARMOR);
                     if Armor_Index > 0 then
                        Hit_Location := Armor_Index;
                     else
                        if Ship = Player_Ship then
                           if Gunner_Index > 0
                             and then Gunner_Order in
                               4 ..
                                     6 then -- aim for part of enemy ship
                              Hit_Location := 0;
                              case Gunner_Order is
                                 when 4 =>
                                    Hit_Location := Find_Enemy_Module(ENGINE);
                                 when 5 =>
                                    Hit_Location := 0;
                                    Find_Hit_Weapon;
                                    if Hit_Location = 0 then
                                       Hit_Location :=
                                         Find_Enemy_Module(BATTERING_RAM);
                                    end if;
                                 when 6 =>
                                    Hit_Location := Find_Enemy_Module(HULL);
                                 when others =>
                                    Hit_Location := 1;
                              end case;
                              if Hit_Location = 0 then
                                 Hit_Location := 1;
                              end if;
                           else
                              Hit_Location :=
                                Get_Random
                                  (Enemy.Ship.Modules.First_Index,
                                   Enemy.Ship.Modules.Last_Index);
                           end if;
                        else
                           if Enemy.Combat_Ai = DISARMER then
                              Hit_Location := 1;
                              Find_Hit_Weapon;
                           else
                              Hit_Location :=
                                Get_Random
                                  (Player_Ship.Modules.First_Index,
                                   Player_Ship.Modules.Last_Index);
                           end if;
                        end if;
                        Get_Hit_Location_Loop :
                        while Enemy_Ship.Modules(Hit_Location).Durability =
                          0 loop
                           Hit_Location := Hit_Location - 1;
                           exit Attack_Loop when Hit_Location = 0;
                        end loop Get_Hit_Location_Loop;
                     end if;
                     Shoot_Message :=
                       Shoot_Message & Enemy_Ship.Modules(Hit_Location).Name &
                       To_Unbounded_String(".");
                     Damage :=
                       1.0 -
                       Damage_Factor
                         (Float(Ship.Modules(K).Durability) /
                          Float(Ship.Modules(K).Max_Durability));
                     if Ship.Modules(K).M_Type = HARPOON_GUN then
                        Weapon_Damage :=
                          Ship.Modules(K).Duration -
                          Natural
                            (Float(Ship.Modules(K).Duration) * Float(Damage));
                     elsif Ship.Modules(K).M_Type = GUN then
                        Weapon_Damage :=
                          Ship.Modules(K).Damage -
                          Natural
                            (Float(Ship.Modules(K).Damage) * Float(Damage));
                     elsif Ship.Modules(K).M_Type = BATTERING_RAM then
                        Weapon_Damage :=
                          Ship.Modules(K).Damage2 -
                          Natural
                            (Float(Ship.Modules(K).Damage2) * Float(Damage));
                        Weapon_Damage :=
                          (if Speed_Bonus < 0 then
                             Weapon_Damage +
                             (abs (Speed_Bonus) *
                              (Count_Ship_Weight(Ship) / 5_000))
                           else Weapon_Damage +
                             (Count_Ship_Weight(Ship) / 5_000));
                     end if;
                     if Weapon_Damage = 0 then
                        Weapon_Damage := 1;
                     end if;
                     if Ammo_Index > 0 then
                        Weapon_Damage :=
                          Weapon_Damage +
                          Items_List
                            (Inventory_Container.Element
                               (Container => Ship.Cargo, Index => Ammo_Index)
                               .Proto_Index)
                            .Value
                            (1);
                     end if;
                     Weapon_Damage :=
                       (if Ship = Player_Ship then
                          Integer
                            (Float(Weapon_Damage) *
                             Float(New_Game_Settings.Player_Damage_Bonus))
                        else Integer
                            (Float(Weapon_Damage) *
                             Float(New_Game_Settings.Enemy_Damage_Bonus)));
                     if Armor_Index = 0 then
                        if Ship.Modules(K).M_Type = HARPOON_GUN then
                           Count_Damage_Loop :
                           for Module of Enemy_Ship.Modules loop
                              if Module.M_Type = HULL then
                                 Weapon_Damage :=
                                   Weapon_Damage - (Module.Max_Modules / 10);
                                 if Weapon_Damage < 1 then
                                    Weapon_Damage := 1;
                                 end if;
                                 exit Count_Damage_Loop;
                              end if;
                           end loop Count_Damage_Loop;
                           if Ship = Player_Ship then
                              Enemy.Harpoon_Duration :=
                                Enemy.Harpoon_Duration + Weapon_Damage;
                           else
                              Harpoon_Duration :=
                                Harpoon_Duration + Weapon_Damage;
                           end if;
                           Weapon_Damage := 1;
                        elsif Ship.Modules(K).M_Type = BATTERING_RAM then
                           if Ship = Player_Ship then
                              Enemy.Harpoon_Duration :=
                                Enemy.Harpoon_Duration + 2;
                           else
                              Harpoon_Duration := Harpoon_Duration + 2;
                           end if;
                        end if;
                     end if;
                     Damage_Module
                       (Enemy_Ship, Hit_Location, Weapon_Damage,
                        "enemy fire in ship combat");
                     if Enemy_Ship.Modules(Hit_Location).Durability = 0 then
                        case Modules_List
                          (Enemy_Ship.Modules(Hit_Location).Proto_Index)
                          .M_Type is
                           when HULL | ENGINE =>
                              End_Combat := True;
                           when TURRET =>
                              if Enemy_Ship = Player_Ship then
                                 Weapon_Index :=
                                   Enemy_Ship.Modules(Hit_Location).Gun_Index;
                                 if Weapon_Index > 0 then
                                    Enemy_Ship.Modules(Weapon_Index)
                                      .Durability :=
                                      0;
                                    Remove_Gun(Weapon_Index);
                                 end if;
                              end if;
                           when GUN =>
                              if Enemy_Ship = Player_Ship then
                                 Remove_Gun(Hit_Location);
                              end if;
                           when others =>
                              null;
                        end case;
                     end if;
                     if Ship = Player_Ship then
                        Add_Message
                          (To_String(Shoot_Message), COMBATMESSAGE, GREEN);
                     else
                        Add_Message
                          (To_String(Shoot_Message), COMBATMESSAGE, YELLOW);
                     end if;
                  else
                     Shoot_Message :=
                       Shoot_Message & To_Unbounded_String(" and misses.");
                     if Ship = Player_Ship then
                        Add_Message
                          (To_String(Shoot_Message), COMBATMESSAGE, BLUE);
                     else
                        Add_Message
                          (To_String(Shoot_Message), COMBATMESSAGE, CYAN);
                     end if;
                  end if;
                  if Ammo_Index > 0 then
                     UpdateCargo
                       (Ship => Ship, CargoIndex => Ammo_Index, Amount => -1);
                  end if;
                  if Ship = Player_Ship and Gunner_Index > 0 then
                     Gain_Exp(2, Gunnery_Skill, Gunner_Index);
                  end if;
                  if Player_Ship.Crew(1).Health = 0 then -- player is dead
                     End_Combat := True;
                  end if;
                  exit Attack_Loop when End_Combat;
               end loop Shooting_Loop;
            end if;
            <<End_Of_Attack_Loop>>
         end loop Attack_Loop;
      end Attack;
      procedure MeleeCombat
        (Attackers, Defenders: in out Crew_Container.Vector;
         PlayerAttack: Boolean) is
         AttackDone, Riposte: Boolean;
         AttackerIndex, DefenderIndex: Positive;
         OrderIndex: Natural;
         function CharacterAttack
           (AttackerIndex, DefenderIndex: Positive; PlayerAttack2: Boolean)
            return Boolean is
            HitChance, Damage: Integer;
            HitLocation: constant Equipment_Locations :=
              Equipment_Locations'Val
                (Get_Random
                   (Equipment_Locations'Pos(HELMET),
                    Equipment_Locations'Pos(LEGS)));
            LocationNames: constant array
              (HELMET .. LEGS) of Unbounded_String :=
              (To_Unbounded_String("head"), To_Unbounded_String("torso"),
               To_Unbounded_String("arm"), To_Unbounded_String("leg"));
            AttackSkill, BaseDamage: Natural;
            Wounds: Damage_Factor := 0.0;
            MessageColor: Message_Color;
            Attacker: Member_Data :=
              (if PlayerAttack2 then Player_Ship.Crew(AttackerIndex)
               else Enemy.Ship.Crew(AttackerIndex));
            Defender: Member_Data :=
              (if PlayerAttack2 then Enemy.Ship.Crew(DefenderIndex)
               else Player_Ship.Crew(DefenderIndex));
            AttackMessage: Unbounded_String :=
              (if PlayerAttack2 then
                 To_String(Source => Attacker.Name) &
                 To_Unbounded_String(" attacks ") &
                 To_String(Source => Defender.Name) &
                 To_Unbounded_String(" (") &
                 To_String(Source => Faction_Name) & To_Unbounded_String(")")
               else To_String(Source => Attacker.Name) &
                 To_Unbounded_String(" (") &
                 To_String(Source => Faction_Name) & To_Unbounded_String(")") &
                 To_Unbounded_String(" attacks ") &
                 To_String(Source => Defender.Name));
         begin
            BaseDamage := Attacker.Attributes(Positive(Strength_Index)).Level;
            if Attacker.Equipment(WEAPON) > 0 then
               BaseDamage :=
                 BaseDamage +
                 Items_List
                   (Inventory_Container.Element
                      (Container => Attacker.Inventory,
                       Index => Attacker.Equipment(WEAPON))
                      .Proto_Index)
                   .Value
                   (2);
            end if;
         -- Count damage based on attacker wounds, fatigue, hunger and thirst
            Wounds := 1.0 - Damage_Factor(Float(Attacker.Health) / 100.0);
            Damage :=
              (BaseDamage - Integer(Float(BaseDamage) * Float(Wounds)));
            if Attacker.Thirst > 40 then
               Wounds := 1.0 - Damage_Factor(Float(Attacker.Thirst) / 100.0);
               Damage := Damage - (Integer(Float(BaseDamage) * Float(Wounds)));
            end if;
            if Attacker.Hunger > 80 then
               Wounds := 1.0 - Damage_Factor(Float(Attacker.Hunger) / 100.0);
               Damage := Damage - (Integer(Float(BaseDamage) * Float(Wounds)));
            end if;
            Damage :=
              (if PlayerAttack2 then
                 Integer
                   (Float(Damage) *
                    Float(New_Game_Settings.Player_Melee_Damage_Bonus))
               else Integer
                   (Float(Damage) *
                    Float(New_Game_Settings.Enemy_Melee_Damage_Bonus)));
            if Attacker.Equipment(WEAPON) > 0 then
               AttackSkill :=
                 Get_Skill_Level
                   (Attacker,
                    Skills_Amount_Range
                      (Items_List
                         (Inventory_Container.Element
                            (Container => Attacker.Inventory,
                             Index => Attacker.Equipment(WEAPON))
                            .Proto_Index)
                         .Value
                         .Element
                         (3)));
               HitChance := AttackSkill + Get_Random(1, 50);
            else
               HitChance :=
                 Get_Skill_Level(Attacker, Unarmed_Skill) + Get_Random(1, 50);
            end if;
            HitChance :=
              HitChance -
              (Get_Skill_Level(Defender, Dodge_Skill) + Get_Random(1, 50));
            Count_Hit_Chance_Loop :
            for I in HELMET .. LEGS loop
               if Defender.Equipment(I) > 0
                 and then
                   Items_List
                     (Inventory_Container.Element
                        (Container => Defender.Inventory,
                         Index => Defender.Equipment(I))
                        .Proto_Index)
                     .Value
                     .Length >
                   2 then
                  HitChance :=
                    HitChance +
                    Items_List
                      (Inventory_Container.Element
                         (Container => Defender.Inventory,
                          Index => Defender.Equipment(I))
                         .Proto_Index)
                      .Value
                      (3);
               end if;
            end loop Count_Hit_Chance_Loop;
            if Defender.Equipment(HitLocation) > 0 then
               Damage :=
                 Damage -
                 Items_List
                   (Inventory_Container.Element
                      (Container => Defender.Inventory,
                       Index => Defender.Equipment(HitLocation))
                      .Proto_Index)
                   .Value
                   (2);
            end if;
            if Defender.Equipment(SHIELD) > 0 then
               Damage :=
                 Damage -
                 Items_List
                   (Inventory_Container.Element
                      (Container => Defender.Inventory,
                       Index => Defender.Equipment(SHIELD))
                      .Proto_Index)
                   .Value
                   (2);
            end if;
            if Attacker.Equipment(WEAPON) = 0 then
               declare
                  DamageBonus: Natural :=
                    Get_Skill_Level(Attacker, Unarmed_Skill) / 200;
               begin
                  if DamageBonus = 0 then
                     DamageBonus := 1;
                  end if;
                  Damage := Damage + DamageBonus;
               end;
            end if;
            if Factions_List(Defender.Faction).Flags.Contains
                (To_Unbounded_String("naturalarmor")) then
               Damage := Damage / 2;
            end if;
            if
              (Factions_List(Attacker.Faction).Flags.Contains
                 (To_Unbounded_String("toxicattack")) and
               Attacker.Equipment(WEAPON) = 0) and
              not Factions_List(Defender.Faction).Flags.Contains
                (To_Unbounded_String("diseaseimmune")) then
               Damage :=
                 (if Damage * 10 < 30 then Damage * 10 else Damage + 30);
            end if;
            if Damage < 1 then
               Damage := 1;
            end if;
            -- Count damage based on damage type of weapon
            if Attacker.Equipment(WEAPON) > 0 then
               if Items_List
                   (Inventory_Container.Element
                      (Container => Attacker.Inventory,
                       Index => Attacker.Equipment(WEAPON))
                      .Proto_Index)
                   .Value
                   (5) =
                 1 then -- cutting weapon
                  Damage := Integer(Float(Damage) * 1.5);
               elsif Items_List
                   (Inventory_Container.Element
                      (Container => Attacker.Inventory,
                       Index => Attacker.Equipment(WEAPON))
                      .Proto_Index)
                   .Value
                   (5) =
                 2 then -- impale weapon
                  Damage := Damage * 2;
               end if;
            end if;
            if HitChance < 1 then
               AttackMessage :=
                 AttackMessage & To_Unbounded_String(" and misses.");
               MessageColor := (if PlayerAttack then BLUE else CYAN);
               if not PlayerAttack then
                  Gain_Exp(2, Dodge_Skill, DefenderIndex);
                  Defender.Skills := Player_Ship.Crew(DefenderIndex).Skills;
                  Defender.Attributes :=
                    Player_Ship.Crew(DefenderIndex).Attributes;
               end if;
            else
               AttackMessage :=
                 AttackMessage & To_Unbounded_String(" and hit ") &
                 LocationNames(HitLocation) & To_Unbounded_String(".");
               MessageColor := (if PlayerAttack2 then GREEN else YELLOW);
               if Attacker.Equipment(WEAPON) > 0 then
                  if PlayerAttack then
                     Damage_Item
                       (Attacker.Inventory, Attacker.Equipment(WEAPON),
                        AttackSkill, AttackerIndex, Ship => Player_Ship);
                  else
                     Damage_Item
                       (Attacker.Inventory, Attacker.Equipment(WEAPON),
                        AttackSkill, AttackerIndex, Ship => Enemy.Ship);
                  end if;
               end if;
               if Defender.Equipment(HitLocation) > 0 then
                  if PlayerAttack then
                     Damage_Item
                       (Defender.Inventory, Defender.Equipment(HitLocation), 0,
                        DefenderIndex, Ship => Enemy.Ship);
                  else
                     Damage_Item
                       (Defender.Inventory, Defender.Equipment(HitLocation), 0,
                        DefenderIndex, Ship => Player_Ship);
                  end if;
               end if;
               if PlayerAttack2 then
                  if Attacker.Equipment(WEAPON) > 0 then
                     Gain_Exp
                       (2,
                        Skills_Amount_Range
                          (Items_List
                             (Inventory_Container.Element
                                (Container => Attacker.Inventory,
                                 Index => Attacker.Equipment(WEAPON))
                                .Proto_Index)
                             .Value
                             .Element
                             (3)),
                        AttackerIndex);
                  else
                     Gain_Exp(2, Unarmed_Skill, AttackerIndex);
                  end if;
                  Attacker.Skills := Player_Ship.Crew(AttackerIndex).Skills;
                  Attacker.Attributes :=
                    Player_Ship.Crew(AttackerIndex).Attributes;
               end if;
               Defender.Health :=
                 (if Damage > Defender.Health then 0
                  else Defender.Health - Damage);
            end if;
            Add_Message(To_String(AttackMessage), COMBATMESSAGE, MessageColor);
            Attacker.Tired :=
              (if Attacker.Tired + 1 > Skill_Range'Last then Skill_Range'Last
               else Attacker.Tired + 1);
            Defender.Tired :=
              (if Defender.Tired + 1 > Skill_Range'Last then Skill_Range'Last
               else Defender.Tired + 1);
            if PlayerAttack2 then
               Player_Ship.Crew(AttackerIndex) := Attacker;
               Enemy.Ship.Crew(DefenderIndex) := Defender;
            else
               Player_Ship.Crew(DefenderIndex) := Defender;
               Enemy.Ship.Crew(AttackerIndex) := Attacker;
            end if;
            if Defender.Health = 0 then
               if PlayerAttack2 then
                  Death
                    (DefenderIndex,
                     To_String(Source => Attacker.Name) &
                     To_Unbounded_String(" blow in melee combat"),
                     Enemy.Ship);
                  Change_Boarding_Order_Loop :
                  for Order of Boarding_Orders loop
                     if Order >= DefenderIndex then
                        Order := Order - 1;
                     end if;
                  end loop Change_Boarding_Order_Loop;
                  Update_Killed_Mobs
                    (Defender,
                     To_Unbounded_String
                       (Source => To_String(Source => Faction_Name)));
                  Update_Goal
                    (KILL,
                     To_Unbounded_String
                       (Source => To_String(Source => Faction_Name)));
                  if Enemy.Ship.Crew.Length = 0 then
                     End_Combat := True;
                  end if;
               else
                  OrderIndex := 0;
                  Change_Order_Loop :
                  for I in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(I).Order = BOARDING then
                        OrderIndex := OrderIndex + 1;
                     end if;
                     if Crew_Container.To_Index(I) = DefenderIndex then
                        Boarding_Orders.Delete(Index => OrderIndex);
                        OrderIndex := OrderIndex - 1;
                        exit Change_Order_Loop;
                     end if;
                  end loop Change_Order_Loop;
                  Death
                    (DefenderIndex,
                     To_String(Source => Attacker.Name) &
                     To_Unbounded_String(" blow in melee combat"),
                     Player_Ship);
                  if DefenderIndex = 1 then -- Player is dead
                     End_Combat := True;
                  end if;
               end if;
               return False;
            else
               return True;
            end if;
         end CharacterAttack;
      begin
         AttackerIndex := Attackers.First_Index;
         OrderIndex := 1;
         Attackers_Attacks_Loop :
         while AttackerIndex <=
           Attackers.Last_Index loop -- Boarding party attacks first
            Riposte := True;
            if Attackers(AttackerIndex).Order /= BOARDING then
               goto End_Of_Attacker_Loop;
            end if;
            AttackDone := False;
            if PlayerAttack then
               exit Attackers_Attacks_Loop when OrderIndex >
                 Boarding_Orders.Last_Index;
               if Boarding_Orders(OrderIndex) in
                   Defenders.First_Index .. Defenders.Last_Index then
                  DefenderIndex := Boarding_Orders(OrderIndex);
                  Riposte :=
                    CharacterAttack
                      (AttackerIndex, DefenderIndex, PlayerAttack);
                  if not End_Combat and Riposte then
                     if Enemy.Ship.Crew(DefenderIndex).Order /= DEFEND then
                        Give_Orders
                          (Enemy.Ship, DefenderIndex, DEFEND, 0, False);
                     end if;
                     Riposte :=
                       CharacterAttack
                         (DefenderIndex, AttackerIndex, not PlayerAttack);
                  else
                     Riposte := True;
                  end if;
                  AttackDone := True;
               elsif Boarding_Orders(OrderIndex) = -1 then
                  Give_Orders(Player_Ship, AttackerIndex, REST);
                  Boarding_Orders.Delete(Index => OrderIndex);
                  OrderIndex := OrderIndex - 1;
                  AttackDone := True;
               end if;
               OrderIndex := OrderIndex + 1;
            end if;
            if not AttackDone then
               Defenders_Riposte_Loop :
               for Defender in
                 Defenders.First_Index .. Defenders.Last_Index loop
                  if Defenders(Defender).Order = DEFEND then
                     Riposte :=
                       CharacterAttack(AttackerIndex, Defender, PlayerAttack);
                     if not End_Combat and Riposte then
                        Riposte :=
                          CharacterAttack
                            (Defender, AttackerIndex, not PlayerAttack);
                     else
                        Riposte := True;
                     end if;
                     AttackDone := True;
                     exit Defenders_Riposte_Loop;
                  end if;
               end loop Defenders_Riposte_Loop;
            end if;
            if not AttackDone then
               DefenderIndex :=
                 Get_Random(Defenders.First_Index, Defenders.Last_Index);
               if PlayerAttack then
                  Give_Orders(Enemy.Ship, DefenderIndex, DEFEND, 0, False);
               else
                  Give_Orders(Player_Ship, DefenderIndex, DEFEND, 0, False);
               end if;
               Riposte :=
                 CharacterAttack
                   (AttackerIndex => AttackerIndex,
                    DefenderIndex => DefenderIndex,
                    PlayerAttack2 => PlayerAttack);
               if not End_Combat and Riposte then
                  Riposte :=
                    CharacterAttack
                      (AttackerIndex => DefenderIndex,
                       DefenderIndex => AttackerIndex,
                       PlayerAttack2 => not PlayerAttack);
               else
                  Riposte := True;
               end if;
            end if;
            <<End_Of_Attacker_Loop>>
            exit Attackers_Attacks_Loop when End_Combat;
            if Riposte then
               AttackerIndex := AttackerIndex + 1;
            end if;
         end loop Attackers_Attacks_Loop;
         DefenderIndex := Defenders.First_Index;
         Defenders_Attacks_Loop :
         while DefenderIndex <= Defenders.Last_Index loop -- Defenders attacks
            Riposte := True;
            if Defenders(DefenderIndex).Order = DEFEND then
               Attackers_Riposte_Loop :
               for Attacker in
                 Attackers.First_Index .. Attackers.Last_Index loop
                  if Attackers(Attacker).Order = BOARDING then
                     Riposte :=
                       CharacterAttack
                         (DefenderIndex, Attacker, not PlayerAttack);
                     if not End_Combat and Riposte then
                        Riposte :=
                          CharacterAttack
                            (Attacker, DefenderIndex, PlayerAttack);
                     end if;
                     exit Attackers_Riposte_Loop;
                  end if;
               end loop Attackers_Riposte_Loop;
            end if;
            if Riposte then
               DefenderIndex := DefenderIndex + 1;
            end if;
         end loop Defenders_Attacks_Loop;
         if Find_Member(BOARDING) = 0 then
            Update_Orders(Enemy.Ship);
         end if;
      end MeleeCombat;
   begin
      if Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type) =
        0 then
         Add_Message
           ("Ship fall from sky due to lack of fuel.", OTHERMESSAGE, RED);
         Death(1, To_Unbounded_String("fall of the ship"), Player_Ship);
         End_Combat := True;
         return;
      end if;
      declare
         ChanceForRun: Integer;
      begin
         Turn_Number := Turn_Number + 1;
         case Enemy.Combat_Ai is
            when ATTACKER =>
               ChanceForRun := Turn_Number - 120;
            when BERSERKER =>
               ChanceForRun := Turn_Number - 200;
            when DISARMER =>
               ChanceForRun := Turn_Number - 60;
            when others =>
               null;
         end case;
         if ChanceForRun > 1 and then Get_Random(1, 100) < ChanceForRun then
            Enemy.Combat_Ai := COWARD;
         end if;
      end;
      Pilot_Engineer_Experience_Loop :
      for I in Player_Ship.Crew.Iterate loop
         case Player_Ship.Crew(I).Order is
            when PILOT =>
               Pilot_Index := Crew_Container.To_Index(I);
               Gain_Exp(2, Piloting_Skill, Pilot_Index);
            when ENGINEER =>
               Engineer_Index := Crew_Container.To_Index(I);
               Gain_Exp(2, Engineering_Skill, Engineer_Index);
            when others =>
               null;
         end case;
      end loop Pilot_Engineer_Experience_Loop;
      if Pilot_Index > 0 then
         case Pilot_Order is
            when 1 =>
               Accuracy_Bonus := 20;
               Evade_Bonus := -10;
            when 2 =>
               Accuracy_Bonus := 10;
               Evade_Bonus := 0;
            when 3 =>
               Accuracy_Bonus := 0;
               Evade_Bonus := 10;
            when 4 =>
               Accuracy_Bonus := -10;
               Evade_Bonus := 20;
            when others =>
               null;
         end case;
         Evade_Bonus :=
           Evade_Bonus +
           Get_Skill_Level(Player_Ship.Crew(Pilot_Index), Piloting_Skill);
      else
         Accuracy_Bonus := 20;
         Evade_Bonus := -10;
      end if;
      Enemy_Pilot_Index := Find_Member(PILOT, Enemy.Ship.Crew);
      if Enemy_Pilot_Index > 0 then
         Accuracy_Bonus :=
           Accuracy_Bonus -
           Get_Skill_Level(Enemy.Ship.Crew(Enemy_Pilot_Index), Piloting_Skill);
      end if;
      if Engineer_Index > 0 or
        Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) then
         Message :=
           To_Unbounded_String
             (ChangeShipSpeed(Ship_Speed'Val(Engineer_Order)));
         if Length(Message) > 0 then
            Add_Message(To_String(Message), ORDERMESSAGE, RED);
         end if;
      end if;
      Speed_Bonus := 20 - (RealSpeed(Player_Ship) / 100);
      if Speed_Bonus < -10 then
         Speed_Bonus := -10;
      end if;
      Accuracy_Bonus := Accuracy_Bonus + Speed_Bonus;
      Evade_Bonus := Evade_Bonus - Speed_Bonus;
      Enemy_Weapon_Loop :
      for I in Enemy.Ship.Modules.Iterate loop
         if Enemy.Ship.Modules(I).Durability = 0 or
           (Enemy.Ship.Modules(I).M_Type not in GUN | BATTERING_RAM |
                HARPOON_GUN) then
            goto End_Of_Enemy_Weapon_Loop;
         end if;
         if Enemy.Ship.Modules(I).M_Type in GUN | HARPOON_GUN then
            if Enemy.Ship.Modules(I).M_Type = GUN and Damage_Range > 5_000 then
               Damage_Range := 5_000;
            elsif Damage_Range > 2_000 then
               Damage_Range := 2_000;
            end if;
            Ammo_Index_2 :=
              (if Enemy.Ship.Modules(I).M_Type = GUN then
                 Enemy.Ship.Modules(I).Ammo_Index
               else Enemy.Ship.Modules(I).Harpoon_Index);
            if Ammo_Index_2 in
                Inventory_Container.First_Index
                      (Container => Enemy.Ship.Cargo) ..
                      Inventory_Container.Last_Index
                        (Container => Enemy.Ship.Cargo) then
               if Items_List
                   (Inventory_Container.Element
                      (Container => Enemy.Ship.Cargo, Index => Ammo_Index_2)
                      .Proto_Index)
                   .I_Type =
                 Items_Types
                   (Modules_List(Enemy.Ship.Modules(I).Proto_Index).Value) then
                  Enemy_Ammo_Index := Ammo_Index_2;
               end if;
            end if;
            if Enemy_Ammo_Index = 0 then
               Enemy_Ammo_Index_Loop :
               for K in Items_List.Iterate loop
                  if Items_List(K).I_Type =
                    Items_Types
                      (Modules_List(Enemy.Ship.Modules(I).Proto_Index)
                         .Value) then
                     Find_Enemy_Ammo_Index_Loop :
                     for J in
                       Inventory_Container.First_Index
                         (Container => Enemy.Ship.Cargo) ..
                         Inventory_Container.Last_Index
                           (Container => Enemy.Ship.Cargo) loop
                        if Inventory_Container.Element
                            (Container => Enemy.Ship.Cargo, Index => J)
                            .Proto_Index =
                          Objects_Container.Key(K) then
                           Enemy_Ammo_Index := J;
                           exit Find_Enemy_Ammo_Index_Loop;
                        end if;
                     end loop Find_Enemy_Ammo_Index_Loop;
                     exit Enemy_Ammo_Index_Loop when Enemy_Ammo_Index > 0;
                  end if;
               end loop Enemy_Ammo_Index_Loop;
            end if;
            if Enemy_Ammo_Index = 0 and
              (Enemy.Combat_Ai in ATTACKER | DISARMER) then
               Enemy.Combat_Ai := COWARD;
               exit Enemy_Weapon_Loop;
            end if;
         elsif Damage_Range > 100 then
            Damage_Range := 100;
         end if;
         Enemy_Weapon_Index := Modules_Container.To_Index(I);
         <<End_Of_Enemy_Weapon_Loop>>
      end loop Enemy_Weapon_Loop;
      if Enemy_Weapon_Index = 0 and
        (Enemy.Combat_Ai in ATTACKER | DISARMER) then
         Enemy.Combat_Ai := COWARD;
      end if;
      case Enemy.Combat_Ai is
         when BERSERKER =>
            if Enemy.Distance > 10 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) + 1);
               Add_Message
                 (To_String(Enemy_Name) & " increases speed.", COMBATMESSAGE);
               Enemy_Pilot_Order := 1;
            elsif Enemy.Distance <= 10 and Enemy.Ship.Speed = FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) - 1);
               Add_Message
                 (To_String(Enemy_Name) & " decreases speed.", COMBATMESSAGE);
               Enemy_Pilot_Order := 2;
            end if;
         when ATTACKER | DISARMER =>
            if Enemy.Distance > Damage_Range and
              Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) + 1);
               Add_Message
                 (To_String(Enemy_Name) & " increases speed.", COMBATMESSAGE);
               Enemy_Pilot_Order := 1;
            elsif Enemy.Distance < Damage_Range and
              Enemy.Ship.Speed > QUARTER_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) - 1);
               Add_Message
                 (To_String(Enemy_Name) & " decreases speed.", COMBATMESSAGE);
               Enemy_Pilot_Order := 2;
            end if;
         when COWARD =>
            if Enemy.Distance < 15_000 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) + 1);
               Add_Message
                 (To_String(Enemy_Name) & " increases speed.", COMBATMESSAGE);
            end if;
            Enemy_Pilot_Order := 4;
         when others =>
            null;
      end case;
      if Enemy.Harpoon_Duration > 0 then
         Enemy.Ship.Speed := FULL_STOP;
         Add_Message
           (To_String(Enemy_Name) & " is stopped by your ship.",
            COMBATMESSAGE);
      elsif Enemy.Ship.Speed = FULL_STOP then
         Enemy.Ship.Speed := QUARTER_SPEED;
      end if;
      if Harpoon_Duration > 0 then
         Player_Ship.Speed := FULL_STOP;
         Add_Message("You are stopped by enemy ship.", COMBATMESSAGE);
      end if;
      case Enemy_Pilot_Order is
         when 1 =>
            Accuracy_Bonus := Accuracy_Bonus + 20;
            Evade_Bonus := Evade_Bonus - 20;
         when 2 =>
            Accuracy_Bonus := Accuracy_Bonus + 10;
            Evade_Bonus := Evade_Bonus - 10;
         when 3 =>
            Accuracy_Bonus := Accuracy_Bonus - 10;
            Evade_Bonus := Evade_Bonus + 10;
         when 4 =>
            Accuracy_Bonus := Accuracy_Bonus - 20;
            Evade_Bonus := Evade_Bonus + 20;
         when others =>
            null;
      end case;
      Speed_Bonus := 20 - (RealSpeed(Enemy.Ship) / 100);
      if Speed_Bonus < -10 then
         Speed_Bonus := -10;
      end if;
      Accuracy_Bonus := Accuracy_Bonus + Speed_Bonus;
      Evade_Bonus := Evade_Bonus - Speed_Bonus;
      Distance_Traveled :=
        (if Enemy_Pilot_Order < 4 then -(RealSpeed(Enemy.Ship))
         else RealSpeed(Enemy.Ship));
      if Pilot_Index > 0 then
         case Pilot_Order is
            when 1 | 3 =>
               Distance_Traveled := Distance_Traveled - RealSpeed(Player_Ship);
            when 2 =>
               Distance_Traveled := Distance_Traveled + RealSpeed(Player_Ship);
               if Distance_Traveled > 0 and Enemy_Pilot_Order /= 4 then
                  Distance_Traveled := 0;
               end if;
            when 4 =>
               Distance_Traveled := Distance_Traveled + RealSpeed(Player_Ship);
            when others =>
               null;
         end case;
      else
         Distance_Traveled := Distance_Traveled - RealSpeed(Player_Ship);
      end if;
      Enemy.Distance := Enemy.Distance + Distance_Traveled;
      if Enemy.Distance < 10 then
         Enemy.Distance := 10;
      end if;
      if Enemy.Distance >= 15_000 then
         if Pilot_Order = 4 then
            Add_Message
              ("You escaped the " & To_String(Enemy_Name) & ".",
               COMBATMESSAGE);
         else
            Add_Message
              (To_String(Enemy_Name) & " escaped from you.", COMBATMESSAGE);
         end if;
         Kill_Boarding_Party_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Order = BOARDING then
               Death
                 (Crew_Container.To_Index(I),
                  To_Unbounded_String("enemy crew"), Player_Ship, False);
            end if;
         end loop Kill_Boarding_Party_Loop;
         End_Combat := True;
         return;
      elsif Enemy.Distance < 15_000 and Enemy.Distance >= 10_000 then
         Accuracy_Bonus := Accuracy_Bonus - 10;
         Evade_Bonus := Evade_Bonus + 10;
         Log_Message("Distance: long", Log.COMBAT);
      elsif Enemy.Distance < 5_000 and Enemy.Distance >= 1_000 then
         Accuracy_Bonus := Accuracy_Bonus + 10;
         Log_Message("Distance: medium", Log.COMBAT);
      elsif Enemy.Distance < 1_000 then
         Accuracy_Bonus := Accuracy_Bonus + 20;
         Evade_Bonus := Evade_Bonus - 10;
         Log_Message("Distance: short or close", Log.COMBAT);
      end if;
      Attack(Player_Ship, Enemy.Ship); -- Player attack
      if not End_Combat then
         Attack(Enemy.Ship, Player_Ship); -- Enemy attack
      end if;
      if not End_Combat then
         declare
            HaveBoardingParty: Boolean := False;
         begin
            Check_For_Boarding_Party_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Order = BOARDING then
                  HaveBoardingParty := True;
                  exit Check_For_Boarding_Party_Loop;
               end if;
            end loop Check_For_Boarding_Party_Loop;
            Check_For_Enemy_Boarding_Party :
            for Member of Enemy.Ship.Crew loop
               if Member.Order = BOARDING then
                  HaveBoardingParty := True;
                  exit Check_For_Enemy_Boarding_Party;
               end if;
            end loop Check_For_Enemy_Boarding_Party;
            if Enemy.Harpoon_Duration > 0 or Harpoon_Duration > 0 or
              HaveBoardingParty then
               if not End_Combat and
                 Enemy.Ship.Crew.Length >
                   0 then -- Characters combat (player boarding party)
                  MeleeCombat(Player_Ship.Crew, Enemy.Ship.Crew, True);
               end if;
               if not End_Combat and
                 Enemy.Ship.Crew.Length >
                   0 then -- Characters combat (enemy boarding party)
                  MeleeCombat(Enemy.Ship.Crew, Player_Ship.Crew, False);
               end if;
            end if;
         end;
      end if;
      if not End_Combat then
         if Enemy.Harpoon_Duration > 0 then
            Enemy.Harpoon_Duration := Enemy.Harpoon_Duration - 1;
         end if;
         if Harpoon_Duration > 0 then
            Harpoon_Duration := Harpoon_Duration - 1;
         end if;
         if Enemy.Harpoon_Duration > 0 or
           Harpoon_Duration >
             0 then -- Set defenders/boarding party on player ship
            Update_Orders(Player_Ship, True);
         end if;
         Update_Game(1, True);
      elsif Player_Ship.Crew(1).Health > 0 then
         declare
            WasBoarded: Boolean := False;
            LootAmount: Integer;
         begin
            if Find_Member(BOARDING) > 0 then
               WasBoarded := True;
            end if;
            Enemy.Ship.Modules(1).Durability := 0;
            Add_Message
              (To_String(Enemy_Name) & " is destroyed!", COMBATMESSAGE);
            LootAmount := Enemy.Loot;
            Ship_Free_Space := FreeCargo((0 - LootAmount));
            if Ship_Free_Space < 0 then
               LootAmount := LootAmount + Ship_Free_Space;
            end if;
            if LootAmount > 0 then
               Add_Message
                 ("You looted" & Integer'Image(LootAmount) & " " &
                  To_String(Money_Name) & " from " & To_String(Enemy_Name) &
                  ".",
                  COMBATMESSAGE);
               UpdateCargo(Player_Ship, Money_Index, LootAmount);
            end if;
            Ship_Free_Space := FreeCargo(0);
            if WasBoarded and Ship_Free_Space > 0 then
               Message :=
                 To_Unbounded_String
                   ("Additionally, your boarding party takes from ") &
                 Enemy_Name & To_Unbounded_String(":");
               Looting_Loop :
               for Item of Enemy.Ship.Cargo loop
                  LootAmount := Item.Amount / 5;
                  Ship_Free_Space := FreeCargo((0 - LootAmount));
                  if Ship_Free_Space < 0 then
                     LootAmount := LootAmount + Ship_Free_Space;
                  end if;
                  if Items_List(Item.Proto_Index).Price = 0 and
                    Item.Proto_Index /= Money_Index then
                     LootAmount := 0;
                  end if;
                  if LootAmount > 0 then
                     if Item /=
                       Inventory_Container.First_Element
                         (Container => Enemy.Ship.Cargo) then
                        Message := Message & To_Unbounded_String(",");
                     end if;
                     UpdateCargo(Player_Ship, Item.Proto_Index, LootAmount);
                     Message :=
                       Message & Positive'Image(LootAmount) &
                       To_Unbounded_String(" ") &
                       Items_List(Item.Proto_Index).Name;
                     Ship_Free_Space := FreeCargo(0);
                     exit Looting_Loop when Item =
                       Inventory_Container.Last_Element
                         (Container => Enemy.Ship.Cargo) or
                       Ship_Free_Space = 0;
                  end if;
               end loop Looting_Loop;
               Add_Message(To_String(Message) & ".", COMBATMESSAGE);
               if Current_Story.Index /= Null_Unbounded_String then
                  declare
                     Step: constant Step_Data :=
                       (if Current_Story.Current_Step = 0 then
                          Stories_List(Current_Story.Index).Starting_Step
                        elsif Current_Story.Current_Step > 0 then
                          Stories_List(Current_Story.Index).Steps
                            (Current_Story.Current_Step)
                        else Stories_List(Current_Story.Index).Final_Step);
                     Tokens: Slice_Set;
                  begin
                     if Step.Finish_Condition = LOOT then
                        Create(Tokens, To_String(Current_Story.Data), ";");
                        if Slice(Tokens, 2) = "any" or
                          Slice(Tokens, 2) = To_String(Enemy_Ship_Index) then
                           if Progress_Story then
                              case Step.Finish_Condition is
                                 when LOOT =>
                                    UpdateCargo
                                      (Player_Ship,
                                       To_Bounded_String(Slice(Tokens, 1)), 1);
                                 when others =>
                                    null;
                              end case;
                           end if;
                        end if;
                     end if;
                  end;
               else
                  Start_Story(Faction_Name, DROPITEM);
               end if;
            end if;
            Give_Orders_Loop :
            for I in Player_Ship.Crew.Iterate loop
               if Player_Ship.Crew(I).Order = BOARDING then
                  Give_Orders(Player_Ship, Crew_Container.To_Index(I), REST);
               elsif Player_Ship.Crew(I).Order = DEFEND then
                  Give_Orders(Player_Ship, Crew_Container.To_Index(I), REST);
               end if;
            end loop Give_Orders_Loop;
         end;
         Enemy.Ship.Speed := FULL_STOP;
         Player_Ship.Speed := Old_Speed;
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
            if Events_List
                (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                .E_Type =
              ATTACKONBASE then
               Gain_Rep
                 (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index, 5);
            end if;
            Delete_Event
              (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index);
         end if;
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index > 0
           and then
             Accepted_Missions
               (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index)
               .M_Type =
             DESTROY
           and then
             Proto_Ships_List
               (Accepted_Missions
                  (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index)
                  .Ship_Index)
               .Name =
             Enemy.Ship.Name then
            Update_Mission
              (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index);
         end if;
         declare
            LostReputationChance: Positive range 10 .. 40 := 10;
         begin
            if Proto_Ships_List(Enemy_Ship_Index).Owner =
              Player_Ship.Crew(1).Faction then
               LostReputationChance := 40;
            end if;
            if Get_Random(1, 100) < LostReputationChance then
               Gain_Rep(Enemy.Ship.Home_Base, -100);
            end if;
         end;
         Update_Destroyed_Ships(Enemy.Ship.Name);
         Update_Goal(DESTROY, Enemy_Ship_Index);
         if Current_Goal.Target_Index /= Null_Unbounded_String then
            Update_Goal
              (DESTROY,
               To_Unbounded_String
                 (Source =>
                    To_String
                      (Source => Proto_Ships_List(Enemy_Ship_Index).Owner)));
         end if;
         if Current_Story.Index /= Null_Unbounded_String then
            declare
               FinishCondition: constant Step_Condition_Type :=
                 (if Current_Story.Current_Step = 0 then
                    Stories_List(Current_Story.Index).Starting_Step
                      .Finish_Condition
                  elsif Current_Story.Current_Step > 0 then
                    Stories_List(Current_Story.Index).Steps
                      (Current_Story.Current_Step)
                      .Finish_Condition
                  else Stories_List(Current_Story.Index).Final_Step
                      .Finish_Condition);
               Tokens: Slice_Set;
            begin
               if FinishCondition /= DESTROYSHIP then
                  return;
               end if;
               Create(Tokens, To_String(Current_Story.Data), ";");
               if Player_Ship.Sky_X = Positive'Value(Slice(Tokens, 1)) and
                 Player_Ship.Sky_Y = Positive'Value(Slice(Tokens, 2)) and
                 Enemy_Ship_Index = To_Unbounded_String(Slice(Tokens, 3)) then
                  if not Progress_Story(True) then
                     return;
                  end if;
               end if;
            end;
         end if;
      end if;
   end Combat_Turn;

end Combat;
