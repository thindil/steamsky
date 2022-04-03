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
        To_Unbounded_String
          (Source =>
             To_String
               (Source =>
                  Generate_Ship_Name
                    (Owner => Proto_Ships_List(Enemy_Index).Owner)));
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
                        if Gun(1) =
                          Modules_Container.To_Index(Position => K) then
                           Gunner_Order := Gun(2);
                           if Gun(3) > 0 then
                              Shoots := Gun(3);
                              if Gunner_Order /= 3 then
                                 Shoots :=
                                   Natural(Float'Ceiling(Float(Shoots) / 2.0));
                              end if;
                              Log_Message
                                (Message =>
                                   "Player Shoots (no cooldown):" &
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
                                (Message =>
                                   "Player Shoots (after cooldown):" &
                                   Natural'Image(Shoots),
                                 Message_Type => Log.COMBAT);
                           end if;
                           exit Count_Player_Shoots_Loop;
                        end if;
                     end loop Count_Player_Shoots_Loop;
                     Log_Message
                       (Message => "Shoots test3:" & Natural'Image(Shoots),
                        Message_Type => Log.COMBAT);
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
                       (Message =>
                          "You don't have ammo to " &
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
            Log_Message
              (Message => "Shoots:" & Integer'Image(Shoots),
               Message_Type => Log.COMBAT);
            if Shoots > 0 then
               Hit_Chance :=
                 (if Ship = Player_Ship then
                    Current_Accuracy_Bonus - Enemy.Evasion
                  else Enemy.Accuracy - Evade_Bonus);
               if Gunner_Index > 0 then
                  Hit_Chance :=
                    Hit_Chance +
                    Get_Skill_Level
                      (Member => Ship.Crew(Gunner_Index),
                       Skill_Index => Gunnery_Skill);
               end if;
               if Hit_Chance < -48 then
                  Hit_Chance := -48;
               end if;
               Log_Message
                 (Message =>
                    "Player Accuracy:" &
                    Integer'Image(Current_Accuracy_Bonus) &
                    " Player Evasion:" & Integer'Image(Evade_Bonus),
                  Message_Type => Log.COMBAT);
               Log_Message
                 (Message =>
                    "Enemy Evasion:" & Integer'Image(Enemy.Evasion) &
                    " Enemy Accuracy:" & Integer'Image(Enemy.Accuracy),
                  Message_Type => Log.COMBAT);
               Log_Message
                 (Message => "Chance to hit:" & Integer'Image(Hit_Chance),
                  Message_Type => Log.COMBAT);
               Shooting_Loop :
               for I in 1 .. Shoots loop
                  if Ship = Player_Ship then
                     Shoot_Message :=
                       (if Ship.Modules(K).M_Type in GUN | HARPOON_GUN then
                          To_String(Source => Ship.Crew(Gunner_Index).Name) &
                          To_Unbounded_String(Source => " shoots at ") &
                          Enemy_Name_Owner
                        else To_Unbounded_String(Source => "You ram ") &
                          Enemy_Name_Owner);
                  else
                     Shoot_Message :=
                       Enemy_Name_Owner &
                       To_Unbounded_String(Source => " attacks");
                  end if;
                  if Hit_Chance + Get_Random(Min => 1, Max => 50) >
                    Get_Random(Min => 1, Max => Hit_Chance + 50) then
                     Shoot_Message :=
                       Shoot_Message &
                       To_Unbounded_String(Source => " and hits ");
                     Armor_Index := Find_Enemy_Module(M_Type => ARMOR);
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
                                    Hit_Location :=
                                      Find_Enemy_Module(M_Type => ENGINE);
                                 when 5 =>
                                    Hit_Location := 0;
                                    Find_Hit_Weapon;
                                    if Hit_Location = 0 then
                                       Hit_Location :=
                                         Find_Enemy_Module
                                           (M_Type => BATTERING_RAM);
                                    end if;
                                 when 6 =>
                                    Hit_Location :=
                                      Find_Enemy_Module(M_Type => HULL);
                                 when others =>
                                    Hit_Location := 1;
                              end case;
                              if Hit_Location = 0 then
                                 Hit_Location := 1;
                              end if;
                           else
                              Hit_Location :=
                                Get_Random
                                  (Min => Enemy.Ship.Modules.First_Index,
                                   Max => Enemy.Ship.Modules.Last_Index);
                           end if;
                        else
                           if Enemy.Combat_Ai = DISARMER then
                              Hit_Location := 1;
                              Find_Hit_Weapon;
                           else
                              Hit_Location :=
                                Get_Random
                                  (Min => Player_Ship.Modules.First_Index,
                                   Max => Player_Ship.Modules.Last_Index);
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
                       To_Unbounded_String(Source => ".");
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
                              (Count_Ship_Weight(Ship => Ship) / 5_000))
                           else Weapon_Damage +
                             (Count_Ship_Weight(Ship => Ship) / 5_000));
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
                       (Ship => Enemy_Ship, Module_Index => Hit_Location,
                        Damage => Weapon_Damage,
                        Death_Reason => "enemy fire in ship combat");
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
                                    Remove_Gun(Module_Index => Weapon_Index);
                                 end if;
                              end if;
                           when GUN =>
                              if Enemy_Ship = Player_Ship then
                                 Remove_Gun(Module_Index => Hit_Location);
                              end if;
                           when others =>
                              null;
                        end case;
                     end if;
                     if Ship = Player_Ship then
                        Add_Message
                          (Message => To_String(Source => Shoot_Message),
                           M_Type => COMBATMESSAGE, Color => GREEN);
                     else
                        Add_Message
                          (Message => To_String(Source => Shoot_Message),
                           M_Type => COMBATMESSAGE, Color => YELLOW);
                     end if;
                  else
                     Shoot_Message :=
                       Shoot_Message &
                       To_Unbounded_String(Source => " and misses.");
                     if Ship = Player_Ship then
                        Add_Message
                          (Message => To_String(Source => Shoot_Message),
                           M_Type => COMBATMESSAGE, Color => BLUE);
                     else
                        Add_Message
                          (Message => To_String(Source => Shoot_Message),
                           M_Type => COMBATMESSAGE, Color => CYAN);
                     end if;
                  end if;
                  if Ammo_Index > 0 then
                     UpdateCargo
                       (Ship => Ship, CargoIndex => Ammo_Index, Amount => -1);
                  end if;
                  if Ship = Player_Ship and Gunner_Index > 0 then
                     Gain_Exp
                       (Amount => 2, Skill_Number => Gunnery_Skill,
                        Crew_Index => Gunner_Index);
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
      procedure Melee_Combat
        (Attackers, Defenders: in out Crew_Container.Vector;
         Player_Attack: Boolean) is
         Attack_Done, Riposte: Boolean;
         Attacker_Index, Defender_Index: Positive;
         Order_Index: Natural;
         function Character_Attack
           (Attacker_Index_2, Defender_Index_2: Positive;
            Player_Attack_2: Boolean) return Boolean is
            Hit_Chance, Damage: Integer;
            Hit_Location: constant Equipment_Locations :=
              Equipment_Locations'Val
                (Get_Random
                   (Min => Equipment_Locations'Pos(HELMET),
                    Max => Equipment_Locations'Pos(LEGS)));
            Location_Names: constant array
              (HELMET .. LEGS) of Unbounded_String :=
              (HELMET => To_Unbounded_String(Source => "head"),
               TORSO => To_Unbounded_String(Source => "torso"),
               ARMS => To_Unbounded_String(Source => "arm"),
               LEGS => To_Unbounded_String(Source => "leg"));
            Attack_Skill, Base_Damage: Natural;
            Wounds: Damage_Factor := 0.0;
            Messages_Color: Message_Color;
            Attacker: Member_Data :=
              (if Player_Attack_2 then Player_Ship.Crew(Attacker_Index_2)
               else Enemy.Ship.Crew(Attacker_Index_2));
            Defender: Member_Data :=
              (if Player_Attack_2 then Enemy.Ship.Crew(Defender_Index_2)
               else Player_Ship.Crew(Defender_Index_2));
            Attack_Message: Unbounded_String :=
              (if Player_Attack_2 then
                 To_String(Source => Attacker.Name) &
                 To_Unbounded_String(Source => " attacks ") &
                 To_String(Source => Defender.Name) &
                 To_Unbounded_String(Source => " (") &
                 To_String(Source => Faction_Name) &
                 To_Unbounded_String(Source => ")")
               else To_String(Source => Attacker.Name) &
                 To_Unbounded_String(Source => " (") &
                 To_String(Source => Faction_Name) &
                 To_Unbounded_String(Source => ")") &
                 To_Unbounded_String(Source => " attacks ") &
                 To_String(Source => Defender.Name));
         begin
            Base_Damage := Attacker.Attributes(Positive(Strength_Index)).Level;
            if Attacker.Equipment(WEAPON) > 0 then
               Base_Damage :=
                 Base_Damage +
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
              (Base_Damage - Integer(Float(Base_Damage) * Float(Wounds)));
            if Attacker.Thirst > 40 then
               Wounds := 1.0 - Damage_Factor(Float(Attacker.Thirst) / 100.0);
               Damage :=
                 Damage - (Integer(Float(Base_Damage) * Float(Wounds)));
            end if;
            if Attacker.Hunger > 80 then
               Wounds := 1.0 - Damage_Factor(Float(Attacker.Hunger) / 100.0);
               Damage :=
                 Damage - (Integer(Float(Base_Damage) * Float(Wounds)));
            end if;
            Damage :=
              (if Player_Attack_2 then
                 Integer
                   (Float(Damage) *
                    Float(New_Game_Settings.Player_Melee_Damage_Bonus))
               else Integer
                   (Float(Damage) *
                    Float(New_Game_Settings.Enemy_Melee_Damage_Bonus)));
            if Attacker.Equipment(WEAPON) > 0 then
               Attack_Skill :=
                 Get_Skill_Level
                   (Member => Attacker,
                    Skill_Index =>
                      Skills_Amount_Range
                        (Items_List
                           (Inventory_Container.Element
                              (Container => Attacker.Inventory,
                               Index => Attacker.Equipment(WEAPON))
                              .Proto_Index)
                           .Value
                           .Element
                           (Index => 3)));
               Hit_Chance := Attack_Skill + Get_Random(Min => 1, Max => 50);
            else
               Hit_Chance :=
                 Get_Skill_Level
                   (Member => Attacker, Skill_Index => Unarmed_Skill) +
                 Get_Random(Min => 1, Max => 50);
            end if;
            Hit_Chance :=
              Hit_Chance -
              (Get_Skill_Level
                 (Member => Defender, Skill_Index => Dodge_Skill) +
               Get_Random(Min => 1, Max => 50));
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
                  Hit_Chance :=
                    Hit_Chance +
                    Items_List
                      (Inventory_Container.Element
                         (Container => Defender.Inventory,
                          Index => Defender.Equipment(I))
                         .Proto_Index)
                      .Value
                      (3);
               end if;
            end loop Count_Hit_Chance_Loop;
            if Defender.Equipment(Hit_Location) > 0 then
               Damage :=
                 Damage -
                 Items_List
                   (Inventory_Container.Element
                      (Container => Defender.Inventory,
                       Index => Defender.Equipment(Hit_Location))
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
               Count_Damage_Bonus_Block :
               declare
                  Damage_Bonus: Natural :=
                    Get_Skill_Level
                      (Member => Attacker, Skill_Index => Unarmed_Skill) /
                    200;
               begin
                  if Damage_Bonus = 0 then
                     Damage_Bonus := 1;
                  end if;
                  Damage := Damage + Damage_Bonus;
               end Count_Damage_Bonus_Block;
            end if;
            if Factions_List(Defender.Faction).Flags.Contains
                (Item => To_Unbounded_String(Source => "naturalarmor")) then
               Damage := Damage / 2;
            end if;
            if
              (Factions_List(Attacker.Faction).Flags.Contains
                 (Item => To_Unbounded_String(Source => "toxicattack")) and
               Attacker.Equipment(WEAPON) = 0) and
              not Factions_List(Defender.Faction).Flags.Contains
                (Item => To_Unbounded_String(Source => "diseaseimmune")) then
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
            if Hit_Chance < 1 then
               Attack_Message :=
                 Attack_Message &
                 To_Unbounded_String(Source => " and misses.");
               Messages_Color := (if Player_Attack then BLUE else CYAN);
               if not Player_Attack then
                  Gain_Exp
                    (Amount => 2, Skill_Number => Dodge_Skill,
                     Crew_Index => Defender_Index_2);
                  Defender.Skills := Player_Ship.Crew(Defender_Index_2).Skills;
                  Defender.Attributes :=
                    Player_Ship.Crew(Defender_Index_2).Attributes;
               end if;
            else
               Attack_Message :=
                 Attack_Message & To_Unbounded_String(Source => " and hit ") &
                 Location_Names(Hit_Location) &
                 To_Unbounded_String(Source => ".");
               Messages_Color := (if Player_Attack_2 then GREEN else YELLOW);
               if Attacker.Equipment(WEAPON) > 0 then
                  if Player_Attack then
                     Damage_Item
                       (Inventory => Attacker.Inventory,
                        Item_Index => Attacker.Equipment(WEAPON),
                        Skill_Level => Attack_Skill,
                        Member_Index => Attacker_Index_2, Ship => Player_Ship);
                  else
                     Damage_Item
                       (Inventory => Attacker.Inventory,
                        Item_Index => Attacker.Equipment(WEAPON),
                        Skill_Level => Attack_Skill,
                        Member_Index => Attacker_Index_2, Ship => Enemy.Ship);
                  end if;
               end if;
               if Defender.Equipment(Hit_Location) > 0 then
                  if Player_Attack then
                     Damage_Item
                       (Inventory => Defender.Inventory,
                        Item_Index => Defender.Equipment(Hit_Location),
                        Skill_Level => 0, Member_Index => Defender_Index_2,
                        Ship => Enemy.Ship);
                  else
                     Damage_Item
                       (Inventory => Defender.Inventory,
                        Item_Index => Defender.Equipment(Hit_Location),
                        Skill_Level => 0, Member_Index => Defender_Index_2,
                        Ship => Player_Ship);
                  end if;
               end if;
               if Player_Attack_2 then
                  if Attacker.Equipment(WEAPON) > 0 then
                     Gain_Exp
                       (Amount => 2,
                        Skill_Number =>
                          Skills_Amount_Range
                            (Items_List
                               (Inventory_Container.Element
                                  (Container => Attacker.Inventory,
                                   Index => Attacker.Equipment(WEAPON))
                                  .Proto_Index)
                               .Value
                               .Element
                               (Index => 3)),
                        Crew_Index => Attacker_Index_2);
                  else
                     Gain_Exp
                       (Amount => 2, Skill_Number => Unarmed_Skill,
                        Crew_Index => Attacker_Index_2);
                  end if;
                  Attacker.Skills := Player_Ship.Crew(Attacker_Index_2).Skills;
                  Attacker.Attributes :=
                    Player_Ship.Crew(Attacker_Index_2).Attributes;
               end if;
               Defender.Health :=
                 (if Damage > Defender.Health then 0
                  else Defender.Health - Damage);
            end if;
            Add_Message
              (Message => To_String(Source => Attack_Message),
               M_Type => COMBATMESSAGE, Color => Messages_Color);
            Attacker.Tired :=
              (if Attacker.Tired + 1 > Skill_Range'Last then Skill_Range'Last
               else Attacker.Tired + 1);
            Defender.Tired :=
              (if Defender.Tired + 1 > Skill_Range'Last then Skill_Range'Last
               else Defender.Tired + 1);
            if Player_Attack_2 then
               Player_Ship.Crew(Attacker_Index_2) := Attacker;
               Enemy.Ship.Crew(Defender_Index_2) := Defender;
            else
               Player_Ship.Crew(Defender_Index_2) := Defender;
               Enemy.Ship.Crew(Attacker_Index_2) := Attacker;
            end if;
            if Defender.Health = 0 then
               if Player_Attack_2 then
                  Death
                    (Member_Index => Defender_Index_2,
                     Reason =>
                       To_String(Source => Attacker.Name) &
                       To_Unbounded_String(Source => " blow in melee combat"),
                     Ship => Enemy.Ship);
                  Change_Boarding_Order_Loop :
                  for Order of Boarding_Orders loop
                     if Order >= Defender_Index_2 then
                        Order := Order - 1;
                     end if;
                  end loop Change_Boarding_Order_Loop;
                  Update_Killed_Mobs
                    (Mob => Defender,
                     Fraction_Name =>
                       To_Unbounded_String
                         (Source => To_String(Source => Faction_Name)));
                  Update_Goal
                    (G_Type => KILL,
                     Target_Index =>
                       To_Unbounded_String
                         (Source => To_String(Source => Faction_Name)));
                  if Enemy.Ship.Crew.Length = 0 then
                     End_Combat := True;
                  end if;
               else
                  Order_Index := 0;
                  Change_Order_Loop :
                  for I in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(I).Order = BOARDING then
                        Order_Index := Order_Index + 1;
                     end if;
                     if Crew_Container.To_Index(Position => I) =
                       Defender_Index_2 then
                        Boarding_Orders.Delete(Index => Order_Index);
                        Order_Index := Order_Index - 1;
                        exit Change_Order_Loop;
                     end if;
                  end loop Change_Order_Loop;
                  Death
                    (Member_Index => Defender_Index_2,
                     Reason =>
                       To_String(Source => Attacker.Name) &
                       To_Unbounded_String(Source => " blow in melee combat"),
                     Ship => Player_Ship);
                  if Defender_Index_2 = 1 then -- Player is dead
                     End_Combat := True;
                  end if;
               end if;
               return False;
            else
               return True;
            end if;
         end Character_Attack;
      begin
         Attacker_Index := Attackers.First_Index;
         Order_Index := 1;
         Attackers_Attacks_Loop :
         while Attacker_Index <=
           Attackers.Last_Index loop -- Boarding party attacks first
            Riposte := True;
            if Attackers(Attacker_Index).Order /= BOARDING then
               goto End_Of_Attacker_Loop;
            end if;
            Attack_Done := False;
            if Player_Attack then
               exit Attackers_Attacks_Loop when Order_Index >
                 Boarding_Orders.Last_Index;
               if Boarding_Orders(Order_Index) in
                   Defenders.First_Index .. Defenders.Last_Index then
                  Defender_Index := Boarding_Orders(Order_Index);
                  Riposte :=
                    Character_Attack
                      (Attacker_Index_2 => Attacker_Index,
                       Defender_Index_2 => Defender_Index,
                       Player_Attack_2 => Player_Attack);
                  if not End_Combat and Riposte then
                     if Enemy.Ship.Crew(Defender_Index).Order /= DEFEND then
                        Give_Orders
                          (Ship => Enemy.Ship, Member_Index => Defender_Index,
                           Given_Order => DEFEND, Module_Index => 0,
                           Check_Priorities => False);
                     end if;
                     Riposte :=
                       Character_Attack
                         (Attacker_Index_2 => Defender_Index,
                          Defender_Index_2 => Attacker_Index,
                          Player_Attack_2 => not Player_Attack);
                  else
                     Riposte := True;
                  end if;
                  Attack_Done := True;
               elsif Boarding_Orders(Order_Index) = -1 then
                  Give_Orders
                    (Ship => Player_Ship, Member_Index => Attacker_Index,
                     Given_Order => REST);
                  Boarding_Orders.Delete(Index => Order_Index);
                  Order_Index := Order_Index - 1;
                  Attack_Done := True;
               end if;
               Order_Index := Order_Index + 1;
            end if;
            if not Attack_Done then
               Defenders_Riposte_Loop :
               for Defender in
                 Defenders.First_Index .. Defenders.Last_Index loop
                  if Defenders(Defender).Order = DEFEND then
                     Riposte :=
                       Character_Attack
                         (Attacker_Index_2 => Attacker_Index,
                          Defender_Index_2 => Defender,
                          Player_Attack_2 => Player_Attack);
                     if not End_Combat and Riposte then
                        Riposte :=
                          Character_Attack
                            (Attacker_Index_2 => Defender,
                             Defender_Index_2 => Attacker_Index,
                             Player_Attack_2 => not Player_Attack);
                     else
                        Riposte := True;
                     end if;
                     Attack_Done := True;
                     exit Defenders_Riposte_Loop;
                  end if;
               end loop Defenders_Riposte_Loop;
            end if;
            if not Attack_Done then
               Defender_Index :=
                 Get_Random
                   (Min => Defenders.First_Index, Max => Defenders.Last_Index);
               if Player_Attack then
                  Give_Orders
                    (Ship => Enemy.Ship, Member_Index => Defender_Index,
                     Given_Order => DEFEND, Module_Index => 0,
                     Check_Priorities => False);
               else
                  Give_Orders
                    (Ship => Player_Ship, Member_Index => Defender_Index,
                     Given_Order => DEFEND, Module_Index => 0,
                     Check_Priorities => False);
               end if;
               Riposte :=
                 Character_Attack
                   (Attacker_Index_2 => Attacker_Index,
                    Defender_Index_2 => Defender_Index,
                    Player_Attack_2 => Player_Attack);
               if not End_Combat and Riposte then
                  Riposte :=
                    Character_Attack
                      (Attacker_Index_2 => Defender_Index,
                       Defender_Index_2 => Attacker_Index,
                       Player_Attack_2 => not Player_Attack);
               else
                  Riposte := True;
               end if;
            end if;
            <<End_Of_Attacker_Loop>>
            exit Attackers_Attacks_Loop when End_Combat;
            if Riposte then
               Attacker_Index := Attacker_Index + 1;
            end if;
         end loop Attackers_Attacks_Loop;
         Defender_Index := Defenders.First_Index;
         Defenders_Attacks_Loop :
         while Defender_Index <= Defenders.Last_Index loop -- Defenders attacks
            Riposte := True;
            if Defenders(Defender_Index).Order = DEFEND then
               Attackers_Riposte_Loop :
               for Attacker in
                 Attackers.First_Index .. Attackers.Last_Index loop
                  if Attackers(Attacker).Order = BOARDING then
                     Riposte :=
                       Character_Attack
                         (Attacker_Index_2 => Defender_Index,
                          Defender_Index_2 => Attacker,
                          Player_Attack_2 => not Player_Attack);
                     if not End_Combat and Riposte then
                        Riposte :=
                          Character_Attack
                            (Attacker_Index_2 => Attacker,
                             Defender_Index_2 => Defender_Index,
                             Player_Attack_2 => Player_Attack);
                     end if;
                     exit Attackers_Riposte_Loop;
                  end if;
               end loop Attackers_Riposte_Loop;
            end if;
            if Riposte then
               Defender_Index := Defender_Index + 1;
            end if;
         end loop Defenders_Attacks_Loop;
         if Find_Member(Order => BOARDING) = 0 then
            Update_Orders(Ship => Enemy.Ship);
         end if;
      end Melee_Combat;
   begin
      if Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type) =
        0 then
         Add_Message
           (Message => "Ship fall from sky due to lack of fuel.",
            M_Type => OTHERMESSAGE, Color => RED);
         Death
           (Member_Index => 1,
            Reason => To_Unbounded_String(Source => "fall of the ship"),
            Ship => Player_Ship);
         End_Combat := True;
         return;
      end if;
      Count_Run_From_Combat_Block :
      declare
         Chance_For_Run: Integer;
      begin
         Turn_Number := Turn_Number + 1;
         case Enemy.Combat_Ai is
            when ATTACKER =>
               Chance_For_Run := Turn_Number - 120;
            when BERSERKER =>
               Chance_For_Run := Turn_Number - 200;
            when DISARMER =>
               Chance_For_Run := Turn_Number - 60;
            when others =>
               null;
         end case;
         if Chance_For_Run > 1
           and then Get_Random(Min => 1, Max => 100) < Chance_For_Run then
            Enemy.Combat_Ai := COWARD;
         end if;
      end Count_Run_From_Combat_Block;
      Pilot_Engineer_Experience_Loop :
      for I in Player_Ship.Crew.Iterate loop
         case Player_Ship.Crew(I).Order is
            when PILOT =>
               Pilot_Index := Crew_Container.To_Index(Position => I);
               Gain_Exp
                 (Amount => 2, Skill_Number => Piloting_Skill,
                  Crew_Index => Pilot_Index);
            when ENGINEER =>
               Engineer_Index := Crew_Container.To_Index(Position => I);
               Gain_Exp
                 (Amount => 2, Skill_Number => Engineering_Skill,
                  Crew_Index => Engineer_Index);
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
           Get_Skill_Level
             (Member => Player_Ship.Crew(Pilot_Index),
              Skill_Index => Piloting_Skill);
      else
         Accuracy_Bonus := 20;
         Evade_Bonus := -10;
      end if;
      Enemy_Pilot_Index :=
        Find_Member(Order => PILOT, Crew => Enemy.Ship.Crew);
      if Enemy_Pilot_Index > 0 then
         Accuracy_Bonus :=
           Accuracy_Bonus -
           Get_Skill_Level
             (Member => Enemy.Ship.Crew(Enemy_Pilot_Index),
              Skill_Index => Piloting_Skill);
      end if;
      if Engineer_Index > 0 or
        Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) then
         Message :=
           To_Unbounded_String
             (Source =>
                ChangeShipSpeed(SpeedValue => Ship_Speed'Val(Engineer_Order)));
         if Length(Source => Message) > 0 then
            Add_Message
              (Message => To_String(Source => Message), M_Type => ORDERMESSAGE,
               Color => RED);
         end if;
      end if;
      Speed_Bonus := 20 - (RealSpeed(Ship => Player_Ship) / 100);
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
                          Objects_Container.Key(Position => K) then
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
         Enemy_Weapon_Index := Modules_Container.To_Index(Position => I);
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
                 (Message =>
                    To_String(Source => Enemy_Name) & " increases speed.",
                  M_Type => COMBATMESSAGE);
               Enemy_Pilot_Order := 1;
            elsif Enemy.Distance <= 10 and Enemy.Ship.Speed = FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) - 1);
               Add_Message
                 (Message =>
                    To_String(Source => Enemy_Name) & " decreases speed.",
                  M_Type => COMBATMESSAGE);
               Enemy_Pilot_Order := 2;
            end if;
         when ATTACKER | DISARMER =>
            if Enemy.Distance > Damage_Range and
              Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) + 1);
               Add_Message
                 (Message =>
                    To_String(Source => Enemy_Name) & " increases speed.",
                  M_Type => COMBATMESSAGE);
               Enemy_Pilot_Order := 1;
            elsif Enemy.Distance < Damage_Range and
              Enemy.Ship.Speed > QUARTER_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) - 1);
               Add_Message
                 (Message =>
                    To_String(Source => Enemy_Name) & " decreases speed.",
                  M_Type => COMBATMESSAGE);
               Enemy_Pilot_Order := 2;
            end if;
         when COWARD =>
            if Enemy.Distance < 15_000 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 Ship_Speed'Val(Ship_Speed'Pos(Enemy.Ship.Speed) + 1);
               Add_Message
                 (Message =>
                    To_String(Source => Enemy_Name) & " increases speed.",
                  M_Type => COMBATMESSAGE);
            end if;
            Enemy_Pilot_Order := 4;
         when others =>
            null;
      end case;
      if Enemy.Harpoon_Duration > 0 then
         Enemy.Ship.Speed := FULL_STOP;
         Add_Message
           (Message =>
              To_String(Source => Enemy_Name) & " is stopped by your ship.",
            M_Type => COMBATMESSAGE);
      elsif Enemy.Ship.Speed = FULL_STOP then
         Enemy.Ship.Speed := QUARTER_SPEED;
      end if;
      if Harpoon_Duration > 0 then
         Player_Ship.Speed := FULL_STOP;
         Add_Message
           (Message => "You are stopped by enemy ship.",
            M_Type => COMBATMESSAGE);
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
      Speed_Bonus := 20 - (RealSpeed(Ship => Enemy.Ship) / 100);
      if Speed_Bonus < -10 then
         Speed_Bonus := -10;
      end if;
      Accuracy_Bonus := Accuracy_Bonus + Speed_Bonus;
      Evade_Bonus := Evade_Bonus - Speed_Bonus;
      Distance_Traveled :=
        (if Enemy_Pilot_Order < 4 then -(RealSpeed(Ship => Enemy.Ship))
         else RealSpeed(Ship => Enemy.Ship));
      if Pilot_Index > 0 then
         case Pilot_Order is
            when 1 | 3 =>
               Distance_Traveled :=
                 Distance_Traveled - RealSpeed(Ship => Player_Ship);
            when 2 =>
               Distance_Traveled :=
                 Distance_Traveled + RealSpeed(Ship => Player_Ship);
               if Distance_Traveled > 0 and Enemy_Pilot_Order /= 4 then
                  Distance_Traveled := 0;
               end if;
            when 4 =>
               Distance_Traveled :=
                 Distance_Traveled + RealSpeed(Ship => Player_Ship);
            when others =>
               null;
         end case;
      else
         Distance_Traveled :=
           Distance_Traveled - RealSpeed(Ship => Player_Ship);
      end if;
      Enemy.Distance := Enemy.Distance + Distance_Traveled;
      if Enemy.Distance < 10 then
         Enemy.Distance := 10;
      end if;
      if Enemy.Distance >= 15_000 then
         if Pilot_Order = 4 then
            Add_Message
              (Message =>
                 "You escaped the " & To_String(Source => Enemy_Name) & ".",
               M_Type => COMBATMESSAGE);
         else
            Add_Message
              (Message =>
                 To_String(Source => Enemy_Name) & " escaped from you.",
               M_Type => COMBATMESSAGE);
         end if;
         Kill_Boarding_Party_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Order = BOARDING then
               Death
                 (Member_Index => Crew_Container.To_Index(Position => I),
                  Reason => To_Unbounded_String(Source => "enemy crew"),
                  Ship => Player_Ship, Create_Body => False);
            end if;
         end loop Kill_Boarding_Party_Loop;
         End_Combat := True;
         return;
      elsif Enemy.Distance < 15_000 and Enemy.Distance >= 10_000 then
         Accuracy_Bonus := Accuracy_Bonus - 10;
         Evade_Bonus := Evade_Bonus + 10;
         Log_Message(Message => "Distance: long", Message_Type => Log.COMBAT);
      elsif Enemy.Distance < 5_000 and Enemy.Distance >= 1_000 then
         Accuracy_Bonus := Accuracy_Bonus + 10;
         Log_Message
           (Message => "Distance: medium", Message_Type => Log.COMBAT);
      elsif Enemy.Distance < 1_000 then
         Accuracy_Bonus := Accuracy_Bonus + 20;
         Evade_Bonus := Evade_Bonus - 10;
         Log_Message
           (Message => "Distance: short or close", Message_Type => Log.COMBAT);
      end if;
      Attack(Ship => Player_Ship, Enemy_Ship => Enemy.Ship); -- Player attack
      if not End_Combat then
         Attack(Ship => Enemy.Ship, Enemy_Ship => Player_Ship); -- Enemy attack
      end if;
      if not End_Combat then
         Boarding_Combat_Block :
         declare
            Have_Boarding_Party: Boolean := False;
         begin
            Check_For_Boarding_Party_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Order = BOARDING then
                  Have_Boarding_Party := True;
                  exit Check_For_Boarding_Party_Loop;
               end if;
            end loop Check_For_Boarding_Party_Loop;
            Check_For_Enemy_Boarding_Party_Loop :
            for Member of Enemy.Ship.Crew loop
               if Member.Order = BOARDING then
                  Have_Boarding_Party := True;
                  exit Check_For_Enemy_Boarding_Party_Loop;
               end if;
            end loop Check_For_Enemy_Boarding_Party_Loop;
            if Enemy.Harpoon_Duration > 0 or Harpoon_Duration > 0 or
              Have_Boarding_Party then
               if not End_Combat and
                 Enemy.Ship.Crew.Length >
                   0 then -- Characters combat (player boarding party)
                  Melee_Combat
                    (Attackers => Player_Ship.Crew,
                     Defenders => Enemy.Ship.Crew, Player_Attack => True);
               end if;
               if not End_Combat and
                 Enemy.Ship.Crew.Length >
                   0 then -- Characters combat (enemy boarding party)
                  Melee_Combat
                    (Attackers => Enemy.Ship.Crew,
                     Defenders => Player_Ship.Crew, Player_Attack => False);
               end if;
            end if;
         end Boarding_Combat_Block;
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
            Update_Orders(Ship => Player_Ship, Combat => True);
         end if;
         Update_Game(Minutes => 1, In_Combat => True);
      elsif Player_Ship.Crew(1).Health > 0 then
         End_Combat_Block :
         declare
            Was_Boarded: Boolean := False;
            Loot_Amount: Integer;
         begin
            if Find_Member(Order => BOARDING) > 0 then
               Was_Boarded := True;
            end if;
            Enemy.Ship.Modules(1).Durability := 0;
            Add_Message
              (Message => To_String(Source => Enemy_Name) & " is destroyed!",
               M_Type => COMBATMESSAGE);
            Loot_Amount := Enemy.Loot;
            Ship_Free_Space := FreeCargo(Amount => (0 - Loot_Amount));
            if Ship_Free_Space < 0 then
               Loot_Amount := Loot_Amount + Ship_Free_Space;
            end if;
            if Loot_Amount > 0 then
               Add_Message
                 (Message =>
                    "You looted" & Integer'Image(Loot_Amount) & " " &
                    To_String(Source => Money_Name) & " from " &
                    To_String(Source => Enemy_Name) & ".",
                  M_Type => COMBATMESSAGE);
               UpdateCargo
                 (Ship => Player_Ship, ProtoIndex => Money_Index,
                  Amount => Loot_Amount);
            end if;
            Ship_Free_Space := FreeCargo(Amount => 0);
            if Was_Boarded and Ship_Free_Space > 0 then
               Message :=
                 To_Unbounded_String
                   (Source =>
                      "Additionally, your boarding party takes from ") &
                 Enemy_Name & To_Unbounded_String(Source => ":");
               Looting_Loop :
               for Item of Enemy.Ship.Cargo loop
                  Loot_Amount := Item.Amount / 5;
                  Ship_Free_Space := FreeCargo(Amount => (0 - Loot_Amount));
                  if Ship_Free_Space < 0 then
                     Loot_Amount := Loot_Amount + Ship_Free_Space;
                  end if;
                  if Items_List(Item.Proto_Index).Price = 0 and
                    Item.Proto_Index /= Money_Index then
                     Loot_Amount := 0;
                  end if;
                  if Loot_Amount > 0 then
                     if Item /=
                       Inventory_Container.First_Element
                         (Container => Enemy.Ship.Cargo) then
                        Message :=
                          Message & To_Unbounded_String(Source => ",");
                     end if;
                     UpdateCargo
                       (Ship => Player_Ship, ProtoIndex => Item.Proto_Index,
                        Amount => Loot_Amount);
                     Message :=
                       Message & Positive'Image(Loot_Amount) &
                       To_Unbounded_String(Source => " ") &
                       Items_List(Item.Proto_Index).Name;
                     Ship_Free_Space := FreeCargo(Amount => 0);
                     exit Looting_Loop when Item =
                       Inventory_Container.Last_Element
                         (Container => Enemy.Ship.Cargo) or
                       Ship_Free_Space = 0;
                  end if;
               end loop Looting_Loop;
               Add_Message
                 (Message => To_String(Source => Message) & ".",
                  M_Type => COMBATMESSAGE);
               if Current_Story.Index /= Null_Unbounded_String then
                  Story_Loot_Block :
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
                        Create
                          (S => Tokens,
                           From => To_String(Source => Current_Story.Data),
                           Separators => ";");
                        if Slice(S => Tokens, Index => 2) = "any" or
                          Slice(S => Tokens, Index => 2) =
                            To_String(Source => Enemy_Ship_Index) then
                           if Progress_Story then
                              case Step.Finish_Condition is
                                 when LOOT =>
                                    UpdateCargo
                                      (Ship => Player_Ship,
                                       ProtoIndex =>
                                         To_Bounded_String
                                           (Source =>
                                              Slice(S => Tokens, Index => 1)),
                                       Amount => 1);
                                 when others =>
                                    null;
                              end case;
                           end if;
                        end if;
                     end if;
                  end Story_Loot_Block;
               else
                  Start_Story
                    (Faction_Name => Faction_Name, Condition => DROPITEM);
               end if;
            end if;
            Give_Orders_Loop :
            for I in Player_Ship.Crew.Iterate loop
               if Player_Ship.Crew(I).Order = BOARDING then
                  Give_Orders
                    (Ship => Player_Ship,
                     Member_Index => Crew_Container.To_Index(Position => I),
                     Given_Order => REST);
               elsif Player_Ship.Crew(I).Order = DEFEND then
                  Give_Orders
                    (Ship => Player_Ship,
                     Member_Index => Crew_Container.To_Index(Position => I),
                     Given_Order => REST);
               end if;
            end loop Give_Orders_Loop;
         end End_Combat_Block;
         Enemy.Ship.Speed := FULL_STOP;
         Player_Ship.Speed := Old_Speed;
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
            if Events_List
                (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                .E_Type =
              ATTACKONBASE then
               Gain_Rep
                 (Base_Index =>
                    Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
                  Points => 5);
            end if;
            Delete_Event
              (Event_Index =>
                 Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index);
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
              (Mission_Index =>
                 Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index);
         end if;
         Lost_Reputation_Block :
         declare
            Lost_Reputation_Chance: Positive range 10 .. 40 := 10;
         begin
            if Proto_Ships_List(Enemy_Ship_Index).Owner =
              Player_Ship.Crew(1).Faction then
               Lost_Reputation_Chance := 40;
            end if;
            if Get_Random(Min => 1, Max => 100) < Lost_Reputation_Chance then
               Gain_Rep(Base_Index => Enemy.Ship.Home_Base, Points => -100);
            end if;
         end Lost_Reputation_Block;
         Update_Destroyed_Ships(Ship_Name => Enemy.Ship.Name);
         Update_Goal(G_Type => DESTROY, Target_Index => Enemy_Ship_Index);
         if Current_Goal.Target_Index /= Null_Unbounded_String then
            Update_Goal
              (G_Type => DESTROY,
               Target_Index =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source => Proto_Ships_List(Enemy_Ship_Index).Owner)));
         end if;
         if Current_Story.Index /= Null_Unbounded_String then
            Update_Current_Story_Block :
            declare
               Finish_Condition: constant Step_Condition_Type :=
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
               if Finish_Condition /= DESTROYSHIP then
                  return;
               end if;
               Create
                 (S => Tokens, From => To_String(Source => Current_Story.Data),
                  Separators => ";");
               if Player_Ship.Sky_X =
                 Positive'Value(Slice(S => Tokens, Index => 1)) and
                 Player_Ship.Sky_Y =
                   Positive'Value(Slice(S => Tokens, Index => 2)) and
                 Enemy_Ship_Index =
                   To_Unbounded_String
                     (Source => Slice(S => Tokens, Index => 3)) then
                  if not Progress_Story(Next_Step => True) then
                     return;
                  end if;
               end if;
            end Update_Current_Story_Block;
         end if;
      end if;
   end Combat_Turn;

end Combat;
