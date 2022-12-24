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

with Bases;
with Careers;
with Config; use Config;
with Combat;
with Crew.Inventory;
with Factions;
with Maps; use Maps;
with Messages; use Messages;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement;
with ShipModules;
with Utils; use Utils;

package body Crew is

   procedure Gain_Exp
     (Amount: Natural; Skill_Number: Skills_Amount_Range;
      Crew_Index: Positive) is
      use Careers;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Skill_Exp, Attribute_Exp, Attribute_Level, New_Amount: Natural := 0;
      --## rule on IMPROPER_INITIALIZATION
      Attribute_Index: constant Skills_Container.Extended_Index :=
        Count_Type
          (SkillsData_Container.Element
             (Container => Skills_List, Index => Skill_Number)
             .Attribute);
      Skill_Index: Skills_Container.Extended_Index := 0;
      Skill_Level: Skill_Range := 0;
      procedure Gain_Exp_In_Attribute(Attribute: Positive) is
         Attribute_To_Check: Mob_Attribute_Record :=
           Player_Ship.Crew(Crew_Index).Attributes(Attribute);
      begin
         if Attribute_To_Check.Level = 50 then
            return;
         end if;
         Attribute_Exp := Attribute_To_Check.Experience + New_Amount;
         Attribute_Level := Attribute_To_Check.Level;
         if Attribute_Exp >= Attribute_Level * 250 then
            --## rule off SIMPLIFIABLE_EXPRESSIONS
            Attribute_Exp := Attribute_Exp - (Attribute_Level * 250);
            --## rule on SIMPLIFIABLE_EXPRESSIONS
            Attribute_Level := Attribute_Level + 1;
         end if;
         Attribute_To_Check.Level := Attribute_Level;
         Attribute_To_Check.Experience := Attribute_Exp;
         Player_Ship.Crew(Crew_Index).Attributes(Attribute) :=
           Attribute_To_Check;
      end Gain_Exp_In_Attribute;
   begin
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      New_Amount :=
        (if
           Careers_List(Player_Career).Skills.Contains
             (Item =>
                To_Unbounded_String
                  (Source =>
                     To_String
                       (Source =>
                          SkillsData_Container.Element
                            (Container => Skills_List, Index => Skill_Number)
                            .Name)))
         then Amount + (Amount / 2)
         else Amount);
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      --## rule off ASSIGNMENTS
      New_Amount :=
        Natural(Float(New_Amount) * New_Game_Settings.Experience_Bonus);
      --## rule on ASSIGNMENTS
      if New_Amount = 0 then
         return;
      end if;
      -- Gain experience in condition assigned attribute
      Gain_Exp_In_Attribute(Attribute => Positive(Condition_Index));
      -- Gain experience in associated attribute
      Gain_Exp_In_Attribute(Attribute => Natural(Attribute_Index));
      -- Gain experience in skill
      Experience_In_Skill_Loop :
      for I in
        Skills_Container.First_Index
          (Container => Player_Ship.Crew(Crew_Index).Skills) ..
          Skills_Container.Last_Index
            (Container => Player_Ship.Crew(Crew_Index).Skills) loop
         if Skills_Container.Element
             (Container => Player_Ship.Crew(Crew_Index).Skills, Index => I)
             .Index =
           Skill_Number then
            Skill_Index := I;
            exit Experience_In_Skill_Loop;
         end if;
      end loop Experience_In_Skill_Loop;
      if Skill_Index > 0 then
         if Skills_Container.Element
             (Container => Player_Ship.Crew(Crew_Index).Skills,
              Index => Skill_Index)
             .Level =
           Skill_Range'Last then
            return;
         end if;
         Skill_Level :=
           Skills_Container.Element
             (Container => Player_Ship.Crew(Crew_Index).Skills,
              Index => Skill_Index)
             .Level;
         Skill_Exp :=
           Skills_Container.Element
             (Container => Player_Ship.Crew(Crew_Index).Skills,
              Index => Skill_Index)
             .Experience +
           New_Amount;
      end if;
      if Skill_Exp >= Skill_Level * 25 then
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Skill_Exp := Skill_Exp - (Skill_Level * 25);
         --## rule on SIMPLIFIABLE_EXPRESSIONS
         Skill_Level := Skill_Level + 1;
      end if;
      if Skill_Index > 0 then
         Skills_Container.Replace_Element
           (Container => Player_Ship.Crew(Crew_Index).Skills,
            Index => Skill_Index,
            New_Item =>
              (Index => Skill_Number, Level => Skill_Level,
               Experience => Skill_Exp));
      else
         Skills_Container.Append
           (Container => Player_Ship.Crew(Crew_Index).Skills,
            New_Item =>
              (Index => Skill_Number, Level => Skill_Level,
               Experience => Skill_Exp));
      end if;
   end Gain_Exp;

   function Generate_Member_Name
     (Gender: Character; Faction_Index: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String is
      use Tiny_String;
      function Generate_Ada_Member_Name
        (Crew_Gender: Character; F_Index: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "generateAdaMemberName";
   begin
      return
        To_Bounded_String
          (Source =>
             Value
               (Item =>
                  Generate_Ada_Member_Name
                    (Crew_Gender => Gender,
                     F_Index =>
                       New_String
                         (Str => To_String(Source => Faction_Index)))));
   end Generate_Member_Name;

   function Find_Cabin(Member_Index: Positive) return Natural is
   begin
      Find_Cabin_Loop :
      for I in Player_Ship.Modules.Iterate loop
         if Player_Ship.Modules(I).M_Type = CABIN then
            Check_Owner_Loop :
            for Owner of Player_Ship.Modules(I).Owner loop
               if Owner = Member_Index then
                  return Modules_Container.To_Index(Position => I);
               end if;
            end loop Check_Owner_Loop;
         end if;
      end loop Find_Cabin_Loop;
      return 0;
   end Find_Cabin;

   procedure Update_Crew
     (Minutes: Positive; Tired_Points: Natural; In_Combat: Boolean := False) is
      use Crew.Inventory;
      use Factions;
      use ShipModules;
      use Tiny_String;

      Tired_Level, Hunger_Level, Thirst_Level: Integer := 0;
      Health_Level: Integer := 100;
      Death_Reason: Unbounded_String := Null_Unbounded_String;
      Tool_Index: Inventory_Container.Extended_Index := 0;
      Cabin_Index: Modules_Container.Extended_Index := 0;
      Times, Rest_Amount: Natural := 0;
      I: Natural;
      Order_Time, Current_Minutes, Heal_Amount: Integer := 0;
      Damage: Damage_Factor := 0.0;
      Need_Cleaning, Have_Medical_Room: Boolean := False;
      Skill_Index: Skills_Container.Extended_Index := 0;
      Faction: Faction_Record;
      procedure Update_Member(Member: in out Member_Data) is
         Back_To_Work: Boolean := True;
         Consume_Result: Natural := 0;
         Member_Faction: constant Faction_Record :=
           Get_Faction(Index => Member.Faction);
         procedure Normalize_Stat
           (Stat: in out Integer; Max_Value: Positive := 100) is
         begin
            if Stat > Max_Value then
               Stat := Max_Value;
            elsif Stat < 0 then
               Stat := 0;
            end if;
         end Normalize_Stat;
         function Consume(Item_Type: Bounded_String) return Natural is
            Consume_Value: Natural := 0;
            Item_Index: Inventory_Container.Extended_Index :=
              Find_Item
                (Inventory => Player_Ship.Cargo, Item_Type => Item_Type);
         begin
            if Item_Index > 0 then
               Consume_Value :=
                 Get_Proto_Item
                   (Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index)
                        .Proto_Index)
                   .Value
                   (1);
               if Get_Proto_Item
                   (Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index)
                        .Proto_Index)
                   .Value'
                   Length >
                 1
                 and then
                   Get_Proto_Item
                     (Index =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => Item_Index)
                          .Proto_Index)
                     .Value
                     (2) /=
                   0 then
                  Update_Morale
                    (Ship => Player_Ship, Member_Index => I,
                     Value =>
                       Get_Proto_Item
                         (Index =>
                            Inventory_Container.Element
                              (Container => Player_Ship.Cargo,
                               Index => Item_Index)
                              .Proto_Index)
                         .Value
                         (2));
               end if;
               Update_Cargo
                 (Ship => Player_Ship,
                  Proto_Index =>
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Item_Index)
                      .Proto_Index,
                  Amount => -1);
               return Consume_Value;
            end if;
            Item_Index :=
              Find_Item
                (Inventory => Player_Ship.Crew(I).Inventory,
                 Item_Type => Item_Type);
            if Item_Index > 0 then
               Consume_Value :=
                 Get_Proto_Item
                   (Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(I).Inventory,
                         Index => Item_Index)
                        .Proto_Index)
                   .Value
                   (1);
               if Get_Proto_Item
                   (Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index)
                        .Proto_Index)
                   .Value
                   (2) /=
                 0 then
                  Update_Morale
                    (Ship => Player_Ship, Member_Index => I,
                     Value =>
                       Get_Proto_Item
                         (Index =>
                            Inventory_Container.Element
                              (Container => Player_Ship.Cargo,
                               Index => Item_Index)
                              .Proto_Index)
                         .Value
                         (2));
               end if;
               Update_Inventory
                 (Member_Index => I, Amount => -1,
                  Inventory_Index => Item_Index, Ship => Player_Ship);
               return Consume_Value;
            end if;
            return 0;
         end Consume;
      begin
         if Member_Faction.Flags.Contains
             (Item => To_Unbounded_String(Source => "nofatigue")) then
            Tired_Level := 0;
         end if;
         if Tired_Level = 0 and Member.Order = REST and
           Member.Previous_Order /= REST then
            if Member.Previous_Order not in REPAIR | CLEAN
              and then Find_Member(Order => Member.Previous_Order) > 0 then
               Back_To_Work := False;
            end if;
            if Member.Previous_Order in GUNNER | CRAFT then
               Module_Loop :
               for Module of Player_Ship.Modules loop
                  if (Member.Previous_Order = GUNNER and Module.M_Type = GUN)
                    and then Module.Owner(1) in I | 0 then
                     Back_To_Work := True;
                     Module.Owner(1) := I;
                     exit Module_Loop;
                  elsif
                    (Member.Previous_Order = CRAFT and
                     Module.M_Type = WORKSHOP)
                    and then Module.Crafting_Index /= Null_Bounded_String then
                     Module_Is_Owner_Loop :
                     for Owner of Module.Owner loop
                        if Owner = I then
                           Back_To_Work := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop Module_Is_Owner_Loop;
                     Module_Empty_Owner_Loop :
                     for Owner of Module.Owner loop
                        if Owner = 0 then
                           Back_To_Work := True;
                           Owner := I;
                           exit Module_Loop;
                        end if;
                     end loop Module_Empty_Owner_Loop;
                  end if;
               end loop Module_Loop;
            end if;
            if Back_To_Work then
               Member.Order := Member.Previous_Order;
               Member.Order_Time := 15;
               Add_Message
                 (Message =>
                    To_String(Source => Member.Name) &
                    " returns to work fully rested.",
                  M_Type => ORDERMESSAGE, Color => YELLOW);
               Update_Morale
                 (Ship => Player_Ship, Member_Index => I, Value => 1);
            end if;
            Member.Previous_Order := REST;
         end if;
         if Tired_Level >
           80 + Member.Attributes(Positive(Condition_Index)).Level and
           Member.Order /= REST and not In_Combat then
            Member_Rest_Block :
            declare
               use Combat;

               Can_Rest: Boolean := True;
            begin
               if Member.Order = BOARDING and Harpoon_Duration = 0 and
                 Combat.Enemy.Harpoon_Duration = 0 then
                  Can_Rest := False;
               end if;
               if Can_Rest then
                  Member.Previous_Order := Member.Order;
                  Member.Order := REST;
                  Member.Order_Time := 15;
                  if Member.Equipment(TOOL) > 0 then
                     Update_Cargo
                       (Ship => Player_Ship,
                        Proto_Index =>
                          Inventory_Container.Element
                            (Container => Member.Inventory,
                             Index => Member.Equipment(TOOL))
                            .Proto_Index,
                        Amount => 1,
                        Durability =>
                          Inventory_Container.Element
                            (Container => Member.Inventory,
                             Index => Member.Equipment(TOOL))
                            .Durability);
                     Update_Inventory
                       (Member_Index => I, Amount => -1,
                        Inventory_Index => Member.Equipment(TOOL),
                        Ship => Player_Ship);
                     Member.Equipment(TOOL) := 0;
                  end if;
                  Add_Message
                    (Message =>
                       To_String(Source => Member.Name) &
                       " is too tired to work, they're going to rest.",
                     M_Type => ORDERMESSAGE, Color => YELLOW);
                  if Find_Cabin(Member_Index => I) = 0 then
                     Modules_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN and Module.Durability > 0 then
                           Find_Cabin_Owner_Loop :
                           for Owner of Module.Owner loop
                              if Owner = 0 then
                                 Owner := I;
                                 Add_Message
                                   (Message =>
                                      To_String(Source => Member.Name) &
                                      " take " &
                                      To_String(Source => Module.Name) &
                                      " as own cabin.",
                                    M_Type => OTHERMESSAGE);
                                 exit Modules_Loop;
                              end if;
                           end loop Find_Cabin_Owner_Loop;
                        end if;
                     end loop Modules_Loop;
                  end if;
               else
                  Add_Message
                    (Message =>
                       To_String(Source => Member.Name) &
                       " is very tired but they can't go to rest.",
                     M_Type => ORDERMESSAGE, Color => RED);
                  Update_Morale
                    (Ship => Player_Ship, Member_Index => I,
                     Value => Get_Random(Min => -5, Max => -1));
               end if;
            end Member_Rest_Block;
         end if;
         Normalize_Stat(Stat => Tired_Level, Max_Value => 150);
         Member.Tired := Tired_Level;
         if Hunger_Level > 80 then
            Find_Food_Loop :
            for Food_Type of Member_Faction.Food_Types loop
               Consume_Result := Consume(Item_Type => Food_Type);
               exit Find_Food_Loop when Consume_Result > 0;
            end loop Find_Food_Loop;
            Hunger_Level :=
              (if Hunger_Level - Consume_Result < Skill_Range'First then
                 Skill_Range'First
               else Hunger_Level - Consume_Result);
            if Consume_Result = 0 then
               Add_Message
                 (Message =>
                    To_String(Source => Member.Name) &
                    " is hungry, but they can't find anything to eat.",
                  M_Type => OTHERMESSAGE, Color => RED);
               Update_Morale
                 (Ship => Player_Ship, Member_Index => I,
                  Value => Get_Random(Min => -10, Max => -5));
            end if;
         end if;
         Normalize_Stat(Stat => Hunger_Level);
         Member.Hunger := Hunger_Level;
         if Thirst_Level > 40 then
            Find_Drink_Loop :
            for Drinks_Type of Member_Faction.Drinks_Types loop
               Consume_Result := Consume(Item_Type => Drinks_Type);
               exit Find_Drink_Loop when Consume_Result > 0;
            end loop Find_Drink_Loop;
            Thirst_Level :=
              (if Thirst_Level - Consume_Result < Skill_Range'First then
                 Skill_Range'First
               else Thirst_Level - Consume_Result);
            if Consume_Result = 0 then
               Add_Message
                 (Message =>
                    To_String(Source => Member.Name) &
                    " is thirsty, but they can't find anything to drink.",
                  M_Type => OTHERMESSAGE, Color => RED);
               Update_Morale
                 (Ship => Player_Ship, Member_Index => I,
                  Value => Get_Random(Min => -20, Max => -10));
            end if;
         end if;
         Normalize_Stat(Stat => Thirst_Level);
         Member.Thirst := Thirst_Level;
         Normalize_Stat(Stat => Health_Level);
         Member.Health := Health_Level;
         if Member.Order not in REPAIR | CRAFT | UPGRADING then
            Member.Order_Time := Order_Time;
         end if;
         if Skills_Container.Length(Container => Member.Skills) = 0 then
            Member.Contract_Length := Member.Contract_Length - Minutes;
            if Member.Contract_Length < 0 then
               Member.Contract_Length := 0;
            end if;
         end if;
      end Update_Member;
   begin
      I := Player_Ship.Crew.First_Index;
      Update_Crew_Loop :
      while I <= Player_Ship.Crew.Last_Index loop
         Current_Minutes := Minutes;
         Order_Time := Player_Ship.Crew(I).Order_Time;
         Times := 0;
         Update_Current_Minutes_Loop :
         while Current_Minutes > 0 loop
            if Current_Minutes >= Order_Time then
               Current_Minutes := Current_Minutes - Order_Time;
               Times := Times + 1;
               Order_Time := 15;
            else
               Order_Time := Order_Time - Current_Minutes;
               Current_Minutes := 0;
            end if;
         end loop Update_Current_Minutes_Loop;
         Health_Level := Player_Ship.Crew(I).Health;
         Hunger_Level := Player_Ship.Crew(I).Hunger;
         Thirst_Level := Player_Ship.Crew(I).Thirst;
         Tired_Level := Player_Ship.Crew(I).Tired;
         if Times = 0 then
            goto End_Of_Loop;
         end if;
         if Player_Ship.Crew(I).Order = REST then
            Cabin_Index := Find_Cabin(Member_Index => I);
            Rest_Amount := 0;
            if Player_Ship.Crew(I).Tired > 0 then
               if Cabin_Index > 0 then
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(Player_Ship.Modules(Cabin_Index).Durability) /
                       Float(Player_Ship.Modules(Cabin_Index).Max_Durability));
                  Rest_Amount :=
                    Player_Ship.Modules(Cabin_Index).Cleanliness -
                    Natural
                      (Float(Player_Ship.Modules(Cabin_Index).Cleanliness) *
                       Float(Damage));
                  if Rest_Amount = 0 then
                     Rest_Amount := 1;
                  end if;
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Tired_Level := Tired_Level - (Times * Rest_Amount);
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               else
                  Tired_Level := Tired_Level - Times;
               end if;
               if Tired_Level < 0 then
                  Tired_Level := 0;
               end if;
            end if;
            if not Get_Faction(Index => Player_Ship.Crew(I).Faction).Flags
                .Contains
                (Item => To_Unbounded_String(Source => "nofatigue")) and
              Health_Level in 1 .. 99 and Cabin_Index > 0 then
               Health_Level := Health_Level + Times;
               if Health_Level > 100 then
                  Health_Level := 100;
               end if;
            end if;
            if Player_Ship.Crew(I).Morale(1) < 50 then
               Update_Morale
                 (Ship => Player_Ship, Member_Index => I,
                  Value => Times + Rest_Amount);
               if Player_Ship.Crew(I).Morale(1) > 50 then
                  Player_Ship.Crew(I).Morale := (1 => 50, 2 => 0);
               end if;
            end if;
         else
            if Player_Ship.Crew(I).Order /= TALK then
               Tired_Level := Tired_Level + Times;
            end if;
            if Tired_Level >
              100 +
                Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                  .Level then
               Tired_Level :=
                 100 +
                 Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                   .Level;
            end if;
            if Tired_Level >=
              50 +
                Player_Ship.Crew(I).Attributes(Positive(Condition_Index))
                  .Level then
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Update_Morale
                 (Ship => Player_Ship, Member_Index => I,
                  Value => ((Times / 5) * (-1)));
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            end if;
            case Player_Ship.Crew(I).Order is
               when PILOT =>
                  if Player_Ship.Speed = DOCKED then
                     Tired_Level := Player_Ship.Crew(I).Tired;
                  else
                     Gain_Exp
                       (Amount => Times, Skill_Number => Piloting_Skill,
                        Crew_Index => I);
                  end if;
               when ENGINEER =>
                  if Player_Ship.Speed = DOCKED then
                     Tired_Level := Player_Ship.Crew(I).Tired;
                  else
                     Gain_Exp
                       (Amount => Times, Skill_Number => Engineering_Skill,
                        Crew_Index => I);
                  end if;
               when GUNNER =>
                  if Player_Ship.Speed = DOCKED then
                     Tired_Level := Player_Ship.Crew(I).Tired;
                  end if;
               when HEAL =>
                  Have_Medical_Room := False;
                  Heal_Module_Loop :
                  for Module of Player_Ship.Modules loop
                     if BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .M_Type =
                       MEDICAL_ROOM and
                       Module.Durability > 0 and
                       Module.Owner.Contains(Item => I) then
                        Have_Medical_Room := True;
                        exit Heal_Module_Loop;
                     end if;
                  end loop Heal_Module_Loop;
                  Heal_Loop :
                  for Member of Player_Ship.Crew loop
                     Faction := Get_Faction(Index => Member.Faction);
                     if Member.Name /= Player_Ship.Crew(I).Name and
                       Member.Health < 100 then
                        Heal_Amount :=
                          Times *
                          (Get_Skill_Level
                             (Member => Player_Ship.Crew(I),
                              Skill_Index => Faction.Healing_Skill) /
                           20);
                        if Heal_Amount < Times then
                           Heal_Amount := Times;
                        end if;
                        if not Have_Medical_Room then
                           Heal_Amount := Heal_Amount / 2;
                        end if;
                        if Heal_Amount > 0 then
                           Heal_Amount := Heal_Amount * (-1);
                           Tool_Index :=
                             Find_Item
                               (Inventory => Player_Ship.Cargo,
                                Item_Type => Faction.Healing_Tools);
                           if Tool_Index > 0 then
                              --## rule off SIMPLIFIABLE_EXPRESSIONS
                              Heal_Amount :=
                                (if
                                   Inventory_Container.Element
                                     (Container => Player_Ship.Cargo,
                                      Index => Tool_Index)
                                     .Amount <
                                   abs (Heal_Amount)
                                 then
                                   Inventory_Container.Element
                                     (Container => Player_Ship.Cargo,
                                      Index => Tool_Index)
                                     .Amount
                                 else abs (Heal_Amount));
                              Update_Cargo
                                (Ship => Player_Ship, Amount => -(Heal_Amount),
                                 Cargo_Index => Tool_Index);
                              --## rule on SIMPLIFIABLE_EXPRESSIONS
                           else
                              Tool_Index :=
                                Find_Item
                                  (Inventory => Player_Ship.Crew(I).Inventory,
                                   Item_Type => Faction.Healing_Tools);
                              if Tool_Index > 0 then
                              --## rule off SIMPLIFIABLE_EXPRESSIONS
                                 Heal_Amount :=
                                   (if
                                      Inventory_Container.Element
                                        (Container =>
                                           Player_Ship.Crew(I).Inventory,
                                         Index => Tool_Index)
                                        .Amount <
                                      abs (Heal_Amount)
                                    then
                                      Inventory_Container.Element
                                        (Container =>
                                           Player_Ship.Crew(I).Inventory,
                                         Index => Tool_Index)
                                        .Amount
                                    else abs (Heal_Amount));
                                 Update_Inventory
                                   (Member_Index => I,
                                    Amount => -(Heal_Amount),
                                    Inventory_Index => Tool_Index,
                                    Ship => Player_Ship);
                              --## rule on SIMPLIFIABLE_EXPRESSIONS
                              end if;
                           end if;
                        end if;
                        if Heal_Amount > 0 then
                           Heal_Crew_Loop :
                           for J in Player_Ship.Crew.Iterate loop
                              if Player_Ship.Crew(J).Health < 100 and
                                Crew_Container.To_Index(Position => J) /=
                                  I then
                                 Player_Ship.Crew(J).Health :=
                                   (if
                                      Player_Ship.Crew(J).Health +
                                      Heal_Amount >
                                      Skill_Range'Last
                                    then Skill_Range'Last
                                    else Player_Ship.Crew(J).Health +
                                      Heal_Amount);
                                 Add_Message
                                   (Message =>
                                      To_String
                                        (Source => Player_Ship.Crew(I).Name) &
                                      " healed " &
                                      To_String
                                        (Source => Player_Ship.Crew(J).Name) &
                                      " a bit.",
                                    M_Type => ORDERMESSAGE);
                                 Gain_Exp
                                   (Amount => Times,
                                    Skill_Number => Faction.Healing_Skill,
                                    Crew_Index => I);
                                 exit Heal_Crew_Loop;
                              end if;
                           end loop Heal_Crew_Loop;
                        else
                           if Tool_Index = 0 then
                              Add_Message
                                (Message =>
                                   "You don't have any " &
                                   To_String(Source => Faction.Healing_Tools) &
                                   " to continue healing the wounded " &
                                   To_String(Source => Member.Name) & ".",
                                 M_Type => ORDERMESSAGE, Color => RED);
                           else
                              Add_Message
                                (Message =>
                                   To_String
                                     (Source => Player_Ship.Crew(I).Name) &
                                   " is not enough experienced to heal " &
                                   To_String(Source => Member.Name) &
                                   " in that amount of time.",
                                 M_Type => ORDERMESSAGE, Color => RED);
                           end if;
                        end if;
                     end if;
                  end loop Heal_Loop;
                  Heal_Amount := 1;
                  Update_Heal_Amount_Loop :
                  for J in Player_Ship.Crew.Iterate loop
                     if Player_Ship.Crew(J).Health < 100 and
                       Crew_Container.To_Index(Position => J) /= I then
                        Heal_Amount := 0;
                        Faction :=
                          Get_Faction(Index => Player_Ship.Crew(J).Faction);
                        Tool_Index :=
                          Find_Item
                            (Inventory => Player_Ship.Cargo,
                             Item_Type => Faction.Healing_Tools);
                        if Tool_Index = 0 then
                           Tool_Index :=
                             Find_Item
                               (Inventory => Player_Ship.Crew(I).Inventory,
                                Item_Type => Faction.Healing_Tools);
                           if Tool_Index = 0 then
                              Heal_Amount := -1;
                           end if;
                        end if;
                        exit Update_Heal_Amount_Loop;
                     end if;
                  end loop Update_Heal_Amount_Loop;
                  if Heal_Amount > 0 then
                     Add_Message
                       (Message =>
                          To_String(Source => Player_Ship.Crew(I).Name) &
                          " finished healing the wounded.",
                        M_Type => ORDERMESSAGE, Color => GREEN);
                  end if;
                  if Heal_Amount /= 0 then
                     Give_Orders
                       (Ship => Player_Ship, Member_Index => I,
                        Given_Order => REST);
                  end if;
               when CLEAN =>
                  Tool_Index :=
                    Find_Tools
                      (Member_Index => I, Item_Type => Cleaning_Tools,
                       Order => CLEAN);
                  Need_Cleaning := False;
                  if Tool_Index > 0 then
                     Update_Clean_Tools_Loop :
                     for Module of Player_Ship.Modules loop
                        if Module.M_Type = CABIN
                          and then Module.Cleanliness < Module.Quality then
                           Module.Cleanliness :=
                             (if Module.Cleanliness + Times > Module.Quality
                              then Module.Quality
                              else Module.Cleanliness + Times);
                           Damage_Item
                             (Inventory => Player_Ship.Crew(I).Inventory,
                              Item_Index => Tool_Index, Member_Index => I,
                              Ship => Player_Ship);
                           exit Update_Clean_Tools_Loop;
                        end if;
                     end loop Update_Clean_Tools_Loop;
                  end if;
                  Check_Dirty_Modules_Loop :
                  for Module of Player_Ship.Modules loop
                     if Module.M_Type = CABIN
                       and then Module.Cleanliness < Module.Quality then
                        Need_Cleaning := True;
                        if Tool_Index = 0 then
                           Add_Message
                             (Message =>
                                To_String(Source => Player_Ship.Crew(I).Name) &
                                " can't continue cleaning the ship because don't have any cleaning tools.",
                              M_Type => ORDERMESSAGE, Color => RED);
                           Give_Orders
                             (Ship => Player_Ship, Member_Index => I,
                              Given_Order => REST);
                        end if;
                        exit Check_Dirty_Modules_Loop;
                     end if;
                  end loop Check_Dirty_Modules_Loop;
                  if not Need_Cleaning then
                     Add_Message
                       (Message => "Cleaning the ship have been finished.",
                        M_Type => ORDERMESSAGE, Color => GREEN);
                     Remove_Clean_Order_Loop :
                     for J in Player_Ship.Crew.Iterate loop
                        if Player_Ship.Crew(J).Order = CLEAN then
                           Give_Orders
                             (Ship => Player_Ship,
                              Member_Index =>
                                Crew_Container.To_Index(Position => J),
                              Given_Order => REST);
                        end if;
                     end loop Remove_Clean_Order_Loop;
                  end if;
               when TALK =>
                  if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index =
                    0 then
                     Give_Orders
                       (Ship => Player_Ship, Member_Index => I,
                        Given_Order => REST);
                  end if;
               when TRAIN =>
                  Modules_Loop :
                  for Module of Player_Ship.Modules loop
                     if Module.M_Type = TRAINING_ROOM then
                        Find_Module_Owner_Loop :
                        for Owner of Module.Owner loop
                           if Owner = I then
                              Skill_Index := Module.Trained_Skill;
                              exit Modules_Loop;
                           end if;
                        end loop Find_Module_Owner_Loop;
                     end if;
                  end loop Modules_Loop;
                  if SkillsData_Container.Element
                      (Container => Skills_List, Index => Skill_Index)
                      .Tool /=
                    Null_Bounded_String then
                     Tool_Index :=
                       Find_Tools
                         (Member_Index => I,
                          Item_Type =>
                            SkillsData_Container.Element
                              (Container => Skills_List, Index => Skill_Index)
                              .Tool,
                          Order => TRAIN,
                          Tool_Quality =>
                            Get_Training_Tool_Quality
                              (Member_Index => I,
                               Skill_Index => Natural(Skill_Index)));
                     if Tool_Index > 0 then
                        Update_Train_Tool_Loop :
                        for J in 1 .. Times loop
                           Gain_Exp
                             (Amount => Get_Random(Min => 1, Max => 5),
                              Skill_Number => Skill_Index, Crew_Index => I);
                           Damage_Item
                             (Inventory => Player_Ship.Crew(I).Inventory,
                              Item_Index => Tool_Index, Member_Index => I,
                              Ship => Player_Ship);
                           Tool_Index :=
                             Find_Tools
                               (Member_Index => I,
                                Item_Type =>
                                  SkillsData_Container.Element
                                    (Container => Skills_List,
                                     Index => Skill_Index)
                                    .Tool,
                                Order => TRAIN);
                           exit Update_Train_Tool_Loop when Tool_Index = 0;
                        end loop Update_Train_Tool_Loop;
                        Add_Message
                          (Message =>
                             To_String(Source => Player_Ship.Crew(I).Name) &
                             " trained a little " &
                             To_String
                               (Source =>
                                  SkillsData_Container.Element
                                    (Container => Skills_List,
                                     Index => Skill_Index)
                                    .Name) &
                             ".",
                           M_Type => ORDERMESSAGE);
                     end if;
                     if Tool_Index = 0 then
                        Add_Message
                          (Message =>
                             To_String(Source => Player_Ship.Crew(I).Name) &
                             " can't continue training because they don't have the proper tools.",
                           M_Type => ORDERMESSAGE, Color => RED);
                        Give_Orders
                          (Ship => Player_Ship, Member_Index => I,
                           Given_Order => REST);
                     end if;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         <<End_Of_Loop>>
         if Tired_Points > 0 then
            Faction := Get_Faction(Index => Player_Ship.Crew(I).Faction);
            if Faction.Food_Types.Length > 0 then
               Hunger_Level :=
                 (if Hunger_Level + Tired_Points > Skill_Range'Last then
                    Skill_Range'Last
                  else Hunger_Level + Tired_Points);
               if Player_Ship.Crew(I).Hunger = Skill_Range'Last then
                  Health_Level := Health_Level - Tired_Points;
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Update_Morale
                    (Ship => Player_Ship, Member_Index => I,
                     Value => -(Tired_Points));
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
                  if Health_Level < 1 then
                     Health_Level := Skill_Range'First;
                     Death_Reason :=
                       To_Unbounded_String(Source => "starvation");
                  end if;
               end if;
            end if;
            if Faction.Drinks_Types.Length > 0 then
               Thirst_Level :=
                 (if Thirst_Level + Tired_Points > Skill_Range'Last then
                    Skill_Range'Last
                  else Thirst_Level + Tired_Points);
               if Player_Ship.Crew(I).Thirst = Skill_Range'Last then
                  Health_Level := Health_Level - Tired_Points;
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Update_Morale
                    (Ship => Player_Ship, Member_Index => I,
                     Value => -(Tired_Points));
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
                  if Health_Level < 1 then
                     Health_Level := Skill_Range'First;
                     Death_Reason :=
                       To_Unbounded_String(Source => "dehydration");
                  end if;
               end if;
            end if;
            if Health_Level = Skill_Range'First then
               if Death_Reason = Null_Unbounded_String then
                  Death_Reason := To_Unbounded_String(Source => "debugging");
               end if;
               Death
                 (Member_Index => I, Reason => Death_Reason,
                  Ship => Player_Ship);
               exit Update_Crew_Loop when I = 1;
            end if;
         end if;
         if Health_Level > Skill_Range'First then
            Player_Ship.Crew.Update_Element
              (Index => I, Process => Update_Member'Access);
            I := I + 1;
         end if;
      end loop Update_Crew_Loop;
   end Update_Crew;

   procedure Wait_For_Rest is
      use Ships.Movement;

      Cabin_Index: Modules_Container.Extended_Index := 0;
      Time_Needed, Temp_Time_Needed: Natural := 0;
      Damage: Damage_Factor := 0.0;
      Cabin_Bonus: Natural := 0;
   begin
      Wait_For_Rest_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Tired > 0 and
           Player_Ship.Crew(I).Order = REST then
            Temp_Time_Needed := 0;
            Cabin_Index :=
              Find_Cabin
                (Member_Index => Crew_Container.To_Index(Position => I));
            if Cabin_Index > 0 then
               Damage :=
                 1.0 -
                 Damage_Factor
                   (Float(Player_Ship.Modules(Cabin_Index).Durability) /
                    Float(Player_Ship.Modules(Cabin_Index).Max_Durability));
               Cabin_Bonus :=
                 Player_Ship.Modules(Cabin_Index).Cleanliness -
                 Natural
                   (Float(Player_Ship.Modules(Cabin_Index).Cleanliness) *
                    Float(Damage));
               if Cabin_Bonus = 0 then
                  Cabin_Bonus := 1;
               end if;
               Temp_Time_Needed :=
                 (Player_Ship.Crew(I).Tired / Cabin_Bonus) * 15;
               if Temp_Time_Needed = 0 then
                  Temp_Time_Needed := 15;
               end if;
            else
               Temp_Time_Needed := Player_Ship.Crew(I).Tired * 15;
            end if;
            Temp_Time_Needed := Temp_Time_Needed + 15;
            if Temp_Time_Needed > Time_Needed then
               Time_Needed := Temp_Time_Needed;
            end if;
         end if;
      end loop Wait_For_Rest_Loop;
      if Time_Needed > 0 then
         Update_Game(Minutes => Time_Needed);
         Wait_In_Place(Minutes => Time_Needed);
      end if;
   end Wait_For_Rest;

   function Get_Skill_Level_Name(Skill_Level: Skill_Range) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Positive'Image(Skill_Level);
      end if;
      case Skill_Level is
         when 0 =>
            return "Untrained";
         when 1 .. 10 =>
            return "Beginner";
         when 11 .. 20 =>
            return "Novice";
         when 21 .. 30 =>
            return "Apprentice";
         when 31 .. 40 =>
            return "Practitioner";
         when 41 .. 50 =>
            return "Competent";
         when 51 .. 60 =>
            return "Respected";
         when 61 .. 70 =>
            return "Renowned";
         when 71 .. 80 =>
            return "Master";
         when 81 .. 90 =>
            return "Grand-Master";
         when 91 .. 99 =>
            return "Legendary";
         when others =>
            return "Ultimate";
      end case;
   end Get_Skill_Level_Name;

   function Get_Attribute_Level_Name
     (Attribute_Level: Positive) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Positive'Image(Attribute_Level);
      end if;
      case Attribute_Level is
         when 1 .. 5 =>
            return "Very low";
         when 6 .. 10 =>
            return "Low";
         when 11 .. 15 =>
            return "Below average";
         when 16 .. 30 =>
            return "Average";
         when 31 .. 35 =>
            return "Above average";
         when 36 .. 40 =>
            return "High";
         when 41 .. 49 =>
            return "Very high";
         when others =>
            return "Outstanding";
      end case;
   end Get_Attribute_Level_Name;

   procedure Daily_Payment is
      use Bases;
      use Tiny_String;

      Money_Index_2: constant Inventory_Container.Extended_Index :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Pay_Message: Unbounded_String := Null_Unbounded_String;
      Member_Index: Crew_Container.Extended_Index;
      Have_Money: Boolean := True;
      Money_Needed: Natural := 0;
   begin
      Member_Index := 1;
      Daily_Payment_Loop :
      for Member of Player_Ship.Crew loop
         if Member.Payment(1) > 0 then
            if Money_Index_2 = 0 and Have_Money then
               Add_Message
                 (Message =>
                    "You don't have any " & To_String(Source => Money_Name) &
                    " to pay your crew members.",
                  M_Type => TRADEMESSAGE, Color => RED);
               Have_Money := False;
            end if;
            if Have_Money then
               if Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Money_Index_2)
                   .Amount <
                 Member.Payment(1) then
                  Money_Needed :=
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Money_Index_2)
                      .Amount;
                  Update_Cargo
                    (Ship => Player_Ship, Proto_Index => Money_Index,
                     Amount => 0 - Money_Needed);
                  Add_Message
                    (Message =>
                       "You don't have enough " &
                       To_String(Source => Money_Name) &
                       " to pay your crew members.",
                     M_Type => TRADEMESSAGE, Color => RED);
                  Have_Money := False;
               end if;
               if Have_Money then
                  Update_Cargo
                    (Ship => Player_Ship, Cargo_Index => Money_Index_2,
                     Amount => 0 - Member.Payment(1));
                  Pay_Message :=
                    To_Unbounded_String(Source => "You pay ") &
                    To_String(Source => Member.Name);
                  if Member.Gender = 'M' then
                     Append(Source => Pay_Message, New_Item => " his ");
                  else
                     Append(Source => Pay_Message, New_Item => " her ");
                  end if;
                  Append(Source => Pay_Message, New_Item => "daily payment.");
                  Add_Message
                    (Message => To_String(Source => Pay_Message),
                     M_Type => TRADEMESSAGE);
                  Update_Morale
                    (Ship => Player_Ship, Member_Index => Member_Index,
                     Value => Get_Random(Min => 1, Max => 5));
               end if;
            end if;
            if not Have_Money then
               Update_Morale
                 (Ship => Player_Ship, Member_Index => Member_Index,
                  Value => Get_Random(Min => -50, Max => -10));
            end if;
         end if;
         Member_Index := Member_Index + 1;
      end loop Daily_Payment_Loop;
      Member_Index := 1;
      Update_Contracts_Loop :
      while Member_Index <= Player_Ship.Crew.Last_Index loop
         if Player_Ship.Crew(Member_Index).Contract_Length > 0 then
            Player_Ship.Crew(Member_Index).Contract_Length :=
              Player_Ship.Crew(Member_Index).Contract_Length - 1;
            if Player_Ship.Crew(Member_Index).Contract_Length = 0 then
               Add_Message
                 (Message =>
                    "Your contract with " &
                    To_String(Source => Player_Ship.Crew(Member_Index).Name) &
                    " has ended.",
                  M_Type => TRADEMESSAGE, Color => RED);
               if Player_Ship.Speed = DOCKED then
                  Delete_Member
                    (Member_Index => Member_Index, Ship => Player_Ship);
                  Sky_Bases
                    (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index)
                    .Population :=
                    Sky_Bases
                      (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                         .Base_Index)
                      .Population +
                    1;
                  Member_Index := Member_Index - 1;
               else
                  Player_Ship.Crew(Member_Index).Orders := (others => 0);
                  Give_Orders
                    (Ship => Player_Ship, Member_Index => Member_Index,
                     Given_Order => REST);
               end if;
            end if;
         end if;
         Member_Index := Member_Index + 1;
      end loop Update_Contracts_Loop;
   end Daily_Payment;

   function Get_Training_Tool_Quality
     (Member_Index, Skill_Index: Positive) return Positive is
      Tool_Quality: Positive := 100;
   begin
      Skill_Loop :
      for Skill of Player_Ship.Crew(Member_Index).Skills loop
         if Positive(Skill.Index) = Skill_Index then
            Tool_Quality_Loop :
            for Quality of SkillsData_Container.Element
              (Container => Skills_List,
               Index => Skills_Amount_Range(Skill_Index))
              .Tools_Quality loop
               if Skill.Level <= Quality.Level then
                  Tool_Quality := Quality.Quality;
                  exit Skill_Loop;
               end if;
            end loop Tool_Quality_Loop;
         end if;
      end loop Skill_Loop;
      return Tool_Quality;
   end Get_Training_Tool_Quality;

   function Member_To_Nim(Member: Member_Data) return Nim_Member_Data is
      use Tiny_String;
      Nim_Member: Nim_Member_Data :=
        (Attributes => (others => (others => 0)),
         Skills => (others => (others => 0)),
         Name => New_String(Str => To_String(Source => Member.Name)),
         Gender => Member.Gender, Health => Member.Health,
         Tired => Member.Tired, Hunger => Member.Hunger,
         Thirst => Member.Thirst, Order => Crew_Orders'Pos(Member.Order),
         Previous_Order => Crew_Orders'Pos(Member.Previous_Order),
         Order_Time => Member.Order_Time, Orders => Member.Orders,
         Equipment => (others => 0), Payment => Member.Payment,
         Contract_Length => Member.Contract_Length, Morale => Member.Morale,
         Loyalty => Member.Loyalty, Home_Base => Member.Home_Base,
         Faction => New_String(Str => To_String(Source => Member.Faction)));
   begin
      Convert_Equipment_Loop :
      for I in Member.Equipment'Range loop
         Nim_Member.Equipment(Equipment_Locations'Pos(I)) :=
           Member.Equipment(I);
      end loop Convert_Equipment_Loop;
      Convert_Atrributes_Loop :
      for I in Member.Attributes'Range loop
         Nim_Member.Attributes(I, 1) := Member.Attributes(I).Level;
         Nim_Member.Attributes(I, 2) := Member.Attributes(I).Experience;
      end loop Convert_Atrributes_Loop;
      Convert_Skills_Loop :
      for I in
        Skills_Container.First_Index(Container => Member.Skills) ..
          Skills_Container.Last_Index(Container => Member.Skills) loop
         Convert_Skill_Block :
         declare
            Skill: constant Skill_Info :=
              Skills_Container.Element(Container => Member.Skills, Index => I);
         begin
            Nim_Member.Skills(Integer(I), 1) := Integer(Skill.Index);
            Nim_Member.Skills(Integer(I), 2) := Skill.Level;
            Nim_Member.Skills(Integer(I), 3) := Skill.Experience;
         end Convert_Skill_Block;
      end loop Convert_Skills_Loop;
      return Nim_Member;
   end Member_To_Nim;

end Crew;
