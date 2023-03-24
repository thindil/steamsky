--    Copyright 2017-2023 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew.Inventory; use Crew.Inventory;
with Config; use Config;
with ShipModules; use ShipModules;

package body Ships.Upgrade is

   procedure Start_Upgrading
     (Module_Index: Modules_Container.Extended_Index;
      Upgrade_Type: Positive) is
      use Tiny_String;

      Local_Max_Value, Upgrade_Progress: Natural := 0;
      Upgrade_Action: Ship_Upgrade := NONE;
   begin
      if Player_Ship.Modules(Module_Index).Durability = 0 and
        Upgrade_Type /= 3 then
         raise Ship_Upgrade_Error
           with "You can't upgrade " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) &
           " because it's destroyed.";
      end if;
      case Upgrade_Type is
         when 1 => -- Upgrade durability
            Local_Max_Value :=
              Natural
                (Float
                   (Get_Module
                      (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Durability) *
                 1.5);
            if Player_Ship.Modules(Module_Index).Max_Durability =
              Local_Max_Value then
               raise Ship_Upgrade_Error
                 with "You can't further improve the durability of " &
                 To_String(Source => Player_Ship.Modules(Module_Index).Name) &
                 ".";
            end if;
            Upgrade_Action := DURABILITY;
            Upgrade_Progress :=
              Integer
                (Float
                   (Get_Module
                      (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Durability) *
                 New_Game_Settings.Upgrade_Cost_Bonus);
         when 2 => -- Upgrade various max value of selected module
            Local_Max_Value :=
              Natural
                (Float
                   (Get_Module
                      (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Max_Value) *
                 1.5);
            case Get_Module
              (Index => Player_Ship.Modules(Module_Index).Proto_Index)
              .M_Type is
               when ENGINE =>
                  if Player_Ship.Modules(Module_Index).Power =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the power of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value /
                          20) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when CABIN =>
                  if Player_Ship.Modules(Module_Index).Quality =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the quality of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when GUN | BATTERING_RAM =>
                  Update_Damage_Block :
                  declare
                     Damage: constant Positive :=
                       (if Player_Ship.Modules(Module_Index).M_Type = GUN then
                          Player_Ship.Modules(Module_Index).Damage
                        else Player_Ship.Modules(Module_Index).Damage2);
                  begin
                     if Damage = Local_Max_Value then
                        raise Ship_Upgrade_Error
                          with "You can't further improve the damage of " &
                          To_String
                            (Source =>
                               Player_Ship.Modules(Module_Index).Name) &
                          ".";
                     end if;
                  end Update_Damage_Block;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value *
                          2) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when HULL =>
                  if Player_Ship.Modules(Module_Index).Max_Modules =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further enlarge the size of" &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value *
                          40) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when HARPOON_GUN =>
                  if Player_Ship.Modules(Module_Index).Duration =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the strength of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value *
                          10) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    " can't be upgraded in that way.";
            end case;
            Upgrade_Action := MAX_VALUE;
         when 3 => -- Upgrade various value of selected module
            case Get_Module
              (Index => Player_Ship.Modules(Module_Index).Proto_Index)
              .M_Type is
               when ENGINE =>
                  Local_Max_Value :=
                    Natural
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Value) /
                       2.0);
                  if Local_Max_Value < 1 then
                     Local_Max_Value := 1;
                  end if;
                  if Player_Ship.Modules(Module_Index).Fuel_Usage =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further reduce the fuel usage of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Value *
                          20) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    " can't be upgraded in that way.";
            end case;
            Upgrade_Action := VALUE;
         when 4 => -- Continue previous upgrade
            if Player_Ship.Modules(Module_Index).Upgrade_Action = NONE then
               raise Ship_Upgrade_Error
                 with To_String
                   (Source => Player_Ship.Modules(Module_Index).Name) &
                 " doesn't have any upgrade set yet.";
            end if;
            Upgrade_Action := Player_Ship.Modules(Module_Index).Upgrade_Action;
         when others =>
            return;
      end case;
      Find_Materials_Block :
      declare
         Material_Index: constant Inventory_Container.Extended_Index :=
           Find_Item
             (Inventory => Player_Ship.Cargo,
              Item_Type =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Repair_Material);
      begin
         if Material_Index = 0 then
            Materials_Loop :
            for I in 1 .. Get_Proto_Amount loop
               if To_String(Source => Get_Proto_Item(Index => I).I_Type) =
                 To_String
                   (Source =>
                      Get_Module
                        (Index =>
                           Player_Ship.Modules(Module_Index).Proto_Index)
                        .Repair_Material) then
                  raise Ship_Upgrade_Error
                    with "You don't have the " &
                    To_String(Source => Get_Proto_Item(Index => I).Name) &
                    " to upgrade " &
                    To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    ".";
               end if;
            end loop Materials_Loop;
         end if;
      end Find_Materials_Block;
      Player_Ship.Upgrade_Module := Module_Index;
      if Player_Ship.Modules(Module_Index).Upgrade_Action /=
        Upgrade_Action then
         Player_Ship.Modules(Module_Index).Upgrade_Progress :=
           Integer
             (Float(Upgrade_Progress) * New_Game_Settings.Upgrade_Cost_Bonus);
         if Player_Ship.Modules(Module_Index).Upgrade_Progress = 0 then
            Player_Ship.Modules(Module_Index).Upgrade_Progress := 1;
         end if;
         Player_Ship.Modules(Module_Index).Upgrade_Action := Upgrade_Action;
      end if;
      Add_Message
        (Message =>
           "You set the " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) &
           " to upgrade.",
         M_Type => ORDERMESSAGE);
   end Start_Upgrading;

   procedure Upgrade_Ship(Minutes: Positive) is
      use Tiny_String;

      Result_Amount, Upgrade_Points, Upgrade_Progress, Material_Cost,
      Local_Max_Value: Natural := 0;
      Upgrade_Value: Positive := 1;
      Weight_Gain: Natural := 0;
      Times: Natural := 0;
      Order_Time, Current_Minutes: Integer;
      Upgraded_Module: Module_Data;
      Upgrade_Material, Upgrade_Tools: Inventory_Container.Extended_Index := 0;
      Worker_Index: Crew_Container.Extended_Index;
      procedure Find_Mats_And_Tools is
      begin
         Upgrade_Tools :=
           Find_Tools
             (Member_Index => Worker_Index, Item_Type => Repair_Tools,
              Order => UPGRADING);
         Upgrade_Material :=
           Find_Item
             (Inventory => Player_Ship.Cargo,
              Item_Type =>
                Get_Module(Index => Upgraded_Module.Proto_Index)
                  .Repair_Material);
      end Find_Mats_And_Tools;
      procedure Max_Upgrade_Reached(Message_Text: String) is
      begin
         Add_Message
           (Message =>
              Message_Text & To_String(Source => Upgraded_Module.Name) & ".",
            M_Type => ORDERMESSAGE, Color => YELLOW);
         Upgraded_Module.Upgrade_Progress := 0;
         Upgraded_Module.Upgrade_Action := NONE;
         Player_Ship.Modules(Player_Ship.Upgrade_Module) := Upgraded_Module;
         Player_Ship.Upgrade_Module := 0;
         Give_Orders
           (Ship => Player_Ship, Member_Index => Worker_Index,
            Given_Order => REST);
      end Max_Upgrade_Reached;
   begin
      if Player_Ship.Upgrade_Module = 0 then
         return;
      end if;
      Worker_Index := Find_Member(Order => UPGRADING);
      if Worker_Index = 0 then
         return;
      end if;
      Upgraded_Module := Player_Ship.Modules(Player_Ship.Upgrade_Module);
      Current_Minutes := Minutes;
      Order_Time := Player_Ship.Crew(Worker_Index).Order_Time;
      if Upgraded_Module.Durability = 0 then
         Add_Message
           (Message =>
              To_String(Source => Player_Ship.Crew(Worker_Index).Name) &
              " stops upgrading " & To_String(Source => Upgraded_Module.Name) &
              " because it's destroyed.",
            M_Type => ORDERMESSAGE, Color => RED);
         Give_Orders
           (Ship => Player_Ship, Member_Index => Worker_Index,
            Given_Order => REST);
         return;
      end if;
      Count_Time_Loop :
      while Current_Minutes > 0 loop
         if Current_Minutes >= Order_Time then
            Current_Minutes := Current_Minutes - Order_Time;
            Times := Times + 1;
            Order_Time := 15;
         else
            Order_Time := Order_Time - Current_Minutes;
            Current_Minutes := 0;
         end if;
      end loop Count_Time_Loop;
      Player_Ship.Crew(Worker_Index).Order_Time := Order_Time;
      if Times = 0 then
         return;
      end if;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Upgrade_Points :=
        ((Get_Skill_Level
            (Member => Player_Ship.Crew(Worker_Index),
             Skill_Index =>
               Get_Module(Index => Upgraded_Module.Proto_Index).Repair_Skill) /
          10) *
         Times) +
        Times;
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Upgrade_Loop :
      while Upgrade_Points > 0 and Upgraded_Module.Upgrade_Progress > 0 loop
         Result_Amount := Upgrade_Points;
         if Result_Amount > Upgraded_Module.Upgrade_Progress then
            Result_Amount := Upgraded_Module.Upgrade_Progress;
         end if;
         Find_Mats_And_Tools;
         if Upgrade_Material = 0 then
            Add_Message
              (Message =>
                 "You don't have enough materials to upgrade " &
                 To_String(Source => Upgraded_Module.Name),
               M_Type => ORDERMESSAGE, Color => RED);
            Give_Orders
              (Ship => Player_Ship, Member_Index => Worker_Index,
               Given_Order => REST);
            exit Upgrade_Loop;
         end if;
         if Upgrade_Tools = 0 then
            Add_Message
              (Message =>
                 "You don't have the repair tool to upgrade " &
                 To_String(Source => Upgraded_Module.Name),
               M_Type => ORDERMESSAGE, Color => RED);
            Give_Orders
              (Ship => Player_Ship, Member_Index => Worker_Index,
               Given_Order => REST);
            exit Upgrade_Loop;
         end if;
         case Upgraded_Module.Upgrade_Action is
            when MAX_VALUE =>
               case Upgraded_Module.M_Type is
                  when ENGINE =>
                     if Result_Amount >
                       Inventory_Container.Element
                           (Container => Player_Ship.Cargo,
                            Index => Upgrade_Material)
                           .Amount *
                         200 then
                        Result_Amount :=
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Upgrade_Material)
                            .Amount *
                          200;
                     end if;
                     Material_Cost := Result_Amount / 200;
                  when CABIN =>
                     if Result_Amount >
                       Inventory_Container.Element
                           (Container => Player_Ship.Cargo,
                            Index => Upgrade_Material)
                           .Amount *
                         20 then
                        Result_Amount :=
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Upgrade_Material)
                            .Amount *
                          20;
                     end if;
                     Material_Cost := Result_Amount / 20;
                  when GUN | BATTERING_RAM | HARPOON_GUN =>
                     if Result_Amount >
                       Inventory_Container.Element
                           (Container => Player_Ship.Cargo,
                            Index => Upgrade_Material)
                           .Amount *
                         10 then
                        Result_Amount :=
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Upgrade_Material)
                            .Amount *
                          10;
                     end if;
                     Material_Cost := Result_Amount / 10;
                  when HULL =>
                     if Result_Amount >
                       Inventory_Container.Element
                           (Container => Player_Ship.Cargo,
                            Index => Upgrade_Material)
                           .Amount *
                         2 then
                        Result_Amount :=
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Upgrade_Material)
                            .Amount *
                          2;
                     end if;
                     Material_Cost := Result_Amount / 2;
                  when others =>
                     if Result_Amount >
                       Inventory_Container.Element
                         (Container => Player_Ship.Cargo,
                          Index => Upgrade_Material)
                         .Amount then
                        Result_Amount :=
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Upgrade_Material)
                            .Amount;
                     end if;
                     Material_Cost := Result_Amount;
               end case;
            when DURABILITY =>
               if Result_Amount >
                 Inventory_Container.Element
                     (Container => Player_Ship.Cargo,
                      Index => Upgrade_Material)
                     .Amount *
                   10 then
                  Result_Amount :=
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo,
                       Index => Upgrade_Material)
                      .Amount *
                    10;
               end if;
               Material_Cost := Result_Amount / 10;
            when others =>
               if Result_Amount >
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Upgrade_Material)
                   .Amount then
                  Result_Amount :=
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo,
                       Index => Upgrade_Material)
                      .Amount;
               end if;
               Material_Cost := Result_Amount;
         end case;
         if Material_Cost < Times then
            Material_Cost := Times;
         end if;
         if Material_Cost >
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Upgrade_Material)
             .Amount then
            Material_Cost :=
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Upgrade_Material)
                .Amount;
         end if;
         Gain_Exp
           (Amount => Result_Amount,
            Skill_Number =>
              Get_Module(Index => Upgraded_Module.Proto_Index).Repair_Skill,
            Crew_Index => Worker_Index);
         Damage_Item
           (Inventory => Player_Ship.Crew(Worker_Index).Inventory,
            Item_Index => Upgrade_Tools,
            Skill_Level =>
              Get_Skill_Level
                (Member => Player_Ship.Crew(Worker_Index),
                 Skill_Index =>
                   Get_Module(Index => Upgraded_Module.Proto_Index)
                     .Repair_Skill),
            Member_Index => Worker_Index, Ship => Player_Ship);
         Find_Mats_And_Tools;
         Upgrade_Progress := Upgraded_Module.Upgrade_Progress - Result_Amount;
         Upgrade_Points := Upgrade_Points - Result_Amount;
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Update_Cargo
           (Ship => Player_Ship,
            Proto_Index =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Upgrade_Material)
                .Proto_Index,
            Amount => -(Material_Cost));
         --## rule on SIMPLIFIABLE_EXPRESSIONS
         if Upgrade_Progress = 0 then
            Weight_Gain :=
              Get_Module(Index => Upgraded_Module.Proto_Index).Weight /
              Get_Module(Index => Upgraded_Module.Proto_Index).Durability;
            if Weight_Gain < 1 then
               Weight_Gain := 1;
            end if;
            case Upgraded_Module.Upgrade_Action is
               when DURABILITY =>
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  if
                    (Get_Module(Index => Upgraded_Module.Proto_Index)
                       .Durability /
                     20) >
                    0 then
                     Upgraded_Module.Max_Durability :=
                       Upgraded_Module.Max_Durability +
                       (Get_Module(Index => Upgraded_Module.Proto_Index)
                          .Durability /
                        20);
                     Upgraded_Module.Weight :=
                       Upgraded_Module.Weight +
                       (Weight_Gain *
                        (Get_Module(Index => Upgraded_Module.Proto_Index)
                           .Durability /
                         20));
                  else
                     Upgraded_Module.Max_Durability :=
                       Upgraded_Module.Max_Durability + 1;
                     Upgraded_Module.Weight :=
                       Upgraded_Module.Weight + Weight_Gain;
                  end if;
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
                  Add_Message
                    (Message =>
                       To_String
                         (Source => Player_Ship.Crew(Worker_Index).Name) &
                       " has upgraded the durability of " &
                       To_String(Source => Upgraded_Module.Name) & ".",
                     M_Type => ORDERMESSAGE, Color => GREEN);
                  Local_Max_Value :=
                    Positive
                      (Float
                         (Get_Module(Index => Upgraded_Module.Proto_Index)
                            .Durability) *
                       1.5);
                  if Upgraded_Module.Max_Durability = Local_Max_Value then
                     Max_Upgrade_Reached
                       (Message_Text =>
                          "You've reached the maximum durability for ");
                     return;
                  end if;
                  Upgraded_Module.Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module(Index => Upgraded_Module.Proto_Index)
                            .Durability) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when MAX_VALUE =>
                  case Upgraded_Module.M_Type is
                     when HULL =>
                        Weight_Gain := Weight_Gain * 10;
                        Upgraded_Module.Max_Modules :=
                          Upgraded_Module.Max_Modules + 1;
                        Upgrade_Value := Upgraded_Module.Max_Modules;
                     when ENGINE =>
                        Weight_Gain :=
                          (Get_Module(Index => Upgraded_Module.Proto_Index)
                             .Max_Value /
                           40);
                        Upgraded_Module.Power :=
                          Upgraded_Module.Power +
                          (Get_Module(Index => Upgraded_Module.Proto_Index)
                             .Max_Value /
                           20);
                        Upgrade_Value := Upgraded_Module.Power;
                     when CABIN =>
                        Upgraded_Module.Quality :=
                          Upgraded_Module.Quality +
                          (Get_Module(Index => Upgraded_Module.Proto_Index)
                             .Max_Value /
                           20);
                        Upgrade_Value := Upgraded_Module.Quality;
                     when GUN =>
                        if
                          (Get_Module(Index => Upgraded_Module.Proto_Index)
                             .Max_Value /
                           20) >
                          0 then
                           Upgraded_Module.Damage :=
                             Upgraded_Module.Damage +
                             (Get_Module(Index => Upgraded_Module.Proto_Index)
                                .Max_Value /
                              20);
                        else
                           Upgraded_Module.Damage :=
                             Upgraded_Module.Damage + 1;
                        end if;
                        Upgrade_Value := Upgraded_Module.Damage;
                     when BATTERING_RAM =>
                        if
                          (Get_Module(Index => Upgraded_Module.Proto_Index)
                             .Max_Value /
                           20) >
                          0 then
                           Upgraded_Module.Damage2 :=
                             Upgraded_Module.Damage2 +
                             (Get_Module(Index => Upgraded_Module.Proto_Index)
                                .Max_Value /
                              20);
                        else
                           Upgraded_Module.Damage2 :=
                             Upgraded_Module.Damage2 + 1;
                        end if;
                        Upgrade_Value := Upgraded_Module.Damage2;
                     when HARPOON_GUN =>
                        if
                          (Get_Module(Index => Upgraded_Module.Proto_Index)
                             .Max_Value /
                           20) >
                          0 then
                           Upgraded_Module.Duration :=
                             Upgraded_Module.Duration +
                             (Get_Module(Index => Upgraded_Module.Proto_Index)
                                .Max_Value /
                              20);
                        else
                           Upgraded_Module.Duration :=
                             Upgraded_Module.Duration + 1;
                        end if;
                        Upgrade_Value := Upgraded_Module.Duration;
                     when others =>
                        null;
                  end case;
                  Upgraded_Module.Weight :=
                    Upgraded_Module.Weight + Weight_Gain;
                  Add_Message
                    (Message =>
                       To_String
                         (Source => Player_Ship.Crew(Worker_Index).Name) &
                       " has upgraded " &
                       To_String(Source => Upgraded_Module.Name) & ".",
                     M_Type => ORDERMESSAGE, Color => GREEN);
                  Local_Max_Value :=
                    Positive
                      (Float
                         (Get_Module(Index => Upgraded_Module.Proto_Index)
                            .Max_Value) *
                       1.5);
                  if Upgrade_Value >= Local_Max_Value then
                     Max_Upgrade_Reached
                       (Message_Text =>
                          "You've reached the maximum upgrade for ");
                     return;
                  else
                     case Get_Module(Index => Upgraded_Module.Proto_Index)
                       .M_Type is
                        when ENGINE =>
                           Upgraded_Module.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Get_Module
                                     (Index =>
                                        Player_Ship.Modules
                                          (Player_Ship.Upgrade_Module)
                                          .Proto_Index)
                                     .Max_Value /
                                   20) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when HARPOON_GUN =>
                           Upgraded_Module.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Get_Module
                                     (Index =>
                                        Player_Ship.Modules
                                          (Player_Ship.Upgrade_Module)
                                          .Proto_Index)
                                     .Max_Value *
                                   10) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when GUN | BATTERING_RAM =>
                           Upgraded_Module.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Get_Module
                                     (Index =>
                                        Player_Ship.Modules
                                          (Player_Ship.Upgrade_Module)
                                          .Proto_Index)
                                     .Max_Value *
                                   2) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when CABIN =>
                           Upgraded_Module.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Get_Module
                                     (Index =>
                                        Player_Ship.Modules
                                          (Player_Ship.Upgrade_Module)
                                          .Proto_Index)
                                     .Max_Value) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when HULL =>
                           Upgraded_Module.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Get_Module
                                     (Index =>
                                        Player_Ship.Modules
                                          (Player_Ship.Upgrade_Module)
                                          .Proto_Index)
                                     .Max_Value *
                                   40) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when others =>
                           null;
                     end case;
                     if Upgraded_Module.Upgrade_Progress = 0 then
                        Upgraded_Module.Upgrade_Progress := 1;
                     end if;
                  end if;
               when VALUE =>
                  if Upgraded_Module.M_Type = ENGINE then
                     Weight_Gain := Weight_Gain * 10;
                     Upgraded_Module.Fuel_Usage :=
                       Upgraded_Module.Fuel_Usage - 1;
                     Upgrade_Value := Upgraded_Module.Fuel_Usage;
                  end if;
                  Upgraded_Module.Weight :=
                    Upgraded_Module.Weight + Weight_Gain;
                  Add_Message
                    (Message =>
                       To_String
                         (Source => Player_Ship.Crew(Worker_Index).Name) &
                       " has upgraded " &
                       To_String(Source => Upgraded_Module.Name) & ".",
                     M_Type => ORDERMESSAGE, Color => GREEN);
                  Local_Max_Value :=
                    Natural
                      (Float
                         (Get_Module(Index => Upgraded_Module.Proto_Index)
                            .Value) /
                       2.0);
                  if Local_Max_Value < 1 then
                     Local_Max_Value := 1;
                  end if;
                  if Upgrade_Value = Local_Max_Value then
                     Max_Upgrade_Reached
                       (Message_Text =>
                          "You've reached the maximum upgrade for ");
                     return;
                  else
                     case Get_Module(Index => Upgraded_Module.Proto_Index)
                       .M_Type is
                        when ENGINE =>
                           Upgraded_Module.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Get_Module
                                     (Index =>
                                        Player_Ship.Modules
                                          (Player_Ship.Upgrade_Module)
                                          .Proto_Index)
                                     .Value *
                                   20) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                           if Upgraded_Module.Upgrade_Progress = 0 then
                              Upgraded_Module.Upgrade_Progress := 1;
                           end if;
                        when others =>
                           null;
                     end case;
                  end if;
               when others =>
                  null;
            end case;
         else
            Upgraded_Module.Upgrade_Progress := Upgrade_Progress;
         end if;
      end loop Upgrade_Loop;
      Player_Ship.Modules(Player_Ship.Upgrade_Module) := Upgraded_Module;
   end Upgrade_Ship;

end Ships.Upgrade;
