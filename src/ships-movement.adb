--    Copyright 2017-2022 Bartek thindil Jasicki
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

with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Statistics; use Statistics;
with Maps; use Maps;
with Messages; use Messages;
with Config; use Config;
with Bases; use Bases;
with Events; use Events;
with Utils; use Utils;
with Factions; use Factions;
with Game.SaveLoad; use Game.SaveLoad;

package body Ships.Movement is

   -- ****it* SMovement/SMovement.Speed_Type
   -- FUNCTION
   -- Used in counting ships speed
   -- SOURCE
   type Speed_Type is digits 2;
   -- ****

   -- ****if* SMovement/SMovement.Have_Order_Requirements
   -- FUNCTION
   -- Check if all requirements for movement orders are valid
   -- RESULT
   -- Empty string if everything is ok, otherwise message what is missing
   -- SOURCE
   function Have_Order_Requirements return String is
      -- ****
      Have_Cockpit, Have_Engine, Have_Pilot, Have_Engineer: Boolean := False;
   begin
      Find_Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = COCKPIT and Module.Durability > 0 then
            Have_Cockpit := True;
         elsif Module.M_Type = ENGINE
           and then (Module.Durability > 1 and not Module.Disabled) then
            Have_Engine := True;
         end if;
         exit Find_Modules_Loop when Have_Engine and Have_Cockpit;
      end loop Find_Modules_Loop;
      if not Have_Engine then
         return
           "You don't have a working engine on your ship or all of the engines are destroyed.";
      end if;
      if not Have_Cockpit then
         return
           "You don't have a cockpit on your ship or the cockpit is destroyed.";
      end if;
      if Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) then
         Have_Pilot := True;
         Have_Engineer := True;
      end if;
      Find_Members_Loop :
      for Member of Player_Ship.Crew loop
         if Member.Order = PILOT then
            Have_Pilot := True;
         elsif Member.Order = ENGINEER then
            Have_Engineer := True;
         end if;
         exit Find_Members_Loop when Have_Pilot and Have_Engineer;
      end loop Find_Members_Loop;
      if not Have_Pilot then
         return "You don't have a pilot on duty.";
      end if;
      if not Have_Engineer then
         return "You don't have an engineer on duty.";
      end if;
      return "";
   end Have_Order_Requirements;

   function Move_Ship
     (X, Y: Integer; Message: in out Unbounded_String) return Natural is
      New_X, New_Y: Integer;
      Time_Passed, Fuel_Needed: Integer := 0;
      Speed: Speed_Type;
      Fuel_Index: Inventory_Container.Extended_Index;
      function Need_Rest(Order: Crew_Orders) return Boolean is
         Member_Index: Crew_Container.Extended_Index;
      begin
         Member_Index := Find_Member(Order => Order);
         if Member_Index = 0 then
            Find_Member_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Previous_Order = Order then
                  return True;
               end if;
            end loop Find_Member_Loop;
         end if;
         return False;
      end Need_Rest;
   begin
      case Player_Ship.Speed is
         when DOCKED =>
            Message :=
              To_Unbounded_String
                (Source => "First you must undock your ship from the base.");
            return 0;
         when FULL_STOP =>
            Message :=
              To_Unbounded_String
                (Source => "First you must set the speed of your ship.");
            return 0;
         when others =>
            null;
      end case;
      Message := To_Unbounded_String(Source => Have_Order_Requirements);
      if Length(Source => Message) > 0 then
         return 0;
      end if;
      Fuel_Index :=
        Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
      if Fuel_Index = 0 then
         Message := To_Unbounded_String(Source => "You don't have any fuel.");
         return 0;
      end if;
      Fuel_Needed := Count_Fuel_Needed;
      if Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Fuel_Index)
          .Amount <
        abs Fuel_Needed then
         Message :=
           To_Unbounded_String
             (Source =>
                "You don't have enough fuel (" &
                To_String
                  (Source =>
                     Items_List
                       (Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => Fuel_Index)
                          .Proto_Index)
                       .Name) &
                ").");
         return 0;
      end if;
      Speed := (Speed_Type(Real_Speed(Ship => Player_Ship)) / 1_000.0);
      if Speed < 0.5 then
         Message :=
           To_Unbounded_String
             (Source => "You can't fly because your ship is overloaded.");
         return 0;
      end if;
      New_X := Player_Ship.Sky_X + X;
      New_Y := Player_Ship.Sky_Y + Y;
      if New_X < 1 or New_X > 1_024 or New_Y < 1 or New_Y > 1_024 then
         return 0;
      end if;
      Player_Ship.Sky_X := New_X;
      Player_Ship.Sky_Y := New_Y;
      Update_Cargo
        (Ship => Player_Ship,
         Proto_Index =>
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Fuel_Index)
             .Proto_Index,
         Amount => Fuel_Needed);
      Time_Passed := Integer(100.0 / Speed);
      if Time_Passed > 0 then
         case Player_Ship.Speed is
            when QUARTER_SPEED =>
               if Time_Passed < 60 then
                  Time_Passed := 60;
               end if;
            when HALF_SPEED =>
               if Time_Passed < 30 then
                  Time_Passed := 30;
               end if;
            when FULL_SPEED =>
               if Time_Passed < 15 then
                  Time_Passed := 15;
               end if;
            when others =>
               null;
         end case;
         Game_Stats.Distance_Traveled := Game_Stats.Distance_Traveled + 1;
         Update_Game(Minutes => Time_Passed);
         Fuel_Index :=
           Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
         if Fuel_Index = 0 then
            Add_Message
              (Message => "Ship falls from the sky due to a lack of fuel.",
               M_Type => OTHERMESSAGE, Color => RED);
            Death
              (Member_Index => 1,
               Reason => To_Unbounded_String(Source => "fall of the ship"),
               Ship => Player_Ship);
            return 0;
         end if;
      end if;
      if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) then
         if Need_Rest(Order => PILOT) then
            if not Game_Settings.Auto_Rest then
               return 6;
            end if;
            return 8;
         end if;
         if Need_Rest(Order => ENGINEER) then
            if not Game_Settings.Auto_Rest then
               return 7;
            end if;
            return 8;
         end if;
      end if;
      return 1;
   end Move_Ship;

   function Dock_Ship
     (Docking: Boolean; Escape: Boolean := False) return String is
      use Tiny_String;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Message: Unbounded_String;
   begin
      Message := To_Unbounded_String(Source => Have_Order_Requirements);
      if Length(Source => Message) > 0 then
         return To_String(Source => Message);
      end if;
      if Docking then
         if Sky_Bases(Base_Index).Population > 0 then
            Add_Message
              (Message =>
                 "Ship docked to base " &
                 To_String(Source => Sky_Bases(Base_Index).Name),
               M_Type => ORDERMESSAGE);
            if Game_Settings.Auto_Save = DOCK then
               Save_Game;
            end if;
            Crew_Resignation_Block :
            declare
               Member_Index: Positive := 1;
            begin
               Resign_Crew_Member_Loop :
               while Member_Index <= Player_Ship.Crew.Last_Index loop
                  if Player_Ship.Crew(Member_Index).Contract_Length = 0 then
                     Delete_Member
                       (Member_Index => Member_Index, Ship => Player_Ship);
                     Sky_Bases(Base_Index).Population :=
                       Sky_Bases(Base_Index).Population + 1;
                  elsif Player_Ship.Crew(Member_Index).Loyalty < 20 and
                    Get_Random
                        (Min => 0,
                         Max => Player_Ship.Crew(Member_Index).Loyalty) <
                      10 then
                     Add_Message
                       (Message =>
                          To_String
                            (Source => Player_Ship.Crew(Member_Index).Name) &
                          " resigns from working for you.",
                        M_Type => ORDERMESSAGE);
                     Delete_Member
                       (Member_Index => Member_Index, Ship => Player_Ship);
                     Sky_Bases(Base_Index).Population :=
                       Sky_Bases(Base_Index).Population + 1;
                     Drop_Morale_Loop :
                     for I in Player_Ship.Crew.Iterate loop
                        Update_Morale
                          (Ship => Player_Ship,
                           Member_Index =>
                             Crew_Container.To_Index(Position => I),
                           Value => Get_Random(Min => -5, Max => -1));
                     end loop Drop_Morale_Loop;
                  else
                     Member_Index := Member_Index + 1;
                  end if;
               end loop Resign_Crew_Member_Loop;
            end Crew_Resignation_Block;
            if Game_Settings.Auto_Ask_For_Bases then
               Ask_For_Bases;
            end if;
            if Game_Settings.Auto_Ask_For_Events then
               Ask_For_Events;
            end if;
         else
            Add_Message
              (Message =>
                 "Ship docked to base " &
                 To_String(Source => Sky_Bases(Base_Index).Name) & ".",
               M_Type => ORDERMESSAGE);
         end if;
         Player_Ship.Speed := DOCKED;
         Update_Game(Minutes => 10);
      else
         Player_Ship.Speed := Game_Settings.Undock_Speed;
         Check_Overload_Block :
         declare
            Speed: constant Speed_Type :=
              (Speed_Type(Real_Speed(Ship => Player_Ship)) / 1_000.0);
         begin
            if Speed < 0.5 then
               return "You can't undock because your ship is overloaded.";
            end if;
         end Check_Overload_Block;
         Player_Ship.Speed := DOCKED;
         if not Escape then
            if Sky_Bases(Base_Index).Population > 0 then
               Undock_From_Base_Block :
               declare
                  Money_Index_2: constant Inventory_Container.Extended_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo,
                       Proto_Index => Money_Index);
                  Docking_Cost: Natural;
                  Fuel_Index: Inventory_Container.Extended_Index;
                  Trader_Index: constant Crew_Container.Extended_Index :=
                    Find_Member(Order => TALK);
               begin
                  if Money_Index_2 = 0 then
                     return
                       "You can't undock from this base because you don't have any " &
                       To_String(Source => Money_Name) &
                       " to pay for docking.";
                  end if;
                  Count_Cost_Loop :
                  for Module of Player_Ship.Modules loop
                     if Module.M_Type = HULL then
                        Docking_Cost := Module.Max_Modules;
                        exit Count_Cost_Loop;
                     end if;
                  end loop Count_Cost_Loop;
                  Docking_Cost :=
                    Natural
                      (Float(Docking_Cost) *
                       Float(New_Game_Settings.Prices_Bonus));
                  if Docking_Cost = 0 then
                     Docking_Cost := 1;
                  end if;
                  Count_Price
                    (Price => Docking_Cost, Trader_Index => Trader_Index);
                  if Docking_Cost >
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Money_Index_2)
                      .Amount then
                     return
                       "You can't undock to this base because you don't have enough " &
                       To_String(Source => Money_Name) &
                       " to pay for docking.";
                  end if;
                  Update_Cargo
                    (Ship => Player_Ship, Cargo_Index => Money_Index_2,
                     Amount => (0 - Docking_Cost));
                  if Trader_Index > 0 then
                     Gain_Exp
                       (Amount => 1, Skill_Number => Talking_Skill,
                        Crew_Index => Trader_Index);
                  end if;
                  Fuel_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
                  if Fuel_Index = 0 then
                     return
                       "You can't undock from base because you don't have any fuel.";
                  end if;
                  Add_Message
                    (Message =>
                       "Ship undocked from base " &
                       To_String(Source => Sky_Bases(Base_Index).Name) &
                       ". You also paid" & Positive'Image(Docking_Cost) & " " &
                       To_String(Source => Money_Name) & " of docking fee.",
                     M_Type => ORDERMESSAGE);
               end Undock_From_Base_Block;
            else
               Check_Fuel_Block :
               declare
                  Fuel_Index: constant Inventory_Container.Extended_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
               begin
                  if Fuel_Index = 0 then
                     return
                       "You can't undock from base because you don't have any fuel.";
                  end if;
                  Add_Message
                    (Message =>
                       "Ship undocked from base " &
                       To_String(Source => Sky_Bases(Base_Index).Name) & ".",
                     M_Type => ORDERMESSAGE);
               end Check_Fuel_Block;
            end if;
         else
            Escape_From_Base_Block :
            declare
               Roll: constant Integer := Get_Random(Min => 1, Max => 100);
               Message_Text: Unbounded_String;
               Color: Message_Color := WHITE;
               Module_Index: Modules_Container.Extended_Index;
            begin
               Message_Text :=
                 To_Unbounded_String
                   (Source =>
                      "Ship escaped from base " &
                      To_String(Source => Sky_Bases(Base_Index).Name) &
                      " without paying.");
               case Roll is
                  when 1 .. 40 =>
                     Module_Index :=
                       Get_Random
                         (Min => Player_Ship.Modules.First_Index,
                          Max => Player_Ship.Modules.Last_Index);
                     Append
                       (Source => Message_Text,
                        New_Item =>
                          " But your ship (" &
                          To_String
                            (Source =>
                               Player_Ship.Modules(Module_Index).Name) &
                          ") takes damage.");
                     Color := RED;
                     Damage_Module
                       (Ship => Player_Ship, Module_Index => Module_Index,
                        Damage => Get_Random(Min => 1, Max => 30),
                        Death_Reason =>
                          "damage during escaping from the base");
                  when others =>
                     null;
               end case;
               Add_Message
                 (Message => To_String(Source => Message_Text),
                  M_Type => ORDERMESSAGE, Color => Color);
               Gain_Rep(Base_Index => Base_Index, Points => -(Get_Random(Min => 10, Max => 30)));
            end Escape_From_Base_Block;
         end if;
         if Player_Ship.Crew(1).Health > 0 then
            Player_Ship.Speed := Game_Settings.Undock_Speed;
            Update_Game(Minutes => 5);
            if Game_Settings.Auto_Save = UNDOCK then
               Save_Game;
            end if;
         end if;
      end if;
      return "";
   end Dock_Ship;

   function Change_Ship_Speed(Speed_Value: Ship_Speed) return String is
      Have_Engine: Boolean := False;
   begin
      Find_Engine_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = ENGINE
           and then (Module.Durability > 0 and not Module.Disabled) then
            Have_Engine := True;
            exit Find_Engine_Loop;
         end if;
      end loop Find_Engine_Loop;
      if not Have_Engine then
         return
           "You don't have a working engine on your ship or all of the engines are destroyed.";
      end if;
      if Find_Member(Order => ENGINEER) = 0 and
        not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) then
         return "You don't have an engineer on duty.";
      end if;
      Player_Ship.Speed := Speed_Value;
      return "";
   end Change_Ship_Speed;

   function Real_Speed
     (Ship: Ship_Record; Info_Only: Boolean := False) return Natural is
      Base_Speed, Speed: Natural := 0;
      Ship_Set_Speed: Ship_Speed;
   begin
      if Ship = Player_Ship and not Info_Only then
         if Have_Order_Requirements'Length > 0 then
            return 0;
         end if;
      end if;
      Count_Damage_Penalty_Block:
      declare
         Damage: Damage_Factor := 0.0;
      begin
         Find_Engine_Loop :
         for Module of Ship.Modules loop
            if Module.M_Type = ENGINE and then not Module.Disabled then
               Base_Speed := Module.Power * 10;
               Damage :=
                 1.0 -
                 Damage_Factor
                   (Float(Module.Durability) / Float(Module.Max_Durability));
               Speed :=
                 Speed +
                 (Base_Speed - Natural(Float(Base_Speed) * Float(Damage)));
            end if;
         end loop Find_Engine_Loop;
      end Count_Damage_Penalty_Block;
      Speed :=
        Natural((Float(Speed) / Float(Count_Ship_Weight(Ship => Ship))) * 100_000.0);
      if Ship.Crew.Length > 0 then
         if not Factions_List(Ship.Crew(1).Faction).Flags.Contains
             (Item => To_Unbounded_String(Source => "sentientships")) then
            Sentinent_Ship_Speed_Loop :
            for I in Ship.Crew.Iterate loop
               if Ship.Crew(I).Order = PILOT then
                  Speed :=
                    Speed +
                    Natural
                      (Float(Speed) *
                       (Float(Get_Skill_Level(Member => Ship.Crew(I), Skill_Index => Piloting_Skill)) /
                        300.0));
               elsif Ship.Crew(I).Order = ENGINEER then
                  Speed :=
                    Speed +
                    Natural
                      (Float(Speed) *
                       (Float
                          (Get_Skill_Level(Member => Ship.Crew(I), Skill_Index => Engineering_Skill)) /
                        300.0));
               end if;
            end loop Sentinent_Ship_Speed_Loop;
         else
            Normal_Ship_Speed_Loop :
            for Module of Ship.Modules loop
               if Module.M_Type = HULL then
                  Speed :=
                    Speed +
                    Natural
                      (Float(Speed) * (Float(Module.Max_Modules * 2) / 300.0));
                  exit Normal_Ship_Speed_Loop;
               end if;
            end loop Normal_Ship_Speed_Loop;
         end if;
      end if;
      if Ship = Player_Ship and (Ship.Speed in DOCKED | FULL_STOP) and
        Info_Only then
         Ship_Set_Speed := Game_Settings.Undock_Speed;
         if Ship_Set_Speed = FULL_STOP then
            Ship_Set_Speed := QUARTER_SPEED;
         end if;
      else
         Ship_Set_Speed := Ship.Speed;
      end if;
      case Ship_Set_Speed is
         when QUARTER_SPEED =>
            Speed := Integer(Float(Speed) * 0.25);
         when HALF_SPEED =>
            Speed := Integer(Float(Speed) * 0.5);
         when FULL_SPEED =>
            null;
         when others =>
            return 0;
      end case;
      Speed := (Speed / 60);
      return Speed;
   end Real_Speed;

   function Count_Fuel_Needed return Integer is
      Fuel_Needed: Integer := 0;
      Speed: Ship_Speed := Player_Ship.Speed;
   begin
      if Speed in DOCKED | FULL_STOP then
         Speed := Game_Settings.Undock_Speed;
      end if;
      Count_Fuel_Needed_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = ENGINE and then not Module.Disabled then
            case Speed is
               when QUARTER_SPEED =>
                  Fuel_Needed := Fuel_Needed - (Module.Fuel_Usage / 4);
               when HALF_SPEED =>
                  Fuel_Needed := Fuel_Needed - (Module.Fuel_Usage / 2);
               when FULL_SPEED =>
                  Fuel_Needed := Fuel_Needed - Module.Fuel_Usage;
               when others =>
                  null;
            end case;
         end if;
      end loop Count_Fuel_Needed_Loop;
      return Fuel_Needed;
   end Count_Fuel_Needed;

   procedure Wait_In_Place(Minutes: Positive) is
      BaseFuelNeeded, FuelNeeded: Integer := 0;
      FuelIndex: Natural;
   begin
      if Player_Ship.Speed = DOCKED then
         return;
      end if;
      Needed_Fuel_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = ENGINE and then not Module.Disabled then
            BaseFuelNeeded := BaseFuelNeeded - 1;
         end if;
      end loop Needed_Fuel_Loop;
      FuelNeeded := BaseFuelNeeded * (Minutes / 10);
      if Get_Random(1, 10) < (Minutes rem 10) then
         FuelNeeded := FuelNeeded + BaseFuelNeeded;
      end if;
      FuelIndex :=
        Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
      if FuelIndex = 0 then
         Add_Message
           ("Ship falls from the sky due to a lack of fuel.", OTHERMESSAGE,
            RED);
         Death(1, To_Unbounded_String("fall of the ship"), Player_Ship);
         return;
      end if;
      if Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => FuelIndex)
          .Amount <=
        abs (FuelNeeded) then
         Add_Message
           ("Ship falls from the sky due to a lack of fuel.", OTHERMESSAGE,
            RED);
         Death(1, To_Unbounded_String("fall of the ship"), Player_Ship);
         return;
      end if;
      Update_Cargo
        (Player_Ship,
         Inventory_Container.Element
           (Container => Player_Ship.Cargo, Index => FuelIndex)
           .Proto_Index,
         FuelNeeded);
   end Wait_In_Place;

end Ships.Movement;
