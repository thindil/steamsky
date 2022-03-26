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
with Ada.Exceptions; use Ada.Exceptions;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Maps; use Maps;
with Items; use Items;
with Bases; use Bases;
with Messages; use Messages;
with Crew; use Crew;
with Statistics; use Statistics;
with Utils; use Utils;
with Config; use Config;
with Events; use Events;
with Goals; use Goals;
with Factions; use Factions;

package body Missions is

   procedure Generate_Missions is
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Mission_X, Mission_Y: Positive range 1 .. 1_024;
      Missions_Amount: Positive range 1 .. 26;
      Tmp_Base_Index: Bases_Range;
      Mission: Mission_Data;
      Missions_Items: TinyString_Container.Vector;
      Bases_In_Range: Positive_Container.Vector;
      Min_X, Min_Y, Max_X, Max_Y: Integer;
      Enemies: UnboundedString_Container.Vector;
      M_Type: Missions_Types;
      Diff_X, Diff_Y: Natural;
      Qualities_Array: constant array(1 .. 10) of Positive :=
        (1 => 1, 2 => 11, 3 => 21, 4 => 31, 5 => 41, 6 => 51, 7 => 61, 8 => 71,
         9 => 81, 10 => 91);
   begin
      if Days_Difference
          (Date_To_Compare => Sky_Bases(Base_Index).Missions_Date) <
        7 or
        Sky_Bases(Base_Index).Population = 0 then
         return;
      end if;
      Missions_Amount :=
        (case Sky_Bases(Base_Index).Population is
           when 1 .. 149 => Get_Random(Min => 1, Max => 5),
           when 150 .. 299 => Get_Random(Min => 1, Max => 10),
           when others => Get_Random(Min => 1, Max => 15));
      Missions_Amount :=
        (case Sky_Bases(Base_Index).Reputation.Level is
           when 1 .. 25 => Missions_Amount + 1,
           when 26 .. 50 => Missions_Amount + 3,
           when 51 .. 75 => Missions_Amount + 5,
           when 76 .. 100 => Missions_Amount + 10,
           when others => Missions_Amount);
      Find_Mission_Items_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).I_Type = Mission_Items_Type then
            Missions_Items.Append
              (New_Item => Objects_Container.Key(Position => I));
         end if;
      end loop Find_Mission_Items_Loop;
      Min_X := Player_Ship.Sky_X - 100;
      Normalize_Coord(Coord => Min_X);
      Max_X := Player_Ship.Sky_X + 100;
      Normalize_Coord(Coord => Max_X);
      Min_Y := Player_Ship.Sky_Y - 100;
      Normalize_Coord(Coord => Min_Y, Is_X_Axis => False);
      Max_Y := Player_Ship.Sky_Y + 100;
      Normalize_Coord(Coord => Max_Y, Is_X_Axis => False);
      Find_Bases_In_Range_Loop :
      for I in Sky_Bases'Range loop
         if I /= Base_Index and Sky_Bases(I).Sky_X in Min_X .. Max_X and
           Sky_Bases(I).Sky_Y in Min_Y .. Max_Y and
           Sky_Bases(I).Population > 0 then
            Bases_In_Range.Append(New_Item => I);
         end if;
      end loop Find_Bases_In_Range_Loop;
      Get_Random_Bases_Loop :
      while Missions_Amount > Positive(Bases_In_Range.Length) loop
         Tmp_Base_Index := Get_Random(Min => 1, Max => 1_024);
         if Bases_In_Range.Find_Index(Item => Tmp_Base_Index) =
           Positive_Container.No_Index and
           Sky_Bases(Tmp_Base_Index).Population > 0 then
            Bases_In_Range.Append(New_Item => Tmp_Base_Index);
         end if;
      end loop Get_Random_Bases_Loop;
      Sky_Bases(Base_Index).Missions.Clear;
      if Get_Random(Min => 1, Max => 100) < 75 then
         Generate_Enemies(Enemies => Enemies, With_Traders => False);
      else
         Generate_Enemies(Enemies => Enemies);
      end if;
      Generate_Missions_Loop :
      for I in 1 .. Missions_Amount loop
         <<Start_Of_Loop>>
         M_Type :=
           Missions_Types'Val
             (Get_Random
                (Min => 0, Max => Missions_Types'Pos(Missions_Types'Last)));
         case M_Type is
            when DELIVER =>
               Mission :=
                 (M_Type => DELIVER, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Item_Index =>
                    Missions_Items
                      (Get_Random
                         (Min => 1, Max => Positive(Missions_Items.Length))),
                  Multiplier => 1.0);
            when DESTROY =>
               Mission :=
                 (M_Type => DESTROY, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Multiplier => 1.0,
                  Ship_Index =>
                    Enemies
                      (Get_Random
                         (Min => Enemies.First_Index,
                          Max => Enemies.Last_Index)));
               if Mission.Ship_Index = Null_Unbounded_String then
                  goto Start_Of_Loop;
               end if;
               Find_Mission_Location_Loop :
               loop
                  Mission_X := Get_Random(Min => Min_X, Max => Max_X);
                  Mission_Y := Get_Random(Min => Min_Y, Max => Max_Y);
                  exit Find_Mission_Location_Loop when Sky_Map
                      (Mission_X, Mission_Y)
                      .Base_Index =
                    0 and
                    Mission_X /= Player_Ship.Sky_X and
                    Mission_Y /= Player_Ship.Sky_Y;
               end loop Find_Mission_Location_Loop;
            when PATROL =>
               Mission :=
                 (M_Type => PATROL, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Multiplier => 1.0, Target => 1);
               Find_Patrol_Mission_Location_Loop :
               for J in 1 .. 10 loop
                  Mission_X := Get_Random(Min => Min_X, Max => Max_X);
                  Mission_Y := Get_Random(Min => Min_Y, Max => Max_Y);
                  if Sky_Map(Mission_X, Mission_Y).Visited and
                    Sky_Map(Mission_X, Mission_Y).Base_Index = 0 then
                     Mission.Target := 0;
                     exit Find_Patrol_Mission_Location_Loop;
                  end if;
               end loop Find_Patrol_Mission_Location_Loop;
               if Mission.Target = 1 then
                  goto Start_Of_Loop;
               end if;
            when EXPLORE =>
               Mission :=
                 (M_Type => EXPLORE, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Multiplier => 1.0, Target => 1);
               Find_Explore_Location_Loop :
               for J in 1 .. 10 loop
                  Mission_X := Get_Random(Min => Min_X, Max => Max_X);
                  Mission_Y := Get_Random(Min => Min_Y, Max => Max_Y);
                  if not Sky_Map(Mission_X, Mission_Y).Visited and
                    Sky_Map(Mission_X, Mission_Y).Base_Index = 0 then
                     Mission.Target := 0;
                     exit Find_Explore_Location_Loop;
                  end if;
               end loop Find_Explore_Location_Loop;
               if Mission.Target = 1 then
                  goto Start_Of_Loop;
               end if;
            when PASSENGER =>
               Mission :=
                 (M_Type => PASSENGER, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Multiplier => 1.0,
                  Data =>
                    Qualities_Array
                      (Get_Random
                         (Min => Qualities_Array'First,
                          Max => Qualities_Array'Last)));
         end case;
         if Mission.M_Type in DELIVER | PASSENGER then
            Find_Base_Mission_Loop :
            loop
               Tmp_Base_Index :=
                 Get_Random
                   (Min => Bases_In_Range.First_Index,
                    Max => Bases_In_Range.Last_Index);
               Mission_X := Sky_Bases(Bases_In_Range(Tmp_Base_Index)).Sky_X;
               Mission_Y := Sky_Bases(Bases_In_Range(Tmp_Base_Index)).Sky_Y;
               exit Find_Base_Mission_Loop when Mission_X /=
                 Player_Ship.Sky_X and
                 Mission_Y /= Player_Ship.Sky_Y;
            end loop Find_Base_Mission_Loop;
         end if;
         Mission.Target_X := Mission_X;
         Mission.Target_Y := Mission_Y;
         Diff_X := abs (Player_Ship.Sky_X - Mission_X);
         Diff_Y := abs (Player_Ship.Sky_Y - Mission_Y);
         case Mission.M_Type is
            when DELIVER =>
               Mission.Time :=
                 Positive(80.0 * Sqrt(X => Float((Diff_X**2) + (Diff_Y**2))));
               Mission.Reward := (Mission.Time / 4);
            when DESTROY | PASSENGER =>
               Mission.Time :=
                 Positive(180.0 * Sqrt(X => Float((Diff_X**2) + (Diff_Y**2))));
               Mission.Reward := (Mission.Time / 4);
            when PATROL | EXPLORE =>
               Mission.Time :=
                 Positive(180.0 * Sqrt(X => Float((Diff_X**2) + (Diff_Y**2))));
               Mission.Reward := (Mission.Time / 5);
         end case;
         Mission.Start_Base := Base_Index;
         Mission.Finished := False;
         Sky_Bases(Base_Index).Missions.Append(New_Item => Mission);
      end loop Generate_Missions_Loop;
      Sky_Bases(Base_Index).Missions_Date := Game_Date;
   end Generate_Missions;

   procedure Accept_Mission(Mission_Index: Positive) is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Mission: Mission_Data := Sky_Bases(Base_Index).Missions(Mission_Index);
      Accept_Message: Unbounded_String;
      Trader_Index: constant Crew_Container.Extended_Index :=
        Find_Member(Order => TALK);
   begin
      if Sky_Bases(Base_Index).Reputation.Level < 0 then
         raise Missions_Accepting_Error
           with "Your reputation in this base is too low to receive any mission.";
      end if;
      Count_Missions_Limit_Block :
      declare
         Missions_Limit: Integer :=
           (case Sky_Bases(Base_Index).Reputation.Level is when 0 .. 25 => 1,
              when 26 .. 50 => 3, when 51 .. 75 => 5, when 76 .. 100 => 10,
              when others => 0);
      begin
         Count_Missions_Limit_Loop :
         for Mission of Accepted_Missions loop
            if Mission.Start_Base = Base_Index then
               Missions_Limit := Missions_Limit - 1;
            end if;
            exit Count_Missions_Limit_Loop when Missions_Limit <= 0;
         end loop Count_Missions_Limit_Loop;
         if Missions_Limit < 1 then
            raise Missions_Accepting_Error
              with "You can't take any more missions from this base. ";
         end if;
      end Count_Missions_Limit_Block;
      if Mission.M_Type = DELIVER
        and then
          FreeCargo(Amount => (0 - Items_List(Mission.Item_Index).Weight)) <
          0 then
         raise Missions_Accepting_Error
           with "You don't have enough cargo space for take this mission.";
      end if;
      if Mission.M_Type = PASSENGER then
         Find_Cabin_Block :
         declare
            Have_Cabin: Boolean := False;
         begin
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not Have_Cabin)
                 and then Module.Quality >= Mission.Data then
                  Have_Cabin := False;
                  Cabin_Owner_Loop :
                  for Owner of Module.Owner loop
                     if Owner = 0 then
                        Have_Cabin := True;
                        exit Cabin_Owner_Loop;
                     end if;
                  end loop Cabin_Owner_Loop;
                  exit Modules_Loop when Have_Cabin;
               end if;
            end loop Modules_Loop;
            if not Have_Cabin then
               raise Missions_Accepting_Error
                 with "You don't have proper (or free) cabin for this passenger.";
            end if;
         end Find_Cabin_Block;
      end if;
      Mission.Start_Base := Base_Index;
      Mission.Finished := False;
      Accept_Message :=
        To_Unbounded_String(Source => "You accepted the mission to ");
      case Mission.M_Type is
         when DELIVER =>
            Append
              (Source => Accept_Message,
               New_Item =>
                 "'Deliver " &
                 To_String(Source => Items_List(Mission.Item_Index).Name) &
                 "'.");
            UpdateCargo
              (Ship => Player_Ship, ProtoIndex => Mission.Item_Index,
               Amount => 1);
         when DESTROY =>
            Append
              (Source => Accept_Message,
               New_Item =>
                 "'Destroy " &
                 To_String
                   (Source => Proto_Ships_List(Mission.Ship_Index).Name) &
                 "'.");
         when PATROL =>
            Append
              (Source => Accept_Message,
               New_Item => "'Patrol selected area'.");
         when EXPLORE =>
            Append
              (Source => Accept_Message,
               New_Item => "'Explore selected area'.");
         when PASSENGER =>
            Append
              (Source => Accept_Message,
               New_Item => "'Transport passenger to base'.");
            Set_Passenger_Block :
            declare
               Passenger_Base: constant Bases_Range :=
                 (if Get_Random(Min => 1, Max => 100) < 60 then Base_Index
                  else Get_Random
                      (Min => Sky_Bases'First, Max => Sky_Bases'Last));
               Gender: Character;
               Skills: Skills_Container.Vector (Capacity => Skills_Amount);
               Inventory: constant Inventory_Container.Vector :=
                 Inventory_Container.Empty_Vector;
               Max_Attribute_Level, Morale: Integer;
               Attributes: Mob_Attributes
                 (1 ..
                      Positive
                        (AttributesData_Container.Length
                           (Container => Attributes_List)));
            begin
               if not Factions_List(Sky_Bases(Passenger_Base).Owner).Flags
                   .Contains
                   (Item => To_Unbounded_String(Source => "nogender")) then
                  Gender :=
                    (if Get_Random(Min => 1, Max => 2) = 1 then 'M' else 'F');
               else
                  Gender := 'M';
               end if;
               if Factions_List(Sky_Bases(Passenger_Base).Owner).Flags.Contains
                   (Item => To_Unbounded_String(Source => "nomorale")) then
                  Morale := 50;
               else
                  Morale := 50 + Sky_Bases(Passenger_Base).Reputation.Level;
                  if Morale < 50 then
                     Morale := 50;
                  end if;
               end if;
               Max_Attribute_Level := Sky_Bases(Base_Index).Reputation.Level;
               if Max_Attribute_Level < 10 then
                  Max_Attribute_Level := 10;
               end if;
               if Get_Random(Min => 1, Max => 100) > 90 then
                  Max_Attribute_Level :=
                    Get_Random(Min => Max_Attribute_Level, Max => 100);
               end if;
               if Max_Attribute_Level > 50 then
                  Max_Attribute_Level := 50;
               end if;
               Set_Attributes_Loop :
               for J in 1 .. Attributes_Amount loop
                  Attributes(J) :=
                    (Level => Get_Random(Min => 3, Max => Max_Attribute_Level),
                     Experience => 0);
               end loop Set_Attributes_Loop;
               Player_Ship.Crew.Append
                 (New_Item =>
                    (Amount_Of_Attributes => Attributes_Amount,
                     Name =>
                       Generate_Member_Name
                         (Gender => Gender,
                          Faction_Index => Sky_Bases(Passenger_Base).Owner),
                     Amount_Of_Skills => Skills_Amount, Gender => Gender,
                     Health => 100, Tired => 0, Skills => Skills, Hunger => 0,
                     Thirst => 0, Order => REST, Previous_Order => REST,
                     Order_Time => 15, Orders => (others => 0),
                     Attributes => Attributes, Inventory => Inventory,
                     Equipment => (others => 0), Payment => (others => 0),
                     Contract_Length => Mission.Time,
                     Morale => (1 => Morale, 2 => 0), Loyalty => Morale,
                     Home_Base => Passenger_Base,
                     Faction => Sky_Bases(Passenger_Base).Owner));
            end Set_Passenger_Block;
            Find_Cabin_Loop :
            for Module of Player_Ship.Modules loop
               if Module.M_Type = CABIN
                 and then
                 (Module.Quality >= Mission.Data and Module.Owner(1) = 0) then
                  Module.Owner(1) := Player_Ship.Crew.Last_Index;
                  exit Find_Cabin_Loop;
               end if;
            end loop Find_Cabin_Loop;
            Mission.Data := Player_Ship.Crew.Last_Index;
      end case;
      Sky_Bases(Base_Index).Missions.Delete(Index => Mission_Index);
      Accepted_Missions.Append(New_Item => Mission);
      Sky_Map(Mission.Target_X, Mission.Target_Y).Mission_Index :=
        Accepted_Missions.Last_Index;
      Add_Message
        (Message => To_String(Source => Accept_Message),
         M_Type => MISSIONMESSAGE);
      Gain_Exp
        (Amount => 1, Skill_Number => Talking_Skill,
         Crew_Index => Trader_Index);
      Game_Stats.Accepted_Missions := Game_Stats.Accepted_Missions + 1;
      Update_Game(Minutes => 5);
   end Accept_Mission;

   procedure Update_Missions(Minutes: Positive) is
      Time: Integer;
      I: Mission_Container.Extended_Index := Accepted_Missions.First_Index;
   begin
      Update_Missions_Loop :
      while I <= Accepted_Missions.Last_Index loop
         Time := Accepted_Missions(I).Time - Minutes;
         if Time < 1 then
            Delete_Mission(Mission_Index => I);
         else
            Accepted_Missions(I).Time := Time;
            I := I + 1;
         end if;
      end loop Update_Missions_Loop;
   end Update_Missions;

   procedure Finish_Mission(Mission_Index: Positive) is
      Message: Unbounded_String;
      Missions_Amount: constant Positive := Positive(Accepted_Missions.Length);
   begin
      if Player_Ship.Speed /= DOCKED then
         Message := To_Unbounded_String(Source => DockShip(Docking => True));
         if Length(Source => Message) > 0 then
            raise Missions_Finishing_Error with To_String(Source => Message);
         end if;
      end if;
      Update_Game(Minutes => 5);
      if Missions_Amount > Natural(Accepted_Missions.Length) then
         return;
      end if;
      case Accepted_Missions(Mission_Index).M_Type is
         when DELIVER =>
            Add_Message
              (Message =>
                 "You finished mission 'Deliver " &
                 To_String
                   (Source =>
                      Items_List(Accepted_Missions(Mission_Index).Item_Index)
                        .Name) &
                 "'.",
               M_Type => MISSIONMESSAGE, Color => GREEN);
         when DESTROY =>
            Add_Message
              (Message =>
                 "You finished mission 'Destroy " &
                 To_String
                   (Source =>
                      Proto_Ships_List
                        (Accepted_Missions(Mission_Index).Ship_Index)
                        .Name) &
                 "'.",
               M_Type => MISSIONMESSAGE, Color => GREEN);
         when PATROL =>
            Add_Message
              (Message => "You finished mission 'Patrol selected area'.",
               M_Type => MISSIONMESSAGE, Color => GREEN);
         when EXPLORE =>
            Add_Message
              (Message => "You finished mission 'Explore selected area'.",
               M_Type => MISSIONMESSAGE, Color => GREEN);
         when PASSENGER =>
            Add_Message
              (Message =>
                 "You finished mission 'Transport passenger to base'.",
               M_Type => MISSIONMESSAGE, Color => GREEN);
      end case;
      Update_Goal
        (G_Type => MISSION,
         Target_Index =>
           To_Unbounded_String
             (Source =>
                Missions_Types'Image
                  (Accepted_Missions(Mission_Index).M_Type)));
      Update_Finished_Missions
        (M_Type =>
           To_Unbounded_String
             (Source =>
                Natural'Image
                  (Missions_Types'Pos
                     (Accepted_Missions(Mission_Index).M_Type))));
      Delete_Mission(Mission_Index => Mission_Index, Failed => False);
   end Finish_Mission;

   procedure Delete_Mission
     (Mission_Index: Positive; Failed: Boolean := True) is
      Message_Text: Unbounded_String :=
        To_Unbounded_String(Source => "You failed your mission to ");
      Mission: constant Mission_Data := Accepted_Missions(Mission_Index);
      Reputation: Natural;
   begin
      Reputation := Mission.Reward / 50;
      if Reputation < 2 then
         Reputation := 2;
      end if;
      Reputation :=
        Natural
          (Float(Reputation) +
           (Float(Reputation) * Float(Mission.Multiplier - 1.0)));
      if Failed then
         Gain_Rep(Base_Index => Mission.Start_Base, Points => -Reputation);
         Update_Morale
           (Ship => Player_Ship, Member_Index => 1,
            Value => Get_Random(Min => -10, Max => -5));
         case Mission.M_Type is
            when DELIVER =>
               Append
                 (Source => Message_Text,
                  New_Item =>
                    "'Deliver " &
                    To_String(Source => Items_List(Mission.Item_Index).Name) &
                    "'.");
            when DESTROY =>
               Append
                 (Source => Message_Text,
                  New_Item =>
                    "'Destroy " &
                    To_String
                      (Source => Proto_Ships_List(Mission.Ship_Index).Name) &
                    "'.");
            when PATROL =>
               Append
                 (Source => Message_Text,
                  New_Item => "'Patrol selected area'.");
            when EXPLORE =>
               Append
                 (Source => Message_Text,
                  New_Item => "'Explore selected area'.");
            when PASSENGER =>
               Append
                 (Source => Message_Text,
                  New_Item => "'Transport passenger to base'.");
         end case;
         Add_Message
           (Message => To_String(Source => Message_Text),
            M_Type => MISSIONMESSAGE, Color => RED);
      else
         if Mission.M_Type in DELIVER | PASSENGER then
            Gain_Rep
              (Base_Index =>
                 Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
               Points => (Reputation / 2));
            Gain_Rep
              (Base_Index => Mission.Start_Base, Points => (Reputation / 2));
         else
            Gain_Rep(Base_Index => Mission.Start_Base, Points => Reputation);
         end if;
         Update_Morale(Ship => Player_Ship, Member_Index => 1, Value => 1);
         Get_Mission_Reward_Block :
         declare
            Free_Space: Integer;
            Trader_Index: constant Natural := Find_Member(Order => TALK);
            Reward_Amount: Natural :=
              Natural(Float(Mission.Reward) * Float(Mission.Multiplier));
         begin
            Count_Price
              (Price => Reward_Amount, Trader_Index => Trader_Index,
               Reduce => False);
            if Trader_Index > 0 then
               Gain_Exp
                 (Amount => 1, Skill_Number => Talking_Skill,
                  Crew_Index => Trader_Index);
            end if;
            Free_Space := FreeCargo(Amount => -(Reward_Amount));
            if Free_Space < 0 then
               Reward_Amount := Reward_Amount + Free_Space;
            end if;
            if Reward_Amount > 0 then
               Add_Message
                 (Message =>
                    "You received" & Integer'Image(Reward_Amount) & " " &
                    To_String(Source => Money_Name) &
                    " for finishing your mission.",
                  M_Type => MISSIONMESSAGE);
               UpdateCargo
                 (Ship => Player_Ship, ProtoIndex => Money_Index,
                  Amount => Reward_Amount);
            end if;
         end Get_Mission_Reward_Block;
      end if;
      Sky_Map(Mission.Target_X, Mission.Target_Y).Mission_Index := 0;
      Sky_Map
        (Sky_Bases(Mission.Start_Base).Sky_X,
         Sky_Bases(Mission.Start_Base).Sky_Y)
        .Mission_Index :=
        0;
      Accepted_Missions.Delete(Index => Mission_Index);
      if Mission.M_Type = DELIVER then
         UpdateCargo
           (Ship => Player_Ship, ProtoIndex => Mission.Item_Index,
            Amount => -1);
      elsif Mission.M_Type = PASSENGER
        and then Mission.Data <= Positive(Player_Ship.Crew.Length) then
         Delete_Member(Member_Index => Mission.Data, Ship => Player_Ship);
      end if;
      Update_Map_Loop :
      for I in
        Accepted_Missions.First_Index .. Accepted_Missions.Last_Index loop
         if Accepted_Missions(I).Finished then
            Sky_Map
              (Sky_Bases(Accepted_Missions(I).Start_Base).Sky_X,
               Sky_Bases(Accepted_Missions(I).Start_Base).Sky_Y)
              .Mission_Index :=
              I;
         else
            Sky_Map
              (Accepted_Missions(I).Target_X, Accepted_Missions(I).Target_Y)
              .Mission_Index :=
              I;
         end if;
      end loop Update_Map_Loop;
   end Delete_Mission;

   procedure Update_Mission(Mission_Index: Positive) is
      Mission: constant Mission_Data := Accepted_Missions(Mission_Index);
      Message_Text: Unbounded_String :=
        To_Unbounded_String(Source => "Return to ") &
        Sky_Bases(Mission.Start_Base).Name &
        To_Unbounded_String(Source => " to finish mission ");
   begin
      Sky_Map(Mission.Target_X, Mission.Target_Y).Mission_Index := 0;
      Accepted_Missions(Mission_Index).Finished := True;
      Sky_Map
        (Sky_Bases(Mission.Start_Base).Sky_X,
         Sky_Bases(Mission.Start_Base).Sky_Y)
        .Mission_Index :=
        Mission_Index;
      case Accepted_Missions(Mission_Index).M_Type is
         when DELIVER =>
            Append
              (Source => Message_Text,
               New_Item =>
                 "'Deliver " &
                 To_String
                   (Source =>
                      Items_List(Accepted_Missions(Mission_Index).Item_Index)
                        .Name) &
                 "'.");
         when DESTROY =>
            Append
              (Source => Message_Text,
               New_Item =>
                 "'Destroy " &
                 To_String
                   (Source =>
                      Proto_Ships_List
                        (Accepted_Missions(Mission_Index).Ship_Index)
                        .Name) &
                 "'.");
         when PATROL =>
            Append
              (Source => Message_Text, New_Item => "'Patrol selected area'.");
         when EXPLORE =>
            Append
              (Source => Message_Text, New_Item => "'Explore selected area'.");
         when PASSENGER =>
            Append
              (Source => Message_Text,
               New_Item => "'Transport passenger to base'.");
      end case;
      Add_Message
        (Message => To_String(Source => Message_Text),
         M_Type => MISSIONMESSAGE);
      if Game_Settings.Auto_Return then
         Player_Ship.Destination_X := Sky_Bases(Mission.Start_Base).Sky_X;
         Player_Ship.Destination_Y := Sky_Bases(Mission.Start_Base).Sky_Y;
         Add_Message
           (Message => "You set the travel destination for your ship.",
            M_Type => ORDERMESSAGE);
      end if;
   end Update_Mission;

   function Auto_Finish_Missions return String is
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      I: Natural := Accepted_Missions.First_Index;
   begin
      if Base_Index = 0 then
         return "";
      end if;
      if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0
        and then
          Events_List
            (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
            .E_Type /=
          DOUBLEPRICE then
         return "";
      end if;
      if Find_Member(Order => TALK) = 0 then
         return "";
      end if;
      Finish_Missions_Loop :
      while I <= Accepted_Missions.Last_Index loop
         if
           (Accepted_Missions(I).Finished and
            Accepted_Missions(I).Start_Base = Base_Index) or
           (Accepted_Missions(I).Target_X = Player_Ship.Sky_X and
            Accepted_Missions(I).Target_Y = Player_Ship.Sky_Y) then
            Finish_Mission(Mission_Index => I);
            I := I - 1;
         end if;
         I := I + 1;
      end loop Finish_Missions_Loop;
      return "";
   exception
      when An_Exception : Missions_Finishing_Error =>
         return Exception_Message(X => An_Exception);
   end Auto_Finish_Missions;

   function Get_Mission_Type(M_Type: Missions_Types) return String is
   begin
      case M_Type is
         when DELIVER =>
            return "Deliver item to base";
         when PATROL =>
            return "Patrol area";
         when DESTROY =>
            return "Destroy ship";
         when EXPLORE =>
            return "Explore area";
         when PASSENGER =>
            return "Transport passenger to base";
      end case;
   end Get_Mission_Type;

end Missions;
