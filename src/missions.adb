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

with Ada.Numerics.Elementary_Functions;
with Ada.Exceptions;
with Ships; use Ships;
with Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement;
with Maps; use Maps;
with Bases; use Bases;
with Messages; use Messages;
with Crew; use Crew;
with Statistics; use Statistics;
with Utils; use Utils;
with Config;
with Events; use Events;
with Goals;
with Factions;
with Items; use Items;
with Ada.Text_IO;

package body Missions is

   procedure Generate_Missions is
      use Ada.Numerics.Elementary_Functions;
      use Tiny_String;

      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Mission_X, Mission_Y: Positive range 1 .. 1_024 := 1;
      Missions_Amount: Positive range 1 .. 26;
      Tmp_Base_Index: Bases_Range := 1;
      --## rule off IMPROPER_INITIALIZATION
      Mission: Mission_Data;
      Missions_Items: Positive_Container.Vector;
      Bases_In_Range: Positive_Container.Vector;
      Enemies: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      Min_X, Min_Y, Max_X, Max_Y: Integer := 0;
      M_Type: Missions_Types := Default_Mission_Type;
      Diff_X, Diff_Y: Natural := 0;
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
      --## rule off ASSIGNMENTS
      Missions_Amount :=
        (case Sky_Bases(Base_Index).Reputation.Level is
           when 1 .. 25 => Missions_Amount + 1,
           when 26 .. 50 => Missions_Amount + 3,
           when 51 .. 75 => Missions_Amount + 5,
           when 76 .. 100 => Missions_Amount + 10,
           when others => Missions_Amount);
      --## rule on ASSIGNMENTS
      Find_Mission_Items_Loop :
      for I in 1 .. Get_Proto_Amount loop
         if To_String(Source => Get_Proto_Item(Index => I).I_Type) =
           To_String(Source => Mission_Items_Type) then
            Missions_Items.Append(New_Item => I);
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
               if Mission.Ship_Index = 0 then
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
         --## rule off SIMPLIFIABLE_EXPRESSIONS
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
         --## rule on SIMPLIFIABLE_EXPRESSIONS
         Mission.Start_Base := Base_Index;
         Mission.Finished := False;
         Sky_Bases(Base_Index).Missions.Append(New_Item => Mission);
      end loop Generate_Missions_Loop;
      Sky_Bases(Base_Index).Missions_Date := Game_Date;
   end Generate_Missions;

   procedure Accept_Mission(Mission_Index: Positive) is
      use Ships.Cargo;
      use Tiny_String;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Mission: Mission_Data := Sky_Bases(Base_Index).Missions(Mission_Index);
      Accept_Message: Unbounded_String := Null_Unbounded_String;
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
         for Accepted_Mission of Accepted_Missions loop
            if Accepted_Mission.Start_Base = Base_Index then
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
         --## rule off SIMPLIFIABLE_EXPRESSIONS

          Free_Cargo
            (Amount => -(Get_Proto_Item(Index => Mission.Item_Index).Weight)) <
         --## rule on SIMPLIFIABLE_EXPRESSIONS

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
                 To_String
                   (Source =>
                      Get_Proto_Item(Index => Mission.Item_Index).Name) &
                 "'.");
            Update_Cargo
              (Ship => Player_Ship, Proto_Index => Mission.Item_Index,
               Amount => 1);
         when DESTROY =>
            Append
              (Source => Accept_Message,
               New_Item =>
                 "'Destroy " &
                 To_String
                   (Source => Get_Proto_Ship(Proto_Index => Mission.Ship_Index).Name) &
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
               use Factions;

               Passenger_Base: constant Bases_Range :=
                 (if Get_Random(Min => 1, Max => 100) < 60 then Base_Index
                  else Get_Random
                      (Min => Sky_Bases'First, Max => Sky_Bases'Last));
               Gender: Character;
               Max_Attribute_Level, Morale: Integer;
               Faction: constant Faction_Record :=
                 Get_Faction(Index => Sky_Bases(Passenger_Base).Owner);
               --## rule off IMPROPER_INITIALIZATION
               Attributes: Mob_Attributes
                 (1 ..
                      Positive
                        (AttributesData_Container.Length
                           (Container => Attributes_List)));
               Inventory: Inventory_Container.Vector (Capacity => 32);
               Skills: Skills_Container.Vector (Capacity => Skills_Amount);
            begin
               Inventory_Container.Delete_Last(Container => Inventory);
               --## rule on IMPROPER_INITIALIZATION
               if Faction.Flags.Contains
                   (Item => To_Unbounded_String(Source => "nogender")) then
                  Gender := 'M';
               else
                  Gender :=
                    (if Get_Random(Min => 1, Max => 2) = 1 then 'M' else 'F');
               end if;
               if Faction.Flags.Contains
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
               --## rule off SIMPLIFIABLE_STATEMENTS
               Set_Attributes_Loop :
               for J in 1 .. Attributes_Amount loop
                  Attributes(J) :=
                    (Level => Get_Random(Min => 3, Max => Max_Attribute_Level),
                     Experience => 0);
               end loop Set_Attributes_Loop;
               --## rule on SIMPLIFIABLE_STATEMENTS
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
      Time: Integer := 0;
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
      use Goals;
      use Ships.Movement;
      use Tiny_String;

      Message: Unbounded_String := Null_Unbounded_String;
      Missions_Amount: constant Positive := Positive(Accepted_Missions.Length);
   begin
      if Player_Ship.Speed /= DOCKED then
         Message := To_Unbounded_String(Source => Dock_Ship(Docking => True));
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
                      Get_Proto_Item
                        (Index => Accepted_Missions(Mission_Index).Item_Index)
                        .Name) &
                 "'.",
               M_Type => MISSIONMESSAGE, Color => GREEN);
         when DESTROY =>
            Add_Message
              (Message =>
                 "You finished mission 'Destroy " &
                 To_String
                   (Source =>
                     Get_Proto_Ship
                        (Proto_Index => Accepted_Missions(Mission_Index).Ship_Index)
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
      Ada.Text_IO.Put_Line(Item => "");
   end Finish_Mission;

   procedure Delete_Mission
     (Mission_Index: Positive; Failed: Boolean := True) is
      Mission: constant Mission_Data := Accepted_Missions(Mission_Index);
      Base_Index: constant Bases_Range := Mission.Start_Base;
      procedure Delete_Ada_Mission(M_Index, Fail: Integer) with
         Import => True,
         Convention => C,
         External_Name => "deleteAdaMission";
   begin
      Get_Accepted_Missions;
      Get_Ada_Ship;
      Get_Ada_Base_Location
        (Base_Index => Base_Index, X => Sky_Bases(Base_Index).Sky_X,
         Y => Sky_Bases(Base_Index).Sky_Y);
      Delete_Ada_Mission
        (M_Index => Mission_Index, Fail => (if Failed then 1 else 0));
      Set_Accepted_Missions;
      Set_Ada_Ship(Ship => Player_Ship);
      Sky_Map(Mission.Target_X, Mission.Target_Y).Mission_Index := 0;
      Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
        .Mission_Index :=
        0;
      Update_Map_Loop :
      for I in Accepted_Missions.Iterate loop
         if Accepted_Missions(I).Finished then
            Sky_Map
              (Sky_Bases(Accepted_Missions(I).Start_Base).Sky_X,
               Sky_Bases(Accepted_Missions(I).Start_Base).Sky_Y)
              .Mission_Index :=
              Mission_Container.To_Index(Position => I);
         else
            Sky_Map
              (Accepted_Missions(I).Target_X, Accepted_Missions(I).Target_Y)
              .Mission_Index :=
              Mission_Container.To_Index(Position => I);
         end if;
      end loop Update_Map_Loop;
   end Delete_Mission;

   procedure Update_Mission(Mission_Index: Positive) is
      use Config;
      use Tiny_String;

      Mission: constant Mission_Data := Accepted_Missions(Mission_Index);
      Message_Text: Unbounded_String :=
        To_Unbounded_String(Source => "Return to ") &
        Tiny_String.To_String(Source => Sky_Bases(Mission.Start_Base).Name) &
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
                      Get_Proto_Item
                        (Index => Accepted_Missions(Mission_Index).Item_Index)
                        .Name) &
                 "'.");
         when DESTROY =>
            Append
              (Source => Message_Text,
               New_Item =>
                 "'Destroy " &
                 To_String
                   (Source =>
                      Get_Proto_Ship
                        (Proto_Index => Accepted_Missions(Mission_Index).Ship_Index)
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
      use Ada.Exceptions;

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

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Mission_Data is record
      Time: Natural;
      Target_X: Natural;
      Target_Y: Natural;
      Reward: Natural;
      Start_Base: Natural;
      Finished: Natural;
      Multiplier: Reward_Multiplier := 0.0;
      M_Type: Natural;
      Data: Natural;
   end record;

   type Nim_Missions_Array is array(0 .. 49) of Nim_Mission_Data;
   --## rule on TYPE_INITIAL_VALUES

   procedure Get_Accepted_Missions is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Missions: Nim_Missions_Array;
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Accepted_Missions(N_Missions: Nim_Missions_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaAcceptedMissions";
   begin
      Convert_Accepted_Missions_Loop :
      for I in Nim_Missions'Range loop
         if I <= Integer(Accepted_Missions.Length - 1) then
            Nim_Missions(I) :=
              (Time => Accepted_Missions(I + 1).Time,
               Target_X => Accepted_Missions(I + 1).Target_X,
               Target_Y => Accepted_Missions(I + 1).Target_Y,
               Reward => Accepted_Missions(I + 1).Reward,
               Start_Base => Accepted_Missions(I + 1).Start_Base,
               Finished =>
                 (if Accepted_Missions(I + 1).Finished then 1 else 0),
               Multiplier => Accepted_Missions(I + 1).Multiplier,
               M_Type => Missions_Types'Pos(Accepted_Missions(I + 1).M_Type),
               Data =>
                 (case Accepted_Missions(I + 1).M_Type is
                    when DELIVER => Accepted_Missions(I + 1).Item_Index,
                    when DESTROY => Accepted_Missions(I + 1).Ship_Index,
                    when PASSENGER => Accepted_Missions(I + 1).Data,
                    when others => Accepted_Missions(I + 1).Target));
         else
            Nim_Missions(I) :=
              (Time => 0, Target_X => 0, Target_Y => 0, Reward => 0,
               Start_Base => 0, Finished => 0, Multiplier => 0.0, M_Type => 0,
               Data => 0);
         end if;
      end loop Convert_Accepted_Missions_Loop;
      Get_Ada_Accepted_Missions(N_Missions => Nim_Missions);
   end Get_Accepted_Missions;

   procedure Set_Accepted_Missions is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Missions: Nim_Missions_Array;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Accepted_Missions
        (N_Missions: out Nim_Missions_Array) with
         Import => True,
         Convention => C,
         External_Name => "setAdaAcceptedMissions";
   begin
      Set_Ada_Accepted_Missions(N_Missions => Nim_Missions);
      Accepted_Missions.Clear;
      Convert_Accepted_Missions_Loop :
      for Nim_Mission of Nim_Missions loop
         exit Convert_Accepted_Missions_Loop when Nim_Mission.Time = 0;
         case Nim_Mission.M_Type is
            when 0 =>
               Accepted_Missions.Append
                 (New_Item =>
                    (M_Type => DELIVER, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Item_Index => Nim_Mission.Data));
            when 1 =>
               Accepted_Missions.Append
                 (New_Item =>
                    (M_Type => DESTROY, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Ship_Index => Nim_Mission.Data));
            when 2 =>
               Accepted_Missions.Append
                 (New_Item =>
                    (M_Type => PATROL, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Target => Nim_Mission.Data));
            when 3 =>
               Accepted_Missions.Append
                 (New_Item =>
                    (M_Type => EXPLORE, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Target => Nim_Mission.Data));
            when 4 =>
               Accepted_Missions.Append
                 (New_Item =>
                    (M_Type => PASSENGER, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Data => Nim_Mission.Data));
            when others =>
               null;
         end case;
      end loop Convert_Accepted_Missions_Loop;
   end Set_Accepted_Missions;

end Missions;
