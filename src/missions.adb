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

with Ada.Exceptions;
with Interfaces.C.Strings;
with Ships; use Ships;
with Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement;
with Maps; use Maps;
with Bases; use Bases;
with Messages; use Messages;
with Crew; use Crew;
with Statistics; use Statistics;
with Utils;
with Config;
with Events;
with Goals;
with Factions;
with Items; use Items;
with Ada.Text_IO;

package body Missions is

   procedure Generate_Missions is
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Generate_Ada_Missions with
         Import => True,
         Convention => C,
         External_Name => "generateAdaMissions";
   begin
      Get_Ada_Base_Date
        (Base_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Missions_Date.Year,
         Month => Sky_Bases(Base_Index).Missions_Date.Month,
         Day => Sky_Bases(Base_Index).Missions_Date.Day,
         Hour => Sky_Bases(Base_Index).Missions_Date.Hour,
         Minutes => Sky_Bases(Base_Index).Missions_Date.Minutes,
         Date_Type => 1);
      Get_Missions(Base_Index => Base_Index);
      Get_Base_Reputation(Base_Index => Base_Index);
      Get_Ada_Base_Population
        (Base_Index => Base_Index,
         Population => Sky_Bases(Base_Index).Population);
      Generate_Ada_Missions;
      Set_Missions(Base_Index => Base_Index);
      Set_Ada_Base_Date
        (Base_Index => Base_Index, Date_Type => 1,
         Year => Sky_Bases(Base_Index).Missions_Date.Year,
         Month => Sky_Bases(Base_Index).Missions_Date.Month,
         Day => Sky_Bases(Base_Index).Missions_Date.Day,
         Hour => Sky_Bases(Base_Index).Missions_Date.Hour,
         Minutes => Sky_Bases(Base_Index).Missions_Date.Minutes);
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
                   (Source =>
                      Get_Proto_Ship(Proto_Index => Mission.Ship_Index).Name) &
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
               use Utils;

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
      procedure Update_Ada_Missions(M: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMissions";
   begin
      Get_Missions;
      Update_Ada_Missions(M => Minutes);
      Set_Missions;
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
                        (Proto_Index =>
                           Accepted_Missions(Mission_Index).Ship_Index)
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
      Get_Missions;
      Get_Ada_Ship;
      Get_Ada_Base_Location
        (Base_Index => Base_Index, X => Sky_Bases(Base_Index).Sky_X,
         Y => Sky_Bases(Base_Index).Sky_Y);
      Delete_Ada_Mission
        (M_Index => Mission_Index, Fail => (if Failed then 1 else 0));
      Set_Missions;
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
                        (Proto_Index =>
                           Accepted_Missions(Mission_Index).Ship_Index)
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
      use Events;

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
      use Interfaces.C.Strings;

      function Get_Ada_Mission_Type(M_T: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaMissionType";
   begin
      return
        Value(Item => Get_Ada_Mission_Type(M_T => Missions_Types'Pos(M_Type)));
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

   procedure Get_Missions(Base_Index: Natural := 0) is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Missions: Nim_Missions_Array;
      Missions_List: constant Mission_Container.Vector :=
        (if Base_Index = 0 then Accepted_Missions
         else Sky_Bases(Base_Index).Missions);
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Missions
        (N_Missions: Nim_Missions_Array; B_Index: Natural) with
         Import => True,
         Convention => C,
         External_Name => "getAdaMissions";
   begin
      Convert_Missions_Loop :
      for I in Nim_Missions'Range loop
         if I <= Integer(Missions_List.Length - 1) then
            Nim_Missions(I) :=
              (Time => Missions_List(I + 1).Time,
               Target_X => Missions_List(I + 1).Target_X,
               Target_Y => Missions_List(I + 1).Target_Y,
               Reward => Missions_List(I + 1).Reward,
               Start_Base => Missions_List(I + 1).Start_Base,
               Finished => (if Missions_List(I + 1).Finished then 1 else 0),
               Multiplier => Missions_List(I + 1).Multiplier,
               M_Type => Missions_Types'Pos(Missions_List(I + 1).M_Type),
               Data =>
                 (case Missions_List(I + 1).M_Type is
                    when DELIVER => Missions_List(I + 1).Item_Index,
                    when DESTROY => Missions_List(I + 1).Ship_Index,
                    when PASSENGER => Missions_List(I + 1).Data,
                    when others => Missions_List(I + 1).Target));
         else
            Nim_Missions(I) :=
              (Time => 0, Target_X => 0, Target_Y => 0, Reward => 0,
               Start_Base => 0, Finished => 0, Multiplier => 0.0, M_Type => 0,
               Data => 0);
         end if;
      end loop Convert_Missions_Loop;
      Get_Ada_Missions(N_Missions => Nim_Missions, B_Index => Base_Index);
   end Get_Missions;

   procedure Set_Missions(Base_Index: Natural := 0) is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Missions: Nim_Missions_Array;
      Missions_List: Mission_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Missions
        (N_Missions: out Nim_Missions_Array; B_Index: Natural) with
         Import => True,
         Convention => C,
         External_Name => "setAdaMissions";
   begin
      Set_Ada_Missions(N_Missions => Nim_Missions, B_Index => Base_Index);
      Convert_Missions_Loop :
      for Nim_Mission of Nim_Missions loop
         exit Convert_Missions_Loop when Nim_Mission.Time = 0;
         case Nim_Mission.M_Type is
            when 0 =>
               Missions_List.Append
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
               Missions_List.Append
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
               Missions_List.Append
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
               Missions_List.Append
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
               Missions_List.Append
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
      end loop Convert_Missions_Loop;
      if Base_Index = 0 then
         Accepted_Missions := Missions_List;
      else
         Sky_Bases(Base_Index).Missions := Missions_List;
      end if;
   end Set_Missions;

end Missions;
