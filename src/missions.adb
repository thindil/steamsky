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
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      MissionX, MissionY: Positive range 1 .. 1_024;
      MissionsAmount: Positive range 1 .. 26;
      TmpBaseIndex: Bases_Range;
      Mission: Mission_Data;
      MissionsItems: TinyString_Container.Vector;
      BasesInRange: Positive_Container.Vector;
      MinX, MinY, MaxX, MaxY: Integer;
      Enemies: UnboundedString_Container.Vector;
      MType: Missions_Types;
      DiffX, DiffY: Natural;
      QualitiesArray: constant array(1 .. 10) of Positive :=
        (1, 11, 21, 31, 41, 51, 61, 71, 81, 91);
   begin
      if Days_Difference(Sky_Bases(BaseIndex).Missions_Date) < 7 or
        Sky_Bases(BaseIndex).Population = 0 then
         return;
      end if;
      MissionsAmount :=
        (case Sky_Bases(BaseIndex).Population is
           when 1 .. 149 => Get_Random(1, 5),
           when 150 .. 299 => Get_Random(1, 10),
           when others => Get_Random(1, 15));
      MissionsAmount :=
        (case Sky_Bases(BaseIndex).Reputation(1) is
           when 1 .. 25 => MissionsAmount + 1,
           when 26 .. 50 => MissionsAmount + 3,
           when 51 .. 75 => MissionsAmount + 5,
           when 76 .. 100 => MissionsAmount + 10,
           when others => MissionsAmount);
      for I in Items_List.Iterate loop
         if Items_List(I).I_Type = Mission_Items_Type then
            MissionsItems.Append(New_Item => Objects_Container.Key(I));
         end if;
      end loop;
      MinX := Player_Ship.Sky_X - 100;
      Normalize_Coord(MinX);
      MaxX := Player_Ship.Sky_X + 100;
      Normalize_Coord(MaxX);
      MinY := Player_Ship.Sky_Y - 100;
      Normalize_Coord(MinY, False);
      MaxY := Player_Ship.Sky_Y + 100;
      Normalize_Coord(MaxY, False);
      Find_Bases_In_Range_Loop :
      for I in Sky_Bases'Range loop
         if I /= BaseIndex and Sky_Bases(I).Sky_X in MinX .. MaxX and
           Sky_Bases(I).Sky_Y in MinY .. MaxY and
           Sky_Bases(I).Population > 0 then
            BasesInRange.Append(New_Item => I);
         end if;
      end loop Find_Bases_In_Range_Loop;
      Get_Random_Bases_Loop :
      while MissionsAmount > Positive(BasesInRange.Length) loop
         TmpBaseIndex := Get_Random(1, 1_024);
         if BasesInRange.Find_Index(Item => TmpBaseIndex) =
           Positive_Container.No_Index and
           Sky_Bases(TmpBaseIndex).Population > 0 then
            BasesInRange.Append(New_Item => TmpBaseIndex);
         end if;
      end loop Get_Random_Bases_Loop;
      Sky_Bases(BaseIndex).Missions.Clear;
      Generate_Enemies(Enemies);
      Generate_Missions_Loop :
      for I in 1 .. MissionsAmount loop
         <<Start_Of_Loop>>
         MType :=
           Missions_Types'Val
             (Get_Random(0, Missions_Types'Pos(Missions_Types'Last)));
         case MType is
            when DELIVER =>
               Mission :=
                 (M_Type => DELIVER, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Item_Index =>
                    MissionsItems
                      (Get_Random(1, Positive(MissionsItems.Length))),
                  Multiplier => 1.0);
            when DESTROY =>
               Mission :=
                 (M_Type => DESTROY, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Multiplier => 1.0,
                  Ship_Index =>
                    Enemies
                      (Get_Random(Enemies.First_Index, Enemies.Last_Index)));
               Find_Mission_Location_Loop :
               loop
                  MissionX := Get_Random(MinX, MaxX);
                  MissionY := Get_Random(MinY, MaxY);
                  exit Find_Mission_Location_Loop when Sky_Map
                      (MissionX, MissionY)
                      .Base_Index =
                    0 and
                    MissionX /= Player_Ship.Sky_X and
                    MissionY /= Player_Ship.Sky_Y;
               end loop Find_Mission_Location_Loop;
            when PATROL =>
               Mission :=
                 (M_Type => PATROL, Time => 1, Target_X => 0, Target_Y => 0,
                  Reward => 1, Start_Base => 1, Finished => False,
                  Multiplier => 1.0, Target => 1);
               Find_Patrol_Mission_Location_Loop :
               for J in 1 .. 10 loop
                  MissionX := Get_Random(MinX, MaxX);
                  MissionY := Get_Random(MinY, MaxY);
                  if Sky_Map(MissionX, MissionY).Visited and
                    Sky_Map(MissionX, MissionY).Base_Index = 0 then
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
                  MissionX := Get_Random(MinX, MaxX);
                  MissionY := Get_Random(MinY, MaxY);
                  if not Sky_Map(MissionX, MissionY).Visited and
                    Sky_Map(MissionX, MissionY).Base_Index = 0 then
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
                    QualitiesArray
                      (Get_Random(QualitiesArray'First, QualitiesArray'Last)));
         end case;
         if Mission.M_Type in DELIVER | PASSENGER then
            Find_Base_Mission_Loop :
            loop
               TmpBaseIndex :=
                 Get_Random(BasesInRange.First_Index, BasesInRange.Last_Index);
               MissionX := Sky_Bases(BasesInRange(TmpBaseIndex)).Sky_X;
               MissionY := Sky_Bases(BasesInRange(TmpBaseIndex)).Sky_Y;
               exit Find_Base_Mission_Loop when MissionX /=
                 Player_Ship.Sky_X and
                 MissionY /= Player_Ship.Sky_Y;
            end loop Find_Base_Mission_Loop;
         end if;
         Mission.Target_X := MissionX;
         Mission.Target_Y := MissionY;
         DiffX := abs (Player_Ship.Sky_X - MissionX);
         DiffY := abs (Player_Ship.Sky_Y - MissionY);
         case Mission.M_Type is
            when DELIVER =>
               Mission.Time :=
                 Positive(80.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
               Mission.Reward := (Mission.Time / 4);
            when DESTROY | PASSENGER =>
               Mission.Time :=
                 Positive(180.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
               Mission.Reward := (Mission.Time / 4);
            when PATROL | EXPLORE =>
               Mission.Time :=
                 Positive(180.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
               Mission.Reward := (Mission.Time / 5);
         end case;
         Mission.Start_Base := BaseIndex;
         Mission.Finished := False;
         Sky_Bases(BaseIndex).Missions.Append(New_Item => Mission);
      end loop Generate_Missions_Loop;
      Sky_Bases(BaseIndex).Missions_Date := Game_Date;
   end Generate_Missions;

   procedure Accept_Mission(Mission_Index: Positive) is
      BaseIndex: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Mission: Mission_Data := Sky_Bases(BaseIndex).Missions(Mission_Index);
      AcceptMessage: Unbounded_String;
      TraderIndex: constant Crew_Container.Extended_Index := FindMember(TALK);
   begin
      if Sky_Bases(BaseIndex).Reputation(1) < 0 then
         raise Missions_Accepting_Error
           with "Your reputation in this base is too low to receive any mission.";
      end if;
      declare
         MissionsLimit: Integer :=
           (case Sky_Bases(BaseIndex).Reputation(1) is when 0 .. 25 => 1,
              when 26 .. 50 => 3, when 51 .. 75 => 5, when 76 .. 100 => 10,
              when others => 0);
      begin
         Count_Missions_Limit_Loop :
         for Mission of Accepted_Missions loop
            if Mission.Start_Base = BaseIndex then
               MissionsLimit := MissionsLimit - 1;
            end if;
            exit Count_Missions_Limit_Loop when MissionsLimit <= 0;
         end loop Count_Missions_Limit_Loop;
         if MissionsLimit < 1 then
            raise Missions_Accepting_Error
              with "You can't take any more missions from this base. ";
         end if;
      end;
      if Mission.M_Type = DELIVER
        and then FreeCargo((0 - Items_List(Mission.Item_Index).Weight)) < 0 then
         raise Missions_Accepting_Error
           with "You don't have enough cargo space for take this mission.";
      end if;
      if Mission.M_Type = PASSENGER then
         declare
            HaveCabin: Boolean := False;
         begin
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not HaveCabin)
                 and then Module.Quality >= Mission.Data then
                  HaveCabin := False;
                  Cabin_Owner_Loop :
                  for Owner of Module.Owner loop
                     if Owner = 0 then
                        HaveCabin := True;
                        exit Cabin_Owner_Loop;
                     end if;
                  end loop Cabin_Owner_Loop;
                  exit Modules_Loop when HaveCabin;
               end if;
            end loop Modules_Loop;
            if not HaveCabin then
               raise Missions_Accepting_Error
                 with "You don't have proper (or free) cabin for this passenger.";
            end if;
         end;
      end if;
      Mission.Start_Base := BaseIndex;
      Mission.Finished := False;
      AcceptMessage := To_Unbounded_String("You accepted the mission to ");
      case Mission.M_Type is
         when DELIVER =>
            Append
              (AcceptMessage,
               "'Deliver " & To_String(Items_List(Mission.Item_Index).Name) &
               "'.");
            UpdateCargo(Player_Ship, Mission.Item_Index, 1);
         when DESTROY =>
            Append
              (AcceptMessage,
               "'Destroy " &
               To_String(Proto_Ships_List(Mission.Ship_Index).Name) & "'.");
         when PATROL =>
            Append(AcceptMessage, "'Patrol selected area'.");
         when EXPLORE =>
            Append(AcceptMessage, "'Explore selected area'.");
         when PASSENGER =>
            Append(AcceptMessage, "'Transport passenger to base'.");
            declare
               PassengerBase: Bases_Range;
               Gender: Character;
               Skills: Skills_Container.Vector;
               Inventory: Inventory_Container.Vector;
               MaxAttributeLevel, Morale: Integer;
               Attributes: Mob_Attributes
                 (1 ..
                      Positive
                        (AttributesData_Container.Length
                           (Container => Attributes_List)));
            begin
               PassengerBase :=
                 (if Get_Random(1, 100) < 60 then BaseIndex
                  else Get_Random(Sky_Bases'First, Sky_Bases'Last));
               if not Factions_List(Sky_Bases(PassengerBase).Owner).Flags
                   .Contains
                   (To_Unbounded_String("nogender")) then
                  Gender := (if Get_Random(1, 2) = 1 then 'M' else 'F');
               else
                  Gender := 'M';
               end if;
               if Factions_List(Sky_Bases(PassengerBase).Owner).Flags.Contains
                   (To_Unbounded_String("nomorale")) then
                  Morale := 50;
               else
                  Morale := 50 + Sky_Bases(PassengerBase).Reputation(1);
                  if Morale < 50 then
                     Morale := 50;
                  end if;
               end if;
               MaxAttributeLevel := Sky_Bases(BaseIndex).Reputation(1);
               if MaxAttributeLevel < 10 then
                  MaxAttributeLevel := 10;
               end if;
               if Get_Random(1, 100) > 90 then
                  MaxAttributeLevel := Get_Random(MaxAttributeLevel, 100);
               end if;
               if MaxAttributeLevel > 50 then
                  MaxAttributeLevel := 50;
               end if;
               for J in 1 .. Attributes_Amount loop
                  Attributes(J) := (Get_Random(3, MaxAttributeLevel), 0);
               end loop;
               Player_Ship.Crew.Append
                 (New_Item =>
                    (Amount_Of_Attributes => Attributes_Amount,
                     Name =>
                       Generate_Member_Name
                         (Gender, Sky_Bases(PassengerBase).Owner),
                     Amount_Of_Skills => Skills_Amount, Gender => Gender,
                     Health => 100, Tired => 0, Skills => Skills, Hunger => 0,
                     Thirst => 0, Order => REST, Previous_Order => REST,
                     Order_Time => 15, Orders => (others => 0),
                     Attributes => Attributes, Inventory => Inventory,
                     Equipment => (others => 0), Payment => (others => 0),
                     Contract_Length => Mission.Time, Morale => (Morale, 0),
                     Loyalty => Morale, Home_Base => PassengerBase,
                     Faction => Sky_Bases(PassengerBase).Owner));
            end;
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
      Sky_Bases(BaseIndex).Missions.Delete(Index => Mission_Index);
      Accepted_Missions.Append(New_Item => Mission);
      Sky_Map(Mission.Target_X, Mission.Target_Y).Mission_Index :=
        Accepted_Missions.Last_Index;
      Add_Message(To_String(AcceptMessage), MISSIONMESSAGE);
      Gain_Exp(1, Talking_Skill, TraderIndex);
      GameStats.AcceptedMissions := GameStats.AcceptedMissions + 1;
      Update_Game(5);
   end Accept_Mission;

   procedure Update_Missions(Minutes: Positive) is
      Time: Integer;
      I: Mission_Container.Extended_Index := Accepted_Missions.First_Index;
   begin
      Update_Missions_Loop :
      while I <= Accepted_Missions.Last_Index loop
         Time := Accepted_Missions(I).Time - Minutes;
         if Time < 1 then
            Delete_Mission(I);
         else
            Accepted_Missions(I).Time := Time;
            I := I + 1;
         end if;
      end loop Update_Missions_Loop;
   end Update_Missions;

   procedure Finish_Mission(Mission_Index: Positive) is
      Message: Unbounded_String;
      MissionsAmount: constant Positive := Positive(Accepted_Missions.Length);
   begin
      if Player_Ship.Speed /= DOCKED then
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            raise Missions_Finishing_Error with To_String(Message);
         end if;
      end if;
      Update_Game(5);
      if MissionsAmount > Natural(Accepted_Missions.Length) then
         return;
      end if;
      case Accepted_Missions(Mission_Index).M_Type is
         when DELIVER =>
            Add_Message
              ("You finished mission 'Deliver " &
               To_String
                 (Items_List(Accepted_Missions(Mission_Index).Item_Index).Name) &
               "'.",
               MISSIONMESSAGE, GREEN);
         when DESTROY =>
            Add_Message
              ("You finished mission 'Destroy " &
               To_String
                 (Proto_Ships_List(Accepted_Missions(Mission_Index).Ship_Index)
                    .Name) &
               "'.",
               MISSIONMESSAGE);
         when PATROL =>
            Add_Message
              ("You finished mission 'Patrol selected area'.", MISSIONMESSAGE,
               GREEN);
         when EXPLORE =>
            Add_Message
              ("You finished mission 'Explore selected area'.", MISSIONMESSAGE,
               GREEN);
         when PASSENGER =>
            Add_Message
              ("You finished mission 'Transport passenger to base'.",
               MISSIONMESSAGE, GREEN);
      end case;
      Update_Goal
        (MISSION,
         To_Unbounded_String
           (Missions_Types'Image(Accepted_Missions(Mission_Index).M_Type)));
      UpdateFinishedMissions
        (To_Unbounded_String
           (Natural'Image
              (Missions_Types'Pos(Accepted_Missions(Mission_Index).M_Type))));
      Delete_Mission(Mission_Index, False);
   end Finish_Mission;

   procedure Delete_Mission(Mission_Index: Positive; Failed: Boolean := True) is
      MessageText: Unbounded_String :=
        To_Unbounded_String("You failed your mission to ");
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
         Gain_Rep(Mission.Start_Base, -Reputation);
         UpdateMorale(Player_Ship, 1, Get_Random(-10, -5));
         case Mission.M_Type is
            when DELIVER =>
               Append
                 (MessageText,
                  "'Deliver " & To_String(Items_List(Mission.Item_Index).Name) &
                  "'.");
            when DESTROY =>
               Append
                 (MessageText,
                  "'Destroy " &
                  To_String(Proto_Ships_List(Mission.Ship_Index).Name) & "'.");
            when PATROL =>
               Append(MessageText, "'Patrol selected area'.");
            when EXPLORE =>
               Append(MessageText, "'Explore selected area'.");
            when PASSENGER =>
               Append(MessageText, "'Transport passenger to base'.");
         end case;
         Add_Message(To_String(MessageText), MISSIONMESSAGE, RED);
      else
         if Mission.M_Type in DELIVER | PASSENGER then
            Gain_Rep
              (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
               (Reputation / 2));
            Gain_Rep(Mission.Start_Base, (Reputation / 2));
         else
            Gain_Rep(Mission.Start_Base, Reputation);
         end if;
         UpdateMorale(Player_Ship, 1, 1);
         declare
            FreeSpace: Integer;
            TraderIndex: constant Natural := FindMember(TALK);
            RewardAmount: Natural :=
              Natural(Float(Mission.Reward) * Float(Mission.Multiplier));
         begin
            Count_Price(RewardAmount, TraderIndex, False);
            if TraderIndex > 0 then
               Gain_Exp(1, Talking_Skill, TraderIndex);
            end if;
            FreeSpace := FreeCargo((0 - RewardAmount));
            if FreeSpace < 0 then
               RewardAmount := RewardAmount + FreeSpace;
            end if;
            if RewardAmount > 0 then
               Add_Message
                 ("You received" & Integer'Image(RewardAmount) & " " &
                  To_String(Money_Name) & " for finishing your mission.",
                  MISSIONMESSAGE);
               UpdateCargo(Player_Ship, Money_Index, RewardAmount);
            end if;
         end;
      end if;
      Sky_Map(Mission.TargetX, Mission.TargetY).Mission_Index := 0;
      Sky_Map
        (Sky_Bases(Mission.StartBase).Sky_X,
         Sky_Bases(Mission.StartBase).Sky_Y)
        .Mission_Index :=
        0;
      Accepted_Missions.Delete(Index => Mission_Index);
      if Mission.MType = Deliver then
         UpdateCargo(Player_Ship, Mission.ItemIndex, -1);
      elsif Mission.MType = Passenger
        and then Mission.Data <= Positive(Player_Ship.Crew.Length) then
         DeleteMember(Mission.Data, Player_Ship);
      end if;
      Update_Map_Loop :
      for I in Accepted_Missions.First_Index .. Accepted_Missions.Last_Index loop
         if Accepted_Missions(I).Finished then
            Sky_Map
              (Sky_Bases(Accepted_Missions(I).StartBase).Sky_X,
               Sky_Bases(Accepted_Missions(I).StartBase).Sky_Y)
              .Mission_Index :=
              I;
         else
            Sky_Map(Accepted_Missions(I).TargetX, Accepted_Missions(I).TargetY)
              .Mission_Index :=
              I;
         end if;
      end loop Update_Map_Loop;
   end Delete_Mission;

   procedure UpdateMission(MissionIndex: Positive) is
      Mission: constant Mission_Data := Accepted_Missions(MissionIndex);
      MessageText: Unbounded_String :=
        To_Unbounded_String("Return to ") & Sky_Bases(Mission.StartBase).Name &
        To_Unbounded_String(" to finish mission ");
   begin
      Sky_Map(Mission.TargetX, Mission.TargetY).Mission_Index := 0;
      Accepted_Missions(MissionIndex).Finished := True;
      Sky_Map
        (Sky_Bases(Mission.StartBase).Sky_X,
         Sky_Bases(Mission.StartBase).Sky_Y)
        .Mission_Index :=
        MissionIndex;
      case Accepted_Missions(MissionIndex).MType is
         when Deliver =>
            Append
              (MessageText,
               "'Deliver " &
               To_String
                 (Items_List(Accepted_Missions(MissionIndex).ItemIndex).Name) &
               "'.");
         when Destroy =>
            Append
              (MessageText,
               "'Destroy " &
               To_String
                 (Proto_Ships_List(Accepted_Missions(MissionIndex).ShipIndex)
                    .Name) &
               "'.");
         when Patrol =>
            Append(MessageText, "'Patrol selected area'.");
         when Explore =>
            Append(MessageText, "'Explore selected area'.");
         when Passenger =>
            Append(MessageText, "'Transport passenger to base'.");
      end case;
      Add_Message(To_String(MessageText), MISSIONMESSAGE);
      if Game_Settings.Auto_Return then
         Player_Ship.Destination_X := Sky_Bases(Mission.StartBase).Sky_X;
         Player_Ship.Destination_Y := Sky_Bases(Mission.StartBase).Sky_Y;
         Add_Message
           ("You set the travel destination for your ship.", ORDERMESSAGE);
      end if;
   end UpdateMission;

   function AutoFinishMissions return String is
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      I: Natural := Accepted_Missions.First_Index;
   begin
      if BaseIndex = 0 then
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
      if FindMember(TALK) = 0 then
         return "";
      end if;
      Finish_Missions_Loop :
      while I <= Accepted_Missions.Last_Index loop
         if
           (Accepted_Missions(I).Finished and
            Accepted_Missions(I).StartBase = BaseIndex) or
           (Accepted_Missions(I).TargetX = Player_Ship.Sky_X and
            Accepted_Missions(I).TargetY = Player_Ship.Sky_Y) then
            FinishMission(I);
            I := I - 1;
         end if;
         I := I + 1;
      end loop Finish_Missions_Loop;
      return "";
   exception
      when An_Exception : Missions_Finishing_Error =>
         return Exception_Message(An_Exception);
   end AutoFinishMissions;

   function Get_Mission_Type(MType: Missions_Types) return String is
   begin
      case MType is
         when Deliver =>
            return "Deliver item to base";
         when Patrol =>
            return "Patrol area";
         when Destroy =>
            return "Destroy ship";
         when Explore =>
            return "Explore area";
         when Passenger =>
            return "Transport passenger to base";
      end case;
   end Get_Mission_Type;

end Missions;
