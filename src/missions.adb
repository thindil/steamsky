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

   procedure GenerateMissions is
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      MissionX, MissionY: Positive range 1 .. 1_024;
      MissionsAmount: Positive range 1 .. 26;
      TmpBaseIndex: Bases_Range;
      Mission: Mission_Data;
      MissionsItems: UnboundedString_Container.Vector;
      BasesInRange: Positive_Container.Vector;
      MinX, MinY, MaxX, MaxY: Integer;
      Enemies: UnboundedString_Container.Vector;
      MType: Missions_Types;
      DiffX, DiffY: Natural;
      QualitiesArray: constant array(1 .. 10) of Positive :=
        (1, 11, 21, 31, 41, 51, 61, 71, 81, 91);
   begin
      if DaysDifference(SkyBases(BaseIndex).MissionsDate) < 7 or
        SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      MissionsAmount :=
        (case SkyBases(BaseIndex).Population is
           when 1 .. 149 => GetRandom(1, 5),
           when 150 .. 299 => GetRandom(1, 10),
           when others => GetRandom(1, 15));
      MissionsAmount :=
        (case SkyBases(BaseIndex).Reputation(1) is
           when 1 .. 25 => MissionsAmount + 1,
           when 26 .. 50 => MissionsAmount + 3,
           when 51 .. 75 => MissionsAmount + 5,
           when 76 .. 100 => MissionsAmount + 10,
           when others => MissionsAmount);
      for I in Items_List.Iterate loop
         if Items_List(I).IType = Mission_Items_Type then
            MissionsItems.Append(New_Item => Objects_Container.Key(I));
         end if;
      end loop;
      MinX := Player_Ship.Sky_X - 100;
      NormalizeCoord(MinX);
      MaxX := Player_Ship.Sky_X + 100;
      NormalizeCoord(MaxX);
      MinY := Player_Ship.Sky_Y - 100;
      NormalizeCoord(MinY, False);
      MaxY := Player_Ship.Sky_Y + 100;
      NormalizeCoord(MaxY, False);
      Find_Bases_In_Range_Loop :
      for I in SkyBases'Range loop
         if I /= BaseIndex and SkyBases(I).SkyX in MinX .. MaxX and
           SkyBases(I).SkyY in MinY .. MaxY and SkyBases(I).Population > 0 then
            BasesInRange.Append(New_Item => I);
         end if;
      end loop Find_Bases_In_Range_Loop;
      Get_Random_Bases_Loop :
      while MissionsAmount > Positive(BasesInRange.Length) loop
         TmpBaseIndex := GetRandom(1, 1_024);
         if BasesInRange.Find_Index(Item => TmpBaseIndex) =
           Positive_Container.No_Index and
           SkyBases(TmpBaseIndex).Population > 0 then
            BasesInRange.Append(New_Item => TmpBaseIndex);
         end if;
      end loop Get_Random_Bases_Loop;
      SkyBases(BaseIndex).Missions.Clear;
      GenerateEnemies(Enemies);
      Generate_Missions_Loop :
      for I in 1 .. MissionsAmount loop
         <<Start_Of_Loop>>
         MType :=
           Missions_Types'Val
             (GetRandom(0, Missions_Types'Pos(Missions_Types'Last)));
         case MType is
            when Deliver =>
               Mission :=
                 (MType => Deliver, Time => 1, TargetX => 0, TargetY => 0,
                  Reward => 1, StartBase => 1, Finished => False,
                  ItemIndex =>
                    MissionsItems
                      (GetRandom(1, Positive(MissionsItems.Length))),
                  Multiplier => 1.0);
            when Destroy =>
               Mission :=
                 (MType => Destroy, Time => 1, TargetX => 0, TargetY => 0,
                  Reward => 1, StartBase => 1, Finished => False,
                  Multiplier => 1.0,
                  ShipIndex =>
                    Enemies
                      (GetRandom(Enemies.First_Index, Enemies.Last_Index)));
               Find_Mission_Location_Loop :
               loop
                  MissionX := GetRandom(MinX, MaxX);
                  MissionY := GetRandom(MinY, MaxY);
                  exit Find_Mission_Location_Loop when SkyMap
                      (MissionX, MissionY)
                      .BaseIndex =
                    0 and
                    MissionX /= Player_Ship.Sky_X and
                    MissionY /= Player_Ship.Sky_Y;
               end loop Find_Mission_Location_Loop;
            when Patrol =>
               Mission :=
                 (MType => Patrol, Time => 1, TargetX => 0, TargetY => 0,
                  Reward => 1, StartBase => 1, Finished => False,
                  Multiplier => 1.0, Target => 1);
               Find_Patrol_Mission_Location_Loop :
               for J in 1 .. 10 loop
                  MissionX := GetRandom(MinX, MaxX);
                  MissionY := GetRandom(MinY, MaxY);
                  if SkyMap(MissionX, MissionY).Visited and
                    SkyMap(MissionX, MissionY).BaseIndex = 0 then
                     Mission.Target := 0;
                     exit Find_Patrol_Mission_Location_Loop;
                  end if;
               end loop Find_Patrol_Mission_Location_Loop;
               if Mission.Target = 1 then
                  goto Start_Of_Loop;
               end if;
            when Explore =>
               Mission :=
                 (MType => Explore, Time => 1, TargetX => 0, TargetY => 0,
                  Reward => 1, StartBase => 1, Finished => False,
                  Multiplier => 1.0, Target => 1);
               Find_Explore_Location_Loop :
               for J in 1 .. 10 loop
                  MissionX := GetRandom(MinX, MaxX);
                  MissionY := GetRandom(MinY, MaxY);
                  if not SkyMap(MissionX, MissionY).Visited and
                    SkyMap(MissionX, MissionY).BaseIndex = 0 then
                     Mission.Target := 0;
                     exit Find_Explore_Location_Loop;
                  end if;
               end loop Find_Explore_Location_Loop;
               if Mission.Target = 1 then
                  goto Start_Of_Loop;
               end if;
            when Passenger =>
               Mission :=
                 (MType => Passenger, Time => 1, TargetX => 0, TargetY => 0,
                  Reward => 1, StartBase => 1, Finished => False,
                  Multiplier => 1.0,
                  Data =>
                    QualitiesArray
                      (GetRandom(QualitiesArray'First, QualitiesArray'Last)));
         end case;
         if Mission.MType in Deliver | Passenger then
            Find_Base_Mission_Loop :
            loop
               TmpBaseIndex :=
                 GetRandom(BasesInRange.First_Index, BasesInRange.Last_Index);
               MissionX := SkyBases(BasesInRange(TmpBaseIndex)).SkyX;
               MissionY := SkyBases(BasesInRange(TmpBaseIndex)).SkyY;
               exit Find_Base_Mission_Loop when MissionX /=
                 Player_Ship.Sky_X and
                 MissionY /= Player_Ship.Sky_Y;
            end loop Find_Base_Mission_Loop;
         end if;
         Mission.TargetX := MissionX;
         Mission.TargetY := MissionY;
         DiffX := abs (Player_Ship.Sky_X - MissionX);
         DiffY := abs (Player_Ship.Sky_Y - MissionY);
         case Mission.MType is
            when Deliver =>
               Mission.Time :=
                 Positive(80.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
               Mission.Reward := (Mission.Time / 4);
            when Destroy | Passenger =>
               Mission.Time :=
                 Positive(180.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
               Mission.Reward := (Mission.Time / 4);
            when Patrol | Explore =>
               Mission.Time :=
                 Positive(180.0 * Sqrt(Float((DiffX**2) + (DiffY**2))));
               Mission.Reward := (Mission.Time / 5);
         end case;
         Mission.StartBase := BaseIndex;
         Mission.Finished := False;
         SkyBases(BaseIndex).Missions.Append(New_Item => Mission);
      end loop Generate_Missions_Loop;
      SkyBases(BaseIndex).MissionsDate := Game_Date;
   end GenerateMissions;

   procedure AcceptMission(MissionIndex: Positive) is
      BaseIndex: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Mission: Mission_Data := SkyBases(BaseIndex).Missions(MissionIndex);
      AcceptMessage: Unbounded_String;
      TraderIndex: constant Crew_Container.Extended_Index := FindMember(Talk);
   begin
      if SkyBases(BaseIndex).Reputation(1) < 0 then
         raise Missions_Accepting_Error
           with "Your reputation in this base is too low to receive any mission.";
      end if;
      declare
         MissionsLimit: Integer :=
           (case SkyBases(BaseIndex).Reputation(1) is when 0 .. 25 => 1,
              when 26 .. 50 => 3, when 51 .. 75 => 5, when 76 .. 100 => 10,
              when others => 0);
      begin
         Count_Missions_Limit_Loop :
         for Mission of AcceptedMissions loop
            if Mission.StartBase = BaseIndex then
               MissionsLimit := MissionsLimit - 1;
            end if;
            exit Count_Missions_Limit_Loop when MissionsLimit <= 0;
         end loop Count_Missions_Limit_Loop;
         if MissionsLimit < 1 then
            raise Missions_Accepting_Error
              with "You can't take any more missions from this base. ";
         end if;
      end;
      if Mission.MType = Deliver
        and then FreeCargo((0 - Items_List(Mission.ItemIndex).Weight)) < 0 then
         raise Missions_Accepting_Error
           with "You don't have enough cargo space for take this mission.";
      end if;
      if Mission.MType = Passenger then
         declare
            HaveCabin: Boolean := False;
         begin
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not HaveCabin)
                 and then Module.Quality >= Mission.Data then
                  HaveCabin := True;
                  Cabin_Owner_Loop :
                  for Owner of Module.Owner loop
                     if Owner > 0 then
                        HaveCabin := False;
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
      Mission.StartBase := BaseIndex;
      Mission.Finished := False;
      AcceptMessage := To_Unbounded_String("You accepted the mission to ");
      case Mission.MType is
         when Deliver =>
            Append
              (AcceptMessage,
               "'Deliver " & To_String(Items_List(Mission.ItemIndex).Name) &
               "'.");
            UpdateCargo(Player_Ship, Mission.ItemIndex, 1);
         when Destroy =>
            Append
              (AcceptMessage,
               "'Destroy " &
               To_String(Proto_Ships_List(Mission.ShipIndex).Name) & "'.");
         when Patrol =>
            Append(AcceptMessage, "'Patrol selected area'.");
         when Explore =>
            Append(AcceptMessage, "'Explore selected area'.");
         when Passenger =>
            Append(AcceptMessage, "'Transport passenger to base'.");
            declare
               PassengerBase: Bases_Range;
               Gender: Character;
               Skills: Skills_Container.Vector;
               Attributes: Attributes_Container.Vector;
               Inventory: Inventory_Container.Vector;
               MaxAttributeLevel, Morale: Integer;
            begin
               PassengerBase :=
                 (if GetRandom(1, 100) < 60 then BaseIndex
                  else GetRandom(SkyBases'First, SkyBases'Last));
               if not Factions_List(SkyBases(PassengerBase).Owner).Flags
                   .Contains
                   (To_Unbounded_String("nogender")) then
                  Gender := (if GetRandom(1, 2) = 1 then 'M' else 'F');
               else
                  Gender := 'M';
               end if;
               if Factions_List(SkyBases(PassengerBase).Owner).Flags.Contains
                   (To_Unbounded_String("nomorale")) then
                  Morale := 50;
               else
                  Morale := 50 + SkyBases(PassengerBase).Reputation(1);
                  if Morale < 50 then
                     Morale := 50;
                  end if;
               end if;
               MaxAttributeLevel := SkyBases(BaseIndex).Reputation(1);
               if MaxAttributeLevel < 10 then
                  MaxAttributeLevel := 10;
               end if;
               if GetRandom(1, 100) > 90 then
                  MaxAttributeLevel := GetRandom(MaxAttributeLevel, 100);
               end if;
               for J in Attributes_List.Iterate loop
                  Attributes.Append
                    (New_Item => (GetRandom(3, MaxAttributeLevel), 0));
               end loop;
               Player_Ship.Crew.Append
                 (New_Item =>
                    (Name =>
                       GenerateMemberName
                         (Gender, SkyBases(PassengerBase).Owner),
                     Gender => Gender, Health => 100, Tired => 0,
                     Skills => Skills, Hunger => 0, Thirst => 0, Order => Rest,
                     PreviousOrder => Rest, OrderTime => 15,
                     Orders => (others => 0), Attributes => Attributes,
                     Inventory => Inventory, Equipment => (others => 0),
                     Payment => (others => 0), ContractLength => Mission.Time,
                     Morale => (Morale, 0), Loyalty => Morale,
                     HomeBase => PassengerBase,
                     Faction => SkyBases(PassengerBase).Owner));
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
      SkyBases(BaseIndex).Missions.Delete(Index => MissionIndex);
      AcceptedMissions.Append(New_Item => Mission);
      SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex :=
        AcceptedMissions.Last_Index;
      AddMessage(To_String(AcceptMessage), MissionMessage);
      GainExp(1, Talking_Skill, TraderIndex);
      GameStats.AcceptedMissions := GameStats.AcceptedMissions + 1;
      Update_Game(5);
   end AcceptMission;

   procedure UpdateMissions(Minutes: Positive) is
      Time: Integer;
      I: Mission_Container.Extended_Index := AcceptedMissions.First_Index;
   begin
      Update_Missions_Loop :
      while I <= AcceptedMissions.Last_Index loop
         Time := AcceptedMissions(I).Time - Minutes;
         if Time < 1 then
            DeleteMission(I);
         else
            AcceptedMissions(I).Time := Time;
            I := I + 1;
         end if;
      end loop Update_Missions_Loop;
   end UpdateMissions;

   procedure FinishMission(MissionIndex: Positive) is
      Message: Unbounded_String;
      MissionsAmount: constant Positive := Positive(AcceptedMissions.Length);
   begin
      if Player_Ship.Speed /= DOCKED then
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            raise Missions_Finishing_Error with To_String(Message);
         end if;
      end if;
      Update_Game(5);
      if MissionsAmount > Natural(AcceptedMissions.Length) then
         return;
      end if;
      case AcceptedMissions(MissionIndex).MType is
         when Deliver =>
            AddMessage
              ("You finished mission 'Deliver " &
               To_String
                 (Items_List(AcceptedMissions(MissionIndex).ItemIndex).Name) &
               "'.",
               MissionMessage, GREEN);
         when Destroy =>
            AddMessage
              ("You finished mission 'Destroy " &
               To_String
                 (Proto_Ships_List(AcceptedMissions(MissionIndex).ShipIndex)
                    .Name) &
               "'.",
               MissionMessage);
         when Patrol =>
            AddMessage
              ("You finished mission 'Patrol selected area'.", MissionMessage,
               GREEN);
         when Explore =>
            AddMessage
              ("You finished mission 'Explore selected area'.", MissionMessage,
               GREEN);
         when Passenger =>
            AddMessage
              ("You finished mission 'Transport passenger to base'.",
               MissionMessage, GREEN);
      end case;
      UpdateGoal
        (MISSION,
         To_Unbounded_String
           (Missions_Types'Image(AcceptedMissions(MissionIndex).MType)));
      UpdateFinishedMissions
        (To_Unbounded_String
           (Natural'Image
              (Missions_Types'Pos(AcceptedMissions(MissionIndex).MType))));
      DeleteMission(MissionIndex, False);
   end FinishMission;

   procedure DeleteMission(MissionIndex: Positive; Failed: Boolean := True) is
      MessageText: Unbounded_String :=
        To_Unbounded_String("You failed your mission to ");
      Mission: constant Mission_Data := AcceptedMissions(MissionIndex);
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
         GainRep(Mission.StartBase, -Reputation);
         UpdateMorale(Player_Ship, 1, GetRandom(-10, -5));
         case Mission.MType is
            when Deliver =>
               Append
                 (MessageText,
                  "'Deliver " & To_String(Items_List(Mission.ItemIndex).Name) &
                  "'.");
            when Destroy =>
               Append
                 (MessageText,
                  "'Destroy " &
                  To_String(Proto_Ships_List(Mission.ShipIndex).Name) & "'.");
            when Patrol =>
               Append(MessageText, "'Patrol selected area'.");
            when Explore =>
               Append(MessageText, "'Explore selected area'.");
            when Passenger =>
               Append(MessageText, "'Transport passenger to base'.");
         end case;
         AddMessage(To_String(MessageText), MissionMessage, RED);
      else
         if Mission.MType in Deliver | Passenger then
            GainRep
              (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex,
               (Reputation / 2));
            GainRep(Mission.StartBase, (Reputation / 2));
         else
            GainRep(Mission.StartBase, Reputation);
         end if;
         UpdateMorale(Player_Ship, 1, 1);
         declare
            FreeSpace: Integer;
            TraderIndex: constant Natural := FindMember(Talk);
            RewardAmount: Natural :=
              Natural(Float(Mission.Reward) * Float(Mission.Multiplier));
         begin
            CountPrice(RewardAmount, TraderIndex, False);
            if TraderIndex > 0 then
               GainExp(1, Talking_Skill, TraderIndex);
            end if;
            FreeSpace := FreeCargo((0 - RewardAmount));
            if FreeSpace < 0 then
               RewardAmount := RewardAmount + FreeSpace;
            end if;
            if RewardAmount > 0 then
               AddMessage
                 ("You received" & Integer'Image(RewardAmount) & " " &
                  To_String(Money_Name) & " for finishing your mission.",
                  MissionMessage);
               UpdateCargo(Player_Ship, Money_Index, RewardAmount);
            end if;
         end;
      end if;
      SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := 0;
      SkyMap
        (SkyBases(Mission.StartBase).SkyX, SkyBases(Mission.StartBase).SkyY)
        .MissionIndex :=
        0;
      if Mission.MType = Deliver then
         UpdateCargo(Player_Ship, Mission.ItemIndex, -1);
      elsif Mission.MType = Passenger then
         DeleteMember(Mission.Data, Player_Ship);
      end if;
      AcceptedMissions.Delete(Index => MissionIndex);
      Update_Map_Loop :
      for I in AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
         if AcceptedMissions(I).Finished then
            SkyMap
              (SkyBases(AcceptedMissions(I).StartBase).SkyX,
               SkyBases(AcceptedMissions(I).StartBase).SkyY)
              .MissionIndex :=
              I;
         else
            SkyMap(AcceptedMissions(I).TargetX, AcceptedMissions(I).TargetY)
              .MissionIndex :=
              I;
         end if;
      end loop Update_Map_Loop;
   end DeleteMission;

   procedure UpdateMission(MissionIndex: Positive) is
      Mission: constant Mission_Data := AcceptedMissions(MissionIndex);
      MessageText: Unbounded_String :=
        To_Unbounded_String("Return to ") & SkyBases(Mission.StartBase).Name &
        To_Unbounded_String(" to finish mission ");
   begin
      SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := 0;
      AcceptedMissions(MissionIndex).Finished := True;
      SkyMap
        (SkyBases(Mission.StartBase).SkyX, SkyBases(Mission.StartBase).SkyY)
        .MissionIndex :=
        MissionIndex;
      case AcceptedMissions(MissionIndex).MType is
         when Deliver =>
            Append
              (MessageText,
               "'Deliver " &
               To_String
                 (Items_List(AcceptedMissions(MissionIndex).ItemIndex).Name) &
               "'.");
         when Destroy =>
            Append
              (MessageText,
               "'Destroy " &
               To_String
                 (Proto_Ships_List(AcceptedMissions(MissionIndex).ShipIndex)
                    .Name) &
               "'.");
         when Patrol =>
            Append(MessageText, "'Patrol selected area'.");
         when Explore =>
            Append(MessageText, "'Explore selected area'.");
         when Passenger =>
            Append(MessageText, "'Transport passenger to base'.");
      end case;
      AddMessage(To_String(MessageText), MissionMessage);
      if Game_Settings.Auto_Return then
         Player_Ship.Destination_X := SkyBases(Mission.StartBase).SkyX;
         Player_Ship.Destination_Y := SkyBases(Mission.StartBase).SkyY;
         AddMessage
           ("You set the travel destination for your ship.", OrderMessage);
      end if;
   end UpdateMission;

   function AutoFinishMissions return String is
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      I: Natural := AcceptedMissions.First_Index;
   begin
      if BaseIndex = 0 then
         return "";
      end if;
      if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex > 0
        and then
          Events_List(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
            .EType /=
          DoublePrice then
         return "";
      end if;
      if FindMember(Talk) = 0 then
         return "";
      end if;
      Finish_Missions_Loop :
      while I <= AcceptedMissions.Last_Index loop
         if
           (AcceptedMissions(I).Finished and
            AcceptedMissions(I).StartBase = BaseIndex) or
           (AcceptedMissions(I).TargetX = Player_Ship.Sky_X and
            AcceptedMissions(I).TargetY = Player_Ship.Sky_Y) then
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
