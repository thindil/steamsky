--    Copyright 2016-2019 Bartek thindil Jasicki
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
with Game; use Game;
with Utils; use Utils;
with ShipModules; use ShipModules;
with Config; use Config;
with Events; use Events;
with Goals; use Goals;
with Factions; use Factions;

package body Missions is

   procedure GenerateMissions is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsAmount, MissionX, MissionY, TmpBaseIndex, DiffX, DiffY: Positive;
      Mission: Mission_Data;
      MissionsItems, BasesInRange, Cabins: Positive_Container.Vector;
      MinX, MinY, MaxX, MaxY: Integer;
      Enemies: Positive_Container.Vector;
   begin
      if DaysDifference(SkyBases(BaseIndex).MissionsDate) < 7 or
        SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      case SkyBases(BaseIndex).Population is
         when 1 .. 149 =>
            MissionsAmount := GetRandom(1, 5);
         when 150 .. 299 =>
            MissionsAmount := GetRandom(1, 10);
         when others =>
            MissionsAmount := GetRandom(1, 15);
      end case;
      case SkyBases(BaseIndex).Reputation(1) is
         when 1 .. 25 =>
            MissionsAmount := MissionsAmount + 1;
         when 26 .. 50 =>
            MissionsAmount := MissionsAmount + 3;
         when 51 .. 75 =>
            MissionsAmount := MissionsAmount + 5;
         when 76 .. 100 =>
            MissionsAmount := MissionsAmount + 10;
         when others =>
            null;
      end case;
      for I in Items_List.Iterate loop
         if Items_List(I).IType = MissionItemsType then
            MissionsItems.Append(New_Item => Objects_Container.To_Index(I));
         end if;
      end loop;
      for I in Modules_List.Iterate loop
         if Modules_List(I).MType = CABIN then
            Cabins.Append(New_Item => BaseModules_Container.To_Index(I));
         end if;
      end loop;
      MinX := PlayerShip.SkyX - 100;
      if MinX < 1 then
         MinX := 1;
      end if;
      MaxX := PlayerShip.SkyX + 100;
      if MaxX > 1024 then
         MaxX := 1024;
      end if;
      MinY := PlayerShip.SkyY - 100;
      if MinY < 1 then
         MinY := 1;
      end if;
      MaxY := PlayerShip.SkyY + 100;
      if MaxY > 1024 then
         MaxY := 1024;
      end if;
      for I in SkyBases'Range loop
         if I /= BaseIndex and SkyBases(I).SkyX in MinX .. MaxX and
           SkyBases(I).SkyY in MinY .. MaxY and SkyBases(I).Population > 0 then
            BasesInRange.Append(New_Item => I);
         end if;
      end loop;
      while MissionsAmount > Positive(BasesInRange.Length) loop
         TmpBaseIndex := GetRandom(1, 1024);
         if BasesInRange.Find_Index(Item => TmpBaseIndex) =
           Positive_Container.No_Index and
           SkyBases(TmpBaseIndex).Population > 0 then
            BasesInRange.Append(New_Item => TmpBaseIndex);
         end if;
      end loop;
      SkyBases(BaseIndex).Missions.Clear;
      GenerateEnemies(Enemies);
      for I in 1 .. MissionsAmount loop
         Mission.MType :=
           Missions_Types'Val
             (GetRandom(0, Missions_Types'Pos(Missions_Types'Last)));
         case Mission.MType is
            when Deliver =>
               Mission.Target :=
                 MissionsItems
                   (GetRandom
                      (MissionsItems.First_Index, MissionsItems.Last_Index));
            when Destroy =>
               Mission.Target :=
                 Enemies(GetRandom(Enemies.First_Index, Enemies.Last_Index));
            when Patrol =>
               Mission.Target := 0;
            when Explore =>
               Mission.Target := 1;
               for J in 1 .. 10 loop
                  MissionX := GetRandom(MinX, MaxX);
                  MissionY := GetRandom(MinY, MaxY);
                  if not SkyMap(MissionX, MissionY).Visited then
                     Mission.Target := 0;
                     exit;
                  end if;
               end loop;
               if Mission.Target = 1 then
                  Mission.Target := 0;
                  Mission.MType := Patrol;
               end if;
            when Passenger =>
               Mission.Target :=
                 Cabins(GetRandom(Cabins.First_Index, Cabins.Last_Index));
         end case;
         loop
            if Mission.MType /= Deliver and Mission.MType /= Passenger then
               MissionX := GetRandom(MinX, MaxX);
               MissionY := GetRandom(MinY, MaxY);
               exit when SkyMap(MissionX, MissionY).BaseIndex = 0 and
                 MissionX /= PlayerShip.SkyX and MissionY /= PlayerShip.SkyY;
            else
               TmpBaseIndex :=
                 GetRandom(BasesInRange.First_Index, BasesInRange.Last_Index);
               MissionX := SkyBases(BasesInRange(TmpBaseIndex)).SkyX;
               MissionY := SkyBases(BasesInRange(TmpBaseIndex)).SkyY;
               exit when MissionX /= PlayerShip.SkyX and
                 MissionY /= PlayerShip.SkyY;
            end if;
         end loop;
         Mission.TargetX := MissionX;
         Mission.TargetY := MissionY;
         DiffX := abs (PlayerShip.SkyX - MissionX);
         DiffY := abs (PlayerShip.SkyY - MissionY);
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
      end loop;
      SkyBases(BaseIndex).MissionsDate := GameDate;
   end GenerateMissions;

   procedure AcceptMission(MissionIndex: Positive) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsLimit: Integer;
      Mission: Mission_Data := SkyBases(BaseIndex).Missions(MissionIndex);
      AcceptMessage: Unbounded_String;
      TraderIndex, PassengerBase: Positive;
      HaveCabin: Boolean := False;
      Gender: Character;
      Skills: Skills_Container.Vector;
      Attributes: Attributes_Container.Vector;
      Inventory: Inventory_Container.Vector;
      MaxAttributeLevel, Morale: Integer;
   begin
      if SkyBases(BaseIndex).Reputation(1) < 0 then
         raise Missions_Accepting_Error
           with "Your reputation in this base is too low to receive any mission.";
      end if;
      case SkyBases(BaseIndex).Reputation(1) is
         when 0 .. 25 =>
            MissionsLimit := 1;
         when 26 .. 50 =>
            MissionsLimit := 3;
         when 51 .. 75 =>
            MissionsLimit := 5;
         when 76 .. 100 =>
            MissionsLimit := 10;
         when others =>
            MissionsLimit := 0;
      end case;
      for Mission of AcceptedMissions loop
         if Mission.StartBase = BaseIndex then
            MissionsLimit := MissionsLimit - 1;
         end if;
         exit when MissionsLimit = 0;
      end loop;
      if MissionsLimit < 1 then
         raise Missions_Accepting_Error
           with "You can't take any more missions from this base. ";
      end if;
      if Mission.MType = Deliver then
         if FreeCargo((0 - Items_List(Mission.Target).Weight)) < 0 then
            raise Missions_Accepting_Error
              with "You don't have enough cargo space for take this mission.";
         end if;
      end if;
      if Mission.MType = Passenger then
         for Module of PlayerShip.Modules loop
            if Module.ProtoIndex = Mission.Target and Module.Owner = 0 then
               HaveCabin := True;
               exit;
            end if;
         end loop;
         if not HaveCabin then
            raise Missions_Accepting_Error
              with "You don't have proper (or free) cabin for this passenger.";
         end if;
      end if;
      TraderIndex := FindMember(Talk);
      Mission.StartBase := BaseIndex;
      Mission.Finished := False;
      AcceptMessage := To_Unbounded_String("You accepted mission ");
      case Mission.MType is
         when Deliver =>
            Append
              (AcceptMessage,
               "'Deliver " & To_String(Items_List(Mission.Target).Name) &
               "'.");
            UpdateCargo(PlayerShip, Mission.Target, 1);
         when Destroy =>
            Append
              (AcceptMessage,
               "'Destroy " & To_String(ProtoShips_List(Mission.Target).Name) &
               "'.");
         when Patrol =>
            Append(AcceptMessage, "'Patrol selected area'.");
         when Explore =>
            Append(AcceptMessage, "'Explore selected area'.");
         when Passenger =>
            Append(AcceptMessage, "'Transport passenger to base'.");
            if GetRandom(1, 100) < 60 then
               PassengerBase := BaseIndex;
            else
               PassengerBase := GetRandom(SkyBases'First, SkyBases'Last);
            end if;
            if not Factions_List(SkyBases(PassengerBase).Owner).Flags.Contains
                (To_Unbounded_String("nogender")) then
               if GetRandom(1, 2) = 1 then
                  Gender := 'M';
               else
                  Gender := 'F';
               end if;
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
            PlayerShip.Crew.Append
              (New_Item =>
                 (Name =>
                    GenerateMemberName
                      (Gender,
                       Factions_List(SkyBases(PassengerBase).Owner).Index),
                  Gender => Gender, Health => 100, Tired => 0,
                  Skills => Skills, Hunger => 0, Thirst => 0, Order => Rest,
                  PreviousOrder => Rest, OrderTime => 15,
                  Orders => (others => 0), Attributes => Attributes,
                  Inventory => Inventory, Equipment => (others => 0),
                  Payment => (others => 0), ContractLength => Mission.Time,
                  Morale => (Morale, 0), Loyalty => Morale,
                  HomeBase => PassengerBase,
                  Faction => SkyBases(PassengerBase).Owner));
            for Module of PlayerShip.Modules loop
               if Module.ProtoIndex = Mission.Target and Module.Owner = 0 then
                  Module.Owner := PlayerShip.Crew.Last_Index;
                  exit;
               end if;
            end loop;
            Mission.Target := PlayerShip.Crew.Last_Index;
      end case;
      SkyBases(BaseIndex).Missions.Delete(Index => MissionIndex);
      AcceptedMissions.Append(New_Item => Mission);
      SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex :=
        AcceptedMissions.Last_Index;
      AddMessage(To_String(AcceptMessage), MissionMessage);
      GainExp(1, TalkingSkill, TraderIndex);
      GameStats.AcceptedMissions := GameStats.AcceptedMissions + 1;
      UpdateGame(5);
   end AcceptMission;

   procedure UpdateMissions(Minutes: Positive) is
      Time: Integer;
      I: Positive := AcceptedMissions.First_Index;
   begin
      while I <= AcceptedMissions.Last_Index loop
         Time := AcceptedMissions(I).Time - Minutes;
         if Time < 1 then
            DeleteMission(I);
         else
            AcceptedMissions(I).Time := Time;
            I := I + 1;
         end if;
      end loop;
   end UpdateMissions;

   procedure FinishMission(MissionIndex: Positive) is
      Message: Unbounded_String;
      MissionsAmount: constant Positive := Positive(AcceptedMissions.Length);
   begin
      if PlayerShip.Speed /= DOCKED then
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            raise Missions_Finishing_Error with To_String(Message);
         end if;
      end if;
      UpdateGame(5);
      if MissionsAmount > Positive(AcceptedMissions.Length) then
         return;
      end if;
      case AcceptedMissions(MissionIndex).MType is
         when Deliver =>
            AddMessage
              ("You finished mission 'Deliver " &
               To_String
                 (Items_List(AcceptedMissions(MissionIndex).Target).Name) &
               "'.",
               MissionMessage, 2);
         when Destroy =>
            AddMessage
              ("You finished mission 'Destroy " &
               To_String
                 (ProtoShips_List(AcceptedMissions(MissionIndex).Target)
                    .Name) &
               "'.",
               MissionMessage);
         when Patrol =>
            AddMessage
              ("You finished mission 'Patrol selected area'.", MissionMessage,
               2);
         when Explore =>
            AddMessage
              ("You finished mission 'Explore selected area'.", MissionMessage,
               2);
         when Passenger =>
            AddMessage
              ("You finished mission 'Transport passenger to base'.",
               MissionMessage, 2);
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
        To_Unbounded_String("You failed mission ");
      Mission: constant Mission_Data := AcceptedMissions(MissionIndex);
      FreeSpace, RewardAmount: Integer;
      Reputation: Natural;
   begin
      Reputation := Mission.Reward / 50;
      if Reputation < 2 then
         Reputation := 2;
      end if;
      if Failed then
         GainRep(Mission.StartBase, (Reputation * (-1)));
         UpdateMorale(PlayerShip, 1, GetRandom(-10, -5));
         case Mission.MType is
            when Deliver =>
               Append
                 (MessageText,
                  "'Deliver " & To_String(Items_List(Mission.Target).Name) &
                  "'.");
            when Destroy =>
               Append
                 (MessageText,
                  "'Destroy " &
                  To_String(ProtoShips_List(Mission.Target).Name) & "'.");
            when Patrol =>
               Append(MessageText, "'Patrol selected area'.");
            when Explore =>
               Append(MessageText, "'Explore selected area'.");
            when Passenger =>
               Append(MessageText, "'Transport passenger to base'.");
         end case;
         AddMessage(To_String(MessageText), MissionMessage, 3);
      else
         if Mission.MType = Deliver or Mission.MType = Passenger then
            GainRep
              (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex,
               (Reputation / 2));
            GainRep(Mission.StartBase, (Reputation / 2));
         else
            GainRep(Mission.StartBase, Reputation);
         end if;
         UpdateMorale(PlayerShip, 1, 1);
         RewardAmount := Mission.Reward;
         FreeSpace := FreeCargo((0 - RewardAmount));
         if FreeSpace < 0 then
            RewardAmount := RewardAmount + FreeSpace;
         end if;
         if RewardAmount > 0 then
            AddMessage
              ("You received" & Integer'Image(RewardAmount) & " " &
               To_String(MoneyName) & " for finished mission.",
               MissionMessage);
            UpdateCargo(PlayerShip, FindProtoItem(MoneyIndex), RewardAmount);
         end if;
      end if;
      SkyMap(Mission.TargetX, Mission.TargetY).MissionIndex := 0;
      SkyMap
        (SkyBases(Mission.StartBase).SkyX, SkyBases(Mission.StartBase).SkyY)
        .MissionIndex :=
        0;
      AcceptedMissions.Delete(Index => MissionIndex);
      if Mission.MType = Deliver then
         UpdateCargo(PlayerShip, Mission.Target, -1);
      elsif Mission.MType = Passenger then
         DeleteMember(Mission.Target, PlayerShip);
      end if;
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
      end loop;
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
                 (Items_List(AcceptedMissions(MissionIndex).Target).Name) &
               "'.");
         when Destroy =>
            Append
              (MessageText,
               "'Destroy " &
               To_String
                 (ProtoShips_List(AcceptedMissions(MissionIndex).Target)
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
      if GameSettings.AutoReturn then
         PlayerShip.DestinationX := SkyBases(Mission.StartBase).SkyX;
         PlayerShip.DestinationY := SkyBases(Mission.StartBase).SkyY;
         AddMessage("You set travel destination for your ship.", OrderMessage);
      end if;
   end UpdateMission;

   function AutoFinishMissions return String is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      I: Natural := AcceptedMissions.First_Index;
   begin
      if BaseIndex = 0 then
         return "";
      end if;
      if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
         if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .EType /=
           DoublePrice then
            return "";
         end if;
      end if;
      if FindMember(Talk) = 0 then
         return "";
      end if;
      while I <= AcceptedMissions.Last_Index loop
         if
           (AcceptedMissions(I).Finished and
            AcceptedMissions(I).StartBase = BaseIndex) or
           (AcceptedMissions(I).TargetX = PlayerShip.SkyX and
            AcceptedMissions(I).TargetY = PlayerShip.SkyY) then
            FinishMission(I);
            I := I - 1;
         end if;
         I := I + 1;
      end loop;
      return "";
   exception
      when An_Exception : Missions_Finishing_Error =>
         return Exception_Message(An_Exception);
   end AutoFinishMissions;

end Missions;
