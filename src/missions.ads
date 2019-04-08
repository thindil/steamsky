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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Missions is

   type Missions_Types is (Deliver, Destroy, Patrol, Explore, Passenger);
   type RewardMultiplier is digits 2 range 0.0 .. 2.0;
   type Mission_Data(MType: Missions_Types := Deliver)
   is -- Data structure for missions
   record
      Time: Positive; -- Amount of minutes to finish the mission
      TargetX: Natural; -- Skymap X-axis for the mission target
      TargetY: Natural; -- Skymap Y-axis for the mission target
      Reward: Positive; -- Amount of money reward for the mission
      StartBase: Positive; -- Index of sky base where the mission starts
      Finished: Boolean; -- Did the mission is finished
      Multiplier: RewardMultiplier; -- Bonus to amount of money or reputation rewards for the mission
      case MType is
         when Deliver =>
            ItemIndex: Unbounded_String; -- Index of proto item to deliver to base
         when Passenger =>
            Data: Positive; -- Minimum quality of cabin needed by passenger (in bases) or passenger index (in player ship)
         when Destroy =>
            ShipIndex: Unbounded_String; -- Index of proto ship which must be destroyed
         when others =>
            Target: Natural;  -- Target for mission (ship, item)
      end case;
   end record;
   package Mission_Container is new Vectors(Positive, Mission_Data);
   AcceptedMissions: Mission_Container
     .Vector; -- List of missions accepted by player
   Missions_Accepting_Error: exception; -- Raised when mission can't be accepted
   Missions_Finishing_Error: exception; -- Raised when mission can't be finished

   procedure GenerateMissions; -- Generate if needed new missions in base
   procedure AcceptMission
     (MissionIndex: Positive); -- Accept selected mission from base
   procedure UpdateMissions(Minutes: Positive); -- Update accepted missions
   procedure FinishMission(MissionIndex: Positive) with
      Pre => MissionIndex <=
      AcceptedMissions.Last_Index; -- Finish selected mission
   procedure DeleteMission(MissionIndex: Positive;
      Failed: Boolean := True) with
      Pre => MissionIndex <=
      AcceptedMissions.Last_Index; -- Delete selected mission
   procedure UpdateMission(MissionIndex: Positive) with
      Pre => MissionIndex <=
      AcceptedMissions.Last_Index; -- Update status of mission
   function AutoFinishMissions
     return String; -- Finish all possible missions, return empty string if all ok

end Missions;
