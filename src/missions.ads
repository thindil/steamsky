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

-- ****t* Missions/Missions_Types
-- SOURCE
   type Missions_Types is (Deliver, Destroy, Patrol, Explore, Passenger);
-- ****

-- ****t* Missions/RewardMultiplier
-- SOURCE
   type RewardMultiplier is digits 2 range 0.0 .. 2.0;
-- ****

-- ****t* Missions/Mission_Data(MType:
-- FUNCTION
-- Data structure for missions
-- SOURCE
   type Mission_Data(MType: Missions_Types := Deliver) is record
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
-- ****

-- ****t* Missions/Mission_Container
-- SOURCE
   package Mission_Container is new Vectors(Positive, Mission_Data);
-- ****

-- ****v* Missions/AcceptedMissions
-- FUNCTION
-- List of missions accepted by player
-- SOURCE
   AcceptedMissions: Mission_Container.Vector;
-- ****
-- ****v* Missions/Missions_Accepting_Error
-- FUNCTION
-- Raised when mission can't be accepted
-- SOURCE
   Missions_Accepting_Error: exception;
-- ****
-- ****v* Missions/Missions_Finishing_Error
-- FUNCTION
-- Raised when mission can't be finished
-- SOURCE
   Missions_Finishing_Error: exception;
-- ****

-- ****f* Missions/GenerateMissions;
-- FUNCTION
-- Generate if needed new missions in base
-- SOURCE
   procedure GenerateMissions;
-- ****
-- ****f* Missions/AcceptMission
-- FUNCTION
-- Accept selected mission from base
-- SOURCE
   procedure AcceptMission(MissionIndex: Positive);
-- ****
-- ****f* Missions/UpdateMissions
-- FUNCTION
-- Update accepted missions
-- SOURCE
   procedure UpdateMissions(Minutes: Positive);
-- ****
-- ****f* Missions/FinishMission
-- FUNCTION
-- Finish selected mission
-- SOURCE
   procedure FinishMission(MissionIndex: Positive) with
      Pre => MissionIndex <= AcceptedMissions.Last_Index;
-- ****
-- ****f* Missions/DeleteMission
-- FUNCTION
-- Delete selected mission
-- SOURCE
   procedure DeleteMission
     (MissionIndex: Positive; Failed: Boolean := True) with
      Pre => MissionIndex <= AcceptedMissions.Last_Index;
-- ****
-- ****f* Missions/UpdateMission
-- FUNCTION
-- Update status of mission
-- SOURCE
   procedure UpdateMission(MissionIndex: Positive) with
      Pre => MissionIndex <= AcceptedMissions.Last_Index;
-- ****
-- ****f* Missions/AutoFinishMissions
-- FUNCTION
-- Finish all possible missions, return empty string if all ok
-- SOURCE
   function AutoFinishMissions return String;
-- ****

end Missions;
