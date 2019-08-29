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

-- ****h* Steamsky/Missions
-- FUNCTION
-- Provides code for manipulate missions data
-- SOURCE
package Missions is
-- ****

   -- ****t* Missions/Missions_Types
   -- FUNCTION
   -- Types of missions
   -- SOURCE
   type Missions_Types is (Deliver, Destroy, Patrol, Explore, Passenger);
   -- ****

   -- ****t* Missions/RewardMultiplier
   -- FUNCTION
   -- Used for count reward for finished missions
   -- SOURCE
   type RewardMultiplier is digits 2 range 0.0 .. 2.0;
   -- ****

   -- ****t* Missions/Mission_Data(MType:
   -- FUNCTION
   -- Data structure for missions
   -- PARAMETERS
   -- Time       - Amount of minutes to finish the mission
   -- TargetX    - Skymap X-axis for the mission target
   -- TargetY    - Skymap Y-axis for the mission target
   -- Reward     - Amount of money reward for the mission
   -- StartBase  - Index of sky base where the mission starts
   -- Finished   - Did the mission is finished
   -- Multiplier - Bonus to amount of money or reputation rewards for the
   --              mission
   -- ItemIndex  - Index of proto item to deliver to base
   -- Data       - Minimum quality of cabin needed by passenger (in bases)
   --              or passenger index (in player ship)
   -- ShipIndex  - Index of proto ship which must be destroyed
   -- Target     - Target for mission (ship, item)
   -- SOURCE
   type Mission_Data(MType: Missions_Types := Deliver) is record
      Time: Positive;
      TargetX: Natural;
      TargetY: Natural;
      Reward: Positive;
      StartBase: Positive;
      Finished: Boolean;
      Multiplier: RewardMultiplier;
      case MType is
         when Deliver =>
            ItemIndex: Unbounded_String;
         when Passenger =>
            Data: Positive;
         when Destroy =>
            ShipIndex: Unbounded_String;
         when others =>
            Target: Natural;
      end case;
   end record;
   -- ****

   -- ****t* Missions/Mission_Container
   -- FUNCTION
   -- Used to store data for missions
   -- SOURCE
   package Mission_Container is new Vectors(Positive, Mission_Data);
   -- ****

   -- ****v* Missions/AcceptedMissions
   -- FUNCTION
   -- List of missions accepted by player
   -- SOURCE
   AcceptedMissions: Mission_Container.Vector;
   -- ****

   -- ****e* Missions/Missions_Accepting_Error
   -- FUNCTION
   -- Raised when mission can't be accepted
   -- SOURCE
   Missions_Accepting_Error: exception;
   -- ****

   -- ****e* Missions/Missions_Finishing_Error
   -- FUNCTION
   -- Raised when mission can't be finished
   -- SOURCE
   Missions_Finishing_Error: exception;
   -- ****

   -- ****f* Missions/GenerateMissions
   -- FUNCTION
   -- Generate if needed new missions in base
   -- SOURCE
   procedure GenerateMissions with
      Test_Case => ("Test_GenerateMissions", Robustness);
      -- ****

      -- ****f* Missions/AcceptMission
      -- FUNCTION
      -- Accept selected mission from base
      -- PARAMETERS
      -- MissionIndex - Base list of available missions index of mission to
      --                accept
      -- SOURCE
   procedure AcceptMission(MissionIndex: Positive) with
      Test_Case => ("Test_AcceptMission", Nominal);
      -- ****

      -- ****f* Missions/UpdateMissions
      -- FUNCTION
      -- Update accepted missions
      -- PARAMETERS
      -- Minutes - Amount of passed minutes
      -- SOURCE
   procedure UpdateMissions(Minutes: Positive) with
      Test_Case => ("Test_UpdateMissions", Robustness);
      -- ****

      -- ****f* Missions/FinishMission
      -- FUNCTION
      -- Finish selected mission
      -- PARAMETERS
      -- MissionIndex - Player ship list of accepted missions index of mission
      --                to finish
      -- SOURCE
   procedure FinishMission(MissionIndex: Positive) with
      Pre => MissionIndex <= AcceptedMissions.Last_Index,
      Test_Case => ("Test_FinishMission", Nominal);
      -- ****

      -- ****f* Missions/DeleteMission
      -- FUNCTION
      -- Delete selected mission
      -- PARAMETERS
      -- MissionIndex - Player ship list of accepted missions index of mission
      --                to delete
      -- Failed       - If true, it is failed mission. Default is true.
      -- SOURCE
   procedure DeleteMission
     (MissionIndex: Positive; Failed: Boolean := True) with
      Pre => MissionIndex <= AcceptedMissions.Last_Index,
      Test_Case => ("Test_DeleteMission", Nominal);
      -- ****

      -- ****f* Missions/UpdateMission
      -- FUNCTION
      -- Update status of mission
      -- PARAMETERS
      -- MissionIndex - Player ship list of accepted missions index of mission
      --                to update
      -- SOURCE
   procedure UpdateMission(MissionIndex: Positive) with
      Pre => MissionIndex <= AcceptedMissions.Last_Index,
      Test_Case => ("Test_UpdateMission", Nominal);
      -- ****

      -- ****f* Missions/AutoFinishMissions
      -- FUNCTION
      -- Finish all possible missions.
      -- RESULT
      -- Empty string if everything is ok, otherwise message with information
      -- what goes wrong
      -- SOURCE
   function AutoFinishMissions return String with
      Test_Case => ("Test_AutoFinishMissions", Robustness);
      -- ****

end Missions;
