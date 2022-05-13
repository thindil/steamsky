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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Game; use Game;
with Items; use Items;

-- ****h* Missions/Missions
-- FUNCTION
-- Provides code for manipulate missions data
-- SOURCE
package Missions is
-- ****

   -- ****t* Missions/Missions.Missions_Types
   -- FUNCTION
   -- Types of missions
   -- SOURCE
   type Missions_Types is (DELIVER, DESTROY, PATROL, EXPLORE, PASSENGER) with
      Default_Value => DELIVER;
   -- ****

   -- ****t* Missions/Missions.Reward_Multiplier
   -- FUNCTION
   -- Used for count reward for finished missions
   -- SOURCE
   type Reward_Multiplier is digits 2 range 0.0 .. 2.0 with
      Default_Value => 1.0;
   -- ****

   -- ****s* Missions/Missions.Mission_Data:
   -- FUNCTION
   -- Data structure for missions
   -- PARAMETERS
   -- Time        - Amount of minutes to finish the mission
   -- Target_X    - Skymap X-axis for the mission target
   -- Target_Y    - Skymap Y-axis for the mission target
   -- Reward      - Amount of money reward for the mission
   -- Start_Base  - Index of sky base where the mission starts
   -- Finished    - Did the mission is finished
   -- Multiplier  - Bonus to amount of money or reputation rewards for the
   --               mission
   -- Item_Index  - Index of proto item to deliver to base
   -- Data        - Minimum quality of cabin needed by passenger (in bases)
   --               or passenger index (in player ship)
   -- Ship_Index  - Index of proto ship which must be destroyed
   -- Target      - Target for mission (ship, item)
   -- SOURCE
   type Mission_Data(M_Type: Missions_Types := DELIVER) is record
      Time: Positive := 1;
      Target_X: Natural range 0 .. Map_X_Range'Last;
      Target_Y: Natural range 0 .. Map_Y_Range'Last;
      Reward: Positive := 1;
      Start_Base: Bases_Range := 1;
      Finished: Boolean;
      Multiplier: Reward_Multiplier := 1.0;
      case M_Type is
         when DELIVER =>
            Item_Index: Objects_Container.Extended_Index;
         when PASSENGER =>
            Data: Positive := 1;
         when DESTROY =>
            Ship_Index: Natural := 0;
         when others =>
            Target: Natural := 0;
      end case;
   end record;
   -- ****

   -- ****t* Missions/Missions.Mission_Container
   -- FUNCTION
   -- Used to store data for missions
   -- SOURCE
   package Mission_Container is new Vectors
     (Index_Type => Positive, Element_Type => Mission_Data);
   -- ****

   -- ****v* Missions/Missions.Accepted_Missions
   -- FUNCTION
   -- List of missions accepted by player
   -- SOURCE
   Accepted_Missions: Mission_Container.Vector;
   -- ****

   -- ****e* Missions/Missions.Missions_Accepting_Error
   -- FUNCTION
   -- Raised when mission can't be accepted
   -- SOURCE
   Missions_Accepting_Error: exception;
   -- ****

   -- ****e* Missions/Missions.Missions_Finishing_Error
   -- FUNCTION
   -- Raised when mission can't be finished
   -- SOURCE
   Missions_Finishing_Error: exception;
   -- ****

   -- ****f* Missions/Missions.Generate_Missions
   -- FUNCTION
   -- Generate if needed new missions in base
   -- SOURCE
   procedure Generate_Missions with
      Test_Case => (Name => "Test_GenerateMissions", Mode => Robustness);
      -- ****

      -- ****f* Missions/Missions.Accept_Mission
      -- FUNCTION
      -- Accept selected mission from base
      -- PARAMETERS
      -- Mission_Index - Base list of available missions index of mission to
      --                 accept
      -- SOURCE
   procedure Accept_Mission(Mission_Index: Positive) with
      Test_Case => (Name => "Test_AcceptMission", Mode => Nominal);
      -- ****

      -- ****f* Missions/Missions.Update_Missions
      -- FUNCTION
      -- Update accepted missions
      -- PARAMETERS
      -- Minutes - Amount of passed minutes
      -- SOURCE
   procedure Update_Missions(Minutes: Positive) with
      Test_Case => (Name => "Test_UpdateMissions", Mode => Robustness);
      -- ****

      -- ****f* Missions/Missions.Finish_Mission
      -- FUNCTION
      -- Finish selected mission
      -- PARAMETERS
      -- Mission_Index - Player ship list of accepted missions index of mission
      --                 to finish
      -- SOURCE
   procedure Finish_Mission(Mission_Index: Positive) with
      Pre => Mission_Index <= Accepted_Missions.Last_Index,
      Test_Case => (Name => "Test_FinishMission", Mode => Nominal);
      -- ****

      -- ****f* Missions/Missions.Delete_Mission
      -- FUNCTION
      -- Delete selected mission
      -- PARAMETERS
      -- Mission_Index - Player ship list of accepted missions index of mission
      --                 to delete
      -- Failed        - If true, it is failed mission. Default is true.
      -- SOURCE
   procedure Delete_Mission
     (Mission_Index: Positive; Failed: Boolean := True) with
      Pre => Mission_Index <= Accepted_Missions.Last_Index,
      Test_Case => (Name => "Test_DeleteMission", Mode => Nominal);
      -- ****

      -- ****f* Missions/Missions.Update_Mission
      -- FUNCTION
      -- Update status of mission
      -- PARAMETERS
      -- Mission_Index - Player ship list of accepted missions index of mission
      --                 to update
      -- SOURCE
   procedure Update_Mission(Mission_Index: Positive) with
      Pre => Mission_Index <= Accepted_Missions.Last_Index,
      Test_Case => (Name => "Test_UpdateMission", Mode => Nominal);
      -- ****

      -- ****f* Missions/Missions.Auto_Finish_Missions
      -- FUNCTION
      -- Finish all possible missions.
      -- RESULT
      -- Empty string if everything is ok, otherwise message with information
      -- what goes wrong
      -- SOURCE
   function Auto_Finish_Missions return String with
      Test_Case => (Name => "Test_AutoFinishMissions", Mode => Robustness);
      -- ****

      -- ****f* Missions/Missions.Get_Mission_Type
      -- FUNCTION
      -- Get the name of the type of the selected mission
      -- PARAMETERS
      -- M_Type - The type of mission which name will be get
      -- RESULT
      -- Name (as words) of the selected mission's type
      -- SOURCE
   function Get_Mission_Type(M_Type: Missions_Types) return String with
      Post => Get_Mission_Type'Result'Length > 0,
      Test_Case => (Name => "Test_Get_Mission_Type", Mode => Nominal);
   -- ****

end Missions;
