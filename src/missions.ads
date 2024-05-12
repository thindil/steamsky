--    Copyright 2016-2024 Bartek thindil Jasicki
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
with Game; use Game;

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

      -- ****d* Missions/Missions.Default_Mission_Type
      -- FUNCTION
      -- Default type of missions
      -- SOURCE
   Default_Mission_Type: constant Missions_Types := DELIVER;
   -- ****

   -- ****t* Missions/Missions.Reward_Multiplier
   -- FUNCTION
   -- Used for count reward for finished missions
   -- SOURCE
   type Reward_Multiplier is digits 2 range 0.0 .. 2.0 with
      Default_Value => 1.0;
      -- ****

      --## rule off REDUCEABLE_SCOPE
      -- ****d* Missions/Missions.Default_Multiplier
      -- FUNCTION
      -- Default multiplier for missions reward
      -- SOURCE
   Default_Multiplier: constant Reward_Multiplier := 1.0;
   -- ****
   --## rule on REDUCEABLE_SCOPE

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
            Item_Index: Positive;
         when PASSENGER =>
            Data: Positive := 1;
         when DESTROY =>
            Ship_Index: Natural := 0;
         when others =>
            Target: Natural := 0;
      end case;
   end record;
   -- ****

   -- ****d* Missions/Missions.Empty_Mission
   -- FUNCTION
   -- Default, empty mission data
   -- SOURCE
   Empty_Mission: constant Mission_Data :=
     (M_Type => Default_Mission_Type, others => <>);
   -- ****

   -- ****t* Missions/Missions.Mission_Container
   -- FUNCTION
   -- Used to store data for missions
   -- SOURCE
   package Mission_Container is new Vectors
     (Index_Type => Positive, Element_Type => Mission_Data);
   -- ****

   -- ****e* Missions/Missions.Missions_Accepting_Error
   -- FUNCTION
   -- Raised when mission can't be accepted
   -- SOURCE
   Missions_Accepting_Error: exception;
   -- ****

   -- ****f* Missions/Missions.Accept_Mission
   -- FUNCTION
   -- Accept selected mission from base
   -- PARAMETERS
   -- Mission_Index - Base list of available missions index of mission to
   --                 accept
   -- SOURCE
   procedure Accept_Mission(Mission_Index: Positive);
   -- ****

   -- ****f* Missions/Missions.Auto_Finish_Missions
   -- FUNCTION
   -- Finish all possible missions.
   -- RESULT
   -- Empty string if everything is ok, otherwise message with information
   -- what goes wrong
   -- SOURCE
   function Auto_Finish_Missions return String;
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
      Post => Get_Mission_Type'Result'Length > 0;
   -- ****

-- Temporary code to interact with Nim

   procedure Get_Missions(Base_Index: Positive);
   procedure Set_Missions(Base_Index: Positive);
   function Get_Accepted_Mission(Mission_Index: Positive) return Mission_Data;
   function Get_Accepted_Missions_Amount return Natural with
      Import => True,
      Convention => C,
      External_Name => "getAdaAcceptedMissionsAmount";

end Missions;
