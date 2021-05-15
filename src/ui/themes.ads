-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

-- ****h* Themes/Themes
-- FUNCTION
-- Provide code for read and set the game UI themes
-- SOURCE
package Themes is
-- ****

   -- ****t* Themes/Themes.FontTypes
   -- FUNCTION
   -- Types of font available in game
   -- SOURCE
   type Font_Types is (HELPFONT, MAPFONT, INTERFACEFONT, ALLFONTS);
   -- ****

   -- ****t* Themes/Themes.Theme_Record
   -- FUNCTION
   -- Data structure for themes settings
   -- PARAMETERS
   -- Name                 - Name of theme
   -- File_Name            - Name of .tcl file of theme
   -- Enemy_Ship_Icon      - Icon used for Enemy Ship event
   -- Attack_On_Base_Icon  - Icon used for Attack on Base event
   -- Disease_Icon         - Icon used for Disease event
   -- Double_PriceIcon     - Icon used for Double Price event
   -- Full_Docks_Icon      - Icon used for Full Docks event
   -- Enemy_Patrol_Icon    - Icon used for Enemy Patrol event
   -- Trader_Icon          - Icon used for Trader event
   -- Friendly_Ship_Icon   - Icon used for Friendly Ship event
   -- Deliver_Icon         - Icon used for Deliver Item mission
   -- Destroy_Icon         - Icon used for Destroy Ship mission
   -- Patrol_Icon          - Icon used for  Patrol Area mission
   -- Explore_Icon         - Icon used for Explore Area mission
   -- Passenger_Icon       - Icon used for Transport Passenger mission
   -- Pilot_Icon           - Icon used for Pilot info
   -- Engineer_Icon        - Icon used for Engineer info
   -- Gunner_Icon          - Icon used for Gunners info
   -- CrewTraderIcon       - Icon used for Trader info
   -- RepairIcon           - Icon used for Repairs info
   -- UpgradeIcon          - Icon used for Upgrade info
   -- CleanIcon            - Icon used for Clean Ship info
   -- ManufactureIcon      - Icon used for Manufacturing info
   -- MoveMapUpIcon        - Icon used for move map up button
   -- MoveMapDownIcon      - Icon used for move map down button
   -- MoveMapLeftIcon      - Icon used for move map left button
   -- MoveMapRightIcon     - Icon used for move map right button
   -- NoFuelIcon           - Icon used for show warning about no fuel
   -- NoFoodIcon           - Icon used for show warning about no food
   -- NoDrinksIcon         - Icon used for show warning about no drinks
   -- NotVisitedBaseIcon   - Icon used for show not visited bases on map
   -- PlayerShipIcon       - Icon used for show player ship on map
   -- EmptyMapIcon         - Icon used for empty map fields
   -- TargetIcon           - Icon used for player selected target on map
   -- StoryIcon            - Icon used for show story event location on map
   -- OverloadedIcon       - Icon used for show warning about overloaded ship
   -- SOURCE
   type Theme_Record is record
      Name: Unbounded_String;
      File_Name: Unbounded_String;
      Enemy_Ship_Icon: Wide_Character;
      Attack_On_Base_Icon: Wide_Character;
      Disease_Icon: Wide_Character;
      Double_Price_Icon: Wide_Character;
      Full_Docks_Icon: Wide_Character;
      Enemy_Patrol_Icon: Wide_Character;
      Trader_Icon: Wide_Character;
      Friendly_Ship_Icon: Wide_Character;
      Deliver_Icon: Wide_Character;
      Destroy_Icon: Wide_Character;
      Patrol_Icon: Wide_Character;
      Explore_Icon: Wide_Character;
      Passenger_Icon: Wide_Character;
      Pilot_Icon: Wide_Character;
      Engineer_Icon: Wide_Character;
      Gunner_Icon: Wide_Character;
      CrewTraderIcon: Wide_Character;
      RepairIcon: Wide_Character;
      UpgradeIcon: Wide_Character;
      CleanIcon: Wide_Character;
      ManufactureIcon: Wide_Character;
      MoveMapUpIcon: Wide_Character;
      MoveMapDownIcon: Wide_Character;
      MoveMapLeftIcon: Wide_Character;
      MoveMapRightIcon: Wide_Character;
      NoFuelIcon: Wide_Character;
      NoFoodIcon: Wide_Character;
      NoDrinksIcon: Wide_Character;
      NotVisitedBaseIcon: Wide_Character;
      PlayerShipIcon: Wide_Character;
      EmptyMapIcon: Wide_Character;
      TargetIcon: Wide_Character;
      StoryIcon: Wide_Character;
      OverloadedIcon: Wide_Character;
   end record;
   -- ****

   -- ****t* Themes/Themes.Themes_Container
   -- FUNCTION
   -- Used to store themes data
   -- SOURCE
   package Themes_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (String,
      Theme_Record,
      Ada.Strings.Hash,
      "=");
   -- ****

   -- ****v* Themes/Themes.Themes_List
   -- FUNCTION
   -- List of all available themes
   -- SOURCE
   Themes_List: Themes_Container.Map;
   -- ****

   -- ****f* Themes/Themes.LoadThemes
   -- FUNCTION
   -- Load data for all themes
   -- SOURCE
   procedure LoadThemes;
   -- ****

   -- ****f* Themes/Themes.SetTheme
   -- FUNCTION
   -- Set values for the current theme
   -- SOURCE
   procedure SetTheme;
   -- ****

end Themes;
