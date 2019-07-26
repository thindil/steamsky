--    Copyright 2018-2019 Bartek thindil Jasicki
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

-- ****h* Steamsky/Themes
-- FUNCTION
-- Provides code for manipulate the game themes
-- SOURCE
package Themes is
-- ****

   -- ****t* Themes/FontTypes
   -- FUNCTION
   -- Types of font available in game
   -- SOURCE
   type FontTypes is (HELPFONT, MAPFONT, INTERFACEFONT, ALLFONTS);
   -- ****
   -- ****t* Themes/ThemeRecord
   -- FUNCTION
   -- Data structure for themes settings
   -- PARAMETERS
   -- Name                 - Name of theme
   -- FileName             - Name of .css file of theme
   -- EnemyShipIcon        - Icon used for Enemy Ship event
   -- AttackOnBaseIcon     - Icon used for Attack on Base event
   -- DiseaseIcon          - Icon used for Disease event
   -- DoublePriceIcon      - Icon used for Double Price event
   -- FullDocksIcon        - Icon used for Full Docks event
   -- EnemyPatrolIcon      - Icon used for Enemy Patrol event
   -- TraderIcon           - Icon used for Trader event
   -- FriendlyShipIcon     - Icon used for Friendly Ship event
   -- DeliverIcon          - Icon used for Deliver Item mission
   -- DestroyIcon          - Icon used for Destroy Ship mission
   -- PatrolIcon           - Icon used for  Patrol Area mission
   -- ExploreIcon          - Icon used for Explore Area mission
   -- PassengerIcon        - Icon used for Transport Passenger mission
   -- PilotIcon            - Icon used for Pilot info
   -- EngineerIcon         - Icon used for Engineer info
   -- GunnerIcon           - Icon used for Gunners info
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
   -- CheckButtonUnchecked - Name of image file used for check buttons when
   --                        unchecked
   -- CheckButtonChecked   - Name of image file used for check buttons when
   --                        checked
   -- SOURCE
   type ThemeRecord is record
      Name: Unbounded_String;
      FileName: Unbounded_String;
      EnemyShipIcon: Wide_Character;
      AttackOnBaseIcon: Wide_Character;
      DiseaseIcon: Wide_Character;
      DoublePriceIcon: Wide_Character;
      FullDocksIcon: Wide_Character;
      EnemyPatrolIcon: Wide_Character;
      TraderIcon: Wide_Character;
      FriendlyShipIcon: Wide_Character;
      DeliverIcon: Wide_Character;
      DestroyIcon: Wide_Character;
      PatrolIcon: Wide_Character;
      ExploreIcon: Wide_Character;
      PassengerIcon: Wide_Character;
      PilotIcon: Wide_Character;
      EngineerIcon: Wide_Character;
      GunnerIcon: Wide_Character;
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
      CheckButtonUnchecked: Unbounded_String;
      CheckButtonChecked: Unbounded_String;
   end record;
   -- ****
   -- ****t* Themes/Themes_Container
   -- FUNCTION
   -- Used to store themes data
   -- SOURCE
   package Themes_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (String, ThemeRecord, Ada.Strings.Hash, "=");
   -- ****
   -- ****v* Themes/Themes_List
   -- FUNCTION
   -- List of all available themes
   -- SOURCE
   Themes_List: Themes_Container.Map;
   -- ****

   -- ****f* Themes/SetFontSize
   -- FUNCTION
   -- Set size of selected font
   -- PARAMETERS
   -- FontType - Type of font to set
   -- SOURCE
   procedure SetFontSize(FontType: FontTypes);
   -- ****
   -- ****f* Themes/ResetFontsSizes
   -- FUNCTION
   -- Reset size of fonts to theme default values
   -- SOURCE
   procedure ResetFontsSizes;
   -- ****
   -- ****f* Themes/LoadThemes
   -- FUNCTION
   -- Load data for all themes
   -- SOURCE
   procedure LoadThemes;
   -- ****

end Themes;
