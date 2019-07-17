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

package Themes is

   -- Types of font available in game
   type FontTypes is (HELPFONT, MAPFONT, INTERFACEFONT, ALLFONTS);
   -- Data structure for themes settings
   type ThemeRecord is record
      Name: Unbounded_String; -- Name of theme
      FileName: Unbounded_String; -- Name of .css file of theme
      EnemyShipIcon: Wide_Character; -- Icon used for Enemy Ship event
      AttackOnBaseIcon: Wide_Character; -- Icon used for Attack on Base event
      DiseaseIcon: Wide_Character; -- Icon used for Disease event
      DoublePriceIcon: Wide_Character; -- Icon used for Double Price event
      FullDocksIcon: Wide_Character; -- Icon used for Full Docks event
      EnemyPatrolIcon: Wide_Character; -- Icon used for Enemy Patrol event
      TraderIcon: Wide_Character; -- Icon used for Trader event
      FriendlyShipIcon: Wide_Character; -- Icon used for Friendly Ship event
      DeliverIcon: Wide_Character; -- Icon used for Deliver Item mission
      DestroyIcon: Wide_Character; -- Icon used for Destroy Ship mission
      PatrolIcon: Wide_Character; -- Icon used for  Patrol Area mission
      ExploreIcon: Wide_Character; -- Icon used for Explore Area mission
      PassengerIcon: Wide_Character; -- Icon used for Transport Passenger mission
      PilotIcon: Wide_Character; -- Icon used for Pilot info
      EngineerIcon: Wide_Character; -- Icon used for Engineer info
      GunnerIcon: Wide_Character; -- Icon used for Gunners info
      CrewTraderIcon: Wide_Character; -- Icon used for Trader info
      RepairIcon: Wide_Character; -- Icon used for Repairs info
      UpgradeIcon: Wide_Character; -- Icon used for Upgrade info
      CleanIcon: Wide_Character; -- Icon used for Clean Ship info
      ManufactureIcon: Wide_Character; -- Icon used for Manufacturing info
      MoveMapUpIcon: Wide_Character; -- Icon used for move map up button
      MoveMapDownIcon: Wide_Character; -- Icon used for move map down button
      MoveMapLeftIcon: Wide_Character; -- Icon used for move map left button
      MoveMapRightIcon: Wide_Character; -- Icon used for move map right button
      NoFuelIcon: Wide_Character; -- Icon used for show warning about no fuel
      NoFoodIcon: Wide_Character; -- Icon used for show warning about no food
      NoDrinksIcon: Wide_Character; -- Icon used for show warning about no drinks
      NotVisitedBaseIcon: Wide_Character; -- Icon used for show not visited bases on map
      PlayerShipIcon: Wide_Character; -- Icon used for show player ship on map
      EmptyMapIcon: Wide_Character; -- Icon used for empty map fields
      TargetIcon: Wide_Character; -- Icon used for player selected target on map
      StoryIcon: Wide_Character; -- Icon used for show story event location on map
      OverloadedIcon: Wide_Character; -- Icon used for show warning about overloaded ship
      CheckButtonUnchecked: Unbounded_String; -- Name of image file used for check buttons when unchecked
      CheckButtonChecked: Unbounded_String; -- Name of image file used for check buttons when checked
   end record;
   package Themes_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (String, ThemeRecord, Ada.Strings.Hash, "=");
   -- List of all available themes
   Themes_List: Themes_Container.Map;

   -- Set size of selected font
   procedure SetFontSize(FontType: FontTypes);
   -- Reset size of fonts to theme default values
   procedure ResetFontsSizes;
   -- Load data for all themes
   procedure LoadThemes;

end Themes;
