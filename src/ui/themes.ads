-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Game; use Game;

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

   -- ****d* Themes/Themes.All_Fonts
   -- FUNCTION
   -- Default value for FontTypes, all fonts
   -- SOURCE
   All_Fonts: constant Font_Types := ALLFONTS;
   -- ****

   -- ****d* Themes/ThemesDefault_Theme_Icons_Path
   -- FUNCTION
   -- Path to the icons in the default the game's theme
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   Default_Theme_Icons_Path: constant Unbounded_String :=
     Data_Directory & "ui" & Dir_Separator & "images" & Dir_Separator &
     "icons" & Dir_Separator;
   -- ****

   -- ****t* Themes/Themes.Theme_Record
   -- FUNCTION
   -- Data structure for themes settings
   -- PARAMETERS
   -- Name                  - Name of theme
   -- File_Name             - Name of .tcl file of theme
   -- Enemy_Ship_Icon       - Icon used for Enemy Ship event
   -- Attack_On_Base_Icon   - Icon used for Attack on Base event
   -- Disease_Icon          - Icon used for Disease event
   -- Double_PriceIcon      - Icon used for Double Price event
   -- Full_Docks_Icon       - Icon used for Full Docks event
   -- Enemy_Patrol_Icon     - Icon used for Enemy Patrol event
   -- Trader_Icon           - Icon used for Trader event
   -- Friendly_Ship_Icon    - Icon used for Friendly Ship event
   -- Deliver_Icon          - Icon used for Deliver Item mission
   -- Destroy_Icon          - Icon used for Destroy Ship mission
   -- Patrol_Icon           - Icon used for Patrol Area mission
   -- Explore_Icon          - Icon used for Explore Area mission
   -- Passenger_Icon        - Icon used for Transport Passenger mission
   -- Pilot_Icon            - Icon used for Pilot info
   -- Engineer_Icon         - Icon used for Engineer info
   -- Gunner_Icon           - Icon used for Gunners info
   -- Crew_Trader_Icon      - Icon used for Trader info
   -- Repair_Icon           - Icon used for Repairs info when repairs are on
   -- No_Repair_Icon        - Icon used for Repairs info when noone is assigned
   -- Repair_Order_Icon     - Icon used for giving repair order to all crew members
   -- Upgrade_Icon          - Icon used for Upgrade info when upgrade is on
   -- No_Upgrade_Icon       - Icon used for Upgrade info when noone is assigned
   -- Clean_Icon            - Icon used for Clean Ship info when cleaning is on
   -- No_Clean_Icon         - Icon used for Clean Ship info when noone is assigned
   -- Clean_Order_Icon      - Icon used for giving clean ship order to all crew members
   -- Manufacture_Icon      - Icon used for Manufacturing info when someone working on it
   -- No_Manufacture_Icon   - Icon used for Manufacturing info when noone is assigned
   -- Move_Map_Up_Icon      - Icon used for move map up button
   -- Move_Map_Down_Icon    - Icon used for move map down button
   -- Move_Map_Left_Icon    - Icon used for move map left button
   -- Move_Map_Right_Icon   - Icon used for move map right button
   -- No_Fuel_Icon          - Icon used for show warning about no fuel
   -- Low_Fuel_Icon         - Icon used for show warning about low level of fuel
   -- No_Food_Icon          - Icon used for show warning about no food
   -- Low_Food_Icon         - Icon used for show warning about low level of food
   -- No_Drinks_Icon        - Icon used for show warning about no drinks
   -- Low_Drinks_Icon       - Icon used for show warning about low level of drinks
   -- Not_Visited_Base_Icon - Icon used for show not visited bases on map
   -- Player_Ship_Icon      - Icon used for show player ship on map
   -- Empty_Map_Icon        - Icon used for empty map fields
   -- Target_Icon           - Icon used for player selected target on map
   -- Story_Icon            - Icon used for show story event location on map
   -- Overloaded_Icon       - Icon used for show warning about overloaded ship
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
      Pilot_Icon: Unbounded_String;
      Engineer_Icon: Unbounded_String;
      Gunner_Icon: Unbounded_String;
      Crew_Trader_Icon: Unbounded_String;
      Repair_Icon: Unbounded_String;
      No_Repair_Icon: Unbounded_String;
      Repair_Order_Icon: Unbounded_String;
      Upgrade_Icon: Unbounded_String;
      No_Upgrade_Icon: Unbounded_String;
      Clean_Icon: Unbounded_String;
      No_Clean_Icon: Unbounded_String;
      Clean_Order_Icon: Unbounded_String;
      Manufacture_Icon: Unbounded_String;
      No_Manufacture_Icon: Unbounded_String;
      Move_Map_Up_Icon: Unbounded_String;
      Move_Map_Down_Icon: Unbounded_String;
      Move_Map_Left_Icon: Unbounded_String;
      Move_Map_Right_Icon: Unbounded_String;
      No_Fuel_Icon: Unbounded_String;
      Low_Fuel_Icon: Unbounded_String;
      No_Food_Icon: Unbounded_String;
      Low_Food_Icon: Unbounded_String;
      No_Drinks_Icon: Unbounded_String;
      Low_Drinks_Icon: Unbounded_String;
      Not_Visited_Base_Icon: Wide_Character;
      Player_Ship_Icon: Wide_Character;
      Empty_Map_Icon: Wide_Character;
      Target_Icon: Wide_Character;
      Story_Icon: Wide_Character;
      Overloaded_Icon: Wide_Character;
   end record;
   -- ****

   -- ****d* Themes/Themes.Default_Theme
   -- FUNCTION
   -- Default the game theme
   -- SOURCE
   Default_Theme: constant Theme_Record :=
     (Name => Null_Unbounded_String, File_Name => Null_Unbounded_String,
      Enemy_Ship_Icon => Wide_Character'Val(16#f51c#),
      Attack_On_Base_Icon => Wide_Character'Val(16#f543#),
      Disease_Icon => Wide_Character'Val(16#f5a6#),
      Double_Price_Icon => Wide_Character'Val(16#f0d6#),
      Full_Docks_Icon => Wide_Character'Val(16#f057#),
      Enemy_Patrol_Icon => Wide_Character'Val(16#f51b#),
      Trader_Icon => Wide_Character'Val(16#f197#),
      Friendly_Ship_Icon => Wide_Character'Val(16#f197#),
      Deliver_Icon => Wide_Character'Val(16#f53b#),
      Destroy_Icon => Wide_Character'Val(16#fc6a#),
      Patrol_Icon => Wide_Character'Val(16#f540#),
      Explore_Icon => Wide_Character'Val(16#f707#),
      Passenger_Icon => Wide_Character'Val(16#f183#),
      Pilot_Icon => Default_Theme_Icons_Path & "pilot.svg",
      Engineer_Icon => Default_Theme_Icons_Path & "engineer.svg",
      Gunner_Icon => Default_Theme_Icons_Path & "gunner.svg",
      Crew_Trader_Icon => Default_Theme_Icons_Path & "crewtrader.svg",
      Repair_Icon => Default_Theme_Icons_Path & "repair.svg",
      No_Repair_Icon => Default_Theme_Icons_Path & "repair-empty.svg",
      Repair_Order_Icon => Default_Theme_Icons_Path & "repair-order.svg",
      Upgrade_Icon => Default_Theme_Icons_Path & "upgrade.svg",
      No_Upgrade_Icon => Default_Theme_Icons_Path & "upgrade-empty.svg",
      Clean_Icon => Default_Theme_Icons_Path & "clean.svg",
      No_Clean_Icon => Default_Theme_Icons_Path & "clean-empty.svg",
      Clean_Order_Icon => Default_Theme_Icons_Path & "clean-order.svg",
      Manufacture_Icon => Default_Theme_Icons_Path & "craft.svg",
      No_Manufacture_Icon => Default_Theme_Icons_Path & "craft-empty.svg",
      Move_Map_Up_Icon => Default_Theme_Icons_Path & "vertical-flip.svg",
      Move_Map_Down_Icon => Default_Theme_Icons_Path & "vertical-flip.svg",
      Move_Map_Left_Icon => Default_Theme_Icons_Path & "horizontal-flip.svg",
      Move_Map_Right_Icon => Default_Theme_Icons_Path & "horizontal-flip.svg",
      No_Fuel_Icon => Default_Theme_Icons_Path & "nofuel.svg",
      Low_Fuel_Icon => Default_Theme_Icons_Path & "lowfuel.svg",
      No_Food_Icon => Default_Theme_Icons_Path & "nofood.svg",
      Low_Food_Icon => Default_Theme_Icons_Path & "lowfood.svg",
      No_Drinks_Icon => Default_Theme_Icons_Path & "nodrinks.svg",
      Low_Drinks_Icon => Default_Theme_Icons_Path & "lowdrinks.svg",
      Not_Visited_Base_Icon => Wide_Character'Val(16#229b#),
      Player_Ship_Icon => Wide_Character'Val(16#f135#),
      Empty_Map_Icon => Wide_Character'Val(16#f0c8#),
      Target_Icon => Wide_Character'Val(16#f05b#),
      Story_Icon => Wide_Character'Val(16#f059#),
      Overloaded_Icon => Wide_Character'Val(16#f55b#));
   -- ****

   -- ****t* Themes/Themes.Themes_Container
   -- FUNCTION
   -- Used to store themes data
   -- SOURCE
   package Themes_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Theme_Record,
      Hash => Ada.Strings.Hash, Equivalent_Keys => "=");
   -- ****

   -- ****v* Themes/Themes.Themes_List
   -- FUNCTION
   -- List of all available themes
   -- SOURCE
   Themes_List: Themes_Container.Map;
   -- ****

   -- ****f* Themes/Themes.Load_Themes
   -- FUNCTION
   -- Load data for all themes
   -- SOURCE
   procedure Load_Themes;
   -- ****

   -- ****f* Themes/Themes.Set_Theme
   -- FUNCTION
   -- Set values for the current theme
   -- SOURCE
   procedure Set_Theme;
   -- ****

   -- ****f* Themes/Themes.Load_Theme_Images
   -- FUNCTION
   -- Load all images for the selected the game's theme
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   procedure Load_Theme_Images;
   -- ****

end Themes;
